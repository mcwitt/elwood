module Elwood.Claude.AgentLoop
  ( runAgentTurn,
    AgentConfig (..),
    AgentResult (..),
    ExhaustionInfo (..),
    formatExhaustion,
    perceiveResultImages,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import Control.Monad (unless, when)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Aeson (Value (Object), encode, object, (.=))
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (toList)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Elwood.AgentSettings (AgentProfile (..), ModelRef (..))
import Elwood.Claude.Client
import Elwood.Claude.Observer
import Elwood.Claude.Pruning
import Elwood.Claude.Turn qualified as Turn
import Elwood.Claude.Types
import Elwood.Config (PruningConfig (..))
import Elwood.Exception (catchSync)
import Elwood.Image (perceiveImageBytes)
import Elwood.Logging (Logger)
import Elwood.Permissions (ToolPolicy (..), getToolPolicy)
import Elwood.Positive (Positive (getPositive))
import Elwood.Thinking (ThinkingMode (..), progressUpdatesEnabled)
import Elwood.Tools.Registry (ToolRegistry, lookupTool, toolSchemas)
import Elwood.Tools.Types

data AgentResult
  = AgentSuccess Text [ClaudeMessage]
  | AgentTruncated Text [ClaudeMessage]
  | AgentExhausted ExhaustionInfo
  | AgentError Text
  | AgentCancelled
  deriving stock (Show)

data ExhaustionInfo = ExhaustionInfo
  {partialOutput :: Text, iterationsUsed :: Int, toolsCalled :: Int, messages :: [ClaudeMessage]}
  deriving stock (Show)

data AgentConfig = AgentConfig
  { logger :: Logger,
    client :: ClaudeClient,
    registry :: ToolRegistry,
    requestApproval :: ApprovalFunction,
    systemPrompt :: Maybe Text,
    agentProfile :: AgentProfile,
    observer :: AgentObserver,
    onRateLimit :: Maybe RateLimitCallback,
    onText :: Maybe TextCallback,
    onToolUse :: Maybe ToolUseCallback,
    onBeforeApiCall :: Maybe (IO ()),
    toolSearch :: Maybe (Set ToolName),
    pruningConfig :: Maybe PruningConfig,
    pruneHorizon :: Int,
    outputFormat :: Maybe OutputFormat,
    maxImageDimension :: Maybe Int,
    isCancelled :: IO Bool
  }

-- | The interpreter carries only this turn's immutable progress. Historical
-- messages are an input to requests, never part of turn accounting.
runAgentTurn :: AgentConfig -> [ClaudeMessage] -> ClaudeMessage -> IO AgentResult
runAgentTurn cfg history input = loop 0 (Turn.begin input)
  where
    profile = cfg.agentProfile
    updates = progressUpdatesEnabled profile.thinking
    schemas = toolSchemas cfg.registry
    stopped = cfg.isCancelled
    loop requests progress = do
      cancelled <- stopped
      if cancelled
        then pure AgentCancelled
        else
          if requests >= profile.maxIterations.getPositive
            then pure (AgentExhausted (ExhaustionInfo (lastText progress) requests (Turn.callsMade progress) (Turn.messages progress)))
            else do
              let transcript = prune (history <> Turn.messages progress)
                  request =
                    MessagesRequest
                      profile.model.model
                      profile.maxTokens.getPositive
                      cfg.systemPrompt
                      transcript
                      schemas
                      (thinking profile.thinking)
                      profile.cache
                      cfg.toolSearch
                      cfg.outputFormat
                  retry = defaultRetryConfig {onRetry = Just (\attemptNumber seconds _ -> mapM_ (\notify -> notify attemptNumber seconds) cfg.onRateLimit)}
              cfg.observer.onInputEstimate cfg.systemPrompt cfg.toolSearch transcript schemas
              sequence_ cfg.onBeforeApiCall
              response <- retryWithBackoff retry (attempt request) (pause . (* 10))
              cancelledAfterRequest <- stopped
              if cancelledAfterRequest
                then pure AgentCancelled
                else case response of
                  Left err -> pure (AgentError (T.pack (show err)))
                  Right Nothing -> pure AgentCancelled
                  Right (Just reply) -> do
                    cfg.observer.onApiResponse reply.stopReason reply.usage
                    case Turn.decide reply.stopReason reply.content of
                      Left err -> pure (AgentError ("Protocol error: " <> err))
                      Right (Turn.Finish outcome) -> pure $ case outcome of
                        Turn.Completed message -> AgentSuccess (Turn.outputText updates message) (Turn.finish progress outcome)
                        Turn.Truncated message -> AgentTruncated (Turn.outputText updates message) (Turn.finish progress outcome)
                      Right (Turn.Continue pending) -> do
                        let text = Turn.outputText updates (Turn.pendingMessage pending)
                            calls = toList (Turn.pendingCalls pending)
                        unless (T.null text) $ mapM_ ($ text) cfg.onText
                        mapM_ (\notify -> notify requests [(call.name.unToolName, Object call.input) | call <- calls]) cfg.onToolUse
                        exchange <- runExceptT $ Turn.complete execute pending
                        case exchange of
                          Left () -> pure AgentCancelled
                          Right completed -> loop (requests + 1) (Turn.append completed progress)
    attempt request = do
      cancelled <- stopped
      if cancelled then pure (Right Nothing) else fmap Just <$> sendMessages cfg.client profile.model.provider request
    pause ticks | ticks <= 0 = pure ()
    pause ticks = do
      cancelled <- stopped
      unless cancelled (threadDelay 100000 >> pause (ticks - 1))
    lastText progress = case [Turn.outputText updates message | message <- reverse (Turn.messages progress), message.role == Assistant] of
      text : _ -> text
      [] -> ""
    prune transcript = case cfg.pruningConfig of
      Nothing -> transcript
      Just p -> pruneThinkingBlocks p.thinking cfg.pruneHorizon $ case p.tools of
        Nothing -> transcript
        Just t -> pruneToolInputs t cfg.pruneHorizon (pruneToolResults t cfg.pruneHorizon transcript)
    execute call = do
      ExceptT ((\yes -> if yes then Left () else Right ()) <$> stopped)
      result <- ExceptT $ Right <$> (executeCall call `catchSync` \(err :: SomeException) -> pure (ToolError (T.take 500 (T.pack (show err)))))
      after <- ExceptT (Right <$> stopped)
      when after (throwE ())
      pure (perceiveResultImages cfg.maxImageDimension result)
    executeCall call = do
      cfg.observer.onToolCall call.name.unToolName
      case lookupTool call.name cfg.registry of
        Nothing -> pure (ToolError ("Unknown tool: " <> call.name.unToolName))
        Just tool -> case getToolPolicy profile.permissions call.name of
          PolicyDeny -> pure (ToolError "Tool denied by policy")
          PolicyAllow -> tool.execute (Object call.input)
          PolicyAsk -> do
            approval <- cfg.requestApproval call.name (T.take 200 (decodeUtf8 (LBS.toStrict (encode call.input))))
            cancelled <- stopped
            if cancelled
              then pure (ToolError "Turn cancelled")
              else case approval of
                ApprovalGranted -> tool.execute (Object call.input)
                outcome -> pure (ToolError (T.pack (show outcome)))
    thinking = fmap $ \case
      Adaptive effort display -> ThinkingConfigAdaptive effort display
      Budget tokens -> ThinkingConfigBudget tokens.getPositive

formatExhaustion :: ExhaustionInfo -> Text
formatExhaustion info =
  decodeUtf8 . LBS.toStrict . encode $
    object
      [ "status" .= failureTag MaxIterations,
        "iterations_used" .= info.iterationsUsed,
        "tools_called" .= info.toolsCalled,
        "partial_output" .= info.partialOutput
      ]

perceiveResultImages :: Maybe Int -> ToolResult -> ToolResult
perceiveResultImages maxDimension (ToolSuccess parts) = ToolSuccess (map perceive parts)
  where
    perceive (ToolResultImage _ encoded) =
      either
        (ToolResultText . ("[image elided: " <>) . (<> "]"))
        imageResultPart
        (perceiveImageBytes maxDimension (B64.decodeLenient (encodeUtf8 encoded)))
    perceive part = part
perceiveResultImages _ err = err
