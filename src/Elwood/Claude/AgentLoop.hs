{-# LANGUAGE StrictData #-}

module Elwood.Claude.AgentLoop
  ( runAgentTurn,
    AgentConfig (..),
    AgentResult (..),
    RateLimitCallback,
    TextCallback,
  )
where

import Data.Aeson (Value (..), encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Elwood.Claude.Client (ClaudeClient, RetryConfig (..), defaultRetryConfig, sendMessagesWithRetry)
import Elwood.Claude.Compaction (CompactionConfig, compactIfNeeded)
import Elwood.Claude.Pruning (pruneToolResults)
import Elwood.Claude.Types
  ( ClaudeError (..),
    ClaudeMessage (..),
    ContentBlock (..),
    MessagesRequest (..),
    MessagesResponse (..),
    Role (..),
    StopReason (..),
    ThinkingConfig (..),
    Usage (..),
    stopReasonToText,
  )
import Elwood.Config (ThinkingEffort (..), ThinkingLevel (..))
import Elwood.Logging (Logger, logError, logInfo, logWarn)
import Elwood.Metrics (MetricsStore, recordApiResponse, recordToolCall)
import Elwood.Permissions (ToolPolicy (..), getToolPolicy)
import Elwood.Tools.Registry
  ( ToolRegistry,
    lookupTool,
    toolSchemas,
  )
import Elwood.Tools.Types (AgentContext (..), ApprovalOutcome (..), Tool (..), ToolResult (..))

-- | Result of running an agent turn
data AgentResult
  = -- | Success with response text and all messages (for persistence)
    AgentSuccess Text [ClaudeMessage]
  | -- | Error that should be shown to user
    AgentError Text
  deriving stock (Show)

-- | Callback for rate limit notifications
-- Arguments: retry attempt number, wait seconds
type RateLimitCallback = Int -> Int -> IO ()

-- | Callback for intermediate text content produced during tool-use turns
type TextCallback = Text -> IO ()

-- | Configuration for the agent loop (all environment/config params)
data AgentConfig = AgentConfig
  { logger :: Logger,
    client :: ClaudeClient,
    registry :: ToolRegistry,
    context :: AgentContext,
    compaction :: CompactionConfig,
    systemPrompt :: Maybe Text,
    model :: Text,
    thinking :: ThinkingLevel,
    maxIterations :: Int,
    metrics :: MetricsStore,
    -- | Source label for metrics
    source :: Text,
    -- | Optional callback for rate limit notifications
    onRateLimit :: Maybe RateLimitCallback,
    -- | Optional callback for intermediate text produced during tool-use turns
    onText :: Maybe TextCallback,
    -- | Tool search (Nothing = disabled, Just neverDefer = enabled with deferred loading)
    toolSearch :: Maybe (Set Text),
    -- | Prune horizon: tool results before this index are replaced with placeholders
    pruneHorizon :: Int
  }

-- | Convert a thinking level to the API thinking config
thinkingToConfig :: ThinkingLevel -> Maybe ThinkingConfig
thinkingToConfig ThinkingOff = Nothing
thinkingToConfig (ThinkingAdaptive effort) = Just (ThinkingConfigAdaptive effort)
thinkingToConfig (ThinkingBudget n) = Just (ThinkingConfigBudget n)

-- | Get the max_tokens value for a thinking level
thinkingMaxTokens :: ThinkingLevel -> Int
thinkingMaxTokens ThinkingOff = 4096
thinkingMaxTokens (ThinkingAdaptive EffortLow) = 4096
thinkingMaxTokens (ThinkingAdaptive EffortMedium) = 16000
thinkingMaxTokens (ThinkingAdaptive EffortHigh) = 32768
thinkingMaxTokens (ThinkingBudget n) = max 4096 (n * 2)

-- | Run a complete agent turn, handling tool use loops
runAgentTurn ::
  AgentConfig ->
  -- | Existing conversation history
  [ClaudeMessage] ->
  -- | New user message
  ClaudeMessage ->
  IO AgentResult
runAgentTurn cfg history userMessage = do
  -- Compact history if needed before adding new message
  compactedHistory <- compactIfNeeded cfg.logger cfg.client cfg.compaction cfg.metrics cfg.source history
  let adjustedHorizon = min cfg.pruneHorizon (length compactedHistory)
      msgs = compactedHistory ++ [userMessage]

  agentLoop cfg {pruneHorizon = adjustedHorizon} msgs 0

-- | The main agent loop
agentLoop ::
  AgentConfig ->
  [ClaudeMessage] ->
  Int ->
  IO AgentResult
agentLoop cfg msgs iteration
  | iteration >= cfg.maxIterations = do
      logError cfg.logger "Agent loop exceeded max iterations" []
      pure $ AgentError "(Agent loop exceeded max iterations)"
  | otherwise = do
      let lgr = cfg.logger
          reg = cfg.registry
          mdl = cfg.model
          thk = cfg.thinking

      -- Always send all tool schemas (tool search handles filtering server-side)
      let schemas = toolSchemas reg

      -- Build and send request
      let request =
            MessagesRequest
              { model = mdl,
                maxTokens = thinkingMaxTokens thk,
                system = cfg.systemPrompt,
                messages = pruneToolResults cfg.pruneHorizon msgs,
                tools = schemas,
                thinking = thinkingToConfig thk,
                cacheControl = True,
                toolSearch = cfg.toolSearch
              }

      logInfo
        lgr
        "Sending request to Claude"
        [ ("iteration", T.pack (show iteration)),
          ("message_count", T.pack (show (length msgs))),
          ("tool_count", T.pack (show (length schemas)))
        ]

      -- Configure retry with logging callback and optional user notification
      let retryConfig =
            defaultRetryConfig
              { onRetry = Just $ \attemptNum waitSecs err -> do
                  logWarn
                    lgr
                    "Rate limited, retrying"
                    [ ("attempt", T.pack (show attemptNum)),
                      ("wait_seconds", T.pack (show waitSecs)),
                      ("error", T.pack (show err))
                    ]
                  -- Notify user if callback is configured
                  case cfg.onRateLimit of
                    Just notify_ -> notify_ attemptNum waitSecs
                    Nothing -> pure ()
              }

      result <- sendMessagesWithRetry cfg.client retryConfig request

      case result of
        Left err -> do
          logError lgr "Claude API error" [("error", T.pack (show err))]
          pure $ AgentError $ formatError err
        Right response -> do
          let u = response.usage
              src = cfg.source
          logInfo
            lgr
            "Claude response received"
            [ ("stop_reason", stopReasonToText response.stopReason),
              ("content_blocks", T.pack (show (length response.content))),
              ("input_tokens", T.pack (show u.inputTokens)),
              ("output_tokens", T.pack (show u.outputTokens)),
              ("cache_read_tokens", T.pack (show u.cacheReadInputTokens)),
              ("cache_creation_tokens", T.pack (show u.cacheCreationInputTokens))
            ]

          -- Record API metrics
          recordApiResponse cfg.metrics mdl src response.stopReason response.usage

          handleResponse cfg msgs response iteration

-- | Classification of a Claude response into a pure action
data AgentAction
  = -- | Normal completion: response text + all messages
    Complete Text [ClaudeMessage]
  | -- | Tool use requested: tool use blocks, intermediate text, all messages so far
    ContinueWithTools [ContentBlock] Text [ClaudeMessage]
  | -- | Response truncated due to token limit
    TruncatedResponse Text [ClaudeMessage]

-- | Pure classification of a response based on stop reason and content.
classifyResponse :: StopReason -> [ContentBlock] -> [ClaudeMessage] -> AgentAction
classifyResponse stopReason content_ msgs =
  let responseText = extractTextContent content_
      assistantMsg = ClaudeMessage Assistant content_
      allMessages = msgs ++ [assistantMsg]
   in case stopReason of
        EndTurn -> Complete responseText allMessages
        ToolUse ->
          let toolUses = extractToolUses content_
           in ContinueWithTools toolUses responseText allMessages
        MaxTokens -> TruncatedResponse responseText allMessages
        StopReasonOther _ -> Complete responseText allMessages

-- | Build the user message for the next iteration from tool results.
buildToolResultMessage :: [ContentBlock] -> [ToolResult] -> ClaudeMessage
buildToolResultMessage toolUses toolResults =
  let resultBlocks = zipWith makeResultBlock toolUses toolResults
   in ClaudeMessage User resultBlocks

-- | Handle Claude's response (thin IO wrapper around pure classification)
handleResponse ::
  AgentConfig ->
  [ClaudeMessage] ->
  MessagesResponse ->
  Int ->
  IO AgentResult
handleResponse cfg msgs response iteration =
  case classifyResponse response.stopReason response.content msgs of
    Complete responseText allMessages ->
      pure $ AgentSuccess responseText allMessages
    TruncatedResponse responseText allMessages -> do
      logWarn cfg.logger "Response hit max tokens" []
      pure $ AgentSuccess (responseText <> "\n\n(Response was truncated due to length)") allMessages
    ContinueWithTools toolUses intermediateText allMessages -> do
      let lgr = cfg.logger
          reg = cfg.registry
          mets = cfg.metrics

      logInfo lgr "Tool use requested" [("tool_count", T.pack (show (length toolUses)))]

      -- Send any intermediate text to the user immediately
      case cfg.onText of
        Just cb | not (T.null intermediateText) -> cb intermediateText
        _ -> pure ()

      -- Record tool call metrics
      mapM_ (\case ToolUseBlock _ n _ -> recordToolCall mets n; _ -> pure ()) toolUses

      -- Execute all tool uses
      toolResults <- executeToolUses lgr reg cfg.context toolUses

      -- Build messages for next iteration (pure)
      let nextIteration = iteration + 1
          userMsg = buildToolResultMessage toolUses toolResults
          newMessages = allMessages ++ [userMsg]

      -- Continue the loop
      agentLoop cfg newMessages nextIteration

-- | Execute all tool uses and return their results.
executeToolUses ::
  Logger ->
  ToolRegistry ->
  AgentContext ->
  [ContentBlock] ->
  IO [ToolResult]
executeToolUses lgr reg ctx = mapM (executeToolUse lgr reg ctx)

-- | Extract text content from content blocks
extractTextContent :: [ContentBlock] -> Text
extractTextContent blocks =
  T.intercalate "\n" [t | TextBlock t <- blocks]

-- | Extract tool use blocks
extractToolUses :: [ContentBlock] -> [ContentBlock]
extractToolUses = filter isToolUse
  where
    isToolUse (ToolUseBlock {}) = True
    isToolUse _ = False

-- | Execute a single tool use with policy checking
executeToolUse :: Logger -> ToolRegistry -> AgentContext -> ContentBlock -> IO ToolResult
executeToolUse lgr reg ctx (ToolUseBlock tid n input) = do
  logInfo lgr "Executing tool" [("tool", n), ("id", tid)]

  case lookupTool n reg of
    Nothing -> do
      logWarn lgr "Unknown tool" [("tool", n)]
      pure $ ToolError $ "Unknown tool: " <> n
    Just tool -> do
      -- Check tool policy before execution
      let policy = getToolPolicy ctx.permissionConfig n

      case policy of
        PolicyDeny -> do
          logWarn lgr "Tool denied by policy" [("tool", n)]
          pure $ ToolError $ "Tool '" <> n <> "' is not allowed by policy"
        PolicyAllow -> do
          executeToolWithLogging lgr tool n input
        PolicyAsk -> do
          -- Request approval before execution
          case ctx.requestApproval of
            Nothing -> do
              -- No approval mechanism configured, fall back to allow
              logWarn lgr "No approval mechanism, allowing tool" [("tool", n)]
              executeToolWithLogging lgr tool n input
            Just reqApproval -> do
              logInfo lgr "Requesting approval for tool" [("tool", n)]
              let inputSummary = T.take 200 (decodeUtf8 (LBS.toStrict (encode input)))
              outcome <- reqApproval n inputSummary
              case outcome of
                ApprovalGranted -> do
                  logInfo lgr "Tool approved by user" [("tool", n)]
                  executeToolWithLogging lgr tool n input
                ApprovalDenied -> do
                  logInfo lgr "Tool denied by user" [("tool", n)]
                  pure $ ToolError "Action denied by user"
                ApprovalTimeout -> do
                  logWarn lgr "Tool approval timed out" [("tool", n)]
                  pure $ ToolError "Approval request timed out"
executeToolUse _ _ _ _ = pure $ ToolError "Invalid tool use block"

-- | Execute a tool and log the result
executeToolWithLogging :: Logger -> Tool -> Text -> Value -> IO ToolResult
executeToolWithLogging lgr tool n input = do
  result <- tool.execute input
  case result of
    ToolSuccess output ->
      logInfo lgr "Tool succeeded" [("tool", n), ("output_length", T.pack (show (T.length output)))]
    ToolError err ->
      logWarn lgr "Tool failed" [("tool", n), ("error", err)]
  pure result

-- | Make a tool result block from a tool use and its result
makeResultBlock :: ContentBlock -> ToolResult -> ContentBlock
makeResultBlock (ToolUseBlock tid _ _) result =
  case result of
    ToolSuccess output ->
      ToolResultBlock tid output False
    ToolError err ->
      ToolResultBlock tid err True
makeResultBlock _ _ =
  ToolResultBlock "" "Invalid tool use" True

-- | Format an error for user display
formatError :: ClaudeError -> Text
formatError (ClaudeRateLimited retryAfter) =
  "I'm being rate limited right now. "
    <> maybe
      "Please try again in a moment."
      (\secs -> "Retry after " <> T.pack (show secs) <> " seconds.")
      retryAfter
formatError (ClaudeOverloaded retryAfter) =
  "Claude is currently overloaded. "
    <> maybe
      "Please try again in a few minutes."
      (\secs -> "Retry after " <> T.pack (show secs) <> " seconds.")
      retryAfter
formatError (ClaudeApiError errType errMsg) =
  "Sorry, I encountered an error: " <> errType <> " - " <> errMsg
formatError (ClaudeHttpError status _) =
  "Sorry, there was a connection error (HTTP " <> T.pack (show status) <> "). Please try again."
formatError (ClaudeParseError _) =
  "Sorry, I received an unexpected response. Please try again."
