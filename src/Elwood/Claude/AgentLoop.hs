module Elwood.Claude.AgentLoop
  ( runAgentTurn,
    AgentConfig (..),
    AgentResult (..),
    ExhaustionInfo (..),
    formatExhaustion,

    -- * Exported for testing
    perceiveResultImages,
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException)
import Data.Aeson (Value (..), encode, object, (.=))
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as LBS
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Elwood.AgentSettings (AgentProfile (..), ModelRef (..))
import Elwood.Claude.Client (ClaudeClient, RetryConfig (..), defaultRetryConfig, sendMessagesWithRetry)
import Elwood.Claude.Observer (AgentObserver (..), RateLimitCallback, TextCallback, ToolUseCallback)
import Elwood.Claude.Pruning (pruneThinkingBlocks, pruneToolInputs, pruneToolResults)
import Elwood.Claude.Types
  ( ClaudeError (..),
    ClaudeMessage (..),
    ContentBlock (..),
    MessagesRequest (..),
    MessagesResponse (..),
    OutputFormat,
    Role (..),
    StopReason (..),
    ThinkingConfig (..),
    ToolName (..),
    ToolResultPart (..),
    ToolUseId (..),
    Usage (..),
    stopReasonToText,
    toolResultText,
  )
import Elwood.Config (PruningConfig (..))
import Elwood.Exception (catchSync)
import Elwood.Image (perceiveImageBytes)
import Elwood.Logging (Logger, logError, logInfo, logWarn)
import Elwood.Notify (Severity (..), formatNotify, sanitizeBackticks)
import Elwood.Permissions (PermissionConfig, ToolPolicy (..), getToolPolicy)
import Elwood.Positive (Positive (getPositive))
import Elwood.Thinking (ThinkingMode (..))
import Elwood.Tools.Registry
  ( ToolRegistry,
    lookupTool,
    toolSchemas,
  )
import Elwood.Tools.Types (ApprovalFunction, ApprovalOutcome (..), FailureMode (..), Tool (..), ToolResult (..), failureTag, imageResultPart)

-- | Result of running an agent turn
data AgentResult
  = -- | Success with response text and all messages (for persistence)
    AgentSuccess Text [ClaudeMessage]
  | -- | Iteration cap hit before end_turn. Carries best-effort partial output
    -- so the caller can salvage it rather than discard the turn.
    AgentExhausted ExhaustionInfo
  | -- | Error that should be shown to user
    AgentError Text
  | -- | Cancelled by user via /stop. The turn is discarded.
    AgentCancelled
  deriving stock (Show)

-- | Partial result returned when the agent loop hits 'maxIterations'.
data ExhaustionInfo = ExhaustionInfo
  { partialOutput :: Text,
    iterationsUsed :: Int,
    toolsCalled :: Int,
    messages :: [ClaudeMessage]
  }
  deriving stock (Show)

-- | Configuration for the agent loop (all environment/config params)
data AgentConfig = AgentConfig
  { logger :: Logger,
    client :: ClaudeClient,
    registry :: ToolRegistry,
    requestApproval :: ApprovalFunction,
    systemPrompt :: Maybe Text,
    agentProfile :: AgentProfile,
    -- | Observer for metrics and telemetry
    observer :: AgentObserver,
    -- | Optional callback for rate limit notifications
    onRateLimit :: Maybe RateLimitCallback,
    -- | Optional callback for intermediate text produced during tool-use turns
    onText :: Maybe TextCallback,
    -- | Optional callback for tool use notifications
    onToolUse :: Maybe ToolUseCallback,
    -- | Optional callback fired before each API call (e.g., typing indicator)
    onBeforeApiCall :: Maybe (IO ()),
    -- | Tool search (Nothing = disabled, Just neverDefer = enabled with deferred loading)
    toolSearch :: Maybe (Set ToolName),
    -- | Pruning configuration (Nothing = disabled)
    pruningConfig :: Maybe PruningConfig,
    -- | Prune horizon: tool results before this index are replaced with placeholders
    pruneHorizon :: Int,
    -- | Structured output format (e.g., JSON schema constraint).
    -- Safe to pass on every iteration: the API only applies format
    -- constraints when stop_reason is end_turn, not during tool_use.
    outputFormat :: Maybe OutputFormat,
    -- | Maximum dimension for tool-result images (Nothing = no resizing).
    -- Perception constraints are enforced centrally here so individual
    -- tools can emit image parts without caring about API limits.
    maxImageDimension :: Maybe Int,
    -- | Check whether the turn has been cancelled (e.g. by /stop).
    -- The loop checks this at each safe point and exits if True.
    isCancelled :: IO Bool
  }

-- | Convert a thinking mode to the API thinking config
thinkingToConfig :: Maybe ThinkingMode -> Maybe ThinkingConfig
thinkingToConfig Nothing = Nothing
thinkingToConfig (Just (Adaptive effort)) = Just (ThinkingConfigAdaptive effort)
thinkingToConfig (Just (Budget n)) = Just (ThinkingConfigBudget n)

-- | Run a complete agent turn, handling tool use loops.
-- Returns only the new messages produced during this turn (the delta),
-- not the full conversation history.
runAgentTurn ::
  AgentConfig ->
  -- | Existing conversation history
  [ClaudeMessage] ->
  -- | New user message
  ClaudeMessage ->
  IO AgentResult
runAgentTurn cfg history userMessage = do
  let msgs = history ++ [userMessage]
      histLen = length history
  result <- agentLoop cfg msgs 0
  pure $ case result of
    AgentSuccess text allMsgs -> AgentSuccess text (drop histLen allMsgs)
    AgentExhausted info ->
      AgentExhausted
        ExhaustionInfo
          { partialOutput = info.partialOutput,
            iterationsUsed = info.iterationsUsed,
            toolsCalled = info.toolsCalled,
            messages = drop histLen info.messages
          }
    AgentCancelled -> AgentCancelled
    err -> err

-- | The main agent loop
agentLoop ::
  AgentConfig ->
  [ClaudeMessage] ->
  Int ->
  IO AgentResult
agentLoop cfg msgs iteration
  | iteration >= cfg.agentProfile.maxIterations.getPositive = do
      let info =
            ExhaustionInfo
              { partialOutput = lastAssistantText msgs,
                iterationsUsed = iteration,
                toolsCalled = countToolUses msgs,
                messages = msgs
              }
      logError
        cfg.logger
        "Agent loop exceeded max iterations"
        [ ("iterations", T.pack (show info.iterationsUsed)),
          ("tools_called", T.pack (show info.toolsCalled)),
          ("partial_output_length", T.pack (show (T.length info.partialOutput)))
        ]
      pure $ AgentExhausted info
  | otherwise = do
      -- Check cancellation before starting a new iteration
      cancelled <- cfg.isCancelled
      if cancelled
        then do
          logInfo cfg.logger "Agent loop cancelled by user" []
          pure AgentCancelled
        else agentLoopIteration cfg msgs iteration

-- | Run a single iteration of the agent loop (API call + tool execution)
agentLoopIteration ::
  AgentConfig ->
  [ClaudeMessage] ->
  Int ->
  IO AgentResult
agentLoopIteration cfg msgs iteration = do
  let lgr = cfg.logger
      reg = cfg.registry
      mdl = cfg.agentProfile.model.model
      thk = cfg.agentProfile.thinking

  -- Send the schemas in the (already availability-filtered) registry. Tool
  -- search, if enabled, marks a subset deferred server-side.
  let schemas = toolSchemas reg
      prunedMsgs = case cfg.pruningConfig of
        Nothing -> msgs
        Just pcfg ->
          let pruneTools = case pcfg.tools of
                Nothing -> id
                Just tc -> pruneToolInputs tc cfg.pruneHorizon . pruneToolResults tc cfg.pruneHorizon
           in pruneThinkingBlocks pcfg.thinking cfg.pruneHorizon . pruneTools $ msgs

  -- Record estimated input token breakdown
  cfg.observer.onInputEstimate cfg.systemPrompt cfg.toolSearch prunedMsgs schemas

  -- Build and send request
  let request =
        MessagesRequest
          { model = mdl,
            maxTokens = cfg.agentProfile.maxTokens.getPositive,
            system = cfg.systemPrompt,
            messages = prunedMsgs,
            tools = schemas,
            thinking = thinkingToConfig thk,
            cacheControl = cfg.agentProfile.cache,
            toolSearch = cfg.toolSearch,
            outputFormat = cfg.outputFormat
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

  -- Fire before-call callback (e.g., typing indicator)
  sequence_ cfg.onBeforeApiCall

  result <- sendMessagesWithRetry cfg.client retryConfig cfg.agentProfile.model.provider request

  case result of
    Left err -> do
      logError lgr "Claude API error" [("error", T.pack (show err))]
      pure $ AgentError $ formatError err
    Right response -> do
      let u = response.usage
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
      cfg.observer.onApiResponse response.stopReason response.usage

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
  let ids = map extractToolUseId toolUses
      resultBlocks = zipWith makeResultBlock ids toolResults
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

      -- Check cancellation before executing tools
      cancelledBeforeTools <- cfg.isCancelled
      if cancelledBeforeTools
        then do
          logInfo lgr "Agent loop cancelled before tool execution" []
          pure AgentCancelled
        else do
          logInfo lgr "Tool use requested" [("tool_count", T.pack (show (length toolUses)))]

          -- Send any intermediate text to the user immediately
          case cfg.onText of
            Just cb | not (T.null intermediateText) -> cb intermediateText
            _ -> pure ()

          -- Notify about tool use
          let toolCalls = [(tn, input) | ToolUseBlock _ (ToolName tn) input <- toolUses]
          case cfg.onToolUse of
            Just cb | not (null toolCalls) -> cb iteration toolCalls
            _ -> pure ()

          -- Record tool call metrics
          mapM_ (\case ToolUseBlock _ (ToolName tn) _ -> cfg.observer.onToolCall tn; _ -> pure ()) toolUses

          -- Execute all tool uses, then enforce perception constraints on
          -- any image parts (resize, media type and size limits)
          toolResults <-
            map (perceiveResultImages cfg.maxImageDimension)
              <$> executeToolUses lgr reg cfg.agentProfile.permissions cfg.requestApproval toolUses

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
  PermissionConfig ->
  ApprovalFunction ->
  [ContentBlock] ->
  IO [ToolResult]
executeToolUses lgr reg perms approve = mapConcurrently execSafe
  where
    execSafe block =
      executeToolUse lgr reg perms approve block `catchSync` \(e :: SomeException) -> do
        logError lgr "Tool execution threw exception" [("error", T.pack (show e))]
        pure $ ToolError $ "Tool execution failed: " <> T.take 500 (T.pack (show e))

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

lastAssistantText :: [ClaudeMessage] -> Text
lastAssistantText msgs =
  case [extractTextContent c | ClaudeMessage Assistant c <- reverse msgs] of
    (t : _) -> t
    [] -> ""

countToolUses :: [ClaudeMessage] -> Int
countToolUses msgs =
  sum [length (extractToolUses c) | ClaudeMessage _ c <- msgs]

-- | JSON shape used as the delegate's tool_error body on exhaustion;
-- documented in issue #49 so the orchestrator can branch on @status@.
formatExhaustion :: ExhaustionInfo -> Text
formatExhaustion info =
  decodeUtf8 . LBS.toStrict $
    encode $
      object
        [ "status" .= failureTag MaxIterations,
          "iterations_used" .= info.iterationsUsed,
          "tools_called" .= info.toolsCalled,
          "partial_output" .= info.partialOutput
        ]

-- | Execute a single tool use with policy checking
executeToolUse :: Logger -> ToolRegistry -> PermissionConfig -> ApprovalFunction -> ContentBlock -> IO ToolResult
executeToolUse lgr reg perms approve (ToolUseBlock tid n input) = do
  let ToolName tn = n
      ToolUseId ti = tid
  logInfo lgr "Executing tool" [("tool", tn), ("id", ti)]

  case lookupTool n reg of
    Nothing -> do
      logWarn lgr "Unknown tool" [("tool", tn)]
      pure $ ToolError $ "Unknown tool: " <> tn
    Just tool -> do
      -- Check tool policy before execution
      let policy = getToolPolicy perms n

      case policy of
        PolicyDeny -> do
          logWarn lgr "Tool denied by policy" [("tool", tn)]
          pure $ ToolError $ "Tool '" <> tn <> "' is not allowed by policy"
        PolicyAllow -> do
          executeToolWithLogging lgr tool tn input
        PolicyAsk -> do
          -- Request approval before execution
          logInfo lgr "Requesting approval for tool" [("tool", tn)]
          let inputSummary = T.take 200 (decodeUtf8 (LBS.toStrict (encode input)))
          outcome <- approve n inputSummary
          case outcome of
            ApprovalGranted -> do
              logInfo lgr "Tool approved by user" [("tool", tn)]
              executeToolWithLogging lgr tool tn input
            ApprovalDenied -> do
              logInfo lgr "Tool denied by user" [("tool", tn)]
              pure $ ToolError "Action denied by user"
            ApprovalTimeout -> do
              logWarn lgr "Tool approval timed out" [("tool", tn)]
              pure $ ToolError "Approval request timed out"
            ApprovalUnavailable -> do
              logWarn lgr "No approval mechanism, denying tool" [("tool", tn)]
              pure $ ToolError $ "Tool '" <> tn <> "' requires approval but no approval channel is available"
executeToolUse _ _ _ _ _ = pure $ ToolError "Invalid tool use block"

-- | Execute a tool and log the result
executeToolWithLogging :: Logger -> Tool -> Text -> Value -> IO ToolResult
executeToolWithLogging lgr tool n input = do
  result <- tool.execute input
  case result of
    ToolSuccess parts ->
      logInfo
        lgr
        "Tool succeeded"
        [ ("tool", n),
          ("output_length", T.pack (show (T.length (toolResultText parts)))),
          ("images", T.pack (show (length [() | ToolResultImage _ _ <- parts])))
        ]
    ToolError err ->
      logWarn lgr "Tool failed" [("tool", n), ("error", err)]
  pure result

-- | Enforce perception constraints on the image parts of a tool result:
-- detect the actual format (declared media types can lie), resize to the
-- configured max dimension, and enforce the API's per-image size limit.
-- Invalid images degrade to a short text part rather than poisoning the
-- API request. Base64 is decoded leniently (whitespace-wrapped input from
-- tools is fine); garbage input fails format detection and is elided.
perceiveResultImages :: Maybe Int -> ToolResult -> ToolResult
perceiveResultImages maxDim (ToolSuccess parts) = ToolSuccess (map perceivePart parts)
  where
    perceivePart (ToolResultImage _declaredMt b64) =
      case perceiveImageBytes maxDim (B64.decodeLenient (encodeUtf8 b64)) of
        Left err -> ToolResultText $ "[image elided: " <> err <> "]"
        Right img -> imageResultPart img
    perceivePart part = part
perceiveResultImages _ err = err

-- | Extract the tool use ID from a ToolUseBlock
extractToolUseId :: ContentBlock -> ToolUseId
extractToolUseId (ToolUseBlock tid _ _) = tid
extractToolUseId _ = ToolUseId "unknown"

-- | Make a tool result block from a tool use ID and its result
makeResultBlock :: ToolUseId -> ToolResult -> ContentBlock
makeResultBlock tid (ToolSuccess parts) = ToolResultBlock tid parts False
makeResultBlock tid (ToolError err) = ToolResultBlock tid [ToolResultText err] True

-- | Format an error for user display using the notify format
formatError :: ClaudeError -> Text
formatError (ClaudeRateLimited retryAfter) =
  formatNotify Error $ "**Rate limited:** `" <> retryMsg retryAfter <> "`"
formatError (ClaudeOverloaded retryAfter) =
  formatNotify Error $ "**Overloaded:** `" <> retryMsg retryAfter <> "`"
formatError (ClaudeApiError errType errMsg) =
  formatNotify Error $ "**API error:** `" <> sanitizeBackticks (errType <> " — " <> errMsg) <> "`"
formatError (ClaudeHttpError status body) =
  formatNotify Error $ "**HTTP " <> T.pack (show status) <> ":** `" <> sanitizeBackticks (T.take 200 body) <> "`"
formatError (ClaudeParseError err) =
  formatNotify Error $ "**Parse error:** `" <> sanitizeBackticks (T.pack err) <> "`"
formatError (ClaudeUnknownProvider name) =
  formatNotify Error $ "**Unknown provider:** `" <> sanitizeBackticks name <> "`"

-- | Format retry-after information
retryMsg :: Maybe Int -> Text
retryMsg = maybe "no retry-after" (\secs -> "retry after " <> T.pack (show secs) <> "s")
