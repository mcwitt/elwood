{-# LANGUAGE StrictData #-}

module Elwood.Claude.AgentLoop
  ( runAgentTurn,
    AgentResult (..),
    RateLimitCallback,
  )
where

import Data.Aeson (Value, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Elwood.Claude.Client (ClaudeClient, RetryConfig (..), defaultRetryConfig, sendMessagesWithRetry)
import Elwood.Claude.Compaction (CompactionConfig, compactIfNeeded)
import Elwood.Claude.Types
import Elwood.Config (ThinkingLevel (..))
import Elwood.Logging (Logger, logError, logInfo, logWarn)
import Elwood.Metrics (MetricsStore, recordApiResponse, recordToolCall)
import Elwood.Permissions (ToolPolicy (..), getToolPolicy)
import Elwood.Tools.Registry (ToolRegistry, lookupTool, toolSchemas)
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

-- | Convert a thinking level to the API thinking config
thinkingToConfig :: ThinkingLevel -> Maybe ThinkingConfig
thinkingToConfig ThinkingOff = Nothing
thinkingToConfig ThinkingLow = Just (ThinkingConfig 1024)
thinkingToConfig ThinkingMedium = Just (ThinkingConfig 4096)
thinkingToConfig ThinkingHigh = Just (ThinkingConfig 16384)

-- | Get the max_tokens value for a thinking level
thinkingMaxTokens :: ThinkingLevel -> Int
thinkingMaxTokens ThinkingOff = 4096
thinkingMaxTokens ThinkingLow = 4096
thinkingMaxTokens ThinkingMedium = 8192
thinkingMaxTokens ThinkingHigh = 32768

-- | Run a complete agent turn, handling tool use loops
runAgentTurn ::
  Logger ->
  ClaudeClient ->
  ToolRegistry ->
  AgentContext ->
  CompactionConfig ->
  -- | System prompt
  Maybe Text ->
  -- | Model name
  Text ->
  -- | Extended thinking level
  ThinkingLevel ->
  -- | Maximum agent loop iterations
  Int ->
  -- | Metrics store
  MetricsStore ->
  -- | Source label for metrics
  Text ->
  -- | Existing conversation history
  [ClaudeMessage] ->
  -- | New user message
  ClaudeMessage ->
  -- | Optional callback for rate limit notifications
  Maybe RateLimitCallback ->
  IO AgentResult
runAgentTurn logger client registry ctx compactionConfig systemPrompt model thinking maxIter metrics source history userMessage onRateLimit = do
  -- Compact history if needed before adding new message
  compactedHistory <- compactIfNeeded logger client compactionConfig metrics source history
  let messages = compactedHistory ++ [userMessage]
  agentLoop logger client registry ctx compactionConfig systemPrompt model thinking maxIter metrics source messages 0 onRateLimit

-- | The main agent loop
agentLoop ::
  Logger ->
  ClaudeClient ->
  ToolRegistry ->
  AgentContext ->
  CompactionConfig ->
  Maybe Text ->
  Text ->
  ThinkingLevel ->
  Int ->
  MetricsStore ->
  Text ->
  [ClaudeMessage] ->
  Int ->
  Maybe RateLimitCallback ->
  IO AgentResult
agentLoop logger client registry ctx compactionConfig systemPrompt model thinking maxIter metrics source messages iteration onRateLimit
  | iteration >= maxIter = do
      logError logger "Agent loop exceeded max iterations" []
      pure $ AgentError "(Agent loop exceeded max iterations)"
  | otherwise = do
      -- Build and send request
      let request =
            MessagesRequest
              { mrModel = model,
                mrMaxTokens = thinkingMaxTokens thinking,
                mrSystem = systemPrompt,
                mrMessages = messages,
                mrTools = toolSchemas registry,
                mrThinking = thinkingToConfig thinking
              }

      logInfo
        logger
        "Sending request to Claude"
        [ ("iteration", T.pack (show iteration)),
          ("message_count", T.pack (show (length messages)))
        ]

      -- Configure retry with logging callback and optional user notification
      let retryConfig =
            defaultRetryConfig
              { rcOnRetry = Just $ \attemptNum waitSecs err -> do
                  logWarn
                    logger
                    "Rate limited, retrying"
                    [ ("attempt", T.pack (show attemptNum)),
                      ("wait_seconds", T.pack (show waitSecs)),
                      ("error", T.pack (show err))
                    ]
                  -- Notify user if callback is configured
                  case onRateLimit of
                    Just notify -> notify attemptNum waitSecs
                    Nothing -> pure ()
              }

      result <- sendMessagesWithRetry client retryConfig request

      case result of
        Left err -> do
          logError logger "Claude API error" [("error", T.pack (show err))]
          pure $ AgentError $ formatError err
        Right response -> do
          logInfo
            logger
            "Claude response received"
            [ ("stop_reason", fromMaybe "none" (mresStopReason response)),
              ("content_blocks", T.pack (show (length (mresContent response))))
            ]

          -- Record API metrics
          recordApiResponse metrics model source (fromMaybe "unknown" (mresStopReason response)) (mresUsage response)

          handleResponse logger client registry ctx compactionConfig systemPrompt model thinking maxIter metrics source messages response iteration onRateLimit

-- | Handle Claude's response
handleResponse ::
  Logger ->
  ClaudeClient ->
  ToolRegistry ->
  AgentContext ->
  CompactionConfig ->
  Maybe Text ->
  Text ->
  ThinkingLevel ->
  Int ->
  MetricsStore ->
  Text ->
  [ClaudeMessage] ->
  MessagesResponse ->
  Int ->
  Maybe RateLimitCallback ->
  IO AgentResult
handleResponse logger client registry ctx compactionConfig systemPrompt model thinking maxIter metrics source messages response iteration onRateLimit =
  case mresStopReason response of
    Just "end_turn" -> do
      -- Normal completion - extract text and return
      let responseText = extractTextContent (mresContent response)
          assistantMsg = ClaudeMessage Assistant (mresContent response)
          allMessages = messages ++ [assistantMsg]
      pure $ AgentSuccess responseText allMessages
    Just "tool_use" -> do
      -- Tool use requested - execute tools and continue
      let toolUses = extractToolUses (mresContent response)

      logInfo
        logger
        "Tool use requested"
        [("tool_count", T.pack (show (length toolUses)))]

      -- Record tool call metrics
      mapM_ (\case ToolUseBlock _ name _ -> recordToolCall metrics name; _ -> pure ()) toolUses

      -- Execute all tool uses
      toolResults <- mapM (executeToolUse logger registry ctx) toolUses

      -- Build messages for next iteration
      let assistantMsg = ClaudeMessage Assistant (mresContent response)
          resultBlocks = zipWith makeResultBlock toolUses toolResults
          userMsg = ClaudeMessage User resultBlocks
          newMessages = messages ++ [assistantMsg, userMsg]

      -- Continue the loop
      agentLoop logger client registry ctx compactionConfig systemPrompt model thinking maxIter metrics source newMessages (iteration + 1) onRateLimit
    Just "max_tokens" -> do
      -- Hit token limit - return what we have
      let responseText = extractTextContent (mresContent response)
          assistantMsg = ClaudeMessage Assistant (mresContent response)
          allMessages = messages ++ [assistantMsg]
      logWarn logger "Response hit max tokens" []
      pure $ AgentSuccess (responseText <> "\n\n(Response was truncated due to length)") allMessages
    other -> do
      -- Unknown stop reason
      logWarn logger "Unknown stop reason" [("reason", fromMaybe "null" other)]
      let responseText = extractTextContent (mresContent response)
          assistantMsg = ClaudeMessage Assistant (mresContent response)
          allMessages = messages ++ [assistantMsg]
      pure $ AgentSuccess responseText allMessages

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
executeToolUse logger registry ctx (ToolUseBlock tid name input) = do
  logInfo logger "Executing tool" [("tool", name), ("id", tid)]

  case lookupTool name registry of
    Nothing -> do
      logWarn logger "Unknown tool" [("tool", name)]
      pure $ ToolError $ "Unknown tool: " <> name
    Just tool -> do
      -- Check tool policy before execution
      let policy = getToolPolicy (acPermissionConfig ctx) name

      case policy of
        PolicyDeny -> do
          logWarn logger "Tool denied by policy" [("tool", name)]
          pure $ ToolError $ "Tool '" <> name <> "' is not allowed by policy"
        PolicyAllow -> do
          executeToolWithLogging logger tool name input
        PolicyAsk -> do
          -- Request approval before execution
          case acRequestApproval ctx of
            Nothing -> do
              -- No approval mechanism configured, fall back to allow
              logWarn logger "No approval mechanism, allowing tool" [("tool", name)]
              executeToolWithLogging logger tool name input
            Just requestApproval -> do
              logInfo logger "Requesting approval for tool" [("tool", name)]
              let inputSummary = T.take 200 (decodeUtf8 (LBS.toStrict (encode input)))
              result <- requestApproval name inputSummary
              case result of
                ApprovalGranted -> do
                  logInfo logger "Tool approved by user" [("tool", name)]
                  executeToolWithLogging logger tool name input
                ApprovalDenied -> do
                  logInfo logger "Tool denied by user" [("tool", name)]
                  pure $ ToolError "Action denied by user"
                ApprovalTimeout -> do
                  logWarn logger "Tool approval timed out" [("tool", name)]
                  pure $ ToolError "Approval request timed out"
executeToolUse _ _ _ _ = pure $ ToolError "Invalid tool use block"

-- | Execute a tool and log the result
executeToolWithLogging :: Logger -> Tool -> Text -> Value -> IO ToolResult
executeToolWithLogging logger tool name input = do
  result <- toolExecute tool input
  case result of
    ToolSuccess output ->
      logInfo logger "Tool succeeded" [("tool", name), ("output_length", T.pack (show (T.length output)))]
    ToolError err ->
      logWarn logger "Tool failed" [("tool", name), ("error", err)]
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
