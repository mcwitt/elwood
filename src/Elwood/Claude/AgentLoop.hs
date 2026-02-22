{-# LANGUAGE StrictData #-}

module Elwood.Claude.AgentLoop
  ( runAgentTurn,
    AgentResult (..),
    RateLimitCallback,
  )
where

import Data.Aeson (Value, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Elwood.Claude.Client (ClaudeClient, RetryConfig (..), defaultRetryConfig, sendMessagesWithRetry)
import Elwood.Claude.Compaction (CompactionConfig, compactIfNeeded)
import Elwood.Claude.Types
import Elwood.Config (DynamicToolLoadingConfig (..), ThinkingEffort (..), ThinkingLevel (..))
import Elwood.Logging (Logger, logError, logInfo, logWarn)
import Elwood.Metrics (MetricsStore, recordApiResponse, recordToolCall)
import Elwood.Permissions (ToolPolicy (..), getToolPolicy)
import Elwood.Tools.Meta (mkFindToolsTool)
import Elwood.Tools.Registry
  ( ActiveToolSet,
    ToolRegistry,
    activateTool,
    activeToolSchemas,
    lookupTool,
    newActiveToolSet,
    registerTool,
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
  -- | Dynamic tool loading config (Nothing = disabled, Just cfg = enabled)
  Maybe DynamicToolLoadingConfig ->
  IO AgentResult
runAgentTurn logger client registry ctx compactionConfig systemPrompt model thinking maxIter metrics source history userMessage onRateLimit dynamicLoading = do
  -- Compact history if needed before adding new message
  compactedHistory <- compactIfNeeded logger client compactionConfig metrics source history
  let messages = compactedHistory ++ [userMessage]

  -- Set up dynamic tool loading state if enabled
  mActiveRef <- case dynamicLoading of
    Nothing -> pure Nothing
    Just dtlConfig -> do
      let initialActive =
            foldr activateTool newActiveToolSet $
              "find_tools" : dtlAlwaysLoad dtlConfig
      activeRef <- newIORef initialActive
      let findTool = mkFindToolsTool registry activeRef
          registryWithMeta = registerTool findTool registry
      pure $ Just (activeRef, registryWithMeta)

  let effectiveRegistry = case mActiveRef of
        Nothing -> registry
        Just (_, regWithMeta) -> regWithMeta
      mActive = fmap fst mActiveRef

  agentLoop logger client effectiveRegistry ctx compactionConfig systemPrompt model thinking maxIter metrics source messages 0 onRateLimit mActive

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
  -- | Active tool set IORef (Nothing = send all tools)
  Maybe (IORef ActiveToolSet) ->
  IO AgentResult
agentLoop logger client registry ctx compactionConfig systemPrompt model thinking maxIter metrics source messages iteration onRateLimit mActiveRef
  | iteration >= maxIter = do
      logError logger "Agent loop exceeded max iterations" []
      pure $ AgentError "(Agent loop exceeded max iterations)"
  | otherwise = do
      -- Determine which tool schemas to send
      schemas <- case mActiveRef of
        Nothing -> pure $ toolSchemas registry
        Just activeRef -> do
          ats <- readIORef activeRef
          pure $ activeToolSchemas registry ats

      -- Build and send request
      let request =
            MessagesRequest
              { mrModel = model,
                mrMaxTokens = thinkingMaxTokens thinking,
                mrSystem = systemPrompt,
                mrMessages = messages,
                mrTools = schemas,
                mrThinking = thinkingToConfig thinking,
                mrCacheControl = True
              }

      logInfo
        logger
        "Sending request to Claude"
        [ ("iteration", T.pack (show iteration)),
          ("message_count", T.pack (show (length messages))),
          ("tool_count", T.pack (show (length schemas)))
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
          let usage = mresUsage response
          logInfo
            logger
            "Claude response received"
            [ ("stop_reason", fromMaybe "none" (mresStopReason response)),
              ("content_blocks", T.pack (show (length (mresContent response)))),
              ("input_tokens", T.pack (show (usageInputTokens usage))),
              ("output_tokens", T.pack (show (usageOutputTokens usage))),
              ("cache_read_tokens", T.pack (show (usageCacheReadInputTokens usage))),
              ("cache_creation_tokens", T.pack (show (usageCacheCreationInputTokens usage)))
            ]

          -- Record API metrics
          recordApiResponse metrics model source (fromMaybe "unknown" (mresStopReason response)) (mresUsage response)

          handleResponse logger client registry ctx compactionConfig systemPrompt model thinking maxIter metrics source messages response iteration onRateLimit mActiveRef

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
  Maybe (IORef ActiveToolSet) ->
  IO AgentResult
handleResponse logger client registry ctx compactionConfig systemPrompt model thinking maxIter metrics source messages response iteration onRateLimit mActiveRef =
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
      let nextIteration = iteration + 1
          assistantMsg = ClaudeMessage Assistant (mresContent response)
          resultBlocks = zipWith makeResultBlock toolUses toolResults
          turnInfo = TextBlock $ formatTurnInfo nextIteration maxIter
          userMsg = ClaudeMessage User (resultBlocks ++ [turnInfo])
          newMessages = messages ++ [assistantMsg, userMsg]

      -- Continue the loop
      agentLoop logger client registry ctx compactionConfig systemPrompt model thinking maxIter metrics source newMessages nextIteration onRateLimit mActiveRef
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

-- | Format turn info with adaptive messaging based on proximity to limit.
--
-- Uses a @\<harness\>@ XML tag to distinguish from user content.
-- Escalates urgency as the turn limit approaches.
formatTurnInfo :: Int -> Int -> Text
formatTurnInfo turn maxTurn =
  let remaining = maxTurn - turn
      prefix = "Turn " <> T.pack (show turn) <> " of " <> T.pack (show maxTurn) <> "."
      body
        | remaining <= 2 = prefix <> " Final turns — respond now, do not make further tool calls."
        | remaining <= 5 = prefix <> " Approaching limit — wrap up soon."
        | otherwise = prefix
   in "<harness>" <> body <> "</harness>"

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
