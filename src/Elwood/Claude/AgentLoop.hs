{-# LANGUAGE StrictData #-}

module Elwood.Claude.AgentLoop
  ( runAgentTurn,
    AgentConfig (..),
    AgentResult (..),
    RateLimitCallback,
    TextCallback,
  )
where

import Data.Aeson (Value (..), encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Elwood.Claude.Client (ClaudeClient, RetryConfig (..), defaultRetryConfig, sendMessagesWithRetry)
import Elwood.Claude.Compaction (CompactionConfig, compactIfNeeded)
import Elwood.Claude.Types
  ( ClaudeError (..),
    ClaudeMessage (..),
    ContentBlock (..),
    MessagesRequest (..),
    MessagesResponse (..),
    Role (..),
    StopReason (..),
    ThinkingConfig (..),
    ToolSchema (..),
    Usage (..),
    stopReasonToText,
  )
import Elwood.Config (ThinkingEffort (..), ThinkingLevel (..))
import Elwood.Logging (Logger, logError, logInfo, logWarn)
import Elwood.Metrics (MetricsStore, recordApiResponse, recordToolCall)
import Elwood.Permissions (ToolPolicy (..), getToolPolicy)
import Elwood.Tools.Meta (searchTools)
import Elwood.Tools.Registry
  ( ToolRegistry,
    activeToolSchemas,
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
  { acLogger :: Logger,
    acClient :: ClaudeClient,
    acRegistry :: ToolRegistry,
    acContext :: AgentContext,
    acCompaction :: CompactionConfig,
    acSystemPrompt :: Maybe Text,
    acModel :: Text,
    acThinking :: ThinkingLevel,
    acMaxIterations :: Int,
    acMetrics :: MetricsStore,
    -- | Source label for metrics
    acSource :: Text,
    -- | Optional callback for rate limit notifications
    acOnRateLimit :: Maybe RateLimitCallback,
    -- | Optional callback for intermediate text produced during tool-use turns
    acOnText :: Maybe TextCallback,
    -- | Always-loaded tools (Nothing = all tools, Just = dynamic loading with these tools)
    acAlwaysLoadTools :: Maybe [Text]
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
  compactedHistory <- compactIfNeeded (acLogger cfg) (acClient cfg) (acCompaction cfg) (acMetrics cfg) (acSource cfg) history
  let messages = compactedHistory ++ [userMessage]

  -- Initialize the active tool set
  let activeTools = case acAlwaysLoadTools cfg of
        Nothing -> Nothing -- disabled: send all tools
        Just alwaysLoad -> Just $ Set.fromList ("find_tools" : alwaysLoad)

  agentLoop cfg messages 0 activeTools

-- | The main agent loop
agentLoop ::
  AgentConfig ->
  [ClaudeMessage] ->
  Int ->
  -- | Active tool set (Nothing = send all tools, Just = dynamic loading)
  Maybe (Set Text) ->
  IO AgentResult
agentLoop cfg messages iteration mActiveTools
  | iteration >= acMaxIterations cfg = do
      logError (acLogger cfg) "Agent loop exceeded max iterations" []
      pure $ AgentError "(Agent loop exceeded max iterations)"
  | otherwise = do
      let logger = acLogger cfg
          registry = acRegistry cfg
          model = acModel cfg
          thinking = acThinking cfg

      -- Determine which tool schemas to send
      let schemas = case mActiveTools of
            Nothing -> toolSchemas registry
            Just active ->
              findToolsSchema : activeToolSchemas registry active

      -- Build and send request
      let request =
            MessagesRequest
              { mrModel = model,
                mrMaxTokens = thinkingMaxTokens thinking,
                mrSystem = acSystemPrompt cfg,
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
                  case acOnRateLimit cfg of
                    Just notify -> notify attemptNum waitSecs
                    Nothing -> pure ()
              }

      result <- sendMessagesWithRetry (acClient cfg) retryConfig request

      case result of
        Left err -> do
          logError logger "Claude API error" [("error", T.pack (show err))]
          pure $ AgentError $ formatError err
        Right response -> do
          let usage = mresUsage response
              source = acSource cfg
          logInfo
            logger
            "Claude response received"
            [ ("stop_reason", stopReasonToText (mresStopReason response)),
              ("content_blocks", T.pack (show (length (mresContent response)))),
              ("input_tokens", T.pack (show (usageInputTokens usage))),
              ("output_tokens", T.pack (show (usageOutputTokens usage))),
              ("cache_read_tokens", T.pack (show (usageCacheReadInputTokens usage))),
              ("cache_creation_tokens", T.pack (show (usageCacheCreationInputTokens usage)))
            ]

          -- Record API metrics
          recordApiResponse (acMetrics cfg) model source (mresStopReason response) (mresUsage response)

          handleResponse cfg messages response iteration mActiveTools

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
classifyResponse stopReason content messages =
  let responseText = extractTextContent content
      assistantMsg = ClaudeMessage Assistant content
      allMessages = messages ++ [assistantMsg]
   in case stopReason of
        EndTurn -> Complete responseText allMessages
        ToolUse ->
          let toolUses = extractToolUses content
           in ContinueWithTools toolUses responseText allMessages
        MaxTokens -> TruncatedResponse responseText allMessages
        StopReasonOther _ -> Complete responseText allMessages

-- | Build the user message for the next iteration from tool results.
buildToolResultMessage :: [ContentBlock] -> [ToolResult] -> Int -> Int -> ClaudeMessage
buildToolResultMessage toolUses toolResults nextIteration maxIter =
  let resultBlocks = zipWith makeResultBlock toolUses toolResults
      turnInfo = TextBlock $ formatTurnInfo nextIteration maxIter
   in ClaudeMessage User (resultBlocks ++ [turnInfo])

-- | Handle Claude's response (thin IO wrapper around pure classification)
handleResponse ::
  AgentConfig ->
  [ClaudeMessage] ->
  MessagesResponse ->
  Int ->
  Maybe (Set Text) ->
  IO AgentResult
handleResponse cfg messages response iteration mActiveTools =
  case classifyResponse (mresStopReason response) (mresContent response) messages of
    Complete responseText allMessages ->
      pure $ AgentSuccess responseText allMessages
    TruncatedResponse responseText allMessages -> do
      logWarn (acLogger cfg) "Response hit max tokens" []
      pure $ AgentSuccess (responseText <> "\n\n(Response was truncated due to length)") allMessages
    ContinueWithTools toolUses intermediateText allMessages -> do
      let logger = acLogger cfg
          registry = acRegistry cfg
          metrics = acMetrics cfg

      logInfo logger "Tool use requested" [("tool_count", T.pack (show (length toolUses)))]

      -- Send any intermediate text to the user immediately
      case acOnText cfg of
        Just cb | not (T.null intermediateText) -> cb intermediateText
        _ -> pure ()

      -- Record tool call metrics
      mapM_ (\case ToolUseBlock _ name _ -> recordToolCall metrics name; _ -> pure ()) toolUses

      -- Execute all tool uses, handling find_tools as a built-in
      (toolResults, updatedActiveTools) <- executeToolUses logger registry (acContext cfg) mActiveTools toolUses

      -- Build messages for next iteration (pure)
      let nextIteration = iteration + 1
          userMsg = buildToolResultMessage toolUses toolResults nextIteration (acMaxIterations cfg)
          newMessages = allMessages ++ [userMsg]

      -- Continue the loop with possibly updated active tools
      agentLoop cfg newMessages nextIteration updatedActiveTools

-- | Execute all tool uses, handling find_tools as a built-in.
-- Returns the results and the (possibly updated) active tool set.
executeToolUses ::
  Logger ->
  ToolRegistry ->
  AgentContext ->
  Maybe (Set Text) ->
  [ContentBlock] ->
  IO ([ToolResult], Maybe (Set Text))
executeToolUses logger registry ctx mActiveTools toolUses = do
  go mActiveTools toolUses []
  where
    go active [] acc = pure (reverse acc, active)
    go active (tu : rest) acc = do
      case tu of
        ToolUseBlock _ "find_tools" input | Just _ <- active -> do
          -- Handle find_tools as built-in
          let result = handleFindTools registry input
              newActive = case result of
                ToolSuccess _ ->
                  -- Extract the query and activate matching tools
                  case extractQuery input of
                    Just query ->
                      let (_text, names) = searchTools registry query
                       in fmap (`Set.union` names) active
                    Nothing -> active
                ToolError _ -> active
          logInfo logger "Executing tool" [("tool", "find_tools")]
          go newActive rest (result : acc)
        _ -> do
          result <- executeToolUse logger registry ctx tu
          go active rest (result : acc)

-- | Handle find_tools as a built-in (pure search, no IORef)
handleFindTools :: ToolRegistry -> Value -> ToolResult
handleFindTools registry input =
  case extractQuery input of
    Nothing -> ToolError "Missing required parameter: query"
    Just query ->
      let (text, _names) = searchTools registry query
       in ToolSuccess text

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

-- | Schema for the built-in find_tools tool
findToolsSchema :: ToolSchema
findToolsSchema =
  ToolSchema
    { tsName = "find_tools",
      tsDescription =
        "Search for available tools by keyword (multi-word queries use AND matching). "
          <> "Returns matching tool names and descriptions, and makes them available for use. "
          <> "Use when you need a tool that isn't already available.",
      tsInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "properties"
              .= object
                [ "query"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("Keyword to search for in tool names and descriptions" :: Text)
                      ]
                ],
            "required" .= (["query"] :: [Text])
          ]
    }

-- | Extract query from find_tools input
extractQuery :: Value -> Maybe Text
extractQuery (Object obj) = case KM.lookup (Key.fromText "query") obj of
  Just (String q) -> Just q
  _ -> Nothing
extractQuery _ = Nothing

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
