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
    -- | Always-loaded tools (Nothing = all tools, Just = dynamic loading with these tools)
    alwaysLoadTools :: Maybe [Text],
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
  let msgs = compactedHistory ++ [userMessage]

  -- Initialize the active tool set
  let activeTools = case cfg.alwaysLoadTools of
        Nothing -> Nothing -- disabled: send all tools
        Just alwaysLoad -> Just $ Set.fromList ("find_tools" : alwaysLoad)

  agentLoop cfg msgs 0 activeTools

-- | The main agent loop
agentLoop ::
  AgentConfig ->
  [ClaudeMessage] ->
  Int ->
  -- | Active tool set (Nothing = send all tools, Just = dynamic loading)
  Maybe (Set Text) ->
  IO AgentResult
agentLoop cfg msgs iteration mActiveTools
  | iteration >= cfg.maxIterations = do
      logError cfg.logger "Agent loop exceeded max iterations" []
      pure $ AgentError "(Agent loop exceeded max iterations)"
  | otherwise = do
      let lgr = cfg.logger
          reg = cfg.registry
          mdl = cfg.model
          thk = cfg.thinking

      -- Determine which tool schemas to send
      let schemas = case mActiveTools of
            Nothing -> toolSchemas reg
            Just active ->
              findToolsSchema : activeToolSchemas reg active

      -- Build and send request
      let request =
            MessagesRequest
              { model = mdl,
                maxTokens = thinkingMaxTokens thk,
                system = cfg.systemPrompt,
                messages = pruneToolResults cfg.pruneHorizon msgs,
                tools = schemas,
                thinking = thinkingToConfig thk,
                cacheControl = True
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

          handleResponse cfg msgs response iteration mActiveTools

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
handleResponse cfg msgs response iteration mActiveTools =
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

      -- Execute all tool uses, handling find_tools as a built-in
      (toolResults, updatedActiveTools) <- executeToolUses lgr reg cfg.context mActiveTools toolUses

      -- Build messages for next iteration (pure)
      let nextIteration = iteration + 1
          userMsg = buildToolResultMessage toolUses toolResults nextIteration cfg.maxIterations
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
executeToolUses lgr reg ctx mActiveTools toolUses = do
  go mActiveTools toolUses []
  where
    go active [] acc = pure (reverse acc, active)
    go active (tu : rest) acc = do
      case tu of
        ToolUseBlock _ "find_tools" input | Just _ <- active -> do
          -- Handle find_tools as built-in
          let result = handleFindTools reg input
              newActive = case result of
                ToolSuccess _ ->
                  -- Extract the query and activate matching tools
                  case extractQuery input of
                    Just query ->
                      let (_text, names) = searchTools reg query
                       in fmap (`Set.union` names) active
                    Nothing -> active
                ToolError _ -> active
          logInfo lgr "Executing tool" [("tool", "find_tools")]
          go newActive rest (result : acc)
        _ -> do
          result <- executeToolUse lgr reg ctx tu
          go active rest (result : acc)

-- | Handle find_tools as a built-in (pure search, no IORef)
handleFindTools :: ToolRegistry -> Value -> ToolResult
handleFindTools reg input =
  case extractQuery input of
    Nothing -> ToolError "Missing required parameter: query"
    Just query ->
      let (txt, _names) = searchTools reg query
       in ToolSuccess txt

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
    { name = "find_tools",
      description =
        "Search for available tools by keyword (multi-word queries use AND matching). "
          <> "Returns matching tool names and descriptions, and makes them available for use. "
          <> "Use when you need a tool that isn't already available.",
      inputSchema =
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
