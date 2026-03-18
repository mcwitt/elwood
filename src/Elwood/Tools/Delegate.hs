module Elwood.Tools.Delegate
  ( mkDelegateTaskTool,
  )
where

import Control.Concurrent.Async qualified as Async
import Control.Exception (SomeException, catch)
import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Elwood.AgentSettings (AgentOverrides (..), AgentPreset (..), AgentProfile (..), CacheOverrides (..), ToolSearchConfig (..), resolveProfile, toOverrides)
import Elwood.Claude.AgentLoop (AgentConfig (..), AgentResult (..), runAgentTurn)
import Elwood.Claude.Client (ClaudeClient)
import Elwood.Claude.Observer (ToolUseCallback)
import Elwood.Claude.Types (ClaudeMessage (..), ContentBlock (..), Role (..), ToolName (..), ToolSchema (..), jsonSchemaFormat)
import Elwood.Config (PruningConfig)
import Elwood.Logging (Logger, logError, logInfo)
import Elwood.Metrics (MetricsStore, metricsObserver)
import Elwood.Notify (truncateText)
import Elwood.Positive (Positive (getPositive))
import Elwood.Prompt (PromptInput (InlineText), assemblePrompt)
import Elwood.Thinking (ThinkingOverrides (..))
import Elwood.Tools.AsyncTask (AsyncTaskStore, TaskId (..), insertTask, storeTtlMicros)
import Elwood.Tools.Command (mkRunCommandTool)
import Elwood.Tools.Registry (ToolRegistry, registerTool)
import Elwood.Tools.Types
import System.Timeout (timeout)

-- | Default overrides for delegate sub-agents.
-- Sets max_iterations to 10 (lower than the parent's default of 20).
-- Other fields (model, thinking, system_prompt, tool_search, permissions) inherit from parent.
delegateDefaults :: AgentOverrides
delegateDefaults = AgentOverrides (Last Nothing) Nothing (Last (Just 10)) (Just (CacheOverrides (Last (Just False)) (Last Nothing))) (Last Nothing) (Last Nothing) (Last Nothing) Nothing

-- | Parsed delegate_task input
data DelegateInput = DelegateInput
  { task :: Text,
    agentName :: Maybe Text,
    overrides :: AgentOverrides,
    outputSchema :: Maybe Value,
    async :: Bool,
    label :: Maybe Text
  }

-- | Construct a delegate_task tool that spawns a sub-agent with an isolated
-- conversation. The sub-agent receives the base tool registry (which does not
-- contain delegate_task), preventing recursive nesting.
mkDelegateTaskTool ::
  Logger ->
  ClaudeClient ->
  ToolRegistry ->
  ApprovalFunction ->
  AgentProfile ->
  Maybe PruningConfig ->
  FilePath ->
  MetricsStore ->
  AgentPreset ->
  Map Text AgentPreset ->
  [Text] ->
  -- | Factory for delegate tool-use callbacks (Nothing = no notifications)
  Maybe (Text -> ToolUseCallback) ->
  -- | Async task store (Nothing = async not available)
  Maybe AsyncTaskStore ->
  Tool
mkDelegateTaskTool logger client baseRegistry approve parentProfile pruning workspace metrics delegateAgentPreset extraAgents allowedModels delegateOnToolUse asyncStore =
  Tool
    { schema =
        ToolSchema
          { name = "delegate_task",
            description =
              "Delegate a task to a sub-agent with its own isolated conversation context. "
                <> "The sub-agent has access to all the same tools (except delegate_task) "
                <> "and runs independently, returning only a text summary. "
                <> "Sub-agents share the same workspace, so they have full access to the filesystem "
                <> "(e.g., reading/writing files, running scripts, using git). Reference existing files "
                <> "by path rather than including their contents in the task description. "
                <> "Use this to keep the main conversation context clean when a task "
                <> "requires many tool calls (e.g., multi-file edits, long research). "
                <> "Pass output_schema to constrain the sub-agent's final response to a JSON schema, "
                <> "making it easy to parse structured results programmatically. "
                <> "Set async: true to run the task in the background and get a task ID back immediately. "
                <> "Use check_task to poll for results later. "
                <> "When using a named agent preset, you can further override individual "
                <> "settings (model, thinking, max_iterations) via tool parameters. "
                <> "Note: when overriding model or thinking, ensure they are compatible "
                <> "(e.g., Haiku does not support adaptive thinking)."
                <> maybe "" (\d -> " Default agent: " <> d <> ".") delegateAgentPreset.description,
            inputSchema = delegateSchema allowedModels extraAgents
          },
      execute = executeDelegateTask
    }
  where
    extraAgentKeys = Map.keys extraAgents

    executeDelegateTask :: Value -> IO ToolResult
    executeDelegateTask input = case parseDelegateInput allowedModels extraAgentKeys input of
      Left err -> pure $ toolError err
      Right di
        | di.async,
          Nothing <- asyncStore ->
            pure $ toolError "Async delegation is not available"
        | otherwise -> do
            -- Layering: parent profile < delegate defaults < config agent < extra agent < tool params
            let extraAgentOvr = maybe mempty (\n -> maybe mempty (.overrides) (Map.lookup n extraAgents)) di.agentName
                baseOverrides = toOverrides parentProfile <> delegateDefaults <> delegateAgentPreset.overrides <> extraAgentOvr
                subProfile = resolveProfile (baseOverrides <> di.overrides)

            logInfo
              logger
              "Delegating task to sub-agent"
              [ ("task_length", T.pack (show (T.length di.task))),
                ("model", subProfile.model),
                ("max_iterations", T.pack (show subProfile.maxIterations.getPositive))
              ]

            -- Assemble sub-agent system prompt from workspace files
            subSystemPrompt <- assemblePrompt workspace subProfile.systemPrompt

            -- Convert sub-profile's tool search to Maybe (Set ToolName)
            let subToolSearch = case subProfile.toolSearch of
                  ToolSearchDisabled -> Nothing
                  ToolSearchEnabled names -> Just (Set.fromList (map ToolName names))

            -- Re-register run_command with sub-profile's permissions (so delegate
            -- permission overrides affect command-pattern checking)
            let subRunCmd = mkRunCommandTool logger workspace subProfile.permissions
                subRegistry = registerTool subRunCmd baseRegistry

            let resolvedLabel = fromMaybe di.task di.label
                notifyLabel = truncateText 20 resolvedLabel
                outputFmt = fmap jsonSchemaFormat di.outputSchema
                subConfig =
                  AgentConfig
                    { logger = logger,
                      client = client,
                      registry = subRegistry,
                      requestApproval = approve,
                      systemPrompt = subSystemPrompt,
                      agentProfile = subProfile,
                      observer = metricsObserver metrics subProfile.model "delegate",
                      onRateLimit = Nothing,
                      onText = Nothing,
                      onToolUse = fmap (\f -> f notifyLabel) delegateOnToolUse,
                      onBeforeApiCall = Nothing,
                      toolSearch = subToolSearch,
                      pruningConfig = pruning,
                      pruneHorizon = 0,
                      outputFormat = outputFmt,
                      isCancelled = pure False
                    }
                userMsg = ClaudeMessage User [TextBlock di.task]

            let runWithCatch =
                  runAgentTurn subConfig [] userMsg
                    `catch` \(e :: SomeException) -> do
                      logError logger "Delegate sub-agent error" [("error", T.pack (show e))]
                      pure $ AgentError $ "Sub-agent error: " <> T.pack (show e)

            case (di.async, asyncStore) of
              (True, Just store) -> do
                taskId <- TaskId . UUID.toText <$> UUID.nextRandom
                let taskLabel = truncateText 40 resolvedLabel
                    ttlMicros = storeTtlMicros store
                -- Fork the sub-agent, then insert into the store.
                -- insertTask must happen before the thread can be observed
                -- via check_task; since Haskell's async spawns the thread
                -- lazily (it won't run until the current thread yields),
                -- the insert below runs before the forked thread starts.
                -- Even if scheduling were eager, the only consequence is a
                -- brief window where check_task returns "still running"
                -- which is correct.
                a <-
                  Async.async $ do
                    mResult <- timeout ttlMicros runWithCatch
                    pure $ fromMaybe (AgentError "Async task timed out") mResult
                insertTask store taskId taskLabel a
                logInfo
                  logger
                  "Async delegate task started"
                  [ ("task_id", taskId.unTaskId),
                    ("label", taskLabel)
                  ]
                pure $ toolSuccess $ "Task started: " <> taskId.unTaskId <> " (" <> taskLabel <> ")"
              _ -> do
                result <- runWithCatch

                case result of
                  AgentSuccess responseText _ -> do
                    logInfo
                      logger
                      "Delegate task completed"
                      [("response_length", T.pack (show (T.length responseText)))]
                    pure $ toolSuccess responseText
                  AgentCancelled -> do
                    logInfo logger "Delegate task cancelled" []
                    pure $ toolError "Task was cancelled"
                  AgentError err -> do
                    logError logger "Delegate task failed" [("error", err)]
                    pure $ toolError err

-- | JSON Schema for delegate_task input
delegateSchema :: [Text] -> Map Text AgentPreset -> Value
delegateSchema allowedModels presets =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          ( [ "task"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Description of the task to delegate. Be specific about what you want accomplished." :: Text)
                  ],
              "thinking"
                .= object
                  [ "description" .= ("Thinking config for the sub-agent. Use {\"enable\": true, \"mode\": {\"adaptive\": {\"effort\": \"medium\"}}} to enable, or {\"enable\": false} to disable. Omit to inherit. Ensure model compatibility (e.g., Haiku does not support adaptive thinking)." :: Text),
                    "type" .= ("object" :: Text),
                    "properties"
                      .= object
                        [ "enable"
                            .= object
                              [ "type" .= ("boolean" :: Text)
                              ],
                          "mode"
                            .= object
                              [ "type" .= ("object" :: Text),
                                "description" .= ("Thinking mode: {\"adaptive\": {\"effort\": \"medium\"}} or {\"fixed\": {\"budget_tokens\": 4096}}" :: Text)
                              ]
                        ]
                  ],
              "max_iterations"
                .= object
                  [ "type" .= ("integer" :: Text),
                    "description" .= ("Maximum agent loop iterations for the sub-agent (default: 10, max: 50)" :: Text),
                    "minimum" .= (1 :: Int),
                    "maximum" .= (50 :: Int)
                  ],
              "system_prompt"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Override the sub-agent's system prompt with this text." :: Text)
                  ],
              "output_schema"
                .= object
                  [ "type" .= ("object" :: Text),
                    "description" .= ("JSON Schema to constrain the sub-agent's final response. When provided, the response will be valid JSON matching this schema." :: Text)
                  ],
              "async"
                .= object
                  [ "type" .= ("boolean" :: Text),
                    "description" .= ("If true, run the task in the background and return a task ID immediately. Use check_task to poll for results." :: Text)
                  ],
              "label"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Human-readable label for this task, e.g. \"fetch weather data\". Used in status messages and tool-use notifications." :: Text)
                  ]
            ]
              ++ modelProp
              ++ agentProp
          ),
      "required" .= (["task"] :: [Text])
    ]
  where
    agentKeys = Map.keys presets
    modelProp
      | null allowedModels = []
      | otherwise =
          [ "model"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("Model to use for the sub-agent" :: Text),
                  "enum" .= allowedModels
                ]
          ]
    agentProp
      | null agentKeys = []
      | otherwise =
          [ "agent"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= agentDescription,
                  "enum" .= agentKeys
                ]
          ]
    agentDescription :: Text
    agentDescription =
      let descs =
            [ n <> maybe "" (\d -> " (" <> d <> ")") p.description
            | (n, p) <- Map.toAscList presets
            ]
       in "Named agent preset. Available: " <> T.intercalate ", " descs

-- | Parse delegate_task input
parseDelegateInput :: [Text] -> [Text] -> Value -> Either Text DelegateInput
parseDelegateInput allowedModels agentKeys (Aeson.Object obj) = do
  task <- case KM.lookup "task" obj of
    Just (Aeson.String t)
      | T.null (T.strip t) -> Left "Task description must not be empty"
      | otherwise -> Right t
    _ -> Left "Missing or invalid 'task' parameter"
  agentParam <- case KM.lookup "agent" obj of
    Just (Aeson.String a)
      | null agentKeys ->
          Left "Agent selection is not enabled (configure delegate.extra_agents)"
      | a `notElem` agentKeys ->
          Left $ "Invalid agent '" <> a <> "'. Available: " <> T.intercalate ", " agentKeys
      | otherwise -> Right (Just a)
    Just _ -> Left "Invalid 'agent' parameter (must be a string)"
    Nothing -> Right Nothing
  modelParam <- case KM.lookup "model" obj of
    Just (Aeson.String m)
      | null allowedModels ->
          Left "Model selection is not enabled (configure delegate.allowed_models)"
      | m `notElem` allowedModels ->
          Left $ "Invalid model '" <> m <> "'. Allowed: " <> T.intercalate ", " allowedModels
      | otherwise -> Right (Just m)
    Just _ -> Left "Invalid 'model' parameter (must be a string)"
    Nothing -> Right Nothing
  thinkingParam <- case KM.lookup "thinking" obj of
    Just (Aeson.Bool False) -> Right (Just (ThinkingOverrides (Last (Just False)) (Last Nothing)))
    Just (Aeson.Bool True) -> Right (Just (ThinkingOverrides (Last (Just True)) (Last Nothing)))
    Just val@(Aeson.Object _) -> case Aeson.fromJSON val of
      Aeson.Success ovr -> Right (Just ovr)
      Aeson.Error err -> Left $ "Invalid thinking config: " <> T.pack err
    Just _ -> Left "Invalid 'thinking' parameter (must be an object like {\"enable\": true, \"mode\": {\"adaptive\": {}}} or false)"
    Nothing -> Right Nothing
  maxIterParam <- case KM.lookup "max_iterations" obj of
    Just (Aeson.Number n) ->
      let i = round n :: Int
       in if i < 1 || i > 50
            then Left "max_iterations must be between 1 and 50"
            else Right (Just (fromIntegral i))
    Just _ -> Left "Invalid 'max_iterations' parameter (must be an integer)"
    Nothing -> Right Nothing
  systemPromptParam <- case KM.lookup "system_prompt" obj of
    Just (Aeson.String t)
      | T.null (T.strip t) -> Left "system_prompt must not be empty"
      | otherwise -> Right (Just [InlineText t])
    Just _ -> Left "Invalid 'system_prompt' parameter (must be a string)"
    Nothing -> Right Nothing
  outputSchemaParam <- case KM.lookup "output_schema" obj of
    Just val@(Aeson.Object _) -> Right (Just val)
    Just _ -> Left "Invalid 'output_schema' parameter (must be a JSON object)"
    Nothing -> Right Nothing
  asyncParam <- case KM.lookup "async" obj of
    Just (Aeson.Bool b) -> Right b
    Just _ -> Left "Invalid 'async' parameter (must be a boolean)"
    Nothing -> Right False
  labelParam <- case KM.lookup "label" obj of
    Just (Aeson.String t)
      | T.null (T.strip t) -> Left "label must not be empty"
      | otherwise -> Right (Just t)
    Just _ -> Left "Invalid 'label' parameter (must be a string)"
    Nothing -> Right Nothing
  let ovr = AgentOverrides {model = Last modelParam, thinking = thinkingParam, maxIterations = Last maxIterParam, cache = Nothing, maxTokens = Last Nothing, systemPrompt = Last systemPromptParam, toolSearch = Last Nothing, permissions = Nothing}
  Right DelegateInput {task, agentName = agentParam, overrides = ovr, outputSchema = outputSchemaParam, async = asyncParam, label = labelParam}
parseDelegateInput _ _ _ = Left "Expected object input"
