{-# LANGUAGE StrictData #-}

module Elwood.Tools.Delegate
  ( mkDelegateTaskTool,
  )
where

import Control.Exception (SomeException, catch)
import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.AgentSettings (AgentOverrides (..), AgentSettings (..), resolveAgent, toOverrides)
import Elwood.Claude.AgentLoop (AgentConfig (..), AgentResult (..), runAgentTurn)
import Elwood.Claude.Client (ClaudeClient)
import Elwood.Claude.Types (ClaudeMessage (..), ContentBlock (..), Role (..), ToolSchema (..))
import Elwood.Config (CompactionConfig (..), PruningConfig)
import Elwood.Logging (Logger, logError, logInfo)
import Elwood.Metrics (MetricsStore, metricsObserver)
import Elwood.Thinking (ThinkingLevel (..), parseThinkingLevel)
import Elwood.Tools.Registry (ToolRegistry)
import Elwood.Tools.Types

-- | Default overrides for delegate sub-agents.
-- Sets max_iterations to 10 (lower than the parent's default of 20).
-- Model and thinking inherit from the parent agent.
delegateDefaults :: AgentOverrides
delegateDefaults = AgentOverrides Nothing Nothing (Just 10)

-- | Parsed delegate_task input
data DelegateInput = DelegateInput
  { task :: Text,
    modelParam :: Maybe Text,
    thinkingParam :: Maybe ThinkingLevel,
    maxIterParam :: Maybe Int
  }

-- | Construct a delegate_task tool that spawns a sub-agent with an isolated
-- conversation. The sub-agent receives the base tool registry (which does not
-- contain delegate_task), preventing recursive nesting.
mkDelegateTaskTool ::
  Logger ->
  ClaudeClient ->
  ToolRegistry ->
  AgentContext ->
  AgentSettings ->
  CompactionConfig ->
  PruningConfig ->
  Maybe Text ->
  MetricsStore ->
  AgentOverrides ->
  [Text] ->
  Tool
mkDelegateTaskTool logger client baseRegistry context agentSettings compaction pruning systemPrompt metrics configOverrides allowedModels =
  Tool
    { schema =
        ToolSchema
          { name = "delegate_task",
            description =
              "Delegate a task to a sub-agent with its own isolated conversation context. "
                <> "The sub-agent has access to all the same tools (except delegate_task) "
                <> "and runs independently, returning only a text summary. "
                <> "Use this to keep the main conversation context clean when a task "
                <> "requires many tool calls (e.g., multi-file edits, long research).",
            inputSchema = delegateSchema allowedModels
          },
      execute = executeDelegateTask
    }
  where
    executeDelegateTask :: Value -> IO ToolResult
    executeDelegateTask input = case parseDelegateInput allowedModels input of
      Left err -> pure $ toolError err
      Right di -> do
        -- Layering: parent settings < delegate defaults < config overrides < tool params
        let baseOverrides = toOverrides agentSettings <> delegateDefaults <> configOverrides
            toolOverrides =
              AgentOverrides
                { model = di.modelParam,
                  thinking = di.thinkingParam,
                  maxIterations = di.maxIterParam
                }
            subSettings = resolveAgent (baseOverrides <> toolOverrides)

        logInfo
          logger
          "Delegating task to sub-agent"
          [ ("task_length", T.pack (show (T.length di.task))),
            ("model", subSettings.model),
            ("max_iterations", T.pack (show subSettings.maxIterations))
          ]

        let -- Disable compaction — sub-agent starts with empty history
            subCompaction = compaction {tokenThreshold = maxBound}
            subConfig =
              AgentConfig
                { logger = logger,
                  client = client,
                  registry = baseRegistry,
                  context = context,
                  compaction = subCompaction,
                  systemPrompt = systemPrompt,
                  agentSettings = subSettings,
                  observer = metricsObserver metrics subSettings.model "delegate",
                  onRateLimit = Nothing,
                  onText = Nothing,
                  onToolUse = Nothing,
                  onBeforeApiCall = Nothing,
                  toolSearch = Nothing,
                  pruningConfig = pruning,
                  pruneHorizon = 0
                }
            userMsg = ClaudeMessage User [TextBlock di.task]

        result <-
          runAgentTurn subConfig [] userMsg
            `catch` \(e :: SomeException) -> do
              logError logger "Delegate sub-agent error" [("error", T.pack (show e))]
              pure $ AgentError $ "Sub-agent error: " <> T.pack (show e)

        case result of
          AgentSuccess responseText _ -> do
            logInfo
              logger
              "Delegate task completed"
              [("response_length", T.pack (show (T.length responseText)))]
            pure $ toolSuccess responseText
          AgentError err -> do
            logError logger "Delegate task failed" [("error", err)]
            pure $ toolError err

-- | JSON Schema for delegate_task input
delegateSchema :: [Text] -> Value
delegateSchema allowedModels =
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
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Thinking level for the sub-agent (default: inherit from config)" :: Text),
                    "enum" .= (["off", "low", "medium", "high"] :: [Text])
                  ],
              "max_iterations"
                .= object
                  [ "type" .= ("integer" :: Text),
                    "description" .= ("Maximum agent loop iterations for the sub-agent (default: 10, max: 50)" :: Text),
                    "minimum" .= (1 :: Int),
                    "maximum" .= (50 :: Int)
                  ]
            ]
              ++ modelProp
          ),
      "required" .= (["task"] :: [Text])
    ]
  where
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

-- | Parse delegate_task input
parseDelegateInput :: [Text] -> Value -> Either Text DelegateInput
parseDelegateInput allowedModels (Aeson.Object obj) = do
  task <- case KM.lookup "task" obj of
    Just (Aeson.String t)
      | T.null (T.strip t) -> Left "Task description must not be empty"
      | otherwise -> Right t
    _ -> Left "Missing or invalid 'task' parameter"
  modelParam <- case KM.lookup "model" obj of
    Just (Aeson.String m)
      | null allowedModels ->
          Left "Model selection is not enabled (configure agent.delegate.allowed_models)"
      | m `notElem` allowedModels ->
          Left $ "Invalid model '" <> m <> "'. Allowed: " <> T.intercalate ", " allowedModels
      | otherwise -> Right (Just m)
    Just _ -> Left "Invalid 'model' parameter (must be a string)"
    Nothing -> Right Nothing
  thinkingParam <- case KM.lookup "thinking" obj of
    Just (Aeson.String t) -> case parseThinkingLevel (Aeson.String t) of
      ThinkingOff
        | T.toLower t /= "off" ->
            Left $ "Invalid thinking level '" <> t <> "'. Allowed: off, low, medium, high"
      level -> Right (Just level)
    Just _ -> Left "Invalid 'thinking' parameter (must be a string)"
    Nothing -> Right Nothing
  maxIterParam <- case KM.lookup "max_iterations" obj of
    Just (Aeson.Number n) ->
      let i = round n
       in if i < 1 || i > 50
            then Left "max_iterations must be between 1 and 50"
            else Right (Just i)
    Just _ -> Left "Invalid 'max_iterations' parameter (must be an integer)"
    Nothing -> Right Nothing
  Right DelegateInput {task, modelParam, thinkingParam, maxIterParam}
parseDelegateInput _ _ = Left "Expected object input"
