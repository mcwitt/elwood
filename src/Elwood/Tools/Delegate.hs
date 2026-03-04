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
import Elwood.AgentSettings (AgentSettings (..))
import Elwood.Claude.AgentLoop (AgentConfig (..), AgentResult (..), runAgentTurn)
import Elwood.Claude.Client (ClaudeClient)
import Elwood.Claude.Types (ClaudeMessage (..), ContentBlock (..), Role (..), ToolSchema (..))
import Elwood.Config (CompactionConfig (..), PruningConfig)
import Elwood.Logging (Logger, logError, logInfo)
import Elwood.Metrics (MetricsStore, metricsObserver)
import Elwood.Tools.Registry (ToolRegistry)
import Elwood.Tools.Types

-- | Default max iterations for sub-agents
defaultMaxIterations :: Int
defaultMaxIterations = 10

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
  Tool
mkDelegateTaskTool logger client baseRegistry context agentSettings compaction pruning systemPrompt metrics =
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
            inputSchema = delegateSchema
          },
      execute = executeDelegateTask
    }
  where
    executeDelegateTask :: Value -> IO ToolResult
    executeDelegateTask input = case parseDelegateInput input of
      Left err -> pure $ toolError err
      Right (task, maxIter) -> do
        let iterations = maybe defaultMaxIterations (min 50 . max 1) maxIter
        logInfo
          logger
          "Delegating task to sub-agent"
          [ ("task_length", T.pack (show (T.length task))),
            ("max_iterations", T.pack (show iterations))
          ]

        let subSettings = agentSettings {maxIterations = iterations}
            -- Disable compaction — sub-agent starts with empty history
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
            userMsg = ClaudeMessage User [TextBlock task]

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
delegateSchema :: Value
delegateSchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "task"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("Description of the task to delegate. Be specific about what you want accomplished." :: Text)
                ],
            "max_iterations"
              .= object
                [ "type" .= ("integer" :: Text),
                  "description" .= ("Maximum agent loop iterations for the sub-agent (default: 10, max: 50)" :: Text)
                ]
          ],
      "required" .= (["task"] :: [Text])
    ]

-- | Parse delegate_task input
parseDelegateInput :: Value -> Either Text (Text, Maybe Int)
parseDelegateInput (Aeson.Object obj) = do
  task <- case KM.lookup "task" obj of
    Just (Aeson.String t)
      | T.null (T.strip t) -> Left "Task description must not be empty"
      | otherwise -> Right t
    _ -> Left "Missing or invalid 'task' parameter"
  let maxIter = case KM.lookup "max_iterations" obj of
        Just (Aeson.Number n) -> Just (round n)
        _ -> Nothing
  Right (task, maxIter)
parseDelegateInput _ = Left "Expected object input"
