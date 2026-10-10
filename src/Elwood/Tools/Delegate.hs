module Elwood.Tools.Delegate (mkDelegateTaskTool, labelMaxLen, inheritedOverrides) where

import Control.Exception (SomeException)
import Control.Monad ((>=>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid (Last (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Elwood.AgentSettings
import Elwood.AgentSettings qualified as Override (AgentOverrides (..))
import Elwood.Claude.AgentLoop
import Elwood.Claude.Client (ClaudeClient)
import Elwood.Claude.Observer (ToolUseCallback)
import Elwood.Claude.Types
import Elwood.Config (PruningConfig)
import Elwood.Exception (catchSync)
import Elwood.Logging (Logger, logInfo)
import Elwood.Metrics (MetricsStore, estimateTextTokens, estimateToolTokens, metricsObserver)
import Elwood.Notify (truncateText)
import Elwood.Positive (literal, positive)
import Elwood.Prompt (PromptInput (InlineText), assemblePrompt)
import Elwood.Thinking
import Elwood.Tools.AsyncTask
import Elwood.Tools.Builder qualified as B
import Elwood.Tools.Command (mkRunCommandTool)
import Elwood.Tools.Registry
import Elwood.Tools.Types
import System.Timeout (timeout)

labelMaxLen :: Int
labelMaxLen = 30

inheritedOverrides :: AgentProfile -> AgentOverrides
inheritedOverrides parent =
  toOverrides parent <> case parent.thinking of
    Just (Adaptive effort (Just _)) -> (mempty :: AgentOverrides) {Override.thinking = Just (ThinkingOverrides mempty (Last (Just (Adaptive effort Nothing))))}
    _ -> mempty

data Input = Input {task :: Text, agent :: Maybe Text, overrides :: AgentOverrides, outputSchema :: Maybe Value, background :: Bool, label :: Maybe Text, seconds :: Maybe Int}

mkDelegateTaskTool :: Logger -> ClaudeClient -> ToolRegistry -> ApprovalFunction -> AgentProfile -> Maybe PruningConfig -> FilePath -> Maybe Int -> MetricsStore -> AgentPreset -> Map Text AgentPreset -> [Text] -> Maybe (Text -> ToolUseCallback) -> Maybe AsyncTaskStore -> Tool
mkDelegateTaskTool logger client registry approve parent pruning workspace imageDimension metrics preset presets models notify store =
  B.tool "delegate_task" description fields (parseInput models (Map.keys presets)) $ \input ->
    if input.background && isNothing store
      then pure (toolError "Async delegation is not available")
      else do
        let defaults = (mempty :: AgentOverrides) {Override.maxIterations = Last (Just (literal @10)), Override.cache = Just (CacheOverrides (Last (Just False)) mempty)}
            named = maybe mempty (maybe mempty (.overrides) . (`Map.lookup` presets)) input.agent
            profile = resolveProfile (inheritedOverrides parent <> defaults <> preset.overrides <> named <> input.overrides)
            taskLabel = truncateText labelMaxLen (fromMaybe input.task input.label)
            tools = applyToolFilter profile.toolFilter $ (if input.background then filterRegistry (/= ToolName "queue_attachment") else id) (registerTool (mkRunCommandTool logger workspace profile.permissions) registry)
        prompt <- assemblePrompt workspace profile.systemPrompt
        let config =
              AgentConfig
                { logger,
                  client,
                  registry = tools,
                  requestApproval = approve,
                  systemPrompt = prompt,
                  agentProfile = profile,
                  observer = metricsObserver metrics profile.model.model "delegate",
                  onRateLimit = Nothing,
                  onText = Nothing,
                  onToolUse = if input.background then Nothing else ($ taskLabel) <$> notify,
                  onBeforeApiCall = Nothing,
                  toolSearch = case profile.toolSearch of
                    ToolSearchDisabled -> Nothing
                    ToolSearchEnabled names -> Just (Set.fromList (map ToolName names)),
                  pruningConfig = pruning,
                  pruneHorizon = 0,
                  outputFormat = jsonSchemaFormat <$> input.outputSchema,
                  maxImageDimension = imageDimension,
                  isCancelled = maybe (pure False) isTurnCancelled store
                }
            run = runAgentTurn config [] (ClaudeMessage User [TextBlock input.task]) `catchSync` \(e :: SomeException) -> pure (AgentError (taggedError Unexpected (T.pack (show e))))
            schemas = toolSchemas tools
            eager = maybe schemas (\names -> filter (\schema -> Set.member schema.name names) schemas) config.toolSearch
            estimatedTokens = maybe 0 estimateTextTokens prompt + estimateTextTokens input.task + sum (map estimateToolTokens eager)
            catalog = T.pack (show (length schemas)) <> " tools (" <> T.pack (show (length eager)) <> " upfront); approximately " <> T.pack (show estimatedTokens) <> " initial input tokens"
            timedOut = "Delegate task timed out; " <> catalog <> ". Omit tools to use the agent preset, or pass a smaller tools array."
            bounded duration = fromMaybe (AgentError (taggedError Timeout timedOut)) <$> timeout duration run
        logInfo logger "Starting delegate" [("model", profile.model.model), ("catalog", catalog)]
        case (input.background, store) of
          (True, Just tasks) -> do
            key <- TaskId . UUID.toText <$> UUID.nextRandom
            let limit = maybe 3600 fromIntegral input.seconds
            startTask tasks key taskLabel (bounded (toMicroseconds limit))
            pure (toolSuccess ("Task started: " <> key.unTaskId <> " (" <> taskLabel <> "); " <> catalog))
          _ -> resultTool <$> maybe run (bounded . (* 1000000)) input.seconds
  where
    description = "Delegate an isolated conversation sharing the workspace and non-delegating tools. Background tasks return an ID; use await_task or check_task to observe results. Background tasks cannot queue attachments or send progress notifications; return artifact paths in their result. " <> fromMaybe "" preset.description
    fields =
      B.schema
        ( [ ("task", B.field "string" "Task instructions"),
            ("thinking", B.field "object" "Thinking configuration: enable and mode"),
            ("max_iterations", B.field "integer" "Model requests, 1–50"),
            ("system_prompt", B.field "string" "Override system prompt"),
            ("output_schema", B.field "object" "JSON schema using Anthropic structured-output constraints. Object schemas default additionalProperties to false; other constraints are preserved."),
            ("async", B.field "boolean" "Run in background"),
            ("label", B.field "string" "Task label"),
            ("timeout_seconds", B.field "integer" "Positive execution timeout; background default 3600 seconds"),
            ("tools", object ["description" .= ("Tool names as a JSON array. Omit to use the agent preset. The string all overrides the preset with the full catalog, which can be expensive; prefer only the tools needed." :: Text), "anyOf" .= [object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("string" :: Text)]], object ["type" .= ("string" :: Text), "enum" .= ["all" :: Text]]]])
          ]
            <> enumField "model" "Allowed model" models
            <> enumField "agent" (T.intercalate ", " [name <> maybe "" (\d -> " (" <> d <> ")") p.description | (name, p) <- Map.toAscList presets]) (Map.keys presets)
        )
        ["task"]
    enumField :: Key -> Text -> [Text] -> [(Key, Value)]
    enumField key explanation values = [(key, object ["type" .= ("string" :: Text), "description" .= explanation, "enum" .= values]) | not (null values)]

resultTool :: AgentResult -> ToolResult
resultTool = \case
  AgentSuccess text _ -> toolSuccess text
  AgentTruncated text _ -> toolError ("[truncated] " <> text)
  AgentExhausted info -> toolError (formatExhaustion info)
  AgentCancelled -> toolError (taggedError Cancelled "Task was cancelled")
  AgentError message -> toolError message

parseInput :: [Text] -> [Text] -> Value -> Parser Input
parseInput models agents = withObject "delegate_task" $ \value -> do
  task <- B.nonempty value "task"
  agent <- selection value "agent" agents
  model <- selection value "model" models
  thinking <-
    value .:! "thinking"
      >>= traverse
        ( \case
            Bool enabled -> pure (ThinkingOverrides (Last (Just enabled)) mempty)
            input -> parseJSON input
        )
  iterations <- value .:! "max_iterations" >>= traverse (B.integer 1 50 >=> maybe (fail "Invalid iterations") pure . positive)
  system <-
    value .:! "system_prompt"
      >>= traverse
        ( \input -> do
            text <- parseJSON @Text input
            if T.null (T.strip text) then fail "system_prompt must not be empty" else pure [InlineText text]
        )
  output <- value .:! "output_schema" >>= traverse (withObject "output_schema" (pure . Object))
  background <- value .:! "async" .!= False
  label <- value .:! "label" >>= traverse (\text -> if T.null (T.strip text) then fail "label must not be empty" else pure text)
  seconds <- value .:! "timeout_seconds" >>= traverse (B.integer 1 (maxBound `div` 1000000))
  tools <-
    value .:! "tools"
      >>= traverse
        ( \case
            String "all" -> pure AllTools
            input@(Array _) -> parseJSON input
            _ -> fail "tools must be an array of tool names or the string \"all\"; JSON-encoded array strings are not accepted"
        )
  let overrides = (mempty :: AgentOverrides) {Override.model = ModelRefOverrides mempty (Last model), Override.thinking = thinking, Override.maxIterations = Last iterations, Override.systemPrompt = Last system, Override.toolFilter = Last tools}
  pure Input {task, agent, overrides, outputSchema = output, background, label, seconds}
  where
    selection value key allowed = do
      selected <- value .:! key
      traverse (\name -> if name `elem` allowed then pure name else fail ("Invalid " <> show key <> "; allowed: " <> show allowed)) selected
