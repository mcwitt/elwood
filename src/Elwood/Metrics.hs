module Elwood.Metrics
  ( MetricsStore,
    newMetricsStore,
    recordApiResponse,
    recordToolCall,
    recordCompaction,
    setMCPServerCount,
    recordInputBreakdown,
    InputTokenType (..),
    estimateTextTokens,
    estimateJsonTokens,
    metricsObserver,
    metricsSource,
    renderMetrics,
  )
where

import Data.Aeson (Value, encode, withObject, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Int (Int64)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Elwood.Claude.Compaction (estimateTokens)
import Elwood.Claude.Conversation qualified as Conversation
import Elwood.Claude.Observer (AgentObserver (..))
import Elwood.Claude.Types qualified as C
import Elwood.Event.Types (EventSource (..))
import Elwood.Tools.Registry qualified as Tools
import Numeric (showFFloat)

data InputTokenType = ItSystemPrompt | ItUserText | ItAssistantText | ItThinking | ItToolSchemas | ItToolUse | ItToolResult
  deriving stock (Eq, Ord, Show)

inputTokenTypeLabel :: InputTokenType -> Text
inputTokenTypeLabel = \case
  ItSystemPrompt -> "system_prompt"
  ItUserText -> "user_text"
  ItAssistantText -> "assistant_text"
  ItThinking -> "thinking"
  ItToolSchemas -> "tool_schemas"
  ItToolUse -> "tool_use"
  ItToolResult -> "tool_result"

-- A sample is its metric suffix and ordered labels. Pointwise addition gives
-- counters a commutative monoid; a request publishes its entire delta atomically.
type Sample = (Text, [(Text, Text)])

data MetricsStore = MetricsStore {counters :: IORef (Map Sample Int64), mcpServers :: IORef Int, started :: UTCTime}

newMetricsStore :: IO MetricsStore
newMetricsStore = MetricsStore <$> newIORef Map.empty <*> newIORef 0 <*> getCurrentTime

record :: MetricsStore -> [(Sample, Int64)] -> IO ()
record store samples = atomicModifyIORef' store.counters (\old -> (Map.unionWith (+) old (Map.fromListWith (+) samples), ()))

labels :: Text -> Text -> [(Text, Text)]
labels model source = [("model", model), ("source", source)]

recordApiResponse :: MetricsStore -> Text -> Text -> C.StopReason -> C.Usage -> IO ()
recordApiResponse store model source reason usage =
  record store $
    [ ((suffix <> "_tokens_total", ls), fromIntegral n)
    | (suffix, n) <- [("input", usage.inputTokens), ("output", usage.outputTokens), ("cache_read", usage.cacheReadInputTokens)]
    ]
      <> [ (("cache_creation_tokens_total", ls <> [("cache_ttl", ttl)]), fromIntegral n)
         | (ttl, n) <- [("5m", usage.cacheCreation5mTokens), ("1h", usage.cacheCreation1hTokens)]
         ]
      <> [(("api_requests_total", ls <> [("stop_reason", C.stopReasonToText reason)]), 1)]
  where
    ls = labels model source

recordToolCall :: MetricsStore -> Text -> IO ()
recordToolCall store tool = record store [(("tool_calls_total", [("tool", tool)]), 1)]

recordCompaction :: MetricsStore -> IO ()
recordCompaction store = record store [(("compactions_total", []), 1)]

setMCPServerCount :: MetricsStore -> Int -> IO ()
setMCPServerCount store = writeIORef store.mcpServers

estimateTextTokens :: Text -> Int64
estimateTextTokens = (`div` 4) . fromIntegral . T.length

estimateJsonTokens :: Value -> Int64
estimateJsonTokens = (`div` 4) . LBS.length . encode

estimateSample :: Text -> Text -> InputTokenType -> Text -> Sample
estimateSample model source typ tool = ("estimated_input_tokens_total", labels model source <> [("type", inputTokenTypeLabel typ)] <> [("tool", tool) | not (T.null tool)])

recordInputBreakdown :: MetricsStore -> Text -> Text -> Maybe Text -> Maybe (Set C.ToolName) -> [C.ClaudeMessage] -> [C.ToolSchema] -> IO ()
recordInputBreakdown store model source system search messages schemas =
  record
    store
    [(estimateSample model source typ tool, n) | (typ, tool, n) <- parts]
  where
    schemaTokens s = estimateTextTokens s.name.unToolName + estimateTextTokens s.description + estimateJsonTokens s.inputSchema
    schemaMap = Map.fromList [(s.name, s) | s <- schemas]
    nonDeferred = fromMaybe (Map.keysSet schemaMap) search
    names = Map.fromList [(tid, name.unToolName) | C.ClaudeMessage C.Assistant blocks <- messages, C.ToolUseBlock tid name _ <- blocks]
    upfront = sum [schemaTokens s | s <- schemas, Set.member s.name nonDeferred]
    parts =
      [(ItSystemPrompt, "", estimateTextTokens t) | t <- maybe [] pure system]
        <> [(ItToolSchemas, "", upfront) | upfront > 0]
        <> concat [concatMap (block role) blocks | C.ClaudeMessage role blocks <- messages]
    block role (C.TextBlock t) = [(if role == C.User then ItUserText else ItAssistantText, "", estimateTextTokens t)]
    block _ (C.ThinkingBlock t _) = [(ItThinking, "", estimateTextTokens t)]
    block _ (C.RedactedThinkingBlock t) = [(ItThinking, "", estimateTextTokens t)]
    block _ (C.ToolUseBlock _ name input) = [(ItToolUse, name.unToolName, estimateJsonTokens input)]
    block _ (C.ToolResultBlock tid content _) = [(ItToolResult, Map.findWithDefault "unknown" tid names, estimateTextTokens (C.toolResultText content))]
    block _ (C.ToolSearchResultBlock _ value) =
      [(ItToolSchemas, "", schemaTokens s) | name <- references value, not (Set.member name nonDeferred), Just s <- [Map.lookup name schemaMap]]
    block _ _ = []
    references value =
      fromMaybe [] $
        parseMaybe
          ( withObject "search" $ \o ->
              o .: "tool_references" >>= mapM (withObject "reference" (\r -> C.ToolName <$> r .: "tool_name"))
          )
          value

metricsSource :: EventSource -> Text
metricsSource (TelegramSource _) = "telegram"
metricsSource (WebhookSource name) = "webhook:" <> name
metricsSource CallbackSource = "callback"

metricsObserver :: MetricsStore -> Text -> Text -> AgentObserver
metricsObserver store model source =
  AgentObserver
    (recordInputBreakdown store model source)
    (recordApiResponse store model source)
    (recordToolCall store)
    (recordCompaction store)

renderMetrics :: MetricsStore -> Conversation.ConversationStore -> Tools.ToolRegistry -> IO LBS.ByteString
renderMetrics store conversations registry = do
  counters <- readIORef store.counters
  mcp <- readIORef store.mcpServers
  now <- getCurrentTime
  history <- conversations.allConversations
  let gauges :: [(Sample, Int64)]
      gauges =
        [ (("tools_registered", []), fromIntegral (length (Tools.allTools registry))),
          (("mcp_servers_active", []), fromIntegral mcp),
          (("uptime_seconds", []), floor (diffUTCTime now store.started))
        ]
          <> [ ((name, [("session", c.sessionId)]), n)
             | c <- Map.elems history,
               (name, n) <-
                 [ ("conversation_messages", fromIntegral (length c.messages)),
                   ("conversation_estimated_tokens", fromIntegral (estimateTokens c.messages)),
                   ("conversation_cache_expires_at", floor (utcTimeToPOSIXSeconds c.cacheExpiresAt))
                 ]
             ]
      samples =
        [(key, ("counter", T.pack (show value))) | (key, value) <- Map.toList counters]
          <> [(key, ("gauge", T.pack (show value))) | (key, value) <- gauges]
          <> costSamples counters
      families = Map.fromListWith (<>) [(name, [(ls, kind, value)]) | ((name, ls), (kind, value)) <- samples]
  pure . LBS.fromStrict . TE.encodeUtf8 . T.concat $
    [ let name = "elwood_" <> suffix
       in "# HELP "
            <> name
            <> " "
            <> T.replace "_" " " suffix
            <> "\n# TYPE "
            <> name
            <> " "
            <> kind
            <> "\n"
            <> T.concat [name <> renderLabels ls <> " " <> value <> "\n" | (ls, _, value) <- rows]
    | (suffix, rows@((_, kind, _) : _)) <- Map.toList families
    ]

renderLabels :: [(Text, Text)] -> Text
renderLabels [] = ""
renderLabels xs = "{" <> T.intercalate "," [k <> "=\"" <> escape v <> "\"" | (k, v) <- xs] <> "}"
  where
    escape = T.replace "\n" "\\n" . T.replace "\"" "\\\"" . T.replace "\\" "\\\\"

costSamples :: Map Sample Int64 -> [(Sample, (Text, Text))]
costSamples counters =
  [ (("cost_dollars", ls), ("counter", T.pack (showFFloat (Just 6) cost "")))
  | (model, source) <- Set.toList (Set.fromList [(m, s) | ((_, ("model", m) : ("source", s) : _), _) <- Map.toList counters]),
    let ls = labels model source,
    let value key = fromIntegral (Map.findWithDefault 0 key counters),
    Just pricing <- [lookupPricing model],
    let cost =
          sum [value (suffix, ls) * rate | (suffix, rate) <- [("input_tokens_total", pricing.inputPerMTok), ("output_tokens_total", pricing.outputPerMTok), ("cache_read_tokens_total", pricing.cacheReadPerMTok)]] / 1e6
            + sum [value ("cache_creation_tokens_total", ls <> [("cache_ttl", ttl)]) * rate | (ttl, rate) <- [("5m", pricing.cache5mWritePerMTok), ("1h", pricing.cache1hWritePerMTok)]] / 1e6
  ]

data ModelPricing = ModelPricing
  { inputPerMTok :: Double,
    outputPerMTok :: Double,
    cache5mWritePerMTok :: Double,
    cache1hWritePerMTok :: Double,
    cacheReadPerMTok :: Double
  }

-- | Prefix-matched pricing table. First match wins, so more specific prefixes come first.
-- Source: https://platform.claude.com/docs/en/about-claude/pricing
pricingTable :: [(Text, ModelPricing)]
pricingTable =
  [ ("claude-fable-5-1", ModelPricing 10 50 12.50 20 0.25),
    ("claude-fable-5", ModelPricing 10 50 12.50 20 1),
    ("claude-opus-5-5", ModelPricing 4 20 5 8 0.20),
    ("claude-opus-5", ModelPricing 5 25 6.25 10 0.50),
    ("claude-opus-4-8", ModelPricing 5 25 6.25 10 0.50),
    ("claude-opus-4-7", ModelPricing 5 25 6.25 10 0.50),
    ("claude-opus-4-6", ModelPricing 5 25 6.25 10 0.50),
    ("claude-opus-4-5", ModelPricing 5 25 6.25 10 0.50),
    ("claude-opus-4-1", ModelPricing 15 75 18.75 30 1.50),
    ("claude-opus-4", ModelPricing 15 75 18.75 30 1.50),
    ("claude-sonnet-5", ModelPricing 2 10 2.50 4 0.20),
    ("claude-sonnet-4", ModelPricing 3 15 3.75 6 0.30),
    ("claude-sonnet-3", ModelPricing 3 15 3.75 6 0.30),
    ("claude-haiku-4", ModelPricing 1 5 1.25 2 0.10),
    ("claude-3-5-haiku", ModelPricing 1 5 1.25 2 0.10),
    ("claude-3-5-sonnet", ModelPricing 3 15 3.75 6 0.30),
    ("claude-3-opus", ModelPricing 15 75 18.75 30 1.50),
    ("claude-3-haiku", ModelPricing 0.25 1.25 0.30 0.50 0.03)
  ]

-- | Look up pricing for a model ID by prefix match
lookupPricing :: Text -> Maybe ModelPricing
lookupPricing model = snd <$> find (\(prefix, _) -> prefix `T.isPrefixOf` model) pricingTable
