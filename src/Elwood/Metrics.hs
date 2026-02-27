{-# LANGUAGE StrictData #-}

module Elwood.Metrics
  ( -- * Store
    MetricsStore,
    newMetricsStore,

    -- * Recording
    recordApiResponse,
    recordToolCall,
    recordCompaction,
    setMCPServerCount,
    recordEstimatedInputTokens,
    recordInputBreakdown,

    -- * Estimation
    InputTokenType (..),
    estimateTextTokens,
    estimateJsonTokens,

    -- * Observer
    metricsObserver,

    -- * Source Labels
    metricsSource,

    -- * Rendering
    renderMetrics,
  )
where

import Control.Monad (when)
import Data.Aeson (Value (..), encode, withObject, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (find, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Elwood.Claude.AgentLoop (AgentObserver (..))
import Elwood.Claude.Conversation qualified as Claude
import Elwood.Claude.Types qualified as Claude
import Elwood.Event.Types (EventSource (..))
import Elwood.Tools.Registry qualified as Tools
import Elwood.Tools.Types qualified as Tools
import Text.Printf (printf)

-- | Content type for estimated input token breakdown
data InputTokenType
  = ItSystemPrompt
  | ItUserText
  | ItAssistantText
  | ItThinking
  | ItToolSchemas
  | ItToolUse
  | ItToolResult
  deriving stock (Eq, Ord, Show)

-- | Convert an InputTokenType to its Prometheus label value
inputTokenTypeLabel :: InputTokenType -> Text
inputTokenTypeLabel ItSystemPrompt = "system_prompt"
inputTokenTypeLabel ItUserText = "user_text"
inputTokenTypeLabel ItAssistantText = "assistant_text"
inputTokenTypeLabel ItThinking = "thinking"
inputTokenTypeLabel ItToolSchemas = "tool_schemas"
inputTokenTypeLabel ItToolUse = "tool_use"
inputTokenTypeLabel ItToolResult = "tool_result"

-- | Key for a counter metric
data CounterKey
  = -- | Token counter: metric_suffix, model, source
    CkTokens Text Text Text
  | -- | API requests: model, source, stop_reason
    CkApiRequests Text Text Text
  | -- | Tool calls: tool name
    CkToolCalls Text
  | -- | Compactions (no labels)
    CkCompactions
  | -- | Estimated input tokens: model, source, type, tool (empty when not applicable)
    CkEstimatedInputTokens Text Text InputTokenType Text
  deriving stock (Eq, Ord, Show)

-- | Thread-safe metrics store
data MetricsStore = MetricsStore
  { counters :: IORef (Map CounterKey Int64),
    mcpServerCount :: IORef Int,
    startTime :: UTCTime
  }

-- | Create an empty metrics store
newMetricsStore :: IO MetricsStore
newMetricsStore = MetricsStore <$> newIORef Map.empty <*> newIORef 0 <*> getCurrentTime

-- | Increment a counter by a given amount
incrementCounter :: MetricsStore -> CounterKey -> Int64 -> IO ()
incrementCounter store k delta =
  atomicModifyIORef' store.counters $ \m ->
    (Map.insertWith (+) k delta m, ())

-- | Record metrics from an API response
recordApiResponse :: MetricsStore -> Text -> Text -> Claude.StopReason -> Claude.Usage -> IO ()
recordApiResponse store m source stopReason u = do
  let reasonText = Claude.stopReasonToText stopReason
  incrementCounter store (CkTokens "input" m source) (fromIntegral u.inputTokens)
  incrementCounter store (CkTokens "output" m source) (fromIntegral u.outputTokens)
  incrementCounter store (CkTokens "cache_read" m source) (fromIntegral u.cacheReadInputTokens)
  incrementCounter store (CkTokens "cache_creation" m source) (fromIntegral u.cacheCreationInputTokens)
  incrementCounter store (CkApiRequests m source reasonText) 1

-- | Record a tool call
recordToolCall :: MetricsStore -> Text -> IO ()
recordToolCall store toolName_ =
  incrementCounter store (CkToolCalls toolName_) 1

-- | Record a compaction event
recordCompaction :: MetricsStore -> IO ()
recordCompaction store =
  incrementCounter store CkCompactions 1

-- | Set the number of active MCP servers
setMCPServerCount :: MetricsStore -> Int -> IO ()
setMCPServerCount store n = writeIORef store.mcpServerCount n

-- | Estimate tokens from text (chars / 4)
estimateTextTokens :: Text -> Int64
estimateTextTokens t = fromIntegral (T.length t) `div` 4

-- | Estimate tokens from a JSON value (encoded bytes / 4)
estimateJsonTokens :: Value -> Int64
estimateJsonTokens v = LBS.length (encode v) `div` 4

-- | Record estimated input tokens for a content type
recordEstimatedInputTokens :: MetricsStore -> Text -> Text -> InputTokenType -> Text -> Int64 -> IO ()
recordEstimatedInputTokens store model source typ tool =
  incrementCounter store (CkEstimatedInputTokens model source typ tool)

-- | Estimate tokens for a single tool schema (name + description + input schema)
estimateSchemaTokens :: Claude.ToolSchema -> Int64
estimateSchemaTokens s =
  let Claude.ToolName tn = s.name
   in estimateTextTokens tn + estimateTextTokens s.description + estimateJsonTokens s.inputSchema

-- | Extract tool names from tool_reference objects in a tool search result.
-- Returns an empty list if the JSON structure doesn't match the expected
-- @{"tool_references": [{"tool_name": "..."}]}@ format.
extractToolReferences :: Value -> [Claude.ToolName]
extractToolReferences v = fromMaybe [] $ parseMaybe parser v
  where
    parser = withObject "search_result" $ \o -> do
      refs <- o .: "tool_references"
      mapM (withObject "tool_ref" (\r -> Claude.ToolName <$> r .: "tool_name")) refs

-- | Record estimated input token breakdown for an API call.
--
-- Iterates over the system prompt, tool schemas, and all message content blocks
-- to estimate tokens by content type. Builds a ToolUseId→ToolName map from
-- assistant messages so tool_result blocks can be attributed to the correct tool.
--
-- When tool search is enabled ('Just' neverDefer set), only non-deferred tools
-- are counted upfront. Deferred tools are counted when their 'tool_reference'
-- blocks appear in the conversation (the server expands these into full
-- definitions each turn). Non-deferred tools appearing in search results are
-- skipped to avoid double-counting.
recordInputBreakdown :: MetricsStore -> Text -> Text -> Maybe Text -> Maybe (Set Claude.ToolName) -> [Claude.ClaudeMessage] -> [Claude.ToolSchema] -> IO ()
recordInputBreakdown store model source systemPrompt toolSearch msgs schemas = do
  -- System prompt
  case systemPrompt of
    Just sp -> record ItSystemPrompt "" (estimateTextTokens sp)
    Nothing -> pure ()

  -- Tool schemas: only count non-deferred tools upfront
  let baseSchemas = filter (\s -> Set.member s.name nonDeferred) schemas
      schemaTokens = sum (map estimateSchemaTokens baseSchemas)
  when (schemaTokens > 0) $ record ItToolSchemas "" schemaTokens

  -- Build ToolUseId → ToolName map from all messages
  let toolNameMap =
        Map.fromList
          [ (tid, tn)
          | Claude.ClaudeMessage Claude.Assistant blocks <- msgs,
            Claude.ToolUseBlock tid tn _ <- blocks
          ]

  -- Schema map for looking up deferred tools referenced in search results
  let schemaMap = Map.fromList [(s.name, s) | s <- schemas]

  -- Content blocks
  mapM_ (recordMessageBlocks toolNameMap schemaMap) msgs
  where
    record = recordEstimatedInputTokens store model source

    roleTokenType Claude.User = ItUserText
    roleTokenType Claude.Assistant = ItAssistantText

    recordMessageBlocks toolNameMap schemaMap (Claude.ClaudeMessage role blocks) =
      mapM_ (recordBlock role toolNameMap schemaMap) blocks

    recordBlock role _ _ (Claude.TextBlock t) =
      record (roleTokenType role) "" (estimateTextTokens t)
    recordBlock _ _ _ (Claude.ThinkingBlock t _sig) =
      record ItThinking "" (estimateTextTokens t)
    recordBlock _ _ _ (Claude.RedactedThinkingBlock d) =
      record ItThinking "" (estimateTextTokens d)
    recordBlock _ _ _ (Claude.ToolUseBlock _ (Claude.ToolName tn) input) =
      record ItToolUse tn (estimateJsonTokens input)
    recordBlock _ toolNameMap _ (Claude.ToolResultBlock tid content_ _isErr) =
      let Claude.ToolName tn = Map.findWithDefault (Claude.ToolName "unknown") tid toolNameMap
       in record ItToolResult tn (estimateTextTokens content_)
    recordBlock _ _ schemaMap (Claude.ToolSearchResultBlock _ searchResult) =
      let refs = extractToolReferences searchResult
       in mapM_ (recordReferencedSchema schemaMap) refs
    -- ImageBlock, ServerToolUseBlock: not estimated (images are base64-encoded
    -- data; server tool use is handled via ToolSearchResultBlock)
    recordBlock _ _ _ _ = pure ()

    -- Record schema tokens for a tool_reference, skipping tools already
    -- counted as non-deferred to avoid double-counting.
    recordReferencedSchema schemaMap tn
      | Set.member tn nonDeferred = pure ()
      | otherwise = case Map.lookup tn schemaMap of
          Just s -> record ItToolSchemas "" (estimateSchemaTokens s)
          Nothing -> pure ()

    nonDeferred = case toolSearch of
      Nothing -> Set.fromList (map (.name) schemas)
      Just neverDefer -> neverDefer

-- | Normalize an EventSource to a metrics label
metricsSource :: EventSource -> Text
metricsSource (TelegramSource _) = "telegram"
metricsSource (WebhookSource n) = "webhook:" <> n
metricsSource (CronSource n) = "cron:" <> n

-- | Build an 'AgentObserver' that records metrics to a 'MetricsStore'.
-- The @model@ and @source@ labels are baked in at construction time.
metricsObserver :: MetricsStore -> Text -> Text -> AgentObserver
metricsObserver store model source =
  AgentObserver
    { onInputEstimate = recordInputBreakdown store model source,
      onApiResponse = recordApiResponse store model source,
      onToolCall = recordToolCall store,
      onCompaction = recordCompaction store
    }

-- | Render all metrics in Prometheus text exposition format
renderMetrics :: MetricsStore -> Claude.ConversationStore -> Tools.ToolRegistry -> IO LBS.ByteString
renderMetrics store convStore registry = do
  cs <- readIORef store.counters
  mcpCount <- readIORef store.mcpServerCount
  now <- getCurrentTime
  convs <- convStore.allConversations
  let ts = Tools.allTools registry
      uptimeSeconds = floor (diffUTCTime now store.startTime) :: Int64
      builder =
        renderCounters cs
          <> renderConversationGauges convs
          <> renderToolGauge ts
          <> renderMCPGauge mcpCount
          <> renderUptimeGauge uptimeSeconds
  pure $ B.toLazyByteString builder

-- | Render all counter metrics
renderCounters :: Map CounterKey Int64 -> B.Builder
renderCounters cs =
  renderTokenCounters "input" cs
    <> renderTokenCounters "output" cs
    <> renderTokenCounters "cache_read" cs
    <> renderTokenCounters "cache_creation" cs
    <> renderEstimatedInputTokenCounters cs
    <> renderCostMetric cs
    <> renderApiRequestCounters cs
    <> renderToolCallCounters cs
    <> renderCompactionCounter cs

-- | Render token counters for a given suffix
renderTokenCounters :: Text -> Map CounterKey Int64 -> B.Builder
renderTokenCounters suffix cs =
  let metricName = "elwood_" <> suffix <> "_tokens_total"
      matching = [(m, source, v) | (CkTokens s m source, v) <- Map.toList cs, s == suffix]
   in if null matching
        then mempty
        else
          helpLine metricName ("Total " <> suffix <> " tokens used")
            <> typeLine metricName "counter"
            <> mconcat
              [ metricLine metricName [("model", m), ("source", source)] v
              | (m, source, v) <- matching
              ]

-- | Render estimated input token counters by content type
renderEstimatedInputTokenCounters :: Map CounterKey Int64 -> B.Builder
renderEstimatedInputTokenCounters cs =
  let metricName = "elwood_estimated_input_tokens_total"
      matching =
        [ (m, source, inputTokenTypeLabel typ, tool, v)
        | (CkEstimatedInputTokens m source typ tool, v) <- Map.toList cs
        ]
   in if null matching
        then mempty
        else
          helpLine metricName "Estimated input tokens by content type"
            <> typeLine metricName "counter"
            <> mconcat
              [ metricLine metricName (labels m source typ tool) v
              | (m, source, typ, tool, v) <- matching
              ]
  where
    labels m source typ tool
      | T.null tool = [("model", m), ("source", source), ("type", typ)]
      | otherwise = [("model", m), ("source", source), ("type", typ), ("tool", tool)]

-- | Render API request counters
renderApiRequestCounters :: Map CounterKey Int64 -> B.Builder
renderApiRequestCounters cs =
  let metricName = "elwood_api_requests_total"
      matching = [(m, source, reason, v) | (CkApiRequests m source reason, v) <- Map.toList cs]
   in if null matching
        then mempty
        else
          helpLine metricName "Total API requests"
            <> typeLine metricName "counter"
            <> mconcat
              [ metricLine metricName [("model", m), ("source", source), ("stop_reason", reason)] v
              | (m, source, reason, v) <- matching
              ]

-- | Render tool call counters
renderToolCallCounters :: Map CounterKey Int64 -> B.Builder
renderToolCallCounters cs =
  let metricName = "elwood_tool_calls_total"
      matching = [(n, v) | (CkToolCalls n, v) <- Map.toList cs]
   in if null matching
        then mempty
        else
          helpLine metricName "Total tool calls"
            <> typeLine metricName "counter"
            <> mconcat
              [ metricLine metricName [("tool", n)] v
              | (n, v) <- matching
              ]

-- | Render compaction counter
renderCompactionCounter :: Map CounterKey Int64 -> B.Builder
renderCompactionCounter cs =
  let metricName = "elwood_compactions_total"
      value = Map.findWithDefault 0 CkCompactions cs
   in if value == 0
        then mempty
        else
          helpLine metricName "Total conversation compactions"
            <> typeLine metricName "counter"
            <> metricLine metricName [] value

-- | Render conversation gauges
renderConversationGauges :: Map Text Claude.Conversation -> B.Builder
renderConversationGauges convs
  | Map.null convs = mempty
  | otherwise =
      helpLine "elwood_conversation_messages" "Number of messages in conversation"
        <> typeLine "elwood_conversation_messages" "gauge"
        <> mconcat
          [ metricLine "elwood_conversation_messages" [("session", conv.sessionId)] (fromIntegral (length conv.messages))
          | conv <- Map.elems convs
          ]
        <> helpLine "elwood_conversation_estimated_tokens" "Estimated tokens in conversation"
        <> typeLine "elwood_conversation_estimated_tokens" "gauge"
        <> mconcat
          [ metricLine "elwood_conversation_estimated_tokens" [("session", conv.sessionId)] (fromIntegral (estimateMessageTokens conv.messages))
          | conv <- Map.elems convs
          ]

-- | Render tools registered gauge
renderToolGauge :: [Tools.Tool] -> B.Builder
renderToolGauge ts =
  helpLine "elwood_tools_registered" "Number of registered tools"
    <> typeLine "elwood_tools_registered" "gauge"
    <> metricLine "elwood_tools_registered" [] (fromIntegral (length ts))

-- | Render MCP server count gauge
renderMCPGauge :: Int -> B.Builder
renderMCPGauge count =
  helpLine "elwood_mcp_servers_active" "Number of active MCP servers"
    <> typeLine "elwood_mcp_servers_active" "gauge"
    <> metricLine "elwood_mcp_servers_active" [] (fromIntegral count)

-- | Render uptime gauge
renderUptimeGauge :: Int64 -> B.Builder
renderUptimeGauge seconds =
  helpLine "elwood_uptime_seconds" "Time since process start in seconds"
    <> typeLine "elwood_uptime_seconds" "gauge"
    <> metricLine "elwood_uptime_seconds" [] seconds

-- | Build a # HELP line
helpLine :: Text -> Text -> B.Builder
helpLine n desc =
  B.byteString "# HELP "
    <> B.byteString (TE.encodeUtf8 n)
    <> B.char7 ' '
    <> B.byteString (TE.encodeUtf8 desc)
    <> B.char7 '\n'

-- | Build a # TYPE line
typeLine :: Text -> Text -> B.Builder
typeLine n ty =
  B.byteString "# TYPE "
    <> B.byteString (TE.encodeUtf8 n)
    <> B.char7 ' '
    <> B.byteString (TE.encodeUtf8 ty)
    <> B.char7 '\n'

-- | Build a metric line with optional labels
metricLine :: Text -> [(Text, Text)] -> Int64 -> B.Builder
metricLine n labels value =
  B.byteString (TE.encodeUtf8 n)
    <> renderLabels labels
    <> B.char7 ' '
    <> B.int64Dec value
    <> B.char7 '\n'

-- | Render label set
renderLabels :: [(Text, Text)] -> B.Builder
renderLabels [] = mempty
renderLabels labels =
  B.char7 '{'
    <> mconcat (intersperse (B.char7 ',') (map renderLabel labels))
    <> B.char7 '}'
  where
    renderLabel (k, v) =
      B.byteString (TE.encodeUtf8 k)
        <> B.byteString "=\""
        <> B.byteString (TE.encodeUtf8 (escapeLabelValue v))
        <> B.char7 '"'

-- | Escape a label value for Prometheus text format
escapeLabelValue :: Text -> Text
escapeLabelValue = T.concatMap escape
  where
    escape '\\' = "\\\\"
    escape '"' = "\\\""
    escape '\n' = "\\n"
    escape c = T.singleton c

-- | Estimate the number of tokens in a message list (JSON length / 4)
estimateMessageTokens :: [Claude.ClaudeMessage] -> Int
estimateMessageTokens msgs =
  let jsonBytes = LBS.length $ encode msgs
   in fromIntegral jsonBytes `div` 4

-- | Intersperse an element between list items
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x : xs) = x : sep : intersperse sep xs

-- | Pricing per model family (all rates in $/MTok)
data ModelPricing = ModelPricing
  { inputPerMTok :: Double,
    outputPerMTok :: Double,
    cacheWritePerMTok :: Double,
    cacheReadPerMTok :: Double
  }

-- | Prefix-matched pricing table. First match wins, so more specific prefixes come first.
pricingTable :: [(Text, ModelPricing)]
pricingTable =
  [ ("claude-opus-4-6", ModelPricing 5 25 6.25 0.50),
    ("claude-opus-4-5", ModelPricing 5 25 6.25 0.50),
    ("claude-opus-4-1", ModelPricing 15 75 18.75 1.50),
    ("claude-opus-4", ModelPricing 15 75 18.75 1.50),
    ("claude-sonnet-4", ModelPricing 3 15 3.75 0.30),
    ("claude-sonnet-3", ModelPricing 3 15 3.75 0.30),
    ("claude-haiku-4", ModelPricing 1 5 1.25 0.10),
    ("claude-3-5-haiku", ModelPricing 1 5 1.25 0.10),
    ("claude-3-5-sonnet", ModelPricing 3 15 3.75 0.30),
    ("claude-3-opus", ModelPricing 15 75 18.75 1.50),
    ("claude-3-haiku", ModelPricing 0.25 1.25 0.30 0.03)
  ]

-- | Look up pricing for a model ID by prefix match
lookupPricing :: Text -> Maybe ModelPricing
lookupPricing model = snd <$> find (\(prefix, _) -> prefix `T.isPrefixOf` model) pricingTable

-- | Render cost metric derived from token counters and pricing table
renderCostMetric :: Map CounterKey Int64 -> B.Builder
renderCostMetric cs =
  let -- Collect all distinct (model, source) pairs from token counters
      pairs = nub [(m, source) | (CkTokens _ m source, _) <- Map.toList cs]
      -- Compute cost lines for models with known pricing
      costLines =
        [ (m, source, microDollars)
        | (m, source) <- pairs,
          Just pricing <- [lookupPricing m],
          let get suffix = fromIntegral (Map.findWithDefault 0 (CkTokens suffix m source) cs) :: Double
              input = get "input"
              output = get "output"
              cacheWrite = get "cache_creation"
              cacheRead = get "cache_read"
              -- tokens * rate_per_MTok = cost in microdollars
              microDollars =
                input * pricing.inputPerMTok
                  + output * pricing.outputPerMTok
                  + cacheWrite * pricing.cacheWritePerMTok
                  + cacheRead * pricing.cacheReadPerMTok
        ]
   in if null costLines
        then mempty
        else
          helpLine metricName "Approximate cumulative API cost in USD"
            <> typeLine metricName "counter"
            <> mconcat
              [ metricLineDouble metricName [("model", m), ("source", source)] (microDollars / 1e6)
              | (m, source, microDollars) <- costLines
              ]
  where
    metricName = "elwood_cost_dollars"

-- | Build a metric line with a Double value (6 decimal places)
metricLineDouble :: Text -> [(Text, Text)] -> Double -> B.Builder
metricLineDouble n labels value =
  B.byteString (TE.encodeUtf8 n)
    <> renderLabels labels
    <> B.char7 ' '
    <> B.string7 (printf "%.6f" value)
    <> B.char7 '\n'
