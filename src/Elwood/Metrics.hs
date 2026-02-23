{-# LANGUAGE StrictData #-}

module Elwood.Metrics
  ( -- * Store
    MetricsStore,
    newMetricsStore,

    -- * Recording
    recordApiResponse,
    recordToolCall,
    recordCompaction,

    -- * Source Labels
    metricsSource,

    -- * Rendering
    renderMetrics,
  )
where

import Data.Aeson (encode)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Elwood.Claude.Conversation (ConversationStore, allConversations)
import Elwood.Claude.Types (ClaudeMessage, Conversation (..), StopReason, Usage (..), stopReasonToText)
import Elwood.Event.Types (EventSource (..))
import Elwood.Tools.Registry (ToolRegistry, allTools)
import Elwood.Tools.Types (Tool (..))

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
  deriving stock (Eq, Ord, Show)

-- | Thread-safe metrics store
newtype MetricsStore = MetricsStore
  { msCounters :: IORef (Map CounterKey Int64)
  }

-- | Create an empty metrics store
newMetricsStore :: IO MetricsStore
newMetricsStore = MetricsStore <$> newIORef Map.empty

-- | Increment a counter by a given amount
incrementCounter :: MetricsStore -> CounterKey -> Int64 -> IO ()
incrementCounter store key delta =
  atomicModifyIORef' (msCounters store) $ \m ->
    (Map.insertWith (+) key delta m, ())

-- | Record metrics from an API response
recordApiResponse :: MetricsStore -> Text -> Text -> StopReason -> Usage -> IO ()
recordApiResponse store model source stopReason usage = do
  let reasonText = stopReasonToText stopReason
  incrementCounter store (CkTokens "input" model source) (fromIntegral (usageInputTokens usage))
  incrementCounter store (CkTokens "output" model source) (fromIntegral (usageOutputTokens usage))
  incrementCounter store (CkTokens "cache_read" model source) (fromIntegral (usageCacheReadInputTokens usage))
  incrementCounter store (CkTokens "cache_creation" model source) (fromIntegral (usageCacheCreationInputTokens usage))
  incrementCounter store (CkApiRequests model source reasonText) 1

-- | Record a tool call
recordToolCall :: MetricsStore -> Text -> IO ()
recordToolCall store toolName =
  incrementCounter store (CkToolCalls toolName) 1

-- | Record a compaction event
recordCompaction :: MetricsStore -> IO ()
recordCompaction store =
  incrementCounter store CkCompactions 1

-- | Normalize an EventSource to a metrics label
metricsSource :: EventSource -> Text
metricsSource (TelegramSource _) = "telegram"
metricsSource (WebhookSource name) = "webhook:" <> name
metricsSource (CronSource name) = "cron:" <> name

-- | Render all metrics in Prometheus text exposition format
renderMetrics :: MetricsStore -> ConversationStore -> ToolRegistry -> Int -> IO LBS.ByteString
renderMetrics store convStore registry mcpServerCount = do
  counters <- readIORef (msCounters store)
  convs <- allConversations convStore
  let tools = allTools registry
      builder =
        renderCounters counters
          <> renderConversationGauges convs
          <> renderToolGauge tools
          <> renderMCPGauge mcpServerCount
  pure $ B.toLazyByteString builder

-- | Render all counter metrics
renderCounters :: Map CounterKey Int64 -> B.Builder
renderCounters counters =
  renderTokenCounters "input" counters
    <> renderTokenCounters "output" counters
    <> renderTokenCounters "cache_read" counters
    <> renderTokenCounters "cache_creation" counters
    <> renderApiRequestCounters counters
    <> renderToolCallCounters counters
    <> renderCompactionCounter counters

-- | Render token counters for a given suffix
renderTokenCounters :: Text -> Map CounterKey Int64 -> B.Builder
renderTokenCounters suffix counters =
  let metricName = "elwood_" <> suffix <> "_tokens_total"
      matching = [(model, source, v) | (CkTokens s model source, v) <- Map.toList counters, s == suffix]
   in if null matching
        then mempty
        else
          helpLine metricName ("Total " <> suffix <> " tokens used")
            <> typeLine metricName "counter"
            <> mconcat
              [ metricLine metricName [("model", model), ("source", source)] v
              | (model, source, v) <- matching
              ]

-- | Render API request counters
renderApiRequestCounters :: Map CounterKey Int64 -> B.Builder
renderApiRequestCounters counters =
  let metricName = "elwood_api_requests_total"
      matching = [(model, source, reason, v) | (CkApiRequests model source reason, v) <- Map.toList counters]
   in if null matching
        then mempty
        else
          helpLine metricName "Total API requests"
            <> typeLine metricName "counter"
            <> mconcat
              [ metricLine metricName [("model", model), ("source", source), ("stop_reason", reason)] v
              | (model, source, reason, v) <- matching
              ]

-- | Render tool call counters
renderToolCallCounters :: Map CounterKey Int64 -> B.Builder
renderToolCallCounters counters =
  let metricName = "elwood_tool_calls_total"
      matching = [(name, v) | (CkToolCalls name, v) <- Map.toList counters]
   in if null matching
        then mempty
        else
          helpLine metricName "Total tool calls"
            <> typeLine metricName "counter"
            <> mconcat
              [ metricLine metricName [("tool", name)] v
              | (name, v) <- matching
              ]

-- | Render compaction counter
renderCompactionCounter :: Map CounterKey Int64 -> B.Builder
renderCompactionCounter counters =
  let metricName = "elwood_compactions_total"
      value = Map.findWithDefault 0 CkCompactions counters
   in if value == 0
        then mempty
        else
          helpLine metricName "Total conversation compactions"
            <> typeLine metricName "counter"
            <> metricLine metricName [] value

-- | Render conversation gauges
renderConversationGauges :: Map Text Conversation -> B.Builder
renderConversationGauges convs
  | Map.null convs = mempty
  | otherwise =
      helpLine "elwood_conversation_messages" "Number of messages in conversation"
        <> typeLine "elwood_conversation_messages" "gauge"
        <> mconcat
          [ metricLine "elwood_conversation_messages" [("session", convSessionId conv)] (fromIntegral (length (convMessages conv)))
          | conv <- Map.elems convs
          ]
        <> helpLine "elwood_conversation_estimated_tokens" "Estimated tokens in conversation"
        <> typeLine "elwood_conversation_estimated_tokens" "gauge"
        <> mconcat
          [ metricLine "elwood_conversation_estimated_tokens" [("session", convSessionId conv)] (fromIntegral (estimateMessageTokens (convMessages conv)))
          | conv <- Map.elems convs
          ]

-- | Render tools registered gauge
renderToolGauge :: [Tool] -> B.Builder
renderToolGauge tools =
  helpLine "elwood_tools_registered" "Number of registered tools"
    <> typeLine "elwood_tools_registered" "gauge"
    <> metricLine "elwood_tools_registered" [] (fromIntegral (length tools))

-- | Render MCP server count gauge
renderMCPGauge :: Int -> B.Builder
renderMCPGauge count =
  helpLine "elwood_mcp_servers_active" "Number of active MCP servers"
    <> typeLine "elwood_mcp_servers_active" "gauge"
    <> metricLine "elwood_mcp_servers_active" [] (fromIntegral count)

-- | Build a # HELP line
helpLine :: Text -> Text -> B.Builder
helpLine name desc =
  B.byteString "# HELP "
    <> B.byteString (TE.encodeUtf8 name)
    <> B.char7 ' '
    <> B.byteString (TE.encodeUtf8 desc)
    <> B.char7 '\n'

-- | Build a # TYPE line
typeLine :: Text -> Text -> B.Builder
typeLine name ty =
  B.byteString "# TYPE "
    <> B.byteString (TE.encodeUtf8 name)
    <> B.char7 ' '
    <> B.byteString (TE.encodeUtf8 ty)
    <> B.char7 '\n'

-- | Build a metric line with optional labels
metricLine :: Text -> [(Text, Text)] -> Int64 -> B.Builder
metricLine name labels value =
  B.byteString (TE.encodeUtf8 name)
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
estimateMessageTokens :: [ClaudeMessage] -> Int
estimateMessageTokens msgs =
  let jsonBytes = LBS.length $ encode msgs
   in fromIntegral jsonBytes `div` 4

-- | Intersperse an element between list items
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x : xs) = x : sep : intersperse sep xs
