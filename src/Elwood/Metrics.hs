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
import Elwood.Claude.Conversation qualified as Claude
import Elwood.Claude.Types qualified as Claude
import Elwood.Event.Types (EventSource (..))
import Elwood.Tools.Registry qualified as Tools
import Elwood.Tools.Types qualified as Tools

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
  { counters :: IORef (Map CounterKey Int64)
  }

-- | Create an empty metrics store
newMetricsStore :: IO MetricsStore
newMetricsStore = MetricsStore <$> newIORef Map.empty

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

-- | Normalize an EventSource to a metrics label
metricsSource :: EventSource -> Text
metricsSource (TelegramSource _) = "telegram"
metricsSource (WebhookSource n) = "webhook:" <> n
metricsSource (CronSource n) = "cron:" <> n

-- | Render all metrics in Prometheus text exposition format
renderMetrics :: MetricsStore -> Claude.ConversationStore -> Tools.ToolRegistry -> Int -> IO LBS.ByteString
renderMetrics store convStore registry mcpServerCount = do
  cs <- readIORef store.counters
  convs <- Claude.allConversations convStore
  let ts = Tools.allTools registry
      builder =
        renderCounters cs
          <> renderConversationGauges convs
          <> renderToolGauge ts
          <> renderMCPGauge mcpServerCount
  pure $ B.toLazyByteString builder

-- | Render all counter metrics
renderCounters :: Map CounterKey Int64 -> B.Builder
renderCounters cs =
  renderTokenCounters "input" cs
    <> renderTokenCounters "output" cs
    <> renderTokenCounters "cache_read" cs
    <> renderTokenCounters "cache_creation" cs
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
