module Test.Elwood.Metrics (tests) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Text qualified as T
import Elwood.Claude.Conversation (ConversationStore (..), newInMemoryConversationStore)
import Elwood.Claude.Types (CacheTtl (..), ClaudeMessage (..), ContentBlock (..), Role (..), StopReason (..), Usage (..))
import Elwood.Event.Types (EventSource (..))
import Elwood.Metrics
  ( metricsSource,
    newMetricsStore,
    recordApiResponse,
    recordCompaction,
    recordToolCall,
    renderMetrics,
    setMCPServerCount,
  )
import Elwood.Tools.Registry (newToolRegistry)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Metrics"
    [ recordingTests,
      renderingTests,
      metricsSourceTests
    ]

recordingTests :: TestTree
recordingTests =
  testGroup
    "Recording"
    [ testCase "recordApiResponse increments counters" $ do
        store <- newMetricsStore
        -- Usage with only 5m caching (sub-object absent → all 10 attributed to 5m)
        let usage =
              Usage
                { inputTokens = 100,
                  outputTokens = 50,
                  cacheCreationInputTokens = 10,
                  cacheReadInputTokens = 20,
                  cacheCreation5mTokens = 10,
                  cacheCreation1hTokens = 0
                }
        recordApiResponse store "claude-3" "telegram" EndTurn usage
        convStore <- newInMemoryConversationStore
        output <- renderMetrics store convStore newToolRegistry
        let s = LBS8.unpack output
        assertBool "contains input tokens" ("elwood_input_tokens_total{model=\"claude-3\",source=\"telegram\"} 100" `isIn` s)
        assertBool "contains output tokens" ("elwood_output_tokens_total{model=\"claude-3\",source=\"telegram\"} 50" `isIn` s)
        assertBool "contains cache read tokens" ("elwood_cache_read_tokens_total{model=\"claude-3\",source=\"telegram\"} 20" `isIn` s)
        assertBool "contains 5m cache_creation" ("elwood_cache_creation_tokens_total{model=\"claude-3\",source=\"telegram\",cache_ttl=\"5m\"} 10" `isIn` s)
        assertBool "contains 1h cache_creation" ("elwood_cache_creation_tokens_total{model=\"claude-3\",source=\"telegram\",cache_ttl=\"1h\"} 0" `isIn` s)
        assertBool "contains api requests" ("elwood_api_requests_total{model=\"claude-3\",source=\"telegram\",stop_reason=\"end_turn\"} 1" `isIn` s),
      testCase "recordApiResponse splits 5m and 1h cache writes" $ do
        store <- newMetricsStore
        let usage =
              Usage
                { inputTokens = 2048,
                  outputTokens = 503,
                  cacheCreationInputTokens = 248,
                  cacheReadInputTokens = 1800,
                  cacheCreation5mTokens = 148,
                  cacheCreation1hTokens = 100
                }
        recordApiResponse store "claude-opus-4-7" "telegram" EndTurn usage
        convStore <- newInMemoryConversationStore
        output <- renderMetrics store convStore newToolRegistry
        let s = LBS8.unpack output
        assertBool "5m bucket" ("elwood_cache_creation_tokens_total{model=\"claude-opus-4-7\",source=\"telegram\",cache_ttl=\"5m\"} 148" `isIn` s)
        assertBool "1h bucket" ("elwood_cache_creation_tokens_total{model=\"claude-opus-4-7\",source=\"telegram\",cache_ttl=\"1h\"} 100" `isIn` s),
      testCase "recordToolCall increments tool counter" $ do
        store <- newMetricsStore
        recordToolCall store "run_command"
        recordToolCall store "run_command"
        recordToolCall store "save_memory"
        convStore <- newInMemoryConversationStore
        output <- renderMetrics store convStore newToolRegistry
        let s = LBS8.unpack output
        assertBool "run_command count is 2" ("elwood_tool_calls_total{tool=\"run_command\"} 2" `isIn` s)
        assertBool "save_memory count is 1" ("elwood_tool_calls_total{tool=\"save_memory\"} 1" `isIn` s),
      testCase "recordCompaction increments compaction counter" $ do
        store <- newMetricsStore
        recordCompaction store
        recordCompaction store
        recordCompaction store
        convStore <- newInMemoryConversationStore
        output <- renderMetrics store convStore newToolRegistry
        let s = LBS8.unpack output
        assertBool "compaction count is 3" ("elwood_compactions_total 3" `isIn` s),
      testCase "multiple API responses accumulate" $ do
        store <- newMetricsStore
        let mkUsage i o = Usage i o 0 0 0 0
            usage1 = mkUsage 100 50
            usage2 = mkUsage 200 100
        recordApiResponse store "claude-3" "telegram" EndTurn usage1
        recordApiResponse store "claude-3" "telegram" EndTurn usage2
        convStore <- newInMemoryConversationStore
        output <- renderMetrics store convStore newToolRegistry
        let s = LBS8.unpack output
        assertBool "input tokens accumulated" ("elwood_input_tokens_total{model=\"claude-3\",source=\"telegram\"} 300" `isIn` s)
        assertBool "output tokens accumulated" ("elwood_output_tokens_total{model=\"claude-3\",source=\"telegram\"} 150" `isIn` s)
    ]

renderingTests :: TestTree
renderingTests =
  testGroup
    "Rendering"
    [ testCase "empty store renders valid output" $ do
        store <- newMetricsStore
        convStore <- newInMemoryConversationStore
        output <- renderMetrics store convStore newToolRegistry
        -- Should still have gauge metrics
        let s = LBS8.unpack output
        assertBool "contains tools_registered" ("elwood_tools_registered" `isIn` s)
        assertBool "contains mcp_servers_active" ("elwood_mcp_servers_active" `isIn` s),
      testCase "output has HELP and TYPE lines" $ do
        store <- newMetricsStore
        recordToolCall store "test_tool"
        convStore <- newInMemoryConversationStore
        output <- renderMetrics store convStore newToolRegistry
        let s = LBS8.unpack output
        assertBool "has HELP line" ("# HELP elwood_tool_calls_total" `isIn` s)
        assertBool "has TYPE line" ("# TYPE elwood_tool_calls_total counter" `isIn` s),
      testCase "Claude Fable 5.1 uses reduced cache read pricing" $ do
        store <- newMetricsStore
        let usage =
              Usage
                { inputTokens = 0,
                  outputTokens = 0,
                  cacheCreationInputTokens = 0,
                  cacheReadInputTokens = 1000000,
                  cacheCreation5mTokens = 0,
                  cacheCreation1hTokens = 0
                }
        recordApiResponse store "claude-fable-5.1" "telegram" EndTurn usage
        convStore <- newInMemoryConversationStore
        output <- renderMetrics store convStore newToolRegistry
        let s = LBS8.unpack output
        assertBool
          "one million cache read tokens cost $0.25"
          ("elwood_cost_dollars{model=\"claude-fable-5.1\",source=\"telegram\"} 0.250000" `isIn` s),
      testCase "MCP server count is rendered" $ do
        store <- newMetricsStore
        setMCPServerCount store 3
        convStore <- newInMemoryConversationStore
        output <- renderMetrics store convStore newToolRegistry
        let s = LBS8.unpack output
        assertBool "mcp count is 3" ("elwood_mcp_servers_active 3" `isIn` s),
      testCase "cache_expires_at rendered for conversations" $ do
        store <- newMetricsStore
        convStore <- newInMemoryConversationStore
        convStore.appendMessages "test-session" [ClaudeMessage User [TextBlock "hi"]] (Just CacheTtl5Min)
        output <- renderMetrics store convStore newToolRegistry
        let s = LBS8.unpack output
        assertBool "contains cache_expires_at metric" ("elwood_conversation_cache_expires_at{session=\"test-session\"}" `isIn` s)
        -- Value should be a real timestamp (> year 2020 in unix seconds)
        assertBool "cache_expires_at is not zero" (not $ "elwood_conversation_cache_expires_at{session=\"test-session\"} 0" `isIn` s),
      testCase "no trailing content after last newline" $ do
        store <- newMetricsStore
        convStore <- newInMemoryConversationStore
        output <- renderMetrics store convStore newToolRegistry
        assertBool "ends with newline" (LBS8.last output == '\n' || LBS.null output),
      testCase "conversation token gauge is image-aware (excludes base64)" $ do
        -- A single image with a large base64 payload. Counting the raw base64
        -- as text (full JSON / 4) would report ~25k tokens; the gauge must use
        -- the same image-aware estimate as compaction, which strips image data
        -- and counts a flat per-image cost — so the value stays well under 5k.
        store <- newMetricsStore
        convStore <- newInMemoryConversationStore
        let bigB64 = T.replicate 100000 "A"
        convStore.appendMessages "img-session" [ClaudeMessage User [ImageBlock "image/png" bigB64]] Nothing
        output <- renderMetrics store convStore newToolRegistry
        let s = LBS8.unpack output
            val = gaugeValue "img-session" s
        assertBool "gauge present" (val >= 0)
        assertBool ("image base64 must not be counted at full size (got " <> show val <> ")") (val < 5000)
    ]

-- | Extract the integer value of the conversation-token gauge for a session
-- from rendered Prometheus output, or -1 if absent.
gaugeValue :: String -> String -> Int
gaugeValue session haystack =
  let prefix = "elwood_conversation_estimated_tokens{session=\"" <> session <> "\"} "
   in case [drop (length prefix) l | l <- lines haystack, prefix `isPrefixOf` l] of
        (v : _) -> read (takeWhile isDigit v)
        [] -> -1

metricsSourceTests :: TestTree
metricsSourceTests =
  testGroup
    "metricsSource"
    [ testCase "telegram source strips chat ID" $
        metricsSource (TelegramSource 12345) @?= "telegram",
      testCase "webhook source includes name" $
        metricsSource (WebhookSource "deploy") @?= "webhook:deploy"
    ]

-- | Check if a substring is contained in a string
isIn :: String -> String -> Bool
isIn needle haystack = T.pack needle `T.isInfixOf` T.pack haystack
