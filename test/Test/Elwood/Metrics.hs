module Test.Elwood.Metrics (tests) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T
import Elwood.Claude.Conversation (newInMemoryConversationStore)
import Elwood.Claude.Types (StopReason (..), Usage (..))
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
        let usage = Usage 100 50 10 20
        recordApiResponse store "claude-3" "telegram" EndTurn usage
        convStore <- newInMemoryConversationStore
        output <- renderMetrics store convStore newToolRegistry
        let s = LBS8.unpack output
        assertBool "contains input tokens" ("elwood_input_tokens_total{model=\"claude-3\",source=\"telegram\"} 100" `isIn` s)
        assertBool "contains output tokens" ("elwood_output_tokens_total{model=\"claude-3\",source=\"telegram\"} 50" `isIn` s)
        assertBool "contains cache read tokens" ("elwood_cache_read_tokens_total{model=\"claude-3\",source=\"telegram\"} 20" `isIn` s)
        assertBool "contains cache creation tokens" ("elwood_cache_creation_tokens_total{model=\"claude-3\",source=\"telegram\"} 10" `isIn` s)
        assertBool "contains api requests" ("elwood_api_requests_total{model=\"claude-3\",source=\"telegram\",stop_reason=\"end_turn\"} 1" `isIn` s),
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
        let usage1 = Usage 100 50 0 0
            usage2 = Usage 200 100 0 0
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
      testCase "MCP server count is rendered" $ do
        store <- newMetricsStore
        setMCPServerCount store 3
        convStore <- newInMemoryConversationStore
        output <- renderMetrics store convStore newToolRegistry
        let s = LBS8.unpack output
        assertBool "mcp count is 3" ("elwood_mcp_servers_active 3" `isIn` s),
      testCase "no trailing content after last newline" $ do
        store <- newMetricsStore
        convStore <- newInMemoryConversationStore
        output <- renderMetrics store convStore newToolRegistry
        assertBool "ends with newline" (LBS8.last output == '\n' || LBS.null output)
    ]

metricsSourceTests :: TestTree
metricsSourceTests =
  testGroup
    "metricsSource"
    [ testCase "telegram source strips chat ID" $
        metricsSource (TelegramSource 12345) @?= "telegram",
      testCase "webhook source includes name" $
        metricsSource (WebhookSource "deploy") @?= "webhook:deploy",
      testCase "cron source includes name" $
        metricsSource (CronSource "heartbeat") @?= "cron:heartbeat"
    ]

-- | Check if a substring is contained in a string
isIn :: String -> String -> Bool
isIn needle haystack = T.pack needle `T.isInfixOf` T.pack haystack
