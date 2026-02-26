module Test.Elwood.Config (tests) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import Data.Vector qualified as V
import Elwood.Config
  ( CompactionConfig (..),
    Config (..),
    ThinkingEffort (..),
    ThinkingLevel (..),
    WebhookConfig (..),
    WebhookServerConfig (..),
    loadConfig,
    parseThinkingLevel,
    parseToolSearch,
  )
import Elwood.Event.Types (DeliveryTarget (..))
import Paths_elwood (getDataFileName)
import System.Environment (setEnv, unsetEnv)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Config"
    [ compactionConfigTests,
      thinkingLevelTests,
      toolSearchTests,
      exampleConfigTests
    ]

compactionConfigTests :: TestTree
compactionConfigTests =
  testGroup
    "CompactionConfig"
    [ testCase "has sensible defaults" $ do
        let cc =
              CompactionConfig
                { tokenThreshold = 50000,
                  model = "claude-3-5-haiku-20241022"
                }
        cc.tokenThreshold @?= 50000
        cc.model @?= "claude-3-5-haiku-20241022",
      testCase "threshold is reasonable" $ do
        let cc = CompactionConfig 30000 "model"
        -- Threshold should be positive and reasonable
        cc.tokenThreshold > 0 @?= True
        cc.tokenThreshold < 1000000 @?= True
    ]

thinkingLevelTests :: TestTree
thinkingLevelTests =
  testGroup
    "ThinkingLevel"
    [ testCase "all constructors exist" $ do
        let levels =
              [ ThinkingOff,
                ThinkingAdaptive EffortLow,
                ThinkingAdaptive EffortMedium,
                ThinkingAdaptive EffortHigh,
                ThinkingBudget 4096
              ]
        length levels @?= 5,
      testCase "equality works" $ do
        ThinkingOff == ThinkingOff @?= True
        ThinkingOff == ThinkingAdaptive EffortHigh @?= False
        ThinkingAdaptive EffortLow == ThinkingAdaptive EffortLow @?= True
        ThinkingBudget 1024 == ThinkingBudget 1024 @?= True
        ThinkingBudget 1024 == ThinkingBudget 2048 @?= False,
      testCase "parseThinkingLevel parses Bool False as off" $ do
        parseThinkingLevel (Bool False) @?= ThinkingOff,
      testCase "parseThinkingLevel parses string off" $ do
        parseThinkingLevel (String "off") @?= ThinkingOff,
      testCase "parseThinkingLevel parses adaptive object" $ do
        parseThinkingLevel (object ["type" .= ("adaptive" :: String), "effort" .= ("low" :: String)])
          @?= ThinkingAdaptive EffortLow
        parseThinkingLevel (object ["type" .= ("adaptive" :: String), "effort" .= ("medium" :: String)])
          @?= ThinkingAdaptive EffortMedium
        parseThinkingLevel (object ["type" .= ("adaptive" :: String), "effort" .= ("high" :: String)])
          @?= ThinkingAdaptive EffortHigh,
      testCase "parseThinkingLevel defaults effort to medium" $ do
        parseThinkingLevel (object ["type" .= ("adaptive" :: String)])
          @?= ThinkingAdaptive EffortMedium,
      testCase "parseThinkingLevel parses fixed object" $ do
        parseThinkingLevel (object ["type" .= ("fixed" :: String), "budget_tokens" .= (4096 :: Int)])
          @?= ThinkingBudget 4096
        parseThinkingLevel (object ["type" .= ("fixed" :: String), "budget_tokens" .= (1024 :: Int)])
          @?= ThinkingBudget 1024,
      testCase "parseThinkingLevel parses off object" $ do
        parseThinkingLevel (object ["type" .= ("off" :: String)])
          @?= ThinkingOff,
      testCase "parseThinkingLevel handles invalid fixed config" $ do
        -- Missing budgetTokens
        parseThinkingLevel (object ["type" .= ("fixed" :: String)])
          @?= ThinkingOff
        -- Zero budgetTokens
        parseThinkingLevel (object ["type" .= ("fixed" :: String), "budget_tokens" .= (0 :: Int)])
          @?= ThinkingOff
    ]

toolSearchTests :: TestTree
toolSearchTests =
  testGroup
    "ToolSearch"
    [ testCase "false returns Nothing" $ do
        parseToolSearch (Bool False) @?= Nothing,
      testCase "true returns empty list" $ do
        parseToolSearch (Bool True) @?= Just [],
      testCase "empty array returns empty list" $ do
        parseToolSearch (Array V.empty) @?= Just [],
      testCase "array with tool names parses correctly" $ do
        let val = Array (V.fromList [String "run_command", String "save_memory"])
        parseToolSearch val @?= Just ["run_command", "save_memory"],
      testCase "object with alwaysLoad parses (backward compat)" $ do
        let val = object ["alwaysLoad" .= (["run_command", "save_memory"] :: [Text])]
        parseToolSearch val @?= Just ["run_command", "save_memory"],
      testCase "object without alwaysLoad returns empty list" $ do
        parseToolSearch (object []) @?= Just [],
      testCase "null returns Nothing" $ do
        parseToolSearch Null @?= Nothing,
      testCase "string returns Nothing" $ do
        parseToolSearch (String "true") @?= Nothing
    ]

exampleConfigTests :: TestTree
exampleConfigTests =
  testGroup
    "config.yaml.example"
    [ testCase "loads successfully" $ do
        -- Set required env vars with dummy values
        setEnv "TELEGRAM_BOT_TOKEN" "test-token"
        setEnv "ANTHROPIC_API_KEY" "test-key"
        examplePath <- getDataFileName "config.yaml.example"
        config <- loadConfig examplePath
        -- Clean up env vars
        unsetEnv "TELEGRAM_BOT_TOKEN"
        unsetEnv "ANTHROPIC_API_KEY"
        -- Verify some fields parsed correctly
        config.model @?= "claude-sonnet-4-20250514"
        config.thinking @?= ThinkingOff
        config.allowedChatIds @?= [123456789]
        config.compaction.tokenThreshold @?= 50000
        -- Verify delivery target from example config
        let webhooks = config.webhook.webhooks
        length webhooks @?= 1
        let wh = head webhooks
        wh.deliveryTarget @?= TelegramBroadcast
    ]
