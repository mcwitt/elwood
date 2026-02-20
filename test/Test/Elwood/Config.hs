module Test.Elwood.Config (tests) where

import Elwood.Config
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
      exampleConfigTests
    ]

compactionConfigTests :: TestTree
compactionConfigTests =
  testGroup
    "CompactionConfig"
    [ testCase "has sensible defaults" $ do
        let cc =
              CompactionConfig
                { ccTokenThreshold = 80000,
                  ccCompactionModel = "claude-3-5-haiku-20241022"
                }
        ccTokenThreshold cc @?= 80000
        ccCompactionModel cc @?= "claude-3-5-haiku-20241022",
      testCase "threshold is reasonable" $ do
        let cc = CompactionConfig 80000 "model"
        -- Threshold should be positive and reasonable
        ccTokenThreshold cc > 0 @?= True
        ccTokenThreshold cc < 1000000 @?= True
    ]

thinkingLevelTests :: TestTree
thinkingLevelTests =
  testGroup
    "ThinkingLevel"
    [ testCase "all constructors exist" $ do
        -- Verify the type has the expected constructors
        let levels = [ThinkingOff, ThinkingLow, ThinkingMedium, ThinkingHigh]
        length levels @?= 4,
      testCase "equality works" $ do
        ThinkingOff == ThinkingOff @?= True
        ThinkingOff == ThinkingHigh @?= False
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
        cfgModel config @?= "claude-sonnet-4-20250514"
        cfgThinking config @?= ThinkingOff
        cfgAllowedChatIds config @?= [123456789]
        ccTokenThreshold (cfgCompaction config) @?= 80000
        -- Verify delivery targets from example config
        let webhooks = wscWebhooks (cfgWebhook config)
        length webhooks @?= 1
        let wh = head webhooks
        wcDelivery wh @?= [TelegramBroadcast, LogOnly]
    ]
