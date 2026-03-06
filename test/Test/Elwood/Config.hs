module Test.Elwood.Config (tests) where

import Data.Aeson (Value (..), object, (.=))
import Elwood.AgentSettings (AgentPreset (..), AgentProfile (..), ToolSearchConfig (..))
import Elwood.Config
  ( CompactionConfig (..),
    CompactionStrategy (..),
    Config (..),
    TelegramChatConfig (..),
    loadConfig,
  )
import Elwood.Event.Types (DeliveryTarget (..), SessionConfig (..))
import Elwood.Positive (unsafePositive)
import Elwood.Thinking (ThinkingEffort (..), ThinkingLevel (..), parseThinkingLevel)
import Elwood.Webhook.Types (WebhookConfig (..), WebhookServerConfig (..))
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
                { tokenThreshold = unsafePositive 50000,
                  model = "claude-3-5-haiku-20241022",
                  prompt = Nothing,
                  strategy = KeepLastTurns (unsafePositive 10)
                }
        cc.tokenThreshold @?= unsafePositive 50000
        cc.model @?= "claude-3-5-haiku-20241022"
        cc.strategy @?= KeepLastTurns (unsafePositive 10)
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
        parseThinkingLevel (Bool False) @?= Right ThinkingOff,
      testCase "parseThinkingLevel rejects string values" $ do
        case parseThinkingLevel (String "off") of
          Left _ -> pure ()
          Right _ -> assertFailure "Expected Left for string input",
      testCase "parseThinkingLevel parses adaptive object" $ do
        parseThinkingLevel (object ["type" .= ("adaptive" :: String), "effort" .= ("low" :: String)])
          @?= Right (ThinkingAdaptive EffortLow)
        parseThinkingLevel (object ["type" .= ("adaptive" :: String), "effort" .= ("medium" :: String)])
          @?= Right (ThinkingAdaptive EffortMedium)
        parseThinkingLevel (object ["type" .= ("adaptive" :: String), "effort" .= ("high" :: String)])
          @?= Right (ThinkingAdaptive EffortHigh),
      testCase "parseThinkingLevel defaults effort to medium" $ do
        parseThinkingLevel (object ["type" .= ("adaptive" :: String)])
          @?= Right (ThinkingAdaptive EffortMedium),
      testCase "parseThinkingLevel parses fixed object" $ do
        parseThinkingLevel (object ["type" .= ("fixed" :: String), "budget_tokens" .= (4096 :: Int)])
          @?= Right (ThinkingBudget 4096)
        parseThinkingLevel (object ["type" .= ("fixed" :: String), "budget_tokens" .= (1024 :: Int)])
          @?= Right (ThinkingBudget 1024),
      testCase "parseThinkingLevel parses off object" $ do
        parseThinkingLevel (object ["type" .= ("off" :: String)])
          @?= Right ThinkingOff,
      testCase "parseThinkingLevel rejects invalid fixed config" $ do
        -- Missing budgetTokens
        case parseThinkingLevel (object ["type" .= ("fixed" :: String)]) of
          Left _ -> pure ()
          Right _ -> assertFailure "Expected Left for missing budget_tokens"
        -- Zero budgetTokens
        case parseThinkingLevel (object ["type" .= ("fixed" :: String), "budget_tokens" .= (0 :: Int)]) of
          Left _ -> pure ()
          Right _ -> assertFailure "Expected Left for zero budget_tokens",
      testCase "parseThinkingLevel rejects invalid effort" $ do
        case parseThinkingLevel (object ["type" .= ("adaptive" :: String), "effort" .= ("ultra" :: String)]) of
          Left _ -> pure ()
          Right _ -> assertFailure "Expected Left for invalid effort",
      testCase "parseThinkingLevel rejects invalid type" $ do
        case parseThinkingLevel (object ["type" .= ("turbo" :: String)]) of
          Left _ -> pure ()
          Right _ -> assertFailure "Expected Left for invalid type"
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
        -- Verify agent profile fields
        config.agentProfile.model @?= "claude-sonnet-4-20250514"
        config.agentProfile.thinking @?= ThinkingOff
        config.agentProfile.toolSearch @?= ToolSearchDisabled
        length config.telegramChats @?= 1
        let tc = head config.telegramChats
        tc.id_ @?= 123456789
        tc.session @?= Named "main"
        config.compaction.tokenThreshold @?= unsafePositive 50000
        config.compaction.strategy @?= KeepLastTurns (unsafePositive 10)
        -- Verify delivery target from example config
        let webhooks = config.webhook.webhooks
        length webhooks @?= 1
        let wh = head webhooks
        wh.deliveryTarget @?= TelegramBroadcast
        -- Verify delegate defaults
        config.delegateDefaultAgent @?= AgentPreset Nothing mempty
        null config.delegateExtraAgents @?= True
        null config.delegateAllowedModels @?= True
    ]
