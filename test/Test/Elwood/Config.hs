module Test.Elwood.Config (tests) where

import Data.Aeson (Result (..), Value (..), fromJSON, object, (.=))
import Elwood.AgentSettings (AgentPreset (..), AgentProfile (..), ToolSearchConfig (..))
import Elwood.Config
  ( CompactionConfig (..),
    CompactionStrategy (..),
    Config (..),
    PruningConfig (..),
    PruningConfigFile (..),
    PruningStrategy (..),
    TelegramChatConfig (..),
    ThinkingPruningConfig (..),
    ThinkingPruningConfigFile (..),
    ToolDirectionConfig (..),
    ToolDirectionConfigFile (..),
    ToolPruningConfig (..),
    ToolPruningConfigFile (..),
    loadConfig,
    resolvePruning,
  )
import Elwood.Event.Types (DeliveryTarget (..), SessionConfig (..))
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
      pruningResolutionTests,
      pruningFromJsonTests,
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
                  model = "claude-3-5-haiku-20241022",
                  prompt = Nothing,
                  strategy = CKeepTurns 10
                }
        cc.tokenThreshold @?= 50000
        cc.model @?= "claude-3-5-haiku-20241022"
        cc.strategy @?= CKeepTurns 10
    ]

thinkingLevelTests :: TestTree
thinkingLevelTests =
  testGroup
    "ThinkingLevel"
    [ testCase "all constructors exist" $ do
        let levels =
              [ ThinkingOff,
                ThinkingAdaptive (Just EffortLow),
                ThinkingAdaptive (Just EffortMedium),
                ThinkingAdaptive (Just EffortHigh),
                ThinkingBudget 4096
              ]
        length levels @?= 5,
      testCase "equality works" $ do
        ThinkingOff == ThinkingOff @?= True
        ThinkingOff == ThinkingAdaptive (Just EffortHigh) @?= False
        ThinkingAdaptive (Just EffortLow) == ThinkingAdaptive (Just EffortLow) @?= True
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
          @?= Right (ThinkingAdaptive (Just EffortLow))
        parseThinkingLevel (object ["type" .= ("adaptive" :: String), "effort" .= ("medium" :: String)])
          @?= Right (ThinkingAdaptive (Just EffortMedium))
        parseThinkingLevel (object ["type" .= ("adaptive" :: String), "effort" .= ("high" :: String)])
          @?= Right (ThinkingAdaptive (Just EffortHigh)),
      testCase "parseThinkingLevel omits effort when not specified" $ do
        parseThinkingLevel (object ["type" .= ("adaptive" :: String)])
          @?= Right (ThinkingAdaptive Nothing),
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

-- ---------------------------------------------------------------------------
-- resolvePruning tests
-- ---------------------------------------------------------------------------

pruningResolutionTests :: TestTree
pruningResolutionTests =
  testGroup
    "PruningConfig resolution"
    [ testCase "empty config resolves to defaults" $ do
        let cfg = resolvePruning mempty
        cfg.strategy @?= KeepTurns 3
        cfg.thinking @?= Just ThinkingPruningConfig {strategy = KeepTurns 3}
        cfg.tools.headChars @?= 500
        cfg.tools.tailChars @?= 500
        cfg.tools.strategy @?= KeepTurns 3
        cfg.tools.input.strategy @?= KeepTurns 3
        cfg.tools.input.headChars @?= 500
        cfg.tools.input.tailChars @?= 500
        cfg.tools.output.strategy @?= KeepTurns 3
        cfg.tools.output.headChars @?= 500
        cfg.tools.output.tailChars @?= 500,
      testCase "global strategy cascades to thinking and directions" $ do
        let cfg = resolvePruning (PruningConfigFile {strategy = Just (KeepTurns 7), thinking = Nothing, tools = Nothing})
        cfg.strategy @?= KeepTurns 7
        cfg.thinking @?= Just ThinkingPruningConfig {strategy = KeepTurns 7}
        cfg.tools.strategy @?= KeepTurns 7
        cfg.tools.input.strategy @?= KeepTurns 7
        cfg.tools.output.strategy @?= KeepTurns 7,
      testCase "per-direction strategy overrides tools level" $ do
        let inputCf = ToolDirectionConfigFile {strategy = Just (KeepTurns 10), headChars = Nothing, tailChars = Nothing}
            toolsCf = ToolPruningConfigFile {headChars = Nothing, tailChars = Nothing, strategy = Nothing, input = Just inputCf, output = Nothing}
            cfg = resolvePruning (PruningConfigFile {strategy = Just (KeepTurns 5), thinking = Nothing, tools = Just toolsCf})
        cfg.tools.input.strategy @?= KeepTurns 10
        cfg.tools.output.strategy @?= KeepTurns 5, -- output inherits global
      testCase "tools-level strategy overrides global" $ do
        let toolsCf = ToolPruningConfigFile {headChars = Nothing, tailChars = Nothing, strategy = Just (KeepTurns 8), input = Nothing, output = Nothing}
            cfg = resolvePruning (PruningConfigFile {strategy = Just (KeepTurns 3), thinking = Nothing, tools = Just toolsCf})
        cfg.strategy @?= KeepTurns 3 -- global unchanged
        cfg.tools.strategy @?= KeepTurns 8
        cfg.tools.input.strategy @?= KeepTurns 8 -- inherits tools
        cfg.tools.output.strategy @?= KeepTurns 8, -- inherits tools
      testCase "thinking: null disables thinking pruning" $ do
        let cfg = resolvePruning (PruningConfigFile {strategy = Nothing, thinking = Just Nothing, tools = Nothing})
        cfg.thinking @?= Nothing,
      testCase "thinking: {} inherits global strategy" $ do
        let cfg = resolvePruning (PruningConfigFile {strategy = Just (KeepTurns 5), thinking = Just (Just (ThinkingPruningConfigFile Nothing)), tools = Nothing})
        cfg.thinking @?= Just ThinkingPruningConfig {strategy = KeepTurns 5},
      testCase "thinking strategy overrides global" $ do
        let cfg = resolvePruning (PruningConfigFile {strategy = Just (KeepTurns 5), thinking = Just (Just (ThinkingPruningConfigFile (Just (KeepTurns 2)))), tools = Nothing})
        cfg.thinking @?= Just ThinkingPruningConfig {strategy = KeepTurns 2},
      testCase "tools headChars/tailChars cascade to directions" $ do
        let toolsCf = ToolPruningConfigFile {headChars = Just 100, tailChars = Just 200, strategy = Nothing, input = Nothing, output = Nothing}
            cfg = resolvePruning (PruningConfigFile {strategy = Nothing, thinking = Nothing, tools = Just toolsCf})
        cfg.tools.headChars @?= 100
        cfg.tools.tailChars @?= 200
        cfg.tools.input.headChars @?= 100
        cfg.tools.input.tailChars @?= 200
        cfg.tools.output.headChars @?= 100
        cfg.tools.output.tailChars @?= 200,
      testCase "direction headChars/tailChars override tools level" $ do
        let outputCf = ToolDirectionConfigFile {strategy = Nothing, headChars = Just 50, tailChars = Just 75}
            toolsCf = ToolPruningConfigFile {headChars = Just 100, tailChars = Just 200, strategy = Nothing, input = Nothing, output = Just outputCf}
            cfg = resolvePruning (PruningConfigFile {strategy = Nothing, thinking = Nothing, tools = Just toolsCf})
        cfg.tools.input.headChars @?= 100 -- inherits tools
        cfg.tools.input.tailChars @?= 200 -- inherits tools
        cfg.tools.output.headChars @?= 50 -- overridden
        cfg.tools.output.tailChars @?= 75 -- overridden
    ]

-- ---------------------------------------------------------------------------
-- FromJSON tests for pruning config types
-- ---------------------------------------------------------------------------

pruningFromJsonTests :: TestTree
pruningFromJsonTests =
  testGroup
    "PruningConfigFile FromJSON"
    [ testCase "parses full nested config" $ do
        let json =
              object
                [ "strategy" .= object ["keep_turns" .= (5 :: Int)],
                  "thinking" .= object ["strategy" .= object ["keep_turns" .= (2 :: Int)]],
                  "tools"
                    .= object
                      [ "head_chars" .= (100 :: Int),
                        "tail_chars" .= (200 :: Int),
                        "input" .= object ["strategy" .= object ["keep_turns" .= (10 :: Int)], "head_chars" .= (50 :: Int)],
                        "output" .= object ["tail_chars" .= (75 :: Int)]
                      ]
                ]
        case fromJSON json of
          Error err -> assertFailure $ "Parse failed: " <> err
          Success (pcf :: PruningConfigFile) -> do
            pcf.strategy @?= Just (KeepTurns 5)
            case pcf.thinking of
              Just (Just tcf) -> tcf.strategy @?= Just (KeepTurns 2)
              other -> assertFailure $ "Expected Just (Just ...), got: " <> show other
            case pcf.tools of
              Just tc -> do
                tc.headChars @?= Just 100
                tc.tailChars @?= Just 200
                case tc.input of
                  Just ic -> do
                    ic.strategy @?= Just (KeepTurns 10)
                    ic.headChars @?= Just 50
                    ic.tailChars @?= Nothing
                  Nothing -> assertFailure "Expected Just for input"
                case tc.output of
                  Just oc -> do
                    oc.strategy @?= Nothing
                    oc.headChars @?= Nothing
                    oc.tailChars @?= Just 75
                  Nothing -> assertFailure "Expected Just for output"
              Nothing -> assertFailure "Expected Just for tools",
      testCase "parses thinking: null as disabled" $ do
        let json = object ["thinking" .= Null]
        case fromJSON json of
          Error err -> assertFailure $ "Parse failed: " <> err
          Success (pcf :: PruningConfigFile) ->
            pcf.thinking @?= Just Nothing,
      testCase "absent thinking parses as Nothing (inherit defaults)" $ do
        let json = object ["strategy" .= object ["keep_turns" .= (3 :: Int)]]
        case fromJSON json of
          Error err -> assertFailure $ "Parse failed: " <> err
          Success (pcf :: PruningConfigFile) ->
            pcf.thinking @?= Nothing,
      testCase "empty object parses with all Nothing" $ do
        let json = object []
        case fromJSON json of
          Error err -> assertFailure $ "Parse failed: " <> err
          Success (pcf :: PruningConfigFile) -> do
            pcf.strategy @?= Nothing
            pcf.thinking @?= Nothing
            pcf.tools @?= Nothing,
      testCase "rejects unknown keys at top level" $ do
        let json = object ["strategy" .= object ["keep_turns" .= (3 :: Int)], "bogus" .= True]
        case fromJSON json :: Result PruningConfigFile of
          Error _ -> pure ()
          Success _ -> assertFailure "Expected parse failure for unknown key",
      testCase "rejects unknown keys in thinking" $ do
        let json = object ["thinking" .= object ["strategy" .= object ["keep_turns" .= (1 :: Int)], "bogus" .= True]]
        case fromJSON json :: Result PruningConfigFile of
          Error _ -> pure ()
          Success _ -> assertFailure "Expected parse failure for unknown key in thinking",
      testCase "rejects unknown keys in tools" $ do
        let json = object ["tools" .= object ["head_chars" .= (100 :: Int), "bogus" .= True]]
        case fromJSON json :: Result PruningConfigFile of
          Error _ -> pure ()
          Success _ -> assertFailure "Expected parse failure for unknown key in tools",
      testCase "rejects unknown keys in tools direction" $ do
        let json = object ["tools" .= object ["input" .= object ["strategy" .= object ["keep_turns" .= (1 :: Int)], "bogus" .= True]]]
        case fromJSON json :: Result PruningConfigFile of
          Error _ -> pure ()
          Success _ -> assertFailure "Expected parse failure for unknown key in tools.input"
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
        config.compaction.tokenThreshold @?= 50000
        config.compaction.strategy @?= CKeepTurns 10
        -- Verify delivery target from example config
        let webhooks = config.webhook.webhooks
        length webhooks @?= 1
        let wh = head webhooks
        wh.deliveryTarget @?= TelegramBroadcast
        -- Verify delegate defaults
        config.delegateAgent @?= AgentPreset Nothing mempty
        null config.delegateExtraAgents @?= True
        null config.delegateAllowedModels @?= True
    ]
