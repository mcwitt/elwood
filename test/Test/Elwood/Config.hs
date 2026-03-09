module Test.Elwood.Config (tests) where

import Data.Aeson (Result (..), Value (..), fromJSON, object, (.=))
import Data.Monoid (Last (..))
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
        Just cfg <- pure $ resolvePruning mempty
        Just tc <- pure cfg.tools
        cfg.strategy @?= KeepTurns 3
        cfg.thinking @?= Just ThinkingPruningConfig {strategy = KeepTurns 3}
        tc.headChars @?= 500
        tc.tailChars @?= 500
        tc.strategy @?= KeepTurns 3
        tc.input.strategy @?= KeepTurns 3
        tc.input.headChars @?= 500
        tc.input.tailChars @?= 500
        tc.output.strategy @?= KeepTurns 3
        tc.output.headChars @?= 500
        tc.output.tailChars @?= 500,
      testCase "global strategy cascades to thinking and directions" $ do
        Just cfg <- pure $ resolvePruning (PruningConfigFile {enable = Last Nothing, strategy = Last (Just (KeepTurns 7)), thinking = Last Nothing, tools = Last Nothing})
        Just tc <- pure cfg.tools
        cfg.strategy @?= KeepTurns 7
        cfg.thinking @?= Just ThinkingPruningConfig {strategy = KeepTurns 7}
        tc.strategy @?= KeepTurns 7
        tc.input.strategy @?= KeepTurns 7
        tc.output.strategy @?= KeepTurns 7,
      testCase "per-direction strategy overrides tools level" $ do
        let inputCf = ToolDirectionConfigFile {strategy = Last (Just (KeepTurns 10)), headChars = Last Nothing, tailChars = Last Nothing}
            toolsCf = ToolPruningConfigFile {enable = Last Nothing, headChars = Last Nothing, tailChars = Last Nothing, strategy = Last Nothing, input = Just inputCf, output = Nothing}
        Just cfg <- pure $ resolvePruning (PruningConfigFile {enable = Last Nothing, strategy = Last (Just (KeepTurns 5)), thinking = Last Nothing, tools = Last (Just (Just toolsCf))})
        Just tc <- pure cfg.tools
        tc.input.strategy @?= KeepTurns 10
        tc.output.strategy @?= KeepTurns 5, -- output inherits global
      testCase "tools-level strategy overrides global" $ do
        let toolsCf = ToolPruningConfigFile {enable = Last Nothing, headChars = Last Nothing, tailChars = Last Nothing, strategy = Last (Just (KeepTurns 8)), input = Nothing, output = Nothing}
        Just cfg <- pure $ resolvePruning (PruningConfigFile {enable = Last Nothing, strategy = Last (Just (KeepTurns 3)), thinking = Last Nothing, tools = Last (Just (Just toolsCf))})
        Just tc <- pure cfg.tools
        cfg.strategy @?= KeepTurns 3 -- global unchanged
        tc.strategy @?= KeepTurns 8
        tc.input.strategy @?= KeepTurns 8 -- inherits tools
        tc.output.strategy @?= KeepTurns 8, -- inherits tools
      testCase "thinking: null disables thinking pruning" $ do
        Just cfg <- pure $ resolvePruning (PruningConfigFile {enable = Last Nothing, strategy = Last Nothing, thinking = Last (Just Nothing), tools = Last Nothing})
        cfg.thinking @?= Nothing,
      testCase "thinking: {} inherits global strategy" $ do
        Just cfg <- pure $ resolvePruning (PruningConfigFile {enable = Last Nothing, strategy = Last (Just (KeepTurns 5)), thinking = Last (Just (Just (ThinkingPruningConfigFile (Last Nothing) (Last Nothing)))), tools = Last Nothing})
        cfg.thinking @?= Just ThinkingPruningConfig {strategy = KeepTurns 5},
      testCase "thinking strategy overrides global" $ do
        Just cfg <- pure $ resolvePruning (PruningConfigFile {enable = Last Nothing, strategy = Last (Just (KeepTurns 5)), thinking = Last (Just (Just (ThinkingPruningConfigFile (Last Nothing) (Last (Just (KeepTurns 2)))))), tools = Last Nothing})
        cfg.thinking @?= Just ThinkingPruningConfig {strategy = KeepTurns 2},
      testCase "tools headChars/tailChars cascade to directions" $ do
        let toolsCf = ToolPruningConfigFile {enable = Last Nothing, headChars = Last (Just 100), tailChars = Last (Just 200), strategy = Last Nothing, input = Nothing, output = Nothing}
        Just cfg <- pure $ resolvePruning (PruningConfigFile {enable = Last Nothing, strategy = Last Nothing, thinking = Last Nothing, tools = Last (Just (Just toolsCf))})
        Just tc <- pure cfg.tools
        tc.headChars @?= 100
        tc.tailChars @?= 200
        tc.input.headChars @?= 100
        tc.input.tailChars @?= 200
        tc.output.headChars @?= 100
        tc.output.tailChars @?= 200,
      testCase "direction headChars/tailChars override tools level" $ do
        let outputCf = ToolDirectionConfigFile {strategy = Last Nothing, headChars = Last (Just 50), tailChars = Last (Just 75)}
            toolsCf = ToolPruningConfigFile {enable = Last Nothing, headChars = Last (Just 100), tailChars = Last (Just 200), strategy = Last Nothing, input = Nothing, output = Just outputCf}
        Just cfg <- pure $ resolvePruning (PruningConfigFile {enable = Last Nothing, strategy = Last Nothing, thinking = Last Nothing, tools = Last (Just (Just toolsCf))})
        Just tc <- pure cfg.tools
        tc.input.headChars @?= 100 -- inherits tools
        tc.input.tailChars @?= 200 -- inherits tools
        tc.output.headChars @?= 50 -- overridden
        tc.output.tailChars @?= 75, -- overridden
      testCase "tools: null disables tool pruning" $ do
        Just cfg <- pure $ resolvePruning (PruningConfigFile {enable = Last Nothing, strategy = Last Nothing, thinking = Last Nothing, tools = Last (Just Nothing)})
        cfg.tools @?= Nothing,
      testCase "enable = false disables all pruning" $ do
        resolvePruning (PruningConfigFile {enable = Last (Just False), strategy = Last Nothing, thinking = Last Nothing, tools = Last Nothing}) @?= Nothing,
      testCase "thinking.enable = false disables thinking" $ do
        Just cfg <- pure $ resolvePruning (PruningConfigFile {enable = Last Nothing, strategy = Last Nothing, thinking = Last (Just (Just (ThinkingPruningConfigFile (Last (Just False)) (Last Nothing)))), tools = Last Nothing})
        cfg.thinking @?= Nothing,
      testCase "tools.enable = false disables tools" $ do
        let toolsCf = ToolPruningConfigFile {enable = Last (Just False), headChars = Last Nothing, tailChars = Last Nothing, strategy = Last Nothing, input = Nothing, output = Nothing}
        Just cfg <- pure $ resolvePruning (PruningConfigFile {enable = Last Nothing, strategy = Last Nothing, thinking = Last Nothing, tools = Last (Just (Just toolsCf))})
        cfg.tools @?= (Nothing :: Maybe ToolPruningConfig),
      testCase "pruning.enable = false overrides child enable = true" $ do
        let toolsCf = ToolPruningConfigFile {enable = Last (Just True), headChars = Last Nothing, tailChars = Last Nothing, strategy = Last Nothing, input = Nothing, output = Nothing}
        resolvePruning (PruningConfigFile {enable = Last (Just False), strategy = Last Nothing, thinking = Last (Just (Just (ThinkingPruningConfigFile (Last (Just True)) (Last Nothing)))), tools = Last (Just (Just toolsCf))}) @?= Nothing
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
            pcf.strategy @?= Last (Just (KeepTurns 5))
            case getLast pcf.thinking of
              Just (Just tcf) -> tcf.strategy @?= Last (Just (KeepTurns 2))
              other -> assertFailure $ "Expected Just (Just ...), got: " <> show other
            case getLast pcf.tools of
              Just (Just tc) -> do
                tc.headChars @?= Last (Just 100)
                tc.tailChars @?= Last (Just 200)
                case tc.input of
                  Just ic -> do
                    ic.strategy @?= Last (Just (KeepTurns 10))
                    ic.headChars @?= Last (Just 50)
                    ic.tailChars @?= Last Nothing
                  Nothing -> assertFailure "Expected Just for input"
                case tc.output of
                  Just oc -> do
                    oc.strategy @?= Last Nothing
                    oc.headChars @?= Last Nothing
                    oc.tailChars @?= Last (Just 75)
                  Nothing -> assertFailure "Expected Just for output"
              other -> assertFailure $ "Expected Just (Just ...) for tools, got: " <> show other,
      testCase "parses thinking: null as disabled" $ do
        let json = object ["thinking" .= Null]
        case fromJSON json of
          Error err -> assertFailure $ "Parse failed: " <> err
          Success (pcf :: PruningConfigFile) ->
            pcf.thinking @?= Last (Just Nothing),
      testCase "absent thinking parses as Nothing (inherit defaults)" $ do
        let json = object ["strategy" .= object ["keep_turns" .= (3 :: Int)]]
        case fromJSON json of
          Error err -> assertFailure $ "Parse failed: " <> err
          Success (pcf :: PruningConfigFile) ->
            pcf.thinking @?= Last Nothing,
      testCase "parses tools: null as disabled" $ do
        let json = object ["tools" .= Null]
        case fromJSON json of
          Error err -> assertFailure $ "Parse failed: " <> err
          Success (pcf :: PruningConfigFile) ->
            pcf.tools @?= Last (Just Nothing),
      testCase "absent tools parses as Nothing (inherit defaults)" $ do
        let json = object ["strategy" .= object ["keep_turns" .= (3 :: Int)]]
        case fromJSON json of
          Error err -> assertFailure $ "Parse failed: " <> err
          Success (pcf :: PruningConfigFile) ->
            pcf.tools @?= Last Nothing,
      testCase "empty object parses with all Nothing" $ do
        let json = object []
        case fromJSON json of
          Error err -> assertFailure $ "Parse failed: " <> err
          Success (pcf :: PruningConfigFile) -> do
            pcf.strategy @?= Last Nothing
            pcf.thinking @?= Last Nothing
            pcf.tools @?= Last Nothing,
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
        Just cc <- pure config.compaction
        cc.tokenThreshold @?= 50000
        cc.strategy @?= CKeepTurns 10
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
