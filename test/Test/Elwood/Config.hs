module Test.Elwood.Config (tests) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (SomeException, bracket_, try)
import Data.Aeson (Result (..), Value (..), fromJSON, object, (.=))
import Data.Aeson.Types (parseEither)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Monoid (Last (..))
import Data.Text qualified
import Data.Text.Encoding qualified as Encoding
import Data.Yaml qualified as Yaml
import Elwood.AgentSettings (AgentPreset (..), AgentProfile (..), ModelRef (..), ToolSearchConfig (..))
import Elwood.Config
  ( CompactionConfig (..),
    CompactionStrategy (..),
    Config (..),
    PruningConfig (..),
    PruningStrategy (..),
    TelegramChatConfig (..),
    ThinkingPruningConfig (..),
    ToolDirectionConfig (..),
    ToolPruningConfig (..),
    loadConfig,
    resolvePruning,
  )
import Elwood.Event.Types (DeliveryTarget (..), SessionConfig (..))
import Elwood.Positive (literal)
import Elwood.Provider (ProviderConfig (..))
import Elwood.Thinking (ThinkingDisplay (..), ThinkingEffort (..), ThinkingMode (..), ThinkingOverrides (..))
import Elwood.Webhook.Types (WebhookConfig (..), WebhookServerConfig (..))
import Paths_elwood (getDataFileName)
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Config"
    [ compactionConfigTests,
      thinkingModeTests,
      thinkingOverridesTests,
      pruningResolutionTests,
      pruningFromJsonTests,
      -- exampleConfigTests/providerTests mutate the process-global environment;
      -- envLock serializes those critical sections (see withConfig/withEnvLock).
      testGroup "env-dependent" [exampleConfigTests, providerTests, toolUseMessagesTests, boundaryTests]
    ]

compactionConfigTests :: TestTree
compactionConfigTests =
  testGroup
    "Compaction parsing"
    [ testCase "absent compaction uses defaults" $ withConfig "{}" setup defaults,
      testCase "null compaction inherits defaults" $ withConfig "compaction: null" setup defaults,
      testCase "empty compaction inherits defaults" $ withConfig "compaction: {}" setup defaults,
      testCase "enable false disables compaction" $ withConfig "compaction: {enable: false}" setup $ \cfg ->
        case cfg.compaction of
          Nothing -> pure ()
          Just _ -> assertFailure "compaction should be disabled",
      testCase "invalid disabled compaction is rejected" $
        withConfigExpectFailure "positive" "compaction: {enable: false, token_threshold: 0}" setup,
      testCase "null root pruning inherits defaults" $ withConfig "pruning: null" setup $ \cfg ->
        case cfg.pruning of
          Just pruning -> pruning.strategy @?= KeepTurns 3
          Nothing -> assertFailure "root null must inherit defaults"
    ]
  where
    setup = setEnv "ANTHROPIC_API_KEY" "test"
    defaults cfg = case cfg.compaction of
      Nothing -> assertFailure "compaction defaults should be enabled"
      Just cc -> do
        cc.tokenThreshold @?= literal @50000
        cc.model @?= ModelRef "anthropic" "claude-3-5-haiku-20241022"
        cc.prompt @?= Nothing
        cc.strategy @?= CKeepTurns (literal @10)

thinkingModeTests :: TestTree
thinkingModeTests =
  testGroup
    "ThinkingMode"
    [ testCase "parses adaptive with effort" $ do
        let json = object ["adaptive" .= object ["effort" .= ("low" :: String)]]
        fromJSON json @?= Success (Adaptive (Just EffortLow) Nothing),
      testCase "parses adaptive without effort" $ do
        let json = object ["adaptive" .= object []]
        fromJSON json @?= Success (Adaptive Nothing Nothing),
      testCase "parses adaptive with null" $ do
        let json = object ["adaptive" .= Null]
        fromJSON json @?= Success (Adaptive Nothing Nothing),
      testCase "parses adaptive with display" $ do
        let json = object ["adaptive" .= object ["effort" .= ("high" :: String), "display" .= ("updates" :: String)]]
        fromJSON json @?= Success (Adaptive (Just EffortHigh) (Just DisplayUpdates)),
      testCase "rejects unknown display" $ do
        let json = object ["adaptive" .= object ["display" .= ("verbose" :: String)]]
        case fromJSON json :: Result ThinkingMode of
          Error _ -> pure ()
          Success _ -> assertFailure "Expected failure for unknown display",
      testCase "rejects unknown adaptive key" $ do
        let json = object ["adaptive" .= object ["budget_tokens" .= (4096 :: Int)]]
        case fromJSON json :: Result ThinkingMode of
          Error _ -> pure ()
          Success _ -> assertFailure "Expected failure for unknown adaptive key",
      testCase "parses fixed with budget_tokens" $ do
        let json = object ["fixed" .= object ["budget_tokens" .= (4096 :: Int)]]
        fromJSON json @?= Success (Budget (literal @4096)),
      testCase "rejects zero budget_tokens" $ do
        let json = object ["fixed" .= object ["budget_tokens" .= (0 :: Int)]]
        case fromJSON json :: Result ThinkingMode of
          Error _ -> pure ()
          Success _ -> assertFailure "Expected failure for zero budget_tokens",
      testCase "rejects unknown mode key" $ do
        let json = object ["turbo" .= object []]
        case fromJSON json :: Result ThinkingMode of
          Error _ -> pure ()
          Success _ -> assertFailure "Expected failure for unknown mode",
      testCase "rejects multi-key object" $ do
        let json = object ["adaptive" .= object [], "fixed" .= object ["budget_tokens" .= (1 :: Int)]]
        case fromJSON json :: Result ThinkingMode of
          Error _ -> pure ()
          Success _ -> assertFailure "Expected failure for multi-key object",
      testCase "equality works" $ do
        Adaptive (Just EffortLow) Nothing == Adaptive (Just EffortLow) Nothing @?= True
        Adaptive Nothing Nothing == Adaptive Nothing Nothing @?= True
        Budget (literal @1024) == Budget (literal @1024) @?= True
        Budget (literal @1024) == Budget (literal @2048) @?= False
        Adaptive (Just EffortHigh) Nothing == Budget (literal @4096) @?= False
    ]

thinkingOverridesTests :: TestTree
thinkingOverridesTests =
  testGroup
    "ThinkingOverrides"
    [ testCase "parses enable + mode" $ do
        let json = object ["enable" .= True, "mode" .= object ["adaptive" .= object ["effort" .= ("medium" :: String)]]]
        case fromJSON json of
          Error err -> assertFailure $ "Parse failed: " <> err
          Success (ovr :: ThinkingOverrides) -> do
            ovr.enable @?= Last (Just True)
            ovr.mode @?= Last (Just (Adaptive (Just EffortMedium) Nothing)),
      testCase "parses enable only" $ do
        let json = object ["enable" .= False]
        case fromJSON json of
          Error err -> assertFailure $ "Parse failed: " <> err
          Success (ovr :: ThinkingOverrides) -> do
            ovr.enable @?= Last (Just False)
            ovr.mode @?= Last Nothing,
      testCase "parses empty object" $ do
        let json = object []
        case fromJSON json of
          Error err -> assertFailure $ "Parse failed: " <> err
          Success (ovr :: ThinkingOverrides) -> do
            ovr.enable @?= Last Nothing
            ovr.mode @?= Last Nothing
    ]

-- ---------------------------------------------------------------------------
-- resolvePruning tests
-- ---------------------------------------------------------------------------

-- Exercise parsing and resolution together.
pruningResolutionTests :: TestTree
pruningResolutionTests =
  testGroup
    "Pruning resolution"
    [ check "empty config resolves defaults" "{}" $ \cfg -> do
        cfg.strategy @?= KeepTurns 3
        cfg.thinking @?= Just (ThinkingPruningConfig (KeepTurns 3))
        Just tc <- pure cfg.tools
        tc.input @?= ToolDirectionConfig (KeepTurns 3) 500 500
        tc.output @?= tc.input,
      check "global retention cascades" "{strategy: {keep_turns: 7}}" $ \cfg -> do
        cfg.thinking @?= Just (ThinkingPruningConfig (KeepTurns 7))
        Just tc <- pure cfg.tools
        tc.input.strategy @?= KeepTurns 7
        tc.output.strategy @?= KeepTurns 7,
      check "direction retention overrides tools" "{strategy: {keep_turns: 5}, tools: {input: {strategy: {keep_turns: 10}}}}" $ \cfg -> do
        Just tc <- pure cfg.tools
        tc.input.strategy @?= KeepTurns 10
        tc.output.strategy @?= KeepTurns 5,
      check "tool retention overrides global" "{strategy: {keep_turns: 3}, tools: {strategy: {keep_turns: 8}}}" $ \cfg -> do
        cfg.strategy @?= KeepTurns 3
        Just tc <- pure cfg.tools
        tc.input.strategy @?= KeepTurns 8
        tc.output.strategy @?= KeepTurns 8,
      check "thinking null disables" "{thinking: null}" $ \cfg -> cfg.thinking @?= Nothing,
      check "thinking empty inherits" "{strategy: {keep_turns: 5}, thinking: {}}" $ \cfg -> cfg.thinking @?= Just (ThinkingPruningConfig (KeepTurns 5)),
      check "thinking retention overrides" "{strategy: {keep_turns: 5}, thinking: {strategy: {keep_turns: 2}}}" $ \cfg -> cfg.thinking @?= Just (ThinkingPruningConfig (KeepTurns 2)),
      check "tool character counts cascade" "{tools: {head_chars: 100, tail_chars: 200}}" $ \cfg -> do
        Just tc <- pure cfg.tools
        tc.input @?= ToolDirectionConfig (KeepTurns 3) 100 200
        tc.output @?= tc.input,
      check "direction character counts override" "{tools: {head_chars: 100, tail_chars: 200, output: {head_chars: 50, tail_chars: 75}}}" $ \cfg -> do
        Just tc <- pure cfg.tools
        tc.input @?= ToolDirectionConfig (KeepTurns 3) 100 200
        tc.output @?= ToolDirectionConfig (KeepTurns 3) 50 75,
      check "tools null disables" "{tools: null}" $ \cfg -> cfg.tools @?= Nothing,
      testCase "enable false disables all" $ parsePruning "{enable: false}" @?= Right Nothing,
      check "thinking enable false disables" "{thinking: {enable: false}}" $ \cfg -> cfg.thinking @?= Nothing,
      check "tools enable false disables" "{tools: {enable: false}}" $ \cfg -> cfg.tools @?= Nothing,
      testCase "global disable dominates child enable" $ parsePruning "{enable: false, thinking: {enable: true}, tools: {enable: true}}" @?= Right Nothing
    ]
  where
    check name yaml action = testCase name $ either assertFailure (maybe (assertFailure "unexpected disabled pruning") action) (parsePruning yaml)

parsePruning :: Data.Text.Text -> Either String (Maybe PruningConfig)
parsePruning yaml = either (Left . show) (parseEither resolvePruning) (Yaml.decodeEither' (Encoding.encodeUtf8 yaml))

pruningFromJsonTests :: TestTree
pruningFromJsonTests =
  testGroup
    "Pruning boundary"
    [ testCase "full nested config resolves every inherited field" $ do
        let parsed = parsePruning "{strategy: {keep_turns: 5}, thinking: {strategy: {keep_turns: 2}}, tools: {head_chars: 100, tail_chars: 200, input: {strategy: {keep_turns: 10}, head_chars: 50}, output: {tail_chars: 75}}}"
        case parsed of
          Right (Just cfg) -> do
            cfg.thinking @?= Just (ThinkingPruningConfig (KeepTurns 2))
            Just tc <- pure cfg.tools
            tc.input @?= ToolDirectionConfig (KeepTurns 10) 50 200
            tc.output @?= ToolDirectionConfig (KeepTurns 5) 100 75
          other -> assertFailure (show other),
      testCase "null thinking is observably different from missing" $ assertBool "null disables" (parsePruning "{thinking: null}" /= parsePruning "{}"),
      testCase "missing thinking inherits defaults" $ parsePruning "{thinking: {}}" @?= parsePruning "{}",
      testCase "null tools is observably different from missing" $ assertBool "null disables" (parsePruning "{tools: null}" /= parsePruning "{}"),
      testCase "missing tools inherits defaults" $ parsePruning "{tools: {}}" @?= parsePruning "{}",
      testCase "empty direction inherits defaults" $ parsePruning "{tools: {input: {}, output: {}}}" @?= parsePruning "{}",
      rejects "unknown root key" "{bogus: true}",
      rejects "unknown thinking key" "{thinking: {bogus: true}}",
      rejects "unknown tools key" "{tools: {bogus: true}}",
      rejects "unknown direction key" "{tools: {input: {bogus: true}}}",
      rejects "invalid disabled child still rejected" "{enable: false, tools: {head_chars: -1}}"
    ]
  where
    rejects name yaml = testCase name $ case parsePruning yaml of
      Left _ -> pure ()
      Right _ -> assertFailure "accepted invalid pruning config"

exampleConfigTests :: TestTree
exampleConfigTests =
  testGroup
    "config.yaml.example"
    [ testCase "loads successfully" $ withEnvLock $ do
        -- Set required env vars with dummy values
        setEnv "TELEGRAM_BOT_TOKEN" "test-token"
        setEnv "ANTHROPIC_API_KEY" "test-key"
        examplePath <- getDataFileName "config.yaml.example"
        config <- loadConfig examplePath
        -- Clean up env vars
        unsetEnv "TELEGRAM_BOT_TOKEN"
        unsetEnv "ANTHROPIC_API_KEY"
        -- Verify agent profile fields
        config.agentProfile.model @?= ModelRef "anthropic" "claude-sonnet-4-20250514"
        config.agentProfile.thinking @?= Nothing
        config.agentProfile.toolSearch @?= ToolSearchDisabled
        length config.telegramChats @?= 1
        let tc = head config.telegramChats
        tc.id_ @?= 123456789
        tc.session @?= Named "main"
        Just cc <- pure config.compaction
        cc.tokenThreshold @?= literal @50000
        cc.strategy @?= CKeepTurns (literal @10)
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

-- | Serializes tests that mutate the process-global environment so they do not
-- race when tasty runs the suite in parallel.
{-# NOINLINE envLock #-}
envLock :: MVar ()
envLock = unsafePerformIO (newMVar ())

-- | Run an action while holding 'envLock'.
withEnvLock :: IO a -> IO a
withEnvLock = withMVar envLock . const

-- | Write a temp config file, run env setup, load it, and assert on the result.
withConfig :: String -> IO () -> (Config -> IO ()) -> IO ()
withConfig yaml envSetup k =
  withEnvLock $
    withSystemTempDirectory "elwood-cfg" $ \dir -> do
      let path = dir </> "config.yaml"
      writeFile path yaml
      bracket_
        (setEnv "TELEGRAM_BOT_TOKEN" "test-token" >> envSetup)
        (unsetEnv "TELEGRAM_BOT_TOKEN")
        (loadConfig path >>= k)

-- | Write a temp config file, run env setup, and assert that loadConfig fails
-- with an exception whose 'show' contains the given substring.
withConfigExpectFailure :: String -> String -> IO () -> IO ()
withConfigExpectFailure expectedSubstr yaml envSetup =
  withEnvLock $
    withSystemTempDirectory "elwood-cfg" $ \dir -> do
      let path = dir </> "config.yaml"
      writeFile path yaml
      bracket_
        (setEnv "TELEGRAM_BOT_TOKEN" "test-token" >> envSetup)
        (unsetEnv "TELEGRAM_BOT_TOKEN")
        ( do
            result <- try (loadConfig path) :: IO (Either SomeException Config)
            case result of
              Left e -> assertBool ("expected substring " <> show expectedSubstr <> " in: " <> show e) (expectedSubstr `isInfixOf` show e)
              Right _ -> assertFailure "expected loadConfig to fail"
        )

toolUseMessagesTests :: TestTree
toolUseMessagesTests =
  testGroup
    "tool_use_messages"
    [ testCase "defaults to off" $
        withConfig "agent:\n  model: claude-opus-4-8\n" (setEnv "ANTHROPIC_API_KEY" "k") $ \cfg ->
          cfg.toolUseMessages @?= False,
      testCase "explicit true enables" $
        withConfig "tool_use_messages: true\nagent:\n  model: claude-opus-4-8\n" (setEnv "ANTHROPIC_API_KEY" "k") $ \cfg ->
          cfg.toolUseMessages @?= True,
      testCase "per-chat tool_use_messages parses and resolves"
        $ withConfig
          "channels:\n  telegram:\n    - id: 1\n      tool_use_messages: true\n    - id: 2\nagent:\n  model: claude-opus-4-8\n"
          (setEnv "ANTHROPIC_API_KEY" "k")
        $ \cfg -> do
          let byId i = [tc | tc <- cfg.telegramChats, tc.id_ == i]
          map (.toolUseMessages) (byId 1) @?= [Just True]
          map (.toolUseMessages) (byId 2) @?= [Nothing]
    ]

providerTests :: TestTree
providerTests =
  testGroup
    "providers"
    [ testCase "built-in anthropic exists and uses ANTHROPIC_API_KEY" $
        withConfig "agent:\n  model: claude-opus-4-8\n" (setEnv "ANTHROPIC_API_KEY" "k1") $ \cfg -> do
          case Map.lookup "anthropic" cfg.providers of
            Just p -> p.apiKey @?= Just "k1"
            Nothing -> assertFailure "missing anthropic provider",
      testCase "fully-local config needs no ANTHROPIC_API_KEY"
        $ withConfig
          "providers:\n  local:\n    base_url: http://h:9000\nagent:\n  model: qwen\n  provider: local\ncompaction:\n  enable: false\n"
          (unsetEnv "ANTHROPIC_API_KEY")
        $ \cfg -> assertBool "local provider present" (Map.member "local" cfg.providers),
      testCase "unknown provider reference fails" $
        withConfigExpectFailure "unknown provider" "agent:\n  model: x\n  provider: nope\n" (setEnv "ANTHROPIC_API_KEY" "k"),
      testCase "anthropic referenced without key fails" $
        withConfigExpectFailure "ANTHROPIC_API_KEY" "agent:\n  model: claude-opus-4-8\n" (unsetEnv "ANTHROPIC_API_KEY"),
      testCase "provider with both api_key and api_key_env fails" $
        withConfigExpectFailure
          "not both"
          "providers:\n  local:\n    base_url: http://h:9000\n    api_key: inline\n    api_key_env: SOME_VAR\nagent:\n  model: qwen\n  provider: local\ncompaction:\n  enable: false\n"
          (setEnv "ANTHROPIC_API_KEY" "k"),
      testCase "empty base_url fails" $
        withConfigExpectFailure
          "base_url must not be empty"
          "providers:\n  local:\n    base_url: \"\"\nagent:\n  model: qwen\n  provider: local\ncompaction:\n  enable: false\n"
          (setEnv "ANTHROPIC_API_KEY" "k"),
      testCase "api_key_env naming an unset variable fails" $
        withConfigExpectFailure
          "api_key_env"
          "providers:\n  local:\n    base_url: http://h:9000\n    api_key_env: ELWOOD_TEST_DEFINITELY_UNSET\nagent:\n  model: qwen\n  provider: local\ncompaction:\n  enable: false\n"
          (unsetEnv "ELWOOD_TEST_DEFINITELY_UNSET"),
      testCase "user-defined keyless anthropic loads without ANTHROPIC_API_KEY"
        $ withConfig
          "providers:\n  anthropic:\n    base_url: http://local:8080\nagent:\n  model: claude-opus-4-8\ncompaction:\n  enable: false\n"
          (unsetEnv "ANTHROPIC_API_KEY")
        $ \cfg -> case Map.lookup "anthropic" cfg.providers of
          Just p -> do
            p.baseUrl @?= "http://local:8080"
            p.apiKey @?= Nothing
          Nothing -> assertFailure "missing anthropic provider",
      testCase "empty ANTHROPIC_API_KEY is treated as missing" $
        withConfigExpectFailure
          "ANTHROPIC_API_KEY"
          "agent:\n  model: claude-opus-4-8\n"
          (setEnv "ANTHROPIC_API_KEY" "")
    ]

boundaryTests :: TestTree
boundaryTests =
  testGroup
    "strict boundaries"
    [ testCase "explicit null disables image resizing" $
        withConfig "max_image_dimension: null\n" setup $
          \cfg -> cfg.maxImageDimension @?= Nothing,
      testCase "fractional iteration budget is rejected" $
        withConfigExpectFailure "Int" "agent: { max_iterations: 1.5 }\n" setup,
      testCase "invalid regular expression fails on load" $
        withConfigExpectFailure "regex" "agent: { permissions: { dangerous_patterns: ['['] } }\n" setup,
      testCase "negative pruning length fails on load" $
        withConfigExpectFailure "nonnegative" "pruning: { tools: { head_chars: -1 } }\n" setup,
      testCase "invalid webhook port fails on load" $
        withConfigExpectFailure "port" "webhook: { port: 65536 }\n" setup,
      testCase "non-HTTP provider fails on load" $
        withConfigExpectFailure "HTTP" "providers: { bad: { base_url: 'file:///tmp/model' } }\n" setup
    ]
  where
    setup = setEnv "ANTHROPIC_API_KEY" "test"
