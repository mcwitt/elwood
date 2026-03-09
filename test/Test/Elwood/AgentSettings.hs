{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Elwood.AgentSettings (tests) where

import Data.Map.Strict qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.AgentSettings
  ( AgentOverrides (..),
    AgentProfile (..),
    CacheOverrides (..),
    ToolSearchConfig (..),
    agentDefaults,
    resolveProfile,
    toOverrides,
  )
import Elwood.Claude.Types (CacheTtl (..), ToolName (..))
import Elwood.Permissions
  ( PermissionConfig (..),
    PermissionConfigFile (..),
    ToolPolicy (..),
    defaultPermissionConfig,
    resolvePermissions,
  )
import Elwood.Positive qualified as P
import Elwood.Prompt (PromptInput (..))
import Elwood.Thinking (ThinkingEffort (..), ThinkingMode (..), ThinkingOverrides (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

{-# ANN module ("HLint: ignore Monoid law, left identity" :: String) #-}

{-# ANN module ("HLint: ignore Monoid law, right identity" :: String) #-}

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

tests :: TestTree
tests =
  testGroup
    "AgentSettings"
    [ semigroupLawTests,
      monoidLawTests,
      overrideTests,
      resolveTests,
      permissionsMergeTests
    ]

instance Arbitrary ThinkingEffort where
  arbitrary = elements [EffortLow, EffortMedium, EffortHigh]

instance Arbitrary ThinkingMode where
  arbitrary =
    oneof
      [ Adaptive <$> arbitrary,
        Budget . getPositive <$> arbitrary
      ]

instance Arbitrary ThinkingOverrides where
  arbitrary = ThinkingOverrides . Last <$> arbitrary <*> (Last <$> arbitrary)

instance Arbitrary CacheTtl where
  arbitrary = elements [CacheTtl5Min, CacheTtl1Hour]

instance Arbitrary CacheOverrides where
  arbitrary = CacheOverrides . Last <$> arbitrary <*> (Last <$> arbitrary)

instance Arbitrary P.Positive where
  arbitrary = fromIntegral . getPositive @Int <$> arbitrary

instance Arbitrary PromptInput where
  arbitrary =
    oneof
      [ WorkspaceFile <$> arbitrary,
        InlineText <$> arbitrary
      ]

instance Arbitrary ToolSearchConfig where
  arbitrary =
    oneof
      [ pure ToolSearchDisabled,
        ToolSearchEnabled <$> arbitrary
      ]

instance Arbitrary ToolName where
  arbitrary = ToolName <$> arbitrary

instance Arbitrary ToolPolicy where
  arbitrary = elements [PolicyAllow, PolicyAsk, PolicyDeny]

instance Arbitrary PermissionConfigFile where
  arbitrary =
    PermissionConfigFile . Last
      <$> arbitrary
      <*> (Last <$> arbitrary)
      <*> (Last <$> arbitrary)
      <*> (Last <$> arbitrary)
      <*> (Last <$> arbitrary)

instance Arbitrary AgentOverrides where
  arbitrary =
    AgentOverrides . Last
      <$> arbitrary
      <*> arbitrary
      <*> (Last <$> arbitrary)
      <*> arbitrary
      <*> (Last <$> arbitrary)
      <*> (Last <$> arbitrary)
      <*> (Last <$> arbitrary)
      <*> arbitrary

semigroupLawTests :: TestTree
semigroupLawTests =
  testGroup
    "Semigroup laws"
    [ testProperty "associativity" $ \(a :: AgentOverrides) b c ->
        (a <> b) <> c == a <> (b <> c)
    ]

monoidLawTests :: TestTree
monoidLawTests =
  testGroup
    "Monoid laws"
    [ testProperty "left identity" $ \(a :: AgentOverrides) ->
        mempty <> a == a,
      testProperty "right identity" $ \(a :: AgentOverrides) ->
        a <> mempty == a
    ]

overrideTests :: TestTree
overrideTests =
  testGroup
    "Override semantics"
    [ testCase "right-biased: later Just wins" $ do
        let a = AgentOverrides (Last (Just "model-a")) Nothing (Last Nothing) Nothing (Last Nothing) (Last Nothing) (Last Nothing) Nothing
            b = AgentOverrides (Last (Just "model-b")) Nothing (Last Nothing) Nothing (Last Nothing) (Last Nothing) (Last Nothing) Nothing
        (a <> b).model @?= Last (Just "model-b"),
      testCase "right-biased: Nothing preserves left" $ do
        let a =
              AgentOverrides
                (Last (Just "model-a"))
                (Just (ThinkingOverrides (Last (Just False)) (Last Nothing)))
                (Last (Just 10))
                (Just (CacheOverrides (Last (Just True)) (Last (Just CacheTtl5Min))))
                (Last (Just 8192))
                (Last (Just [WorkspaceFile "SOUL.md"]))
                (Last (Just ToolSearchDisabled))
                (Just mempty)
            b = mempty
        (a <> b) @?= a,
      testCase "toOverrides roundtrips through resolveProfile" $ do
        let s =
              AgentProfile
                "model-x"
                (Just (Budget 4096))
                15
                (Just CacheTtl1Hour)
                32768
                [InlineText "test"]
                (ToolSearchEnabled ["run_command"])
                defaultPermissionConfig
        resolveProfile (toOverrides s) @?= s,
      testCase "toOverrides roundtrips cache=Nothing (disabled)" $ do
        let s =
              AgentProfile
                "model-x"
                Nothing
                15
                Nothing
                32768
                [InlineText "test"]
                ToolSearchDisabled
                defaultPermissionConfig
        resolveProfile (toOverrides s) @?= s,
      testCase "cache overrides merge field-level" $ do
        let a = AgentOverrides (Last Nothing) Nothing (Last Nothing) (Just (CacheOverrides (Last (Just True)) (Last (Just CacheTtl5Min)))) (Last Nothing) (Last Nothing) (Last Nothing) Nothing
            b = AgentOverrides (Last Nothing) Nothing (Last Nothing) (Just (CacheOverrides (Last Nothing) (Last (Just CacheTtl1Hour)))) (Last Nothing) (Last Nothing) (Last Nothing) Nothing
        (a <> b).cache @?= Just (CacheOverrides (Last (Just True)) (Last (Just CacheTtl1Hour)))
    ]

resolveTests :: TestTree
resolveTests =
  testGroup
    "resolveProfile"
    [ testCase "mempty resolves to hardcoded defaults" $ do
        let s = resolveProfile mempty
        s.model @?= "claude-sonnet-4-20250514"
        s.thinking @?= Nothing
        s.maxIterations @?= 20
        s.maxTokens @?= 16384
        s.systemPrompt @?= [WorkspaceFile "SOUL.md"]
        s.toolSearch @?= ToolSearchDisabled
        s.permissions @?= resolvePermissions mempty,
      testCase "agentDefaults resolves to same defaults" $ do
        resolveProfile agentDefaults @?= resolveProfile mempty,
      testCase "cache disabled resolves to Nothing" $ do
        let o = AgentOverrides (Last Nothing) Nothing (Last Nothing) (Just (CacheOverrides (Last (Just False)) (Last Nothing))) (Last Nothing) (Last Nothing) (Last Nothing) Nothing
            s = resolveProfile o
        s.cache @?= Nothing,
      testCase "cache enabled with ttl resolves to Just ttl" $ do
        let o = AgentOverrides (Last Nothing) Nothing (Last Nothing) (Just (CacheOverrides (Last (Just True)) (Last (Just CacheTtl1Hour)))) (Last Nothing) (Last Nothing) (Last Nothing) Nothing
            s = resolveProfile o
        s.cache @?= Just CacheTtl1Hour,
      testCase "overrides are applied" $ do
        let o = agentDefaults <> AgentOverrides (Last (Just "custom-model")) Nothing (Last (Just 50)) Nothing (Last (Just 8192)) (Last Nothing) (Last Nothing) Nothing
            s = resolveProfile o
        s.model @?= "custom-model"
        s.thinking @?= Nothing
        s.maxIterations @?= 50
        s.maxTokens @?= 8192,
      testCase "thinking enable=true resolves to Just mode" $ do
        let o = AgentOverrides (Last Nothing) (Just (ThinkingOverrides (Last (Just True)) (Last (Just (Adaptive (Just EffortMedium)))))) (Last Nothing) Nothing (Last Nothing) (Last Nothing) (Last Nothing) Nothing
            s = resolveProfile o
        s.thinking @?= Just (Adaptive (Just EffortMedium)),
      testCase "thinking enable=true without mode defaults to Adaptive Nothing" $ do
        let o = AgentOverrides (Last Nothing) (Just (ThinkingOverrides (Last (Just True)) (Last Nothing))) (Last Nothing) Nothing (Last Nothing) (Last Nothing) (Last Nothing) Nothing
            s = resolveProfile o
        s.thinking @?= Just (Adaptive Nothing),
      testCase "thinking enable=false resolves to Nothing" $ do
        let o = AgentOverrides (Last Nothing) (Just (ThinkingOverrides (Last (Just False)) (Last (Just (Budget 4096))))) (Last Nothing) Nothing (Last Nothing) (Last Nothing) (Last Nothing) Nothing
            s = resolveProfile o
        s.thinking @?= Nothing
    ]

permissionsMergeTests :: TestTree
permissionsMergeTests =
  testGroup
    "Permissions merge"
    [ testCase "permissions use field-level merge" $ do
        let a = AgentOverrides (Last Nothing) Nothing (Last Nothing) Nothing (Last Nothing) (Last Nothing) (Last Nothing) (Just (PermissionConfigFile (Last Nothing) (Last Nothing) (Last (Just (Map.singleton "run_command" PolicyDeny))) (Last Nothing) (Last Nothing)))
            b = AgentOverrides (Last Nothing) Nothing (Last Nothing) Nothing (Last Nothing) (Last Nothing) (Last Nothing) (Just (PermissionConfigFile (Last (Just ["^ls\\b"])) (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing)))
        -- b's safePatterns should merge with a's toolPolicies (not replace)
        let merged = a <> b
        case merged.permissions of
          Nothing -> assertFailure "Expected Just permissions"
          Just pcf -> do
            pcf.safePatterns @?= Last (Just ["^ls\\b"])
            pcf.toolPolicies @?= Last (Just (Map.singleton "run_command" PolicyDeny)),
      testCase "permissions: both Nothing stays Nothing" $ do
        let a = mempty :: AgentOverrides
            b = mempty :: AgentOverrides
        (a <> b).permissions @?= Nothing,
      testCase "roundtrip permissions through toPermissionConfigFile" $ do
        let pc =
              PermissionConfig
                { safePatterns = ["^ls\\b"],
                  dangerousPatterns = ["\\brm\\b"],
                  toolPolicies = Map.singleton "run_command" PolicyAsk,
                  defaultPolicy = PolicyAllow,
                  approvalTimeoutSeconds = 120
                }
            profile =
              AgentProfile
                "model"
                Nothing
                20
                (Just CacheTtl5Min)
                16384
                [WorkspaceFile "SOUL.md"]
                ToolSearchDisabled
                pc
        let roundtripped = resolveProfile (toOverrides profile)
        roundtripped.permissions @?= pc
    ]
