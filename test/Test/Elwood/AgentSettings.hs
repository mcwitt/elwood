{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Elwood.AgentSettings (tests) where

import Data.Map.Strict qualified as Map
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
import Elwood.Thinking (ThinkingEffort (..), ThinkingLevel (..))
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

instance Arbitrary ThinkingLevel where
  arbitrary =
    oneof
      [ pure ThinkingOff,
        ThinkingAdaptive <$> arbitrary,
        ThinkingBudget . getPositive <$> arbitrary
      ]

instance Arbitrary CacheTtl where
  arbitrary = elements [CacheTtl5Min, CacheTtl1Hour]

instance Arbitrary CacheOverrides where
  arbitrary = CacheOverrides <$> arbitrary <*> arbitrary

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
    PermissionConfigFile
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary AgentOverrides where
  arbitrary =
    AgentOverrides
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
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
        let a = AgentOverrides (Just "model-a") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
            b = AgentOverrides (Just "model-b") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        (a <> b).model @?= Just "model-b",
      testCase "right-biased: Nothing preserves left" $ do
        let a =
              AgentOverrides
                (Just "model-a")
                (Just ThinkingOff)
                (Just 10)
                (Just (CacheOverrides (Just True) (Just CacheTtl5Min)))
                (Just 8192)
                (Just [WorkspaceFile "SOUL.md"])
                (Just ToolSearchDisabled)
                (Just mempty)
            b = mempty
        (a <> b) @?= a,
      testCase "toOverrides roundtrips through resolveProfile" $ do
        let s =
              AgentProfile
                "model-x"
                (ThinkingBudget 4096)
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
                ThinkingOff
                15
                Nothing
                32768
                [InlineText "test"]
                ToolSearchDisabled
                defaultPermissionConfig
        resolveProfile (toOverrides s) @?= s,
      testCase "cache overrides merge field-level" $ do
        let a = AgentOverrides Nothing Nothing Nothing (Just (CacheOverrides (Just True) (Just CacheTtl5Min))) Nothing Nothing Nothing Nothing
            b = AgentOverrides Nothing Nothing Nothing (Just (CacheOverrides Nothing (Just CacheTtl1Hour))) Nothing Nothing Nothing Nothing
        (a <> b).cache @?= Just (CacheOverrides (Just True) (Just CacheTtl1Hour))
    ]

resolveTests :: TestTree
resolveTests =
  testGroup
    "resolveProfile"
    [ testCase "mempty resolves to hardcoded defaults" $ do
        let s = resolveProfile mempty
        s.model @?= "claude-sonnet-4-20250514"
        s.thinking @?= ThinkingOff
        s.maxIterations @?= 20
        s.maxTokens @?= 16384
        s.systemPrompt @?= [WorkspaceFile "SOUL.md"]
        s.toolSearch @?= ToolSearchDisabled
        s.permissions @?= resolvePermissions mempty,
      testCase "agentDefaults resolves to same defaults" $ do
        resolveProfile agentDefaults @?= resolveProfile mempty,
      testCase "cache disabled resolves to Nothing" $ do
        let o = AgentOverrides Nothing Nothing Nothing (Just (CacheOverrides (Just False) Nothing)) Nothing Nothing Nothing Nothing
            s = resolveProfile o
        s.cache @?= Nothing,
      testCase "cache enabled with ttl resolves to Just ttl" $ do
        let o = AgentOverrides Nothing Nothing Nothing (Just (CacheOverrides (Just True) (Just CacheTtl1Hour))) Nothing Nothing Nothing Nothing
            s = resolveProfile o
        s.cache @?= Just CacheTtl1Hour,
      testCase "overrides are applied" $ do
        let o = agentDefaults <> AgentOverrides (Just "custom-model") Nothing (Just 50) Nothing (Just 8192) Nothing Nothing Nothing
            s = resolveProfile o
        s.model @?= "custom-model"
        s.thinking @?= ThinkingOff
        s.maxIterations @?= 50
        s.maxTokens @?= 8192
    ]

permissionsMergeTests :: TestTree
permissionsMergeTests =
  testGroup
    "Permissions merge"
    [ testCase "permissions use field-level merge" $ do
        let a = AgentOverrides Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just (PermissionConfigFile Nothing Nothing (Just (Map.singleton "run_command" PolicyDeny)) Nothing Nothing))
            b = AgentOverrides Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just (PermissionConfigFile (Just ["^ls\\b"]) Nothing Nothing Nothing Nothing))
        -- b's safePatterns should merge with a's toolPolicies (not replace)
        let merged = a <> b
        case merged.permissions of
          Nothing -> assertFailure "Expected Just permissions"
          Just pcf -> do
            pcf.safePatterns @?= Just ["^ls\\b"]
            pcf.toolPolicies @?= Just (Map.singleton "run_command" PolicyDeny),
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
                ThinkingOff
                20
                (Just CacheTtl5Min)
                16384
                [WorkspaceFile "SOUL.md"]
                ToolSearchDisabled
                pc
        let roundtripped = resolveProfile (toOverrides profile)
        roundtripped.permissions @?= pc
    ]
