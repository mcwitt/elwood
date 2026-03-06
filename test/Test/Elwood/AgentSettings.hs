{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Elwood.AgentSettings (tests) where

import Data.Text (Text)
import Data.Text qualified as T
import Elwood.AgentSettings
  ( AgentOverrides (..),
    AgentSettings (..),
    agentDefaults,
    resolveAgent,
    toOverrides,
  )
import Elwood.Claude.Types (CacheTtl (..))
import Elwood.Positive qualified as P
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
      resolveTests
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

instance Arbitrary P.Positive where
  arbitrary = P.unsafePositive . getPositive <$> arbitrary

instance Arbitrary AgentOverrides where
  arbitrary =
    AgentOverrides
      <$> arbitrary
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
        let a = AgentOverrides (Just "model-a") Nothing Nothing Nothing Nothing
            b = AgentOverrides (Just "model-b") Nothing Nothing Nothing Nothing
        (a <> b).model @?= Just "model-b",
      testCase "right-biased: Nothing preserves left" $ do
        let a = AgentOverrides (Just "model-a") (Just ThinkingOff) (Just (P.unsafePositive 10)) (Just CacheTtl5Min) (Just (P.unsafePositive 8192))
            b = mempty
        (a <> b) @?= a,
      testCase "toOverrides roundtrips through resolveAgent" $ do
        let s = AgentSettings "model-x" (ThinkingBudget 4096) (P.unsafePositive 15) CacheTtl1Hour (P.unsafePositive 32768)
        resolveAgent (toOverrides s) @?= s
    ]

resolveTests :: TestTree
resolveTests =
  testGroup
    "resolveAgent"
    [ testCase "mempty resolves to hardcoded defaults" $ do
        let s = resolveAgent mempty
        s.model @?= "claude-sonnet-4-20250514"
        s.thinking @?= ThinkingOff
        s.maxIterations @?= P.unsafePositive 20
        s.maxTokens @?= P.unsafePositive 16384,
      testCase "agentDefaults resolves to same defaults" $ do
        resolveAgent agentDefaults @?= resolveAgent mempty,
      testCase "overrides are applied" $ do
        let o = agentDefaults <> AgentOverrides (Just "custom-model") Nothing (Just (P.unsafePositive 50)) Nothing (Just (P.unsafePositive 8192))
            s = resolveAgent o
        s.model @?= "custom-model"
        s.thinking @?= ThinkingOff
        s.maxIterations @?= P.unsafePositive 50
        s.maxTokens @?= P.unsafePositive 8192
    ]
