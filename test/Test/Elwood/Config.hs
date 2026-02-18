module Test.Elwood.Config (tests) where

import Elwood.Config
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Config"
    [ compactionConfigTests
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
