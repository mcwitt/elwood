module Test.Elwood.Config (tests) where

import Elwood.Config
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Config"
    [ heartbeatConfigTests,
      cronJobTests,
      compactionConfigTests
    ]

heartbeatConfigTests :: TestTree
heartbeatConfigTests =
  testGroup
    "HeartbeatConfig"
    [ testCase "has sensible defaults" $ do
        let defaultHb =
              HeartbeatConfig
                { hbEnabled = True,
                  hbIntervalMinutes = 30,
                  hbActiveHoursStart = 8,
                  hbActiveHoursEnd = 22
                }
        -- Test that defaults are reasonable
        hbEnabled defaultHb @?= True
        hbIntervalMinutes defaultHb @?= 30
        hbActiveHoursStart defaultHb @?= 8
        hbActiveHoursEnd defaultHb @?= 22,
      testCase "active hours are valid range" $ do
        let hb = HeartbeatConfig True 30 8 22
        -- Start should be less than end for normal operation
        hbActiveHoursStart hb < hbActiveHoursEnd hb @?= True
        -- Hours should be valid (0-23)
        hbActiveHoursStart hb >= 0 @?= True
        hbActiveHoursEnd hb <= 24 @?= True
    ]

cronJobTests :: TestTree
cronJobTests =
  testGroup
    "CronJob"
    [ testCase "can create isolated job" $ do
        let job =
              CronJob
                { cjName = "test-job",
                  cjIntervalMinutes = 60,
                  cjPrompt = "Do something",
                  cjIsolated = True
                }
        cjName job @?= "test-job"
        cjIsolated job @?= True,
      testCase "can create shared session job" $ do
        let job =
              CronJob
                { cjName = "shared-job",
                  cjIntervalMinutes = 120,
                  cjPrompt = "Check something",
                  cjIsolated = False
                }
        cjIsolated job @?= False,
      testCase "interval can be daily" $ do
        let dailyJob = CronJob "daily" 1440 "Daily check" True
        cjIntervalMinutes dailyJob @?= 1440 -- 24 * 60
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
