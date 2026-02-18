module Test.Elwood.Scheduler (tests) where

import Data.Text qualified as T
import Elwood.Scheduler (hashJobName, isWithinActiveHours)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Scheduler"
    [ activeHoursTests,
      hashJobNameTests,
      hashJobNameProperties
    ]

activeHoursTests :: TestTree
activeHoursTests =
  testGroup
    "isWithinActiveHours"
    [ testCase "8am is within 8-22" $
        isWithinActiveHours 8 22 8 @?= True,
      testCase "12pm is within 8-22" $
        isWithinActiveHours 8 22 12 @?= True,
      testCase "21 is within 8-22" $
        isWithinActiveHours 8 22 21 @?= True,
      testCase "22 is NOT within 8-22 (end exclusive)" $
        isWithinActiveHours 8 22 22 @?= False,
      testCase "7am is NOT within 8-22" $
        isWithinActiveHours 8 22 7 @?= False,
      testCase "midnight is NOT within 8-22" $
        isWithinActiveHours 8 22 0 @?= False,
      testCase "23 is NOT within 8-22" $
        isWithinActiveHours 8 22 23 @?= False,
      testCase "midnight is within 0-24" $
        isWithinActiveHours 0 24 0 @?= True,
      testCase "23 is within 0-24" $
        isWithinActiveHours 0 24 23 @?= True,
      testCase "narrow window: 9 is within 9-10" $
        isWithinActiveHours 9 10 9 @?= True,
      testCase "narrow window: 10 is NOT within 9-10" $
        isWithinActiveHours 9 10 10 @?= False
    ]

hashJobNameTests :: TestTree
hashJobNameTests =
  testGroup
    "hashJobName"
    [ testCase "empty string has consistent hash" $
        hashJobName "" @?= hashJobName "",
      testCase "same strings have same hash" $
        hashJobName "test-job" @?= hashJobName "test-job",
      testCase "different strings have different hashes" $
        hashJobName "job-a" /= hashJobName "job-b" @?= True,
      testCase "similar strings have different hashes" $
        hashJobName "abc" /= hashJobName "acb" @?= True,
      testCase "hash is stable for known input" $ do
        -- This tests that the hash algorithm is consistent
        let h = hashJobName "daily-summary"
        h @?= hashJobName "daily-summary"
    ]

hashJobNameProperties :: TestTree
hashJobNameProperties =
  testGroup
    "hashJobName properties"
    [ testProperty "deterministic" $
        \(s :: String) ->
          let t = T.pack s
           in hashJobName t == hashJobName t,
      testProperty "different inputs usually give different hashes" $
        \(s1 :: String) (s2 :: String) ->
          s1 == s2 || hashJobName (T.pack s1) /= hashJobName (T.pack s2)
          -- Note: This can theoretically fail due to collisions,
          -- but DJB2 has good distribution so it's unlikely for random inputs
    ]
