module Main (main) where

import Test.Tasty

import qualified Test.Elwood.Memory
import qualified Test.Elwood.Permissions
import qualified Test.Elwood.Config
import qualified Test.Elwood.Scheduler
import qualified Test.Elwood.Claude.Types
import qualified Test.Elwood.Claude.Compaction

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Elwood Tests"
  [ Test.Elwood.Memory.tests
  , Test.Elwood.Permissions.tests
  , Test.Elwood.Config.tests
  , Test.Elwood.Scheduler.tests
  , Test.Elwood.Claude.Types.tests
  , Test.Elwood.Claude.Compaction.tests
  ]
