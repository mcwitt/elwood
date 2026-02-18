module Main (main) where

import Test.Elwood.Claude.Compaction qualified
import Test.Elwood.Claude.Types qualified
import Test.Elwood.Config qualified
import Test.Elwood.MCP qualified
import Test.Elwood.Memory qualified
import Test.Elwood.Permissions qualified
import Test.Elwood.Scheduler qualified
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Elwood Tests"
    [ Test.Elwood.Memory.tests,
      Test.Elwood.Permissions.tests,
      Test.Elwood.Config.tests,
      Test.Elwood.Scheduler.tests,
      Test.Elwood.Claude.Types.tests,
      Test.Elwood.Claude.Compaction.tests,
      Test.Elwood.MCP.tests
    ]
