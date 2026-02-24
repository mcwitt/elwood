module Main (main) where

import Test.Elwood.Attachment qualified
import Test.Elwood.Claude.Client qualified
import Test.Elwood.Claude.Compaction qualified
import Test.Elwood.Claude.Pruning qualified
import Test.Elwood.Claude.Types qualified
import Test.Elwood.Config qualified
import Test.Elwood.Event qualified
import Test.Elwood.MCP qualified
import Test.Elwood.Memory qualified
import Test.Elwood.Metrics qualified
import Test.Elwood.Permissions qualified
import Test.Elwood.Tools.Registry qualified
import Test.Elwood.Webhook qualified
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
      Test.Elwood.Claude.Types.tests,
      Test.Elwood.Claude.Compaction.tests,
      Test.Elwood.Claude.Pruning.tests,
      Test.Elwood.Claude.Client.tests,
      Test.Elwood.MCP.tests,
      Test.Elwood.Event.tests,
      Test.Elwood.Webhook.tests,
      Test.Elwood.Attachment.tests,
      Test.Elwood.Metrics.tests,
      Test.Elwood.Tools.Registry.tests
    ]
