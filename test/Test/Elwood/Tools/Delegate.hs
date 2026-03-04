module Test.Elwood.Tools.Delegate (tests) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import Elwood.Claude.Types (ToolName (..), ToolSchema (..))
import Elwood.Tools.Delegate (mkDelegateTaskTool)
import Elwood.Tools.Types
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Tools.Delegate"
    [ schemaTests,
      inputParsingTests
    ]

schemaTests :: TestTree
schemaTests =
  testGroup
    "Schema"
    [ testCase "tool name is delegate_task" $ do
        let tool = mkStubDelegateTool
        tool.schema.name @?= ToolName "delegate_task",
      testCase "description is non-empty" $ do
        let tool = mkStubDelegateTool
        assertBool "description should not be empty" (tool.schema.description /= ("" :: Text))
    ]

inputParsingTests :: TestTree
inputParsingTests =
  testGroup
    "Input parsing"
    [ testCase "missing task returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object [])
        result @?= ToolError "Missing or invalid 'task' parameter",
      testCase "empty task returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("  " :: Text)])
        result @?= ToolError "Task description must not be empty",
      testCase "non-string task returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= (42 :: Int)])
        result @?= ToolError "Missing or invalid 'task' parameter",
      testCase "non-object input returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (String "not an object")
        result @?= ToolError "Expected object input"
    ]

-- | Build a delegate tool that will fail at the API call level but can
-- validate input parsing. Uses undefined for fields only needed at
-- runtime (logger, client, registry, etc.) since input validation
-- happens before they are accessed.
mkStubDelegateTool :: Tool
mkStubDelegateTool =
  mkDelegateTaskTool
    undefined -- logger
    undefined -- client
    undefined -- baseRegistry
    undefined -- context
    undefined -- agentSettings
    undefined -- compaction
    undefined -- pruning
    undefined -- systemPrompt
    undefined -- metrics
