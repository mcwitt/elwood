module Test.Elwood.Tools.Delegate (tests) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import Elwood.Claude.Types (CacheTtl (..), ToolName (..), ToolSchema (..))
import Elwood.Tools.Delegate (mkDelegateTaskTool)
import Elwood.Tools.Types
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Tools.Delegate"
    [ schemaTests,
      inputParsingTests,
      modelValidationTests,
      thinkingValidationTests,
      maxIterationsValidationTests
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

modelValidationTests :: TestTree
modelValidationTests =
  testGroup
    "Model validation"
    [ testCase "invalid model returns error" $ do
        let tool = mkStubDelegateToolWithModels ["model-a", "model-b"]
        result <- tool.execute (object ["task" .= ("test" :: Text), "model" .= ("bad-model" :: Text)])
        result @?= ToolError "Invalid model 'bad-model'. Allowed: model-a, model-b",
      testCase "non-string model returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "model" .= (42 :: Int)])
        result @?= ToolError "Invalid 'model' parameter (must be a string)",
      testCase "model rejected when allowed_models is empty" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "model" .= ("any-model" :: Text)])
        result @?= ToolError "Model selection is not enabled (configure delegate.allowed_models)"
    ]

thinkingValidationTests :: TestTree
thinkingValidationTests =
  testGroup
    "Thinking validation"
    [ testCase "invalid thinking returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "thinking" .= ("ultra" :: Text)])
        result @?= ToolError "Invalid thinking level 'ultra'. Allowed: off, low, medium, high",
      testCase "non-string thinking returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "thinking" .= (42 :: Int)])
        result @?= ToolError "Invalid 'thinking' parameter (must be a string)"
    ]

maxIterationsValidationTests :: TestTree
maxIterationsValidationTests =
  testGroup
    "max_iterations validation"
    [ testCase "non-integer returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "max_iterations" .= ("five" :: Text)])
        result @?= ToolError "Invalid 'max_iterations' parameter (must be an integer)",
      testCase "too large returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "max_iterations" .= (100 :: Int)])
        result @?= ToolError "max_iterations must be between 1 and 50",
      testCase "zero returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "max_iterations" .= (0 :: Int)])
        result @?= ToolError "max_iterations must be between 1 and 50"
    ]

-- | Build a delegate tool that will fail at the API call level but can
-- validate input parsing. Uses undefined for fields only needed at
-- runtime (logger, client, registry, etc.) since input validation
-- happens before they are accessed.
mkStubDelegateTool :: Tool
mkStubDelegateTool = mkStubDelegateToolWithModels []

mkStubDelegateToolWithModels :: [Text] -> Tool
mkStubDelegateToolWithModels models =
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
    mempty -- configOverrides
    models
    CacheTtl5Min -- cacheTtl
