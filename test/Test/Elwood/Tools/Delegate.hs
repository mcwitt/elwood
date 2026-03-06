module Test.Elwood.Tools.Delegate (tests) where

import Data.Aeson (Value (..), object, (.=))
import Data.Map.Strict qualified as Map
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
      inputParsingTests,
      modelValidationTests,
      thinkingValidationTests,
      maxIterationsValidationTests,
      agentSelectionTests
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
    [ testCase "string thinking returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "thinking" .= ("medium" :: Text)])
        result @?= ToolError "Invalid 'thinking' parameter (must be an object like {\"type\": \"adaptive\", \"effort\": \"medium\"})",
      testCase "non-object thinking returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "thinking" .= (42 :: Int)])
        result @?= ToolError "Invalid 'thinking' parameter (must be an object like {\"type\": \"adaptive\", \"effort\": \"medium\"})",
      testCase "invalid thinking type in object returns error" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "thinking" .= object ["type" .= ("turbo" :: Text)]])
        result @?= ToolError "Invalid thinking config: Invalid thinking type 'turbo'. Allowed: off, adaptive, fixed"
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

agentSelectionTests :: TestTree
agentSelectionTests =
  testGroup
    "Agent selection"
    [ testCase "agent rejected when extra_agents is empty" $ do
        let tool = mkStubDelegateTool
        result <- tool.execute (object ["task" .= ("test" :: Text), "agent" .= ("fast" :: Text)])
        result @?= ToolError "Agent selection is not enabled (configure delegate.extra_agents)",
      testCase "invalid agent name returns error" $ do
        let tool = mkStubDelegateToolWithAgents ["fast", "deep"]
        result <- tool.execute (object ["task" .= ("test" :: Text), "agent" .= ("turbo" :: Text)])
        result @?= ToolError "Invalid agent 'turbo'. Available: deep, fast",
      testCase "non-string agent returns error" $ do
        let tool = mkStubDelegateToolWithAgents ["fast"]
        result <- tool.execute (object ["task" .= ("test" :: Text), "agent" .= (42 :: Int)])
        result @?= ToolError "Invalid 'agent' parameter (must be a string)"
    ]

-- | Build a delegate tool that will fail at the API call level but can
-- validate input parsing. Uses undefined for fields only needed at
-- runtime (logger, client, registry, etc.) since input validation
-- happens before they are accessed.
mkStubDelegateTool :: Tool
mkStubDelegateTool = mkStubDelegateToolWithModels []

mkStubDelegateToolWithModels :: [Text] -> Tool
mkStubDelegateToolWithModels =
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
    mempty -- defaultAgentOverrides
    Map.empty -- extraAgents

mkStubDelegateToolWithAgents :: [Text] -> Tool
mkStubDelegateToolWithAgents agentNames =
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
    mempty -- defaultAgentOverrides
    (Map.fromList [(n, mempty) | n <- agentNames])
    [] -- allowedModels
