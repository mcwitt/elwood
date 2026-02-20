module Test.Elwood.Tools.Registry (tests) where

import Data.Aeson (object, (.=))
import Data.Set qualified as Set
import Data.Text (Text)
import Elwood.Claude.Types (ToolSchema (..))
import Elwood.Tools.Registry
import Elwood.Tools.Types (Tool (..), ToolResult (..))
import Test.Tasty
import Test.Tasty.HUnit

-- | A simple no-op tool for testing
mkTestTool :: Text -> Tool
mkTestTool name =
  Tool
    { toolName = name,
      toolDescription = "Test tool: " <> name,
      toolInputSchema = object ["type" .= ("object" :: Text)],
      toolExecute = \_ -> pure (ToolSuccess "ok")
    }

tests :: TestTree
tests =
  testGroup
    "Tools.Registry"
    [ registryBasicsTests,
      activeToolSetTests,
      activeToolSchemasTests
    ]

registryBasicsTests :: TestTree
registryBasicsTests =
  testGroup
    "Registry basics"
    [ testCase "lookupTool finds registered tools" $ do
        let reg =
              registerTool (mkTestTool "a") $
                registerTool (mkTestTool "b") newToolRegistry
        case lookupTool "a" reg of
          Nothing -> assertFailure "Should find tool a"
          Just t -> toolName t @?= "a"
        case lookupTool "b" reg of
          Nothing -> assertFailure "Should find tool b"
          Just t -> toolName t @?= "b",
      testCase "allTools returns all registered tools" $ do
        let reg =
              registerTool (mkTestTool "a") $
                registerTool (mkTestTool "b") newToolRegistry
        length (allTools reg) @?= 2,
      testCase "toolSchemas returns schemas for all tools" $ do
        let reg =
              registerTool (mkTestTool "a") $
                registerTool (mkTestTool "b") newToolRegistry
        length (toolSchemas reg) @?= 2
    ]

activeToolSetTests :: TestTree
activeToolSetTests =
  testGroup
    "ActiveToolSet"
    [ testCase "activateTool is idempotent" $ do
        let ats0 = newActiveToolSet
            ats1 = activateTool "x" ats0
            ats2 = activateTool "x" ats1
        ats1 @?= ats2
    ]

activeToolSchemasTests :: TestTree
activeToolSchemasTests =
  testGroup
    "activeToolSchemas"
    [ testCase "returns only tools in ActiveToolSet" $ do
        let reg =
              registerTool (mkTestTool "a") $
                registerTool (mkTestTool "b") $
                  registerTool (mkTestTool "c") newToolRegistry
            ats = activateTool "a" $ activateTool "c" newActiveToolSet
            schemas = activeToolSchemas reg ats
            names = Set.fromList (map tsName schemas)
        names @?= Set.fromList ["a", "c"],
      testCase "returns all tools when all activated" $ do
        let reg =
              registerTool (mkTestTool "a") $
                registerTool (mkTestTool "b") newToolRegistry
            ats = activateTool "a" $ activateTool "b" newActiveToolSet
            schemas = activeToolSchemas reg ats
        length schemas @?= 2,
      testCase "returns empty when none activated" $ do
        let reg =
              registerTool (mkTestTool "a") $
                registerTool (mkTestTool "b") newToolRegistry
            schemas = activeToolSchemas reg newActiveToolSet
        length schemas @?= 0
    ]
