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
    [ toolCategoryTests,
      activeToolSetTests,
      activeToolSchemasTests
    ]

toolCategoryTests :: TestTree
toolCategoryTests =
  testGroup
    "ToolCategory"
    [ testCase "registerTool defaults to AlwaysLoaded" $ do
        let reg = registerTool (mkTestTool "t1") newToolRegistry
        hasDynamicTools reg @?= False,
      testCase "registerToolWith DynamicLoadable is dynamic" $ do
        let reg = registerToolWith DynamicLoadable (mkTestTool "t1") newToolRegistry
        hasDynamicTools reg @?= True,
      testCase "lookupTool finds both categories" $ do
        let reg =
              registerTool (mkTestTool "a") $
                registerToolWith DynamicLoadable (mkTestTool "b") newToolRegistry
        case lookupTool "a" reg of
          Nothing -> assertFailure "Should find AlwaysLoaded tool"
          Just t -> toolName t @?= "a"
        case lookupTool "b" reg of
          Nothing -> assertFailure "Should find DynamicLoadable tool"
          Just t -> toolName t @?= "b",
      testCase "allTools returns both categories" $ do
        let reg =
              registerTool (mkTestTool "a") $
                registerToolWith DynamicLoadable (mkTestTool "b") newToolRegistry
        length (allTools reg) @?= 2
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
    [ testCase "includes AlwaysLoaded and loaded dynamic tools" $ do
        let reg =
              registerTool (mkTestTool "builtin") $
                registerToolWith DynamicLoadable (mkTestTool "mcp_a") $
                  registerToolWith DynamicLoadable (mkTestTool "mcp_b") newToolRegistry
            ats = activateTool "mcp_a" newActiveToolSet
            schemas = activeToolSchemas reg ats
            names = map tsName schemas
        Set.fromList names @?= Set.fromList ["builtin", "mcp_a"],
      testCase "returns all tools when all dynamic tools loaded" $ do
        let reg =
              registerTool (mkTestTool "a") $
                registerToolWith DynamicLoadable (mkTestTool "b") newToolRegistry
            ats = activateTool "b" newActiveToolSet
            schemas = activeToolSchemas reg ats
        length schemas @?= 2,
      testCase "returns only AlwaysLoaded when none loaded" $ do
        let reg =
              registerTool (mkTestTool "a") $
                registerToolWith DynamicLoadable (mkTestTool "b") $
                  registerToolWith DynamicLoadable (mkTestTool "c") newToolRegistry
            schemas = activeToolSchemas reg newActiveToolSet
            names = map tsName schemas
        names @?= ["a"]
    ]
