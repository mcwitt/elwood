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
        alwaysLoadedNames reg @?= Set.singleton "t1",
      testCase "registerToolWith DynamicLoadable is not always-loaded" $ do
        let reg = registerToolWith DynamicLoadable (mkTestTool "t1") newToolRegistry
        alwaysLoadedNames reg @?= Set.empty,
      testCase "mixed categories tracked correctly" $ do
        let reg =
              registerTool (mkTestTool "builtin") $
                registerToolWith DynamicLoadable (mkTestTool "mcp_foo") newToolRegistry
        alwaysLoadedNames reg @?= Set.singleton "builtin",
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
    [ testCase "newActiveToolSet includes always-loaded" $ do
        let ats = newActiveToolSet (Set.fromList ["a", "b"])
        isToolActive "a" ats @?= True
        isToolActive "b" ats @?= True
        isToolActive "c" ats @?= False,
      testCase "activateTool adds to loaded set" $ do
        let ats = activateTool "c" (newActiveToolSet (Set.singleton "a"))
        isToolActive "a" ats @?= True
        isToolActive "c" ats @?= True
        isToolActive "d" ats @?= False,
      testCase "activateTool is idempotent" $ do
        let ats0 = newActiveToolSet Set.empty
            ats1 = activateTool "x" ats0
            ats2 = activateTool "x" ats1
        ats1 @?= ats2
    ]

activeToolSchemasTests :: TestTree
activeToolSchemasTests =
  testGroup
    "activeToolSchemas"
    [ testCase "returns only active tools" $ do
        let reg =
              registerTool (mkTestTool "builtin") $
                registerToolWith DynamicLoadable (mkTestTool "mcp_a") $
                  registerToolWith DynamicLoadable (mkTestTool "mcp_b") newToolRegistry
            ats = activateTool "mcp_a" (newActiveToolSet (alwaysLoadedNames reg))
            schemas = activeToolSchemas reg ats
            names = map tsName schemas
        Set.fromList names @?= Set.fromList ["builtin", "mcp_a"],
      testCase "returns all tools when all active" $ do
        let reg =
              registerTool (mkTestTool "a") $
                registerToolWith DynamicLoadable (mkTestTool "b") newToolRegistry
            ats = activateTool "b" (newActiveToolSet (alwaysLoadedNames reg))
            schemas = activeToolSchemas reg ats
        length schemas @?= 2,
      testCase "returns no dynamic tools when none loaded" $ do
        let reg =
              registerTool (mkTestTool "a") $
                registerToolWith DynamicLoadable (mkTestTool "b") $
                  registerToolWith DynamicLoadable (mkTestTool "c") newToolRegistry
            ats = newActiveToolSet (alwaysLoadedNames reg)
            schemas = activeToolSchemas reg ats
            names = map tsName schemas
        names @?= ["a"]
    ]
