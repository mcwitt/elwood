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
mkTestTool n =
  Tool
    { name = n,
      description = "Test tool: " <> n,
      inputSchema = object ["type" .= ("object" :: Text)],
      execute = \_ -> pure (ToolSuccess "ok")
    }

tests :: TestTree
tests =
  testGroup
    "Tools.Registry"
    [ registryBasicsTests,
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
          Just t -> t.name @?= "a"
        case lookupTool "b" reg of
          Nothing -> assertFailure "Should find tool b"
          Just t -> t.name @?= "b",
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

activeToolSchemasTests :: TestTree
activeToolSchemasTests =
  testGroup
    "activeToolSchemas"
    [ testCase "returns only tools in the active set" $ do
        let reg =
              registerTool (mkTestTool "a") $
                registerTool (mkTestTool "b") $
                  registerTool (mkTestTool "c") newToolRegistry
            active = Set.fromList ["a", "c"]
            schemas = activeToolSchemas reg active
            names = Set.fromList (map (.name) schemas)
        names @?= Set.fromList ["a", "c"],
      testCase "returns all tools when all activated" $ do
        let reg =
              registerTool (mkTestTool "a") $
                registerTool (mkTestTool "b") newToolRegistry
            active = Set.fromList ["a", "b"]
            schemas = activeToolSchemas reg active
        length schemas @?= 2,
      testCase "returns empty when none activated" $ do
        let reg =
              registerTool (mkTestTool "a") $
                registerTool (mkTestTool "b") newToolRegistry
            schemas = activeToolSchemas reg Set.empty
        length schemas @?= 0
    ]
