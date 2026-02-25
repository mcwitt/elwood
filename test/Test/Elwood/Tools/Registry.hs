module Test.Elwood.Tools.Registry (tests) where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import Elwood.Claude.Types (ToolName (..), ToolSchema (..))
import Elwood.Tools.Registry
import Elwood.Tools.Types (Tool (..), ToolResult (..))
import Test.Tasty
import Test.Tasty.HUnit

-- | A simple no-op tool for testing
mkTestTool :: ToolName -> Tool
mkTestTool n =
  let ToolName nt = n
   in Tool
        { schema =
            ToolSchema
              { name = n,
                description = "Test tool: " <> nt,
                inputSchema = object ["type" .= ("object" :: Text)]
              },
          execute = \_ -> pure (ToolSuccess "ok")
        }

tests :: TestTree
tests =
  testGroup
    "Tools.Registry"
    [ registryBasicsTests
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
          Just t -> t.schema.name @?= "a"
        case lookupTool "b" reg of
          Nothing -> assertFailure "Should find tool b"
          Just t -> t.schema.name @?= "b",
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
