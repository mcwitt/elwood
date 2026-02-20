module Test.Elwood.Tools.Meta (tests) where

import Data.Aeson (object, (.=))
import Data.IORef (newIORef, readIORef)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Tools.Meta (mkDiscoverToolsTool, mkLoadToolTool)
import Elwood.Tools.Registry
import Elwood.Tools.Types (Tool (..), ToolResult (..))
import Test.Tasty
import Test.Tasty.HUnit

-- | A simple no-op tool for testing
mkTestTool :: Text -> Text -> Tool
mkTestTool name desc =
  Tool
    { toolName = name,
      toolDescription = desc,
      toolInputSchema = object ["type" .= ("object" :: Text)],
      toolExecute = \_ -> pure (ToolSuccess "ok")
    }

-- | Build a test registry with a mix of tools
testRegistry :: ToolRegistry
testRegistry =
  registerTool (mkTestTool "run_command" "Execute shell commands") $
    registerToolWith DynamicLoadable (mkTestTool "mcp_weather_forecast" "Get weather forecast") $
      registerToolWith DynamicLoadable (mkTestTool "mcp_calendar_list" "List calendar events") newToolRegistry

tests :: TestTree
tests =
  testGroup
    "Tools.Meta"
    [ discoverToolsTests,
      loadToolTests
    ]

discoverToolsTests :: TestTree
discoverToolsTests =
  testGroup
    "discover_tools"
    [ testCase "lists all tools with no query" $ do
        let tool = mkDiscoverToolsTool testRegistry
        result <- toolExecute tool (object [])
        case result of
          ToolError err -> assertFailure $ "Expected success, got error: " <> T.unpack err
          ToolSuccess output -> do
            assertBool "Should contain run_command" ("run_command" `T.isInfixOf` output)
            assertBool "Should contain mcp_weather" ("mcp_weather_forecast" `T.isInfixOf` output)
            assertBool "Should contain mcp_calendar" ("mcp_calendar_list" `T.isInfixOf` output),
      testCase "filters by query on name" $ do
        let tool = mkDiscoverToolsTool testRegistry
        result <- toolExecute tool (object ["query" .= ("weather" :: Text)])
        case result of
          ToolError err -> assertFailure $ "Expected success, got error: " <> T.unpack err
          ToolSuccess output -> do
            assertBool "Should contain weather tool" ("mcp_weather_forecast" `T.isInfixOf` output)
            assertBool "Should not contain calendar tool" (not $ "mcp_calendar_list" `T.isInfixOf` output),
      testCase "filters by query on description" $ do
        let tool = mkDiscoverToolsTool testRegistry
        result <- toolExecute tool (object ["query" .= ("calendar" :: Text)])
        case result of
          ToolError err -> assertFailure $ "Expected success, got error: " <> T.unpack err
          ToolSuccess output -> do
            assertBool "Should contain calendar tool" ("mcp_calendar_list" `T.isInfixOf` output)
            assertBool "Should not contain weather tool" (not $ "mcp_weather_forecast" `T.isInfixOf` output),
      testCase "case-insensitive filtering" $ do
        let tool = mkDiscoverToolsTool testRegistry
        result <- toolExecute tool (object ["query" .= ("WEATHER" :: Text)])
        case result of
          ToolError err -> assertFailure $ "Expected success, got error: " <> T.unpack err
          ToolSuccess output ->
            assertBool "Should find weather tool" ("mcp_weather_forecast" `T.isInfixOf` output),
      testCase "no matches returns message" $ do
        let tool = mkDiscoverToolsTool testRegistry
        result <- toolExecute tool (object ["query" .= ("nonexistent_xyz" :: Text)])
        case result of
          ToolError _ -> assertFailure "Expected success even with no matches"
          ToolSuccess output ->
            assertBool "Should indicate no results" ("No tools found" `T.isInfixOf` output)
    ]

loadToolTests :: TestTree
loadToolTests =
  testGroup
    "load_tool"
    [ testCase "loads existing tool" $ do
        activeRef <- newIORef (newActiveToolSet (alwaysLoadedNames testRegistry))
        let tool = mkLoadToolTool testRegistry activeRef
        result <- toolExecute tool (object ["name" .= ("mcp_weather_forecast" :: Text)])
        case result of
          ToolError err -> assertFailure $ "Expected success, got error: " <> T.unpack err
          ToolSuccess output -> do
            assertBool "Should confirm load" ("Loaded tool: mcp_weather_forecast" `T.isInfixOf` output)
            assertBool "Should include description" ("Get weather forecast" `T.isInfixOf` output),
      testCase "updates active tool set" $ do
        activeRef <- newIORef (newActiveToolSet (alwaysLoadedNames testRegistry))
        let tool = mkLoadToolTool testRegistry activeRef
        _ <- toolExecute tool (object ["name" .= ("mcp_calendar_list" :: Text)])
        ats <- readIORef activeRef
        isToolActive "mcp_calendar_list" ats @?= True
        isToolActive "run_command" ats @?= True -- always loaded
        isToolActive "mcp_weather_forecast" ats @?= False, -- not loaded yet
      testCase "rejects unknown tool" $ do
        activeRef <- newIORef (newActiveToolSet Set.empty)
        let tool = mkLoadToolTool testRegistry activeRef
        result <- toolExecute tool (object ["name" .= ("nonexistent" :: Text)])
        case result of
          ToolSuccess _ -> assertFailure "Expected error for unknown tool"
          ToolError err -> do
            assertBool "Should mention unknown" ("Unknown tool" `T.isInfixOf` err)
            assertBool "Should suggest discover_tools" ("discover_tools" `T.isInfixOf` err),
      testCase "rejects missing name" $ do
        activeRef <- newIORef (newActiveToolSet Set.empty)
        let tool = mkLoadToolTool testRegistry activeRef
        result <- toolExecute tool (object [])
        case result of
          ToolSuccess _ -> assertFailure "Expected error for missing name"
          ToolError err ->
            assertBool "Should mention missing param" ("Missing" `T.isInfixOf` err)
    ]
