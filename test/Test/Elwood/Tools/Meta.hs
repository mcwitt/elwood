module Test.Elwood.Tools.Meta (tests) where

import Data.Aeson (object, (.=))
import Data.IORef (newIORef, readIORef)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Claude.Types (ToolSchema (..))
import Elwood.Tools.Meta (mkFindToolsTool)
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

-- | Build a test registry with several tools
testRegistry :: ToolRegistry
testRegistry =
  registerTool (mkTestTool "run_command" "Execute shell commands") $
    registerTool (mkTestTool "mcp_weather_forecast" "Get weather forecast") $
      registerTool (mkTestTool "mcp_calendar_list" "List calendar events") $
        registerTool (mkTestTool "mcp_github_list_issues" "List GitHub issues") $
          registerTool (mkTestTool "mcp_github_search_repos" "Search GitHub repositories") $
            registerTool (mkTestTool "general_issue_tracker" "Track issues and bugs for GitHub projects") newToolRegistry

tests :: TestTree
tests =
  testGroup
    "Tools.Meta"
    [findToolsTests]

findToolsTests :: TestTree
findToolsTests =
  testGroup
    "find_tools"
    [ testCase "searches and loads matching tools" $ do
        activeRef <- newIORef newActiveToolSet
        let tool = mkFindToolsTool testRegistry activeRef
        result <- toolExecute tool (object ["query" .= ("weather" :: Text)])
        case result of
          ToolError err -> assertFailure $ "Expected success, got error: " <> T.unpack err
          ToolSuccess output -> do
            assertBool "Should contain weather tool" ("mcp_weather_forecast" `T.isInfixOf` output)
            assertBool "Should not contain calendar tool" (not $ "mcp_calendar_list" `T.isInfixOf` output),
      testCase "substring match on name and description" $ do
        activeRef <- newIORef newActiveToolSet
        let tool = mkFindToolsTool testRegistry activeRef
        result <- toolExecute tool (object ["query" .= ("calendar" :: Text)])
        case result of
          ToolError err -> assertFailure $ "Expected success, got error: " <> T.unpack err
          ToolSuccess output -> do
            assertBool "Should contain calendar tool (name match)" ("mcp_calendar_list" `T.isInfixOf` output)
            assertBool "Should not contain weather tool" (not $ "mcp_weather_forecast" `T.isInfixOf` output),
      testCase "case-insensitive matching" $ do
        activeRef <- newIORef newActiveToolSet
        let tool = mkFindToolsTool testRegistry activeRef
        result <- toolExecute tool (object ["query" .= ("WEATHER" :: Text)])
        case result of
          ToolError err -> assertFailure $ "Expected success, got error: " <> T.unpack err
          ToolSuccess output ->
            assertBool "Should find weather tool" ("mcp_weather_forecast" `T.isInfixOf` output),
      testCase "no matches returns helpful message" $ do
        activeRef <- newIORef newActiveToolSet
        let tool = mkFindToolsTool testRegistry activeRef
        result <- toolExecute tool (object ["query" .= ("nonexistent_xyz" :: Text)])
        case result of
          ToolError _ -> assertFailure "Expected success even with no matches"
          ToolSuccess output ->
            assertBool "Should suggest different keywords" ("No tools found" `T.isInfixOf` output),
      testCase "loaded tools appear in activeToolSchemas" $ do
        activeRef <- newIORef newActiveToolSet
        let tool = mkFindToolsTool testRegistry activeRef
        _ <- toolExecute tool (object ["query" .= ("weather" :: Text)])
        ats <- readIORef activeRef
        let schemas = activeToolSchemas testRegistry ats
            names = Set.fromList (map tsName schemas)
        assertBool "Should contain loaded tool" (Set.member "mcp_weather_forecast" names)
        assertBool "Should not contain unloaded tool" (not $ Set.member "mcp_calendar_list" names),
      testCase "top-20 limiting" $ do
        let bigRegistry =
              foldr
                (\i -> registerTool (mkTestTool ("tool_match_" <> T.pack (show i)) "A matching tool"))
                newToolRegistry
                ([1 .. 25] :: [Int])
        activeRef <- newIORef newActiveToolSet
        let tool = mkFindToolsTool bigRegistry activeRef
        result <- toolExecute tool (object ["query" .= ("match" :: Text)])
        ats <- readIORef activeRef
        let schemas = activeToolSchemas bigRegistry ats
        assertBool "Should load at most 20 tools" (length schemas <= 20)
        case result of
          ToolError err -> assertFailure $ "Expected success, got error: " <> T.unpack err
          ToolSuccess output ->
            assertBool "Should indicate more matches exist" ("20 of 25" `T.isInfixOf` output),
      testCase "multi-word query matches tool with all terms" $ do
        activeRef <- newIORef newActiveToolSet
        let tool = mkFindToolsTool testRegistry activeRef
        result <- toolExecute tool (object ["query" .= ("weather forecast" :: Text)])
        case result of
          ToolError err -> assertFailure $ "Expected success, got error: " <> T.unpack err
          ToolSuccess output -> do
            assertBool "Should match tool with both terms" ("mcp_weather_forecast" `T.isInfixOf` output)
            assertBool "Should not match calendar tool" (not $ "mcp_calendar_list" `T.isInfixOf` output),
      testCase "multi-word query rejects tool missing a term" $ do
        activeRef <- newIORef newActiveToolSet
        let tool = mkFindToolsTool testRegistry activeRef
        result <- toolExecute tool (object ["query" .= ("github forecast" :: Text)])
        case result of
          ToolError _ -> assertFailure "Expected success even with no matches"
          ToolSuccess output ->
            assertBool "Should not match any tool" ("No tools found" `T.isInfixOf` output),
      testCase "name matches rank above description-only matches" $ do
        activeRef <- newIORef newActiveToolSet
        let tool = mkFindToolsTool testRegistry activeRef
        -- "github issues": mcp_github_list_issues has both terms in name (score 2),
        -- general_issue_tracker has "issues" only in description (score 0)
        result <- toolExecute tool (object ["query" .= ("github issues" :: Text)])
        case result of
          ToolError err -> assertFailure $ "Expected success, got error: " <> T.unpack err
          ToolSuccess output -> do
            assertBool "Should contain github list issues tool" ("mcp_github_list_issues" `T.isInfixOf` output)
            -- Verify the name-match tool appears before description-only matches
            let githubPos = T.breakOn "mcp_github_list_issues" output
                issueTrackerPos = T.breakOn "general_issue_tracker" output
            assertBool
              "github_list_issues should rank before general_issue_tracker"
              (T.length (fst githubPos) < T.length (fst issueTrackerPos))
    ]
