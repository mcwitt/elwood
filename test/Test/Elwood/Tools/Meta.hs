module Test.Elwood.Tools.Meta (tests) where

import Data.Aeson (object, (.=))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Claude.Types (ToolSchema (..))
import Elwood.Tools.Meta (searchTools)
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
    [searchToolsTests]

searchToolsTests :: TestTree
searchToolsTests =
  testGroup
    "searchTools"
    [ testCase "searches and returns matching tools" $ do
        let (text, names) = searchTools testRegistry "weather"
        assertBool "Should contain weather tool in text" ("mcp_weather_forecast" `T.isInfixOf` text)
        assertBool "Should not contain calendar tool in text" (not $ "mcp_calendar_list" `T.isInfixOf` text)
        assertBool "Should include weather tool name" (Set.member "mcp_weather_forecast" names),
      testCase "substring match on name and description" $ do
        let (text, names) = searchTools testRegistry "calendar"
        assertBool "Should contain calendar tool (name match)" ("mcp_calendar_list" `T.isInfixOf` text)
        assertBool "Should not contain weather tool" (not $ "mcp_weather_forecast" `T.isInfixOf` text)
        assertBool "Should include calendar tool name" (Set.member "mcp_calendar_list" names),
      testCase "case-insensitive matching" $ do
        let (text, _names) = searchTools testRegistry "WEATHER"
        assertBool "Should find weather tool" ("mcp_weather_forecast" `T.isInfixOf` text),
      testCase "no matches returns helpful message" $ do
        let (text, names) = searchTools testRegistry "nonexistent_xyz"
        assertBool "Should suggest different keywords" ("No tools found" `T.isInfixOf` text)
        assertBool "Should return empty set" (Set.null names),
      testCase "returned names match activeToolSchemas" $ do
        let (_text, names) = searchTools testRegistry "weather"
            schemas = activeToolSchemas testRegistry names
            schemaNames = Set.fromList (map tsName schemas)
        assertBool "Should contain loaded tool" (Set.member "mcp_weather_forecast" schemaNames)
        assertBool "Should not contain unloaded tool" (not $ Set.member "mcp_calendar_list" schemaNames),
      testCase "top-20 limiting" $ do
        let bigRegistry =
              foldr
                (\i -> registerTool (mkTestTool ("tool_match_" <> T.pack (show i)) "A matching tool"))
                newToolRegistry
                ([1 .. 25] :: [Int])
            (text, names) = searchTools bigRegistry "match"
        assertBool "Should load at most 20 tools" (Set.size names <= 20)
        assertBool "Should indicate more matches exist" ("20 of 25" `T.isInfixOf` text),
      testCase "multi-word query matches tool with all terms" $ do
        let (text, _names) = searchTools testRegistry "weather forecast"
        assertBool "Should match tool with both terms" ("mcp_weather_forecast" `T.isInfixOf` text)
        assertBool "Should not match calendar tool" (not $ "mcp_calendar_list" `T.isInfixOf` text),
      testCase "multi-word query rejects tool missing a term" $ do
        let (text, names) = searchTools testRegistry "github forecast"
        assertBool "Should not match any tool" ("No tools found" `T.isInfixOf` text)
        assertBool "Should return empty set" (Set.null names),
      testCase "name matches rank above description-only matches" $ do
        let (text, _names) = searchTools testRegistry "github issues"
        assertBool "Should contain github list issues tool" ("mcp_github_list_issues" `T.isInfixOf` text)
        -- Verify the name-match tool appears before description-only matches
        let githubPos = T.breakOn "mcp_github_list_issues" text
            issueTrackerPos = T.breakOn "general_issue_tracker" text
        assertBool
          "github_list_issues should rank before general_issue_tracker"
          (T.length (fst githubPos) < T.length (fst issueTrackerPos))
    ]
