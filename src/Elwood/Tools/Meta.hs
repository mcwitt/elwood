{-# LANGUAGE StrictData #-}

module Elwood.Tools.Meta
  ( mkFindToolsTool,
  )
where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.IORef (IORef, modifyIORef')
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Claude.Types (ToolSchema (..))
import Elwood.Tools.Registry (ActiveToolSet, ToolRegistry, activateTool, toolSchemas)
import Elwood.Tools.Types (Tool (..), ToolResult (..))

-- | Create the find_tools meta-tool
--
-- Searches available tools by case-insensitive substring match on name
-- and description, activates the top 5 matches, and returns their names
-- and descriptions.
mkFindToolsTool :: ToolRegistry -> IORef ActiveToolSet -> Tool
mkFindToolsTool registry activeRef =
  Tool
    { toolName = "find_tools",
      toolDescription =
        "Search for available tools by keyword. "
          <> "Returns matching tool names and descriptions, and makes them available for use. "
          <> "Use when you need a tool that isn't already available.",
      toolInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "properties"
              .= object
                [ "query"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("Keyword to search for in tool names and descriptions" :: Text)
                      ]
                ],
            "required" .= (["query"] :: [Text])
          ],
      toolExecute = \input -> do
        case extractQuery input of
          Nothing -> pure $ ToolError "Missing required parameter: query"
          Just query -> do
            let q = T.toLower query
                schemas = toolSchemas registry
                matches ts =
                  q `T.isInfixOf` T.toLower (tsName ts)
                    || q `T.isInfixOf` T.toLower (tsDescription ts)
                allMatches = filter matches schemas
                totalCount = length allMatches
                shown = take 5 allMatches

            if null shown
              then pure $ ToolSuccess "No tools found matching your query. Try different keywords."
              else do
                -- Activate shown tools
                mapM_ (modifyIORef' activeRef . activateTool . tsName) shown

                let formatTool ts = tsName ts <> " — " <> tsDescription ts
                    header
                      | totalCount > 5 =
                          "Showing 5 of "
                            <> T.pack (show totalCount)
                            <> " matches. Try a more specific query to find other tools.\n"
                      | otherwise = ""
                pure $ ToolSuccess $ header <> T.unlines (map formatTool shown)
    }

extractQuery :: Value -> Maybe Text
extractQuery (Object obj) = case KM.lookup (Key.fromText "query") obj of
  Just (String q) -> Just q
  _ -> Nothing
extractQuery _ = Nothing
