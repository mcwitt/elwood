{-# LANGUAGE StrictData #-}

module Elwood.Tools.Meta
  ( mkDiscoverToolsTool,
    mkLoadToolTool,
  )
where

import Data.Aeson (Value (..), encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, modifyIORef')
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Elwood.Claude.Types (ToolSchema (..))
import Elwood.Tools.Registry (ActiveToolSet, ToolRegistry, activateTool, lookupTool, toolSchemas)
import Elwood.Tools.Types (Tool (..), ToolResult (..))

-- | Create the discover_tools meta-tool
--
-- Lists available tools with name and description, optionally filtered
-- by a case-insensitive substring match on name or description.
mkDiscoverToolsTool :: ToolRegistry -> Tool
mkDiscoverToolsTool registry =
  Tool
    { toolName = "discover_tools",
      toolDescription =
        "List available tools. Returns tool names and descriptions. "
          <> "Use the optional 'query' parameter to filter by substring match on name or description.",
      toolInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "properties"
              .= object
                [ "query"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("Optional substring to filter tools by name or description" :: Text)
                      ]
                ],
            "required" .= ([] :: [Text])
          ],
      toolExecute = \input -> do
        let mQuery = case input of
              Object obj -> case KM.lookup (Key.fromText "query") obj of
                Just (String q) -> Just (T.toLower q)
                _ -> Nothing
              _ -> Nothing

            schemas = toolSchemas registry

            matches ts = case mQuery of
              Nothing -> True
              Just q ->
                q `T.isInfixOf` T.toLower (tsName ts)
                  || q `T.isInfixOf` T.toLower (tsDescription ts)

            filtered = filter matches schemas

            formatTool ts = tsName ts <> " — " <> tsDescription ts

        if null filtered
          then pure $ ToolSuccess "No tools found matching your query."
          else pure $ ToolSuccess $ T.unlines (map formatTool filtered)
    }

-- | Create the load_tool meta-tool
--
-- Loads a tool by name into the active tool set for the current turn.
-- Returns the tool's description and full input schema as confirmation.
mkLoadToolTool :: ToolRegistry -> IORef ActiveToolSet -> Tool
mkLoadToolTool registry activeRef =
  Tool
    { toolName = "load_tool",
      toolDescription =
        "Load a tool by name so it becomes available for use in this conversation turn. "
          <> "Use discover_tools first to find available tool names.",
      toolInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "properties"
              .= object
                [ "name"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("The exact name of the tool to load" :: Text)
                      ]
                ],
            "required" .= (["name"] :: [Text])
          ],
      toolExecute = \input -> do
        case extractName input of
          Nothing -> pure $ ToolError "Missing required parameter: name"
          Just name -> case lookupTool name registry of
            Nothing -> pure $ ToolError $ "Unknown tool: " <> name <> ". Use discover_tools to see available tools."
            Just tool -> do
              modifyIORef' activeRef (activateTool name)
              let schema = decodeUtf8 (LBS.toStrict (encode (toolInputSchema tool)))
              pure $
                ToolSuccess $
                  "Loaded tool: "
                    <> name
                    <> "\n\nDescription: "
                    <> toolDescription tool
                    <> "\n\nInput schema: "
                    <> schema
    }

extractName :: Value -> Maybe Text
extractName (Object obj) = case KM.lookup (Key.fromText "name") obj of
  Just (String n) -> Just n
  _ -> Nothing
extractName _ = Nothing
