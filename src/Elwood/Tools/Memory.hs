{-# LANGUAGE StrictData #-}

module Elwood.Tools.Memory
  ( saveMemoryTool
  , searchMemoryTool
  ) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

import Elwood.Logging (logInfo)
import Elwood.Memory (MemoryResult (..), saveMemory, searchMemory)
import Elwood.Tools.Types

-- | Tool for saving memories
saveMemoryTool :: Tool
saveMemoryTool =
  Tool
    { toolName = "save_memory"
    , toolDescription =
        "Save information to persistent memory for future reference. "
          <> "Use this to remember important facts, preferences, or information "
          <> "that should persist across conversations. "
          <> "The key should be a short, descriptive identifier."
    , toolInputSchema = saveMemorySchema
    , toolExecute = executeSaveMemory
    }

-- | Tool for searching memories
searchMemoryTool :: Tool
searchMemoryTool =
  Tool
    { toolName = "search_memory"
    , toolDescription =
        "Search persistent memory for previously saved information. "
          <> "Use this to recall facts, preferences, or context from past conversations. "
          <> "The query can contain multiple keywords."
    , toolInputSchema = searchMemorySchema
    , toolExecute = executeSearchMemory
    }

-- | JSON Schema for save_memory input
saveMemorySchema :: Value
saveMemorySchema =
  object
    [ "type" .= ("object" :: Text)
    , "properties"
        .= object
          [ "key"
              .= object
                [ "type" .= ("string" :: Text)
                , "description" .= ("Short identifier for this memory (e.g., 'user-birthday', 'project-goals')" :: Text)
                ]
          , "content"
              .= object
                [ "type" .= ("string" :: Text)
                , "description" .= ("The information to remember" :: Text)
                ]
          ]
    , "required" .= (["key", "content"] :: [Text])
    ]

-- | JSON Schema for search_memory input
searchMemorySchema :: Value
searchMemorySchema =
  object
    [ "type" .= ("object" :: Text)
    , "properties"
        .= object
          [ "query"
              .= object
                [ "type" .= ("string" :: Text)
                , "description" .= ("Keywords to search for in memories" :: Text)
                ]
          ]
    , "required" .= (["query"] :: [Text])
    ]

-- | Execute save_memory
executeSaveMemory :: ToolEnv -> Value -> IO ToolResult
executeSaveMemory env input = do
  case parseSaveInput input of
    Left err -> pure $ toolError err
    Right (key, content) -> do
      logInfo (teLogger env) "Saving memory" [("key", key)]
      result <- saveMemory (teMemoryStore env) key content
      case result of
        Left err -> pure $ toolError err
        Right () -> pure $ toolSuccess $ "Memory saved with key: " <> key

-- | Execute search_memory
executeSearchMemory :: ToolEnv -> Value -> IO ToolResult
executeSearchMemory env input = do
  case parseSearchInput input of
    Left err -> pure $ toolError err
    Right query -> do
      logInfo (teLogger env) "Searching memory" [("query", query)]
      results <- searchMemory (teMemoryStore env) query
      if null results
        then pure $ toolSuccess "No memories found matching the query."
        else pure $ toolSuccess $ formatResults results

-- | Parse save_memory input
parseSaveInput :: Value -> Either Text (Text, Text)
parseSaveInput (Aeson.Object obj) = do
  key <- case KM.lookup "key" obj of
    Just (Aeson.String k) -> Right k
    _ -> Left "Missing or invalid 'key' parameter"
  content <- case KM.lookup "content" obj of
    Just (Aeson.String c) -> Right c
    _ -> Left "Missing or invalid 'content' parameter"
  Right (key, content)
parseSaveInput _ = Left "Expected object input"

-- | Parse search_memory input
parseSearchInput :: Value -> Either Text Text
parseSearchInput (Aeson.Object obj) =
  case KM.lookup "query" obj of
    Just (Aeson.String q) -> Right q
    _ -> Left "Missing or invalid 'query' parameter"
parseSearchInput _ = Left "Expected object input"

-- | Format search results for display
formatResults :: [MemoryResult] -> Text
formatResults results =
  T.intercalate "\n\n" $ map formatResult results
  where
    formatResult :: MemoryResult -> Text
    formatResult r =
      "**" <> mrKey r <> "**\n" <> limitContent (mrContent r)

    limitContent :: Text -> Text
    limitContent t
      | T.length t > 500 = T.take 500 t <> "..."
      | otherwise = t
