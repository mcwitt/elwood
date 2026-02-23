{-# LANGUAGE StrictData #-}

module Elwood.Tools.Memory
  ( mkSaveMemoryTool,
    mkSearchMemoryTool,
  )
where

import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Logging (Logger, logInfo)
import Elwood.Memory (MemoryResult (..), MemoryStore, saveMemory, searchMemory)
import Elwood.Tools.Types

-- | Construct a tool for saving memories
mkSaveMemoryTool :: Logger -> MemoryStore -> Tool
mkSaveMemoryTool logger memStore =
  Tool
    { name = "save_memory",
      description =
        "Save information to persistent memory for future reference. "
          <> "Use this to remember important facts, preferences, or information "
          <> "that should persist across conversations. "
          <> "The key should be a short, descriptive identifier.",
      inputSchema = saveMemorySchema,
      execute = \input -> case parseSaveInput input of
        Left err -> pure $ toolError err
        Right (k, c) -> do
          logInfo logger "Saving memory" [("key", k)]
          result <- saveMemory memStore k c
          case result of
            Left err -> pure $ toolError err
            Right () -> pure $ toolSuccess $ "Memory saved with key: " <> k
    }

-- | Construct a tool for searching memories
mkSearchMemoryTool :: Logger -> MemoryStore -> Tool
mkSearchMemoryTool logger memStore =
  Tool
    { name = "search_memory",
      description =
        "Search persistent memory for previously saved information. "
          <> "Use this to recall facts, preferences, or context from past conversations. "
          <> "The query can contain multiple keywords.",
      inputSchema = searchMemorySchema,
      execute = \input -> case parseSearchInput input of
        Left err -> pure $ toolError err
        Right query -> do
          logInfo logger "Searching memory" [("query", query)]
          results <- searchMemory memStore query
          if null results
            then pure $ toolSuccess "No memories found matching the query."
            else pure $ toolSuccess $ formatResults results
    }

-- | JSON Schema for save_memory input
saveMemorySchema :: Value
saveMemorySchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "key"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("Short identifier for this memory (e.g., 'user-birthday', 'project-goals')" :: Text)
                ],
            "content"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("The information to remember" :: Text)
                ]
          ],
      "required" .= (["key", "content"] :: [Text])
    ]

-- | JSON Schema for search_memory input
searchMemorySchema :: Value
searchMemorySchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "query"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("Keywords to search for in memories" :: Text)
                ]
          ],
      "required" .= (["query"] :: [Text])
    ]

-- | Parse save_memory input
parseSaveInput :: Value -> Either Text (Text, Text)
parseSaveInput (Aeson.Object obj) = do
  k <- case KM.lookup "key" obj of
    Just (Aeson.String kv) -> Right kv
    _ -> Left "Missing or invalid 'key' parameter"
  c <- case KM.lookup "content" obj of
    Just (Aeson.String cv) -> Right cv
    _ -> Left "Missing or invalid 'content' parameter"
  Right (k, c)
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
      "**" <> r.key <> "**\n" <> limitContent r.content

    limitContent :: Text -> Text
    limitContent t
      | T.length t > 500 = T.take 500 t <> "..."
      | otherwise = t
