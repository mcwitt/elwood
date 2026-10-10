module Elwood.Tools.Memory (mkSaveMemoryTool, mkSearchMemoryTool) where

import Data.Aeson (withObject)
import Data.Text qualified as T
import Elwood.Logging (Logger, logInfo)
import Elwood.Memory qualified as Memory
import Elwood.Tools.Builder qualified as B
import Elwood.Tools.Types

mkSaveMemoryTool :: Logger -> Memory.MemoryStore -> Tool
mkSaveMemoryTool logger store = B.tool "save_memory" "Save persistent information under a key of 1–100 letters, digits, hyphens or underscores." fields parser $ \(key, content) -> do
  logInfo logger "Saving memory" [("key", key)]
  either toolError (const (toolSuccess ("Memory saved with key: " <> key))) <$> Memory.saveMemory store key content
  where
    fields = B.schema [("key", B.field "string" "Memory identifier"), ("content", B.field "string" "Information to remember")] ["key", "content"]
    parser = withObject "save_memory" $ \value -> (,) <$> B.nonempty value "key" <*> B.text value "content"

mkSearchMemoryTool :: Logger -> Memory.MemoryStore -> Tool
mkSearchMemoryTool logger store = B.tool "search_memory" "Search persistent memory by keywords." (B.schema [("query", B.field "string" "Keywords")] ["query"]) (withObject "search_memory" (`B.text` "query")) $ \query -> do
  logInfo logger "Searching memory" [("query", query)]
  results <- Memory.searchMemory store query
  pure $ toolSuccess $ if null results then "No memories found matching the query." else T.intercalate "\n\n" ["**" <> r.key <> "**\n" <> T.take 500 r.content <> if T.length r.content > 500 then "..." else "" | r <- results]
