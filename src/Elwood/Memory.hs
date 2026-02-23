{-# LANGUAGE StrictData #-}

module Elwood.Memory
  ( -- * Memory Store
    MemoryStore,
    newMemoryStore,

    -- * Operations
    saveMemory,
    searchMemory,
    listMemories,

    -- * Types
    MemoryResult (..),

    -- * Exported for testing
    sanitizeKey,
  )
where

import Control.Exception (SomeException, catch)
import Data.Char (isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory
  ( createDirectoryIfMissing,
    listDirectory,
  )
import System.FilePath (takeBaseName, (</>))

-- | Memory store handle
newtype MemoryStore = MemoryStore
  { -- | Directory containing memory files
    directory :: FilePath
  }
  deriving stock (Show)

-- | Result from a memory search
data MemoryResult = MemoryResult
  { -- | Memory key (filename without extension)
    key :: Text,
    -- | Memory content
    content :: Text,
    -- | Relevance score (number of matching terms)
    score :: Int
  }
  deriving stock (Show, Eq)

-- | Create a new memory store
newMemoryStore :: FilePath -> IO MemoryStore
newMemoryStore stateDir_ = do
  let memDir = stateDir_ </> "memory"
  createDirectoryIfMissing True memDir
  pure MemoryStore {directory = memDir}

-- | Save a memory with the given key
-- Keys are sanitized to be filesystem-safe
saveMemory :: MemoryStore -> Text -> Text -> IO (Either Text ())
saveMemory store k c = do
  case sanitizeKey k of
    Nothing -> pure $ Left "Invalid memory key (must contain alphanumeric characters)"
    Just safeKey -> do
      let path = store.directory </> T.unpack safeKey <> ".md"
      catch
        (Right <$> TIO.writeFile path c)
        (\(e :: SomeException) -> pure $ Left $ "Failed to save memory: " <> T.pack (show e))

-- | Search memories by keyword
-- Returns memories containing any of the search terms, sorted by relevance
searchMemory :: MemoryStore -> Text -> IO [MemoryResult]
searchMemory store query = do
  let terms = filter (not . T.null) $ T.words (T.toLower query)
  if null terms
    then pure []
    else do
      memories <- listMemories store
      results <- mapM (scoreMemory terms) memories
      pure $ sortOn (Down . (.score)) $ filter (\r -> r.score > 0) results
  where
    scoreMemory :: [Text] -> (Text, Text) -> IO MemoryResult
    scoreMemory terms (k, c) = do
      let lowerContent = T.toLower c
          lowerKey = T.toLower k
          -- Score: count how many terms appear in content or key
          s = length $ filter (\t -> t `T.isInfixOf` lowerContent || t `T.isInfixOf` lowerKey) terms
      pure
        MemoryResult
          { key = k,
            content = c,
            score = s
          }

-- | List all memories with their contents
listMemories :: MemoryStore -> IO [(Text, Text)]
listMemories store = do
  files <-
    catch
      (listDirectory store.directory)
      (\(_ :: SomeException) -> pure [])
  let mdFiles = filter (\f -> ".md" `T.isSuffixOf` T.pack f) files
  mapM readMemoryFile mdFiles
  where
    readMemoryFile :: FilePath -> IO (Text, Text)
    readMemoryFile filename = do
      let k = T.pack $ takeBaseName filename
          path = store.directory </> filename
      c <-
        catch
          (TIO.readFile path)
          (\(_ :: SomeException) -> pure "")
      pure (k, c)

-- | Sanitize a key to be filesystem-safe
-- Returns Nothing if the key would be empty after sanitization
sanitizeKey :: Text -> Maybe Text
sanitizeKey k =
  let sanitized = T.filter (\c -> isAlphaNum c || c == '-' || c == '_') k
   in if T.null sanitized
        then Nothing
        else Just $ T.take 100 sanitized -- Limit length
