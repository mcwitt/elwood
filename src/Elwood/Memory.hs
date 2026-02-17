{-# LANGUAGE StrictData #-}

module Elwood.Memory
  ( -- * Memory Store
    MemoryStore
  , newMemoryStore

    -- * Operations
  , saveMemory
  , searchMemory
  , listMemories
  , readMemory

    -- * Types
  , MemoryResult (..)

    -- * Exported for testing
  , sanitizeKey
  ) where

import Control.Exception (SomeException, catch)
import Data.Char (isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , listDirectory
  )
import System.FilePath ((</>), takeBaseName)

-- | Memory store handle
data MemoryStore = MemoryStore
  { msDirectory :: FilePath
  -- ^ Directory containing memory files
  }
  deriving stock (Show)

-- | Result from a memory search
data MemoryResult = MemoryResult
  { mrKey :: Text
  -- ^ Memory key (filename without extension)
  , mrContent :: Text
  -- ^ Memory content
  , mrScore :: Int
  -- ^ Relevance score (number of matching terms)
  }
  deriving stock (Show, Eq)

-- | Create a new memory store
newMemoryStore :: FilePath -> IO MemoryStore
newMemoryStore stateDir = do
  let memDir = stateDir </> "memory"
  createDirectoryIfMissing True memDir
  pure MemoryStore {msDirectory = memDir}

-- | Save a memory with the given key
-- Keys are sanitized to be filesystem-safe
saveMemory :: MemoryStore -> Text -> Text -> IO (Either Text ())
saveMemory store key content = do
  case sanitizeKey key of
    Nothing -> pure $ Left "Invalid memory key (must contain alphanumeric characters)"
    Just safeKey -> do
      let path = msDirectory store </> T.unpack safeKey <> ".md"
      result <- catch
        (Right <$> TIO.writeFile path content)
        (\(e :: SomeException) -> pure $ Left $ "Failed to save memory: " <> T.pack (show e))
      pure result

-- | Read a specific memory by key
readMemory :: MemoryStore -> Text -> IO (Maybe Text)
readMemory store key = do
  case sanitizeKey key of
    Nothing -> pure Nothing
    Just safeKey -> do
      let path = msDirectory store </> T.unpack safeKey <> ".md"
      exists <- doesFileExist path
      if exists
        then catch
          (Just <$> TIO.readFile path)
          (\(_ :: SomeException) -> pure Nothing)
        else pure Nothing

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
      pure $ sortOn (Down . mrScore) $ filter ((> 0) . mrScore) results
  where
    scoreMemory :: [Text] -> (Text, Text) -> IO MemoryResult
    scoreMemory terms (key, content) = do
      let lowerContent = T.toLower content
          lowerKey = T.toLower key
          -- Score: count how many terms appear in content or key
          score = length $ filter (\t -> t `T.isInfixOf` lowerContent || t `T.isInfixOf` lowerKey) terms
      pure MemoryResult
        { mrKey = key
        , mrContent = content
        , mrScore = score
        }

-- | List all memories with their contents
listMemories :: MemoryStore -> IO [(Text, Text)]
listMemories store = do
  files <- catch
    (listDirectory (msDirectory store))
    (\(_ :: SomeException) -> pure [])
  let mdFiles = filter (\f -> ".md" `T.isSuffixOf` T.pack f) files
  mapM readMemoryFile mdFiles
  where
    readMemoryFile :: FilePath -> IO (Text, Text)
    readMemoryFile filename = do
      let key = T.pack $ takeBaseName filename
          path = msDirectory store </> filename
      content <- catch
        (TIO.readFile path)
        (\(_ :: SomeException) -> pure "")
      pure (key, content)

-- | Sanitize a key to be filesystem-safe
-- Returns Nothing if the key would be empty after sanitization
sanitizeKey :: Text -> Maybe Text
sanitizeKey key =
  let sanitized = T.filter (\c -> isAlphaNum c || c == '-' || c == '_') key
   in if T.null sanitized
        then Nothing
        else Just $ T.take 100 sanitized  -- Limit length
