module Elwood.Memory (MemoryStore, newMemoryStore, saveMemory, searchMemory, listMemories, MemoryResult (..), sanitizeKey) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Elwood.Persistence qualified as Persistence
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))

newtype MemoryStore = MemoryStore FilePath deriving stock (Show)

data MemoryResult = MemoryResult {key :: Text, content :: Text, score :: Int} deriving stock (Show, Eq)

newMemoryStore :: FilePath -> IO MemoryStore
newMemoryStore directory = do
  let path = directory </> "memory"
  createDirectoryIfMissing True path
  pure (MemoryStore path)

-- Keys are preserved exactly: accepting an input must not alias another key.
sanitizeKey :: Text -> Maybe Text
sanitizeKey key
  | not (T.null key), T.length key <= 100, BS.length (T.encodeUtf8 key) <= 240, T.all (\c -> isAlphaNum c || c `elem` ['-', '_']) key = Just key
  | otherwise = Nothing

saveMemory :: MemoryStore -> Text -> Text -> IO (Either Text ())
saveMemory (MemoryStore directory) key content = case sanitizeKey key of
  Nothing -> pure (Left "Invalid memory key: use 1–100 letters, digits, hyphens or underscores, at most 240 UTF-8 bytes")
  Just safe -> Right <$> Persistence.writeBytes (directory </> T.unpack safe <> ".md") (BL.fromStrict (T.encodeUtf8 content))

listMemories :: MemoryStore -> IO [(Text, Text)]
listMemories (MemoryStore directory) = do
  files <- filter ((== ".md") . takeExtension) <$> listDirectory directory
  traverse (\file -> (T.pack (takeBaseName file),) <$> T.readFile (directory </> file)) files

searchMemory :: MemoryStore -> Text -> IO [MemoryResult]
searchMemory store query = do
  memories <- listMemories store
  let terms = T.words (T.toLower query)
      score (key, content) = MemoryResult key content (length [term | term <- terms, term `T.isInfixOf` T.toLower (key <> " " <> content)])
  pure (sortOn (Down . (.score)) (filter ((> 0) . (.score)) (map score memories)))
