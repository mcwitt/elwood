module Elwood.Claude.Conversation
  ( ConversationStore (..),
    newConversationStore,
    newInMemoryConversationStore,
  )
where

import Control.Concurrent.MVar
import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import Data.Char (isAlphaNum)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Elwood.Claude.Types
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

-- | In-memory cache of conversations
type ConversationCache = Map.Map Text Conversation

-- | Abstract store for conversation persistence.
-- Use 'newConversationStore' for file-backed persistence
-- or 'newInMemoryConversationStore' for testing.
data ConversationStore = ConversationStore
  { -- | Get or create a conversation for a session
    getConversation :: Text -> IO Conversation,
    -- | Append messages and extend the cache expiry (single write)
    appendMessages :: Text -> [ClaudeMessage] -> CacheTtl -> IO (),
    -- | Replace all messages and reset cache expiry to epoch
    replaceMessages :: Text -> [ClaudeMessage] -> IO (),
    -- | Clear a conversation's history
    clearConversation :: Text -> IO (),
    -- | Get all conversations currently in the cache
    allConversations :: IO (Map.Map Text Conversation)
  }

-- | Create a new file-backed conversation store
newConversationStore :: FilePath -> IO ConversationStore
newConversationStore dir = do
  let convDir = dir </> "conversations"
  createDirectoryIfMissing True convDir
  cache <- newMVar Map.empty
  pure
    ConversationStore
      { getConversation = fileGetConversation convDir cache,
        appendMessages = fileAppendMessages convDir cache,
        replaceMessages = fileReplaceMessages convDir cache,
        clearConversation = fileClearConversation convDir cache,
        allConversations = readMVar cache
      }

-- | Create an in-memory conversation store (no filesystem, useful for testing)
newInMemoryConversationStore :: IO ConversationStore
newInMemoryConversationStore = do
  cache <- newMVar Map.empty
  pure
    ConversationStore
      { getConversation = memGetConversation cache,
        appendMessages = memAppendMessages cache,
        replaceMessages = memReplaceMessages cache,
        clearConversation = memClearConversation cache,
        allConversations = readMVar cache
      }

-- ============================================================
-- File-backed implementation
-- ============================================================

-- | Sanitize a text value for use as a filename
sanitizeFilename :: Text -> String
sanitizeFilename = map sanitize . T.unpack
  where
    sanitize ch
      | isAlphaNum ch = ch
      | ch `elem` ['-', '_', '.'] = ch
      | otherwise = '_'

-- | Get the file path for a conversation
conversationPath :: FilePath -> Text -> FilePath
conversationPath convDir sid =
  convDir </> sanitizeFilename sid <> ".json"

fileGetConversation :: FilePath -> MVar ConversationCache -> Text -> IO Conversation
fileGetConversation convDir cache sid = do
  c <- readMVar cache
  case Map.lookup sid c of
    Just conv -> pure conv
    Nothing -> fileLoadOrCreateConversation convDir cache sid

fileLoadOrCreateConversation :: FilePath -> MVar ConversationCache -> Text -> IO Conversation
fileLoadOrCreateConversation convDir cache sid = do
  let path = conversationPath convDir sid
  exists <- doesFileExist path

  conv <-
    if exists
      then do
        result <- eitherDecodeFileStrict path
        case result of
          Right loadedConv -> pure loadedConv
          Left err -> fail $ "Failed to parse conversation file " <> path <> ": " <> err
      else pure $ createEmptyConversation sid

  -- Update cache
  modifyMVar_ cache $ \c ->
    pure $ Map.insert sid conv c

  pure conv

fileAppendMessages :: FilePath -> MVar ConversationCache -> Text -> [ClaudeMessage] -> CacheTtl -> IO ()
fileAppendMessages convDir cache sid msgs ttl = do
  now <- getCurrentTime

  -- Atomically read old conversation, append messages, extend expiry
  conv <- modifyMVar cache $ \c -> do
    let old = Map.findWithDefault (createEmptyConversation sid) sid c
        conv =
          Conversation
            { sessionId = sid,
              messages = old.messages ++ msgs,
              cacheExpiresAt = extendCacheExpiry now ttl old.cacheExpiresAt
            }
    pure (Map.insert sid conv c, conv)

  fileSaveConversation convDir conv

fileReplaceMessages :: FilePath -> MVar ConversationCache -> Text -> [ClaudeMessage] -> IO ()
fileReplaceMessages convDir cache sid msgs = do
  let conv =
        Conversation
          { sessionId = sid,
            messages = msgs,
            cacheExpiresAt = epoch
          }

  modifyMVar_ cache $ \c ->
    pure $ Map.insert sid conv c

  fileSaveConversation convDir conv

fileClearConversation :: FilePath -> MVar ConversationCache -> Text -> IO ()
fileClearConversation convDir cache sid = do
  let conv = createEmptyConversation sid

  -- Update cache
  modifyMVar_ cache $ \c ->
    pure $ Map.insert sid conv c

  -- Persist empty conversation
  fileSaveConversation convDir conv

fileSaveConversation :: FilePath -> Conversation -> IO ()
fileSaveConversation convDir conv = do
  let path = conversationPath convDir conv.sessionId
  encodeFile path conv

-- ============================================================
-- In-memory implementation
-- ============================================================

memGetConversation :: MVar ConversationCache -> Text -> IO Conversation
memGetConversation cache sid = do
  c <- readMVar cache
  case Map.lookup sid c of
    Just conv -> pure conv
    Nothing -> do
      let conv = createEmptyConversation sid
      modifyMVar_ cache $ \c' ->
        pure $ Map.insert sid conv c'
      pure conv

memAppendMessages :: MVar ConversationCache -> Text -> [ClaudeMessage] -> CacheTtl -> IO ()
memAppendMessages cache sid msgs ttl = do
  now <- getCurrentTime
  modifyMVar_ cache $ \c -> do
    let old = Map.findWithDefault (createEmptyConversation sid) sid c
        conv =
          Conversation
            { sessionId = sid,
              messages = old.messages ++ msgs,
              cacheExpiresAt = extendCacheExpiry now ttl old.cacheExpiresAt
            }
    pure $ Map.insert sid conv c

memReplaceMessages :: MVar ConversationCache -> Text -> [ClaudeMessage] -> IO ()
memReplaceMessages cache sid msgs = do
  let conv =
        Conversation
          { sessionId = sid,
            messages = msgs,
            cacheExpiresAt = epoch
          }
  modifyMVar_ cache $ \c ->
    pure $ Map.insert sid conv c

memClearConversation :: MVar ConversationCache -> Text -> IO ()
memClearConversation cache sid = do
  let conv = createEmptyConversation sid
  modifyMVar_ cache $ \c ->
    pure $ Map.insert sid conv c

-- ============================================================
-- Shared helpers
-- ============================================================

-- | Create an empty conversation with epoch expiry (cache considered expired)
createEmptyConversation :: Text -> Conversation
createEmptyConversation sid =
  Conversation
    { sessionId = sid,
      messages = [],
      cacheExpiresAt = epoch
    }
