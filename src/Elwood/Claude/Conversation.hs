{-# LANGUAGE StrictData #-}

module Elwood.Claude.Conversation
  ( ConversationStore (..),
    newConversationStore,
    newInMemoryConversationStore,
  )
where

import Control.Concurrent.MVar
import Control.Exception (SomeException, catch)
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
    -- | Update conversation with a complete message list
    updateConversation :: Text -> [ClaudeMessage] -> IO (),
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
        updateConversation = fileUpdateConversation convDir cache,
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
        updateConversation = memUpdateConversation cache,
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
          Left _err -> createEmptyConversation sid
      else createEmptyConversation sid

  -- Update cache
  modifyMVar_ cache $ \c ->
    pure $ Map.insert sid conv c

  pure conv

fileUpdateConversation :: FilePath -> MVar ConversationCache -> Text -> [ClaudeMessage] -> IO ()
fileUpdateConversation convDir cache sid msgs = do
  now <- getCurrentTime

  let conv =
        Conversation
          { sessionId = sid,
            messages = msgs,
            lastUpdated = now
          }

  -- Update cache and persist
  modifyMVar_ cache $ \c ->
    pure $ Map.insert sid conv c

  fileSaveConversation convDir conv

fileClearConversation :: FilePath -> MVar ConversationCache -> Text -> IO ()
fileClearConversation convDir cache sid = do
  conv <- createEmptyConversation sid

  -- Update cache
  modifyMVar_ cache $ \c ->
    pure $ Map.insert sid conv c

  -- Persist empty conversation
  fileSaveConversation convDir conv

fileSaveConversation :: FilePath -> Conversation -> IO ()
fileSaveConversation convDir conv = do
  let path = conversationPath convDir conv.sessionId
  encodeFile path conv
    `catch` \(_ :: SomeException) ->
      -- Silently ignore write errors for now
      -- In a production system we'd want better error handling
      pure ()

-- ============================================================
-- In-memory implementation
-- ============================================================

memGetConversation :: MVar ConversationCache -> Text -> IO Conversation
memGetConversation cache sid = do
  c <- readMVar cache
  case Map.lookup sid c of
    Just conv -> pure conv
    Nothing -> do
      conv <- createEmptyConversation sid
      modifyMVar_ cache $ \c' ->
        pure $ Map.insert sid conv c'
      pure conv

memUpdateConversation :: MVar ConversationCache -> Text -> [ClaudeMessage] -> IO ()
memUpdateConversation cache sid msgs = do
  now <- getCurrentTime
  let conv =
        Conversation
          { sessionId = sid,
            messages = msgs,
            lastUpdated = now
          }
  modifyMVar_ cache $ \c ->
    pure $ Map.insert sid conv c

memClearConversation :: MVar ConversationCache -> Text -> IO ()
memClearConversation cache sid = do
  conv <- createEmptyConversation sid
  modifyMVar_ cache $ \c ->
    pure $ Map.insert sid conv c

-- ============================================================
-- Shared helpers
-- ============================================================

-- | Create an empty conversation
createEmptyConversation :: Text -> IO Conversation
createEmptyConversation sid = do
  now <- getCurrentTime
  pure
    Conversation
      { sessionId = sid,
        messages = [],
        lastUpdated = now
      }
