{-# LANGUAGE StrictData #-}

module Elwood.Claude.Conversation
  ( ConversationStore (..),
    newConversationStore,
    getConversation,
    allConversations,
    updateConversation,
    clearConversation,
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

-- | Store for conversation persistence
data ConversationStore = ConversationStore
  { -- | Directory for conversation files
    stateDir :: FilePath,
    -- | In-memory cache
    cache :: MVar ConversationCache
  }

-- | Create a new conversation store
newConversationStore :: FilePath -> IO ConversationStore
newConversationStore dir = do
  -- Ensure conversations directory exists
  let convDir = dir </> "conversations"
  createDirectoryIfMissing True convDir

  c <- newMVar Map.empty
  pure
    ConversationStore
      { stateDir = convDir,
        cache = c
      }

-- | Sanitize a text value for use as a filename
sanitizeFilename :: Text -> String
sanitizeFilename = map sanitize . T.unpack
  where
    sanitize ch
      | isAlphaNum ch = ch
      | ch `elem` ['-', '_', '.'] = ch
      | otherwise = '_'

-- | Get the file path for a conversation
conversationPath :: ConversationStore -> Text -> FilePath
conversationPath store sid =
  store.stateDir </> sanitizeFilename sid <> ".json"

-- | Get or create a conversation for a session
getConversation :: ConversationStore -> Text -> IO Conversation
getConversation store sid = do
  c <- readMVar store.cache
  case Map.lookup sid c of
    Just conv -> pure conv
    Nothing -> loadOrCreateConversation store sid

-- | Load conversation from disk, or create a new one
loadOrCreateConversation :: ConversationStore -> Text -> IO Conversation
loadOrCreateConversation store sid = do
  let path = conversationPath store sid
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
  modifyMVar_ store.cache $ \c ->
    pure $ Map.insert sid conv c

  pure conv

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

-- | Save a conversation to disk
saveConversation :: ConversationStore -> Conversation -> IO ()
saveConversation store conv = do
  let path = conversationPath store conv.sessionId
  encodeFile path conv
    `catch` \(_ :: SomeException) ->
      -- Silently ignore write errors for now
      -- In a production system we'd want better error handling
      pure ()

-- | Update conversation with a complete message list
-- Used by agent loop to persist full conversation including tool interactions
-- Note: Context limits are managed by compaction, not message trimming
updateConversation :: ConversationStore -> Text -> [ClaudeMessage] -> IO ()
updateConversation store sid msgs = do
  now <- getCurrentTime

  let conv =
        Conversation
          { sessionId = sid,
            messages = msgs,
            lastUpdated = now
          }

  -- Update cache and persist
  modifyMVar_ store.cache $ \c ->
    pure $ Map.insert sid conv c

  saveConversation store conv

-- | Clear a conversation's history
clearConversation :: ConversationStore -> Text -> IO ()
clearConversation store sid = do
  conv <- createEmptyConversation sid

  -- Update cache
  modifyMVar_ store.cache $ \c ->
    pure $ Map.insert sid conv c

  -- Persist empty conversation
  saveConversation store conv

-- | Get all conversations currently in the cache
allConversations :: ConversationStore -> IO (Map.Map Text Conversation)
allConversations store = readMVar store.cache
