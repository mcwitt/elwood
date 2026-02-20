{-# LANGUAGE StrictData #-}

module Elwood.Claude.Conversation
  ( ConversationStore (..),
    newConversationStore,
    getConversation,
    allConversations,
    appendMessage,
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
    csStateDir :: FilePath,
    -- | In-memory cache
    csCache :: MVar ConversationCache
  }

-- | Create a new conversation store
newConversationStore :: FilePath -> IO ConversationStore
newConversationStore stateDir = do
  -- Ensure conversations directory exists
  let convDir = stateDir </> "conversations"
  createDirectoryIfMissing True convDir

  cache <- newMVar Map.empty
  pure
    ConversationStore
      { csStateDir = convDir,
        csCache = cache
      }

-- | Sanitize a text value for use as a filename
sanitizeFilename :: Text -> String
sanitizeFilename = map sanitize . T.unpack
  where
    sanitize c
      | isAlphaNum c = c
      | c `elem` ['-', '_', '.'] = c
      | otherwise = '_'

-- | Get the file path for a conversation
conversationPath :: ConversationStore -> Text -> FilePath
conversationPath store sessionId =
  csStateDir store </> sanitizeFilename sessionId <> ".json"

-- | Get or create a conversation for a session
getConversation :: ConversationStore -> Text -> IO Conversation
getConversation store sessionId = do
  cache <- readMVar (csCache store)
  case Map.lookup sessionId cache of
    Just conv -> pure conv
    Nothing -> loadOrCreateConversation store sessionId

-- | Load conversation from disk, or create a new one
loadOrCreateConversation :: ConversationStore -> Text -> IO Conversation
loadOrCreateConversation store sessionId = do
  let path = conversationPath store sessionId
  exists <- doesFileExist path

  conv <-
    if exists
      then do
        result <- eitherDecodeFileStrict path
        case result of
          Right loadedConv -> pure loadedConv
          Left _err -> createEmptyConversation sessionId
      else createEmptyConversation sessionId

  -- Update cache
  modifyMVar_ (csCache store) $ \c ->
    pure $ Map.insert sessionId conv c

  pure conv

-- | Create an empty conversation
createEmptyConversation :: Text -> IO Conversation
createEmptyConversation sessionId = do
  now <- getCurrentTime
  pure
    Conversation
      { convSessionId = sessionId,
        convMessages = [],
        convLastUpdated = now
      }

-- | Append a message to a conversation and persist to disk
-- Note: Context limits are managed by compaction, not message trimming
appendMessage :: ConversationStore -> Text -> ClaudeMessage -> IO Conversation
appendMessage store sessionId msg = do
  conv <- getConversation store sessionId
  now <- getCurrentTime

  let conv' =
        conv
          { convMessages = convMessages conv ++ [msg],
            convLastUpdated = now
          }

  -- Update cache and persist
  modifyMVar_ (csCache store) $ \c ->
    pure $ Map.insert sessionId conv' c

  saveConversation store conv'
  pure conv'

-- | Save a conversation to disk
saveConversation :: ConversationStore -> Conversation -> IO ()
saveConversation store conv = do
  let path = conversationPath store (convSessionId conv)
  encodeFile path conv
    `catch` \(_ :: SomeException) ->
      -- Silently ignore write errors for now
      -- In a production system we'd want better error handling
      pure ()

-- | Update conversation with a complete message list
-- Used by agent loop to persist full conversation including tool interactions
-- Note: Context limits are managed by compaction, not message trimming
updateConversation :: ConversationStore -> Text -> [ClaudeMessage] -> IO ()
updateConversation store sessionId messages = do
  now <- getCurrentTime

  let conv =
        Conversation
          { convSessionId = sessionId,
            convMessages = messages,
            convLastUpdated = now
          }

  -- Update cache and persist
  modifyMVar_ (csCache store) $ \c ->
    pure $ Map.insert sessionId conv c

  saveConversation store conv

-- | Clear a conversation's history
clearConversation :: ConversationStore -> Text -> IO ()
clearConversation store sessionId = do
  conv <- createEmptyConversation sessionId

  -- Update cache
  modifyMVar_ (csCache store) $ \c ->
    pure $ Map.insert sessionId conv c

  -- Persist empty conversation
  saveConversation store conv

-- | Get all conversations currently in the cache
allConversations :: ConversationStore -> IO (Map.Map Text Conversation)
allConversations store = readMVar (csCache store)
