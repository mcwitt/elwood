{-# LANGUAGE StrictData #-}

module Elwood.Claude.Conversation
  ( ConversationStore (..),
    newConversationStore,
    getConversation,
    appendMessage,
    updateConversation,
    clearConversation,
  )
where

import Control.Concurrent.MVar
import Control.Exception (SomeException, catch)
import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Time (getCurrentTime)
import Elwood.Claude.Types
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

-- | In-memory cache of conversations
type ConversationCache = Map.Map Int64 Conversation

-- | Store for conversation persistence
data ConversationStore = ConversationStore
  { -- | Directory for conversation files
    csStateDir :: FilePath,
    -- | Maximum messages to keep per conversation
    csMaxHistory :: Int,
    -- | In-memory cache
    csCache :: MVar ConversationCache
  }

-- | Create a new conversation store
newConversationStore :: FilePath -> Int -> IO ConversationStore
newConversationStore stateDir maxHistory = do
  -- Ensure conversations directory exists
  let convDir = stateDir </> "conversations"
  createDirectoryIfMissing True convDir

  cache <- newMVar Map.empty
  pure
    ConversationStore
      { csStateDir = convDir,
        csMaxHistory = maxHistory,
        csCache = cache
      }

-- | Get the file path for a conversation
conversationPath :: ConversationStore -> Int64 -> FilePath
conversationPath store chatId =
  csStateDir store </> show chatId <> ".json"

-- | Get or create a conversation for a chat
getConversation :: ConversationStore -> Int64 -> IO Conversation
getConversation store chatId = do
  cache <- readMVar (csCache store)
  case Map.lookup chatId cache of
    Just conv -> pure conv
    Nothing -> loadOrCreateConversation store chatId

-- | Load conversation from disk, or create a new one
loadOrCreateConversation :: ConversationStore -> Int64 -> IO Conversation
loadOrCreateConversation store chatId = do
  let path = conversationPath store chatId
  exists <- doesFileExist path

  conv <-
    if exists
      then do
        result <- eitherDecodeFileStrict path
        case result of
          Right conv -> pure conv
          Left _err -> createEmptyConversation chatId
      else createEmptyConversation chatId

  -- Update cache
  modifyMVar_ (csCache store) $ \c ->
    pure $ Map.insert chatId conv c

  pure conv

-- | Create an empty conversation
createEmptyConversation :: Int64 -> IO Conversation
createEmptyConversation chatId = do
  now <- getCurrentTime
  pure
    Conversation
      { convChatId = chatId,
        convMessages = [],
        convLastUpdated = now
      }

-- | Append a message to a conversation
-- Trims to maxHistory and persists to disk
appendMessage :: ConversationStore -> Int64 -> ClaudeMessage -> IO Conversation
appendMessage store chatId msg = do
  conv <- getConversation store chatId
  now <- getCurrentTime

  let messages = convMessages conv ++ [msg]
      -- Keep only the last maxHistory messages
      trimmed = takeEnd (csMaxHistory store) messages
      conv' =
        conv
          { convMessages = trimmed,
            convLastUpdated = now
          }

  -- Update cache and persist
  modifyMVar_ (csCache store) $ \c ->
    pure $ Map.insert chatId conv' c

  saveConversation store conv'
  pure conv'
  where
    takeEnd n xs = drop (max 0 (length xs - n)) xs

-- | Save a conversation to disk
saveConversation :: ConversationStore -> Conversation -> IO ()
saveConversation store conv = do
  let path = conversationPath store (convChatId conv)
  encodeFile path conv
    `catch` \(_ :: SomeException) ->
      -- Silently ignore write errors for now
      -- In a production system we'd want better error handling
      pure ()

-- | Update conversation with a complete message list
-- Used by agent loop to persist full conversation including tool interactions
updateConversation :: ConversationStore -> Int64 -> [ClaudeMessage] -> IO ()
updateConversation store chatId messages = do
  now <- getCurrentTime

  let trimmed = takeEnd (csMaxHistory store) messages
      conv =
        Conversation
          { convChatId = chatId,
            convMessages = trimmed,
            convLastUpdated = now
          }

  -- Update cache and persist
  modifyMVar_ (csCache store) $ \c ->
    pure $ Map.insert chatId conv c

  saveConversation store conv
  where
    takeEnd n xs = drop (max 0 (length xs - n)) xs

-- | Clear a conversation's history
clearConversation :: ConversationStore -> Int64 -> IO ()
clearConversation store chatId = do
  conv <- createEmptyConversation chatId

  -- Update cache
  modifyMVar_ (csCache store) $ \c ->
    pure $ Map.insert chatId conv c

  -- Persist empty conversation
  saveConversation store conv
