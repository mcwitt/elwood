module Elwood.Claude.Conversation (ConversationStore (..), newConversationStore, newInMemoryConversationStore) where

import Control.Concurrent.MVar
import Control.Exception (mask_)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Time (getCurrentTime)
import Elwood.Claude.Types
import Elwood.Persistence qualified as Persistence
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (joinPath, takeDirectory, (</>))

data ConversationStore = ConversationStore
  { getConversation :: Text -> IO Conversation,
    appendMessages :: Text -> [ClaudeMessage] -> Maybe CacheTtl -> IO (),
    replaceMessages :: Text -> [ClaudeMessage] -> IO (),
    clearConversation :: Text -> IO (),
    allConversations :: IO (Map.Map Text Conversation)
  }

newConversationStore :: FilePath -> IO ConversationStore
newConversationStore directory = do
  let root = directory </> "conversations" </> "v2"
      path sid = root </> (joinPath (if null chunks then ["empty"] else chunks) <> ".json")
        where
          chunks = map T.unpack (T.chunksOf 100 (T.pack (concatMap hex (BS.unpack (Text.encodeUtf8 sid)))))
      persist conversation = do
        createDirectoryIfMissing True (takeDirectory (path conversation.sessionId))
        Persistence.writeJSON (path conversation.sessionId) conversation
      hex byte = let digits = showHex byte "" in replicate (2 - length digits) '0' <> digits
  createDirectoryIfMissing True root
  let load sid = do
        current <- doesFileExist (path sid)
        let legacy = directory </> "conversations" </> map (\ch -> if isAlphaNum ch || ch `elem` ['-', '_', '.'] then ch else '_') (T.unpack sid) <> ".json"
        old <- doesFileExist legacy
        if current || not old
          then Persistence.readJSON (path sid) (empty sid)
          else do
            conversation <- Persistence.readJSON legacy (empty sid)
            if conversation.sessionId /= sid
              then ioError (userError "Legacy conversation filename collision: explicit migration required")
              else do
                persist conversation
                pure conversation
  store load persist

newInMemoryConversationStore :: IO ConversationStore
newInMemoryConversationStore = store (pure . empty) (const (pure ()))

empty :: Text -> Conversation
empty sid = Conversation sid [] epoch

store :: (Text -> IO Conversation) -> (Conversation -> IO ()) -> IO ConversationStore
store load save = do
  cache <- newMVar Map.empty
  let alter sid update = mask_ $ modifyMVar_ cache $ \current -> do
        old <- maybe (load sid) pure (Map.lookup sid current)
        when (old.sessionId /= sid) (ioError (userError "Conversation identifier does not match its file"))
        next <- update old
        save next
        pure (Map.insert sid next current)
      get sid = modifyMVar cache $ \current -> do
        value <- maybe (load sid) pure (Map.lookup sid current)
        if value.sessionId /= sid then ioError (userError "Conversation identifier does not match its file") else pure (Map.insert sid value current, value)
      replace sid messages = alter sid (const (pure (Conversation sid messages epoch)))
  pure
    ConversationStore
      { getConversation = get,
        appendMessages = \sid messages ttl ->
          alter
            sid
            ( \old -> do
                now <- getCurrentTime
                pure old {messages = old.messages <> messages, cacheExpiresAt = maybe old.cacheExpiresAt (\value -> extendCacheExpiry now value old.cacheExpiresAt) ttl}
            ),
        replaceMessages = replace,
        clearConversation = (`replace` []),
        allConversations = readMVar cache
      }
