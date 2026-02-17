module Elwood.Telegram.Polling
  ( MessageHandler
  , runPolling
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_)
import Data.Int (Int64)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T

import Elwood.Logging
import Elwood.Telegram.Client
import Elwood.Telegram.Types

-- | Handler for incoming messages
-- Returns Just reply to send a response, Nothing to skip
type MessageHandler = Message -> IO (Maybe Text)

-- | Run the Telegram long-polling loop
--
-- This function blocks indefinitely, polling for updates and dispatching
-- messages to the handler. Only messages from allowed chat IDs are processed.
runPolling ::
  Logger ->
  TelegramClient ->
  [Int64] ->
  MessageHandler ->
  IO ()
runPolling logger client allowedChats handler = do
  logInfo logger "Starting Telegram polling loop" []
  offsetRef <- newIORef 0
  loop offsetRef
  where
    loop :: IORef Int -> IO ()
    loop offsetRef = do
      offset <- readIORef offsetRef

      -- Poll for updates with error handling
      updates <-
        getUpdates client offset `catch` \(e :: SomeException) -> do
          logError logger "Error fetching updates" [("error", T.pack (show e))]
          -- Wait before retrying on error
          threadDelay (5 * 1000000)
          pure []

      -- Process each update
      forM_ updates $ \update -> do
        -- Always update offset to avoid reprocessing
        let newOffset = updateId update + 1
        writeIORef offsetRef newOffset

        -- Process message if present
        case message update of
          Nothing -> pure ()
          Just msg -> processMessage msg

      -- Continue polling
      loop offsetRef

    processMessage :: Message -> IO ()
    processMessage msg = do
      let cid = chatId (chat msg)
          userName = maybe "unknown" firstName (from msg)

      -- Check if chat is allowed
      if cid `notElem` allowedChats
        then
          logWarn
            logger
            "Ignoring message from unauthorized chat"
            [ ("chat_id", T.pack (show cid))
            , ("user", userName)
            ]
        else do
          logInfo
            logger
            "Received message"
            [ ("chat_id", T.pack (show cid))
            , ("user", userName)
            , ("text", maybe "<no text>" (T.take 50) (text msg))
            ]

          -- Call handler and send reply if any
          reply <-
            handler msg `catch` \(e :: SomeException) -> do
              logError logger "Handler error" [("error", T.pack (show e))]
              pure $ Just $ "Error processing message: " <> T.pack (show e)

          case reply of
            Nothing -> pure ()
            Just replyText -> do
              sendMessage client cid replyText `catch` \(e :: SomeException) ->
                logError
                  logger
                  "Failed to send reply"
                  [ ("chat_id", T.pack (show cid))
                  , ("error", T.pack (show e))
                  ]
              logInfo
                logger
                "Sent reply"
                [ ("chat_id", T.pack (show cid))
                , ("text", T.take 50 replyText)
                ]
