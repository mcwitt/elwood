module Elwood.Telegram.Polling
  ( MessageHandler,
    CallbackHandler,
    runPolling,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, catch)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Logging
import Elwood.Notify qualified as Notify
import Elwood.Telegram.Client (TelegramClient, getUpdatesAllowed, sendMessage)
import Elwood.Telegram.Types (CallbackQuery (..), Chat (..), Message (..), Update (..), User (..))

-- | Handler for incoming messages
-- Returns Just reply to send a response, Nothing to skip
type MessageHandler = Message -> IO (Maybe Text)

-- | Handler for callback queries (inline keyboard button presses)
-- Takes the callback query and returns Nothing (response is handled separately)
type CallbackHandler = CallbackQuery -> IO ()

-- | Run the Telegram long-polling loop
--
-- This function blocks indefinitely, polling for updates and dispatching
-- messages to the handler. Only messages from allowed chat IDs are processed.
-- Callback queries are dispatched to the callback handler.
runPolling ::
  Logger ->
  TelegramClient ->
  [Int64] ->
  MessageHandler ->
  CallbackHandler ->
  IO ()
runPolling logger client allowedChats handler callbackHandler = do
  logInfo logger "Starting Telegram polling loop" []
  offsetRef <- newIORef 0
  loop offsetRef
  where
    loop :: IORef Int -> IO ()
    loop offsetRef = do
      offset <- readIORef offsetRef

      -- Poll for updates with error handling
      updates <-
        getUpdatesWithCallbacks client offset `catch` \(e :: SomeException) -> do
          logError logger "Error fetching updates" [("error", T.pack (show e))]
          -- Wait before retrying on error
          threadDelay (5 * 1000000)
          pure []

      -- Process each update
      for_ updates $ \update -> do
        -- Always update offset to avoid reprocessing
        let newOffset = update.id_ + 1
        writeIORef offsetRef newOffset

        -- Process message if present (forked to avoid blocking callback processing)
        for_ update.message $ \msg -> do
          _ <- forkIO $ processMessage msg
          pure ()

        -- Process callback query if present
        for_ update.callbackQuery processCallback

      -- Continue polling
      loop offsetRef

    processCallback :: CallbackQuery -> IO ()
    processCallback cq = do
      let userName = cq.from_.firstName
      logInfo
        logger
        "Received callback query"
        [ ("callback_id", cq.id_),
          ("user", userName),
          ("data", fromMaybe "<no data>" cq.data_)
        ]

      -- Check if user is from an allowed chat
      case cq.message of
        Just msg ->
          let cid = msg.chat.id_
           in if cid `notElem` allowedChats
                then
                  logWarn
                    logger
                    "Ignoring callback from unauthorized chat"
                    [("chat_id", T.pack (show cid))]
                else do
                  callbackHandler cq `catch` \(e :: SomeException) ->
                    logError logger "Callback handler error" [("error", T.pack (show e))]
        Nothing ->
          logWarn logger "Callback query without message" []

    -- Extended getUpdates that includes callback_query in allowed_updates
    getUpdatesWithCallbacks :: TelegramClient -> Int -> IO [Update]
    getUpdatesWithCallbacks c o = getUpdatesAllowed c o ["message", "callback_query"]

    processMessage :: Message -> IO ()
    processMessage msg = do
      let cid = msg.chat.id_
          userName = maybe "unknown" (.firstName) msg.from_

      -- Check if chat is allowed
      if cid `notElem` allowedChats
        then
          logWarn
            logger
            "Ignoring message from unauthorized chat"
            [ ("chat_id", T.pack (show cid)),
              ("user", userName)
            ]
        else do
          logInfo
            logger
            "Received message"
            [ ("chat_id", T.pack (show cid)),
              ("user", userName),
              ("text", maybe "<no text>" (T.take 50) msg.text)
            ]

          -- Call handler and send reply if any
          reply <-
            handler msg `catch` \(e :: SomeException) -> do
              logError logger "Handler error" [("error", T.pack (show e))]
              pure $ Just $ Notify.formatNotify Notify.Error $ "**Handler error:** `" <> Notify.sanitizeBackticks (T.pack (show e)) <> "`"

          case reply of
            Nothing -> pure ()
            Just replyText -> do
              sendMessage client cid replyText `catch` \(e :: SomeException) ->
                logError
                  logger
                  "Failed to send reply"
                  [ ("chat_id", T.pack (show cid)),
                    ("error", T.pack (show e))
                  ]
              logInfo
                logger
                "Sent reply"
                [ ("chat_id", T.pack (show cid)),
                  ("text", T.take 50 replyText)
                ]
