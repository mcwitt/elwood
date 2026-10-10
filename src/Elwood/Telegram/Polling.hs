module Elwood.Telegram.Polling (MessageHandler, CallbackHandler, runPolling) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Exception (catchSync)
import Elwood.Logging (Logger, logError)
import Elwood.Telegram.Client (TelegramClient, getUpdatesAllowed, sendMessage)
import Elwood.Telegram.Types

type MessageHandler = Message -> IO (Maybe Text)

type CallbackHandler = CallbackQuery -> IO ()

-- The supplied dispatcher reserves each message's lane before the next update
-- is admitted. Approval callbacks bypass lanes so they can unblock active turns.
runPolling :: Logger -> TelegramClient -> [Int64] -> (Message -> IO () -> IO ()) -> MessageHandler -> CallbackHandler -> IO ()
runPolling logger client allowed dispatch handler callback = loop 0
  where
    loop offset = do
      updates <- catchSync (getUpdatesAllowed client offset ["message", "callback_query"]) $ \err -> do
        report err
        threadDelay 5000000
        pure []
      forM_ updates $ \update -> case update.payload of
        UpdateMessage message -> when (message.chat.id_ `elem` allowed) $ dispatch message $ do
          reply <- catchSync (handler message) $ \err -> report err >> pure (Just "The request failed.")
          forM_ reply (\text -> catchSync (sendMessage client message.chat.id_ text) report)
        UpdateCallbackQuery query -> forM_ query.message $ \message ->
          when (message.chat.id_ `elem` allowed) (catchSync (callback query) report)
        UpdateIgnored -> pure ()
      loop (maximum (offset : map ((+ 1) . (.id_)) updates))
    report err = logError logger "Telegram processing failed" [("error", T.pack (show err))]
