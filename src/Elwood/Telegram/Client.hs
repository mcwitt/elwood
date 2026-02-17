{-# LANGUAGE StrictData #-}

module Elwood.Telegram.Client
  ( TelegramClient (..)
  , TelegramError (..)
  , newTelegramClient
  , getUpdates
  , sendMessage
  ) where

import Control.Exception (Exception, throwIO)
import Data.Aeson (encode, eitherDecode, object, (.=))
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

import Elwood.Telegram.Types

-- | Telegram API client
data TelegramClient = TelegramClient
  { tcManager :: Manager
  -- ^ HTTP connection manager
  , tcToken :: Text
  -- ^ Bot token
  , tcBaseUrl :: String
  -- ^ Base URL for API calls
  }

-- | Errors from Telegram API calls
data TelegramError
  = TelegramHttpError Int ByteString
  | TelegramParseError String
  | TelegramApiError String
  deriving stock (Show)

instance Exception TelegramError

-- | Create a new Telegram client
newTelegramClient :: Text -> IO TelegramClient
newTelegramClient token = do
  manager <- newManager tlsManagerSettings
  pure
    TelegramClient
      { tcManager = manager
      , tcToken = token
      , tcBaseUrl = "https://api.telegram.org/bot" <> T.unpack token
      }

-- | Build a request for a Telegram API method
buildRequest :: TelegramClient -> String -> IO Request
buildRequest client method = do
  req <- parseRequest $ tcBaseUrl client <> "/" <> method
  pure
    req
      { requestHeaders =
          [ ("Content-Type", "application/json")
          ]
      }

-- | Get updates from Telegram using long polling
--
-- Parameters:
--   - client: The Telegram client
--   - offset: Update offset (use last update_id + 1)
--
-- Returns a list of updates
getUpdates :: TelegramClient -> Int -> IO [Update]
getUpdates client offset = do
  req <- buildRequest client "getUpdates"
  let body =
        encode $
          object
            [ "offset" .= offset
            , "timeout" .= (30 :: Int)
            , "allowed_updates" .= (["message"] :: [Text])
            ]
      req' =
        req
          { method = "POST"
          , requestBody = RequestBodyLBS body
          -- Set timeout longer than Telegram's 30s long poll
          , responseTimeout = responseTimeoutMicro (35 * 1000000)
          }

  response <- httpLbs req' (tcManager client)

  let status = statusCode $ responseStatus response
  if status /= 200
    then throwIO $ TelegramHttpError status (responseBody response)
    else case eitherDecode (responseBody response) of
      Left err -> throwIO $ TelegramParseError err
      Right resp
        | gurOk resp -> pure $ gurResult resp
        | otherwise -> throwIO $ TelegramApiError "API returned ok=false"

-- | Send a message to a chat
sendMessage :: TelegramClient -> Int64 -> Text -> IO ()
sendMessage client chatId msgText = do
  req <- buildRequest client "sendMessage"
  let body =
        encode
          SendMessageRequest
            { smrChatId = chatId
            , smrText = msgText
            , smrParseMode = Nothing
            }
      req' =
        req
          { method = "POST"
          , requestBody = RequestBodyLBS body
          }

  response <- httpLbs req' (tcManager client)

  let status = statusCode $ responseStatus response
  if status /= 200
    then throwIO $ TelegramHttpError status (responseBody response)
    else case eitherDecode (responseBody response) :: Either String SendMessageResponse of
      Left err -> throwIO $ TelegramParseError err
      Right resp
        | smresOk resp -> pure ()
        | otherwise -> throwIO $ TelegramApiError "sendMessage returned ok=false"
