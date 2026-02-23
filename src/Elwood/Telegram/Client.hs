{-# LANGUAGE StrictData #-}

module Elwood.Telegram.Client
  ( TelegramClient (..),
    TelegramError (..),
    newClient,
    getUpdatesAllowed,
    sendMessage,
    sendMessageWithKeyboard,
    answerCallbackQuery,
    editMessageReplyMarkup,
    notify,
    getFile,
    downloadFile,
    sendPhoto,
    sendDocument,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.Aeson (eitherDecode, encode, object, (.=))
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Elwood.Logging (Logger, logInfo)
import Elwood.Telegram.Types
  ( AnswerCallbackQueryRequest (..),
    EditMessageReplyMarkupRequest (..),
    GetFileResponse (..),
    GetUpdatesResponse (..),
    InlineKeyboardMarkup,
    Message (..),
    SendMessageRequest (..),
    SendMessageResponse (..),
    SendMessageWithKeyboardRequest (..),
    TelegramFile (..),
    Update,
  )
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData (formDataBody, partBS, partFileSource)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

-- | Telegram API client
data TelegramClient = TelegramClient
  { -- | HTTP connection manager
    tcManager :: Manager,
    -- | Bot token
    tcToken :: Text,
    -- | Base URL for API calls
    tcBaseUrl :: String
  }

-- | Errors from Telegram API calls
data TelegramError
  = TelegramHttpError Int ByteString
  | TelegramParseError String
  | TelegramApiError String
  deriving stock (Show)

instance Exception TelegramError

-- | Create a new Telegram client
newClient :: Text -> IO TelegramClient
newClient token = do
  mgr <- newManager tlsManagerSettings
  pure
    TelegramClient
      { tcManager = mgr,
        tcToken = token,
        tcBaseUrl = "https://api.telegram.org/bot" <> T.unpack token
      }

-- | Build a request for a Telegram API method
buildRequest :: TelegramClient -> String -> IO Request
buildRequest client method_ = do
  req <- parseRequest $ client.tcBaseUrl <> "/" <> method_
  pure
    req
      { requestHeaders =
          [ ("Content-Type", "application/json")
          ]
      }

-- | Get updates with custom allowed update types
--
-- Parameters:
--   - client: The Telegram client
--   - offset: Update offset (use last update_id + 1)
--   - allowedUpdates: List of update types to receive (e.g., ["message", "callback_query"])
--
-- Returns a list of updates
getUpdatesAllowed :: TelegramClient -> Int -> [Text] -> IO [Update]
getUpdatesAllowed client offset allowedUpdates = do
  req <- buildRequest client "getUpdates"
  let body =
        encode $
          object
            [ "offset" .= offset,
              "timeout" .= (30 :: Int),
              "allowed_updates" .= allowedUpdates
            ]
      req' =
        req
          { method = "POST",
            requestBody = RequestBodyLBS body,
            -- Set timeout longer than Telegram's 30s long poll
            responseTimeout = responseTimeoutMicro (35 * 1000000)
          }

  response <- httpLbs req' client.tcManager

  let status = statusCode $ responseStatus response
  if status /= 200
    then throwIO $ TelegramHttpError status (responseBody response)
    else case eitherDecode (responseBody response) :: Either String GetUpdatesResponse of
      Left err -> throwIO $ TelegramParseError err
      Right resp
        | resp.ok -> pure resp.result
        | otherwise -> throwIO $ TelegramApiError "API returned ok=false"

-- | Send a message to a chat
sendMessage :: TelegramClient -> Int64 -> Text -> IO ()
sendMessage client chatId_ msgText = do
  req <- buildRequest client "sendMessage"
  let body =
        encode
          SendMessageRequest
            { chatId = chatId_,
              text = msgText,
              parseMode = Just "Markdown"
            }
      req' =
        req
          { method = "POST",
            requestBody = RequestBodyLBS body
          }

  response <- httpLbs req' client.tcManager

  let status = statusCode $ responseStatus response
  if status /= 200
    then throwIO $ TelegramHttpError status (responseBody response)
    else case eitherDecode (responseBody response) :: Either String SendMessageResponse of
      Left err -> throwIO $ TelegramParseError err
      Right resp
        | resp.ok -> pure ()
        | otherwise -> throwIO $ TelegramApiError "sendMessage returned ok=false"

-- | Send a message with an inline keyboard
sendMessageWithKeyboard :: TelegramClient -> Int64 -> Text -> InlineKeyboardMarkup -> IO Int
sendMessageWithKeyboard client chatIdVal msgText keyboard = do
  req <- buildRequest client "sendMessage"
  let body =
        encode
          SendMessageWithKeyboardRequest
            { chatId = chatIdVal,
              text = msgText,
              parseMode = Just "Markdown",
              replyMarkup = keyboard
            }
      req' =
        req
          { method = "POST",
            requestBody = RequestBodyLBS body
          }

  response <- httpLbs req' client.tcManager

  let status = statusCode $ responseStatus response
  if status /= 200
    then throwIO $ TelegramHttpError status (responseBody response)
    else case eitherDecode (responseBody response) :: Either String SendMessageResponse of
      Left err -> throwIO $ TelegramParseError err
      Right resp
        | resp.ok -> pure $ maybe 0 (.id_) resp.result
        | otherwise -> throwIO $ TelegramApiError "sendMessage with keyboard returned ok=false"

-- | Answer a callback query (acknowledge button press)
answerCallbackQuery :: TelegramClient -> Text -> Maybe Text -> IO ()
answerCallbackQuery client callbackQueryId_ responseText = do
  req <- buildRequest client "answerCallbackQuery"
  let body =
        encode
          AnswerCallbackQueryRequest
            { callbackQueryId = callbackQueryId_,
              text = responseText,
              showAlert = False
            }
      req' =
        req
          { method = "POST",
            requestBody = RequestBodyLBS body
          }

  response <- httpLbs req' client.tcManager

  let status = statusCode $ responseStatus response
  when (status /= 200) $
    throwIO $
      TelegramHttpError status (responseBody response)

-- | Edit the reply markup of a message (to remove buttons after response)
editMessageReplyMarkup :: TelegramClient -> Int64 -> Int -> Maybe InlineKeyboardMarkup -> IO ()
editMessageReplyMarkup client chatIdVal msgId newMarkup = do
  req <- buildRequest client "editMessageReplyMarkup"
  let body =
        encode
          EditMessageReplyMarkupRequest
            { chatId = chatIdVal,
              messageId = msgId,
              replyMarkup = newMarkup
            }
      req' =
        req
          { method = "POST",
            requestBody = RequestBodyLBS body
          }

  response <- httpLbs req' client.tcManager

  let status = statusCode $ responseStatus response
  when (status /= 200) $
    throwIO $
      TelegramHttpError status (responseBody response)

-- | Send a proactive notification with logging
notify :: Logger -> TelegramClient -> Int64 -> Text -> IO ()
notify logger client chatIdVal msgText = do
  logInfo logger "Sending notification" [("chat_id", T.pack (show chatIdVal))]
  sendMessage client chatIdVal msgText

-- | Get file information for downloading
getFile :: TelegramClient -> Text -> IO (Maybe TelegramFile)
getFile client fileId_ = do
  req <- buildRequest client "getFile"
  let body = encode $ object ["file_id" .= fileId_]
      req' =
        req
          { method = "POST",
            requestBody = RequestBodyLBS body
          }

  response <- httpLbs req' client.tcManager

  let status = statusCode $ responseStatus response
  if status /= 200
    then throwIO $ TelegramHttpError status (responseBody response)
    else case eitherDecode (responseBody response) :: Either String GetFileResponse of
      Left err -> throwIO $ TelegramParseError err
      Right resp
        | resp.ok -> pure resp.result
        | otherwise -> pure Nothing

-- | Download file content as raw bytes
downloadFile :: TelegramClient -> Text -> IO ByteString
downloadFile client fp = do
  let url = "https://api.telegram.org/file/bot" <> T.unpack client.tcToken <> "/" <> T.unpack fp
  req <- parseRequest url
  response <- httpLbs req client.tcManager

  let status = statusCode $ responseStatus response
  if status /= 200
    then throwIO $ TelegramHttpError status (responseBody response)
    else pure (responseBody response)

-- | Send a photo to a chat via multipart upload
sendPhoto :: TelegramClient -> Int64 -> FilePath -> Maybe Text -> IO ()
sendPhoto client chatIdVal path mCaption = do
  req <- parseRequest $ client.tcBaseUrl <> "/sendPhoto"
  let parts =
        [ partBS "chat_id" (TE.encodeUtf8 $ T.pack $ show chatIdVal),
          partFileSource "photo" path
        ]
          <> maybe [] (\c -> [partBS "caption" (TE.encodeUtf8 c)]) mCaption
  req' <- formDataBody parts req
  response <- httpLbs req' client.tcManager
  let status = statusCode $ responseStatus response
  when (status /= 200) $
    throwIO $
      TelegramHttpError status (responseBody response)

-- | Send a document to a chat via multipart upload
sendDocument :: TelegramClient -> Int64 -> FilePath -> Maybe Text -> IO ()
sendDocument client chatIdVal path mCaption = do
  req <- parseRequest $ client.tcBaseUrl <> "/sendDocument"
  let parts =
        [ partBS "chat_id" (TE.encodeUtf8 $ T.pack $ show chatIdVal),
          partFileSource "document" path
        ]
          <> maybe [] (\c -> [partBS "caption" (TE.encodeUtf8 c)]) mCaption
  req' <- formDataBody parts req
  response <- httpLbs req' client.tcManager
  let status = statusCode $ responseStatus response
  when (status /= 200) $
    throwIO $
      TelegramHttpError status (responseBody response)
