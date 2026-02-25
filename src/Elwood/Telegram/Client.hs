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
    sendChatAction,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.Aeson (eitherDecode, encode, object, (.=))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Elwood.Logging (Logger, logInfo, logWarn)
import Elwood.Telegram.Markdown (markdownToTelegramHtml)
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
  ( Manager,
    Request,
    RequestBody (RequestBodyLBS),
    httpLbs,
    method,
    newManager,
    parseRequest,
    requestBody,
    requestHeaders,
    responseBody,
    responseStatus,
    responseTimeout,
    responseTimeoutMicro,
  )
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
    tcBaseUrl :: String,
    -- | Logger for warnings/diagnostics
    tcLogger :: Logger
  }

-- | Errors from Telegram API calls
data TelegramError
  = TelegramHttpError Int ByteString
  | TelegramParseError String
  | TelegramApiError String
  deriving stock (Show)

instance Exception TelegramError

-- | Create a new Telegram client
newClient :: Logger -> Text -> IO TelegramClient
newClient logger token = do
  mgr <- newManager tlsManagerSettings
  pure
    TelegramClient
      { tcManager = mgr,
        tcToken = token,
        tcBaseUrl = "https://api.telegram.org/bot" <> T.unpack token,
        tcLogger = logger
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
--
-- Converts the message text from markdown to Telegram HTML, then sends with
-- HTML parse mode. If Telegram rejects the HTML, falls back to sending the
-- original markdown as plain text so the message is never silently lost.
sendMessage :: TelegramClient -> Int64 -> Text -> IO ()
sendMessage client chatId_ msgText = do
  let htmlText = markdownToTelegramHtml msgText
      msgReq =
        SendMessageRequest
          { chatId = chatId_,
            text = htmlText,
            parseMode = Just "HTML"
          }
  sendMessageRaw client msgReq >>= \case
    Right () -> pure ()
    Left (status, body)
      | status == 400,
        isParseEntityError body -> do
          logWarn client.tcLogger "HTML parse failed, falling back to plain text" []
          -- Retry with original markdown as plain text (not the HTML)
          let plainReq =
                SendMessageRequest
                  { chatId = msgReq.chatId,
                    text = msgText,
                    parseMode = Nothing
                  }
           in sendMessageRaw client plainReq >>= \case
                Right () -> pure ()
                Left (s, b) -> throwIO $ TelegramHttpError s b
      | otherwise -> throwIO $ TelegramHttpError status body

-- | Low-level send: returns Right () on success, Left (status, body) on HTTP error
sendMessageRaw :: TelegramClient -> SendMessageRequest -> IO (Either (Int, ByteString) ())
sendMessageRaw client msgReq = do
  req <- buildRequest client "sendMessage"
  let req' =
        req
          { method = "POST",
            requestBody = RequestBodyLBS (encode msgReq)
          }
  response <- httpLbs req' client.tcManager
  let status = statusCode $ responseStatus response
  if status /= 200
    then pure $ Left (status, responseBody response)
    else case eitherDecode (responseBody response) :: Either String SendMessageResponse of
      Left err -> throwIO $ TelegramParseError err
      Right resp
        | resp.ok -> pure $ Right ()
        | otherwise -> throwIO $ TelegramApiError "sendMessage returned ok=false"

-- | Check if a Telegram error response indicates a markdown entity parsing failure
isParseEntityError :: ByteString -> Bool
isParseEntityError body =
  "can't parse entities" `T.isInfixOf` TE.decodeUtf8Lenient (LBS.toStrict body)

-- | Send a message with an inline keyboard
--
-- Converts markdown to Telegram HTML with plain-text fallback (same as sendMessage).
sendMessageWithKeyboard :: TelegramClient -> Int64 -> Text -> InlineKeyboardMarkup -> IO Int
sendMessageWithKeyboard client chatIdVal msgText keyboard = do
  let htmlText = markdownToTelegramHtml msgText
      send txt pm = do
        req <- buildRequest client "sendMessage"
        let body =
              encode
                SendMessageWithKeyboardRequest
                  { chatId = chatIdVal,
                    text = txt,
                    parseMode = pm,
                    replyMarkup = keyboard
                  }
            req' =
              req
                { method = "POST",
                  requestBody = RequestBodyLBS body
                }
        response <- httpLbs req' client.tcManager
        let status = statusCode $ responseStatus response
        pure (status, responseBody response)

  (status, body) <- send htmlText (Just "HTML")
  (finalStatus, finalBody) <-
    if status == 400 && isParseEntityError body
      then do
        logWarn client.tcLogger "HTML parse failed for keyboard message, falling back to plain text" []
        send msgText Nothing
      else pure (status, body)

  if finalStatus /= 200
    then throwIO $ TelegramHttpError finalStatus finalBody
    else case eitherDecode finalBody :: Either String SendMessageResponse of
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

-- | Send a chat action (e.g., "typing" indicator). Best-effort: logs a
-- warning on failure instead of throwing.
sendChatAction :: TelegramClient -> Int64 -> IO ()
sendChatAction client chatIdVal = do
  req <- buildRequest client "sendChatAction"
  let body =
        encode $
          object
            [ "chat_id" .= chatIdVal,
              "action" .= ("typing" :: Text)
            ]
      req' =
        req
          { method = "POST",
            requestBody = RequestBodyLBS body
          }
  response <- httpLbs req' client.tcManager
  let status = statusCode $ responseStatus response
  when (status /= 200) $
    logWarn client.tcLogger "sendChatAction failed" [("status", T.pack (show status))]
