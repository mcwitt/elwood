module Elwood.Telegram.Client
  ( TelegramClient (..),
    TelegramError (..),
    newClient,
    getUpdatesAllowed,
    sendMessage,
    sendMessageHtml,
    sendMessageWithKeyboard,
    answerCallbackQuery,
    editMessageReplyMarkup,
    notifyHtml,
    getFile,
    downloadFile,
    sendPhoto,
    sendDocument,
    sendChatAction,
    splitMessage,
    splitMessageAt,
    splitForTelegram,
    telegramApiLimit,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (Exception, catch, throwIO)
import Control.Monad (unless, void)
import Data.Aeson
import Data.Aeson.Types (Pair, parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Elwood.Exception (catchSync)
import Elwood.Logging (Logger, logWarn)
import Elwood.Telegram.Markdown (markdownToTelegramHtml)
import Elwood.Telegram.Types
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData (formDataBody, partBS, partFileSource)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

data TelegramClient = TelegramClient
  {tcManager :: Manager, tcToken :: Text, tcBaseUrl :: String, tcLogger :: Logger}

data TelegramError = TelegramHttpError Int ByteString | TelegramParseError String | TelegramApiError String
  deriving stock (Show)

instance Exception TelegramError

newClient :: Logger -> Text -> IO TelegramClient
newClient logger token = do
  manager <- newManager tlsManagerSettings
  pure (TelegramClient manager token ("https://api.telegram.org/bot" <> T.unpack token) logger)

-- All Telegram methods use the same envelope; missing required results are errors.
call :: (FromJSON a) => TelegramClient -> String -> [Pair] -> IO a
call client endpoint fields = do
  request <- parseRequest (client.tcBaseUrl <> "/" <> endpoint)
  execute
    client
    request
      { method = "POST",
        requestBody = RequestBodyLBS (encode (object fields)),
        requestHeaders = [("Content-Type", "application/json")],
        responseTimeout = responseTimeoutMicro 35000000
      }

execute :: (FromJSON a) => TelegramClient -> Request -> IO a
execute client request = do
  body <- fetch client request
  value <- either (throwIO . TelegramParseError) pure (eitherDecode body)
  either (throwIO . TelegramApiError) pure $
    parseEither
      ( withObject "Telegram response" $ \o -> do
          ok <- o .: "ok"
          unless ok (fail "Telegram returned ok=false")
          o .: "result"
      )
      value

fetch :: TelegramClient -> Request -> IO ByteString
fetch client request = do
  response <- httpLbs request client.tcManager
  let status = statusCode response.responseStatus
  if status == 200
    then pure response.responseBody
    else throwIO (TelegramHttpError status response.responseBody)

getUpdatesAllowed :: TelegramClient -> Int -> [Text] -> IO [Update]
getUpdatesAllowed client offset allowed =
  call
    client
    "getUpdates"
    ["offset" .= offset, "timeout" .= (30 :: Int), "allowed_updates" .= allowed]

sendMessage :: TelegramClient -> Int64 -> Text -> IO ()
sendMessage client chat =
  mapM_ (\chunk -> sendMessageHtml client chat (markdownToTelegramHtml chunk) chunk)
    . filter (not . T.null)
    . splitForTelegram

sendMessageHtml :: TelegramClient -> Int64 -> Text -> Text -> IO ()
sendMessageHtml client chat html plain = void (sendFormatted client chat [] html plain)

sendFormatted :: TelegramClient -> Int64 -> [Pair] -> Text -> Text -> IO Value
sendFormatted client chat extra html plain =
  send html ["parse_mode" .= ("HTML" :: Text)] `catch` \err ->
    case err of
      TelegramHttpError 400 body
        | any
            (`T.isInfixOf` TE.decodeUtf8Lenient (LBS.toStrict body))
            ["can't parse entities", "message is too long"] ->
            send plain []
      _ -> throwIO err
  where
    send text mode = call client "sendMessage" (["chat_id" .= chat, "text" .= text] <> mode <> extra)

sendMessageWithKeyboard :: TelegramClient -> Int64 -> Text -> InlineKeyboardMarkup -> IO Int
sendMessageWithKeyboard client chat text keyboard = do
  value <- sendFormatted client chat ["reply_markup" .= keyboard] (markdownToTelegramHtml text) text
  either (throwIO . TelegramParseError) pure (parseEither (withObject "sent message" (.: "message_id")) value)

answerCallbackQuery :: TelegramClient -> Text -> Maybe Text -> IO ()
answerCallbackQuery client query text =
  void $
    call @Value
      client
      "answerCallbackQuery"
      (["callback_query_id" .= query] <> maybe [] (\t -> ["text" .= t]) text)

editMessageReplyMarkup :: TelegramClient -> Int64 -> Int -> Maybe InlineKeyboardMarkup -> IO ()
editMessageReplyMarkup client chat message keyboard =
  void $
    call @Value
      client
      "editMessageReplyMarkup"
      ["chat_id" .= chat, "message_id" .= message, "reply_markup" .= fromMaybe (InlineKeyboardMarkup []) keyboard]

notifyHtml :: Logger -> TelegramClient -> Int64 -> Text -> Text -> IO ()
notifyHtml _ = sendMessageHtml

getFile :: TelegramClient -> Text -> IO (Maybe TelegramFile)
getFile client file = call client "getFile" ["file_id" .= file]

downloadFile :: TelegramClient -> Text -> IO ByteString
downloadFile client path = parseRequest ("https://api.telegram.org/file/bot" <> T.unpack client.tcToken <> "/" <> T.unpack path) >>= fetch client

sendPhoto, sendDocument :: TelegramClient -> Int64 -> FilePath -> Maybe Text -> IO ()
sendPhoto = upload "sendPhoto" "photo"
sendDocument = upload "sendDocument" "document"

upload :: String -> Text -> TelegramClient -> Int64 -> FilePath -> Maybe Text -> IO ()
upload endpoint field client chat path caption = do
  request <- parseRequest (client.tcBaseUrl <> "/" <> endpoint)
  body <-
    formDataBody
      ( [partBS "chat_id" (TE.encodeUtf8 (T.pack (show chat))), partFileSource field path]
          <> maybe [] (\c -> [partBS "caption" (TE.encodeUtf8 c)]) caption
      )
      request
  void (execute @Value client body)

sendChatAction :: TelegramClient -> Int64 -> IO ()
sendChatAction client chat =
  catchSync
    (void $ call @Value client "sendChatAction" ["chat_id" .= chat, "action" .= ("typing" :: Text)])
    (\err -> logWarn client.tcLogger "Typing indicator failed" [("error", T.pack (show err))])

telegramApiLimit :: Int
telegramApiLimit = 4096

utf16Length :: Text -> Int
utf16Length = (`div` 2) . BS.length . TE.encodeUtf16LE

splitMessage :: Text -> [Text]
splitMessage = splitMessageAt 4000

-- Prefer paragraph boundaries without discarding any source text.
splitMessageAt :: Int -> Text -> [Text]
splitMessageAt limit text
  | limit <= 0 = [text]
  | utf16Length text <= limit = [text]
  | otherwise = chunk : splitMessageAt limit rest
  where
    candidate = T.pack $ takeUnits limit (T.unpack text)
    takeUnits _ [] = []
    takeUnits n (c : cs)
      | n < cost = []
      | otherwise = c : takeUnits (n - cost) cs
      where
        cost = if fromEnum c > 65535 then 2 else 1
    boundary sep =
      let (before, _) = T.breakOnEnd sep candidate
       in if T.length before > T.length candidate `div` 4 then Just before else Nothing
    chunk = fromMaybe (if T.null candidate then T.take 1 text else candidate) (boundary "\n\n" <|> boundary "\n")
    rest = T.drop (T.length chunk) text

splitForTelegram :: Text -> [Text]
splitForTelegram = concatMap fit . splitMessage
  where
    fit chunk
      | utf16Length (markdownToTelegramHtml chunk) <= telegramApiLimit = [chunk]
      | otherwise = concatMap fit (splitMessageAt (max 2 (utf16Length chunk `div` 2)) chunk)
