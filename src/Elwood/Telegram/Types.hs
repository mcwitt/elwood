module Elwood.Telegram.Types
  ( Update (..),
    UpdatePayload (..),
    Message (..),
    Chat (..),
    ChatType (..),
    User (..),
    TelegramFile (..),
    CallbackQuery (..),
    InlineKeyboardButton (..),
    InlineKeyboardMarkup (..),
    AttachmentKind (..),
    InboundAttachment (..),
  )
where

import Control.Monad (void)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.Ord (Down (..))
import Data.Text (Text)
import Elwood.Aeson (enum, recordOptions)
import GHC.Generics (Generic)

data UpdatePayload = UpdateMessage Message | UpdateCallbackQuery CallbackQuery | UpdateIgnored deriving stock (Show)

data Update = Update {id_ :: Int, payload :: UpdatePayload} deriving stock (Show)

instance FromJSON Update where
  parseJSON = withObject "Update" $ \o -> Update <$> o .: "update_id" <*> payload o
    where
      payload o
        | KM.member "message" o = UpdateMessage <$> o .: "message"
        | KM.member "callback_query" o = UpdateCallbackQuery <$> o .: "callback_query"
        | otherwise = pure UpdateIgnored

data AttachmentKind = KindPhoto | KindDocument | KindVoice | KindAudio deriving stock (Show, Eq)

data InboundAttachment = InboundAttachment
  {fileId, fileUniqueId :: Text, kind :: AttachmentKind, fileName, mimeType :: Maybe Text, fileSize :: Maybe Int}
  deriving stock (Show, Eq)

-- Parse media once into the archive's domain. Required protocol fields remain
-- checked even when they carry no information needed by subsequent stages.
parseAttachment :: AttachmentKind -> Value -> Parser InboundAttachment
parseAttachment kind = withObject "attachment" $ \o -> do
  case kind of
    KindPhoto -> void ((,) <$> (o .: "width" :: Parser Int) <*> (o .: "height" :: Parser Int))
    KindVoice -> void (o .: "duration" :: Parser Int)
    KindAudio -> void (o .: "duration" :: Parser Int)
    KindDocument -> pure ()
  InboundAttachment <$> o .: "file_id" <*> o .: "file_unique_id" <*> pure kind <*> o .:? "file_name" <*> o .:? "mime_type" <*> o .:? "file_size"

data Message = Message
  {id_ :: Int, chat :: Chat, text :: Maybe Text, from_ :: Maybe User, caption :: Maybe Text, attachments :: [InboundAttachment]}
  deriving stock (Show)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o -> do
    photos <- o .:? "photo" .!= [] >>= traverse (parseAttachment KindPhoto)
    other <-
      traverse
        (\(key, kind) -> o .:? key >>= traverse (parseAttachment kind))
        [("document", KindDocument), ("voice", KindVoice), ("audio", KindAudio)]
    let attachments = take 1 (sortOn (Down . (.fileSize)) photos) <> catMaybes other
    Message <$> o .: "message_id" <*> o .: "chat" <*> o .:? "text" <*> o .:? "from" <*> o .:? "caption" <*> pure attachments

data ChatType = Private | Group | Supergroup | Channel deriving stock (Show, Eq)

instance FromJSON ChatType where
  parseJSON = enum "ChatType" [("private", Private), ("group", Group), ("supergroup", Supergroup), ("channel", Channel)]

data Chat = Chat {id_ :: Int64, type_ :: ChatType} deriving stock (Show, Generic)

instance FromJSON Chat where parseJSON = genericParseJSON telegramFields

data User = User {id_ :: Int64, firstName :: Text, lastName, username :: Maybe Text} deriving stock (Show, Generic)

instance FromJSON User where parseJSON = genericParseJSON telegramFields

data TelegramFile = TelegramFile {fileId, fileUniqueId :: Text, fileSize :: Maybe Int, filePath :: Maybe Text} deriving stock (Show, Generic)

instance FromJSON TelegramFile where parseJSON = genericParseJSON telegramFields

data CallbackQuery = CallbackQuery {id_ :: Text, from_ :: User, message :: Maybe Message, data_ :: Maybe Text} deriving stock (Show, Generic)

instance FromJSON CallbackQuery where parseJSON = genericParseJSON telegramFields

data InlineKeyboardButton = InlineKeyboardButton {text, callbackData :: Text} deriving stock (Show, Generic)

instance ToJSON InlineKeyboardButton where toJSON = genericToJSON telegramFields

newtype InlineKeyboardMarkup = InlineKeyboardMarkup {inlineKeyboard :: [[InlineKeyboardButton]]} deriving stock (Show, Generic)

instance ToJSON InlineKeyboardMarkup where toJSON = genericToJSON telegramFields

telegramFields :: Options
telegramFields = recordOptions {rejectUnknownFields = False}
