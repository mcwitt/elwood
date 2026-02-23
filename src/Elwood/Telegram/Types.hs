{-# LANGUAGE StrictData #-}

module Elwood.Telegram.Types
  ( Update (..),
    Message (..),
    Chat (..),
    User (..),
    PhotoSize (..),
    TelegramFile (..),
    SendMessageRequest (..),
    GetUpdatesResponse (..),
    SendMessageResponse (..),
    GetFileResponse (..),

    -- * Callback Query Types
    CallbackQuery (..),

    -- * Inline Keyboard Types
    InlineKeyboardButton (..),
    InlineKeyboardMarkup (..),
    SendMessageWithKeyboardRequest (..),
    AnswerCallbackQueryRequest (..),
    EditMessageReplyMarkupRequest (..),
  )
where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Incoming update from Telegram's getUpdates API
data Update = Update
  { -- | Unique update identifier
    id_ :: Int,
    -- | New incoming message (if any)
    message :: Maybe Message,
    -- | Callback query from inline keyboard button press
    callbackQuery :: Maybe CallbackQuery
  }
  deriving stock (Show, Generic)

instance FromJSON Update where
  parseJSON = withObject "Update" $ \v ->
    Update
      <$> v .: "update_id"
      <*> v .:? "message"
      <*> v .:? "callback_query"

-- | A photo size variant (Telegram sends multiple sizes)
data PhotoSize = PhotoSize
  { -- | Identifier for this file
    fileId :: Text,
    -- | Unique file identifier (consistent across bots)
    fileUniqueId :: Text,
    -- | Photo width
    width :: Int,
    -- | Photo height
    height :: Int,
    -- | File size in bytes (optional)
    fileSize :: Maybe Int
  }
  deriving stock (Show, Generic)

instance FromJSON PhotoSize where
  parseJSON = withObject "PhotoSize" $ \v ->
    PhotoSize
      <$> v .: "file_id"
      <*> v .: "file_unique_id"
      <*> v .: "width"
      <*> v .: "height"
      <*> v .:? "file_size"

-- | A Telegram message
data Message = Message
  { -- | Unique message identifier
    id_ :: Int,
    -- | Chat the message belongs to
    chat :: Chat,
    -- | Text content of the message
    text :: Maybe Text,
    -- | Sender of the message
    from_ :: Maybe User,
    -- | Photos attached to the message (multiple sizes)
    photo :: Maybe [PhotoSize],
    -- | Caption for media messages
    caption :: Maybe Text
  }
  deriving stock (Show, Generic)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v ->
    Message
      <$> v .: "message_id"
      <*> v .: "chat"
      <*> v .:? "text"
      <*> v .:? "from"
      <*> v .:? "photo"
      <*> v .:? "caption"

-- | A Telegram chat
data Chat = Chat
  { -- | Unique chat identifier
    id_ :: Int64,
    -- | Type of chat: "private", "group", "supergroup", or "channel"
    type_ :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON Chat where
  parseJSON = withObject "Chat" $ \v ->
    Chat
      <$> v .: "id"
      <*> v .: "type"

-- | A Telegram user
data User = User
  { -- | Unique user identifier
    id_ :: Int64,
    -- | User's first name
    firstName :: Text,
    -- | User's last name (optional)
    lastName :: Maybe Text,
    -- | User's username (optional)
    username :: Maybe Text
  }
  deriving stock (Show, Generic)

instance FromJSON User where
  parseJSON = withObject "User" $ \v ->
    User
      <$> v .: "id"
      <*> v .: "first_name"
      <*> v .:? "last_name"
      <*> v .:? "username"

-- | Request body for sendMessage API
data SendMessageRequest = SendMessageRequest
  { chatId :: Int64,
    text :: Text,
    parseMode :: Maybe Text
  }
  deriving stock (Show, Generic)

instance ToJSON SendMessageRequest where
  toJSON req =
    object $
      [ "chat_id" .= req.chatId,
        "text" .= req.text
      ]
        ++ maybe [] (\pm -> ["parse_mode" .= pm]) req.parseMode

-- | Response wrapper for Telegram API
data GetUpdatesResponse = GetUpdatesResponse
  { ok :: Bool,
    result :: [Update]
  }
  deriving stock (Show, Generic)

instance FromJSON GetUpdatesResponse where
  parseJSON = withObject "GetUpdatesResponse" $ \v ->
    GetUpdatesResponse
      <$> v .: "ok"
      <*> v .: "result"

-- | Response for sendMessage API
data SendMessageResponse = SendMessageResponse
  { ok :: Bool,
    result :: Maybe Message
  }
  deriving stock (Show, Generic)

instance FromJSON SendMessageResponse where
  parseJSON = withObject "SendMessageResponse" $ \v ->
    SendMessageResponse
      <$> v .: "ok"
      <*> v .:? "result"

-- | File information from Telegram getFile API
data TelegramFile = TelegramFile
  { -- | Identifier for this file
    fileId :: Text,
    -- | Unique file identifier
    fileUniqueId :: Text,
    -- | File size in bytes (optional)
    fileSize :: Maybe Int,
    -- | File path for download (optional, use with file download URL)
    filePath :: Maybe Text
  }
  deriving stock (Show, Generic)

instance FromJSON TelegramFile where
  parseJSON = withObject "TelegramFile" $ \v ->
    TelegramFile
      <$> v .: "file_id"
      <*> v .: "file_unique_id"
      <*> v .:? "file_size"
      <*> v .:? "file_path"

-- | Response for getFile API
data GetFileResponse = GetFileResponse
  { ok :: Bool,
    result :: Maybe TelegramFile
  }
  deriving stock (Show, Generic)

instance FromJSON GetFileResponse where
  parseJSON = withObject "GetFileResponse" $ \v ->
    GetFileResponse
      <$> v .: "ok"
      <*> v .:? "result"

-- | Callback query from pressing an inline keyboard button
data CallbackQuery = CallbackQuery
  { -- | Unique callback query identifier
    id_ :: Text,
    -- | User who pressed the button
    from_ :: User,
    -- | Message with the callback button (if available)
    message :: Maybe Message,
    -- | Data associated with the callback button
    data_ :: Maybe Text
  }
  deriving stock (Show, Generic)

instance FromJSON CallbackQuery where
  parseJSON = withObject "CallbackQuery" $ \v ->
    CallbackQuery
      <$> v .: "id"
      <*> v .: "from"
      <*> v .:? "message"
      <*> v .:? "data"

-- | Inline keyboard button
data InlineKeyboardButton = InlineKeyboardButton
  { -- | Button text
    text :: Text,
    -- | Data to be sent in callback query
    callbackData :: Text
  }
  deriving stock (Show, Generic)

instance ToJSON InlineKeyboardButton where
  toJSON btn =
    object
      [ "text" .= btn.text,
        "callback_data" .= btn.callbackData
      ]

-- | Inline keyboard markup (array of button rows)
newtype InlineKeyboardMarkup = InlineKeyboardMarkup
  { inlineKeyboard :: [[InlineKeyboardButton]]
  }
  deriving stock (Show, Generic)

instance ToJSON InlineKeyboardMarkup where
  toJSON markup =
    object ["inline_keyboard" .= markup.inlineKeyboard]

-- | Request body for sendMessage with inline keyboard
data SendMessageWithKeyboardRequest = SendMessageWithKeyboardRequest
  { chatId :: Int64,
    text :: Text,
    parseMode :: Maybe Text,
    replyMarkup :: InlineKeyboardMarkup
  }
  deriving stock (Show, Generic)

instance ToJSON SendMessageWithKeyboardRequest where
  toJSON req =
    object $
      [ "chat_id" .= req.chatId,
        "text" .= req.text,
        "reply_markup" .= req.replyMarkup
      ]
        ++ maybe [] (\pm -> ["parse_mode" .= pm]) req.parseMode

-- | Request body for answerCallbackQuery
data AnswerCallbackQueryRequest = AnswerCallbackQueryRequest
  { callbackQueryId :: Text,
    text :: Maybe Text,
    showAlert :: Bool
  }
  deriving stock (Show, Generic)

instance ToJSON AnswerCallbackQueryRequest where
  toJSON req =
    object $
      ["callback_query_id" .= req.callbackQueryId]
        ++ maybe [] (\t -> ["text" .= t]) req.text
        ++ ["show_alert" .= req.showAlert | req.showAlert]

-- | Request body for editMessageReplyMarkup
data EditMessageReplyMarkupRequest = EditMessageReplyMarkupRequest
  { chatId :: Int64,
    messageId :: Int,
    replyMarkup :: Maybe InlineKeyboardMarkup
  }
  deriving stock (Show, Generic)

instance ToJSON EditMessageReplyMarkupRequest where
  toJSON req =
    object $
      [ "chat_id" .= req.chatId,
        "message_id" .= req.messageId
      ]
        ++ maybe [] (\rm -> ["reply_markup" .= rm]) req.replyMarkup
