{-# LANGUAGE StrictData #-}

module Elwood.Telegram.Types
  ( Update (..)
  , Message (..)
  , Chat (..)
  , User (..)
  , SendMessageRequest (..)
  , GetUpdatesResponse (..)
  , SendMessageResponse (..)
  ) where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Incoming update from Telegram's getUpdates API
data Update = Update
  { updateId :: Int
  -- ^ Unique update identifier
  , message :: Maybe Message
  -- ^ New incoming message (if any)
  }
  deriving stock (Show, Generic)

instance FromJSON Update where
  parseJSON = withObject "Update" $ \v ->
    Update
      <$> v .: "update_id"
      <*> v .:? "message"

-- | A Telegram message
data Message = Message
  { messageId :: Int
  -- ^ Unique message identifier
  , chat :: Chat
  -- ^ Chat the message belongs to
  , text :: Maybe Text
  -- ^ Text content of the message
  , from :: Maybe User
  -- ^ Sender of the message
  }
  deriving stock (Show, Generic)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v ->
    Message
      <$> v .: "message_id"
      <*> v .: "chat"
      <*> v .:? "text"
      <*> v .:? "from"

-- | A Telegram chat
data Chat = Chat
  { chatId :: Int64
  -- ^ Unique chat identifier
  , chatType :: Text
  -- ^ Type of chat: "private", "group", "supergroup", or "channel"
  }
  deriving stock (Show, Generic)

instance FromJSON Chat where
  parseJSON = withObject "Chat" $ \v ->
    Chat
      <$> v .: "id"
      <*> v .: "type"

-- | A Telegram user
data User = User
  { userId :: Int64
  -- ^ Unique user identifier
  , firstName :: Text
  -- ^ User's first name
  , lastName :: Maybe Text
  -- ^ User's last name (optional)
  , username :: Maybe Text
  -- ^ User's username (optional)
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
  { smrChatId :: Int64
  , smrText :: Text
  , smrParseMode :: Maybe Text
  }
  deriving stock (Show, Generic)

instance ToJSON SendMessageRequest where
  toJSON req =
    object $
      [ "chat_id" .= smrChatId req
      , "text" .= smrText req
      ]
        ++ maybe [] (\pm -> ["parse_mode" .= pm]) (smrParseMode req)

-- | Response wrapper for Telegram API
data GetUpdatesResponse = GetUpdatesResponse
  { gurOk :: Bool
  , gurResult :: [Update]
  }
  deriving stock (Show, Generic)

instance FromJSON GetUpdatesResponse where
  parseJSON = withObject "GetUpdatesResponse" $ \v ->
    GetUpdatesResponse
      <$> v .: "ok"
      <*> v .: "result"

-- | Response for sendMessage API
data SendMessageResponse = SendMessageResponse
  { smresOk :: Bool
  , smresResult :: Maybe Message
  }
  deriving stock (Show, Generic)

instance FromJSON SendMessageResponse where
  parseJSON = withObject "SendMessageResponse" $ \v ->
    SendMessageResponse
      <$> v .: "ok"
      <*> v .:? "result"
