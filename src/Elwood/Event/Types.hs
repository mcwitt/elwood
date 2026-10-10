module Elwood.Event.Types (EventSource (..), SessionConfig (..), DeliveryTarget (..), ImageData (..), MediaType (..), Base64Data (..), SavedAttachment (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as T

data EventSource
  = WebhookSource Text
  | -- | Telegram message from chat ID
    TelegramSource Int64
  | -- | A due scheduled callback (one-shot self-wakeup) fired by the scheduler
    CallbackSource
  deriving stock (Show, Eq)

data SessionConfig
  = Isolated
  | -- | Named persistent session
    Named Text
  deriving stock (Show, Eq)

newtype MediaType = MediaType {unMediaType :: Text}
  deriving stock (Show, Eq)

newtype Base64Data = Base64Data {unBase64Data :: Text}
  deriving stock (Show, Eq)

data ImageData = ImageData
  { mediaType :: MediaType,
    base64Data :: Base64Data
  }
  deriving stock (Show, Eq)

data SavedAttachment = SavedAttachment
  { path :: FilePath,
    mediaType :: Text,
    sizeBytes :: Int,
    perceivable :: Bool,
    originalName :: Maybe Text
  }
  deriving stock (Show, Eq)

data DeliveryTarget
  = TelegramDelivery (NonEmpty Int64)
  | -- | Broadcast to all allowed chats
    TelegramBroadcast
  | -- | Just log, no notification
    LogOnly
  deriving stock (Show, Eq)

instance ToJSON SessionConfig where
  toJSON Isolated = object ["tag" .= ("isolated" :: Text)]
  toJSON (Named n) = object ["tag" .= ("named" :: Text), "name" .= n]

instance FromJSON SessionConfig where
  parseJSON = withObject "SessionConfig" $ \v ->
    (v .: "tag" :: Parser Text) >>= \case
      "isolated" -> pure Isolated
      "named" -> Named <$> v .: "name"
      other -> fail ("Unknown session tag: " <> T.unpack other)

instance ToJSON DeliveryTarget where
  toJSON (TelegramDelivery chatIds) = object ["tag" .= ("telegram" :: Text), "chat_ids" .= chatIds]
  toJSON TelegramBroadcast = object ["tag" .= ("broadcast" :: Text)]
  toJSON LogOnly = object ["tag" .= ("log_only" :: Text)]

instance FromJSON DeliveryTarget where
  parseJSON = withObject "DeliveryTarget" $ \v ->
    (v .: "tag" :: Parser Text) >>= \case
      "telegram" -> TelegramDelivery <$> v .: "chat_ids"
      "broadcast" -> pure TelegramBroadcast
      "log_only" -> pure LogOnly
      other -> fail ("Unknown delivery target tag: " <> T.unpack other)
