module Elwood.Webhook.Types (WebhookConfig (..), WebhookServerConfig (..)) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Text qualified as Text
import Elwood.Aeson (rejectUnknownKeys)
import Elwood.AgentSettings (AgentOverrides)
import Elwood.Event.Types (DeliveryTarget (..), SessionConfig (..))
import Elwood.Prompt (PromptInput)

data WebhookConfig = WebhookConfig
  { name :: Text,
    prompt :: [PromptInput],
    session :: SessionConfig,
    deliveryTarget :: DeliveryTarget,
    suppressIfContains :: Maybe Text,
    overrides :: AgentOverrides
  }
  deriving stock (Show, Eq)

data WebhookServerConfig = WebhookServerConfig
  {enabled :: Bool, port :: Int, secret :: Maybe Text, webhooks :: [WebhookConfig]}
  deriving stock (Show, Eq)

instance FromJSON WebhookServerConfig where
  parseJSON = withObject "webhook" $ \v -> do
    rejectUnknownKeys "webhook" ["enabled", "port", "secret", "endpoints"] v
    WebhookServerConfig
      <$> v .:? "enabled" .!= False
      <*> v .:? "port" .!= 8080
      <*> v .:? "secret"
      <*> v .:? "endpoints" .!= []

instance FromJSON WebhookConfig where
  parseJSON = withObject "webhook endpoint" $ \v -> do
    rejectUnknownKeys "webhook endpoint" ["name", "prompt", "session", "delivery_target", "suppress_if_contains", "agent"] v
    WebhookConfig
      <$> v .: "name"
      <*> v .:? "prompt" .!= []
      <*> (maybe Isolated Named <$> v .:? "session")
      <*> (v .:? "delivery_target" >>= maybe (pure TelegramBroadcast) delivery)
      <*> v .:? "suppress_if_contains"
      <*> v .:? "agent" .!= mempty
    where
      delivery = withObject "delivery target" $ \v -> do
        kind <- Text.toLower <$> v .: "type" :: Parser Text
        rejectUnknownKeys "delivery target" (["type"] <> ["chat_ids" | kind == "telegram"]) v
        case kind of
          "telegram" -> TelegramDelivery <$> v .: "chat_ids"
          "telegram_broadcast" -> pure TelegramBroadcast
          "log_only" -> pure LogOnly
          _ -> fail ("Unknown delivery target type: " <> Text.unpack kind)
