{-# LANGUAGE StrictData #-}

module Elwood.Webhook.Types
  ( -- * Configuration Types
    WebhookConfig (..),
    WebhookServerConfig (..),

    -- * Configuration File Types (for YAML parsing)
    WebhookServerConfigFile (..),
    WebhookConfigFile (..),
    DeliveryTargetFile (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import Elwood.Aeson (rejectUnknownKeys)
import Elwood.Event.Types (DeliveryTarget (..), SessionConfig (..))
import GHC.Generics (Generic)

-- | Configuration for a single webhook endpoint
data WebhookConfig = WebhookConfig
  { -- | Route name: /webhook/<name>
    name :: Text,
    -- | Required secret (header: X-Webhook-Secret)
    secret :: Maybe Text,
    -- | Template with {{.field}} placeholders
    promptTemplate :: Maybe Text,
    -- | Session mode: isolated or named:<id>
    session :: SessionConfig,
    -- | Where to deliver responses
    delivery :: [DeliveryTarget],
    -- | Suppress notification if response contains this string
    suppressIfEquals :: Maybe Text,
    -- | Model override for this endpoint (Nothing = use global)
    model :: Maybe Text,
    -- | Thinking level override for this endpoint (Nothing = use global)
    thinking :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

-- | Configuration for the webhook server
data WebhookServerConfig = WebhookServerConfig
  { -- | Whether webhook server is enabled
    enabled :: Bool,
    -- | Port to listen on (default: 8080)
    port :: Int,
    -- | Global secret (fallback if webhook has no secret)
    globalSecret :: Maybe Text,
    -- | Configured webhook endpoints
    webhooks :: [WebhookConfig]
  }
  deriving stock (Show, Eq, Generic)

-- | YAML file configuration for webhook server
data WebhookServerConfigFile = WebhookServerConfigFile
  { enabled :: Maybe Bool,
    port :: Maybe Int,
    globalSecret :: Maybe Text,
    endpoints :: Maybe [WebhookConfigFile]
  }
  deriving stock (Show, Generic)

-- | YAML file configuration for a webhook endpoint
data WebhookConfigFile = WebhookConfigFile
  { name :: Text,
    secret :: Maybe Text,
    promptTemplate :: Maybe Text,
    session :: Maybe Text,
    deliver :: Maybe [DeliveryTargetFile],
    suppressIfEquals :: Maybe Text,
    model :: Maybe Text,
    thinking :: Maybe Value
  }
  deriving stock (Show, Generic)

-- | YAML file configuration for a delivery target
data DeliveryTargetFile = DeliveryTargetFile
  { type_ :: Text,
    session :: Maybe Text
  }
  deriving stock (Show, Generic)

instance FromJSON DeliveryTargetFile where
  parseJSON = withObject "DeliveryTargetFile" $ \v -> do
    rejectUnknownKeys "DeliveryTargetFile" ["type", "session"] v
    DeliveryTargetFile
      <$> v .: "type"
      <*> v .:? "session"

instance FromJSON WebhookServerConfigFile where
  parseJSON = withObject "WebhookServerConfigFile" $ \v -> do
    rejectUnknownKeys "WebhookServerConfigFile" ["enabled", "port", "globalSecret", "endpoints"] v
    WebhookServerConfigFile
      <$> v .:? "enabled"
      <*> v .:? "port"
      <*> v .:? "globalSecret"
      <*> v .:? "endpoints"

instance FromJSON WebhookConfigFile where
  parseJSON = withObject "WebhookConfigFile" $ \v -> do
    rejectUnknownKeys "WebhookConfigFile" ["name", "secret", "promptTemplate", "session", "deliver", "suppressIfEquals", "model", "thinking"] v
    WebhookConfigFile
      <$> v .: "name"
      <*> v .:? "secret"
      <*> v .:? "promptTemplate"
      <*> v .:? "session"
      <*> v .:? "deliver"
      <*> v .:? "suppressIfEquals"
      <*> v .:? "model"
      <*> v .:? "thinking"
