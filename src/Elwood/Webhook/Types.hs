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
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Aeson (rejectUnknownKeys)
import Elwood.Event.Types (DeliveryTarget (..), SessionConfig (..))
import Elwood.Prompt (PromptInput (..), PromptInputFile (..))
import GHC.Generics (Generic)

-- | Configuration for a single webhook endpoint
data WebhookConfig = WebhookConfig
  { -- | Route name: /webhook/<name>
    name :: Text,
    -- | Required secret (header: X-Webhook-Secret)
    secret :: Maybe Text,
    -- | Prompt inputs (assembled at request time)
    prompt :: [PromptInput],
    -- | Session mode: isolated or named:<id>
    session :: SessionConfig,
    -- | Where to deliver responses
    delivery :: [DeliveryTarget],
    -- | Suppress notification if response contains this string
    suppressIfContains :: Maybe Text,
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
    prompt :: Maybe [PromptInputFile],
    session :: Maybe Text,
    deliver :: Maybe [DeliveryTargetFile],
    suppressIfContains :: Maybe Text,
    model :: Maybe Text,
    thinking :: Maybe Value
  }
  deriving stock (Show, Generic)

-- | YAML file configuration for a delivery target
data DeliveryTargetFile
  = DeliveryTargetFileTelegram (Maybe Text)
  | DeliveryTargetFileLog
  deriving stock (Show, Generic)

instance FromJSON DeliveryTargetFile where
  parseJSON = withObject "DeliveryTargetFile" $ \v -> do
    rejectUnknownKeys "DeliveryTargetFile" ["type", "session"] v
    t <- v .: "type" :: Parser Text
    case T.toLower t of
      "telegram" -> DeliveryTargetFileTelegram <$> v .:? "session"
      "log" -> pure DeliveryTargetFileLog
      other -> fail $ "Unknown delivery target type: " <> T.unpack other

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
    rejectUnknownKeys "WebhookConfigFile" ["name", "secret", "prompt", "session", "deliver", "suppressIfContains", "model", "thinking"] v
    WebhookConfigFile
      <$> v .: "name"
      <*> v .:? "secret"
      <*> v .:? "prompt"
      <*> v .:? "session"
      <*> v .:? "deliver"
      <*> v .:? "suppressIfContains"
      <*> v .:? "model"
      <*> v .:? "thinking"
