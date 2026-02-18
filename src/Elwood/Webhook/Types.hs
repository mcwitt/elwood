{-# LANGUAGE StrictData #-}

module Elwood.Webhook.Types
  ( -- * Configuration Types
    WebhookConfig (..),
    WebhookServerConfig (..),

    -- * Configuration File Types (for YAML parsing)
    WebhookServerConfigFile (..),
    WebhookConfigFile (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import Elwood.Event.Types (DeliveryTarget (..), SessionConfig (..))
import GHC.Generics (Generic)

-- | Configuration for a single webhook endpoint
data WebhookConfig = WebhookConfig
  { -- | Route name: /webhook/<name>
    wcName :: Text,
    -- | Required secret (header: X-Webhook-Secret)
    wcSecret :: Maybe Text,
    -- | Template with {{.field}} placeholders
    wcPromptTemplate :: Text,
    -- | Session mode: isolated or named:<id>
    wcSession :: SessionConfig,
    -- | Where to deliver responses
    wcDelivery :: [DeliveryTarget]
  }
  deriving stock (Show, Eq, Generic)

-- | Configuration for the webhook server
data WebhookServerConfig = WebhookServerConfig
  { -- | Whether webhook server is enabled
    wscEnabled :: Bool,
    -- | Port to listen on (default: 8080)
    wscPort :: Int,
    -- | Global secret (fallback if webhook has no secret)
    wscGlobalSecret :: Maybe Text,
    -- | Configured webhook endpoints
    wscWebhooks :: [WebhookConfig]
  }
  deriving stock (Show, Eq, Generic)

-- | YAML file configuration for webhook server
data WebhookServerConfigFile = WebhookServerConfigFile
  { wscfEnabled :: Maybe Bool,
    wscfPort :: Maybe Int,
    wscfGlobalSecret :: Maybe Text,
    wscfEndpoints :: Maybe [WebhookConfigFile]
  }
  deriving stock (Show, Generic)

-- | YAML file configuration for a webhook endpoint
data WebhookConfigFile = WebhookConfigFile
  { wcfName :: Text,
    wcfSecret :: Maybe Text,
    wcfPromptTemplate :: Text,
    wcfSession :: Maybe Text,
    wcfDeliver :: Maybe [Text]
  }
  deriving stock (Show, Generic)

instance FromJSON WebhookServerConfigFile where
  parseJSON = withObject "WebhookServerConfigFile" $ \v ->
    WebhookServerConfigFile
      <$> v .:? "enabled"
      <*> v .:? "port"
      <*> v .:? "globalSecret"
      <*> v .:? "endpoints"

instance FromJSON WebhookConfigFile where
  parseJSON = withObject "WebhookConfigFile" $ \v ->
    WebhookConfigFile
      <$> v .: "name"
      <*> v .:? "secret"
      <*> v .: "promptTemplate"
      <*> v .:? "session"
      <*> v .:? "deliver"
