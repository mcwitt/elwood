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
    wcName :: Text,
    -- | Required secret (header: X-Webhook-Secret)
    wcSecret :: Maybe Text,
    -- | Template with {{.field}} placeholders (mutually exclusive with wcPromptFile)
    wcPromptTemplate :: Maybe Text,
    -- | Absolute path to prompt file (resolved from workspaceDir at config load time)
    wcPromptFile :: Maybe FilePath,
    -- | Session mode: isolated or named:<id>
    wcSession :: SessionConfig,
    -- | Where to deliver responses
    wcDelivery :: [DeliveryTarget],
    -- | Suppress notification if response contains this string
    wcSuppressIfContains :: Maybe Text,
    -- | Model override for this endpoint (Nothing = use global)
    wcModel :: Maybe Text,
    -- | Thinking level override for this endpoint (Nothing = use global)
    wcThinking :: Maybe Value
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
    wcfPromptTemplate :: Maybe Text,
    wcfPromptFile :: Maybe FilePath,
    wcfSession :: Maybe Text,
    wcfDeliver :: Maybe [DeliveryTargetFile],
    wcfSuppressIfContains :: Maybe Text,
    wcfModel :: Maybe Text,
    wcfThinking :: Maybe Value
  }
  deriving stock (Show, Generic)

-- | YAML file configuration for a delivery target
data DeliveryTargetFile = DeliveryTargetFile
  { dtfType :: Text,
    dtfSession :: Maybe Text
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
    rejectUnknownKeys "WebhookConfigFile" ["name", "secret", "promptTemplate", "promptFile", "session", "deliver", "suppressIfContains", "model", "thinking"] v
    WebhookConfigFile
      <$> v .: "name"
      <*> v .:? "secret"
      <*> v .:? "promptTemplate"
      <*> v .:? "promptFile"
      <*> v .:? "session"
      <*> v .:? "deliver"
      <*> v .:? "suppressIfContains"
      <*> v .:? "model"
      <*> v .:? "thinking"
