module Elwood.Provider
  ( ApiFormat (..),
    ProviderConfig (..),
    ProviderConfigFile (..),
  )
where

import Data.Aeson (FromJSON (..), withObject, withText, (.:), (.:?))
import Data.Text (Text)
import Elwood.Aeson (rejectUnknownKeys)

-- | Wire format a provider speaks. The seam for future OpenAI support;
-- only 'AnthropicFormat' is implemented now.
data ApiFormat = AnthropicFormat
  deriving stock (Show, Eq)

instance FromJSON ApiFormat where
  parseJSON = withText "ApiFormat" $ \case
    "anthropic" -> pure AnthropicFormat
    other ->
      fail $
        "Unsupported provider format: "
          <> show other
          <> ". Only \"anthropic\" is currently supported."

-- | A resolved provider endpoint.
data ProviderConfig = ProviderConfig
  { -- | Map key, kept for logging and error messages
    name :: Text,
    -- | Base URL, e.g. "https://api.anthropic.com" or "http://host:port"
    baseUrl :: Text,
    -- | Optional API key (local servers need none)
    apiKey :: Maybe Text,
    -- | Wire format (determines request/response encoding)
    format :: ApiFormat
  }
  deriving stock (Show, Eq)

-- | Provider endpoint as parsed from YAML (partial). 'baseUrl' is required.
data ProviderConfigFile = ProviderConfigFile
  { -- | Endpoint base URL (required)
    baseUrl :: Text,
    -- | Inline API key (discouraged; prefer apiKeyEnv)
    apiKey :: Maybe Text,
    -- | Name of an environment variable holding the API key
    apiKeyEnv :: Maybe Text,
    -- | Wire format (default: anthropic)
    format :: Maybe ApiFormat
  }
  deriving stock (Show, Eq)

instance FromJSON ProviderConfigFile where
  parseJSON = withObject "ProviderConfigFile" $ \v -> do
    rejectUnknownKeys "ProviderConfigFile" ["base_url", "api_key", "api_key_env", "format"] v
    ProviderConfigFile
      <$> v .: "base_url"
      <*> v .:? "api_key"
      <*> v .:? "api_key_env"
      <*> v .:? "format"
