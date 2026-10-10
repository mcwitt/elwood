module Elwood.Provider (ApiFormat (..), ToolResultImageMode (..), ProviderConfig (..), ProviderConfigFile (..)) where

import Data.Aeson (FromJSON (..), genericParseJSON)
import Data.Text (Text)
import Elwood.Aeson (enum, recordOptions)
import GHC.Generics (Generic)

data ApiFormat = AnthropicFormat
  deriving stock (Show, Eq)

instance FromJSON ApiFormat where
  parseJSON = enum "provider format" [("anthropic", AnthropicFormat)]

data ToolResultImageMode = ImagesEmbedded | ImagesHoisted
  deriving stock (Show, Eq)

instance FromJSON ToolResultImageMode where
  parseJSON = enum "tool result images" [("embedded", ImagesEmbedded), ("hoisted", ImagesHoisted)]

data ProviderConfig = ProviderConfig
  { name :: Text,
    baseUrl :: Text,
    apiKey :: Maybe Text,
    format :: ApiFormat,
    toolResultImages :: ToolResultImageMode
  }
  deriving stock (Show, Eq)

data ProviderConfigFile = ProviderConfigFile
  { baseUrl :: Text,
    apiKey :: Maybe Text,
    apiKeyEnv :: Maybe Text,
    format :: Maybe ApiFormat,
    toolResultImages :: Maybe ToolResultImageMode
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ProviderConfigFile where
  parseJSON = genericParseJSON recordOptions
