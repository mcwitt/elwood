module Elwood.MCP.Types
  ( MCPServer (..),
    MCPTool (..),
    MCPError (..),
    JsonRpcRequest (..),
    JsonRpcResponse (..),
    JsonRpcError (..),
  )
where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Elwood.Aeson (recordOptions)
import GHC.Generics (Generic)

data MCPServer = MCPServer
  {request :: Int -> Text -> Maybe Value -> IO (Either MCPError Value), close :: IO ()}

data MCPTool = MCPTool {name :: Text, description :: Maybe Text, inputSchema :: Value}
  deriving stock (Show, Eq, Generic)

instance FromJSON MCPTool where
  parseJSON = genericParseJSON defaultOptions

data MCPError
  = MCPSpawnError Text
  | MCPInitializeError Text
  | MCPRequestError Text
  | MCPProtocolError Text
  | MCPToolError Int Text
  deriving stock (Show, Eq)

data JsonRpcRequest = JsonRpcRequest {jsonrpc :: Text, method :: Text, params :: Maybe Value, id_ :: Int}
  deriving stock (Show, Eq, Generic)

instance ToJSON JsonRpcRequest where
  toJSON = genericToJSON recordOptions {omitNothingFields = True}

data JsonRpcResponse = JsonRpcResponse {jsonrpc :: Text, result :: Maybe Value, error :: Maybe JsonRpcError, id_ :: Maybe Int}
  deriving stock (Show, Eq)

-- JSON null is a valid result, distinct from a missing result field.
instance FromJSON JsonRpcResponse where
  parseJSON = withObject "JsonRpcResponse" $ \value ->
    JsonRpcResponse <$> value .: "jsonrpc" <*> pure (KM.lookup "result" value) <*> value .:? "error" <*> value .:? "id"

data JsonRpcError = JsonRpcError {code :: Int, message :: Text, data_ :: Maybe Value}
  deriving stock (Show, Eq, Generic)

instance FromJSON JsonRpcError where
  parseJSON = genericParseJSON recordOptions {rejectUnknownFields = False}
