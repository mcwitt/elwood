{-# LANGUAGE StrictData #-}

module Elwood.MCP.Types
  ( -- * MCP Server Types
    MCPServer (..),
    MCPTool (..),
    MCPError (..),

    -- * JSON-RPC Types
    JsonRpcRequest (..),
    JsonRpcResponse (..),
    JsonRpcError (..),
  )
where

import Data.Aeson
import Data.IORef (IORef)
import Data.Text (Text)
import Elwood.Config (MCPServerConfig)
import GHC.Generics (Generic)
import System.IO (Handle)
import System.Process (ProcessHandle)

-- | A running MCP server instance
data MCPServer = MCPServer
  { -- | Configuration used to spawn this server
    msConfig :: MCPServerConfig,
    -- | Handle to the subprocess
    msProcess :: ProcessHandle,
    -- | Handle to write JSON-RPC requests
    msStdin :: Handle,
    -- | Handle to read JSON-RPC responses
    msStdout :: Handle,
    -- | Counter for generating unique request IDs
    msRequestId :: IORef Int
  }

-- | Tool definition from MCP server
data MCPTool = MCPTool
  { -- | Tool name (without server prefix)
    mtName :: Text,
    -- | Tool description
    mtDescription :: Maybe Text,
    -- | JSON Schema for tool input
    mtInputSchema :: Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON MCPTool where
  parseJSON = withObject "MCPTool" $ \v ->
    MCPTool
      <$> v .: "name"
      <*> v .:? "description"
      <*> v .: "inputSchema"

-- | Errors that can occur during MCP operations
data MCPError
  = -- | Failed to spawn the MCP server process
    MCPSpawnError Text
  | -- | Failed during initialize handshake
    MCPInitializeError Text
  | -- | Error sending/receiving JSON-RPC request
    MCPRequestError Text
  | -- | Protocol-level error from server
    MCPProtocolError Text
  | -- | Tool execution error with code and message
    MCPToolError Int Text
  deriving stock (Show, Eq)

-- | JSON-RPC 2.0 request
data JsonRpcRequest = JsonRpcRequest
  { -- | Always "2.0"
    jrqJsonrpc :: Text,
    -- | Method name
    jrqMethod :: Text,
    -- | Method parameters
    jrqParams :: Maybe Value,
    -- | Request ID for matching responses
    jrqId :: Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON JsonRpcRequest where
  toJSON req =
    object $
      [ "jsonrpc" .= jrqJsonrpc req,
        "method" .= jrqMethod req,
        "id" .= jrqId req
      ]
        ++ maybe [] (\p -> ["params" .= p]) (jrqParams req)

-- | JSON-RPC 2.0 response
data JsonRpcResponse = JsonRpcResponse
  { -- | Always "2.0"
    jrsJsonrpc :: Text,
    -- | Result on success
    jrsResult :: Maybe Value,
    -- | Error on failure
    jrsError :: Maybe JsonRpcError,
    -- | Request ID (may be null for notifications/errors)
    jrsId :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON JsonRpcResponse where
  parseJSON = withObject "JsonRpcResponse" $ \v ->
    JsonRpcResponse
      <$> v .: "jsonrpc"
      <*> v .:? "result"
      <*> v .:? "error"
      <*> v .:? "id"

-- | JSON-RPC 2.0 error object
data JsonRpcError = JsonRpcError
  { -- | Error code
    jreCode :: Int,
    -- | Error message
    jreMessage :: Text,
    -- | Additional error data
    jreData :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON JsonRpcError where
  parseJSON = withObject "JsonRpcError" $ \v ->
    JsonRpcError
      <$> v .: "code"
      <*> v .: "message"
      <*> v .:? "data"
