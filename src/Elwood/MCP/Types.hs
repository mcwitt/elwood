{-# LANGUAGE StrictData #-}

module Elwood.MCP.Types
  ( -- * MCP Server Types
    MCPServer (..)
  , MCPTool (..)
  , MCPError (..)

    -- * JSON-RPC Types
  , JsonRpcRequest (..)
  , JsonRpcResponse (..)
  , JsonRpcError (..)
  ) where

import Data.Aeson
import Data.IORef (IORef)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.IO (Handle)
import System.Process (ProcessHandle)

import Elwood.Config (MCPServerConfig)

-- | A running MCP server instance
data MCPServer = MCPServer
  { msConfig :: MCPServerConfig
  -- ^ Configuration used to spawn this server
  , msProcess :: ProcessHandle
  -- ^ Handle to the subprocess
  , msStdin :: Handle
  -- ^ Handle to write JSON-RPC requests
  , msStdout :: Handle
  -- ^ Handle to read JSON-RPC responses
  , msRequestId :: IORef Int
  -- ^ Counter for generating unique request IDs
  }

-- | Tool definition from MCP server
data MCPTool = MCPTool
  { mtName :: Text
  -- ^ Tool name (without server prefix)
  , mtDescription :: Maybe Text
  -- ^ Tool description
  , mtInputSchema :: Value
  -- ^ JSON Schema for tool input
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
  = MCPSpawnError Text
  -- ^ Failed to spawn the MCP server process
  | MCPInitializeError Text
  -- ^ Failed during initialize handshake
  | MCPRequestError Text
  -- ^ Error sending/receiving JSON-RPC request
  | MCPProtocolError Text
  -- ^ Protocol-level error from server
  | MCPToolError Int Text
  -- ^ Tool execution error with code and message
  deriving stock (Show, Eq)

-- | JSON-RPC 2.0 request
data JsonRpcRequest = JsonRpcRequest
  { jrqJsonrpc :: Text
  -- ^ Always "2.0"
  , jrqMethod :: Text
  -- ^ Method name
  , jrqParams :: Maybe Value
  -- ^ Method parameters
  , jrqId :: Int
  -- ^ Request ID for matching responses
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON JsonRpcRequest where
  toJSON req =
    object $
      [ "jsonrpc" .= jrqJsonrpc req
      , "method" .= jrqMethod req
      , "id" .= jrqId req
      ]
        ++ maybe [] (\p -> ["params" .= p]) (jrqParams req)

-- | JSON-RPC 2.0 response
data JsonRpcResponse = JsonRpcResponse
  { jrsJsonrpc :: Text
  -- ^ Always "2.0"
  , jrsResult :: Maybe Value
  -- ^ Result on success
  , jrsError :: Maybe JsonRpcError
  -- ^ Error on failure
  , jrsId :: Maybe Int
  -- ^ Request ID (may be null for notifications/errors)
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
  { jreCode :: Int
  -- ^ Error code
  , jreMessage :: Text
  -- ^ Error message
  , jreData :: Maybe Value
  -- ^ Additional error data
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON JsonRpcError where
  parseJSON = withObject "JsonRpcError" $ \v ->
    JsonRpcError
      <$> v .: "code"
      <*> v .: "message"
      <*> v .:? "data"
