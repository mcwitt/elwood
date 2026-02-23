module Elwood.MCP
  ( -- * Server Lifecycle
    spawnServer,
    stopServer,

    -- * Tool Discovery
    discoverTools,
    toTool,
    startMCPServers,

    -- * Communication
    sendRequest,

    -- * Types
    MCPServer (..),
    MCPTool (..),
    MCPError (..),
    JsonRpcRequest (..),
    JsonRpcResponse (..),
    JsonRpcError (..),
  )
where

import Elwood.MCP.Client
import Elwood.MCP.Registry
import Elwood.MCP.Types
