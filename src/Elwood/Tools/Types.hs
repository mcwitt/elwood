{-# LANGUAGE StrictData #-}

module Elwood.Tools.Types
  ( -- * Tool Types
    Tool (..)
  , ToolResult (..)
  , ToolEnv (..)

    -- * Result Helpers
  , toolSuccess
  , toolError
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Network.HTTP.Client (Manager)

import Elwood.Logging (Logger)
import Elwood.Memory (MemoryStore)
import Elwood.Permissions (PermissionChecker)

-- | Result of executing a tool
data ToolResult
  = ToolSuccess Text
  -- ^ Successful execution with output
  | ToolError Text
  -- ^ Execution failed with error message
  deriving stock (Show, Eq)

-- | Environment available to tools during execution
data ToolEnv = ToolEnv
  { teLogger :: Logger
  -- ^ Logger for tool operations
  , teWorkspaceDir :: FilePath
  -- ^ Workspace directory (user files)
  , teStateDir :: FilePath
  -- ^ State directory (bot state)
  , tePermissions :: PermissionChecker
  -- ^ Permission checker
  , teHttpManager :: Manager
  -- ^ Shared HTTP manager for web tools
  , teBraveApiKey :: Maybe Text
  -- ^ Brave Search API key (optional)
  , teMemoryStore :: MemoryStore
  -- ^ Persistent memory store
  }

-- | A tool that can be used by Claude
data Tool = Tool
  { toolName :: Text
  -- ^ Unique tool name
  , toolDescription :: Text
  -- ^ Description of what the tool does
  , toolInputSchema :: Value
  -- ^ JSON Schema for input parameters
  , toolExecute :: ToolEnv -> Value -> IO ToolResult
  -- ^ Execute the tool with given input
  }

-- | Create a success result
toolSuccess :: Text -> ToolResult
toolSuccess = ToolSuccess

-- | Create an error result
toolError :: Text -> ToolResult
toolError = ToolError
