{-# LANGUAGE StrictData #-}

module Elwood.Tools.Types
  ( -- * Tool Types
    Tool (..),
    ToolResult (..),
    ToolEnv (..),
    ApprovalOutcome (..),

    -- * Result Helpers
    toolSuccess,
    toolError,
  )
where

import Data.Aeson (Value)
import Data.Int (Int64)
import Data.Text (Text)
import Elwood.Logging (Logger)
import Elwood.Memory (MemoryStore)
import Elwood.Permissions (PermissionChecker)
import Network.HTTP.Client (Manager)

-- | Result of executing a tool
data ToolResult
  = -- | Successful execution with output
    ToolSuccess Text
  | -- | Execution failed with error message
    ToolError Text
  deriving stock (Show, Eq)

-- | Environment available to tools during execution
data ToolEnv = ToolEnv
  { -- | Logger for tool operations
    teLogger :: Logger,
    -- | Workspace directory (user files)
    teWorkspaceDir :: FilePath,
    -- | State directory (bot state)
    teStateDir :: FilePath,
    -- | Permission checker
    tePermissions :: PermissionChecker,
    -- | Shared HTTP manager for web tools
    teHttpManager :: Manager,
    -- | Brave Search API key (optional)
    teBraveApiKey :: Maybe Text,
    -- | Persistent memory store
    teMemoryStore :: MemoryStore,
    -- | Chat ID for the current conversation (for approval requests)
    teChatId :: Maybe Int64,
    -- | Approval request function (sends Telegram message, returns result)
    teRequestApproval :: Maybe (Text -> Text -> IO ApprovalOutcome)
  }

-- | Outcome of an approval request (simplified for ToolEnv)
data ApprovalOutcome
  = ApprovalGranted
  | ApprovalDenied
  | ApprovalTimeout
  deriving stock (Show, Eq)

-- | A tool that can be used by Claude
data Tool = Tool
  { -- | Unique tool name
    toolName :: Text,
    -- | Description of what the tool does
    toolDescription :: Text,
    -- | JSON Schema for input parameters
    toolInputSchema :: Value,
    -- | Execute the tool with given input
    toolExecute :: ToolEnv -> Value -> IO ToolResult
  }

-- | Create a success result
toolSuccess :: Text -> ToolResult
toolSuccess = ToolSuccess

-- | Create an error result
toolError :: Text -> ToolResult
toolError = ToolError
