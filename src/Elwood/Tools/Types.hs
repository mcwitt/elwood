{-# LANGUAGE StrictData #-}

module Elwood.Tools.Types
  ( -- * Tool Types
    Tool (..),
    ToolResult (..),
    AgentContext (..),
    ApprovalOutcome (..),

    -- * Attachment Types
    Attachment (..),
    AttachmentType (..),

    -- * Approval Defaults
    noApprovalChannel,

    -- * Result Helpers
    toolSuccess,
    toolError,
  )
where

import Data.Aeson (Value)
import Data.Text (Text)
import Elwood.Claude.Types qualified as Claude
import Elwood.Permissions (PermissionConfig)

-- | Result of executing a tool
data ToolResult
  = -- | Successful execution with output
    ToolSuccess Text
  | -- | Execution failed with error message
    ToolError Text
  deriving stock (Show, Eq)

-- | Agent-level context for policy checking and approval
data AgentContext = AgentContext
  { -- | Permission configuration for tool policy
    permissionConfig :: PermissionConfig,
    -- | Approval request function (transport-agnostic)
    requestApproval :: Claude.ToolName -> Text -> IO ApprovalOutcome
  }

-- | Outcome of an approval request
data ApprovalOutcome
  = ApprovalGranted
  | ApprovalDenied
  | ApprovalTimeout
  | -- | No approval channel available (e.g. webhook-triggered events)
    ApprovalUnavailable
  deriving stock (Show, Eq)

-- | Default approval function for contexts without an interactive approval channel.
-- Always returns 'ApprovalUnavailable'.
noApprovalChannel :: Claude.ToolName -> Text -> IO ApprovalOutcome
noApprovalChannel _ _ = pure ApprovalUnavailable

-- | Type of attachment to send
data AttachmentType
  = -- | Send as a photo (compressed, preview in chat)
    AttachPhoto
  | -- | Send as a document (uncompressed, no preview)
    AttachDocument
  | -- | Auto-detect based on file extension
    AttachAuto
  deriving stock (Show, Eq)

-- | An attachment queued for sending after the text response
data Attachment = Attachment
  { -- | Absolute path to the file
    path :: FilePath,
    -- | How to send the file
    type_ :: AttachmentType,
    -- | Optional caption
    caption :: Maybe Text
  }
  deriving stock (Show, Eq)

-- | A tool that can be used by Claude
data Tool = Tool
  { -- | Tool schema (name, description, input schema)
    schema :: Claude.ToolSchema,
    -- | Execute the tool with given input
    execute :: Value -> IO ToolResult
  }

-- | Create a success result
toolSuccess :: Text -> ToolResult
toolSuccess = ToolSuccess

-- | Create an error result
toolError :: Text -> ToolResult
toolError = ToolError
