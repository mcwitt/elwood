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

    -- * Result Helpers
    toolSuccess,
    toolError,
  )
where

import Data.Aeson (Value)
import Data.Text (Text)
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
    acPermissionConfig :: PermissionConfig,
    -- | Approval request function (sends Telegram message, returns result)
    acRequestApproval :: Maybe (Text -> Text -> IO ApprovalOutcome)
  }

-- | Outcome of an approval request
data ApprovalOutcome
  = ApprovalGranted
  | ApprovalDenied
  | ApprovalTimeout
  deriving stock (Show, Eq)

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
    attPath :: FilePath,
    -- | How to send the file
    attType :: AttachmentType,
    -- | Optional caption
    attCaption :: Maybe Text
  }
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
    toolExecute :: Value -> IO ToolResult
  }

-- | Create a success result
toolSuccess :: Text -> ToolResult
toolSuccess = ToolSuccess

-- | Create an error result
toolError :: Text -> ToolResult
toolError = ToolError
