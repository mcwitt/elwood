module Elwood.Tools.Types
  ( -- * Tool Types
    Tool (..),
    ToolResult (..),
    ApprovalFunction,
    ApprovalOutcome (..),

    -- * Attachment Types
    Attachment (..),
    AttachmentType (..),

    -- * Approval Defaults
    noApprovalChannel,

    -- * Result Helpers
    toolSuccess,
    toolError,
    taggedError,
    imageResultPart,

    -- * Failure Modes
    FailureMode (..),
    failureTag,
  )
where

import Data.Aeson (Value)
import Data.Text (Text)
import Elwood.Claude.Types qualified as Claude
import Elwood.Event.Types (Base64Data (..), ImageData (..), MediaType (..))

-- | Result of executing a tool
data ToolResult
  = -- | Successful execution with output parts (text and/or images)
    ToolSuccess [Claude.ToolResultPart]
  | -- | Execution failed with error message
    ToolError Text
  deriving stock (Show, Eq)

-- | Transport-agnostic approval request function.
-- Takes a tool name and input summary, returns the approval outcome.
type ApprovalFunction = Claude.ToolName -> Text -> IO ApprovalOutcome

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
noApprovalChannel :: ApprovalFunction
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

-- | Create a text-only success result
toolSuccess :: Text -> ToolResult
toolSuccess t = ToolSuccess [Claude.ToolResultText t]

-- | Create an error result
toolError :: Text -> ToolResult
toolError = ToolError

-- | Convert perceivable image data into a tool result part
imageResultPart :: ImageData -> Claude.ToolResultPart
imageResultPart img = Claude.ToolResultImage img.mediaType.unMediaType img.base64Data.unBase64Data

-- | The way a tool-level operation can fail. Surfaced verbatim to the
-- delegating agent so it can branch on the kind without parsing prose.
data FailureMode
  = -- | Sub-agent hit its @max_iterations@ cap before reaching end_turn.
    MaxIterations
  | -- | Wall-clock timeout (e.g. @timeout_seconds@ on a delegate task).
    Timeout
  | -- | User cancellation (e.g. @/stop@ or @cancel_task@).
    Cancelled
  | -- | Caught exception or other unclassified failure.
    Unexpected
  deriving stock (Show, Eq)

-- | Wire-level tag for a failure mode. The 'MaxIterations' variant uses
-- the longer form because it is also the JSON @status@ in delegate
-- exhaustion bodies (see 'Elwood.Claude.AgentLoop.formatExhaustion').
failureTag :: FailureMode -> Text
failureTag MaxIterations = "max_iterations_exceeded"
failureTag Timeout = "timeout"
failureTag Cancelled = "cancelled"
failureTag Unexpected = "error"

-- | Prefix an error message with a parseable failure-mode tag so the
-- orchestrator can branch on the kind without parsing prose.
taggedError :: FailureMode -> Text -> Text
taggedError mode msg = "[" <> failureTag mode <> "] " <> msg
