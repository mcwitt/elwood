module Elwood.Tools.Types
  ( Tool (..),
    ToolResult (..),
    ApprovalFunction,
    ApprovalOutcome (..),
    Attachment (..),
    AttachmentType (..),
    noApprovalChannel,
    toolSuccess,
    toolError,
    taggedError,
    imageResultPart,
    FailureMode (..),
    failureTag,
  )
where

import Data.Aeson (Value)
import Data.Text (Text)
import Elwood.Claude.Types qualified as Claude
import Elwood.Event.Types (Base64Data (..), ImageData (..), MediaType (..))

-- A tool's interpreter receives only parsed inputs through Tools.Builder.
data Tool = Tool {schema :: Claude.ToolSchema, execute :: Value -> IO ToolResult}

data ToolResult = ToolSuccess [Claude.ToolResultPart] | ToolError Text
  deriving stock (Show, Eq)

type ApprovalFunction = Claude.ToolName -> Text -> IO ApprovalOutcome

data ApprovalOutcome = ApprovalGranted | ApprovalDenied | ApprovalTimeout | ApprovalUnavailable
  deriving stock (Show, Eq)

noApprovalChannel :: ApprovalFunction
noApprovalChannel _ _ = pure ApprovalUnavailable

data AttachmentType = AttachPhoto | AttachDocument | AttachAuto
  deriving stock (Show, Eq)

data Attachment = Attachment {path :: FilePath, type_ :: AttachmentType, caption :: Maybe Text}
  deriving stock (Show, Eq)

toolSuccess :: Text -> ToolResult
toolSuccess = ToolSuccess . pure . Claude.ToolResultText

toolError :: Text -> ToolResult
toolError = ToolError

imageResultPart :: ImageData -> Claude.ToolResultPart
imageResultPart image = Claude.ToolResultImage image.mediaType.unMediaType image.base64Data.unBase64Data

-- These tags are a wire contract consumed by delegating agents.
data FailureMode = MaxIterations | Timeout | Cancelled | Unexpected
  deriving stock (Show, Eq)

failureTag :: FailureMode -> Text
failureTag MaxIterations = "max_iterations_exceeded"
failureTag Timeout = "timeout"
failureTag Cancelled = "cancelled"
failureTag Unexpected = "error"

taggedError :: FailureMode -> Text -> Text
taggedError mode message = "[" <> failureTag mode <> "] " <> message
