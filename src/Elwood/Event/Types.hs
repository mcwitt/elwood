-- | Core event types that don't depend on other Elwood modules.
-- This module breaks the import cycle between Event, Config, and Webhook.Types.
module Elwood.Event.Types
  ( -- * Event Source
    EventSource (..),

    -- * Session Configuration
    SessionConfig (..),

    -- * Delivery Targets
    DeliveryTarget (..),

    -- * Image Data
    ImageData (..),
    MediaType (..),
    Base64Data (..),

    -- * Archived Attachments
    SavedAttachment (..),
  )
where

import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

-- | Source of an event
data EventSource
  = -- | Webhook with name
    WebhookSource Text
  | -- | Telegram message from chat ID
    TelegramSource Int64
  deriving stock (Show, Eq)

-- | Session configuration determines conversation persistence
data SessionConfig
  = -- | Fresh conversation each time (no history)
    Isolated
  | -- | Named persistent session
    Named Text
  deriving stock (Show, Eq)

-- | MIME type (e.g., "image/jpeg", "image/png")
newtype MediaType = MediaType {unMediaType :: Text}
  deriving stock (Show, Eq)

-- | Base64-encoded binary data
newtype Base64Data = Base64Data {unBase64Data :: Text}
  deriving stock (Show, Eq)

-- | Image data with media type and base64-encoded content
data ImageData = ImageData
  { mediaType :: MediaType,
    base64Data :: Base64Data
  }
  deriving stock (Show, Eq)

-- | An inbound attachment archived to the workspace inbox.
data SavedAttachment = SavedAttachment
  { -- | Workspace-relative path, e.g. "inbox/482-AgADabc.jpg"
    path :: FilePath,
    -- | MIME type, e.g. "image/jpeg", "application/pdf"
    mediaType :: Text,
    -- | Size of the archived original in bytes
    sizeBytes :: Int,
    -- | True iff also sent to the model as an image block
    perceivable :: Bool,
    -- | Original filename (documents), if any
    originalName :: Maybe Text
  }
  deriving stock (Show, Eq)

-- | Where to deliver event responses
data DeliveryTarget
  = -- | Send to specific Telegram chat(s)
    TelegramDelivery (NonEmpty Int64)
  | -- | Broadcast to all allowed chats
    TelegramBroadcast
  | -- | Just log, no notification
    LogOnly
  deriving stock (Show, Eq)
