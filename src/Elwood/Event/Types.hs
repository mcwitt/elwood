{-# LANGUAGE StrictData #-}

-- | Core event types that don't depend on other Elwood modules.
-- This module breaks the import cycle between Event, Config, and Webhook.Types.
module Elwood.Event.Types
  ( -- * Event Source
    EventSource (..),

    -- * Session Configuration
    SessionConfig (..),

    -- * Delivery Targets
    DeliveryTarget (..),
  )
where

import Data.Int (Int64)
import Data.Text (Text)

-- | Source of an event
data EventSource
  = -- | Webhook with name
    WebhookSource Text
  | -- | Cron job with name
    CronSource Text
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

-- | Where to deliver event responses
data DeliveryTarget
  = -- | Send to specific Telegram chat
    TelegramDelivery Int64
  | -- | Broadcast to all allowed chats
    TelegramBroadcast
  | -- | Reply to originating chat (for TelegramSource)
    TelegramReply
  | -- | Just log, no notification
    LogOnly
  deriving stock (Show, Eq)
