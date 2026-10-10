module Elwood.Tools.Schedule (mkScheduleCallbackTool, mkListCallbacksTool, mkCancelCallbackTool) where

import Data.Aeson (withObject)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Elwood.Event.Types (DeliveryTarget, SessionConfig)
import Elwood.Logging (Logger)
import Elwood.Notify (truncateText)
import Elwood.Scheduler
import Elwood.Tools.Builder qualified as B
import Elwood.Tools.Types

mkScheduleCallbackTool :: Logger -> CallbackStore -> SessionConfig -> DeliveryTarget -> Tool
mkScheduleCallbackTool _ store session deliveryTarget = B.tool "schedule_callback" "Schedule a one-shot wakeup in this conversation at an ISO 8601 UTC timestamp. Include self-contained instructions in prompt." fields parser $ \(fireAt, prompt) -> do
  key <- CallbackId . UUID.toText <$> nextRandom
  now <- getCurrentTime
  either toolError (const (toolSuccess ("Scheduled callback " <> key.unCallbackId <> " for " <> T.pack (show fireAt) <> "."))) <$> scheduleCallback store (Callback key fireAt prompt session deliveryTarget now)
  where
    fields = B.schema [("at", B.field "string" "ISO 8601 UTC timestamp"), ("prompt", B.field "string" "Instructions for the future turn")] ["at", "prompt"]
    parser = withObject "schedule_callback" $ \value -> do
      at <- B.nonempty value "at"
      timestamp <- maybe (fail "Invalid 'at' timestamp; expected ISO 8601 UTC") pure (iso8601ParseM (T.unpack at))
      (timestamp,) <$> B.nonempty value "prompt"

mkListCallbacksTool :: CallbackStore -> Tool
mkListCallbacksTool store = B.tool "list_callbacks" "List pending callbacks." (B.schema [] []) (withObject "list_callbacks" (const (pure ()))) $ \() -> do
  pending <- listCallbacks store
  pure $ toolSuccess $ if null pending then "No scheduled callbacks." else T.intercalate "\n" [callback.id_.unCallbackId <> "  " <> T.pack (show callback.fireAt) <> "  " <> truncateText 80 (T.replace "\n" " " callback.prompt) | callback <- pending]

mkCancelCallbackTool :: CallbackStore -> Tool
mkCancelCallbackTool store = B.tool "cancel_callback" "Cancel a pending callback by ID." (B.schema [("id", B.field "string" "Callback ID")] ["id"]) (withObject "cancel_callback" (`B.nonempty` "id")) $ \key -> do
  result <- cancelCallback store (CallbackId key)
  pure $ case result of
    Left err -> toolError ("Failed to cancel callback: " <> err)
    Right True -> toolSuccess ("Cancelled callback " <> key <> ".")
    Right False -> toolError ("Unknown callback: " <> key)
