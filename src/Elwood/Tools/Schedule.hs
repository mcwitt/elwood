{-# LANGUAGE OverloadedRecordDot #-}

module Elwood.Tools.Schedule
  ( mkScheduleCallbackTool,
    mkListCallbacksTool,
    mkCancelCallbackTool,
  )
where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Elwood.Claude.Types (ToolSchema (..))
import Elwood.Event.Types (DeliveryTarget, SessionConfig)
import Elwood.Logging (Logger, logInfo)
import Elwood.Notify (truncateText)
import Elwood.Scheduler
  ( Callback (..),
    CallbackId (..),
    CallbackStore,
    cancelCallback,
    listCallbacks,
    scheduleCallback,
  )
import Elwood.Tools.Types (Tool (..), ToolResult, toolError, toolSuccess)

-- | schedule_callback: enqueue a one-shot wakeup at an absolute time. Built
-- per-request so it captures the scheduling turn's session and delivery target.
mkScheduleCallbackTool :: Logger -> CallbackStore -> SessionConfig -> DeliveryTarget -> Tool
mkScheduleCallbackTool lgr store session deliveryTarget =
  Tool
    { schema =
        ToolSchema
          { name = "schedule_callback",
            description =
              "Schedule a one-shot wakeup at an absolute time. At the scheduled "
                <> "time a new turn is injected into THIS conversation and delivered "
                <> "to THIS chat. Provide 'at' as an ISO 8601 UTC timestamp "
                <> "(e.g. 2026-06-13T13:15:00Z) and 'prompt' as self-contained "
                <> "instructions for your future self — include everything needed to "
                <> "act without re-deriving state. One-shot only; use list_callbacks "
                <> "and cancel_callback to manage pending callbacks.",
            inputSchema = scheduleSchema
          },
      execute = executeSchedule lgr store session deliveryTarget
    }

scheduleSchema :: Value
scheduleSchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "at"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("Absolute fire time, ISO 8601 UTC (e.g. 2026-06-13T13:15:00Z)" :: Text)
                ],
            "prompt"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("Self-contained instructions for the woken turn" :: Text)
                ]
          ],
      "required" .= (["at", "prompt"] :: [Text])
    ]

executeSchedule :: Logger -> CallbackStore -> SessionConfig -> DeliveryTarget -> Value -> IO ToolResult
executeSchedule lgr store session deliveryTarget input =
  case (,) <$> reqText "at" input <*> reqText "prompt" input of
    Left err -> pure (toolError err)
    Right (atText, promptText) ->
      case iso8601ParseM (T.unpack atText) of
        Nothing ->
          pure (toolError ("Invalid 'at' timestamp (expected ISO 8601 UTC, e.g. 2026-06-13T13:15:00Z): " <> atText))
        Just fireAt -> do
          uuid <- nextRandom
          now <- getCurrentTime
          let cb =
                Callback
                  { id_ = CallbackId (UUID.toText uuid),
                    fireAt = fireAt,
                    prompt = promptText,
                    session = session,
                    deliveryTarget = deliveryTarget,
                    createdAt = now
                  }
          res <- scheduleCallback store cb
          case res of
            Left err -> pure (toolError err)
            Right () -> do
              logInfo lgr "Scheduled callback" [("id", UUID.toText uuid), ("fire_at", T.pack (show fireAt))]
              pure (toolSuccess ("Scheduled callback " <> UUID.toText uuid <> " for " <> atText <> "."))

-- | list_callbacks: show pending callbacks (id, fire time, prompt preview).
mkListCallbacksTool :: CallbackStore -> Tool
mkListCallbacksTool store =
  Tool
    { schema =
        ToolSchema
          { name = "list_callbacks",
            description = "List pending scheduled callbacks with their id, fire time, and a prompt preview.",
            inputSchema = emptyObjectSchema
          },
      execute = \_ -> do
        cbs <- listCallbacks store
        pure $
          if null cbs
            then toolSuccess "No scheduled callbacks."
            else toolSuccess (T.intercalate "\n" (map formatCallback cbs))
    }

formatCallback :: Callback -> Text
formatCallback c =
  c.id_.unCallbackId <> "  " <> T.pack (show c.fireAt) <> "  " <> preview c.prompt
  where
    preview = truncateText 80 . T.map (\ch -> if ch == '\n' then ' ' else ch)

-- | cancel_callback: cancel a pending callback by id.
mkCancelCallbackTool :: CallbackStore -> Tool
mkCancelCallbackTool store =
  Tool
    { schema =
        ToolSchema
          { name = "cancel_callback",
            description = "Cancel a pending scheduled callback by its id.",
            inputSchema = cancelSchema
          },
      execute = \input ->
        case reqText "id" input of
          Left err -> pure (toolError err)
          Right cid -> do
            res <- cancelCallback store (CallbackId cid)
            pure $ case res of
              Left err -> toolError ("Failed to cancel callback: " <> err)
              Right True -> toolSuccess ("Cancelled callback " <> cid <> ".")
              Right False -> toolError ("Unknown callback: " <> cid)
    }

cancelSchema :: Value
cancelSchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "id"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("Id of the callback to cancel" :: Text)
                ]
          ],
      "required" .= (["id"] :: [Text])
    ]

emptyObjectSchema :: Value
emptyObjectSchema = object ["type" .= ("object" :: Text), "properties" .= object []]

-- | Require a non-empty string field from an object input.
reqText :: Text -> Value -> Either Text Text
reqText name (Object o) = case KM.lookup (Key.fromText name) o of
  Just (String s)
    | not (T.null (T.strip s)) -> Right s
    | otherwise -> Left (name <> " must not be empty")
  Just _ -> Left (name <> " must be a string")
  Nothing -> Left ("Missing required '" <> name <> "' parameter")
reqText _ _ = Left "Expected object input"
