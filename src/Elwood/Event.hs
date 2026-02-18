{-# LANGUAGE StrictData #-}

module Elwood.Event
  ( -- * Event Types
    Event (..),

    -- * Re-exports from Event.Types
    EventSource (..),
    SessionConfig (..),
    DeliveryTarget (..),

    -- * Event Environment
    EventEnv (..),

    -- * Event Handling
    handleEvent,

    -- * Session ID Utilities
    sessionToConversationId,
  )
where

import Control.Exception (SomeException, catch)
import Data.Aeson (Value)
import Data.Bits (shiftL, xor)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Elwood.Claude.AgentLoop (AgentResult (..), runAgentTurn)
import Elwood.Claude.Client (ClaudeClient)
import Elwood.Claude.Compaction (CompactionConfig)
import Elwood.Claude.Conversation (ConversationStore, getConversation, updateConversation)
import Elwood.Claude.Types (ClaudeMessage (..), ContentBlock (..), Conversation (..), Role (..))
import Elwood.Event.Types
  ( DeliveryTarget (..),
    EventSource (..),
    SessionConfig (..),
  )
import Elwood.Logging (Logger, logError, logInfo)
import Elwood.Telegram.Client (TelegramClient, notify, sendMessage)
import Elwood.Tools.Registry (ToolRegistry)
import Elwood.Tools.Types (ToolEnv)

-- | Unified event type for all sources
data Event = Event
  { -- | Where this event came from
    evSource :: EventSource,
    -- | When the event occurred
    evTimestamp :: UTCTime,
    -- | Source-specific JSON data (e.g., webhook payload)
    evPayload :: Value,
    -- | Rendered prompt for the agent
    evPrompt :: Text,
    -- | Optional image data (media type, base64 data)
    evImage :: Maybe (Text, Text),
    -- | Session configuration
    evSession :: SessionConfig,
    -- | Where to deliver responses
    evDelivery :: [DeliveryTarget]
  }
  deriving stock (Show)

-- | Environment for event handling
data EventEnv = EventEnv
  { eeLogger :: Logger,
    eeTelegram :: TelegramClient,
    eeClaude :: ClaudeClient,
    eeConversations :: ConversationStore,
    eeRegistry :: ToolRegistry,
    eeToolEnv :: ToolEnv,
    eeCompaction :: CompactionConfig,
    eeSystemPrompt :: Maybe Text,
    eeModel :: Text,
    -- | All allowed chat IDs (for broadcast)
    eeNotifyChatIds :: [Int64]
  }

-- | Handle any event through the agent pipeline
--
-- Returns Either error message or success response text
handleEvent :: EventEnv -> Event -> IO (Either Text Text)
handleEvent env event = do
  let logger = eeLogger env
      source = evSource event

  logInfo
    logger
    "Handling event"
    [ ("source", formatSource source),
      ("session", T.pack (show (evSession event))),
      ("delivery_targets", T.pack (show (length (evDelivery event))))
    ]

  -- Determine conversation ID from session config
  let conversationId = sessionToConversationId (evSession event) source

  -- Get existing conversation (empty for Isolated)
  conv <- getConversation (eeConversations env) conversationId

  -- Build user message with optional image
  let contentBlocks = case evImage event of
        Just (mediaType, imageData) ->
          [ImageBlock mediaType imageData, TextBlock (evPrompt event)]
        Nothing ->
          [TextBlock (evPrompt event)]
      userMsg = ClaudeMessage User contentBlocks

  -- Run the agent turn
  result <-
    runAgentTurn
      logger
      (eeClaude env)
      (eeRegistry env)
      (eeToolEnv env)
      (eeCompaction env)
      (eeSystemPrompt env)
      (eeModel env)
      (if evSession event == Isolated then [] else convMessages conv)
      userMsg
      `catch` \(e :: SomeException) -> do
        logError logger "Event handler agent error" [("error", T.pack (show e))]
        pure $ AgentError $ "Agent error: " <> T.pack (show e)

  case result of
    AgentSuccess responseText allMessages -> do
      -- Update conversation (skip for isolated sessions)
      case evSession event of
        Isolated -> pure ()
        Named _ -> updateConversation (eeConversations env) conversationId allMessages

      -- Deliver response
      deliverResponse env event responseText

      logInfo
        logger
        "Event handled successfully"
        [ ("source", formatSource source),
          ("response_length", T.pack (show (T.length responseText)))
        ]

      pure (Right responseText)
    AgentError err -> do
      logError logger "Event handling failed" [("source", formatSource source), ("error", err)]
      pure (Left err)

-- | Deliver response to configured targets
deliverResponse :: EventEnv -> Event -> Text -> IO ()
deliverResponse env event responseText = do
  mapM_ deliver (evDelivery event)
  where
    deliver :: DeliveryTarget -> IO ()
    deliver target = case target of
      TelegramDelivery chatId ->
        notifySafe env chatId responseText
      TelegramBroadcast ->
        mapM_ (\cid -> notifySafe env cid responseText) (eeNotifyChatIds env)
      TelegramReply ->
        case evSource event of
          TelegramSource chatId ->
            sendMessageSafe env chatId responseText
          _ ->
            -- Can't reply if not from Telegram, fall back to broadcast
            mapM_ (\cid -> notifySafe env cid responseText) (eeNotifyChatIds env)
      LogOnly ->
        logInfo (eeLogger env) "Event response (log only)" [("response", T.take 100 responseText)]

-- | Safely send notification, catching any errors
notifySafe :: EventEnv -> Int64 -> Text -> IO ()
notifySafe env chatId msg = do
  notify (eeLogger env) (eeTelegram env) chatId msg
    `catch` \(e :: SomeException) ->
      logError (eeLogger env) "Failed to send notification" [("chat_id", T.pack (show chatId)), ("error", T.pack (show e))]

-- | Safely send message, catching any errors
sendMessageSafe :: EventEnv -> Int64 -> Text -> IO ()
sendMessageSafe env chatId msg = do
  sendMessage (eeTelegram env) chatId msg
    `catch` \(e :: SomeException) ->
      logError (eeLogger env) "Failed to send message" [("chat_id", T.pack (show chatId)), ("error", T.pack (show e))]

-- | Convert session config to conversation ID
--
-- For Isolated sessions, returns a unique negative ID based on timestamp.
-- For Named sessions, returns a stable negative ID based on hash.
-- For TelegramSource with Named, uses the chat ID directly.
sessionToConversationId :: SessionConfig -> EventSource -> Int64
sessionToConversationId session source = case session of
  Isolated ->
    -- Use a large negative number - in practice, unique per invocation
    -- The caller should use a timestamp-based ID for true uniqueness
    -999999
  Named name ->
    case source of
      TelegramSource chatId ->
        -- For Telegram, use the actual chat ID for conversation continuity
        chatId
      _ ->
        -- For other sources, hash the session name to a stable negative ID
        negate (fromIntegral (abs (hashSessionName name)) + 2)

-- | DJB2 hash function for session names
hashSessionName :: Text -> Int
hashSessionName = T.foldl' step 5381
  where
    step :: Int -> Char -> Int
    step h c = ((h `shiftL` 5) + h) `xor` fromEnum c

-- | Format event source for logging
formatSource :: EventSource -> Text
formatSource (WebhookSource name) = "webhook:" <> name
formatSource (CronSource name) = "cron:" <> name
formatSource HeartbeatSource = "heartbeat"
formatSource (TelegramSource chatId) = "telegram:" <> T.pack (show chatId)
