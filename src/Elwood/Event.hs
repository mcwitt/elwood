{-# LANGUAGE StrictData #-}

module Elwood.Event
  ( -- * Event Types
    Event (..),

    -- * Re-exports from Event.Types
    EventSource (..),
    SessionConfig (..),
    DeliveryTarget (..),

    -- * Event Environment
    AppEnv (..),

    -- * Event Handling
    handleEvent,

    -- * Delivery
    deliverToTargets,

    -- * Session ID Utilities
    sessionToConversationId,

    -- * Utilities
    parseChatId,
    sendAttachmentSafe,
  )
where

import Control.Exception (SomeException, catch)
import Control.Monad (unless)
import Data.Aeson (Value)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Elwood.Claude.AgentLoop (AgentResult (..), RateLimitCallback, runAgentTurn)
import Elwood.Claude.Client (ClaudeClient)
import Elwood.Claude.Compaction (CompactionConfig)
import Elwood.Claude.Conversation (ConversationStore, getConversation, updateConversation)
import Elwood.Claude.Types (ClaudeMessage (..), ContentBlock (..), Conversation (..), Role (..))
import Elwood.Config (ThinkingLevel)
import Elwood.Event.Types
  ( DeliveryTarget (..),
    EventSource (..),
    SessionConfig (..),
  )
import Elwood.Logging (Logger, logError, logInfo)
import Elwood.Telegram.Client (TelegramClient, notify, sendDocument, sendPhoto)
import Elwood.Tools.Attachment (isPhotoExtension)
import Elwood.Tools.Registry (ToolRegistry)
import Elwood.Tools.Types (Attachment (..), AttachmentType (..), ToolEnv (..))
import Text.Read (readMaybe)

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
data AppEnv = AppEnv
  { eeLogger :: Logger,
    eeTelegram :: TelegramClient,
    eeClaude :: ClaudeClient,
    eeConversations :: ConversationStore,
    eeRegistry :: ToolRegistry,
    eeToolEnv :: ToolEnv,
    eeCompaction :: CompactionConfig,
    eeSystemPrompt :: Maybe Text,
    eeModel :: Text,
    -- | Extended thinking level
    eeThinking :: ThinkingLevel,
    -- | All allowed chat IDs (for broadcast)
    eeNotifyChatIds :: [Int64],
    -- | Attachment queue (shared with ToolEnv)
    eeAttachmentQueue :: IORef [Attachment]
  }

-- | Handle any event through the agent pipeline
--
-- Returns Either error message or success response text
handleEvent :: AppEnv -> Event -> IO (Either Text Text)
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
  let mConversationId = sessionToConversationId (evSession event)

  -- Get existing conversation history (empty for Isolated)
  history <- case mConversationId of
    Nothing -> pure []
    Just cid -> convMessages <$> getConversation (eeConversations env) cid

  -- Build user message with optional image
  let contentBlocks = case evImage event of
        Just (mediaType, imageData) ->
          [ImageBlock mediaType imageData, TextBlock (evPrompt event)]
        Nothing ->
          [TextBlock (evPrompt event)]
      userMsg = ClaudeMessage User contentBlocks

  -- Create rate limit notification callback based on delivery targets
  let rateLimitCallback = mkRateLimitCallback env event

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
      (eeThinking env)
      history
      userMsg
      (Just rateLimitCallback)
      `catch` \(e :: SomeException) -> do
        logError logger "Event handler agent error" [("error", T.pack (show e))]
        pure $ AgentError $ "Agent error: " <> T.pack (show e)

  case result of
    AgentSuccess responseText allMessages -> do
      -- Update conversation (skip for isolated sessions)
      case mConversationId of
        Nothing -> pure ()
        Just cid -> updateConversation (eeConversations env) cid allMessages

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

-- | Deliver response to configured targets, then send queued attachments
deliverResponse :: AppEnv -> Event -> Text -> IO ()
deliverResponse env event = deliverToTargets env (evDelivery event)

-- | Deliver a message to the specified targets, then send queued attachments
deliverToTargets :: AppEnv -> [DeliveryTarget] -> Text -> IO ()
deliverToTargets env targets msg = do
  mapM_ deliver targets
  -- Send queued attachments to Telegram targets (if any)
  -- Only drain the queue when there are real Telegram targets; for LogOnly
  -- the polling path in App.hs handles attachment delivery separately.
  let chatIds = concatMap (targetChatIds env) targets
  unless (null chatIds) $ do
    attachments <- readIORef (eeAttachmentQueue env)
    mapM_ (\att -> mapM_ (\cid -> sendAttachmentSafe env cid att) chatIds) attachments
    writeIORef (eeAttachmentQueue env) []
  where
    deliver :: DeliveryTarget -> IO ()
    deliver target = case target of
      TelegramDelivery session ->
        case parseChatId session of
          Just chatId -> notifySafe env chatId msg
          Nothing -> logError (eeLogger env) "Invalid chat ID in TelegramDelivery" [("session", session)]
      TelegramBroadcast ->
        mapM_ (\cid -> notifySafe env cid msg) (eeNotifyChatIds env)
      LogOnly ->
        logInfo (eeLogger env) "Event response (log only)" [("response", T.take 100 msg)]

-- | Resolve a delivery target to its Telegram chat IDs
targetChatIds :: AppEnv -> DeliveryTarget -> [Int64]
targetChatIds _ (TelegramDelivery session) =
  case parseChatId session of
    Just cid -> [cid]
    Nothing -> []
targetChatIds env TelegramBroadcast = eeNotifyChatIds env
targetChatIds _ LogOnly = []

-- | Parse a chat ID from text
parseChatId :: Text -> Maybe Int64
parseChatId = readMaybe . T.unpack

-- | Send a single attachment to a chat, choosing photo vs document
sendAttachmentSafe :: AppEnv -> Int64 -> Attachment -> IO ()
sendAttachmentSafe env chatId att = do
  let send = case attType att of
        AttachPhoto -> sendPhoto
        AttachDocument -> sendDocument
        AttachAuto
          | isPhotoExtension (attPath att) -> sendPhoto
          | otherwise -> sendDocument
  send (eeTelegram env) chatId (attPath att) (attCaption att)
    `catch` \(e :: SomeException) ->
      logError
        (eeLogger env)
        "Failed to send attachment"
        [ ("chat_id", T.pack (show chatId)),
          ("path", T.pack (attPath att)),
          ("error", T.pack (show e))
        ]

-- | Safely send notification, catching any errors
notifySafe :: AppEnv -> Int64 -> Text -> IO ()
notifySafe env chatId msg = do
  notify (eeLogger env) (eeTelegram env) chatId msg
    `catch` \(e :: SomeException) ->
      logError (eeLogger env) "Failed to send notification" [("chat_id", T.pack (show chatId)), ("error", T.pack (show e))]

-- | Convert session config to conversation ID
--
-- For Isolated sessions, returns Nothing (no conversation persistence).
-- For Named sessions, returns the session name as the conversation key.
sessionToConversationId :: SessionConfig -> Maybe Text
sessionToConversationId Isolated = Nothing
sessionToConversationId (Named name) = Just name

-- | Create rate limit notification callback based on event delivery targets
mkRateLimitCallback :: AppEnv -> Event -> RateLimitCallback
mkRateLimitCallback env event attemptNum waitSecs = do
  let msg = "(rate limited, retry " <> T.pack (show attemptNum) <> " in " <> T.pack (show waitSecs) <> "s)"
  -- Notify based on delivery targets
  mapM_ (notifyRateLimit msg) (evDelivery event)
  where
    notifyRateLimit :: Text -> DeliveryTarget -> IO ()
    notifyRateLimit msg target = case target of
      TelegramDelivery session ->
        case parseChatId session of
          Just chatId -> notifySafe env chatId msg
          Nothing -> pure ()
      TelegramBroadcast ->
        mapM_ (\cid -> notifySafe env cid msg) (eeNotifyChatIds env)
      LogOnly ->
        logInfo (eeLogger env) "Rate limited" [("attempt", T.pack (show attemptNum)), ("wait_seconds", T.pack (show waitSecs))]

-- | Format event source for logging
formatSource :: EventSource -> Text
formatSource (WebhookSource name) = "webhook:" <> name
formatSource (CronSource name) = "cron:" <> name
formatSource (TelegramSource chatId) = "telegram:" <> T.pack (show chatId)
