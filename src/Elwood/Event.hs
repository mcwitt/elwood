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
    handleTelegramMessage,

    -- * Delivery
    deliverToTargets,

    -- * Session ID Utilities
    sessionToConversationId,

    -- * Utilities
    parseChatId,
    sendAttachmentSafe,
  )
where

import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Exception (SomeException, catch)
import Control.Monad (unless)
import Data.Aeson (Value (..))
import Data.ByteString.Base64.Lazy qualified as B64
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, getCurrentTime)
import Elwood.Claude.AgentLoop (AgentConfig (..), AgentResult (..), RateLimitCallback, TextCallback, runAgentTurn)
import Elwood.Claude.Client (ClaudeClient)
import Elwood.Claude.Compaction (CompactionConfig)
import Elwood.Claude.Conversation (ConversationStore, clearConversation, getConversation, updateConversation)
import Elwood.Claude.Types (ClaudeMessage (..), ContentBlock (..), Conversation (..), Role (..))
import Elwood.Config (ThinkingLevel)
import Elwood.Event.Types
  ( DeliveryTarget (..),
    EventSource (..),
    SessionConfig (..),
  )
import Elwood.Logging (Logger, logError, logInfo, logWarn)
import Elwood.Metrics (MetricsStore, metricsSource)
import Elwood.Telegram.Client (TelegramClient, downloadFile, getFile, notify, sendDocument, sendPhoto)
import Elwood.Telegram.Types (Chat (..), Message (..), PhotoSize (..), TelegramFile (..))
import Elwood.Tools.Attachment (isPhotoExtension)
import Elwood.Tools.Registry (ToolRegistry)
import Elwood.Tools.Types (AgentContext, Attachment (..), AttachmentType (..))
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
    eeAgentContext :: AgentContext,
    eeCompaction :: CompactionConfig,
    eeSystemPrompt :: Maybe Text,
    eeModel :: Text,
    -- | Extended thinking level
    eeThinking :: ThinkingLevel,
    -- | All allowed chat IDs (for broadcast)
    eeNotifyChatIds :: [Int64],
    -- | Attachment queue
    eeAttachmentQueue :: TVar [Attachment],
    -- | Maximum agent loop iterations per turn
    eeMaxIterations :: Int,
    -- | Metrics store for Prometheus counters
    eeMetrics :: MetricsStore,
    -- | Number of active MCP servers
    eeMCPServerCount :: Int,
    -- | Always-loaded tools for dynamic loading (Nothing = disabled, Just = enabled)
    eeAlwaysLoadTools :: Maybe [Text]
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

  -- Build agent config from environment
  let agentConfig =
        AgentConfig
          { acLogger = logger,
            acClient = eeClaude env,
            acRegistry = eeRegistry env,
            acContext = eeAgentContext env,
            acCompaction = eeCompaction env,
            acSystemPrompt = eeSystemPrompt env,
            acModel = eeModel env,
            acThinking = eeThinking env,
            acMaxIterations = eeMaxIterations env,
            acMetrics = eeMetrics env,
            acSource = metricsSource source,
            acOnRateLimit = Just (mkRateLimitCallback env event),
            acOnText = Just (mkTextCallback env event),
            acAlwaysLoadTools = eeAlwaysLoadTools env
          }

  -- Run the agent turn
  result <-
    runAgentTurn agentConfig history userMsg
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
    attachments <- atomically $ do
      atts <- readTVar (eeAttachmentQueue env)
      writeTVar (eeAttachmentQueue env) []
      pure atts
    mapM_ (\att -> mapM_ (\cid -> sendAttachmentSafe env cid att) chatIds) attachments
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

-- | Create text callback to deliver intermediate text during tool-use turns
mkTextCallback :: AppEnv -> Event -> TextCallback
mkTextCallback env event = deliverToTargets env (evDelivery event)

-- | Create rate limit notification callback based on event delivery targets
mkRateLimitCallback :: AppEnv -> Event -> RateLimitCallback
mkRateLimitCallback env event attemptNum waitSecs = do
  let msg = "rate limited, retry " <> T.pack (show attemptNum) <> " in " <> T.pack (show waitSecs) <> "s)"
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

-- | Handle a Telegram message: commands, image fetching, and event dispatch.
--
-- Returns 'Nothing' on success (the event system delivers via Telegram),
-- or 'Just errorMsg' on failure (for the polling loop to send).
handleTelegramMessage :: AppEnv -> Message -> IO (Maybe Text)
handleTelegramMessage env msg =
  case (text msg, photo msg) of
    -- Handle /clear command
    (Just txt, _) | T.strip txt == "/clear" -> handleClear
    -- Ignore other slash commands
    (Just txt, _) | T.isPrefixOf "/" txt -> pure Nothing
    -- Handle text with optional photo
    (Just txt, photos) -> handleMessageWithPhoto txt photos
    -- Handle photo with caption only
    (Nothing, Just photos@(_ : _)) -> handleMessageWithPhoto (fromMaybe "" (caption msg)) (Just photos)
    -- No text and no photos (or empty photo list)
    (Nothing, _) -> pure Nothing
  where
    logger :: Logger
    logger = eeLogger env

    chatIdVal :: Int64
    chatIdVal = chatId (chat msg)

    handleClear :: IO (Maybe Text)
    handleClear = do
      clearConversation (eeConversations env) (T.pack (show chatIdVal))
      logInfo logger "Conversation cleared" [("chat_id", T.pack (show chatIdVal))]
      pure (Just "Conversation cleared. Starting fresh!")

    handleMessageWithPhoto :: Text -> Maybe [PhotoSize] -> IO (Maybe Text)
    handleMessageWithPhoto userText maybePhotos = do
      logInfo
        logger
        "Processing message"
        [ ("chat_id", T.pack (show chatIdVal)),
          ("text_length", T.pack (show (T.length userText))),
          ("has_photo", T.pack (show (maybe False (not . null) maybePhotos)))
        ]

      -- Fetch image if present
      imageData <- case maybePhotos of
        Just photos@(_ : _) -> fetchImageData photos
        _ -> pure Nothing

      -- If no content, skip
      if T.null userText && isNothing imageData
        then pure Nothing
        else do
          -- Create Telegram event
          now <- getCurrentTime
          let event =
                Event
                  { evSource = TelegramSource chatIdVal,
                    evTimestamp = now,
                    evPayload = Null,
                    evPrompt = userText,
                    evImage = imageData,
                    evSession = Named (T.pack (show chatIdVal)),
                    evDelivery = [TelegramDelivery (T.pack (show chatIdVal))]
                  }

          -- Handle the event - delivery to Telegram is done by the event system
          result <- handleEvent env event

          case result of
            Right responseText -> do
              logInfo
                logger
                "Agent turn completed"
                [ ("chat_id", T.pack (show chatIdVal)),
                  ("response_length", T.pack (show (T.length responseText)))
                ]
              -- Return Nothing: the event system already delivered to Telegram
              pure Nothing
            Left errorMsg -> do
              logInfo
                logger
                "Agent turn failed"
                [ ("chat_id", T.pack (show chatIdVal)),
                  ("error", errorMsg)
                ]
              -- Errors are still returned for the polling loop to send,
              -- since the event system only delivers on success
              pure (Just errorMsg)

    -- Fetch the largest photo and return (mediaType, base64Data)
    fetchImageData :: [PhotoSize] -> IO (Maybe (Text, Text))
    fetchImageData photos = do
      let telegram = eeTelegram env
      -- Get the largest photo (sort by file size descending, take first)
      let largestPhoto = listToMaybe $ sortOn (Down . psFileSize) photos
      case largestPhoto of
        Nothing -> pure Nothing
        Just ps -> do
          logInfo
            logger
            "Fetching photo"
            [ ("file_id", psFileId ps),
              ("width", T.pack (show (psWidth ps))),
              ("height", T.pack (show (psHeight ps)))
            ]

          -- Get file info from Telegram
          maybeFile <- getFile telegram (psFileId ps)
          case maybeFile of
            Nothing -> do
              logWarn logger "Failed to get file info" [("file_id", psFileId ps)]
              pure Nothing
            Just file -> case tfFilePath file of
              Nothing -> do
                logWarn logger "No file path in response" [("file_id", psFileId ps)]
                pure Nothing
              Just filePath -> do
                -- Download the file
                rawImageData <-
                  downloadFile telegram filePath
                    `catch` \(e :: SomeException) -> do
                      logWarn logger "Failed to download file" [("error", T.pack (show e))]
                      pure LBS.empty

                if LBS.null rawImageData
                  then pure Nothing
                  else do
                    -- Determine media type from file extension
                    let mediaType = guessMediaType filePath
                        base64Data = TE.decodeUtf8 $ LBS.toStrict $ B64.encode rawImageData
                    logInfo
                      logger
                      "Photo downloaded and encoded"
                      [ ("media_type", mediaType),
                        ("size_bytes", T.pack (show (LBS.length rawImageData)))
                      ]
                    pure $ Just (mediaType, base64Data)

    -- Guess media type from file path
    guessMediaType :: Text -> Text
    guessMediaType path
      | T.isSuffixOf ".jpg" path || T.isSuffixOf ".jpeg" path = "image/jpeg"
      | T.isSuffixOf ".png" path = "image/png"
      | T.isSuffixOf ".gif" path = "image/gif"
      | T.isSuffixOf ".webp" path = "image/webp"
      | otherwise = "image/jpeg" -- Default to JPEG for Telegram photos

-- | Format event source for logging
formatSource :: EventSource -> Text
formatSource (WebhookSource name) = "webhook:" <> name
formatSource (CronSource name) = "cron:" <> name
formatSource (TelegramSource chatId) = "telegram:" <> T.pack (show chatId)
