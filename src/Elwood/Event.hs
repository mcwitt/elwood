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
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Elwood.Claude qualified as Claude
import Elwood.Claude.Pruning (PruneHorizons, anthropicCacheTtl, getAndUpdateHorizon)
import Elwood.Config (CompactionConfig, ThinkingLevel)
import Elwood.Event.Types
  ( DeliveryTarget (..),
    EventSource (..),
    SessionConfig (..),
  )
import Elwood.Logging (Logger, logError, logInfo, logWarn)
import Elwood.Metrics (MetricsStore, metricsSource)
import Elwood.Prompt (PromptInput, assemblePrompt)
import Elwood.Telegram qualified as Telegram
import Elwood.Tools qualified as Tools
import Elwood.Tools.Attachment (isPhotoExtension)
import Text.Read (readMaybe)

-- | Unified event type for all sources
data Event = Event
  { -- | Where this event came from
    source :: EventSource,
    -- | When the event occurred
    timestamp :: UTCTime,
    -- | Source-specific JSON data (e.g., webhook payload)
    payload :: Value,
    -- | Rendered prompt for the agent
    prompt :: Text,
    -- | Optional image data (media type, base64 data)
    image :: Maybe (Text, Text),
    -- | Session configuration
    session :: SessionConfig,
    -- | Where to deliver responses
    delivery :: [DeliveryTarget]
  }
  deriving stock (Show)

-- | Environment for event handling
data AppEnv = AppEnv
  { logger :: Logger,
    telegram :: Telegram.TelegramClient,
    claude :: Claude.ClaudeClient,
    conversations :: Claude.ConversationStore,
    registry :: Tools.ToolRegistry,
    agentContext :: Tools.AgentContext,
    compaction :: CompactionConfig,
    -- | System prompt inputs (resolved per-request from workspace files)
    systemPromptInputs :: [PromptInput],
    -- | Directory containing workspace files
    workspaceDir :: FilePath,
    model :: Text,
    -- | Extended thinking level
    thinking :: ThinkingLevel,
    -- | All allowed chat IDs (for broadcast)
    notifyChatIds :: [Int64],
    -- | Attachment queue
    attachmentQueue :: TVar [Tools.Attachment],
    -- | Maximum agent loop iterations per turn
    maxIterations :: Int,
    -- | Metrics store for Prometheus counters
    metrics :: MetricsStore,
    -- | Number of active MCP servers
    mcpServerCount :: Int,
    -- | Tool search (Nothing = disabled, Just neverDefer = enabled with deferred loading)
    toolSearch :: Maybe (Set Text),
    -- | Per-session prune horizons for tool result pruning
    pruneHorizons :: PruneHorizons
  }

-- | Handle any event through the agent pipeline
--
-- Returns Either error message or success response text
handleEvent :: AppEnv -> Event -> IO (Either Text Text)
handleEvent env event = do
  let lgr = env.logger
      src = event.source

  logInfo
    lgr
    "Handling event"
    [ ("source", formatSource src),
      ("session", T.pack (show event.session)),
      ("delivery_targets", T.pack (show (length event.delivery)))
    ]

  -- Determine conversation ID from session config
  let mConversationId = sessionToConversationId event.session

  -- Get existing conversation (empty for Isolated)
  now <- getCurrentTime
  (history, pruneHorizon) <- case mConversationId of
    Nothing -> pure ([], 0)
    Just cid -> do
      conv <- Claude.getConversation env.conversations cid
      let cacheExpired = diffUTCTime now conv.lastUpdated > anthropicCacheTtl
      h <- getAndUpdateHorizon env.pruneHorizons cid (length conv.messages) cacheExpired
      pure (conv.messages, h)

  -- Assemble system prompt from workspace files (re-read each request)
  systemPrompt <- assemblePrompt env.workspaceDir env.systemPromptInputs

  -- Build user message with optional image
  let contentBlocks = case event.image of
        Just (mt, imageData) ->
          [Claude.ImageBlock mt imageData, Claude.TextBlock event.prompt]
        Nothing ->
          [Claude.TextBlock event.prompt]
      userMsg = Claude.ClaudeMessage Claude.User contentBlocks

  -- Build agent config from environment
  let agentConfig =
        Claude.AgentConfig
          { logger = lgr,
            client = env.claude,
            registry = env.registry,
            context = env.agentContext,
            compaction = env.compaction,
            systemPrompt = systemPrompt,
            model = env.model,
            thinking = env.thinking,
            maxIterations = env.maxIterations,
            metrics = env.metrics,
            source = metricsSource src,
            onRateLimit = Just (mkRateLimitCallback env event),
            onText = Just (mkTextCallback env event),
            toolSearch = env.toolSearch,
            pruneHorizon = pruneHorizon
          }

  -- Run the agent turn
  result <-
    Claude.runAgentTurn agentConfig history userMsg
      `catch` \(e :: SomeException) -> do
        logError lgr "Event handler agent error" [("error", T.pack (show e))]
        pure $ Claude.AgentError $ "Agent error: " <> T.pack (show e)

  case result of
    Claude.AgentSuccess responseText allMessages -> do
      -- Update conversation (skip for isolated sessions)
      case mConversationId of
        Nothing -> pure ()
        Just cid -> Claude.updateConversation env.conversations cid allMessages

      -- Deliver response
      deliverResponse env event responseText

      logInfo
        lgr
        "Event handled successfully"
        [ ("source", formatSource src),
          ("response_length", T.pack (show (T.length responseText)))
        ]

      pure (Right responseText)
    Claude.AgentError err -> do
      logError lgr "Event handling failed" [("source", formatSource src), ("error", err)]
      pure (Left err)

-- | Deliver response to configured targets, then send queued attachments
deliverResponse :: AppEnv -> Event -> Text -> IO ()
deliverResponse env event = deliverToTargets env event.delivery

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
      atts <- readTVar env.attachmentQueue
      writeTVar env.attachmentQueue []
      pure atts
    mapM_ (\att -> mapM_ (\cid -> sendAttachmentSafe env cid att) chatIds) attachments
  where
    deliver :: DeliveryTarget -> IO ()
    deliver target = case target of
      TelegramDelivery s ->
        case parseChatId s of
          Just chatId_ -> notifySafe env chatId_ msg
          Nothing -> logError env.logger "Invalid chat ID in TelegramDelivery" [("session", s)]
      TelegramBroadcast ->
        mapM_ (\cid -> notifySafe env cid msg) env.notifyChatIds
      LogOnly ->
        logInfo env.logger "Event response (log only)" [("response", T.take 100 msg)]

-- | Resolve a delivery target to its Telegram chat IDs
targetChatIds :: AppEnv -> DeliveryTarget -> [Int64]
targetChatIds _ (TelegramDelivery s) =
  case parseChatId s of
    Just cid -> [cid]
    Nothing -> []
targetChatIds env TelegramBroadcast = env.notifyChatIds
targetChatIds _ LogOnly = []

-- | Parse a chat ID from text
parseChatId :: Text -> Maybe Int64
parseChatId = readMaybe . T.unpack

-- | Send a single attachment to a chat, choosing photo vs document
sendAttachmentSafe :: AppEnv -> Int64 -> Tools.Attachment -> IO ()
sendAttachmentSafe env chatId_ att = do
  let send = case att.type_ of
        Tools.AttachPhoto -> Telegram.sendPhoto
        Tools.AttachDocument -> Telegram.sendDocument
        Tools.AttachAuto
          | isPhotoExtension att.path -> Telegram.sendPhoto
          | otherwise -> Telegram.sendDocument
  send env.telegram chatId_ att.path att.caption
    `catch` \(e :: SomeException) ->
      logError
        env.logger
        "Failed to send attachment"
        [ ("chat_id", T.pack (show chatId_)),
          ("path", T.pack att.path),
          ("error", T.pack (show e))
        ]

-- | Safely send notification, catching any errors
notifySafe :: AppEnv -> Int64 -> Text -> IO ()
notifySafe env chatId_ msg = do
  Telegram.notify env.logger env.telegram chatId_ msg
    `catch` \(e :: SomeException) ->
      logError env.logger "Failed to send notification" [("chat_id", T.pack (show chatId_)), ("error", T.pack (show e))]

-- | Convert session config to conversation ID
--
-- For Isolated sessions, returns Nothing (no conversation persistence).
-- For Named sessions, returns the session name as the conversation key.
sessionToConversationId :: SessionConfig -> Maybe Text
sessionToConversationId Isolated = Nothing
sessionToConversationId (Named n) = Just n

-- | Create text callback to deliver intermediate text during tool-use turns
mkTextCallback :: AppEnv -> Event -> Claude.TextCallback
mkTextCallback env event = deliverToTargets env event.delivery

-- | Create rate limit notification callback based on event delivery targets
mkRateLimitCallback :: AppEnv -> Event -> Claude.RateLimitCallback
mkRateLimitCallback env event attemptNum waitSecs = do
  let msg = "rate limited, retry " <> T.pack (show attemptNum) <> " in " <> T.pack (show waitSecs) <> "s)"
  -- Notify based on delivery targets
  mapM_ (notifyRateLimit msg) event.delivery
  where
    notifyRateLimit :: Text -> DeliveryTarget -> IO ()
    notifyRateLimit m target = case target of
      TelegramDelivery s ->
        case parseChatId s of
          Just chatId_ -> notifySafe env chatId_ m
          Nothing -> pure ()
      TelegramBroadcast ->
        mapM_ (\cid -> notifySafe env cid m) env.notifyChatIds
      LogOnly ->
        logInfo env.logger "Rate limited" [("attempt", T.pack (show attemptNum)), ("wait_seconds", T.pack (show waitSecs))]

-- | Handle a Telegram message: commands, image fetching, and event dispatch.
--
-- Returns 'Nothing' on success (the event system delivers via Telegram),
-- or 'Just errorMsg' on failure (for the polling loop to send).
handleTelegramMessage :: AppEnv -> Telegram.Message -> IO (Maybe Text)
handleTelegramMessage env msg =
  case (msg.text, msg.photo) of
    -- Handle /clear command
    (Just txt, _) | T.strip txt == "/clear" -> handleClear
    -- Ignore other slash commands
    (Just txt, _) | T.isPrefixOf "/" txt -> pure Nothing
    -- Handle text with optional photo
    (Just txt, photos) -> handleMessageWithPhoto txt photos
    -- Handle photo with caption only
    (Nothing, Just photos@(_ : _)) -> handleMessageWithPhoto (fromMaybe "" msg.caption) (Just photos)
    -- No text and no photos (or empty photo list)
    (Nothing, _) -> pure Nothing
  where
    lgr :: Logger
    lgr = env.logger

    chatIdVal :: Int64
    chatIdVal = msg.chat.id_

    handleClear :: IO (Maybe Text)
    handleClear = do
      Claude.clearConversation env.conversations (T.pack (show chatIdVal))
      logInfo lgr "Conversation cleared" [("chat_id", T.pack (show chatIdVal))]
      pure (Just "Conversation cleared. Starting fresh!")

    handleMessageWithPhoto :: Text -> Maybe [Telegram.PhotoSize] -> IO (Maybe Text)
    handleMessageWithPhoto userText maybePhotos = do
      logInfo
        lgr
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
          let evt =
                Event
                  { source = TelegramSource chatIdVal,
                    timestamp = now,
                    payload = Null,
                    prompt = userText,
                    image = imageData,
                    session = Named (T.pack (show chatIdVal)),
                    delivery = [TelegramDelivery (T.pack (show chatIdVal))]
                  }

          -- Handle the event - delivery to Telegram is done by the event system
          result <- handleEvent env evt

          case result of
            Right responseText -> do
              logInfo
                lgr
                "Agent turn completed"
                [ ("chat_id", T.pack (show chatIdVal)),
                  ("response_length", T.pack (show (T.length responseText)))
                ]
              -- Return Nothing: the event system already delivered to Telegram
              pure Nothing
            Left errorMsg -> do
              logInfo
                lgr
                "Agent turn failed"
                [ ("chat_id", T.pack (show chatIdVal)),
                  ("error", errorMsg)
                ]
              -- Errors are still returned for the polling loop to send,
              -- since the event system only delivers on success
              pure (Just errorMsg)

    -- Fetch the largest photo and return (mediaType, base64Data)
    fetchImageData :: [Telegram.PhotoSize] -> IO (Maybe (Text, Text))
    fetchImageData photos = do
      let tg = env.telegram
      -- Get the largest photo (sort by file size descending, take first)
      let largestPhoto = listToMaybe $ sortOn (Down . (.fileSize)) photos
      case largestPhoto of
        Nothing -> pure Nothing
        Just ps -> do
          logInfo
            lgr
            "Fetching photo"
            [ ("file_id", ps.fileId),
              ("width", T.pack (show ps.width)),
              ("height", T.pack (show ps.height))
            ]

          -- Get file info from Telegram
          maybeFile <- Telegram.getFile tg ps.fileId
          case maybeFile of
            Nothing -> do
              logWarn lgr "Failed to get file info" [("file_id", ps.fileId)]
              pure Nothing
            Just file -> case file.filePath of
              Nothing -> do
                logWarn lgr "No file path in response" [("file_id", ps.fileId)]
                pure Nothing
              Just fp -> do
                -- Download the file
                rawImageData <-
                  Telegram.downloadFile tg fp
                    `catch` \(e :: SomeException) -> do
                      logWarn lgr "Failed to download file" [("error", T.pack (show e))]
                      pure LBS.empty

                if LBS.null rawImageData
                  then pure Nothing
                  else do
                    -- Determine media type from file extension
                    let mt = guessMediaType fp
                        base64Data = TE.decodeUtf8 $ LBS.toStrict $ B64.encode rawImageData
                    logInfo
                      lgr
                      "Photo downloaded and encoded"
                      [ ("media_type", mt),
                        ("size_bytes", T.pack (show (LBS.length rawImageData)))
                      ]
                    pure $ Just (mt, base64Data)

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
formatSource (WebhookSource n) = "webhook:" <> n
formatSource (CronSource n) = "cron:" <> n
formatSource (TelegramSource chatId_) = "telegram:" <> T.pack (show chatId_)
