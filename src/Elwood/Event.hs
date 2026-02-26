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
    handleEventBuffered,
    BufferedResult (..),
    handleTelegramMessage,

    -- * Core Pipeline
    DeliveryCallbacks (..),
    handleEventCore,

    -- * Delivery
    deliverToTargets,
    deliverTextOnly,
    sendTypingToTargets,

    -- * Session ID Utilities
    sessionToConversationId,

    -- * Utilities
    parseChatId,
    sendAttachmentSafe,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Exception (SomeException, catch)
import Control.Monad (unless)
import Data.Aeson (Value (..))
import Data.ByteString.Base64.Lazy qualified as B64
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (modifyIORef', newIORef, readIORef)
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
import Elwood.Notify (Severity (..), formatNotify, sanitizeBackticks)
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
    -- | Tool search (Nothing = disabled, Just neverDefer = enabled with deferred loading)
    toolSearch :: Maybe (Set Claude.ToolName),
    -- | Per-session prune horizons for tool result pruning
    pruneHorizons :: PruneHorizons
  }

-- | Callbacks wired into the agent loop for delivery during a turn
data DeliveryCallbacks = DeliveryCallbacks
  { onText :: Maybe Claude.TextCallback,
    onRateLimit :: Maybe Claude.RateLimitCallback,
    onBeforeApiCall :: Maybe (IO ()),
    onResponse :: Text -> IO ()
  }

-- | Handle any event through the agent pipeline
--
-- Returns Either error message or success response text
handleEvent :: AppEnv -> Event -> IO (Either Text Text)
handleEvent env event = handleEventCore env event (eagerCallbacks env event)

-- | Construct eager (immediate-delivery) callbacks from an event's delivery targets
eagerCallbacks :: AppEnv -> Event -> DeliveryCallbacks
eagerCallbacks env event =
  DeliveryCallbacks
    { onText = Just (mkTextCallback env event),
      onRateLimit = Just (mkRateLimitCallback env event),
      onBeforeApiCall = Just (mkBeforeApiCallCallback env event),
      onResponse = deliverResponse env event
    }

-- | Core event handler parameterised by delivery callbacks
handleEventCore :: AppEnv -> Event -> DeliveryCallbacks -> IO (Either Text Text)
handleEventCore env event callbacks = do
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
            onRateLimit = callbacks.onRateLimit,
            onText = callbacks.onText,
            onBeforeApiCall = callbacks.onBeforeApiCall,
            toolSearch = env.toolSearch,
            pruneHorizon = pruneHorizon
          }

  -- Run the agent turn
  result <-
    Claude.runAgentTurn agentConfig history userMsg
      `catch` \(e :: SomeException) -> do
        logError lgr "Event handler agent error" [("error", T.pack (show e))]
        pure $ Claude.AgentError $ formatNotify Error $ "**Agent error:** `" <> sanitizeBackticks (T.pack (show e)) <> "`"

  case result of
    Claude.AgentSuccess responseText allMessages -> do
      -- Update conversation (skip for isolated sessions)
      case mConversationId of
        Nothing -> pure ()
        Just cid -> Claude.updateConversation env.conversations cid allMessages

      -- Deliver response
      callbacks.onResponse responseText

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

-- | Buffered output item captured during an agent turn
data BufferedItem = BufferedText Text | BufferedRateLimit Text

-- | Result of a buffered event handler run
data BufferedResult
  = -- | Agent turn failed
    BufferedError Text
  | -- | Agent turn succeeded with response text and flush action.
    -- Call the flush action to deliver all buffered output.
    BufferedSuccess
      -- | Response text
      Text
      -- | Flush action: replays buffered output then delivers final response
      (IO ())

-- | Handle an event with buffered delivery
--
-- All intermediate text and rate-limit messages are captured in a buffer.
-- On success, returns a 'BufferedSuccess' whose 'flush' action replays the
-- buffer (with typing indicators and pacing) then delivers the final
-- response including attachments. On error, the buffer is discarded.
handleEventBuffered ::
  AppEnv -> Event -> [DeliveryTarget] -> IO BufferedResult
handleEventBuffered env event targets = do
  bufRef <- newIORef ([] :: [BufferedItem])
  let callbacks =
        DeliveryCallbacks
          { onText = Just (\t -> modifyIORef' bufRef (BufferedText t :)),
            onRateLimit =
              Just
                ( \n s -> do
                    let m = formatNotify Warn $ "Rate limited, retry " <> T.pack (show n) <> " in " <> T.pack (show s) <> "s"
                    modifyIORef' bufRef (BufferedRateLimit m :)
                ),
            onBeforeApiCall = Nothing,
            onResponse = \_ -> pure ()
          }
  result <- handleEventCore env event callbacks
  case result of
    Left err -> do
      drainAttachmentQueue env
      pure (BufferedError err)
    Right rsp -> do
      buf <- readIORef bufRef
      pure $ BufferedSuccess rsp (flushBuffer env targets (reverse buf) rsp)

-- | Replay buffered items with typing indicators and pacing, then deliver
-- the final response (including attachment drain).
flushBuffer :: AppEnv -> [DeliveryTarget] -> [BufferedItem] -> Text -> IO ()
flushBuffer env targets items finalResponse = do
  mapM_ flushItem items
  -- Final response: typing → delay → deliver with attachments
  sendTypingToTargets env targets
  threadDelay 500000
  deliverToTargets env targets finalResponse
  where
    flushItem :: BufferedItem -> IO ()
    flushItem (BufferedText t) = do
      sendTypingToTargets env targets
      threadDelay 500000
      deliverTextOnly env targets t
    flushItem (BufferedRateLimit m) = do
      deliverTextOnly env targets m

-- | Drain the attachment queue, discarding all queued attachments
drainAttachmentQueue :: AppEnv -> IO ()
drainAttachmentQueue env =
  atomically $ writeTVar env.attachmentQueue []

-- | Deliver response to configured targets, then send queued attachments
deliverResponse :: AppEnv -> Event -> Text -> IO ()
deliverResponse env event = deliverToTargets env event.delivery

-- | Deliver text to the specified targets without draining the attachment queue
deliverTextOnly :: AppEnv -> [DeliveryTarget] -> Text -> IO ()
deliverTextOnly env targets msg =
  mapM_ deliver targets
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

-- | Deliver a message to the specified targets, then send queued attachments
deliverToTargets :: AppEnv -> [DeliveryTarget] -> Text -> IO ()
deliverToTargets env targets msg = do
  deliverTextOnly env targets msg
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
  let msg = formatNotify Warn $ "Rate limited, retry " <> T.pack (show attemptNum) <> " in " <> T.pack (show waitSecs) <> "s"
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

-- | Send typing indicator to a list of delivery targets
sendTypingToTargets :: AppEnv -> [DeliveryTarget] -> IO ()
sendTypingToTargets env =
  mapM_ sendTyping
  where
    sendTyping :: DeliveryTarget -> IO ()
    sendTyping (TelegramDelivery s) =
      case parseChatId s of
        Just chatId_ -> Telegram.sendChatAction env.telegram chatId_
        Nothing -> pure ()
    sendTyping TelegramBroadcast =
      mapM_ (Telegram.sendChatAction env.telegram) env.notifyChatIds
    sendTyping LogOnly = pure ()

-- | Create before-API-call callback to send typing indicators
mkBeforeApiCallCallback :: AppEnv -> Event -> IO ()
mkBeforeApiCallCallback env event =
  sendTypingToTargets env event.delivery

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
      pure (Just $ formatNotify Info "Conversation cleared")

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
