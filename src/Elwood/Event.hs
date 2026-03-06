module Elwood.Event
  ( -- * Event Types
    Event (..),

    -- * Re-exports from Event.Types
    EventSource (..),
    SessionConfig (..),
    DeliveryTarget (..),
    ImageData (..),
    MediaType (..),
    Base64Data (..),

    -- * Event Environment
    AppEnv (..),

    -- * Event Handling
    handleEvent,
    handleEventBuffered,
    BufferedResult (..),

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
    sendAttachmentSafe,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Exception (SomeException, catch)
import Control.Monad (unless)
import Data.Aeson (Value (..))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Elwood.AgentSettings (AgentPreset, AgentSettings (..))
import Elwood.Claude qualified as Claude
import Elwood.Claude.Pruning (PruneHorizons, getAndUpdateHorizon)
import Elwood.Claude.Types (cacheTtlSeconds)
import Elwood.Config (CompactionConfig, PruningConfig, TelegramChatConfig (..))
import Elwood.Event.Types
  ( Base64Data (..),
    DeliveryTarget (..),
    EventSource (..),
    ImageData (..),
    MediaType (..),
    SessionConfig (..),
  )
import Elwood.Logging (Logger, logError, logInfo)
import Elwood.Metrics (MetricsStore, metricsObserver, metricsSource)
import Elwood.Notify (Severity (..), escapeUnderscores, formatNotify, sanitizeBackticks)
import Elwood.Prompt (PromptInput, assemblePrompt)
import Elwood.Session (SessionLocks, withSessionLock)
import Elwood.Telegram qualified as Telegram
import Elwood.Tools qualified as Tools
import Elwood.Tools.Attachment (isPhotoExtension)

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
    -- | Optional image data
    image :: Maybe ImageData,
    -- | Session configuration
    session :: SessionConfig,
    -- | Where to deliver responses
    deliveryTarget :: DeliveryTarget
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
    -- | Tool result pruning configuration
    pruning :: PruningConfig,
    -- | System prompt inputs (resolved per-request from workspace files)
    systemPromptInputs :: [PromptInput],
    -- | Directory containing workspace files
    workspaceDir :: FilePath,
    -- | Resolved agent settings (model, thinking, maxIterations)
    agentSettings :: AgentSettings,
    -- | Telegram chat ID to full chat config (source of truth for allowed chats)
    telegramChatMap :: Map Int64 TelegramChatConfig,
    -- | Attachment queue
    attachmentQueue :: TVar [Tools.Attachment],
    -- | Metrics store for Prometheus counters
    metrics :: MetricsStore,
    -- | Tool search (Nothing = disabled, Just neverDefer = enabled with deferred loading)
    toolSearch :: Maybe (Set Claude.ToolName),
    -- | Per-session prune horizons for tool result pruning
    pruneHorizons :: PruneHorizons,
    -- | Per-session locks for serializing concurrent event handling
    sessionLocks :: SessionLocks,
    -- | Send notification messages when the agent uses tools
    toolUseMessages :: Bool,
    -- | Default delegate sub-agent preset (overrides + optional description)
    delegateDefaultAgent :: AgentPreset,
    -- | Named agent presets for delegate_task
    delegateExtraAgents :: Map Text AgentPreset,
    -- | Allowed models for delegate_task tool parameter
    delegateAllowedModels :: [Text],
    -- | Maximum image dimension for resizing (Nothing = disabled)
    maxImageDimension :: Maybe Int
  }

-- | Callbacks wired into the agent loop for delivery during a turn
data DeliveryCallbacks = DeliveryCallbacks
  { onText :: Maybe Claude.TextCallback,
    onToolUse :: Maybe Claude.ToolUseCallback,
    onRateLimit :: Maybe Claude.RateLimitCallback,
    onBeforeApiCall :: Maybe (IO ()),
    onResponse :: Text -> IO ()
  }

-- | Handle any event through the agent pipeline
--
-- Returns Either error message or success response text
handleEvent :: AppEnv -> Event -> IO (Either Text Text)
handleEvent env event =
  withSessionLockIfNamed env event $
    handleEventCore env event (eagerCallbacks env event)

-- | Construct eager (immediate-delivery) callbacks from an event's delivery targets
eagerCallbacks :: AppEnv -> Event -> DeliveryCallbacks
eagerCallbacks env event =
  DeliveryCallbacks
    { onText = Just (mkTextCallback env event),
      onToolUse = if env.toolUseMessages then Just (mkToolUseCallback env event) else Nothing,
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
      ("delivery_target", T.pack (show event.deliveryTarget))
    ]

  -- Determine conversation ID from session config
  let mConversationId = sessionToConversationId event.session

  -- Get existing conversation (empty for Isolated)
  now <- getCurrentTime
  (history, pruneHorizon) <- case mConversationId of
    Nothing -> pure ([], 0)
    Just cid -> do
      conv <- env.conversations.getConversation cid
      let cacheExpired = diffUTCTime now conv.lastUpdated > cacheTtlSeconds env.agentSettings.cacheTtl
      h <- getAndUpdateHorizon env.pruneHorizons cid (length conv.messages) cacheExpired
      pure (conv.messages, h)

  -- Assemble system prompt from workspace files (re-read each request)
  systemPrompt <- assemblePrompt env.workspaceDir env.systemPromptInputs

  -- Build user message with optional image
  let contentBlocks = case event.image of
        Just img ->
          [Claude.ImageBlock img.mediaType.unMediaType img.base64Data.unBase64Data, Claude.TextBlock event.prompt]
        Nothing ->
          [Claude.TextBlock event.prompt]
      userMsg = Claude.ClaudeMessage Claude.User contentBlocks

  -- Build registry with delegate tool (base registry has no delegate_task,
  -- preventing recursive nesting)
  let delegateTool =
        Tools.mkDelegateTaskTool
          lgr
          env.claude
          env.registry
          env.agentContext
          env.agentSettings
          env.compaction
          env.pruning
          systemPrompt
          env.metrics
          env.delegateDefaultAgent
          env.delegateExtraAgents
          env.delegateAllowedModels
      registryWithDelegate = Tools.registerTool delegateTool env.registry

  -- Build agent config from environment
  let agentConfig =
        Claude.AgentConfig
          { logger = lgr,
            client = env.claude,
            registry = registryWithDelegate,
            context = env.agentContext,
            compaction = env.compaction,
            systemPrompt = systemPrompt,
            agentSettings = env.agentSettings,
            observer = metricsObserver env.metrics env.agentSettings.model (metricsSource src),
            onRateLimit = callbacks.onRateLimit,
            onText = callbacks.onText,
            onToolUse = callbacks.onToolUse,
            onBeforeApiCall = callbacks.onBeforeApiCall,
            toolSearch = env.toolSearch,
            pruningConfig = env.pruning,
            pruneHorizon = pruneHorizon,
            outputFormat = Nothing
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
        Just cid -> env.conversations.updateConversation cid allMessages

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

-- | Acquire the per-session lock for named sessions; run directly for isolated ones.
withSessionLockIfNamed :: AppEnv -> Event -> IO a -> IO a
withSessionLockIfNamed env event action =
  case sessionToConversationId event.session of
    Nothing -> action
    Just sid -> withSessionLock env.sessionLocks sid action

-- | Delay in microseconds between typing indicator and message delivery
-- during buffered flush (500ms)
flushPacingDelay :: Int
flushPacingDelay = 500000

-- | Buffered output item captured during an agent turn
data BufferedItem
  = -- | Intermediate text with any attachments queued before it
    BufferedText Text [Tools.Attachment]
  | BufferedRateLimit Text

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
  AppEnv -> Event -> DeliveryTarget -> IO BufferedResult
handleEventBuffered env event targets = do
  bufRef <- newIORef ([] :: [BufferedItem])
  let callbacks =
        DeliveryCallbacks
          { onText = Just $ \t -> do
              -- Drain any attachments queued since the last text callback
              atts <- atomically $ do
                a <- readTVar env.attachmentQueue
                writeTVar env.attachmentQueue []
                pure a
              modifyIORef' bufRef (BufferedText t atts :),
            onToolUse =
              if env.toolUseMessages
                then Just $ \names -> do
                  let m = formatToolUseMessage names
                  modifyIORef' bufRef (BufferedText m [] :)
                else Nothing,
            onRateLimit =
              Just
                ( \n s -> do
                    let m = formatNotify Warn $ "Rate limited, retry " <> T.pack (show n) <> " in " <> T.pack (show s) <> "s"
                    modifyIORef' bufRef (BufferedRateLimit m :)
                ),
            onBeforeApiCall = Nothing,
            onResponse = \_ -> pure ()
          }
  result <- withSessionLockIfNamed env event $ handleEventCore env event callbacks
  case result of
    Left err -> do
      drainAttachmentQueue env
      pure (BufferedError err)
    Right rsp -> do
      buf <- readIORef bufRef
      pure $ BufferedSuccess rsp (flushBuffer env targets (reverse buf) rsp)

-- | Replay buffered items with typing indicators and pacing, then deliver
-- the final response (including attachment drain).
flushBuffer :: AppEnv -> DeliveryTarget -> [BufferedItem] -> Text -> IO ()
flushBuffer env target items finalResponse = do
  let chatIds = targetChatIds env target
  mapM_ (flushItem chatIds) items
  -- Final response: typing → delay → deliver with attachments
  sendTypingToTargets env target
  threadDelay flushPacingDelay
  deliverToTargets env target finalResponse
  where
    flushItem :: [Int64] -> BufferedItem -> IO ()
    flushItem chatIds (BufferedText t atts) = do
      sendTypingToTargets env target
      threadDelay flushPacingDelay
      deliverTextOnly env target t
      -- Deliver attachments that were queued before this text
      mapM_ (\att -> mapM_ (\cid -> sendAttachmentSafe env cid att) chatIds) atts
    flushItem _ (BufferedRateLimit m) = do
      deliverTextOnly env target m

-- | Drain the attachment queue, discarding all queued attachments
drainAttachmentQueue :: AppEnv -> IO ()
drainAttachmentQueue env =
  atomically $ writeTVar env.attachmentQueue []

-- | Deliver response to configured targets, then send queued attachments
deliverResponse :: AppEnv -> Event -> Text -> IO ()
deliverResponse env event = deliverToTargets env event.deliveryTarget

-- | Deliver text to the specified target without draining the attachment queue
deliverTextOnly :: AppEnv -> DeliveryTarget -> Text -> IO ()
deliverTextOnly env target msg = case target of
  TelegramDelivery chatIds ->
    mapM_ (\cid -> notifySafe env cid msg) chatIds
  TelegramBroadcast ->
    mapM_ (\cid -> notifySafe env cid msg) (Map.keys env.telegramChatMap)
  LogOnly ->
    logInfo env.logger "Event response (log only)" [("response", T.take 100 msg)]

-- | Deliver a message to the specified target, then send queued attachments
deliverToTargets :: AppEnv -> DeliveryTarget -> Text -> IO ()
deliverToTargets env target msg = do
  deliverTextOnly env target msg
  -- Send queued attachments to Telegram targets (if any)
  -- Only drain the queue when there are real Telegram targets; for LogOnly
  -- the polling path in App.hs handles attachment delivery separately.
  let chatIds = targetChatIds env target
  unless (null chatIds) $ do
    attachments <- atomically $ do
      atts <- readTVar env.attachmentQueue
      writeTVar env.attachmentQueue []
      pure atts
    mapM_ (\att -> mapM_ (\cid -> sendAttachmentSafe env cid att) chatIds) attachments

-- | Resolve a delivery target to its Telegram chat IDs
targetChatIds :: AppEnv -> DeliveryTarget -> [Int64]
targetChatIds _ (TelegramDelivery chatIds) = NE.toList chatIds
targetChatIds env TelegramBroadcast = Map.keys env.telegramChatMap
targetChatIds _ LogOnly = []

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
mkTextCallback env event = deliverToTargets env event.deliveryTarget

-- | Create rate limit notification callback based on event delivery targets
mkRateLimitCallback :: AppEnv -> Event -> Claude.RateLimitCallback
mkRateLimitCallback env event attemptNum waitSecs = do
  let msg = formatNotify Warn $ "Rate limited, retry " <> T.pack (show attemptNum) <> " in " <> T.pack (show waitSecs) <> "s"
  -- Notify based on delivery target
  case event.deliveryTarget of
    TelegramDelivery chatIds ->
      mapM_ (\cid -> notifySafe env cid msg) chatIds
    TelegramBroadcast ->
      mapM_ (\cid -> notifySafe env cid msg) (Map.keys env.telegramChatMap)
    LogOnly ->
      logInfo env.logger "Rate limited" [("attempt", T.pack (show attemptNum)), ("wait_seconds", T.pack (show waitSecs))]

-- | Format a tool use notification message
formatToolUseMessage :: [Text] -> Text
formatToolUseMessage [] = ""
formatToolUseMessage (first : rest)
  | length rest < 5 = "\128295 _Using " <> escapeUnderscores (T.intercalate ", " (first : rest)) <> "_"
  | otherwise = "\128295 _Using " <> escapeUnderscores first <> " + " <> T.pack (show (length rest)) <> " others_"

-- | Create tool use notification callback based on event delivery targets
mkToolUseCallback :: AppEnv -> Event -> Claude.ToolUseCallback
mkToolUseCallback env event names = do
  let msg = formatToolUseMessage names
  case event.deliveryTarget of
    TelegramDelivery chatIds ->
      mapM_ (\cid -> notifySafe env cid msg) chatIds
    TelegramBroadcast ->
      mapM_ (\cid -> notifySafe env cid msg) (Map.keys env.telegramChatMap)
    LogOnly ->
      logInfo env.logger "Tool use" [("tools", T.intercalate ", " names)]

-- | Send typing indicator to a delivery target
sendTypingToTargets :: AppEnv -> DeliveryTarget -> IO ()
sendTypingToTargets env target = case target of
  TelegramDelivery chatIds ->
    mapM_ (Telegram.sendChatAction env.telegram) chatIds
  TelegramBroadcast ->
    mapM_ (Telegram.sendChatAction env.telegram) (Map.keys env.telegramChatMap)
  LogOnly -> pure ()

-- | Create before-API-call callback to send typing indicators
mkBeforeApiCallCallback :: AppEnv -> Event -> IO ()
mkBeforeApiCallCallback env event =
  sendTypingToTargets env event.deliveryTarget

-- | Format event source for logging
formatSource :: EventSource -> Text
formatSource (WebhookSource n) = "webhook:" <> n
formatSource (TelegramSource chatId_) = "telegram:" <> T.pack (show chatId_)
