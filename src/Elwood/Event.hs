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
import Data.List (foldl', sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Elwood.AgentSettings (AgentSettings (..))
import Elwood.Claude qualified as Claude
import Elwood.Claude.Compaction qualified as Compaction
import Elwood.Claude.Pruning (PruneHorizons, anthropicCacheTtl, getAndUpdateHorizon)
import Elwood.Command qualified as Cmd
import Elwood.Config (CompactionConfig, PruningConfig, TelegramChatConfig (..))
import Elwood.Event.Types
  ( DeliveryTarget (..),
    EventSource (..),
    SessionConfig (..),
  )
import Elwood.Logging (Logger, logError, logInfo, logWarn)
import Elwood.Metrics (MetricsStore, estimateJsonTokens, estimateTextTokens, metricsObserver, metricsSource, recordApiResponse, recordCompaction)
import Elwood.Notify (Severity (..), escapeUnderscores, formatNotify, sanitizeBackticks)
import Elwood.Prompt (PromptInput, assemblePrompt)
import Elwood.Session (SessionLocks, withSessionLock)
import Elwood.Telegram qualified as Telegram
import Elwood.Tools qualified as Tools
import Elwood.Tools.Attachment (isPhotoExtension)
import System.Exit (ExitCode (..))

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
    toolUseMessages :: Bool
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

-- | Handle a Telegram message: commands, image fetching, and event dispatch.
--
-- Returns 'Nothing' on success (the event system delivers via Telegram),
-- or 'Just errorMsg' on failure (for the polling loop to send).
handleTelegramMessage :: AppEnv -> Telegram.Message -> IO (Maybe Text)
handleTelegramMessage env msg =
  case (msg.text, msg.photo) of
    -- Handle /clear command
    (Just txt, _) | T.strip txt == "/clear" -> handleClear
    -- Handle /compact command
    (Just txt, _) | T.strip txt == "/compact" -> handleCompact
    -- Handle /context command
    (Just txt, _) | T.strip txt == "/context" -> handleContext
    -- Handle /run <cmd>
    (Just txt, _) | Just cmd <- T.stripPrefix "/run " (T.strip txt), not (T.null (T.strip cmd)) -> handleRun (T.strip cmd)
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

    chatConfig :: Maybe TelegramChatConfig
    chatConfig = Map.lookup chatIdVal env.telegramChatMap

    chatSession :: SessionConfig
    chatSession = maybe Isolated (.session) chatConfig

    handleClear :: IO (Maybe Text)
    handleClear = do
      case sessionToConversationId chatSession of
        Nothing -> do
          logInfo lgr "Clear requested for isolated chat" [("chat_id", T.pack (show chatIdVal))]
          pure (Just $ formatNotify Info "This chat has no persistent session to clear")
        Just cid -> do
          env.conversations.clearConversation cid
          logInfo lgr "Conversation cleared" [("chat_id", T.pack (show chatIdVal)), ("session", cid)]
          pure (Just $ formatNotify Info "Conversation cleared")

    handleCompact :: IO (Maybe Text)
    handleCompact = do
      case sessionToConversationId chatSession of
        Nothing -> do
          logInfo lgr "Compact requested for isolated chat" [("chat_id", T.pack (show chatIdVal))]
          pure (Just $ formatNotify Info "This chat has no persistent session to compact")
        Just cid -> withSessionLock env.sessionLocks cid $ do
          conv <- env.conversations.getConversation cid
          let msgs = conv.messages
              msgCount = length msgs
          if null msgs
            then do
              logInfo lgr "Compact requested on empty conversation" [("chat_id", T.pack (show chatIdVal))]
              pure (Just $ formatNotify Info "Conversation is empty, nothing to compact")
            else
              if msgCount < 4
                then do
                  logInfo lgr "Compact requested on short conversation" [("chat_id", T.pack (show chatIdVal)), ("message_count", T.pack (show msgCount))]
                  pure (Just $ formatNotify Info $ "Conversation is too short to compact (" <> T.pack (show msgCount) <> " messages)")
                else do
                  let beforeTokens = Compaction.estimateTokens msgs
                  result <-
                    (Right <$> Compaction.compactMessages lgr env.claude env.compaction (recordCompaction env.metrics) (recordApiResponse env.metrics env.agentSettings.model "telegram") msgs)
                      `catch` \(e :: SomeException) -> do
                        logError lgr "Manual compaction failed" [("chat_id", T.pack (show chatIdVal)), ("error", T.pack (show e))]
                        pure (Left e)
                  case result of
                    Left _ ->
                      pure (Just $ formatNotify Error "Compaction failed")
                    Right compacted -> do
                      env.conversations.updateConversation cid compacted
                      let afterTokens = Compaction.estimateTokens compacted
                          afterCount = length compacted
                      logInfo
                        lgr
                        "Manual compaction complete"
                        [ ("chat_id", T.pack (show chatIdVal)),
                          ("before_tokens", T.pack (show beforeTokens)),
                          ("after_tokens", T.pack (show afterTokens)),
                          ("before_messages", T.pack (show msgCount)),
                          ("after_messages", T.pack (show afterCount))
                        ]
                      pure
                        ( Just $
                            formatNotify Info $
                              "Compaction complete: ~"
                                <> T.pack (show beforeTokens)
                                <> " → ~"
                                <> T.pack (show afterTokens)
                                <> " tokens ("
                                <> T.pack (show msgCount)
                                <> " → "
                                <> T.pack (show afterCount)
                                <> " messages)"
                        )

    handleContext :: IO (Maybe Text)
    handleContext = do
      case sessionToConversationId chatSession of
        Nothing -> do
          logInfo lgr "Context requested for isolated chat" [("chat_id", T.pack (show chatIdVal))]
          pure (Just $ formatNotify Info "This chat has no persistent session")
        Just cid -> do
          conv <- env.conversations.getConversation cid
          let msgs = conv.messages
          if null msgs
            then pure (Just $ formatNotify Info "Conversation is empty")
            else do
              let msgCount = length msgs
                  totalTokens = Compaction.estimateTokens msgs
                  -- Walk all messages counting tokens per category
                  (userTok, assistTok, thinkTok, toolCallTok, toolResultTok) = foldl' countMessage (0, 0, 0, 0, 0) msgs
                  catSum = userTok + assistTok + thinkTok + toolCallTok + toolResultTok
                  pct :: Int64 -> Text
                  pct tok
                    | catSum == 0 = "0%"
                    | otherwise = T.pack (show (tok * 100 `div` catSum)) <> "%"
                  categories =
                    filter
                      (\(_, v) -> v > 0)
                      [ ("User text" :: Text, userTok),
                        ("Assistant text", assistTok),
                        ("Thinking", thinkTok),
                        ("Tool calls", toolCallTok),
                        ("Tool results", toolResultTok)
                      ]
                  header =
                    "Context usage ("
                      <> T.pack (show msgCount)
                      <> " messages, ~"
                      <> formatKTok totalTokens
                      <> " tokens):"
                  catLines = map (\(label, tok) -> "• " <> label <> ": " <> pct tok) categories
              pure (Just $ formatNotify Info $ T.intercalate "\n" (header : catLines))

    countMessage :: (Int64, Int64, Int64, Int64, Int64) -> Claude.ClaudeMessage -> (Int64, Int64, Int64, Int64, Int64)
    countMessage acc msg_ = foldl' (countBlock msg_.role) acc msg_.content

    countBlock :: Claude.Role -> (Int64, Int64, Int64, Int64, Int64) -> Claude.ContentBlock -> (Int64, Int64, Int64, Int64, Int64)
    countBlock Claude.User (u, a, th, tc, tr) (Claude.TextBlock t) = (u + estimateTextTokens t, a, th, tc, tr)
    countBlock Claude.Assistant (u, a, th, tc, tr) (Claude.TextBlock t) = (u, a + estimateTextTokens t, th, tc, tr)
    countBlock _ (u, a, th, tc, tr) (Claude.ThinkingBlock t _) = (u, a, th + estimateTextTokens t, tc, tr)
    countBlock _ (u, a, th, tc, tr) (Claude.RedactedThinkingBlock d) = (u, a, th + estimateTextTokens d, tc, tr)
    countBlock _ (u, a, th, tc, tr) (Claude.ToolUseBlock _ _ input) = (u, a, th, tc + estimateJsonTokens input, tr)
    countBlock _ (u, a, th, tc, tr) (Claude.ServerToolUseBlock _ _ input) = (u, a, th, tc + estimateJsonTokens input, tr)
    countBlock _ (u, a, th, tc, tr) (Claude.ToolResultBlock _ content_ _) = (u, a, th, tc, tr + estimateTextTokens content_)
    countBlock _ (u, a, th, tc, tr) (Claude.ToolSearchResultBlock _ v) = (u, a, th, tc, tr + estimateJsonTokens v)
    countBlock _ acc _ = acc

    formatKTok :: Int -> Text
    formatKTok tokens = T.pack (show (round (fromIntegral tokens / 1e3 :: Double) :: Int)) <> "k"

    handleRun :: Text -> IO (Maybe Text)
    handleRun cmd = do
      logInfo lgr "Running command from Telegram" [("chat_id", T.pack (show chatIdVal)), ("command", cmd)]
      result <- Cmd.runCommandWithTimeout cmd 30 env.workspaceDir
      let prefix = case result.exitCode of
            ExitSuccess -> ""
            ExitFailure code -> "[exit " <> T.pack (show code) <> "] "
      pure (Just $ formatNotify Info $ prefix <> "\n```\n$ " <> cmd <> "\n" <> result.output <> "\n```")

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
                    session = chatSession,
                    deliveryTarget = TelegramDelivery (pure chatIdVal)
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
formatSource (TelegramSource chatId_) = "telegram:" <> T.pack (show chatId_)
