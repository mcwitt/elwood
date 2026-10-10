module Elwood.Event
  ( Event (..),
    AppEnv (..),
    DeliveryPolicy (..),
    Runner (..),
    eventRunner,
    handleEvent,
    handleEventWith,
    lookupToolUseMessages,
    sessionToConversationId,
    attachPromptNote,
  )
where

import Control.Concurrent.STM
import Control.Monad (filterM, void, when)
import Data.Aeson (Value)
import Data.Either (lefts)
import Data.Int (Int64)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Elwood.AgentSettings (AgentOverrides, AgentPreset, AgentProfile (..), ModelRef (..), ToolSearchConfig (..), resolveProfile, toOverrides)
import Elwood.Claude qualified as Claude
import Elwood.Claude.Compaction (compactIfNeeded)
import Elwood.Claude.Pruning (PruneHorizons, getAndUpdateHorizon)
import Elwood.Config (CompactionConfig, PruningConfig, TelegramChatConfig (..))
import Elwood.Event.Types
import Elwood.Exception (catchSync)
import Elwood.Logging (Logger, logError, logInfo)
import Elwood.Metrics (MetricsStore, metricsObserver, metricsSource)
import Elwood.Prompt (assemblePrompt)
import Elwood.Scheduler (CallbackStore)
import Elwood.Session (SessionLocks, sessionCancelFlag, withSessionLock)
import Elwood.Telegram qualified as Telegram
import Elwood.Telegram.ToolUse (ToolUseNote (..), formatToolUseNote)
import Elwood.Tools qualified as Tools
import Elwood.Tools.AsyncTask (forTurn, scopeTasks)
import Elwood.Tools.Attachment (isPhotoExtension)

data Event = Event
  { -- | Where this event came from
    source :: EventSource,
    -- | When the event occurred
    timestamp :: UTCTime,
    -- | Source-specific JSON data (e.g., webhook payload)
    payload :: Value,
    -- | Rendered prompt for the agent
    prompt :: Text,
    -- | Optional image data (largest inbound photo, for perception)
    image :: Maybe ImageData,
    -- | Attachments archived to disk for this event ([] when none)
    attachments :: [SavedAttachment],
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
    requestApproval :: Tools.ApprovalFunction,
    compaction :: Maybe CompactionConfig,
    -- | Tool result pruning configuration (Nothing = disabled)
    pruning :: Maybe PruningConfig,
    -- | Directory containing workspace files
    workspace :: FilePath,
    -- | Resolved agent profile (model, thinking, permissions, system prompt, tool search, etc.)
    agentProfile :: AgentProfile,
    -- | Telegram chat ID to full chat config (source of truth for allowed chats)
    telegramChatMap :: Map Int64 TelegramChatConfig,
    -- | Metrics store for Prometheus counters
    metrics :: MetricsStore,
    -- | Per-session prune horizons for tool result pruning
    pruneHorizons :: PruneHorizons,
    -- | Per-session locks for serializing concurrent event handling
    sessionLocks :: SessionLocks,
    -- | Send notification messages when the agent uses tools
    toolUseMessages :: Bool,
    -- | Per-Telegram-chat overrides for 'toolUseMessages'.
    -- Sparse: a missing chat falls back to the global default above.
    -- Set at runtime by the @/tools@ slash command; not persisted across restarts.
    toolUseMessagesOverrides :: TVar (Map Int64 Bool),
    -- | Delegate sub-agent preset (overrides + optional description)
    delegateAgent :: AgentPreset,
    -- | Named agent presets for delegate_task
    delegateExtraAgents :: Map Text AgentPreset,
    -- | Allowed models for delegate_task tool parameter
    delegateAllowedModels :: [Text],
    -- | Maximum image dimension for resizing (Nothing = disabled)
    maxImageDimension :: Maybe Int,
    -- | Async task store for delegate_task async mode
    asyncTaskStore :: Tools.AsyncTaskStore,
    -- | Store of pending scheduled callbacks (one-shot self-wakeups)
    callbackStore :: CallbackStore
  }

-- Adapters submit resolved input through this handle; only the composition
-- boundary owns application resources and profile resolution.
newtype Runner = Runner {run :: AgentOverrides -> DeliveryPolicy -> Event -> IO (Either Text Text)}

eventRunner :: AppEnv -> Runner
eventRunner env = Runner $ \overrides ->
  handleEventWith
    env {agentProfile = resolveProfile (toOverrides env.agentProfile <> overrides)}

data DeliveryPolicy = Immediate | SuppressContaining Text
  deriving stock (Eq, Show)

data Output = Narration Text [Tools.Attachment] | ToolNotice (Maybe Text) [(Text, Value)] | RateLimit Text

handleEvent :: AppEnv -> Event -> IO (Either Text Text)
handleEvent env = handleEventWith env Immediate

handleEventWith :: AppEnv -> DeliveryPolicy -> Event -> IO (Either Text Text)
handleEventWith env policy event = lock $ do
  attachments <- newTVarIO []
  pending <- newTVarIO []
  let capture text = do
        output <- atomically $ do
          queued <- readTVar attachments
          writeTVar attachments []
          pure (Narration text queued)
        emit output
      emit output = case policy of
        Immediate -> deliver env event.deliveryTarget output
        SuppressContaining _ -> atomically (modifyTVar' pending (output :))
      finish text messages = do
        mapM_ (\cid -> env.conversations.appendMessages cid messages env.agentProfile.cache) session
        capture text
        case policy of
          SuppressContaining pattern_
            | not (pattern_ `T.isInfixOf` text) ->
                readTVarIO pending >>= mapM_ (deliver env event.deliveryTarget) . reverse
          _ -> pure ()
        pure (Right text)
  result <-
    catchSync
      (runTurn env event attachments capture emit)
      (pure . Claude.AgentError . T.pack . show)
  case result of
    Claude.AgentSuccess text messages -> finish text messages
    Claude.AgentTruncated text messages -> finish (text <> "\n\n[Response truncated by token limit]") messages
    Claude.AgentExhausted info ->
      finish
        ( info.partialOutput
            <> "\n\n[Iteration limit reached: "
            <> T.pack (show info.iterationsUsed)
            <> " requests, "
            <> T.pack (show info.toolsCalled)
            <> " tool calls]"
        )
        info.messages
    Claude.AgentCancelled -> pure (Right "")
    Claude.AgentError err -> pure (Left err)
  where
    session = sessionToConversationId event.session
    lock = maybe id (withSessionLock env.sessionLocks) session

runTurn :: AppEnv -> Event -> TVar [Tools.Attachment] -> (Text -> IO ()) -> (Output -> IO ()) -> IO Claude.AgentResult
runTurn env event attachments narrate emit = do
  let profile = env.agentProfile
      observer = metricsObserver env.metrics profile.model.model (metricsSource event.source)
  now <- getCurrentTime
  (history, horizon) <- case sessionToConversationId event.session of
    Nothing -> pure ([], 0)
    Just cid -> do
      conversation <- env.conversations.getConversation cid
      history <-
        maybe
          (pure conversation.messages)
          ( \config ->
              compactIfNeeded
                env.logger
                env.claude
                config
                observer.onCompaction
                observer.onApiResponse
                (env.conversations.replaceMessages cid)
                conversation.messages
          )
          env.compaction
      current <- env.conversations.getConversation cid
      horizon <- getAndUpdateHorizon env.pruneHorizons cid (length history) (now > current.cacheExpiresAt)
      pure (history, horizon)
  system <- assemblePrompt env.workspace profile.systemPrompt
  cancellation <- maybe (pure Nothing) (sessionCancelFlag env.sessionLocks) (sessionToConversationId event.session)
  let base =
        foldr
          Tools.registerTool
          env.registry
          [ Tools.mkRunCommandTool env.logger env.workspace profile.permissions,
            Tools.mkQueueAttachmentTool env.logger attachments,
            Tools.mkScheduleCallbackTool env.logger env.callbackStore event.session event.deliveryTarget
          ]
      tasks = maybe id forTurn cancellation $ scopeTasks (fromMaybe (T.pack (show event.source) <> T.pack (show event.timestamp)) (sessionToConversationId event.session)) env.asyncTaskStore
      delegateNote task _ calls = emit (ToolNotice (Just task) calls)
      delegated =
        foldr
          Tools.registerTool
          base
          [ Tools.mkDelegateTaskTool
              env.logger
              env.claude
              base
              env.requestApproval
              profile
              env.pruning
              env.workspace
              env.maxImageDimension
              env.metrics
              env.delegateAgent
              env.delegateExtraAgents
              env.delegateAllowedModels
              (Just delegateNote)
              (Just tasks),
            Tools.mkCheckTaskTool tasks,
            Tools.mkAwaitTaskTool tasks,
            Tools.mkCancelTaskTool tasks
          ]
      registry = case recipients env event.deliveryTarget of
        [] -> delegated
        chats ->
          Tools.registerTool
            ( Tools.mkSendMessageTool env.logger $ \text -> do
                errors <- lefts <$> mapM (\chat -> sendText env chat text) chats
                pure (if null errors then Right () else Left (T.intercalate "; " errors))
            )
            delegated
      config =
        Claude.AgentConfig
          { logger = env.logger,
            client = env.claude,
            registry = Tools.applyToolFilter profile.toolFilter registry,
            requestApproval = env.requestApproval,
            systemPrompt = system,
            agentProfile = profile,
            observer = observer,
            onRateLimit = Just (\attempt delay -> emit (RateLimit ("Rate limited: retry " <> T.pack (show attempt) <> " in " <> T.pack (show delay) <> "s"))),
            onText = Just narrate,
            onToolUse = Just (\_ calls -> emit (ToolNotice Nothing calls)),
            onBeforeApiCall = Just (mapM_ (Telegram.sendChatAction env.telegram) (recipients env event.deliveryTarget)),
            toolSearch = case profile.toolSearch of ToolSearchDisabled -> Nothing; ToolSearchEnabled names -> Just (Set.fromList (map Claude.ToolName names)),
            pruningConfig = env.pruning,
            pruneHorizon = horizon,
            outputFormat = Nothing,
            maxImageDimension = env.maxImageDimension,
            isCancelled = maybe (pure False) readTVarIO cancellation
          }
      images = [Claude.ImageBlock img.mediaType.unMediaType img.base64Data.unBase64Data | img <- maybe [] pure event.image]
      input = Claude.ClaudeMessage Claude.User (images <> [Claude.TextBlock (attachPromptNote event.prompt event.attachments)])
  Claude.runAgentTurn config history input

lookupToolUseMessages :: AppEnv -> Int64 -> STM Bool
lookupToolUseMessages env chat = Map.findWithDefault env.toolUseMessages chat <$> readTVar env.toolUseMessagesOverrides

recipients :: AppEnv -> DeliveryTarget -> [Int64]
recipients _ (TelegramDelivery chats) = NE.toList chats
recipients env TelegramBroadcast = Map.keys env.telegramChatMap
recipients _ LogOnly = []

deliver :: AppEnv -> DeliveryTarget -> Output -> IO ()
deliver env target output = do
  chats <- case output of
    ToolNotice _ _ -> atomically (filterM (lookupToolUseMessages env) (recipients env target))
    _ -> pure (recipients env target)
  case output of
    Narration text attachments -> do
      mapM_ (\chat -> void (sendText env chat text)) chats
      mapM_ (\attachment -> mapM_ (\chat -> safe (sendAttachment chat attachment)) chats) attachments
      when (null chats) (logInfo env.logger "Event output" [("text", text)])
    RateLimit text -> mapM_ (\chat -> void (sendText env chat text)) chats
    ToolNotice label calls -> do
      let note = formatToolUseNote label calls
      mapM_ (\chat -> safe (Telegram.notifyHtml env.logger env.telegram chat note.html note.plain)) chats
  where
    safe action = catchSync action (\err -> logError env.logger "Delivery failed" [("error", T.pack (show err))])
    sendAttachment chat attachment =
      let send = case attachment.type_ of
            Tools.AttachPhoto -> Telegram.sendPhoto
            Tools.AttachDocument -> Telegram.sendDocument
            Tools.AttachAuto -> if isPhotoExtension attachment.path then Telegram.sendPhoto else Telegram.sendDocument
       in send env.telegram chat attachment.path attachment.caption

sendText :: AppEnv -> Int64 -> Text -> IO (Either Text ())
sendText env chat text = catchSync (Right <$> Telegram.sendMessage env.telegram chat text) $ \err -> do
  let message = T.pack (show err)
  logError env.logger "Delivery failed" [("error", message)]
  pure (Left message)

sessionToConversationId :: SessionConfig -> Maybe Text
sessionToConversationId Isolated = Nothing
sessionToConversationId (Named name) = Just name

attachPromptNote :: Text -> [SavedAttachment] -> Text
attachPromptNote prompt [] = prompt
attachPromptNote prompt attachments =
  T.intercalate "\n" $
    [prompt | not (T.null prompt)]
      <> ["Archived inbound attachments:"]
      <> [ T.pack attachment.path
             <> maybe "" (\name -> " (" <> name <> ")") attachment.originalName
             <> " — "
             <> attachment.mediaType
             <> ", "
             <> T.pack (show attachment.sizeBytes)
             <> " bytes"
             <> if attachment.perceivable then " (attached for viewing)" else " (saved only)"
         | attachment <- attachments
         ]
