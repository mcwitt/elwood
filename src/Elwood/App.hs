module Elwood.App
  ( runApp,
  )
where

import Control.Concurrent.Async (async, wait)
import Control.Exception (SomeException, catch, finally)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Elwood.Approval
  ( ApprovalCoordinator,
    ApprovalResult (..),
    formatApprovalRequest,
    newApprovalCoordinator,
    parseCallbackData,
    requestApproval,
    respondToApproval,
  )
import Elwood.Claude.Client (ClaudeClient, newClaudeClient)
import Elwood.Claude.Conversation
import Elwood.Claude.Handler
import Elwood.Config
import Elwood.Event (AppEnv (..))
import Elwood.Logging
import Elwood.MCP.Client (stopMCPServer)
import Elwood.MCP.Registry (startMCPServers)
import Elwood.Memory (newMemoryStore)
import Elwood.Metrics (MetricsStore, newMetricsStore)
import Elwood.Permissions (pcApprovalTimeoutSeconds)
import Elwood.Telegram.Client
  ( TelegramClient,
    answerCallbackQuery,
    editMessageReplyMarkup,
    newTelegramClient,
    sendDocument,
    sendMessageWithKeyboard,
    sendPhoto,
  )
import Elwood.Telegram.Polling
import Elwood.Telegram.Types
  ( CallbackQuery (..),
    Chat (..),
    InlineKeyboardButton (..),
    InlineKeyboardMarkup (..),
    Message (..),
  )
import Elwood.Tools.Attachment (isPhotoExtension, mkQueueAttachmentTool)
import Elwood.Tools.Command (mkRunCommandTool)
import Elwood.Tools.Memory (mkSaveMemoryTool, mkSearchMemoryTool)
import Elwood.Tools.Registry
import Elwood.Tools.Types (AgentContext (..), ApprovalOutcome (..), Attachment (..), AttachmentType (..))
import Elwood.Webhook.Server (runWebhookServer)
import System.Directory (createDirectoryIfMissing)

-- | Initialize and run the application
runApp :: Config -> IO ()
runApp config = do
  -- Initialize logging
  logger <- newLogger Info

  logInfo logger "Elwood starting up" []
  logInfo logger "Configuration loaded" [("state_dir", T.pack (cfgStateDir config))]

  -- Ensure state directory exists
  createDirectoryIfMissing True (cfgStateDir config)

  -- Initialize Telegram client
  telegram <- newTelegramClient (cfgTelegramToken config)
  logInfo logger "Telegram client initialized" []

  -- Initialize Claude client
  claude <- newClaudeClient (cfgAnthropicApiKey config)
  logInfo logger "Claude client initialized" [("model", cfgModel config)]

  -- Initialize conversation store
  conversations <- newConversationStore (cfgStateDir config)
  logInfo logger "Conversation store initialized" []

  -- Load system prompt
  systemPrompt <- loadSystemPrompt (cfgWorkspaceDir config)
  case systemPrompt of
    Just _ -> logInfo logger "System prompt loaded from SOUL.md" []
    Nothing -> logWarn logger "No SOUL.md found, running without system prompt" []

  -- Initialize memory store
  memoryStore <- newMemoryStore (cfgStateDir config)
  logInfo logger "Memory store initialized" []

  -- Initialize approval coordinator
  let timeoutSeconds = pcApprovalTimeoutSeconds (cfgPermissions config)
  approvalCoordinator <- newApprovalCoordinator timeoutSeconds
  logInfo logger "Approval coordinator initialized" [("timeout_seconds", T.pack (show timeoutSeconds))]

  -- Initialize attachment queue
  attachmentQueue <- newIORef []

  -- Construct tools with explicit dependencies
  let builtinRegistry =
        registerTool (mkQueueAttachmentTool logger attachmentQueue) $
          registerTool (mkRunCommandTool logger (cfgWorkspaceDir config) (cfgPermissions config)) $
            registerTool (mkSaveMemoryTool logger memoryStore) $
              registerTool
                (mkSearchMemoryTool logger memoryStore)
                newToolRegistry

  logInfo
    logger
    "Built-in tools registered"
    [("tool_count", T.pack (show (length (allTools builtinRegistry))))]

  -- Initialize MCP servers and merge tools
  (registry, mcpServers) <- startMCPServers logger (cfgMCPServers config) builtinRegistry

  logInfo
    logger
    "MCP initialized"
    [("servers", T.pack (show (length mcpServers)))]

  -- Initialize metrics store
  metrics <- newMetricsStore
  logInfo logger "Metrics store initialized" []

  -- Get compaction config from main config
  let compactionConfig = cfgCompaction config

  -- Dynamic tool loading config
  let dynamicLoading = cfgDynamicToolLoading config

  logInfo
    logger
    "Dynamic tool loading"
    [("enabled", T.pack (show (isJust dynamicLoading)))]

  logInfo
    logger
    "Tool registry initialized"
    [("tool_count", T.pack (show (length (allTools registry))))]

  -- Log allowed chats
  logInfo
    logger
    "Allowed chat IDs"
    [("chats", T.pack (show (cfgAllowedChatIds config)))]

  -- Create base agent context (without per-chat approval function)
  let baseAgentContext =
        AgentContext
          { acPermissionConfig = cfgPermissions config,
            acRequestApproval = Nothing
          }

  -- Helper to create agent context with approval function for a specific chat
  let mkAgentContextWithApproval :: Int64 -> AgentContext
      mkAgentContextWithApproval cid =
        baseAgentContext
          { acRequestApproval = Just (requestToolApproval logger telegram approvalCoordinator cid)
          }

  -- Create callback handler for approval responses
  let callbackHandler = handleApprovalCallback logger telegram approvalCoordinator

  -- Create webhook app environment
  let mcpServerCount = length mcpServers
      webhookAppEnv =
        AppEnv
          { eeLogger = logger,
            eeTelegram = telegram,
            eeClaude = claude,
            eeConversations = conversations,
            eeRegistry = registry,
            eeAgentContext = baseAgentContext,
            eeCompaction = compactionConfig,
            eeSystemPrompt = systemPrompt,
            eeModel = cfgModel config,
            eeThinking = cfgThinking config,
            eeNotifyChatIds = cfgAllowedChatIds config,
            eeAttachmentQueue = attachmentQueue,
            eeWorkspaceDir = cfgWorkspaceDir config,
            eeMaxIterations = cfgMaxIterations config,
            eeMetrics = metrics,
            eeMCPServerCount = mcpServerCount,
            eeDynamicToolLoading = dynamicLoading
          }

  -- Log webhook configuration
  let webhookConfig = cfgWebhook config
  if wscEnabled webhookConfig
    then
      logInfo
        logger
        "Webhook server enabled"
        [ ("port", T.pack (show (wscPort webhookConfig))),
          ("endpoints", T.pack (show (length (wscWebhooks webhookConfig))))
        ]
    else logInfo logger "Webhook server disabled" []

  -- Run polling and optionally webhook server, with MCP cleanup on exit
  finally
    ( do
        -- Start webhook server in background if enabled
        webhookThread <-
          if wscEnabled webhookConfig
            then do
              logInfo logger "Starting webhook server" [("port", T.pack (show (wscPort webhookConfig)))]
              Just <$> async (runWebhookServer webhookConfig webhookAppEnv)
            else pure Nothing

        -- Run Telegram polling
        runPolling
          logger
          telegram
          (cfgAllowedChatIds config)
          (claudeHandlerWithApproval logger claude telegram conversations registry mkAgentContextWithApproval compactionConfig systemPrompt (cfgModel config) (cfgThinking config) (cfgMaxIterations config) (cfgAllowedChatIds config) attachmentQueue (cfgWorkspaceDir config) metrics mcpServerCount dynamicLoading)
          callbackHandler

        -- Wait for webhook thread (this won't happen in normal operation)
        for_ webhookThread wait
    )
    ( do
        logInfo logger "Shutting down MCP servers" []
        mapM_ stopMCPServer mcpServers
    )

-- | Claude handler that injects per-chat approval function into AgentContext
--   and sends queued attachments after the text reply
claudeHandlerWithApproval ::
  Logger ->
  ClaudeClient ->
  TelegramClient ->
  ConversationStore ->
  ToolRegistry ->
  (Int64 -> AgentContext) ->
  CompactionConfig ->
  Maybe Text ->
  Text ->
  ThinkingLevel ->
  Int ->
  [Int64] ->
  IORef [Attachment] ->
  FilePath ->
  MetricsStore ->
  Int ->
  Maybe DynamicToolLoadingConfig ->
  Message ->
  IO (Maybe Text)
claudeHandlerWithApproval logger client telegram store registry mkCtx compactionConfig systemPrompt model thinking maxIterations allowedChatIds attachmentQueue workspaceDir metrics mcpServerCount dynamicLoading msg = do
  let cid = chatId (chat msg)
      ctxForChat = mkCtx cid
  result <- claudeHandler logger client telegram store registry ctxForChat compactionConfig systemPrompt model thinking maxIterations allowedChatIds attachmentQueue workspaceDir metrics mcpServerCount dynamicLoading msg
  -- Send queued attachments after the text reply
  attachments <- readIORef attachmentQueue
  mapM_ (sendAttachmentToChat logger telegram cid) attachments
  writeIORef attachmentQueue []
  pure result

-- | Send a single attachment to a specific chat
sendAttachmentToChat :: Logger -> TelegramClient -> Int64 -> Attachment -> IO ()
sendAttachmentToChat logger telegram cid att = do
  let send = case attType att of
        AttachPhoto -> sendPhoto
        AttachDocument -> sendDocument
        AttachAuto
          | isPhotoExtension (attPath att) -> sendPhoto
          | otherwise -> sendDocument
  send telegram cid (attPath att) (attCaption att)
    `catch` \(e :: SomeException) ->
      logError
        logger
        "Failed to send attachment"
        [ ("chat_id", T.pack (show cid)),
          ("path", T.pack (attPath att)),
          ("error", T.pack (show e))
        ]

-- | Request tool approval via Telegram inline keyboard
requestToolApproval ::
  Logger ->
  TelegramClient ->
  ApprovalCoordinator ->
  Int64 ->
  Text ->
  Text ->
  IO ApprovalOutcome
requestToolApproval logger telegram coordinator cid toolName inputSummary = do
  -- Create approval request and get UUID
  (requestId, waitForResult) <- requestApproval coordinator

  -- Build the approval message with inline keyboard
  let messageText = formatApprovalRequest toolName inputSummary
      keyboard =
        InlineKeyboardMarkup
          [ [ InlineKeyboardButton "✅ Approve" ("approve:" <> UUID.toText requestId),
              InlineKeyboardButton "❌ Deny" ("deny:" <> UUID.toText requestId)
            ]
          ]

  -- Send the message with keyboard
  msgId <- sendMessageWithKeyboard telegram cid messageText keyboard
  logInfo
    logger
    "Sent approval request"
    [ ("tool", toolName),
      ("request_id", UUID.toText requestId),
      ("message_id", T.pack (show msgId))
    ]

  -- Wait for result (blocks until approval, denial, or timeout)
  result <- waitForResult

  -- Convert to ApprovalOutcome
  case result of
    Approved -> pure ApprovalGranted
    Denied -> pure ApprovalDenied
    TimedOut -> pure ApprovalTimeout

-- | Handle callback queries for approval buttons
handleApprovalCallback ::
  Logger ->
  TelegramClient ->
  ApprovalCoordinator ->
  CallbackQuery ->
  IO ()
handleApprovalCallback logger telegram coordinator cq = do
  case cqData cq of
    Nothing -> do
      logWarn logger "Callback query without data" []
      answerCallbackQuery telegram (cqId cq) (Just "Invalid callback")
    Just callbackData -> do
      case parseCallbackData callbackData of
        Nothing -> do
          logWarn logger "Failed to parse callback data" [("data", callbackData)]
          answerCallbackQuery telegram (cqId cq) (Just "Invalid callback data")
        Just (isApproved, requestId) -> do
          let result = if isApproved then Approved else Denied
          success <- respondToApproval coordinator requestId result

          if success
            then do
              let responseText = if isApproved then "✅ Approved" else "❌ Denied"
              logInfo
                logger
                "Approval response received"
                [ ("request_id", UUID.toText requestId),
                  ("approved", T.pack (show isApproved))
                ]

              -- Acknowledge the callback
              answerCallbackQuery telegram (cqId cq) (Just responseText)

              -- Remove the inline keyboard from the message
              case cqMessage cq of
                Just msg -> do
                  let cid = chatId (chat msg)
                  editMessageReplyMarkup telegram cid (messageId msg) Nothing
                Nothing -> pure ()
            else do
              logWarn logger "Approval request not found or already responded" [("request_id", UUID.toText requestId)]
              answerCallbackQuery telegram (cqId cq) (Just "Request expired or already responded")
