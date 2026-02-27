module Elwood.App
  ( runApp,
  )
where

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (newTVarIO)
import Control.Exception (finally)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Elwood.Approval
  ( ApprovalCoordinator,
    ApprovalResult (..),
    Decision (..),
    formatApprovalRequest,
    newApprovalCoordinator,
    parseCallbackData,
    requestApproval,
    respondToApproval,
  )
import Elwood.Claude qualified as Claude
import Elwood.Claude.Pruning (newPruneHorizons)
import Elwood.Config
import Elwood.Event (AppEnv (..), handleTelegramMessage)
import Elwood.Logging
import Elwood.MCP qualified as MCP
import Elwood.Memory (newMemoryStore)
import Elwood.Metrics (newMetricsStore, setMCPServerCount)
import Elwood.Telegram qualified as Telegram
import Elwood.Tools qualified as Tools
import Elwood.Webhook qualified as Webhook
import System.Directory (createDirectoryIfMissing)

-- | Initialize and run the application
runApp :: Config -> IO ()
runApp config = do
  -- Initialize logging
  logger <- newLogger Info

  logInfo logger "Elwood starting up" []
  logInfo logger "Configuration loaded" [("state_dir", T.pack config.stateDir)]

  -- Ensure state directory exists
  createDirectoryIfMissing True config.stateDir

  -- Initialize Telegram client
  tg <- Telegram.newClient logger config.telegramToken
  logInfo logger "Telegram client initialized" []

  -- Initialize Claude client
  claude <- Claude.newClient config.anthropicApiKey
  logInfo logger "Claude client initialized" [("model", config.model)]

  -- Initialize conversation store
  convs <- Claude.newConversationStore config.stateDir
  logInfo logger "Conversation store initialized" []

  -- Initialize memory store
  memoryStore <- newMemoryStore config.stateDir
  logInfo logger "Memory store initialized" []

  -- Initialize approval coordinator
  let perms = config.permissions :: PermissionConfig
      timeoutSecs = perms.approvalTimeoutSeconds
  approvalCoordinator <- newApprovalCoordinator timeoutSecs
  logInfo logger "Approval coordinator initialized" [("timeout_seconds", T.pack (show timeoutSecs))]

  -- Initialize attachment queue
  attachmentQueue_ <- newTVarIO []

  -- Initialize prune horizons
  pruneHorizons_ <- newPruneHorizons

  -- Construct tools with explicit dependencies
  let builtinRegistry =
        Tools.registerTool (Tools.mkQueueAttachmentTool logger attachmentQueue_) $
          Tools.registerTool (Tools.mkRunCommandTool logger config.workspaceDir config.permissions) $
            Tools.registerTool (Tools.mkSaveMemoryTool logger memoryStore) $
              Tools.registerTool
                (Tools.mkSearchMemoryTool logger memoryStore)
                Tools.newToolRegistry

  logInfo
    logger
    "Built-in tools registered"
    [("tool_count", T.pack (show (length (Tools.allTools builtinRegistry))))]

  -- Initialize MCP servers and merge tools
  (reg, mcpServers) <- MCP.startMCPServers logger config.mcpServers builtinRegistry

  logInfo
    logger
    "MCP initialized"
    [("servers", T.pack (show (length mcpServers)))]

  -- Initialize metrics store
  mets <- newMetricsStore
  logInfo logger "Metrics store initialized" []

  -- Tool search config: convert to Maybe (Set ToolName)
  let toolSearch_ = fmap (Set.fromList . map Claude.ToolName) config.toolSearch

  logInfo
    logger
    "Tool search"
    [("enabled", T.pack (show (isJust toolSearch_)))]

  logInfo
    logger
    "Tool registry initialized"
    [("tool_count", T.pack (show (length (Tools.allTools reg))))]

  -- Log allowed chats
  logInfo
    logger
    "Allowed chat IDs"
    [("chats", T.pack (show config.allowedChatIds))]

  -- Create base agent context (without per-chat approval function)
  let baseAgentContext =
        Tools.AgentContext
          { permissionConfig = config.permissions,
            requestApproval = Tools.noApprovalChannel
          }

  -- Helper to create agent context with approval function for a specific chat
  let mkAgentContextWithApproval :: Int64 -> Tools.AgentContext
      mkAgentContextWithApproval cid =
        Tools.AgentContext
          { permissionConfig = config.permissions,
            requestApproval = requestToolApproval logger tg approvalCoordinator cid
          }

  -- Create callback handler for approval responses
  let callbackHandler = handleApprovalCallback logger tg approvalCoordinator

  -- Record MCP server count in metrics
  setMCPServerCount mets (length mcpServers)

  -- Create base app environment (shared by webhook and telegram handlers)
  let appEnv =
        AppEnv
          { logger = logger,
            telegram = tg,
            claude = claude,
            conversations = convs,
            registry = reg,
            agentContext = baseAgentContext,
            compaction = config.compaction,
            systemPromptInputs = config.systemPrompt,
            workspaceDir = config.workspaceDir,
            model = config.model,
            thinking = config.thinking,
            notifyChatIds = config.allowedChatIds,
            attachmentQueue = attachmentQueue_,
            maxIterations = config.maxIterations,
            metrics = mets,
            toolSearch = toolSearch_,
            pruneHorizons = pruneHorizons_
          }

  -- Telegram message handler: inject per-chat approval into AgentContext
  let msgHandler msg =
        let cid = msg.chat.id_
            envForChat = appEnv {agentContext = mkAgentContextWithApproval cid}
         in handleTelegramMessage envForChat msg

  -- Log webhook configuration
  let webhookCfg = config.webhook
  if webhookCfg.enabled
    then
      logInfo
        logger
        "Webhook server enabled"
        [ ("port", T.pack (show webhookCfg.port)),
          ("endpoints", T.pack (show (length webhookCfg.webhooks)))
        ]
    else logInfo logger "Webhook server disabled" []

  -- Run polling and optionally webhook server, with MCP cleanup on exit
  finally
    ( do
        -- Start webhook server in background if enabled
        webhookThread <-
          if webhookCfg.enabled
            then do
              logInfo logger "Starting webhook server" [("port", T.pack (show webhookCfg.port))]
              Just <$> async (Webhook.runWebhookServer webhookCfg appEnv)
            else pure Nothing

        -- Run Telegram polling
        Telegram.runPolling
          logger
          tg
          config.allowedChatIds
          msgHandler
          callbackHandler

        -- Wait for webhook thread (this won't happen in normal operation)
        for_ webhookThread wait
    )
    ( do
        logInfo logger "Shutting down MCP servers" []
        mapM_ MCP.stopServer mcpServers
    )

-- | Request tool approval via Telegram inline keyboard
requestToolApproval ::
  Logger ->
  Telegram.TelegramClient ->
  ApprovalCoordinator ->
  Int64 ->
  Claude.ToolName ->
  Text ->
  IO Tools.ApprovalOutcome
requestToolApproval logger tg coordinator cid toolName_ inputSummary = do
  -- Create approval request and get UUID
  (requestId_, waitForResult) <- requestApproval coordinator

  -- Build the approval message with inline keyboard
  let messageText = formatApprovalRequest toolName_ inputSummary
      keyboard =
        Telegram.InlineKeyboardMarkup
          [ [ Telegram.InlineKeyboardButton "✅ Approve" ("approve:" <> UUID.toText requestId_),
              Telegram.InlineKeyboardButton "❌ Deny" ("deny:" <> UUID.toText requestId_)
            ]
          ]

  -- Send the message with keyboard
  msgId <- Telegram.sendMessageWithKeyboard tg cid messageText keyboard
  logInfo
    logger
    "Sent approval request"
    [ ("tool", let Claude.ToolName tn = toolName_ in tn),
      ("request_id", UUID.toText requestId_),
      ("message_id", T.pack (show msgId))
    ]

  -- Wait for result (blocks until approval, denial, or timeout)
  result <- waitForResult

  -- Convert to ApprovalOutcome
  case result of
    Approved -> pure Tools.ApprovalGranted
    Denied -> pure Tools.ApprovalDenied
    TimedOut -> pure Tools.ApprovalTimeout

-- | Handle callback queries for approval buttons
handleApprovalCallback ::
  Logger ->
  Telegram.TelegramClient ->
  ApprovalCoordinator ->
  Telegram.CallbackQuery ->
  IO ()
handleApprovalCallback logger tg coordinator cq = do
  case cq.data_ of
    Nothing -> do
      logWarn logger "Callback query without data" []
      Telegram.answerCallbackQuery tg cq.id_ (Just "Invalid callback")
    Just callbackData -> do
      case parseCallbackData callbackData of
        Nothing -> do
          logWarn logger "Failed to parse callback data" [("data", callbackData)]
          Telegram.answerCallbackQuery tg cq.id_ (Just "Invalid callback data")
        Just (decision, requestId_) -> do
          let result = case decision of
                Approve -> Approved
                Deny -> Denied
          success <- respondToApproval coordinator requestId_ result

          if success
            then do
              let responseText = case decision of
                    Approve -> "✅ Approved"
                    Deny -> "❌ Denied"
              logInfo
                logger
                "Approval response received"
                [ ("request_id", UUID.toText requestId_),
                  ("decision", T.pack (show decision))
                ]

              -- Acknowledge the callback
              Telegram.answerCallbackQuery tg cq.id_ (Just responseText)

              -- Remove the inline keyboard from the message
              case cq.message of
                Just cbMsg -> do
                  let cbCid = cbMsg.chat.id_
                  Telegram.editMessageReplyMarkup tg cbCid cbMsg.id_ Nothing
                Nothing -> pure ()
            else do
              logWarn logger "Approval request not found or already responded" [("request_id", UUID.toText requestId_)]
              Telegram.answerCallbackQuery tg cq.id_ (Just "Request expired or already responded")
