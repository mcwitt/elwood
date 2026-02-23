module Elwood.App
  ( runApp,
  )
where

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (newTVarIO)
import Control.Exception (SomeException, catch, finally)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
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
import Elwood.Claude.Client (newClaudeClient)
import Elwood.Claude.Conversation (newConversationStore)
import Elwood.Config
import Elwood.Event (AppEnv (..), handleTelegramMessage)
import Elwood.Logging
import Elwood.MCP.Client (stopMCPServer)
import Elwood.MCP.Registry (startMCPServers)
import Elwood.Memory (newMemoryStore)
import Elwood.Metrics (newMetricsStore)
import Elwood.Permissions (pcApprovalTimeoutSeconds)
import Elwood.Telegram.Client
  ( TelegramClient,
    answerCallbackQuery,
    editMessageReplyMarkup,
    newTelegramClient,
    sendMessageWithKeyboard,
  )
import Elwood.Telegram.Polling
import Elwood.Telegram.Types
  ( CallbackQuery (..),
    Chat (..),
    InlineKeyboardButton (..),
    InlineKeyboardMarkup (..),
    Message (..),
  )
import Elwood.Tools.Attachment (mkQueueAttachmentTool)
import Elwood.Tools.Command (mkRunCommandTool)
import Elwood.Tools.Memory (mkSaveMemoryTool, mkSearchMemoryTool)
import Elwood.Tools.Registry
import Elwood.Tools.Types (AgentContext (..), ApprovalOutcome (..))
import Elwood.Webhook.Server (runWebhookServer)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

-- | Load system prompt from SOUL.md file
loadSystemPrompt :: FilePath -> IO (Maybe Text)
loadSystemPrompt workspaceDir = do
  let soulPath = workspaceDir </> "SOUL.md"
  exists <- doesFileExist soulPath
  if exists
    then do
      content <-
        TIO.readFile soulPath
          `catch` \(_ :: SomeException) -> pure ""
      if T.null content
        then pure Nothing
        else pure (Just content)
    else pure Nothing

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
  attachmentQueue <- newTVarIO []

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

  -- Dynamic tool loading config: convert to Maybe [Text]
  let alwaysLoadTools = fmap dtlAlwaysLoad (cfgDynamicToolLoading config)

  logInfo
    logger
    "Dynamic tool loading"
    [("enabled", T.pack (show (isJust alwaysLoadTools)))]

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

  -- Create base app environment (shared by webhook and telegram handlers)
  let mcpServerCount = length mcpServers
      appEnv =
        AppEnv
          { eeLogger = logger,
            eeTelegram = telegram,
            eeClaude = claude,
            eeConversations = conversations,
            eeRegistry = registry,
            eeAgentContext = baseAgentContext,
            eeCompaction = cfgCompaction config,
            eeSystemPrompt = systemPrompt,
            eeModel = cfgModel config,
            eeThinking = cfgThinking config,
            eeNotifyChatIds = cfgAllowedChatIds config,
            eeAttachmentQueue = attachmentQueue,
            eeMaxIterations = cfgMaxIterations config,
            eeMetrics = metrics,
            eeMCPServerCount = mcpServerCount,
            eeAlwaysLoadTools = alwaysLoadTools
          }

  -- Telegram message handler: inject per-chat approval into AgentContext
  let msgHandler msg =
        let cid = chatId (chat msg)
            envForChat = appEnv {eeAgentContext = mkAgentContextWithApproval cid}
         in handleTelegramMessage envForChat msg

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
              Just <$> async (runWebhookServer webhookConfig appEnv)
            else pure Nothing

        -- Run Telegram polling
        runPolling
          logger
          telegram
          (cfgAllowedChatIds config)
          msgHandler
          callbackHandler

        -- Wait for webhook thread (this won't happen in normal operation)
        for_ webhookThread wait
    )
    ( do
        logInfo logger "Shutting down MCP servers" []
        mapM_ stopMCPServer mcpServers
    )

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
                Just cbMsg -> do
                  let cbCid = chatId (chat cbMsg)
                  editMessageReplyMarkup telegram cbCid (messageId cbMsg) Nothing
                Nothing -> pure ()
            else do
              logWarn logger "Approval request not found or already responded" [("request_id", UUID.toText requestId)]
              answerCallbackQuery telegram (cqId cq) (Just "Request expired or already responded")
