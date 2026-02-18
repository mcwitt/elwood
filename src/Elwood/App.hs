module Elwood.App
  ( AppEnv (..),
    runApp,
  )
where

import Control.Concurrent.Async (concurrently_)
import Control.Exception (SomeException, catch, finally)
import Data.Int (Int64)
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
import Elwood.Claude.Client (ClaudeClient, newClaudeClient)
import Elwood.Claude.Conversation
import Elwood.Claude.Handler
import Elwood.Config
import Elwood.Logging
import Elwood.MCP.Client (stopMCPServer)
import Elwood.MCP.Registry (startMCPServers)
import Elwood.Memory (newMemoryStore)
import Elwood.Permissions (newPermissionChecker, pcApprovalTimeoutSeconds)
import Elwood.Scheduler (SchedulerEnv (..), runScheduler)
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
import Elwood.Tools.Command (runCommandTool)
import Elwood.Tools.FileSystem (readFileTool, writeFileTool)
import Elwood.Tools.Memory (saveMemoryTool, searchMemoryTool)
import Elwood.Tools.Registry
import Elwood.Tools.Types (ApprovalOutcome (..), ToolEnv (..))
import Elwood.Tools.Web (webFetchTool, webSearchTool)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

-- | Application environment containing all initialized components
data AppEnv = AppEnv
  { appConfig :: Config,
    appLogger :: Logger,
    appTelegram :: TelegramClient,
    appClaude :: ClaudeClient,
    appConversations :: ConversationStore,
    appSystemPrompt :: Maybe Text,
    appToolRegistry :: ToolRegistry,
    appToolEnv :: ToolEnv
  }

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
  conversations <- newConversationStore (cfgStateDir config) (cfgMaxHistory config)
  logInfo
    logger
    "Conversation store initialized"
    [("max_history", T.pack (show (cfgMaxHistory config)))]

  -- Load system prompt
  systemPrompt <- loadSystemPrompt (cfgWorkspaceDir config)
  case systemPrompt of
    Just _ -> logInfo logger "System prompt loaded from SOUL.md" []
    Nothing -> logWarn logger "No SOUL.md found, running without system prompt" []

  -- Initialize permission checker
  let permChecker = newPermissionChecker (cfgPermissions config) (cfgWorkspaceDir config)
  logInfo logger "Permission checker initialized" []

  -- Initialize HTTP manager for web tools
  httpManager <- newManager tlsManagerSettings

  -- Initialize memory store
  memoryStore <- newMemoryStore (cfgStateDir config)
  logInfo logger "Memory store initialized" []

  -- Initialize approval coordinator
  let timeoutSeconds = pcApprovalTimeoutSeconds (cfgPermissions config)
  approvalCoordinator <- newApprovalCoordinator timeoutSeconds
  logInfo logger "Approval coordinator initialized" [("timeout_seconds", T.pack (show timeoutSeconds))]

  -- Create base tool environment (without per-chat approval function)
  let baseToolEnv =
        ToolEnv
          { teLogger = logger,
            teWorkspaceDir = cfgWorkspaceDir config,
            teStateDir = cfgStateDir config,
            tePermissions = permChecker,
            teHttpManager = httpManager,
            teBraveApiKey = cfgBraveApiKey config,
            teMemoryStore = memoryStore,
            teChatId = Nothing,
            teRequestApproval = Nothing
          }

  -- Helper to create tool environment with approval function for a specific chat
  let mkToolEnvWithApproval :: Int64 -> ToolEnv
      mkToolEnvWithApproval cid =
        baseToolEnv
          { teChatId = Just cid,
            teRequestApproval = Just (requestToolApproval logger telegram approvalCoordinator cid)
          }

  -- Initialize tool registry with built-in tools
  let builtinRegistry =
        registerTool runCommandTool $
          registerTool readFileTool $
            registerTool writeFileTool $
              registerTool webSearchTool $
                registerTool webFetchTool $
                  registerTool saveMemoryTool $
                    registerTool
                      searchMemoryTool
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

  -- Get compaction config from main config
  let compactionConfig = cfgCompaction config

  logInfo
    logger
    "Tool registry initialized"
    [("tool_count", T.pack (show (length (allTools registry))))]

  let _env =
        AppEnv
          { appConfig = config,
            appLogger = logger,
            appTelegram = telegram,
            appClaude = claude,
            appConversations = conversations,
            appSystemPrompt = systemPrompt,
            appToolRegistry = registry,
            appToolEnv = baseToolEnv
          }

  -- Log allowed chats
  logInfo
    logger
    "Allowed chat IDs"
    [("chats", T.pack (show (cfgAllowedChatIds config)))]

  -- Log Brave API key status
  case cfgBraveApiKey config of
    Just _ -> logInfo logger "Brave Search API key configured" []
    Nothing -> logWarn logger "No Brave Search API key, web_search will be unavailable" []

  -- Load HEARTBEAT.md
  heartbeatPrompt <- loadHeartbeatPrompt (cfgWorkspaceDir config)
  case heartbeatPrompt of
    Just _ -> logInfo logger "Heartbeat prompt loaded from HEARTBEAT.md" []
    Nothing -> logInfo logger "No HEARTBEAT.md found, heartbeat will be inactive" []

  -- Log cron jobs
  let cronJobs = cfgCronJobs config
  logInfo logger "Cron jobs configured" [("count", T.pack (show (length cronJobs)))]

  -- Create scheduler environment (uses base toolEnv without approval, since scheduler is not interactive)
  let schedulerEnv =
        SchedulerEnv
          { seLogger = logger,
            seTelegram = telegram,
            seClaude = claude,
            seConversations = conversations,
            seRegistry = registry,
            seToolEnv = baseToolEnv,
            seCompaction = compactionConfig,
            seSystemPrompt = systemPrompt,
            seModel = cfgModel config,
            seHeartbeatConfig = cfgHeartbeat config,
            seHeartbeatPrompt = heartbeatPrompt,
            seNotifyChatIds = cfgAllowedChatIds config,
            seCronJobs = cronJobs
          }

  -- Create callback handler for approval responses
  let callbackHandler = handleApprovalCallback logger telegram approvalCoordinator

  -- Run polling and scheduler concurrently, with MCP cleanup on exit
  finally
    ( concurrently_
        ( runPolling
            logger
            telegram
            (cfgAllowedChatIds config)
            (claudeHandlerWithApproval logger claude telegram conversations registry mkToolEnvWithApproval compactionConfig systemPrompt (cfgModel config))
            callbackHandler
        )
        (runScheduler schedulerEnv)
    )
    ( do
        logInfo logger "Shutting down MCP servers" []
        mapM_ stopMCPServer mcpServers
    )

-- | Load heartbeat prompt from HEARTBEAT.md file
loadHeartbeatPrompt :: FilePath -> IO (Maybe Text)
loadHeartbeatPrompt workspaceDir = do
  let heartbeatPath = workspaceDir </> "HEARTBEAT.md"
  exists <- doesFileExist heartbeatPath
  if exists
    then do
      content <-
        TIO.readFile heartbeatPath
          `catch` \(_ :: SomeException) -> pure ""
      if T.null content
        then pure Nothing
        else pure (Just content)
    else pure Nothing

-- | Claude handler that injects per-chat approval function into ToolEnv
claudeHandlerWithApproval ::
  Logger ->
  ClaudeClient ->
  TelegramClient ->
  ConversationStore ->
  ToolRegistry ->
  (Int64 -> ToolEnv) ->
  CompactionConfig ->
  Maybe Text ->
  Text ->
  Message ->
  IO (Maybe Text)
claudeHandlerWithApproval logger client telegram store registry mkToolEnv compactionConfig systemPrompt model msg = do
  let cid = chatId (chat msg)
      toolEnvForChat = mkToolEnv cid
  claudeHandler logger client telegram store registry toolEnvForChat compactionConfig systemPrompt model msg

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
