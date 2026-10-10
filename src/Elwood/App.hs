module Elwood.App (runApp, fireCallback) where

import Control.Concurrent.Async (race_)
import Control.Concurrent.STM (newTVarIO)
import Control.Exception (bracket)
import Control.Monad (void)
import Data.Aeson (Value (Null))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Elwood.AgentSettings (AgentProfile (..), resolveProfile, toOverrides)
import Elwood.Approval qualified as Approval
import Elwood.Claude qualified as Claude
import Elwood.Claude.Pruning (newPruneHorizons)
import Elwood.Config
import Elwood.Event
import Elwood.Event.Types
import Elwood.Exception (catchSync)
import Elwood.Logging
import Elwood.MCP qualified as MCP
import Elwood.Memory (newMemoryStore)
import Elwood.Metrics qualified as Metrics
import Elwood.Scheduler qualified as Scheduler
import Elwood.Session (newSessionLocks, sessionCancelFlag)
import Elwood.Telegram qualified as Telegram
import Elwood.Telegram.Handler (handleTelegramMessage, parseCommand)
import Elwood.Telegram.Markdown (warmupMarkdown)
import Elwood.Tools qualified as Tools
import Elwood.Tools.AsyncTask (closeTaskStore)
import Elwood.Webhook qualified as Webhook
import Elwood.Workers qualified as Workers
import System.Directory (createDirectoryIfMissing)

runApp :: Config -> IO ()
runApp config = do
  logger <- newLogger Info
  warmupMarkdown
  createDirectoryIfMissing True config.stateDir
  telegram <- Telegram.newClient logger config.telegramToken
  claude <- Claude.newClient config.providers
  conversations <- Claude.newConversationStore config.stateDir
  memory <- newMemoryStore config.stateDir
  callbacks <- Scheduler.newCallbackStore logger config.stateDir
  approvals <- Approval.newApprovalCoordinator
  horizons <- newPruneHorizons
  sessions <- newSessionLocks
  metrics <- Metrics.newMetricsStore
  toggles <- newTVarIO (Map.fromList [(chat.id_, enabled) | chat <- config.telegramChats, Just enabled <- [chat.toolUseMessages]])
  let builtins =
        foldr
          Tools.registerTool
          Tools.newToolRegistry
          [ Tools.mkListCallbacksTool callbacks,
            Tools.mkCancelCallbackTool callbacks,
            Tools.mkViewImageTool logger config.workspace,
            Tools.mkSaveMemoryTool logger memory,
            Tools.mkSearchMemoryTool logger memory
          ]
  bracket (MCP.startMCPServers logger config.mcpServers builtins) (mapM_ MCP.stopServer . snd) $ \(registry, servers) ->
    bracket (Tools.newAsyncTaskStore 3600) closeTaskStore $ \tasks -> Workers.withWorkers $ \workers -> do
      Metrics.setMCPServerCount metrics (length servers)
      let env =
            AppEnv
              { logger = logger,
                telegram = telegram,
                claude = claude,
                conversations = conversations,
                registry = registry,
                requestApproval = Tools.noApprovalChannel,
                compaction = config.compaction,
                pruning = config.pruning,
                workspace = config.workspace,
                agentProfile = config.agentProfile,
                telegramChatMap = Map.fromList [(chat.id_, chat) | chat <- config.telegramChats],
                metrics = metrics,
                pruneHorizons = horizons,
                sessionLocks = sessions,
                toolUseMessages = config.toolUseMessages,
                toolUseMessagesOverrides = toggles,
                delegateAgent = config.delegateAgent,
                delegateExtraAgents = config.delegateExtraAgents,
                delegateAllowedModels = config.delegateAllowedModels,
                maxImageDimension = config.maxImageDimension,
                asyncTaskStore = tasks,
                callbackStore = callbacks
              }
          chatEnv cid =
            let profile = resolveProfile (toOverrides config.agentProfile <> maybe mempty (.overrides) (Map.lookup cid env.telegramChatMap))
                approve tool input = do
                  token <- maybe (pure Nothing) (sessionCancelFlag sessions) (Map.lookup cid env.telegramChatMap >>= sessionToConversationId . (.session))
                  result <- Approval.withCancellableApproval token approvals profile.permissions.approvalTimeoutSeconds $ \key ->
                    void $
                      Telegram.sendMessageWithKeyboard
                        telegram
                        cid
                        (Approval.formatApprovalRequest tool input)
                        (Telegram.InlineKeyboardMarkup [[Telegram.InlineKeyboardButton label (decision <> UUID.toText key) | (label, decision) <- [("Approve", "approve:"), ("Deny", "deny:")]]])
                  pure $ case result of Approval.Approved -> Tools.ApprovalGranted; Approval.Denied -> Tools.ApprovalDenied; Approval.TimedOut -> Tools.ApprovalTimeout
             in env {agentProfile = profile, requestApproval = approve}
          lane Isolated = Nothing
          lane (Named name) = Just name
          dispatch message =
            Workers.submit workers $
              if fmap fst (message.text >>= parseCommand) == Just "stop"
                then Nothing
                else lane (maybe Isolated (.session) (Map.lookup message.chat.id_ env.telegramChatMap))
          callback query = case query.data_ >>= Approval.parseCallbackData of
            Nothing -> Telegram.answerCallbackQuery telegram query.id_ (Just "Invalid callback")
            Just (decision, key) -> do
              accepted <- Approval.respondToApproval approvals key (case decision of Approval.Approve -> Approval.Approved; Approval.Deny -> Approval.Denied)
              Telegram.answerCallbackQuery telegram query.id_ (Just (if accepted then "Recorded" else "Request expired"))
              case query.message of
                Just message | accepted -> Telegram.editMessageReplyMarkup telegram message.chat.id_ message.id_ Nothing
                _ -> pure ()
          fire scheduled = Workers.submit workers (lane scheduled.session) $ do
            let selected = case scheduled.deliveryTarget of
                  TelegramDelivery (cid :| []) | Map.member cid env.telegramChatMap -> chatEnv cid
                  _ -> env
            fireCallback logger (handleEvent selected) scheduled
          polling =
            Telegram.runPolling
              logger
              telegram
              (Map.keys env.telegramChatMap)
              dispatch
              (\message -> handleTelegramMessage (chatEnv message.chat.id_) message)
              callback
          webhook =
            Webhook.runWebhookServer
              config.webhook
              Webhook.WebhookHandle
                { runner = eventRunner env,
                  workspace = config.workspace,
                  metrics = Metrics.renderMetrics metrics conversations registry,
                  submit = Workers.submit workers,
                  logger = logger
                }
          services = race_ polling (Scheduler.runScheduler logger callbacks fire)
      logInfo logger "Elwood started" [("state_dir", T.pack config.stateDir)]
      if config.webhook.enabled then race_ services webhook else services

-- The job owns error reporting after admission; queue submission alone cannot
-- observe failures from model execution or conversation persistence.
fireCallback :: Logger -> (Event -> IO (Either Text Text)) -> Scheduler.Callback -> IO ()
fireCallback logger run scheduled =
  catchSync
    (run (Event CallbackSource scheduled.fireAt Null scheduled.prompt Nothing [] scheduled.session scheduled.deliveryTarget) >>= either report (const (pure ())))
    (report . T.pack . show)
  where
    report errorText = logError logger "Scheduled callback failed" [("callback", scheduled.id_.unCallbackId), ("error", errorText)]
