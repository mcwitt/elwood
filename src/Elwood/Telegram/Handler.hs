module Elwood.Telegram.Handler (handleTelegramMessage, parseCommand) where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (atomically, modifyTVar', writeTVar)
import Data.Aeson (Value (Null))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (diffUTCTime, getCurrentTime)
import Elwood.AgentSettings (AgentProfile (..), ModelRef (..))
import Elwood.Claude qualified as C
import Elwood.Claude.Compaction qualified as Compaction
import Elwood.Command qualified as Command
import Elwood.Config (CompactionConfig (..), TelegramChatConfig (..))
import Elwood.Event (AppEnv (..), Event (..), handleEvent, lookupToolUseMessages, sessionToConversationId)
import Elwood.Event.Types
import Elwood.Exception (catchSync)
import Elwood.Metrics (estimateJsonTokens, estimateTextTokens, recordApiResponse, recordCompaction)
import Elwood.Positive (Positive (getPositive))
import Elwood.Session (sessionCancelFlag, withSessionLock)
import Elwood.Telegram.Inbox qualified as Inbox
import Elwood.Telegram.Types (Chat (..), Message (..))
import Elwood.Tools.AsyncTask (cancelTurn)
import System.Exit (ExitCode (..))

parseCommand :: Text -> Maybe (Text, Text)
parseCommand text = do
  rest <- T.stripPrefix "/" (T.strip text)
  let (name, args) = T.break (== ' ') rest
  if T.null name then Nothing else Just (name, T.strip args)

handleTelegramMessage :: AppEnv -> Message -> IO (Maybe Text)
handleTelegramMessage env message = case message.text >>= parseCommand of
  Just (command, argument) ->
    Just <$> case command of
      "help" -> pure "/help — commands\n/clear — clear history\n/compact — compact history\n/context — context usage\n/stop — stop this session\n/tools — toggle tool notifications\n/run <command> — execute a shell command"
      "clear" -> inSession $ \sid ->
        withSessionLock env.sessionLocks sid $
          env.conversations.clearConversation sid >> pure "Conversation cleared"
      "compact" -> inSession $ \sid -> withSessionLock env.sessionLocks sid $ compact sid
      "context" -> inSession context
      "stop" -> do
        token <- maybe (pure Nothing) (sessionCancelFlag env.sessionLocks) sessionId
        case token of
          Nothing -> pure "Nothing to stop"
          Just active -> do
            atomically (writeTVar active True)
            _ <- cancelTurn active env.asyncTaskStore
            pure "Stopping..."
      "tools" -> do
        enabled <- atomically $ do
          current <- lookupToolUseMessages env chat
          modifyTVar' env.toolUseMessagesOverrides (Map.insert chat (not current))
          pure (not current)
        pure ("Tool-use notifications: " <> if enabled then "on" else "off")
      "run"
        | T.null argument -> pure "Usage: /run <command>"
        | otherwise -> do
            result <- Command.runCommandWithTimeout argument 30 env.workspace
            let status = case result.exitCode of ExitSuccess -> ""; ExitFailure code -> "[exit " <> T.pack (show code) <> "]\n"
            pure (status <> "```\n$ " <> argument <> "\n" <> result.output <> "\n```")
      _ -> pure "Unknown command. Use /help."
  Nothing | maybe False (T.isPrefixOf "/" . T.strip) message.text -> pure Nothing
  Nothing -> do
    let text = fromMaybe "" (message.text <|> message.caption)
    inbound <- Inbox.processInbound env.logger env.telegram env.workspace env.maxImageDimension message
    if T.null text && null inbound.saved
      then pure Nothing
      else do
        now <- getCurrentTime
        outcome <- handleEvent env (Event (TelegramSource chat) now Null text inbound.perception inbound.saved session (TelegramDelivery (pure chat)))
        pure (either Just (const Nothing) outcome)
  where
    chat = message.chat.id_
    session = maybe Isolated (.session) (Map.lookup chat env.telegramChatMap)
    sessionId = sessionToConversationId session
    inSession action = maybe (pure "This chat has no persistent session") action sessionId
    compact sid = case env.compaction of
      Nothing -> pure "Compaction is disabled"
      Just config -> do
        conversation <- env.conversations.getConversation sid
        let messages = conversation.messages
        if length messages < 4
          then pure "Conversation is too short to compact"
          else
            catchSync
              ( do
                  result <-
                    Compaction.compactMessages
                      env.logger
                      env.claude
                      config
                      (recordCompaction env.metrics)
                      (recordApiResponse env.metrics env.agentProfile.model.model "telegram")
                      (env.conversations.replaceMessages sid)
                      messages
                  pure ("Compaction complete: ~" <> shown (Compaction.estimateTokens messages) <> " → ~" <> shown (Compaction.estimateTokens result) <> " tokens")
              )
              (const (pure "Compaction failed"))
    context sid = do
      conversation <- env.conversations.getConversation sid
      now <- getCurrentTime
      let messages = conversation.messages
          counts = Map.fromListWith (+) (concatMap messageCounts messages)
          total = sum (Map.elems counts)
          percent value = if total == 0 then "0%" else shown (100 * value `div` total) <> "%"
          remaining = diffUTCTime conversation.cacheExpiresAt now
          cache
            | conversation.cacheExpiresAt == C.epoch = "no cache"
            | remaining <= 0 = "expired"
            | otherwise = shown (ceiling remaining :: Int) <> " seconds"
          threshold = maybe "" (\config -> " / " <> shown config.tokenThreshold.getPositive <> " threshold") env.compaction
      pure $
        if null messages
          then "Conversation is empty"
          else
            T.unlines $
              ["Context usage:", shown (length messages) <> " messages", "~" <> shown (Compaction.estimateTokens messages) <> " tokens" <> threshold]
                <> [name <> ": " <> percent value | (name, value) <- Map.toList counts, value > 0]
                <> ["Cache TTL: " <> cache]

shown :: (Show a) => a -> Text
shown = T.pack . show

messageCounts :: C.ClaudeMessage -> [(Text, Integer)]
messageCounts message = concatMap block message.content
  where
    text kind value = [(kind, fromIntegral (estimateTextTokens value))]
    json kind value = [(kind, fromIntegral (estimateJsonTokens value))]
    block (C.TextBlock value) = text (if message.role == C.User then "User text" else "Assistant text") value
    block (C.ThinkingBlock value _) = text "Thinking" value
    block (C.RedactedThinkingBlock value) = text "Thinking" value
    block (C.ToolUseBlock _ _ value) = json "Tool calls" value
    block (C.ServerToolUseBlock _ _ value) = json "Tool calls" value
    block (C.ToolResultBlock _ parts _) = text "Tool results" (C.toolResultText parts)
    block (C.ToolSearchResultBlock _ value) = json "Tool results" value
    block _ = []
