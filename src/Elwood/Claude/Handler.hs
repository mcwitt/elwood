module Elwood.Claude.Handler
  ( loadSystemPrompt,
    claudeHandler,
  )
where

import Control.Exception (SomeException, catch)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Elwood.Claude.AgentLoop (AgentResult (..), runAgentTurn)
import Elwood.Claude.Client (ClaudeClient)
import Elwood.Claude.Compaction (CompactionConfig)
import Elwood.Claude.Conversation
import Elwood.Claude.Types
import Elwood.Logging (Logger, logError, logInfo)
import Elwood.Telegram.Types (Chat (..), Message (..))
import Elwood.Tools.Registry (ToolRegistry)
import Elwood.Tools.Types (ToolEnv)
import System.Directory (doesFileExist)
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

-- | Create a Claude message handler with tool support
claudeHandler ::
  Logger ->
  ClaudeClient ->
  ConversationStore ->
  ToolRegistry ->
  ToolEnv ->
  CompactionConfig ->
  -- | System prompt
  Maybe Text ->
  -- | Model name
  Text ->
  Message ->
  IO (Maybe Text)
claudeHandler logger client store registry toolEnv compactionConfig systemPrompt model msg =
  case text msg of
    Nothing -> pure Nothing
    Just txt
      | T.strip txt == "/clear" -> handleClear
      | T.isPrefixOf "/" txt -> pure Nothing -- Ignore other commands
      | otherwise -> handleMessage txt
  where
    chatIdVal :: Int64
    chatIdVal = chatId (chat msg)

    handleClear :: IO (Maybe Text)
    handleClear = do
      clearConversation store chatIdVal
      logInfo logger "Conversation cleared" [("chat_id", T.pack (show chatIdVal))]
      pure (Just "Conversation cleared. Starting fresh!")

    handleMessage :: Text -> IO (Maybe Text)
    handleMessage userText = do
      logInfo
        logger
        "Processing message"
        [ ("chat_id", T.pack (show chatIdVal)),
          ("text_length", T.pack (show (T.length userText)))
        ]

      -- Get existing conversation
      conv <- getConversation store chatIdVal

      -- Create user message with content blocks
      let userMsg = ClaudeMessage User [TextBlock userText]

      -- Run the agent turn (handles tool use loop internally)
      result <-
        runAgentTurn
          logger
          client
          registry
          toolEnv
          compactionConfig
          systemPrompt
          model
          (convMessages conv)
          userMsg

      case result of
        AgentSuccess responseText allMessages -> do
          -- Update conversation with all messages (including tool interactions)
          updateConversation store chatIdVal allMessages

          logInfo
            logger
            "Agent turn completed"
            [ ("chat_id", T.pack (show chatIdVal)),
              ("response_length", T.pack (show (T.length responseText))),
              ("message_count", T.pack (show (length allMessages)))
            ]

          pure (Just responseText)
        AgentError errorMsg -> do
          logError
            logger
            "Agent turn failed"
            [ ("chat_id", T.pack (show chatIdVal)),
              ("error", errorMsg)
            ]
          pure (Just errorMsg)
