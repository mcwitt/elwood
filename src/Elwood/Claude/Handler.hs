module Elwood.Claude.Handler
  ( loadSystemPrompt
  , claudeHandler
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception (catch, SomeException)
import System.FilePath ((</>))
import System.Directory (doesFileExist)

import Elwood.Claude.Client
import Elwood.Claude.Conversation
import Elwood.Claude.Types
import Elwood.Logging
import Elwood.Telegram.Types (Message (..), Chat (..))

-- | Load system prompt from SOUL.md file
loadSystemPrompt :: FilePath -> IO (Maybe Text)
loadSystemPrompt workspaceDir = do
  let soulPath = workspaceDir </> "SOUL.md"
  exists <- doesFileExist soulPath
  if exists
    then do
      content <- TIO.readFile soulPath
        `catch` \(_ :: SomeException) -> pure ""
      if T.null content
        then pure Nothing
        else pure (Just content)
    else pure Nothing

-- | Create a Claude message handler
claudeHandler
  :: Logger
  -> ClaudeClient
  -> ConversationStore
  -> Maybe Text
  -- ^ System prompt
  -> Text
  -- ^ Model name
  -> Message
  -> IO (Maybe Text)
claudeHandler logger client store systemPrompt model msg =
  case text msg of
    Nothing -> pure Nothing
    Just txt
      | T.strip txt == "/clear" -> handleClear
      | T.isPrefixOf "/" txt -> pure Nothing  -- Ignore other commands
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
        [ ("chat_id", T.pack (show chatIdVal))
        , ("text_length", T.pack (show (T.length userText)))
        ]

      -- Add user message to history
      let userMsg = ClaudeMessage {cmRole = User, cmContent = userText}
      conv <- appendMessage store chatIdVal userMsg

      -- Build request
      let request =
            MessagesRequest
              { mrModel = model
              , mrMaxTokens = 4096
              , mrSystem = systemPrompt
              , mrMessages = convMessages conv
              }

      -- Call Claude
      result <- sendMessages client request

      case result of
        Left err -> handleError err
        Right response -> handleSuccess response

    handleSuccess :: MessagesResponse -> IO (Maybe Text)
    handleSuccess response = do
      -- Extract text from response
      let responseText = extractText (mresContent response)
          usage = mresUsage response

      -- Log token usage
      logInfo
        logger
        "Claude response received"
        [ ("chat_id", T.pack (show chatIdVal))
        , ("input_tokens", T.pack (show (usageInputTokens usage)))
        , ("output_tokens", T.pack (show (usageOutputTokens usage)))
        ]

      -- Add assistant response to history
      let assistantMsg = ClaudeMessage {cmRole = Assistant, cmContent = responseText}
      _ <- appendMessage store chatIdVal assistantMsg

      pure (Just responseText)

    handleError :: ClaudeError -> IO (Maybe Text)
    handleError err = do
      logError
        logger
        "Claude API error"
        [ ("chat_id", T.pack (show chatIdVal))
        , ("error", T.pack (show err))
        ]

      let userMessage = case err of
            ClaudeRateLimited ->
              "I'm being rate limited right now. Please try again in a moment."
            ClaudeOverloaded ->
              "Claude is currently overloaded. Please try again in a few minutes."
            ClaudeApiError errType errMsg ->
              "Sorry, I encountered an error: " <> errType <> " - " <> errMsg
            ClaudeHttpError status _ ->
              "Sorry, there was a connection error (HTTP " <> T.pack (show status) <> "). Please try again."
            ClaudeParseError _ ->
              "Sorry, I received an unexpected response. Please try again."

      pure (Just userMessage)

-- | Extract text content from response blocks
extractText :: [ContentBlock] -> Text
extractText blocks =
  T.intercalate "\n" $
    [t | ContentBlock {cbType = "text", cbText = Just t} <- blocks]
