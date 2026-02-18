module Elwood.Claude.Handler
  ( loadSystemPrompt,
    claudeHandler,
  )
where

import Control.Exception (SomeException, catch)
import Data.ByteString.Base64.Lazy qualified as B64
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Elwood.Claude.AgentLoop (AgentResult (..), runAgentTurn)
import Elwood.Claude.Client (ClaudeClient)
import Elwood.Claude.Compaction (CompactionConfig)
import Elwood.Claude.Conversation
import Elwood.Claude.Types
import Elwood.Logging (Logger, logError, logInfo, logWarn)
import Elwood.Telegram.Client (TelegramClient, downloadFile, getFile)
import Elwood.Telegram.Types (Chat (..), Message (..), PhotoSize (..), TelegramFile (..))
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
  TelegramClient ->
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
claudeHandler logger client telegram store registry toolEnv compactionConfig systemPrompt model msg =
  case (text msg, photo msg) of
    -- Handle /clear command
    (Just txt, _) | T.strip txt == "/clear" -> handleClear
    -- Ignore other slash commands
    (Just txt, _) | T.isPrefixOf "/" txt -> pure Nothing
    -- Handle text with optional photo
    (Just txt, photos) -> handleMessageWithPhoto txt photos
    -- Handle photo with caption only
    (Nothing, Just photos@(_ : _)) -> handleMessageWithPhoto (fromMaybe "" (caption msg)) (Just photos)
    -- No text and no photos (or empty photo list)
    (Nothing, _) -> pure Nothing
  where
    chatIdVal :: Int64
    chatIdVal = chatId (chat msg)

    handleClear :: IO (Maybe Text)
    handleClear = do
      clearConversation store chatIdVal
      logInfo logger "Conversation cleared" [("chat_id", T.pack (show chatIdVal))]
      pure (Just "Conversation cleared. Starting fresh!")

    handleMessageWithPhoto :: Text -> Maybe [PhotoSize] -> IO (Maybe Text)
    handleMessageWithPhoto userText maybePhotos = do
      logInfo
        logger
        "Processing message"
        [ ("chat_id", T.pack (show chatIdVal)),
          ("text_length", T.pack (show (T.length userText))),
          ("has_photo", T.pack (show (maybe False (not . null) maybePhotos)))
        ]

      -- Build content blocks: image first (if present), then text
      imageBlock <- case maybePhotos of
        Just photos@(_ : _) -> fetchImageBlock photos
        _ -> pure Nothing

      let textBlocks = [TextBlock userText | not (T.null userText)]
          contentBlocks = maybeToList imageBlock ++ textBlocks

      -- If no content, skip
      if null contentBlocks
        then pure Nothing
        else do
          -- Get existing conversation
          conv <- getConversation store chatIdVal

          -- Create user message with content blocks
          let userMsg = ClaudeMessage User contentBlocks

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

    -- Fetch the largest photo and convert to ImageBlock
    fetchImageBlock :: [PhotoSize] -> IO (Maybe ContentBlock)
    fetchImageBlock photos = do
      -- Get the largest photo (sort by file size descending, take first)
      let largestPhoto = listToMaybe $ sortOn (Down . psFileSize) photos
      case largestPhoto of
        Nothing -> pure Nothing
        Just ps -> do
          logInfo
            logger
            "Fetching photo"
            [ ("file_id", psFileId ps),
              ("width", T.pack (show (psWidth ps))),
              ("height", T.pack (show (psHeight ps)))
            ]

          -- Get file info from Telegram
          maybeFile <- getFile telegram (psFileId ps)
          case maybeFile of
            Nothing -> do
              logWarn logger "Failed to get file info" [("file_id", psFileId ps)]
              pure Nothing
            Just file -> case tfFilePath file of
              Nothing -> do
                logWarn logger "No file path in response" [("file_id", psFileId ps)]
                pure Nothing
              Just filePath -> do
                -- Download the file
                imageData <-
                  downloadFile telegram filePath
                    `catch` \(e :: SomeException) -> do
                      logWarn logger "Failed to download file" [("error", T.pack (show e))]
                      pure LBS.empty

                if LBS.null imageData
                  then pure Nothing
                  else do
                    -- Determine media type from file extension
                    let mediaType = guessMediaType filePath
                        base64Data = TE.decodeUtf8 $ LBS.toStrict $ B64.encode imageData
                    logInfo
                      logger
                      "Photo downloaded and encoded"
                      [ ("media_type", mediaType),
                        ("size_bytes", T.pack (show (LBS.length imageData)))
                      ]
                    pure $ Just $ ImageBlock mediaType base64Data

    -- Guess media type from file path
    guessMediaType :: Text -> Text
    guessMediaType path
      | T.isSuffixOf ".jpg" path || T.isSuffixOf ".jpeg" path = "image/jpeg"
      | T.isSuffixOf ".png" path = "image/png"
      | T.isSuffixOf ".gif" path = "image/gif"
      | T.isSuffixOf ".webp" path = "image/webp"
      | otherwise = "image/jpeg" -- Default to JPEG for Telegram photos
