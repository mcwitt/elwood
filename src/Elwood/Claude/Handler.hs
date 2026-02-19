module Elwood.Claude.Handler
  ( loadSystemPrompt,
    claudeHandler,
  )
where

import Control.Exception (SomeException, catch)
import Data.Aeson (Value (..))
import Data.ByteString.Base64.Lazy qualified as B64
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime)
import Elwood.Claude.Client (ClaudeClient)
import Elwood.Claude.Compaction (CompactionConfig)
import Elwood.Claude.Conversation (ConversationStore, clearConversation)
import Elwood.Config (ThinkingLevel)
import Elwood.Event
  ( DeliveryTarget (..),
    Event (..),
    EventEnv (..),
    EventSource (..),
    SessionConfig (..),
    handleEvent,
  )
import Elwood.Logging (Logger, logInfo, logWarn)
import Elwood.Telegram.Client (TelegramClient, downloadFile, getFile)
import Elwood.Telegram.Types (Chat (..), Message (..), PhotoSize (..), TelegramFile (..))
import Elwood.Tools.Registry (ToolRegistry)
import Elwood.Tools.Types (ToolEnv (..))
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
  -- | Extended thinking level
  ThinkingLevel ->
  -- | Allowed chat IDs (for event env)
  [Int64] ->
  Message ->
  IO (Maybe Text)
claudeHandler logger client telegram store registry toolEnv compactionConfig systemPrompt model thinking allowedChatIds msg =
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
      clearConversation store (T.pack (show chatIdVal))
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

      -- Fetch image if present
      imageData <- case maybePhotos of
        Just photos@(_ : _) -> fetchImageData photos
        _ -> pure Nothing

      -- If no content, skip
      if T.null userText && isNothing imageData
        then pure Nothing
        else do
          -- Create event environment
          let eventEnv =
                EventEnv
                  { eeLogger = logger,
                    eeTelegram = telegram,
                    eeClaude = client,
                    eeConversations = store,
                    eeRegistry = registry,
                    eeToolEnv = toolEnv,
                    eeCompaction = compactionConfig,
                    eeSystemPrompt = systemPrompt,
                    eeModel = model,
                    eeThinking = thinking,
                    eeNotifyChatIds = allowedChatIds,
                    eeAttachmentQueue = teAttachmentQueue toolEnv
                  }

          -- Create Telegram event
          now <- getCurrentTime
          let event =
                Event
                  { evSource = TelegramSource chatIdVal,
                    evTimestamp = now,
                    evPayload = Null,
                    evPrompt = userText,
                    evImage = imageData,
                    evSession = Named (T.pack (show chatIdVal)),
                    -- Don't use TelegramReply since polling handles reply
                    evDelivery = [LogOnly]
                  }

          -- Handle the event
          result <- handleEvent eventEnv event

          case result of
            Right responseText -> do
              logInfo
                logger
                "Agent turn completed"
                [ ("chat_id", T.pack (show chatIdVal)),
                  ("response_length", T.pack (show (T.length responseText)))
                ]
              pure (Just responseText)
            Left errorMsg -> do
              logInfo
                logger
                "Agent turn failed"
                [ ("chat_id", T.pack (show chatIdVal)),
                  ("error", errorMsg)
                ]
              pure (Just errorMsg)

    -- Fetch the largest photo and return (mediaType, base64Data)
    fetchImageData :: [PhotoSize] -> IO (Maybe (Text, Text))
    fetchImageData photos = do
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
                rawImageData <-
                  downloadFile telegram filePath
                    `catch` \(e :: SomeException) -> do
                      logWarn logger "Failed to download file" [("error", T.pack (show e))]
                      pure LBS.empty

                if LBS.null rawImageData
                  then pure Nothing
                  else do
                    -- Determine media type from file extension
                    let mediaType = guessMediaType filePath
                        base64Data = TE.decodeUtf8 $ LBS.toStrict $ B64.encode rawImageData
                    logInfo
                      logger
                      "Photo downloaded and encoded"
                      [ ("media_type", mediaType),
                        ("size_bytes", T.pack (show (LBS.length rawImageData)))
                      ]
                    pure $ Just (mediaType, base64Data)

    -- Guess media type from file path
    guessMediaType :: Text -> Text
    guessMediaType path
      | T.isSuffixOf ".jpg" path || T.isSuffixOf ".jpeg" path = "image/jpeg"
      | T.isSuffixOf ".png" path = "image/png"
      | T.isSuffixOf ".gif" path = "image/gif"
      | T.isSuffixOf ".webp" path = "image/webp"
      | otherwise = "image/jpeg" -- Default to JPEG for Telegram photos
