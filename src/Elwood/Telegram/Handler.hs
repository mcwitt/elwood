{-# LANGUAGE StrictData #-}

-- | Telegram-specific message and command handling.
-- Dispatches /clear, /compact, /context, /run commands and
-- converts Telegram messages into events for the generic pipeline.
module Elwood.Telegram.Handler
  ( handleTelegramMessage,
  )
where

import Control.Exception (SomeException, catch)
import Data.Aeson (Value (..))
import Data.ByteString.Base64.Lazy qualified as B64
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.List (foldl', sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime)
import Elwood.AgentSettings (AgentSettings (..))
import Elwood.Claude qualified as Claude
import Elwood.Claude.Compaction qualified as Compaction
import Elwood.Command qualified as Cmd
import Elwood.Config (TelegramChatConfig (..))
import Elwood.Event
  ( AppEnv (..),
    Event (..),
    handleEvent,
    sessionToConversationId,
  )
import Elwood.Event.Types
  ( DeliveryTarget (..),
    EventSource (..),
    SessionConfig (..),
  )
import Elwood.Logging (Logger, logError, logInfo, logWarn)
import Elwood.Metrics (estimateJsonTokens, estimateTextTokens, recordApiResponse, recordCompaction)
import Elwood.Notify (Severity (..), formatNotify)
import Elwood.Session (withSessionLock)
import Elwood.Telegram qualified as Telegram
import System.Exit (ExitCode (..))

-- | Handle a Telegram message: commands, image fetching, and event dispatch.
--
-- Returns 'Nothing' on success (the event system delivers via Telegram),
-- or 'Just errorMsg' on failure (for the polling loop to send).
handleTelegramMessage :: AppEnv -> Telegram.Message -> IO (Maybe Text)
handleTelegramMessage env msg =
  case (msg.text, msg.photo) of
    -- Handle /clear command
    (Just txt, _) | T.strip txt == "/clear" -> handleClear
    -- Handle /compact command
    (Just txt, _) | T.strip txt == "/compact" -> handleCompact
    -- Handle /context command
    (Just txt, _) | T.strip txt == "/context" -> handleContext
    -- Handle /run <cmd>
    (Just txt, _) | Just cmd <- T.stripPrefix "/run " (T.strip txt), not (T.null (T.strip cmd)) -> handleRun (T.strip cmd)
    -- Ignore other slash commands
    (Just txt, _) | T.isPrefixOf "/" txt -> pure Nothing
    -- Handle text with optional photo
    (Just txt, photos) -> handleMessageWithPhoto txt photos
    -- Handle photo with caption only
    (Nothing, Just photos@(_ : _)) -> handleMessageWithPhoto (fromMaybe "" msg.caption) (Just photos)
    -- No text and no photos (or empty photo list)
    (Nothing, _) -> pure Nothing
  where
    lgr :: Logger
    lgr = env.logger

    chatIdVal :: Int64
    chatIdVal = msg.chat.id_

    chatConfig :: Maybe TelegramChatConfig
    chatConfig = Map.lookup chatIdVal env.telegramChatMap

    chatSession :: SessionConfig
    chatSession = maybe Isolated (.session) chatConfig

    handleClear :: IO (Maybe Text)
    handleClear = do
      case sessionToConversationId chatSession of
        Nothing -> do
          logInfo lgr "Clear requested for isolated chat" [("chat_id", T.pack (show chatIdVal))]
          pure (Just $ formatNotify Info "This chat has no persistent session to clear")
        Just cid -> do
          env.conversations.clearConversation cid
          logInfo lgr "Conversation cleared" [("chat_id", T.pack (show chatIdVal)), ("session", cid)]
          pure (Just $ formatNotify Info "Conversation cleared")

    handleCompact :: IO (Maybe Text)
    handleCompact = do
      case sessionToConversationId chatSession of
        Nothing -> do
          logInfo lgr "Compact requested for isolated chat" [("chat_id", T.pack (show chatIdVal))]
          pure (Just $ formatNotify Info "This chat has no persistent session to compact")
        Just cid -> withSessionLock env.sessionLocks cid $ do
          conv <- env.conversations.getConversation cid
          let msgs = conv.messages
              msgCount = length msgs
          if null msgs
            then do
              logInfo lgr "Compact requested on empty conversation" [("chat_id", T.pack (show chatIdVal))]
              pure (Just $ formatNotify Info "Conversation is empty, nothing to compact")
            else
              if msgCount < 4
                then do
                  logInfo lgr "Compact requested on short conversation" [("chat_id", T.pack (show chatIdVal)), ("message_count", T.pack (show msgCount))]
                  pure (Just $ formatNotify Info $ "Conversation is too short to compact (" <> T.pack (show msgCount) <> " messages)")
                else do
                  let beforeTokens = Compaction.estimateTokens msgs
                  result <-
                    (Right <$> Compaction.compactMessages lgr env.claude env.compaction (recordCompaction env.metrics) (recordApiResponse env.metrics env.agentSettings.model "telegram") msgs)
                      `catch` \(e :: SomeException) -> do
                        logError lgr "Manual compaction failed" [("chat_id", T.pack (show chatIdVal)), ("error", T.pack (show e))]
                        pure (Left e)
                  case result of
                    Left _ ->
                      pure (Just $ formatNotify Error "Compaction failed")
                    Right compacted -> do
                      env.conversations.updateConversation cid compacted
                      let afterTokens = Compaction.estimateTokens compacted
                          afterCount = length compacted
                      logInfo
                        lgr
                        "Manual compaction complete"
                        [ ("chat_id", T.pack (show chatIdVal)),
                          ("before_tokens", T.pack (show beforeTokens)),
                          ("after_tokens", T.pack (show afterTokens)),
                          ("before_messages", T.pack (show msgCount)),
                          ("after_messages", T.pack (show afterCount))
                        ]
                      pure
                        ( Just $
                            formatNotify Info $
                              "Compaction complete: ~"
                                <> T.pack (show beforeTokens)
                                <> " → ~"
                                <> T.pack (show afterTokens)
                                <> " tokens ("
                                <> T.pack (show msgCount)
                                <> " → "
                                <> T.pack (show afterCount)
                                <> " messages)"
                        )

    handleContext :: IO (Maybe Text)
    handleContext = do
      case sessionToConversationId chatSession of
        Nothing -> do
          logInfo lgr "Context requested for isolated chat" [("chat_id", T.pack (show chatIdVal))]
          pure (Just $ formatNotify Info "This chat has no persistent session")
        Just cid -> do
          conv <- env.conversations.getConversation cid
          let msgs = conv.messages
          if null msgs
            then pure (Just $ formatNotify Info "Conversation is empty")
            else do
              let msgCount = length msgs
                  totalTokens = Compaction.estimateTokens msgs
                  -- Walk all messages counting tokens per category
                  (userTok, assistTok, thinkTok, toolCallTok, toolResultTok) = foldl' countMessage (0, 0, 0, 0, 0) msgs
                  catSum = userTok + assistTok + thinkTok + toolCallTok + toolResultTok
                  pct :: Int64 -> Text
                  pct tok
                    | catSum == 0 = "0%"
                    | otherwise = T.pack (show (tok * 100 `div` catSum)) <> "%"
                  categories =
                    filter
                      (\(_, v) -> v > 0)
                      [ ("User text" :: Text, userTok),
                        ("Assistant text", assistTok),
                        ("Thinking", thinkTok),
                        ("Tool calls", toolCallTok),
                        ("Tool results", toolResultTok)
                      ]
                  header =
                    "Context usage ("
                      <> T.pack (show msgCount)
                      <> " messages, ~"
                      <> formatKTok totalTokens
                      <> " tokens):"
                  catLines = map (\(label, tok) -> "• " <> label <> ": " <> pct tok) categories
              pure (Just $ formatNotify Info $ T.intercalate "\n" (header : catLines))

    countMessage :: (Int64, Int64, Int64, Int64, Int64) -> Claude.ClaudeMessage -> (Int64, Int64, Int64, Int64, Int64)
    countMessage acc msg_ = foldl' (countBlock msg_.role) acc msg_.content

    countBlock :: Claude.Role -> (Int64, Int64, Int64, Int64, Int64) -> Claude.ContentBlock -> (Int64, Int64, Int64, Int64, Int64)
    countBlock Claude.User (u, a, th, tc, tr) (Claude.TextBlock t) = (u + estimateTextTokens t, a, th, tc, tr)
    countBlock Claude.Assistant (u, a, th, tc, tr) (Claude.TextBlock t) = (u, a + estimateTextTokens t, th, tc, tr)
    countBlock _ (u, a, th, tc, tr) (Claude.ThinkingBlock t _) = (u, a, th + estimateTextTokens t, tc, tr)
    countBlock _ (u, a, th, tc, tr) (Claude.RedactedThinkingBlock d) = (u, a, th + estimateTextTokens d, tc, tr)
    countBlock _ (u, a, th, tc, tr) (Claude.ToolUseBlock _ _ input) = (u, a, th, tc + estimateJsonTokens input, tr)
    countBlock _ (u, a, th, tc, tr) (Claude.ServerToolUseBlock _ _ input) = (u, a, th, tc + estimateJsonTokens input, tr)
    countBlock _ (u, a, th, tc, tr) (Claude.ToolResultBlock _ content_ _) = (u, a, th, tc, tr + estimateTextTokens content_)
    countBlock _ (u, a, th, tc, tr) (Claude.ToolSearchResultBlock _ v) = (u, a, th, tc, tr + estimateJsonTokens v)
    countBlock _ acc _ = acc

    formatKTok :: Int -> Text
    formatKTok tokens = T.pack (show (round (fromIntegral tokens / 1e3 :: Double) :: Int)) <> "k"

    handleRun :: Text -> IO (Maybe Text)
    handleRun cmd = do
      logInfo lgr "Running command from Telegram" [("chat_id", T.pack (show chatIdVal)), ("command", cmd)]
      result <- Cmd.runCommandWithTimeout cmd 30 env.workspaceDir
      let prefix = case result.exitCode of
            ExitSuccess -> ""
            ExitFailure code -> "[exit " <> T.pack (show code) <> "] "
      pure (Just $ formatNotify Info $ prefix <> "\n```\n$ " <> cmd <> "\n" <> result.output <> "\n```")

    handleMessageWithPhoto :: Text -> Maybe [Telegram.PhotoSize] -> IO (Maybe Text)
    handleMessageWithPhoto userText maybePhotos = do
      logInfo
        lgr
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
          -- Create Telegram event
          now <- getCurrentTime
          let evt =
                Event
                  { source = TelegramSource chatIdVal,
                    timestamp = now,
                    payload = Null,
                    prompt = userText,
                    image = imageData,
                    session = chatSession,
                    deliveryTarget = TelegramDelivery (pure chatIdVal)
                  }

          -- Handle the event - delivery to Telegram is done by the event system
          result <- handleEvent env evt

          case result of
            Right responseText -> do
              logInfo
                lgr
                "Agent turn completed"
                [ ("chat_id", T.pack (show chatIdVal)),
                  ("response_length", T.pack (show (T.length responseText)))
                ]
              -- Return Nothing: the event system already delivered to Telegram
              pure Nothing
            Left errorMsg -> do
              logInfo
                lgr
                "Agent turn failed"
                [ ("chat_id", T.pack (show chatIdVal)),
                  ("error", errorMsg)
                ]
              -- Errors are still returned for the polling loop to send,
              -- since the event system only delivers on success
              pure (Just errorMsg)

    -- Fetch the largest photo and return (mediaType, base64Data)
    fetchImageData :: [Telegram.PhotoSize] -> IO (Maybe (Text, Text))
    fetchImageData photos = do
      let tg = env.telegram
      -- Get the largest photo (sort by file size descending, take first)
      let largestPhoto = listToMaybe $ sortOn (Down . (.fileSize)) photos
      case largestPhoto of
        Nothing -> pure Nothing
        Just ps -> do
          logInfo
            lgr
            "Fetching photo"
            [ ("file_id", ps.fileId),
              ("width", T.pack (show ps.width)),
              ("height", T.pack (show ps.height))
            ]

          -- Get file info from Telegram
          maybeFile <- Telegram.getFile tg ps.fileId
          case maybeFile of
            Nothing -> do
              logWarn lgr "Failed to get file info" [("file_id", ps.fileId)]
              pure Nothing
            Just file -> case file.filePath of
              Nothing -> do
                logWarn lgr "No file path in response" [("file_id", ps.fileId)]
                pure Nothing
              Just fp -> do
                -- Download the file
                rawImageData <-
                  Telegram.downloadFile tg fp
                    `catch` \(e :: SomeException) -> do
                      logWarn lgr "Failed to download file" [("error", T.pack (show e))]
                      pure LBS.empty

                if LBS.null rawImageData
                  then pure Nothing
                  else do
                    -- Determine media type from file extension
                    let mt = guessMediaType fp
                        base64Data = TE.decodeUtf8 $ LBS.toStrict $ B64.encode rawImageData
                    logInfo
                      lgr
                      "Photo downloaded and encoded"
                      [ ("media_type", mt),
                        ("size_bytes", T.pack (show (LBS.length rawImageData)))
                      ]
                    pure $ Just (mt, base64Data)

    -- Guess media type from file path
    guessMediaType :: Text -> Text
    guessMediaType path
      | T.isSuffixOf ".jpg" path || T.isSuffixOf ".jpeg" path = "image/jpeg"
      | T.isSuffixOf ".png" path = "image/png"
      | T.isSuffixOf ".gif" path = "image/gif"
      | T.isSuffixOf ".webp" path = "image/webp"
      | otherwise = "image/jpeg" -- Default to JPEG for Telegram photos
