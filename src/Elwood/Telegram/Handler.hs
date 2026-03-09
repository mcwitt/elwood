-- | Telegram-specific message and command handling.
-- Dispatches slash commands and converts Telegram messages
-- into events for the generic pipeline.
module Elwood.Telegram.Handler
  ( handleTelegramMessage,
  )
where

import Control.Exception (SomeException, catch)
import Data.Aeson (Value (..))
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.List (find, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (diffUTCTime, getCurrentTime)
import Elwood.AgentSettings (AgentProfile (..))
import Elwood.Claude qualified as Claude
import Elwood.Claude.Compaction qualified as Compaction
import Elwood.Claude.Types (epoch)
import Elwood.Command qualified as Cmd
import Elwood.Config (TelegramChatConfig (..))
import Elwood.Event
  ( AppEnv (..),
    Base64Data (..),
    Event (..),
    ImageData (..),
    MediaType (..),
    handleEvent,
    sessionToConversationId,
  )
import Elwood.Event.Types
  ( DeliveryTarget (..),
    EventSource (..),
    SessionConfig (..),
  )
import Elwood.Image (ResizeResult (..), resizeImage)
import Elwood.Logging (Logger, logError, logInfo, logWarn)
import Elwood.Metrics (estimateJsonTokens, estimateTextTokens, recordApiResponse, recordCompaction)
import Elwood.Notify (Severity (..), formatNotify)
import Elwood.Positive (Positive (getPositive))
import Elwood.Session (withSessionLock)
import Elwood.Telegram qualified as Telegram
import Numeric (showFFloat)
import System.Exit (ExitCode (..))

-- | Token count breakdown by category for /context display
data TokenBreakdown = TokenBreakdown
  { userTokens :: Int64,
    assistantTokens :: Int64,
    thinkingTokens :: Int64,
    toolCallTokens :: Int64,
    toolResultTokens :: Int64
  }

emptyBreakdown :: TokenBreakdown
emptyBreakdown = TokenBreakdown 0 0 0 0 0

breakdownTotal :: TokenBreakdown -> Int64
breakdownTotal tb = tb.userTokens + tb.assistantTokens + tb.thinkingTokens + tb.toolCallTokens + tb.toolResultTokens

-- | How a command handles arguments.
data CommandHandler
  = -- | Command takes no arguments.
    NoArgs (IO (Maybe Text))
  | -- | Command requires an argument. The 'Text' is the arg label for help/usage.
    WithArg Text (Text -> IO (Maybe Text))

-- | A slash command: name, help description, and handler.
data Command = Command
  { name :: Text,
    description :: Text,
    handler :: CommandHandler
  }

-- | Parse a @/command args@ from message text.
-- Returns @(commandName, remainingArgs)@ with both stripped.
parseCommand :: Text -> Maybe (Text, Text)
parseCommand txt =
  case T.stripPrefix "/" (T.strip txt) of
    Nothing -> Nothing
    Just rest ->
      let (cmd, args) = T.break (== ' ') rest
       in if T.null cmd then Nothing else Just (cmd, T.strip args)

-- | Handle a Telegram message: commands, image fetching, and event dispatch.
--
-- Returns 'Nothing' on success (the event system delivers via Telegram),
-- or 'Just errorMsg' on failure (for the polling loop to send).
handleTelegramMessage :: AppEnv -> Telegram.Message -> IO (Maybe Text)
handleTelegramMessage env msg =
  case (msg.text, msg.photo) of
    -- Slash commands: dispatch via command list
    (Just txt, _)
      | Just (cmdName, args) <- parseCommand txt ->
          case find (\c -> c.name == cmdName) commands of
            Just cmd -> runCommand cmd args
            Nothing -> pure Nothing -- unknown command, ignore
            -- Ignore bare "/" or other non-parseable slash input
    (Just txt, _) | T.isPrefixOf "/" (T.strip txt) -> pure Nothing
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

    -- All slash commands. Adding a command here automatically includes it in /help.
    commands :: [Command]
    commands =
      [ Command "help" "List available commands" $ NoArgs handleHelp,
        Command "clear" "Clear conversation history" $ NoArgs handleClear,
        Command "compact" "Compact conversation to save context" $ NoArgs handleCompact,
        Command "context" "Show token usage breakdown" $ NoArgs handleContext,
        Command "run" "Execute a shell command" $ WithArg "command" handleRun
      ]

    runCommand :: Command -> Text -> IO (Maybe Text)
    runCommand cmd args = case cmd.handler of
      NoArgs action -> action
      WithArg argLabel action
        | T.null args -> pure (Just $ formatNotify Info $ "Usage: /" <> cmd.name <> " \\<" <> argLabel <> "\\>")
        | otherwise -> action args

    handleHelp :: IO (Maybe Text)
    handleHelp = do
      let formatCmd c = case c.handler of
            NoArgs _ -> "/" <> c.name <> " — " <> c.description
            WithArg argLabel _ -> "/" <> c.name <> " \\<" <> argLabel <> "\\> — " <> c.description
      pure (Just $ formatNotify Info $ T.intercalate "\n" ("Available commands:" : map formatCmd commands))

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
      case env.compaction of
        Nothing -> do
          logInfo lgr "Compact requested but compaction is disabled" [("chat_id", T.pack (show chatIdVal))]
          pure (Just $ formatNotify Info "Compaction is disabled")
        Just compactCfg ->
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
                        (Right <$> Compaction.compactMessages lgr env.claude compactCfg (recordCompaction env.metrics) (recordApiResponse env.metrics env.agentProfile.model "telegram") (env.conversations.replaceMessages cid) msgs)
                          `catch` \(e :: SomeException) -> do
                            logError lgr "Manual compaction failed" [("chat_id", T.pack (show chatIdVal)), ("error", T.pack (show e))]
                            pure (Left e)
                      case result of
                        Left _ ->
                          pure (Just $ formatNotify Error "Compaction failed")
                        Right compacted -> do
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
              now <- getCurrentTime
              let msgCount = length msgs
                  totalTokens = Compaction.estimateTokens msgs
                  -- Walk all messages counting tokens per category
                  tb = foldl' countMessage emptyBreakdown msgs
                  catSum = breakdownTotal tb
                  pct :: Int64 -> Text
                  pct tok
                    | catSum == 0 = "0%"
                    | otherwise = T.pack (show ((tok * 100) `div` catSum)) <> "%"
                  categories =
                    filter
                      (\(_, v) -> v > 0)
                      [ ("User text" :: Text, tb.userTokens),
                        ("Assistant text", tb.assistantTokens),
                        ("Thinking", tb.thinkingTokens),
                        ("Tool calls", tb.toolCallTokens),
                        ("Tool results", tb.toolResultTokens)
                      ]
                  header = "Context usage:"
                  thresholdInfo = case env.compaction of
                    Just cc ->
                      let tokenThreshold = cc.tokenThreshold.getPositive
                          thresholdPct = (totalTokens * 100) `div` tokenThreshold
                       in " (" <> T.pack (show thresholdPct) <> "% of compaction threshold)"
                    Nothing -> ""
                  summary =
                    [ "• " <> T.pack (show msgCount) <> " messages",
                      "• ~" <> formatKTok totalTokens <> " tokens" <> thresholdInfo
                    ]
                  catLines = map (\(label, tok) -> "• " <> label <> ": " <> pct tok) categories
                  cacheTtl
                    | conv.cacheExpiresAt == epoch = "no cache"
                    | remaining <= 0 = "expired"
                    | otherwise =
                        let mins = (realToFrac remaining :: Double) / 60
                         in T.pack (showFFloat (Just 1) mins "") <> " minutes"
                    where
                      remaining = diffUTCTime conv.cacheExpiresAt now
              pure (Just $ formatNotify Info $ T.intercalate "\n" (header : summary <> [""] <> ["Context breakdown:"] <> catLines <> [""] <> ["Cache TTL: " <> cacheTtl]))

    countMessage :: TokenBreakdown -> Claude.ClaudeMessage -> TokenBreakdown
    countMessage acc msg_ = foldl' (countBlock msg_.role) acc msg_.content

    countBlock :: Claude.Role -> TokenBreakdown -> Claude.ContentBlock -> TokenBreakdown
    countBlock Claude.User tb (Claude.TextBlock t) = tb {userTokens = tb.userTokens + estimateTextTokens t}
    countBlock Claude.Assistant tb (Claude.TextBlock t) = tb {assistantTokens = tb.assistantTokens + estimateTextTokens t}
    countBlock _ tb (Claude.ThinkingBlock t _) = tb {thinkingTokens = tb.thinkingTokens + estimateTextTokens t}
    countBlock _ tb (Claude.RedactedThinkingBlock d) = tb {thinkingTokens = tb.thinkingTokens + estimateTextTokens d}
    countBlock _ tb (Claude.ToolUseBlock _ _ input) = tb {toolCallTokens = tb.toolCallTokens + estimateJsonTokens input}
    countBlock _ tb (Claude.ServerToolUseBlock _ _ input) = tb {toolCallTokens = tb.toolCallTokens + estimateJsonTokens input}
    countBlock _ tb (Claude.ToolResultBlock _ content_ _) = tb {toolResultTokens = tb.toolResultTokens + estimateTextTokens content_}
    countBlock _ tb (Claude.ToolSearchResultBlock _ v) = tb {toolResultTokens = tb.toolResultTokens + estimateJsonTokens v}
    countBlock _ tb _ = tb

    formatKTok :: Int -> Text
    formatKTok tokens = T.pack (show (round (fromIntegral tokens / 1e3 :: Double) :: Int)) <> "k"

    handleRun :: Text -> IO (Maybe Text)
    handleRun cmd = do
      logInfo lgr "Running command from Telegram" [("chat_id", T.pack (show chatIdVal)), ("command", cmd)]
      result <- Cmd.runCommandWithTimeout cmd 30 env.workspace
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

    -- Fetch the largest photo and return ImageData
    fetchImageData :: [Telegram.PhotoSize] -> IO (Maybe ImageData)
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
                    let strictBytes = LBS.toStrict rawImageData
                        mt = guessMediaType fp
                        (imageBytes, finalMt, resizeResult) = case env.maxImageDimension of
                          Nothing -> (strictBytes, mt, Unchanged)
                          Just maxDim -> resizeImage maxDim strictBytes mt
                        b64 = Base64Data $ TE.decodeUtf8 $ B64.encode imageBytes
                    case resizeResult of
                      DecodeFailed err ->
                        logWarn
                          lgr
                          "Image decode failed, sending original"
                          [ ("error", T.pack err),
                            ("media_type", mt.unMediaType),
                            ("size_bytes", T.pack (show (BS.length strictBytes)))
                          ]
                      _ -> pure ()
                    logInfo
                      lgr
                      "Photo downloaded and encoded"
                      [ ("media_type", finalMt.unMediaType),
                        ("original_bytes", T.pack (show (BS.length strictBytes))),
                        ("final_bytes", T.pack (show (BS.length imageBytes)))
                      ]
                    pure $ Just ImageData {mediaType = finalMt, base64Data = b64}

    -- Guess media type from file path
    guessMediaType :: Text -> MediaType
    guessMediaType path
      | T.isSuffixOf ".jpg" path || T.isSuffixOf ".jpeg" path = MediaType "image/jpeg"
      | T.isSuffixOf ".png" path = MediaType "image/png"
      | T.isSuffixOf ".gif" path = MediaType "image/gif"
      | T.isSuffixOf ".webp" path = MediaType "image/webp"
      | otherwise = MediaType "image/jpeg" -- Default to JPEG for Telegram photos
