module Elwood.Claude.Compaction
  ( -- * Compaction
    compactIfNeeded,
    compactMessages,
    estimateTokens,

    -- * Re-export config
    CompactionConfig (..),
    CompactionStrategy (..),

    -- * Exported for testing
    defaultCompactionPrompt,
    extractText,
    formatMessagesForSummary,
    strategySplit,
  )
where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Claude.Client (ClaudeClient, sendMessages)
import Elwood.Claude.Types
import Elwood.Config (CompactionConfig (..), CompactionStrategy (..))
import Elwood.Logging (Logger, logInfo, logWarn)
import Elwood.Positive (Positive (getPositive))

-- | Estimate the number of tokens in a message list
-- Uses a rough heuristic: JSON length / 4
estimateTokens :: [ClaudeMessage] -> Int
estimateTokens msgs =
  let jsonBytes = LBS.length $ encode msgs
   in fromIntegral jsonBytes `div` 4

-- | Compact messages if they exceed the token threshold
-- Returns the (possibly compacted) message list
compactIfNeeded ::
  Logger ->
  ClaudeClient ->
  CompactionConfig ->
  -- | Compaction event callback
  IO () ->
  -- | API response callback
  (StopReason -> Usage -> IO ()) ->
  [ClaudeMessage] ->
  IO [ClaudeMessage]
compactIfNeeded logger client config onCompaction onApiResponse msgs = do
  let tokens = estimateTokens msgs
  if tokens < config.tokenThreshold.getPositive
    then pure msgs
    else do
      logInfo
        logger
        "Context compaction triggered"
        [ ("estimated_tokens", T.pack (show tokens)),
          ("threshold", T.pack (show config.tokenThreshold.getPositive)),
          ("message_count", T.pack (show (length msgs)))
        ]
      compactMessages logger client config onCompaction onApiResponse msgs

-- | Perform the actual compaction
compactMessages ::
  Logger ->
  ClaudeClient ->
  CompactionConfig ->
  IO () ->
  (StopReason -> Usage -> IO ()) ->
  [ClaudeMessage] ->
  IO [ClaudeMessage]
compactMessages logger client config onCompaction onApiResponse msgs =
  case strategySplit config.strategy config.tokenThreshold msgs of
    Nothing -> do
      logWarn
        logger
        "Compaction no-op: all messages fall in keep region"
        [ ("strategy", T.pack (show config.strategy)),
          ("message_count", T.pack (show (length msgs)))
        ]
      pure msgs
    Just (oldMsgs, recentMsgs) -> do
      logInfo
        logger
        "Compacting messages"
        [ ("old_messages", T.pack (show (length oldMsgs))),
          ("recent_messages", T.pack (show (length recentMsgs)))
        ]

      -- Record compaction event
      onCompaction

      -- Generate summary of old messages
      summaryResult <- summarizeMessages client config onApiResponse oldMsgs

      case summaryResult of
        Left err -> do
          logWarn logger "Compaction failed, keeping original" [("error", err)]
          -- If summarization fails, just truncate to recent messages
          pure recentMsgs
        Right summary -> do
          logInfo
            logger
            "Compaction successful"
            [("summary_length", T.pack (show (T.length summary)))]
          -- Create a synthetic message with the summary
          let summaryMsg =
                ClaudeMessage
                  { role = User,
                    content =
                      [ TextBlock $
                          "[Previous conversation summary]\n\n" <> summary
                      ]
                  }
          pure $ summaryMsg : recentMsgs

-- | Default structured compaction prompt.
--
-- Designed for a personal assistant that maintains long-running context
-- about the user across sessions.  The summary replaces older messages
-- while recent messages are kept verbatim, so it focuses on background
-- context that would otherwise be lost.
defaultCompactionPrompt :: Text
defaultCompactionPrompt =
  T.unlines
    [ "Summarize the following conversation excerpt. This summary will replace",
      "these messages while the most recent messages are kept intact, so focus",
      "on context that would be lost.",
      "",
      "## User Profile",
      "Personal details, preferences, relationships, and circumstances the user has shared.",
      "",
      "## Projects & Goals",
      "What the user is working on, their motivations, and broader objectives.",
      "",
      "## Key Facts & Decisions",
      "Important facts established, decisions made, resources referenced, and constraints set.",
      "",
      "## Progress",
      "What was accomplished in this portion of the conversation.",
      "",
      "## Open Threads",
      "Anything unresolved, promised, or awaiting follow-up.",
      "",
      "Be concise but preserve everything the user would expect you to remember."
    ]

-- | Summarize a list of messages using a fast model
summarizeMessages ::
  ClaudeClient ->
  CompactionConfig ->
  (StopReason -> Usage -> IO ()) ->
  [ClaudeMessage] ->
  IO (Either Text Text)
summarizeMessages client config onApiResponse msgs = do
  -- Build a prompt asking for a summary
  let conversationText = formatMessagesForSummary msgs
      promptText = fromMaybe defaultCompactionPrompt config.prompt
      summaryRequest =
        ClaudeMessage
          { role = User,
            content =
              [ TextBlock $ promptText <> "\nConversation:\n" <> conversationText
              ]
          }
      request =
        MessagesRequest
          { model = config.model,
            maxTokens = 2048,
            system = Just "You are a helpful assistant that summarizes conversations concisely.",
            messages = [summaryRequest],
            tools = [],
            thinking = Nothing,
            cacheControl = Nothing,
            toolSearch = Nothing,
            outputFormat = Nothing
          }

  result <- sendMessages client request

  case result of
    Left err -> pure $ Left $ T.pack (show err)
    Right response -> do
      onApiResponse response.stopReason response.usage
      let txt = extractText response.content
       in if T.null txt
            then pure $ Left "Empty summary response"
            else pure $ Right txt

-- | Format messages for the summary prompt
formatMessagesForSummary :: [ClaudeMessage] -> Text
formatMessagesForSummary msgs =
  T.intercalate "\n\n" $ map formatMsg msgs
  where
    formatMsg :: ClaudeMessage -> Text
    formatMsg msg =
      let r = case msg.role of
            User -> "User"
            Assistant -> "Assistant"
          c = extractText msg.content
       in r <> ": " <> limitText 1000 c

    limitText :: Int -> Text -> Text
    limitText maxLen t
      | T.length t > maxLen = T.take maxLen t <> "..."
      | otherwise = t

-- | Extract text from content blocks
extractText :: [ContentBlock] -> Text
extractText blocks =
  T.intercalate "\n" [t | TextBlock t <- blocks]

-- | Split messages according to a compaction strategy.
--
-- Returns @Nothing@ when all messages fall in the keep region (no-op).
-- Otherwise returns @Just (old, recent)@ where @old@ will be compacted
-- and @recent@ kept verbatim.
strategySplit :: CompactionStrategy -> Positive -> [ClaudeMessage] -> Maybe ([ClaudeMessage], [ClaudeMessage])
strategySplit (KeepLastTurns n) _threshold msgs =
  let boundaries = turnBoundaryIndices msgs
   in -- We need at least n+1 boundaries: n to keep, 1+ to compact
      case drop (length boundaries - n.getPositive) boundaries of
        (splitIdx : _)
          | splitIdx > 0 -> nonEmptySplit splitIdx msgs
        _ -> Nothing
strategySplit (KeepLastFraction f) threshold msgs =
  let keepTokens = floor (f * fromIntegral threshold.getPositive) :: Int
      boundaries = turnBoundaryIndices msgs
      -- Walk backward through messages, accumulating token estimates
      -- until we've accounted for keepTokens. The split point is the
      -- index of the first message in the keep region.
      splitIdx = findKeepBoundary keepTokens (reverse (zip [0 ..] msgs))
      -- Snap to the first turn boundary at or after the raw split point,
      -- so the keep region starts at a clean turn boundary.
      snapped = firstBoundaryAtOrAfter splitIdx boundaries
   in case snapped of
        Nothing -> Nothing
        Just idx
          | idx <= 0 -> Nothing
          | otherwise -> nonEmptySplit idx msgs

-- | Walk backward through indexed messages, accumulating token estimates.
-- Returns the index where the accumulated tokens first reach the budget.
-- If all messages fit within the budget, returns 0.
findKeepBoundary :: Int -> [(Int, ClaudeMessage)] -> Int
findKeepBoundary _ [] = 0
findKeepBoundary budget ((i, m) : rest)
  | budget <= 0 = i + 1 -- previous message exhausted the budget
  | otherwise = findKeepBoundary (budget - estimateTokens [m]) rest

-- | Find the first turn boundary index that is >= the given position.
-- Returns Nothing if no such boundary exists (all messages are in keep region).
firstBoundaryAtOrAfter :: Int -> [Int] -> Maybe Int
firstBoundaryAtOrAfter pos boundaries =
  case filter (>= pos) boundaries of
    (b : _) -> Just b
    [] -> Nothing

-- | Split at the given index; return Nothing if @old@ would be empty.
-- Since 'strategySplit' always splits at turn boundaries (user messages
-- with a TextBlock), the split can never land in the middle of a
-- tool_use/tool_result pair.
nonEmptySplit :: Int -> [ClaudeMessage] -> Maybe ([ClaudeMessage], [ClaudeMessage])
nonEmptySplit splitIdx msgs =
  case splitAt splitIdx msgs of
    ([], _) -> Nothing
    result -> Just result
