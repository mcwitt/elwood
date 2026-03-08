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

-- | Compact messages if they exceed the token threshold.
-- When compaction occurs, the persist callback is called with the
-- compacted messages (e.g. to call 'replaceMessages' and reset cache expiry).
compactIfNeeded ::
  Logger ->
  ClaudeClient ->
  CompactionConfig ->
  -- | Compaction event callback
  IO () ->
  -- | API response callback
  (StopReason -> Usage -> IO ()) ->
  -- | Persist compacted messages (called only when messages change)
  ([ClaudeMessage] -> IO ()) ->
  [ClaudeMessage] ->
  IO [ClaudeMessage]
compactIfNeeded logger client config onCompaction onApiResponse persist msgs = do
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
      compactMessages logger client config onCompaction onApiResponse persist msgs

-- | Perform the actual compaction.
-- When messages are rewritten, the persist callback is called.
compactMessages ::
  Logger ->
  ClaudeClient ->
  CompactionConfig ->
  IO () ->
  (StopReason -> Usage -> IO ()) ->
  -- | Persist compacted messages (called only when messages change)
  ([ClaudeMessage] -> IO ()) ->
  [ClaudeMessage] ->
  IO [ClaudeMessage]
compactMessages logger client config onCompaction onApiResponse persist msgs =
  case strategySplit config.strategy msgs of
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
          logWarn logger "Compaction summary failed, truncating to recent messages" [("error", err)]
          persist recentMsgs
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
              result = summaryMsg : recentMsgs
          persist result
          pure result

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
--
-- Both 'CKeepTurns' and 'CKeepFraction' operate in terms of turns
-- (matching the pruning convention). 'CKeepFraction' first converts
-- the fraction to a turn count via @ceiling(f * totalTurns)@, then
-- follows the same logic as 'CKeepTurns'.
strategySplit :: CompactionStrategy -> [ClaudeMessage] -> Maybe ([ClaudeMessage], [ClaudeMessage])
strategySplit strat msgs =
  let boundaries = turnBoundaryIndices msgs
      keepN :: Int
      keepN = case strat of
        CKeepTurns n -> n.getPositive
        CKeepFraction f -> ceiling (f * fromIntegral (length boundaries))
   in -- We need at least keepN+1 boundaries: keepN to keep, 1+ to compact.
      -- Since the split always lands on a turn boundary (a user message with
      -- a TextBlock), it can never land in the middle of a tool_use/tool_result
      -- pair.
      case drop (length boundaries - keepN) boundaries of
        (splitIdx : _)
          | splitIdx > 0 -> Just (splitAt splitIdx msgs)
        _ -> Nothing
