{-# LANGUAGE StrictData #-}

module Elwood.Claude.Compaction
  ( -- * Compaction
    compactIfNeeded,
    estimateTokens,

    -- * Re-export config
    CompactionConfig (..),

    -- * Exported for testing
    extractText,
    formatMessagesForSummary,
    safeSplit,
  )
where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Claude.Client (ClaudeClient, sendMessages)
import Elwood.Claude.Types
import Elwood.Config (CompactionConfig (..))
import Elwood.Logging (Logger, logInfo, logWarn)

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
  if tokens < config.tokenThreshold
    then pure msgs
    else do
      logInfo
        logger
        "Context compaction triggered"
        [ ("estimated_tokens", T.pack (show tokens)),
          ("threshold", T.pack (show config.tokenThreshold)),
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
compactMessages logger client config onCompaction onApiResponse msgs = do
  -- Split messages: keep recent half, summarize old half
  -- We must be careful not to split in the middle of a tool_use/tool_result pair
  let totalMsgs = length msgs
      splitPoint = totalMsgs `div` 2
      (oldMsgs, recentMsgs) = safeSplit splitPoint msgs

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
      summaryRequest =
        ClaudeMessage
          { role = User,
            content =
              [ TextBlock $
                  "Please summarize the following conversation concisely. "
                    <> "Focus on key facts, decisions, and context that would be important "
                    <> "for continuing the conversation. Be brief but comprehensive.\n\n"
                    <> "Conversation:\n"
                    <> conversationText
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
            cacheControl = False,
            toolSearch = Nothing
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

-- | Split messages at a safe boundary that doesn't break tool_use/tool_result pairs
-- If the split point would leave tool_results without their tool_uses,
-- adjust the split to include both in the recent messages
safeSplit :: Int -> [ClaudeMessage] -> ([ClaudeMessage], [ClaudeMessage])
safeSplit splitPoint msgs =
  let (old, recent) = splitAt splitPoint msgs
   in if startsWithToolResult recent
        then adjustSplit old recent
        else (old, recent)
  where
    -- Check if messages start with a user message containing tool_result blocks
    startsWithToolResult :: [ClaudeMessage] -> Bool
    startsWithToolResult [] = False
    startsWithToolResult (m : _) =
      m.role == User && any isToolResult m.content

    isToolResult :: ContentBlock -> Bool
    isToolResult (ToolResultBlock {}) = True
    isToolResult _ = False

    -- Move messages from old to recent until we find the matching tool_use
    adjustSplit :: [ClaudeMessage] -> [ClaudeMessage] -> ([ClaudeMessage], [ClaudeMessage])
    adjustSplit [] recent = ([], recent)
    adjustSplit old recent =
      let lastOld = last old
          initOld = init old
       in if hasToolUse lastOld
            then (initOld, lastOld : recent)
            else adjustSplit initOld (lastOld : recent)

    hasToolUse :: ClaudeMessage -> Bool
    hasToolUse m =
      m.role == Assistant && any isToolUse m.content

    isToolUse :: ContentBlock -> Bool
    isToolUse (ToolUseBlock {}) = True
    isToolUse _ = False
