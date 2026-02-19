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
  [ClaudeMessage] ->
  IO [ClaudeMessage]
compactIfNeeded logger client config msgs = do
  let tokens = estimateTokens msgs
  if tokens < ccTokenThreshold config
    then pure msgs
    else do
      logInfo
        logger
        "Context compaction triggered"
        [ ("estimated_tokens", T.pack (show tokens)),
          ("threshold", T.pack (show (ccTokenThreshold config))),
          ("message_count", T.pack (show (length msgs)))
        ]
      compactMessages logger client config msgs

-- | Perform the actual compaction
compactMessages ::
  Logger ->
  ClaudeClient ->
  CompactionConfig ->
  [ClaudeMessage] ->
  IO [ClaudeMessage]
compactMessages logger client config msgs = do
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

  -- Generate summary of old messages
  summaryResult <- summarizeMessages client config oldMsgs

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
              { cmRole = User,
                cmContent =
                  [ TextBlock $
                      "[Previous conversation summary]\n\n" <> summary
                  ]
              }
      pure $ summaryMsg : recentMsgs

-- | Summarize a list of messages using a fast model
summarizeMessages ::
  ClaudeClient ->
  CompactionConfig ->
  [ClaudeMessage] ->
  IO (Either Text Text)
summarizeMessages client config msgs = do
  -- Build a prompt asking for a summary
  let conversationText = formatMessagesForSummary msgs
      summaryRequest =
        ClaudeMessage
          { cmRole = User,
            cmContent =
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
          { mrModel = ccCompactionModel config,
            mrMaxTokens = 2048,
            mrSystem = Just "You are a helpful assistant that summarizes conversations concisely.",
            mrMessages = [summaryRequest],
            mrTools = [],
            mrThinking = Nothing
          }

  result <- sendMessages client request

  case result of
    Left err -> pure $ Left $ T.pack (show err)
    Right response ->
      let text = extractText (mresContent response)
       in if T.null text
            then pure $ Left "Empty summary response"
            else pure $ Right text

-- | Format messages for the summary prompt
formatMessagesForSummary :: [ClaudeMessage] -> Text
formatMessagesForSummary msgs =
  T.intercalate "\n\n" $ map formatMsg msgs
  where
    formatMsg :: ClaudeMessage -> Text
    formatMsg msg =
      let role = case cmRole msg of
            User -> "User"
            Assistant -> "Assistant"
          content = extractText (cmContent msg)
       in role <> ": " <> limitText 1000 content

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
      cmRole m == User && any isToolResult (cmContent m)

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
      cmRole m == Assistant && any isToolUse (cmContent m)

    isToolUse :: ContentBlock -> Bool
    isToolUse (ToolUseBlock {}) = True
    isToolUse _ = False
