module Elwood.Claude.Compaction
  ( compactIfNeeded,
    compactMessages,
    estimateTokens,
    CompactionConfig (..),
    CompactionStrategy (..),
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
import Elwood.AgentSettings (ModelRef (..))
import Elwood.Claude.Client (ClaudeClient, sendMessages)
import Elwood.Claude.Turn qualified as Turn
import Elwood.Claude.Types
import Elwood.Config (CompactionConfig (..), CompactionStrategy (..))
import Elwood.Logging (Logger, logWarn)
import Elwood.Positive (Positive (getPositive))

-- Image bytes do not behave like text tokens. Fold count and redacted wire
-- content together, so nested and top-level images follow the same estimate.
estimateTokens :: [ClaudeMessage] -> Int
estimateTokens messages = fromIntegral (LBS.length (encode stripped) `div` 4) + 3000 * images
  where
    folded = [(ClaudeMessage message.role (map fst blocks), sum (map snd blocks)) | message <- messages, let blocks = map strip message.content]
    stripped = map fst folded
    images = sum (map snd folded)
    strip (ImageBlock media _) = (ImageBlock media "", 1)
    strip (ToolResultBlock ident parts failed) =
      let pieces = map stripPart parts in (ToolResultBlock ident (map fst pieces) failed, sum (map snd pieces))
    strip block = (block, 0)
    stripPart (ToolResultImage media _) = (ToolResultImage media "", 1)
    stripPart part = (part, 0)

compactIfNeeded :: Logger -> ClaudeClient -> CompactionConfig -> IO () -> (StopReason -> Usage -> IO ()) -> ([ClaudeMessage] -> IO ()) -> [ClaudeMessage] -> IO [ClaudeMessage]
compactIfNeeded logger client config notify observe persist messages
  | estimateTokens messages < config.tokenThreshold.getPositive = pure messages
  | otherwise = compactMessages logger client config notify observe persist messages

compactMessages :: Logger -> ClaudeClient -> CompactionConfig -> IO () -> (StopReason -> Usage -> IO ()) -> ([ClaudeMessage] -> IO ()) -> [ClaudeMessage] -> IO [ClaudeMessage]
compactMessages logger client config notify observe persist messages = case strategySplit config.strategy messages of
  Nothing -> pure messages
  Just (old, recent) -> do
    notify
    let input = ClaudeMessage User [TextBlock (fromMaybe defaultCompactionPrompt config.prompt <> "\nConversation:\n" <> formatMessagesForSummary old)]
        request = MessagesRequest config.model.model 2048 (Just "Summarize conversations concisely.") [input] [] Nothing Nothing Nothing Nothing
    response <- sendMessages client config.model.provider request
    summary <- case response of
      Left err -> pure (Left (T.pack (show err)))
      Right reply -> do
        observe reply.stopReason reply.usage
        pure $ case Turn.decide reply.stopReason reply.content of
          Right (Turn.Finish (Turn.Completed message)) | not (T.null (T.strip (extractText message.content))) -> Right (extractText message.content)
          _ -> Left "Summary was empty, incomplete, or malformed"
    case summary of
      Left err -> logWarn logger "Compaction failed; retaining conversation" [("error", err)] >> pure messages
      Right text -> do
        let replacement = ClaudeMessage User [TextBlock ("[Previous conversation summary]\n\n" <> text)] : recent
        persist replacement
        pure replacement

defaultCompactionPrompt :: Text
defaultCompactionPrompt = "Summarize the conversation, preserving user preferences, projects and goals, key facts and decisions, progress, and unresolved questions. Be concise and retain context needed for future work."

extractText :: [ContentBlock] -> Text
extractText blocks = T.intercalate "\n" [text | TextBlock text <- blocks]

formatMessagesForSummary :: [ClaudeMessage] -> Text
formatMessagesForSummary = T.intercalate "\n\n" . map (\message -> T.pack (show message.role) <> ": " <> limit (extractText message.content))
  where
    limit text = T.take 1000 text <> if T.length text > 1000 then "..." else ""

strategySplit :: CompactionStrategy -> [ClaudeMessage] -> Maybe ([ClaudeMessage], [ClaudeMessage])
strategySplit strategy messages = case drop (length boundaries - keep) boundaries of
  index : _ | index > 0 -> Just (splitAt index messages)
  _ -> Nothing
  where
    boundaries = turnBoundaryIndices messages
    keep = case strategy of
      CKeepTurns count -> count.getPositive
      CKeepFraction fraction -> ceiling (fraction * fromIntegral (length boundaries))
