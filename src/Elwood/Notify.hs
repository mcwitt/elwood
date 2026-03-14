module Elwood.Notify
  ( Severity (..),
    formatNotify,
    sanitizeBackticks,
    escapeUnderscores,
    truncateText,
  )
where

import Data.Text (Text)
import Data.Text qualified as T

-- | Severity level for system notification messages
data Severity = Info | Warn | Error
  deriving stock (Show, Eq)

-- | Format a system notification message with severity prefix.
--
-- Info messages are plain, Warn messages are italic, Error messages
-- are passed through as-is (caller structures with bold label + inline code).
formatNotify :: Severity -> Text -> Text
formatNotify Info msg = "\9898 [INFO] " <> msg
formatNotify Warn msg = "\128993 [WARN] _" <> escapeUnderscores msg <> "_"
formatNotify Error msg = "\128308 [ERROR] " <> msg

-- | Escape underscores so they don't break Telegram MarkdownV2 italic parsing.
escapeUnderscores :: Text -> Text
escapeUnderscores = T.replace "_" "\\_"

-- | Replace backticks with single quotes to avoid breaking inline code spans.
sanitizeBackticks :: Text -> Text
sanitizeBackticks = T.replace "`" "'"

-- | Truncate text for display, adding ellipsis if needed.
truncateText :: Int -> Text -> Text
truncateText maxLen t =
  let stripped = T.strip t
   in if T.length stripped <= maxLen
        then stripped
        else T.take maxLen stripped <> "\8230"
