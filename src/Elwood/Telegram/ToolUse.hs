-- | Format tool-use notifications as Telegram HTML, with each call's
-- arguments in a collapsed-by-default expandable blockquote (Bot API 7.4),
-- plus a plain-text rendering used as fallback when Telegram rejects HTML.
--
-- Pre-rendered HTML deliberately bypasses the markdown pipeline: CommonMark
-- strips the indentation that makes pretty-printed JSON readable, and
-- @\<pre\>@ inside @\<blockquote\>@ is not guaranteed by the Bot API.
module Elwood.Telegram.ToolUse
  ( ToolUseNote (..),
    formatToolUseNote,
  )
where

import Data.Aeson (Value (..))
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), defConfig, encodePrettyToTextBuilder')
import Data.Aeson.KeyMap qualified as KM
import Data.Char (ord)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import Elwood.Telegram.Client (telegramApiLimit)
import Elwood.Telegram.Markdown (escapeHtmlText)

-- | A tool-use notification in two renderings: Telegram HTML (primary)
-- and plain text (fallback, also used for LogOnly targets).
data ToolUseNote = ToolUseNote
  { html :: Text,
    plain :: Text
  }
  deriving stock (Eq, Show)

-- | Per-call budget for pretty-printed arguments, in characters. This is a
-- readability cap, independent of the transport limit (which 'messageBudget'
-- enforces).
argsBudget :: Int
argsBudget = 1000

-- | Budget for the whole message, measured in UTF-16 code units on the plain
-- rendering — the unit Telegram counts against 'telegramApiLimit'. Headroom
-- leaves room for the elision marker and for the richer HTML rendering, which
-- falls back to this (guaranteed-fitting) plain text if it overflows.
messageBudget :: Int
messageBudget = telegramApiLimit - 96

-- | Format tool calls as a notification. The optional label names a
-- delegate sub-agent task and is rendered as a bold prefix. Calls beyond
-- the message budget are elided with a @… +N more@ marker.
formatToolUseNote :: Maybe Text -> [(Text, Value)] -> ToolUseNote
formatToolUseNote label calls = go 0 [] [] (map (renderCall label) calls)
  where
    go _ htmls plains [] = note htmls plains
    go used htmls plains ((h, p) : rest)
      | used > 0 && used + len + 1 > messageBudget =
          let marker = "\8230 +" <> T.pack (show (length rest + 1)) <> " more"
           in note (marker : htmls) (marker : plains)
      | otherwise = go (used + len + 1) (h : htmls) (p : plains) rest
      where
        len = utf16Length p
    note hs ps = ToolUseNote (T.intercalate "\n" (reverse hs)) (T.intercalate "\n" (reverse ps))

-- | Length of a 'Text' in UTF-16 code units (how Telegram measures message
-- length); astral-plane code points count as two.
utf16Length :: Text -> Int
utf16Length = T.foldl' (\n c -> n + if ord c >= 0x10000 then 2 else 1) 0

-- | Render one call as (html, plain).
renderCall :: Maybe Text -> (Text, Value) -> (Text, Text)
renderCall label (name, args)
  | emptyArgs args = (headerHtml, headerPlain)
  | otherwise =
      ( headerHtml <> "\n<blockquote expandable>" <> escapeHtmlText pretty <> "</blockquote>",
        headerPlain <> "\n" <> pretty
      )
  where
    headerHtml = "\128295 " <> maybe "" (\l -> "<b>" <> escapeHtmlText l <> "</b>: ") label <> "<code>" <> escapeHtmlText name <> "</code>"
    headerPlain = "\128295 " <> maybe "" (<> ": ") label <> name
    pretty = truncateArgs (prettyJson args)

-- | Inputs that warrant no details blockquote.
emptyArgs :: Value -> Bool
emptyArgs (Object o) = KM.null o
emptyArgs Null = True
emptyArgs _ = False

-- | Pretty-print a JSON value with 2-space indent and sorted keys
-- (deterministic for tests). Renders lazily and keeps only @argsBudget + 1@
-- characters so 'truncateArgs' never materializes a large payload in full.
prettyJson :: Value -> Text
prettyJson =
  TL.toStrict
    . TL.take (fromIntegral argsBudget + 1)
    . B.toLazyText
    . encodePrettyToTextBuilder' defConfig {confIndent = Spaces 2, confCompare = compare}

-- | Truncate pretty-printed args to 'argsBudget' with an ellipsis marker.
truncateArgs :: Text -> Text
truncateArgs t
  | T.length t <= argsBudget = t
  | otherwise = T.take argsBudget t <> "\8230 (truncated)"
