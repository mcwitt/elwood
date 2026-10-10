module Elwood.Telegram.ToolUse (ToolUseNote (..), formatToolUseNote) where

import Data.Aeson (Value (..))
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), defConfig, encodePrettyToTextBuilder')
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import Elwood.Notify (truncateText)
import Elwood.Telegram.Markdown (escapeHtmlText)

data ToolUseNote = ToolUseNote {html, plain :: Text} deriving stock (Eq, Show)

-- A note is an ordered prefix of calls that fits the transport budget. Bound
-- each header too: even the first call must fit when names contain astral text.
formatToolUseNote :: Maybe Text -> [(Text, Value)] -> ToolUseNote
formatToolUseNote label calls = ToolUseNote (combine (.html)) (combine (.plain))
  where
    notes = map (renderCall (truncateText 256 <$> label)) calls
    sizes :: [Int]
    sizes = scanl1 (+) [1 + T.foldl' (\n c -> n + if fromEnum c > 65535 then 2 else 1) 0 note.plain | note <- notes]
    kept = map fst (takeWhile ((<= 4000) . snd) (zip notes sizes))
    omitted = length notes - length kept
    combine field = T.intercalate "\n" (map field kept <> ["… +" <> T.pack (show omitted) <> " more" | omitted > 0])

renderCall :: Maybe Text -> (Text, Value) -> ToolUseNote
renderCall label (rawName, args) =
  ToolUseNote
    ("🔧 " <> maybe "" (\l -> "<b>" <> escapeHtmlText l <> "</b>: ") label <> "<code>" <> escapeHtmlText name <> "</code>" <> if empty then "" else "\n<blockquote expandable>" <> escapeHtmlText pretty <> "</blockquote>")
    ("🔧 " <> maybe "" (<> ": ") label <> name <> if empty then "" else "\n" <> pretty)
  where
    name = truncateText 256 rawName
    empty = case args of Null -> True; Object fields -> KM.null fields; _ -> False
    encoded = TL.toStrict . TL.take 1001 . B.toLazyText $ encodePrettyToTextBuilder' defConfig {confIndent = Spaces 2, confCompare = compare} args
    pretty = T.take 1000 encoded <> if T.length encoded > 1000 then "… (truncated)" else ""
