module Elwood.Telegram.Markdown (markdownToTelegramHtml, warmupMarkdown, escapeHtmlText) where

import CMarkGFM
import Control.Exception (evaluate)
import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as T

markdownToTelegramHtml :: Text -> Text
markdownToTelegramHtml = render . commonmarkToNode [optSmart] [extStrikethrough, extTable]

-- cmark-gfm registers extensions lazily; force registration before threading.
warmupMarkdown :: IO ()
warmupMarkdown = void (evaluate (T.length (markdownToTelegramHtml "~~s~~ `c`\n\n| a | b |\n|---|---|\n| 1 | 2 |")))

render :: Node -> Text
render (Node _ kind children) = case kind of
  DOCUMENT -> content
  PARAGRAPH -> content <> "\n\n"
  HEADING _ -> tag "b" content <> "\n\n"
  BLOCK_QUOTE -> tag "blockquote" (trim content)
  CODE_BLOCK language text -> "<pre><code" <> attribute language <> ">" <> escapeHtmlText text <> "</code></pre>\n\n"
  THEMATIC_BREAK -> "---\n\n"
  LIST attrs -> T.concat (zipWith (item attrs) [attrs.listStart ..] children) <> "\n"
  TEXT text -> escapeHtmlText text
  SOFTBREAK -> "\n"
  LINEBREAK -> "\n"
  CODE text -> tag "code" (escapeHtmlText text)
  EMPH -> tag "i" content
  STRONG -> tag "b" content
  STRIKETHROUGH -> tag "s" content
  LINK url _ -> link url
  IMAGE url _ -> link url
  TABLE _ -> tag "pre" (foldMap row children) <> "\n\n"
  TABLE_ROW -> ""
  TABLE_CELL -> ""
  HTML_BLOCK _ -> ""
  HTML_INLINE _ -> ""
  _ -> content
  where
    content = foldMap render children
    trim = T.dropWhileEnd (== '\n')
    link url = "<a href=\"" <> escapeAttribute url <> "\">" <> content <> "</a>"
    attribute language = if T.null language then "" else " class=\"language-" <> escapeAttribute language <> "\""
    item attrs index (Node _ _ nodes) =
      (case attrs.listType of BULLET_LIST -> "• "; ORDERED_LIST -> T.pack (show index) <> ". ")
        <> trim (foldMap render nodes)
        <> "\n"
    row (Node _ _ cells) = "| " <> T.intercalate " | " [T.strip (foldMap render nodes) | Node _ _ nodes <- cells] <> " |\n"

tag :: Text -> Text -> Text
tag name text = "<" <> name <> ">" <> text <> "</" <> name <> ">"

escapeHtmlText :: Text -> Text
escapeHtmlText = T.replace ">" "&gt;" . T.replace "<" "&lt;" . T.replace "&" "&amp;"

escapeAttribute :: Text -> Text
escapeAttribute = T.replace "\"" "&quot;" . escapeHtmlText
