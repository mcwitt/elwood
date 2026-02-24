-- | Convert markdown to Telegram-supported HTML.
--
-- Telegram supports a limited subset of HTML:
-- @\<b\>@, @\<i\>@, @\<s\>@, @\<code\>@, @\<pre\>@, @\<a\>@, @\<blockquote\>@.
--
-- We parse Claude's markdown via @cmark-gfm@ and emit only these tags,
-- with appropriate fallbacks for unsupported elements (headings become bold,
-- lists become text with bullet\/number prefixes, tables become @\<pre\>@ blocks).
module Elwood.Telegram.Markdown
  ( markdownToTelegramHtml,
  )
where

import CMarkGFM
  ( CMarkExtension,
    ListAttributes (..),
    ListType (..),
    Node (..),
    NodeType (..),
    commonmarkToNode,
    extStrikethrough,
    extTable,
    optSmart,
  )
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as B

-- | Convert markdown text to Telegram-compatible HTML.
--
-- Parses the input as GitHub-flavoured CommonMark (with strikethrough and
-- table extensions) and renders the AST using only the HTML tags that
-- Telegram's Bot API supports.
markdownToTelegramHtml :: Text -> Text
markdownToTelegramHtml md =
  let node = commonmarkToNode [optSmart] extensions md
   in TL.toStrict $ B.toLazyText $ renderNode node

extensions :: [CMarkExtension]
extensions = [extStrikethrough, extTable]

-- | Render a cmark Node to a Telegram HTML Builder.
renderNode :: Node -> Builder
renderNode (Node _ nodeType children) = case nodeType of
  DOCUMENT -> renderChildren children
  -- Block elements
  PARAGRAPH -> renderChildren children <> "\n\n"
  HEADING _ -> "<b>" <> renderChildren children <> "</b>\n\n"
  BLOCK_QUOTE -> "<blockquote>" <> trimBlock (renderChildren children) <> "</blockquote>"
  CODE_BLOCK info content ->
    let langAttr
          | T.null info = mempty
          | otherwise = " class=\"language-" <> escapeAttrB info <> "\""
     in "<pre><code"
          <> langAttr
          <> ">"
          <> escapeHtml content
          <> "</code></pre>\n\n"
  THEMATIC_BREAK -> "---\n\n"
  LIST listAttrs -> renderList listAttrs children <> "\n"
  ITEM -> renderChildren children
  -- Inline elements
  TEXT t -> escapeHtml t
  SOFTBREAK -> "\n"
  LINEBREAK -> "\n"
  CODE t -> "<code>" <> escapeHtml t <> "</code>"
  EMPH -> "<i>" <> renderChildren children <> "</i>"
  STRONG -> "<b>" <> renderChildren children <> "</b>"
  STRIKETHROUGH -> "<s>" <> renderChildren children <> "</s>"
  -- Telegram HTML has no title attribute support on links
  LINK url _title -> "<a href=\"" <> escapeAttrB url <> "\">" <> renderChildren children <> "</a>"
  IMAGE url _title -> "<a href=\"" <> escapeAttrB url <> "\">" <> renderChildren children <> "</a>"
  -- Tables: render as pre-formatted pipe tables
  TABLE _alignments -> "<pre>" <> renderTable children <> "</pre>\n\n"
  TABLE_ROW -> mempty -- handled by renderTable
  TABLE_CELL -> mempty -- handled by renderTable
  -- Raw HTML: strip it (don't pass through)
  HTML_BLOCK _ -> mempty
  HTML_INLINE _ -> mempty
  -- Anything else: just render children
  _ -> renderChildren children

renderChildren :: [Node] -> Builder
renderChildren = foldMap renderNode

-- | Render a list with bullet or number prefixes.
renderList :: ListAttributes -> [Node] -> Builder
renderList attrs items =
  -- Infinite index list is truncated by zipWith to the length of items
  mconcat $ zipWith (renderItem isBullet) [attrs.listStart ..] items
  where
    isBullet = case attrs.listType of
      BULLET_LIST -> True
      ORDERED_LIST -> False

renderItem :: Bool -> Int -> Node -> Builder
renderItem isBullet idx (Node _ _ children) =
  let prefix
        | isBullet = "• "
        | otherwise = B.fromText (T.pack (show idx)) <> ". "
      body = stripTrailingNewlines $ renderChildren children
   in prefix <> body <> "\n"

stripTrailingNewlines :: Builder -> Builder
stripTrailingNewlines b =
  let t = TL.toStrict $ B.toLazyText b
   in B.fromText $ T.dropWhileEnd (== '\n') t

-- | Render table rows as pipe-separated pre-formatted text.
renderTable :: [Node] -> Builder
renderTable rows = mconcat $ map renderTableRow rows

renderTableRow :: Node -> Builder
renderTableRow (Node _ _ cells) =
  let cellTexts = map renderTableCell cells
   in "| " <> B.fromText (T.intercalate " | " cellTexts) <> " |\n"

renderTableCell :: Node -> Text
renderTableCell (Node _ _ children) =
  -- Render cell content as plain text with HTML escaping, no nested block tags
  let b = renderChildren children
      t = TL.toStrict $ B.toLazyText b
   in T.strip t

-- | Strip trailing newlines from a Builder (used to clean up inner block content).
trimBlock :: Builder -> Builder
trimBlock = B.fromText . T.dropWhileEnd (== '\n') . TL.toStrict . B.toLazyText

-- | Common HTML entity escaping for content: @&@, @<@, @>@.
escapeCharCommon :: Char -> Text
escapeCharCommon '&' = "&amp;"
escapeCharCommon '<' = "&lt;"
escapeCharCommon '>' = "&gt;"
escapeCharCommon c = T.singleton c

-- | Escape text for HTML content.
escapeHtml :: Text -> Builder
escapeHtml = B.fromText . T.concatMap escapeCharCommon

-- | Escape text for use in an HTML attribute value (adds @\"@ escaping).
escapeAttr :: Text -> Text
escapeAttr = T.concatMap escapeAttrChar
  where
    escapeAttrChar '"' = "&quot;"
    escapeAttrChar c = escapeCharCommon c

-- | 'escapeAttr' as a Builder.
escapeAttrB :: Text -> Builder
escapeAttrB = B.fromText . escapeAttr
