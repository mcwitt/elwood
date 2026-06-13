module Test.Elwood.Telegram.ToolUse (tests) where

import Data.Aeson (Value (..), object, (.=))
import Data.Char (ord)
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Telegram.ToolUse (ToolUseNote (..), formatToolUseNote)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Telegram.ToolUse"
    [ testCase "single call renders header and expandable blockquote" $ do
        let note = formatToolUseNote Nothing [("run_command", object ["command" .= ("git status" :: Text)])]
        assertBool "has tool name in code tags" ("\128295 <code>run_command</code>" `T.isInfixOf` note.html)
        assertBool "has expandable blockquote" ("<blockquote expandable>" `T.isInfixOf` note.html)
        assertBool "has pretty arg" ("\"command\": \"git status\"" `T.isInfixOf` note.html),
      testCase "empty args render name line without blockquote" $ do
        let note = formatToolUseNote Nothing [("get_time", object [])]
        note.html @?= "\128295 <code>get_time</code>"
        note.plain @?= "\128295 get_time",
      testCase "null args render name line without blockquote" $ do
        let note = formatToolUseNote Nothing [("get_time", Null)]
        note.html @?= "\128295 <code>get_time</code>",
      testCase "HTML special characters in args are escaped" $ do
        let note = formatToolUseNote Nothing [("write_file", object ["content" .= ("<b>1 & 2</b>" :: Text)])]
        assertBool "escapes <" ("&lt;b&gt;" `T.isInfixOf` note.html)
        assertBool "escapes &" ("1 &amp; 2" `T.isInfixOf` note.html)
        assertBool "raw tag absent" (not ("<b>1" `T.isInfixOf` note.html)),
      testCase "long args are truncated with marker" $ do
        let big = T.replicate 3000 "x"
            note = formatToolUseNote Nothing [("write_file", object ["content" .= big])]
        assertBool "has truncation marker" ("\8230 (truncated)" `T.isInfixOf` note.html)
        assertBool "args bounded" (T.length note.plain < 1200),
      testCase "delegate label renders bold prefix" $ do
        let note = formatToolUseNote (Just "research task") [("web_search", object ["query" .= ("cats" :: Text)])]
        assertBool "bold label" ("\128295 <b>research task</b>: <code>web_search</code>" `T.isInfixOf` note.html)
        assertBool "plain label" ("\128295 research task: web_search" `T.isInfixOf` note.plain),
      testCase "multiple calls are each listed" $ do
        let note = formatToolUseNote Nothing [(n, object []) | n <- ["a", "b", "c", "d", "e", "f"]]
        assertBool "first listed" ("<code>a</code>" `T.isInfixOf` note.html)
        assertBool "sixth listed (no + N others)" ("<code>f</code>" `T.isInfixOf` note.html),
      testCase "calls beyond the message budget are elided with marker" $ do
        let big = T.replicate 2000 "y"
            calls = [(T.pack ("tool" <> show i), object ["data" .= big]) | i <- [1 :: Int .. 8]]
            note = formatToolUseNote Nothing calls
        assertBool "has elision marker" ("\8230 +" `T.isInfixOf` note.plain)
        assertBool "plain bounded under telegram limit" (T.length note.plain <= 4096),
      testCase "astral-heavy args stay within the UTF-16 message limit" $ do
        -- Each emoji is one code point but two UTF-16 code units; the budget
        -- must count units, not code points (Telegram measures units).
        let emoji = T.replicate 900 "\128512" -- U+1F600, astral
            calls = [(T.pack ("tool" <> show i), object ["data" .= emoji]) | i <- [1 :: Int .. 8]]
            note = formatToolUseNote Nothing calls
        assertBool "plain within UTF-16 limit" (utf16Len note.plain <= 4096),
      testCase "plain rendering contains no HTML tags" $ do
        let note = formatToolUseNote (Just "task") [("run_command", object ["command" .= ("ls" :: Text)])]
        assertBool "no tags" (not ("<" `T.isInfixOf` note.plain))
    ]

-- | UTF-16 code-unit length (astral code points count as two).
utf16Len :: Text -> Int
utf16Len = T.foldl' (\n c -> n + if ord c >= 0x10000 then 2 else 1) 0
