module Test.Elwood.Telegram.Markdown (tests) where

import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Telegram.Markdown (markdownToTelegramHtml)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Telegram.Markdown"
    [ testGroup "inline formatting" inlineTests,
      testGroup "code" codeTests,
      testGroup "links" linkTests,
      testGroup "headings" headingTests,
      testGroup "lists" listTests,
      testGroup "block quotes" blockQuoteTests,
      testGroup "tables" tableTests,
      testGroup "images" imageTests,
      testGroup "raw HTML stripping" rawHtmlTests,
      testGroup "HTML escaping" escapingTests,
      testGroup "edge cases" edgeCaseTests
    ]

-- Helper: trim trailing whitespace for comparison
trim :: Text -> Text
trim = T.stripEnd

convert :: Text -> Text
convert = trim . markdownToTelegramHtml

inlineTests :: [TestTree]
inlineTests =
  [ testCase "bold" $
      convert "**bold**" @?= "<b>bold</b>",
    testCase "italic" $
      convert "*italic*" @?= "<i>italic</i>",
    testCase "strikethrough" $
      convert "~~strike~~" @?= "<s>strike</s>",
    testCase "inline code" $
      convert "`code`" @?= "<code>code</code>",
    testCase "nested bold and italic" $
      convert "***bold italic***" @?= "<i><b>bold italic</b></i>"
  ]

codeTests :: [TestTree]
codeTests =
  [ testCase "code block without language" $
      convert "```\nfoo\n```" @?= "<pre><code>foo\n</code></pre>",
    testCase "code block with language" $
      convert "```python\nprint(1)\n```" @?= "<pre><code class=\"language-python\">print(1)\n</code></pre>",
    testCase "HTML chars inside code block" $
      convert "```\na < b && c > d\n```" @?= "<pre><code>a &lt; b &amp;&amp; c &gt; d\n</code></pre>",
    testCase "HTML chars in inline code" $
      convert "`<div>`" @?= "<code>&lt;div&gt;</code>"
  ]

linkTests :: [TestTree]
linkTests =
  [ testCase "standard link" $
      convert "[text](https://example.com)" @?= "<a href=\"https://example.com\">text</a>",
    testCase "link with ampersand in URL" $
      convert "[q](https://example.com?a=1&b=2)" @?= "<a href=\"https://example.com?a=1&amp;b=2\">q</a>"
  ]

headingTests :: [TestTree]
headingTests =
  [ testCase "h1 becomes bold" $
      convert "# Heading" @?= "<b>Heading</b>",
    testCase "h2 becomes bold" $
      convert "## Heading 2" @?= "<b>Heading 2</b>",
    testCase "h3 becomes bold" $
      convert "### Heading 3" @?= "<b>Heading 3</b>"
  ]

listTests :: [TestTree]
listTests =
  [ testCase "bullet list" $
      convert "- one\n- two\n- three"
        @?= T.intercalate "\n" ["• one", "• two", "• three"],
    testCase "ordered list" $
      convert "1. first\n2. second\n3. third"
        @?= T.intercalate "\n" ["1. first", "2. second", "3. third"]
  ]

blockQuoteTests :: [TestTree]
blockQuoteTests =
  [ testCase "block quote" $
      convert "> quoted text" @?= "<blockquote>quoted text</blockquote>"
  ]

tableTests :: [TestTree]
tableTests =
  [ testCase "simple table renders as pre" $
      let result = convert "| A | B |\n|---|---|\n| 1 | 2 |"
       in do
            assertBool "wrapped in pre" ("<pre>" `T.isPrefixOf` result)
            assertBool "contains pipe-separated cells" ("| 1 | 2 |" `T.isInfixOf` result),
    testCase "table with formatting in cells" $
      let result = convert "| Name | Value |\n|---|---|\n| **x** | `y` |"
       in do
            assertBool "wrapped in pre" ("<pre>" `T.isPrefixOf` result)
            assertBool "contains bold" ("<b>x</b>" `T.isInfixOf` result)
            assertBool "contains code" ("<code>y</code>" `T.isInfixOf` result)
  ]

imageTests :: [TestTree]
imageTests =
  [ testCase "image becomes link" $
      convert "![alt text](https://example.com/img.png)"
        @?= "<a href=\"https://example.com/img.png\">alt text</a>"
  ]

rawHtmlTests :: [TestTree]
rawHtmlTests =
  [ testCase "HTML block is stripped" $
      convert "<div>hello</div>" @?= "",
    testCase "inline HTML tags are stripped" $
      -- cmark parses <span> as HTML_INLINE; our renderer strips the tags
      -- but the text between open/close tags is a separate TEXT node kept by cmark
      convert "before <span>mid</span> after" @?= "before mid after"
  ]

escapingTests :: [TestTree]
escapingTests =
  [ testCase "ampersand in text" $
      convert "a & b" @?= "a &amp; b",
    testCase "angle brackets in text" $
      convert "a < b > c" @?= "a &lt; b &gt; c"
  ]

edgeCaseTests :: [TestTree]
edgeCaseTests =
  [ testCase "empty string" $
      convert "" @?= "",
    testCase "plain text no markdown" $
      convert "hello world" @?= "hello world",
    testCase "multiple paragraphs" $
      convert "para one\n\npara two" @?= "para one\n\npara two"
  ]
