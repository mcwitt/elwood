module Test.Elwood.Telegram.Client (tests) where

import Data.Text qualified as T
import Elwood.Telegram.Client (splitForTelegram, splitMessage, splitMessageAt, telegramApiLimit)
import Elwood.Telegram.Markdown (markdownToTelegramHtml)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Telegram.Client"
    [ testGroup "splitMessage" splitMessageTests,
      testGroup "splitMessageAt" splitMessageAtTests,
      testGroup "splitForTelegram" splitForTelegramTests
    ]

splitMessageTests :: [TestTree]
splitMessageTests =
  [ testCase "short message is not split" $ do
      let chunks = splitMessage "hello"
      length chunks @?= 1,
    testCase "message at limit is not split" $ do
      let msg = T.replicate 4000 "x"
      length (splitMessage msg) @?= 1,
    testCase "message over limit is split" $ do
      let msg = T.replicate 4001 "x"
      assertBool "should produce multiple chunks" $ length (splitMessage msg) > 1,
    testCase "splits on paragraph boundary" $ do
      let msg = T.replicate 3000 "x" <> "\n\n" <> T.replicate 2000 "y"
          chunks = splitMessage msg
      length chunks @?= 2
      assertBool "first chunk ends with x's" $ T.all (== 'x') (T.strip (head chunks)),
    testCase "all text is preserved" $ do
      let msg = T.replicate 3000 "a" <> "\n\n" <> T.replicate 3000 "b"
          chunks = splitMessage msg
          reassembled = T.concat chunks
      T.filter (/= '\n') reassembled @?= T.filter (/= '\n') msg
  ]

splitMessageAtTests :: [TestTree]
splitMessageAtTests =
  [ testCase "custom limit respected" $ do
      let chunks = splitMessageAt 10 "hello world, this is a test"
      assertBool "all chunks <= 10 chars" $ all ((<= 10) . T.length) chunks,
    testCase "halving produces smaller chunks" $ do
      let msg = T.replicate 100 "x"
          chunks = splitMessageAt 50 msg
      assertBool "should produce 2 chunks" $ length chunks == 2
  ]

splitForTelegramTests :: [TestTree]
splitForTelegramTests =
  [ testCase "heading-heavy message fits after re-split" $ do
      let headings = T.intercalate "\n\n" ["# Heading number " <> T.pack (show n) | n <- [1 :: Int .. 200]]
          chunks = splitForTelegram headings
          htmlChunks = map markdownToTelegramHtml chunks
      assertBool
        ("all HTML chunks <= " <> show telegramApiLimit)
        $ all ((<= telegramApiLimit) . T.length) htmlChunks,
    testCase "inline-code-heavy message fits after re-split" $ do
      let codes = T.intercalate " " $ replicate 500 "`x`"
          chunks = splitForTelegram codes
          htmlChunks = map markdownToTelegramHtml chunks
      assertBool
        ("all HTML chunks <= " <> show telegramApiLimit)
        $ all ((<= telegramApiLimit) . T.length) htmlChunks,
    testCase "long plain text needs no extra splitting" $ do
      let msg = T.replicate 6000 "a"
          initialChunks = splitMessage msg
          telegramChunks = splitForTelegram msg
      length telegramChunks @?= length initialChunks
  ]
