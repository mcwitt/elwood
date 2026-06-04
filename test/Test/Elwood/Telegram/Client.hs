module Test.Elwood.Telegram.Client (tests) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Data.Text qualified as T
import Elwood.Telegram.Client (splitForTelegram, splitMessage, splitMessageAt, telegramApiLimit)
import Elwood.Telegram.Markdown (markdownToTelegramHtml)
import Elwood.Telegram.Types (Audio (..), Document (..), Message (..), Voice (..))
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Telegram.Client"
    [ testGroup "splitMessage" splitMessageTests,
      testGroup "splitMessageAt" splitMessageAtTests,
      testGroup "splitForTelegram" splitForTelegramTests,
      testGroup "media FromJSON" mediaFromJsonTests
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

mediaFromJsonTests :: [TestTree]
mediaFromJsonTests =
  [ testCase "parses a document message" $ do
      let json = "{\"message_id\":1,\"chat\":{\"id\":5,\"type\":\"private\"},\"caption\":\"file\",\"document\":{\"file_id\":\"D1\",\"file_unique_id\":\"U1\",\"file_name\":\"report.pdf\",\"mime_type\":\"application/pdf\",\"file_size\":2048}}" :: ByteString
      case eitherDecode json of
        Left e -> assertFailure e
        Right (m :: Message) -> do
          (m.document >>= (.fileName)) @?= Just "report.pdf"
          (m.document >>= (.mimeType)) @?= Just "application/pdf"
          fmap (.fileUniqueId) m.document @?= Just "U1",
    testCase "parses a voice message" $ do
      let json = "{\"message_id\":2,\"chat\":{\"id\":5,\"type\":\"private\"},\"voice\":{\"file_id\":\"V1\",\"file_unique_id\":\"U2\",\"duration\":5,\"mime_type\":\"audio/ogg\",\"file_size\":1024}}" :: ByteString
      case eitherDecode json of
        Left e -> assertFailure e
        Right (m :: Message) -> do
          fmap (.duration) m.voice @?= Just 5
          (m.voice >>= (.mimeType)) @?= Just "audio/ogg",
    testCase "parses an audio message" $ do
      let json = "{\"message_id\":3,\"chat\":{\"id\":5,\"type\":\"private\"},\"audio\":{\"file_id\":\"A1\",\"file_unique_id\":\"U3\",\"duration\":120,\"file_name\":\"song.mp3\",\"mime_type\":\"audio/mpeg\"}}" :: ByteString
      case eitherDecode json of
        Left e -> assertFailure e
        Right (m :: Message) -> do
          (m.audio >>= (.fileName)) @?= Just "song.mp3"
          (m.audio >>= (.fileSize)) @?= Nothing,
    testCase "voice missing required duration fails to parse" $ do
      let json = "{\"message_id\":5,\"chat\":{\"id\":5,\"type\":\"private\"},\"voice\":{\"file_id\":\"V\",\"file_unique_id\":\"U\"}}" :: ByteString
      case eitherDecode json :: Either String Message of
        Left _ -> pure ()
        Right _ -> assertFailure "expected parse failure for voice missing duration",
    testCase "plain text message has no media" $ do
      let json = "{\"message_id\":4,\"chat\":{\"id\":5,\"type\":\"private\"},\"text\":\"hi\"}" :: ByteString
      case eitherDecode json of
        Left e -> assertFailure e
        Right (m :: Message) -> do
          (m.document, m.voice, m.audio) @?= (Nothing, Nothing, Nothing)
  ]
