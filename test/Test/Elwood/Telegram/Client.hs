module Test.Elwood.Telegram.Client (tests) where

import Colog.Core (LogAction (..))
import Control.Exception (try)
import Data.Aeson (Value (..), eitherDecode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (ByteString)
import Data.IORef
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Elwood.Telegram.Client
  ( TelegramClient (..),
    TelegramError (..),
    newClient,
    sendMessageHtml,
    sendMessageWithKeyboard,
    splitForTelegram,
    splitMessage,
    splitMessageAt,
    telegramApiLimit,
  )
import Elwood.Telegram.Markdown (markdownToTelegramHtml)
import Elwood.Telegram.Types (AttachmentKind (..), InboundAttachment (..), InlineKeyboardMarkup (..), Message (..))
import Network.HTTP.Types.Status (status200, status400)
import Network.Wai (responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Telegram.Client"
    [ transportTests,
      testGroup "splitMessage" splitMessageTests,
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
  [ testCase "non-BMP characters fit the UTF-16 limit without losing text" $ do
      let original = T.replicate 5000 "😀"
          chunks = splitForTelegram original
      T.concat chunks @?= original
      assertBool "UTF-16 limit" $ all ((<= telegramApiLimit * 2) . BS.length . TE.encodeUtf16LE . markdownToTelegramHtml) chunks,
    testCase "heading-heavy message fits after re-split" $ do
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
          map (.fileName) m.attachments @?= [Just "report.pdf"]
          map (.mimeType) m.attachments @?= [Just "application/pdf"]
          map (.fileUniqueId) m.attachments @?= ["U1"],
    testCase "parses a voice message" $ do
      let json = "{\"message_id\":2,\"chat\":{\"id\":5,\"type\":\"private\"},\"voice\":{\"file_id\":\"V1\",\"file_unique_id\":\"U2\",\"duration\":5,\"mime_type\":\"audio/ogg\",\"file_size\":1024}}" :: ByteString
      case eitherDecode json of
        Left e -> assertFailure e
        Right (m :: Message) -> do
          map (.kind) m.attachments @?= [KindVoice]
          map (.mimeType) m.attachments @?= [Just "audio/ogg"],
    testCase "parses an audio message" $ do
      let json = "{\"message_id\":3,\"chat\":{\"id\":5,\"type\":\"private\"},\"audio\":{\"file_id\":\"A1\",\"file_unique_id\":\"U3\",\"duration\":120,\"file_name\":\"song.mp3\",\"mime_type\":\"audio/mpeg\"}}" :: ByteString
      case eitherDecode json of
        Left e -> assertFailure e
        Right (m :: Message) -> do
          map (.fileName) m.attachments @?= [Just "song.mp3"]
          map (.fileSize) m.attachments @?= [Nothing],
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
          m.attachments @?= []
  ]

transportTests :: TestTree
transportTests =
  testGroup
    "transport contracts"
    [ testCase "HTML rejection retries once as plain text" $ do
        calls <- newIORef ([] :: [Value])
        let application request respond = do
              body <- strictRequestBody request
              value <- either (fail . show) pure (eitherDecode body)
              prior <- atomicModifyIORef' calls (\old -> (old <> [value], old))
              respond $
                if null prior
                  then responseLBS status400 [] "{\"description\":\"can't parse entities\"}"
                  else responseLBS status200 [] "{\"ok\":true,\"result\":{\"message_id\":1}}"
        testWithApplication (pure application) $ \port -> do
          client <- testClient port
          sendMessageHtml client 1 "<invalid>" "plain"
        values <- readIORef calls
        case values of
          [Object html, Object plain] -> do
            KM.lookup "parse_mode" html @?= Just (String "HTML")
            KM.lookup "parse_mode" plain @?= Nothing
            KM.lookup "text" plain @?= Just (String "plain")
          _ -> assertFailure (show values),
      testCase "keyboard send rejects a missing message ID" $ do
        let application _ respond = respond (responseLBS status200 [] "{\"ok\":true,\"result\":{}}")
        testWithApplication (pure application) $ \port -> do
          client <- testClient port
          result <- try @TelegramError (sendMessageWithKeyboard client 1 "approve" (InlineKeyboardMarkup []))
          case result of
            Left (TelegramParseError _) -> pure ()
            other -> assertFailure (show other)
    ]
  where
    testClient port = do
      client <- newClient (LogAction (const (pure ()))) "token"
      pure client {tcBaseUrl = "http://127.0.0.1:" <> show port}
