module Test.Elwood.Telegram.Inbox (tests) where

import Data.Aeson (eitherDecode, encode, object, (.=))
import Data.Aeson.Types (Pair)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Elwood.Telegram.Inbox
  ( AttachmentKind (..),
    InboundAttachment (..),
    inboxFileName,
    messageAttachments,
    writeInboxFile,
  )
import Elwood.Telegram.Types (Message)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

messageWith :: [Pair] -> IO Message
messageWith fields =
  either fail pure $
    eitherDecode $
      encode $
        object
          (["message_id" .= (100 :: Int), "chat" .= object ["id" .= (1 :: Int), "type" .= ("private" :: Text)]] <> fields)

tests :: TestTree
tests =
  testGroup
    "Telegram.Inbox"
    [ testGroup "messageAttachments" messageAttachmentsTests,
      testGroup "inboxFileName" inboxFileNameTests,
      testGroup "writeInboxFile" writeInboxFileTests
    ]

messageAttachmentsTests :: [TestTree]
messageAttachmentsTests =
  [ testCase "no media yields no attachments" $ do
      message <- messageWith []
      messageAttachments message @?= [],
    testCase "picks the largest photo only" $ do
      message <- messageWith ["photo" .= [media "small" 100 photo, media "big" 9000 photo]]
      map (.fileUniqueId) (messageAttachments message) @?= ["big"]
      map (.kind) (messageAttachments message) @?= [KindPhoto],
    testCase "document maps to one KindDocument attachment" $ do
      message <- messageWith ["document" .= media "doc" 10 ["file_name" .= ("r.pdf" :: Text), "mime_type" .= ("application/pdf" :: Text)]]
      map (.kind) (messageAttachments message) @?= [KindDocument]
      map (.fileName) (messageAttachments message) @?= [Just "r.pdf"],
    testCase "voice and audio both surface" $ do
      message <- messageWith ["voice" .= media "voice" 5 timed, "audio" .= media "audio" 7 timed]
      map (.kind) (messageAttachments message) @?= [KindVoice, KindAudio],
    testCase "all media kinds surface in order photo, document, voice, audio" $ do
      message <- messageWith ["photo" .= [media "photo" 9 photo], "document" .= media "doc" 10 [], "voice" .= media "voice" 5 timed, "audio" .= media "audio" 7 timed]
      map (.kind) (messageAttachments message) @?= [KindPhoto, KindDocument, KindVoice, KindAudio]
  ]
  where
    photo = ["width" .= (1280 :: Int), "height" .= (1280 :: Int)]
    timed = ["duration" .= (3 :: Int)]
    media name size extra = object (["file_id" .= (name :: Text), "file_unique_id" .= name, "file_size" .= (size :: Int)] <> extra)

photoAtt :: InboundAttachment
photoAtt = InboundAttachment {fileId = "fid", fileUniqueId = "AgADabc", kind = KindPhoto, fileName = Nothing, mimeType = Nothing, fileSize = Just 1}

docAtt :: InboundAttachment
docAtt = InboundAttachment {fileId = "fid2", fileUniqueId = "BQADxyz", kind = KindDocument, fileName = Just "r.pdf", mimeType = Just "application/pdf", fileSize = Just 1}

voiceAtt :: InboundAttachment
voiceAtt = InboundAttachment {fileId = "v", fileUniqueId = "UV", kind = KindVoice, fileName = Nothing, mimeType = Nothing, fileSize = Just 1}

audioAtt :: InboundAttachment
audioAtt = InboundAttachment {fileId = "a", fileUniqueId = "UA", kind = KindAudio, fileName = Nothing, mimeType = Nothing, fileSize = Just 1}

-- | Override the fileUniqueId field without triggering -Wambiguous-fields.
withUniqueId :: Text -> InboundAttachment -> InboundAttachment
withUniqueId uid att = InboundAttachment att.fileId uid att.kind att.fileName att.mimeType att.fileSize

inboxFileNameTests :: [TestTree]
inboxFileNameTests =
  [ testCase "extension comes from the Telegram file path" $
      inboxFileName 482 photoAtt (Just "photos/file_0.jpg") @?= "inbox/482-AgADabc.jpg",
    testCase "falls back to mime type when file path has no extension" $
      inboxFileName 482 docAtt (Just "files/document_0") @?= "inbox/482-BQADxyz.pdf",
    testCase "falls back to mime type when no file path" $
      inboxFileName 482 docAtt Nothing @?= "inbox/482-BQADxyz.pdf",
    testCase "falls back to per-kind default when nothing else" $
      inboxFileName 7 photoAtt Nothing @?= "inbox/7-AgADabc.jpg",
    testCase "message id prefix keeps otherwise-identical files distinct" $
      inboxFileName 5 photoAtt Nothing /= inboxFileName 6 photoAtt Nothing @?= True,
    testCase "voice falls back to .ogg default" $
      inboxFileName 1 voiceAtt Nothing @?= "inbox/1-UV.ogg",
    testCase "audio falls back to .mp3 default" $
      inboxFileName 1 audioAtt Nothing @?= "inbox/1-UA.mp3",
    testCase "hostile unique id cannot escape the inbox dir" $
      inboxFileName 1 (withUniqueId "../../etc/passwd" photoAtt) Nothing @?= "inbox/1-______etc_passwd.jpg"
  ]

writeInboxFileTests :: [TestTree]
writeInboxFileTests =
  [ testCase "writes bytes and creates nested dirs" $
      withSystemTempDirectory "inbox-test" $ \dir -> do
        let rel = "inbox" </> "100-AgADabc.jpg"
            payload = BS.pack [1, 2, 3, 4]
        writeInboxFile dir rel payload
        exists <- doesFileExist (dir </> rel)
        assertBool "file should exist" exists
        readBack <- BS.readFile (dir </> rel)
        readBack @?= payload
  ]
