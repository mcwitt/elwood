module Test.Elwood.Telegram.Inbox (tests) where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Elwood.Telegram.Inbox
  ( AttachmentKind (..),
    InboundAttachment (..),
    inboxFileName,
    messageAttachments,
    writeInboxFile,
  )
import Elwood.Telegram.Types
  ( Audio (..),
    Chat (..),
    ChatType (..),
    Document (..),
    Message (..),
    PhotoSize (..),
    Voice (..),
  )
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

-- A message with all media fields empty; override one field per test.
baseMessage :: Message
baseMessage =
  Message
    { id_ = 100,
      chat = Chat {id_ = 1, type_ = Private},
      text = Nothing,
      from_ = Nothing,
      photo = Nothing,
      caption = Nothing,
      document = Nothing,
      voice = Nothing,
      audio = Nothing
    }

photoSize :: Int -> Text -> Maybe Int -> PhotoSize
photoSize w fuid sz = PhotoSize {fileId = fuid <> "-id", fileUniqueId = fuid, width = w, height = w, fileSize = sz}

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
  [ testCase "no media yields no attachments" $
      messageAttachments baseMessage @?= [],
    testCase "picks the largest photo only" $ do
      let m = baseMessage {photo = Just [photoSize 90 "small" (Just 100), photoSize 1280 "big" (Just 9000)]}
          atts = messageAttachments m
      map (.fileUniqueId) atts @?= ["big"]
      map (.kind) atts @?= [KindPhoto],
    testCase "document maps to one KindDocument attachment" $ do
      let d = Document {fileId = "D", fileUniqueId = "U", fileName = Just "r.pdf", mimeType = Just "application/pdf", fileSize = Just 10}
          atts = messageAttachments baseMessage {document = Just d}
      map (.kind) atts @?= [KindDocument]
      map (.fileName) atts @?= [Just "r.pdf"],
    testCase "voice and audio both surface" $ do
      let v = Voice {fileId = "V", fileUniqueId = "UV", duration = 3, mimeType = Just "audio/ogg", fileSize = Just 5}
          a = Audio {fileId = "A", fileUniqueId = "UA", duration = 9, fileName = Nothing, mimeType = Just "audio/mpeg", fileSize = Just 7}
          atts = messageAttachments baseMessage {voice = Just v, audio = Just a}
      map (.kind) atts @?= [KindVoice, KindAudio]
  ]

photoAtt :: InboundAttachment
photoAtt = InboundAttachment {fileId = "fid", fileUniqueId = "AgADabc", kind = KindPhoto, fileName = Nothing, mimeType = Nothing, fileSize = Just 1}

docAtt :: InboundAttachment
docAtt = InboundAttachment {fileId = "fid2", fileUniqueId = "BQADxyz", kind = KindDocument, fileName = Just "r.pdf", mimeType = Just "application/pdf", fileSize = Just 1}

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
      inboxFileName 5 photoAtt Nothing /= inboxFileName 6 photoAtt Nothing @?= True
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
