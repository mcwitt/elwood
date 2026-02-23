module Test.Elwood.Attachment (tests) where

import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import Elwood.Logging (LogLevel (..), newLogger)
import Elwood.Tools.Attachment (isPhotoExtension, mkQueueAttachmentTool)
import Elwood.Tools.Types
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Attachment"
    [ isPhotoExtensionTests,
      queueAttachmentTests
    ]

isPhotoExtensionTests :: TestTree
isPhotoExtensionTests =
  testGroup
    "isPhotoExtension"
    [ testCase ".png is photo" $ isPhotoExtension "image.png" @?= True,
      testCase ".jpg is photo" $ isPhotoExtension "photo.jpg" @?= True,
      testCase ".jpeg is photo" $ isPhotoExtension "photo.jpeg" @?= True,
      testCase ".gif is photo" $ isPhotoExtension "anim.gif" @?= True,
      testCase ".webp is photo" $ isPhotoExtension "pic.webp" @?= True,
      testCase ".PNG is photo (case insensitive)" $ isPhotoExtension "IMAGE.PNG" @?= True,
      testCase ".pdf is not photo" $ isPhotoExtension "doc.pdf" @?= False,
      testCase ".csv is not photo" $ isPhotoExtension "data.csv" @?= False,
      testCase ".txt is not photo" $ isPhotoExtension "notes.txt" @?= False,
      testCase "no extension is not photo" $ isPhotoExtension "noext" @?= False
    ]

queueAttachmentTests :: TestTree
queueAttachmentTests =
  testGroup
    "queueAttachmentTool"
    [ testCase "queues valid file" $ withSystemTempDirectory "att-test" $ \tmpDir -> do
        let filePath = tmpDir <> "/test.png"
        writeFile filePath "fake image data"
        queue <- newTVarIO []
        logger <- newLogger Error
        let tool = mkQueueAttachmentTool logger queue
            input = object ["path" .= (filePath :: String)]
        result <- tool.execute input
        assertSuccess result
        attachments <- readTVarIO queue
        length attachments @?= 1
        let att = head attachments
        att.path @?= filePath
        att.type_ @?= AttachAuto
        att.caption @?= (Nothing :: Maybe Text),
      testCase "rejects missing file" $ withSystemTempDirectory "att-test" $ \tmpDir -> do
        queue <- newTVarIO []
        logger <- newLogger Error
        let tool = mkQueueAttachmentTool logger queue
            input = object ["path" .= ("/nonexistent/file.png" :: String)]
        result <- tool.execute input
        assertError result
        attachments <- readTVarIO queue
        length attachments @?= 0,
      testCase "parses photo type" $ withSystemTempDirectory "att-test" $ \tmpDir -> do
        let filePath = tmpDir <> "/test.pdf"
        writeFile filePath "fake pdf"
        queue <- newTVarIO []
        logger <- newLogger Error
        let tool = mkQueueAttachmentTool logger queue
            input = object ["path" .= (filePath :: String), "type" .= ("photo" :: Text)]
        result <- tool.execute input
        assertSuccess result
        attachments <- readTVarIO queue
        let att = head attachments
        att.type_ @?= AttachPhoto,
      testCase "parses document type" $ withSystemTempDirectory "att-test" $ \tmpDir -> do
        let filePath = tmpDir <> "/test.png"
        writeFile filePath "fake png"
        queue <- newTVarIO []
        logger <- newLogger Error
        let tool = mkQueueAttachmentTool logger queue
            input = object ["path" .= (filePath :: String), "type" .= ("document" :: Text)]
        result <- tool.execute input
        assertSuccess result
        attachments <- readTVarIO queue
        let att = head attachments
        att.type_ @?= AttachDocument,
      testCase "parses caption" $ withSystemTempDirectory "att-test" $ \tmpDir -> do
        let filePath = tmpDir <> "/test.png"
        writeFile filePath "fake png"
        queue <- newTVarIO []
        logger <- newLogger Error
        let tool = mkQueueAttachmentTool logger queue
            input = object ["path" .= (filePath :: String), "caption" .= ("My caption" :: Text)]
        result <- tool.execute input
        assertSuccess result
        attachments <- readTVarIO queue
        let att = head attachments
        att.caption @?= Just "My caption",
      testCase "caption is Nothing when omitted" $ withSystemTempDirectory "att-test" $ \tmpDir -> do
        let filePath = tmpDir <> "/test.png"
        writeFile filePath "fake png"
        queue <- newTVarIO []
        logger <- newLogger Error
        let tool = mkQueueAttachmentTool logger queue
            input = object ["path" .= (filePath :: String)]
        result <- tool.execute input
        assertSuccess result
        attachments <- readTVarIO queue
        let att = head attachments
        att.caption @?= (Nothing :: Maybe Text)
    ]

-- | Assert a ToolResult is ToolSuccess
assertSuccess :: ToolResult -> IO ()
assertSuccess (ToolSuccess _) = pure ()
assertSuccess (ToolError err) = assertFailure $ "Expected success but got error: " <> show err

-- | Assert a ToolResult is ToolError
assertError :: ToolResult -> IO ()
assertError (ToolError _) = pure ()
assertError (ToolSuccess s) = assertFailure $ "Expected error but got success: " <> show s
