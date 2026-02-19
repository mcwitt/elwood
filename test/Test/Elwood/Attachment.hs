module Test.Elwood.Attachment (tests) where

import Data.Aeson (object, (.=))
import Data.IORef (IORef, newIORef, readIORef)
import Data.Text (Text)
import Elwood.Logging (LogLevel (..), newLogger)
import Elwood.Memory (newMemoryStore)
import Elwood.Permissions (defaultPermissionConfig, newPermissionChecker)
import Elwood.Tools.Attachment (isPhotoExtension, queueAttachmentTool)
import Elwood.Tools.Types
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
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

-- | Create a ToolEnv for testing
mkTestToolEnv :: FilePath -> IORef [Attachment] -> IO ToolEnv
mkTestToolEnv tmpDir queue = do
  logger <- newLogger Error
  manager <- newManager tlsManagerSettings
  memStore <- newMemoryStore tmpDir
  let perms = newPermissionChecker defaultPermissionConfig tmpDir
  pure
    ToolEnv
      { teLogger = logger,
        teWorkspaceDir = tmpDir,
        teStateDir = tmpDir,
        tePermissions = perms,
        teHttpManager = manager,
        teBraveApiKey = Nothing,
        teMemoryStore = memStore,
        teChatId = Nothing,
        teRequestApproval = Nothing,
        teAttachmentQueue = queue
      }

queueAttachmentTests :: TestTree
queueAttachmentTests =
  testGroup
    "queueAttachmentTool"
    [ testCase "queues valid file" $ withSystemTempDirectory "att-test" $ \tmpDir -> do
        let filePath = tmpDir <> "/test.png"
        writeFile filePath "fake image data"
        queue <- newIORef []
        env <- mkTestToolEnv tmpDir queue
        let input = object ["path" .= (filePath :: String)]
        result <- toolExecute queueAttachmentTool env input
        assertSuccess result
        attachments <- readIORef queue
        length attachments @?= 1
        attPath (head attachments) @?= filePath
        attType (head attachments) @?= AttachAuto
        attCaption (head attachments) @?= Nothing,
      testCase "rejects missing file" $ withSystemTempDirectory "att-test" $ \tmpDir -> do
        queue <- newIORef []
        env <- mkTestToolEnv tmpDir queue
        let input = object ["path" .= ("/nonexistent/file.png" :: String)]
        result <- toolExecute queueAttachmentTool env input
        assertError result
        attachments <- readIORef queue
        length attachments @?= 0,
      testCase "parses photo type" $ withSystemTempDirectory "att-test" $ \tmpDir -> do
        let filePath = tmpDir <> "/test.pdf"
        writeFile filePath "fake pdf"
        queue <- newIORef []
        env <- mkTestToolEnv tmpDir queue
        let input = object ["path" .= (filePath :: String), "type" .= ("photo" :: Text)]
        result <- toolExecute queueAttachmentTool env input
        assertSuccess result
        attachments <- readIORef queue
        attType (head attachments) @?= AttachPhoto,
      testCase "parses document type" $ withSystemTempDirectory "att-test" $ \tmpDir -> do
        let filePath = tmpDir <> "/test.png"
        writeFile filePath "fake png"
        queue <- newIORef []
        env <- mkTestToolEnv tmpDir queue
        let input = object ["path" .= (filePath :: String), "type" .= ("document" :: Text)]
        result <- toolExecute queueAttachmentTool env input
        assertSuccess result
        attachments <- readIORef queue
        attType (head attachments) @?= AttachDocument,
      testCase "parses caption" $ withSystemTempDirectory "att-test" $ \tmpDir -> do
        let filePath = tmpDir <> "/test.png"
        writeFile filePath "fake png"
        queue <- newIORef []
        env <- mkTestToolEnv tmpDir queue
        let input = object ["path" .= (filePath :: String), "caption" .= ("My caption" :: Text)]
        result <- toolExecute queueAttachmentTool env input
        assertSuccess result
        attachments <- readIORef queue
        attCaption (head attachments) @?= Just "My caption",
      testCase "caption is Nothing when omitted" $ withSystemTempDirectory "att-test" $ \tmpDir -> do
        let filePath = tmpDir <> "/test.png"
        writeFile filePath "fake png"
        queue <- newIORef []
        env <- mkTestToolEnv tmpDir queue
        let input = object ["path" .= (filePath :: String)]
        result <- toolExecute queueAttachmentTool env input
        assertSuccess result
        attachments <- readIORef queue
        attCaption (head attachments) @?= Nothing
    ]

-- | Assert a ToolResult is ToolSuccess
assertSuccess :: ToolResult -> IO ()
assertSuccess (ToolSuccess _) = pure ()
assertSuccess (ToolError err) = assertFailure $ "Expected success but got error: " <> show err

-- | Assert a ToolResult is ToolError
assertError :: ToolResult -> IO ()
assertError (ToolError _) = pure ()
assertError (ToolSuccess s) = assertFailure $ "Expected error but got success: " <> show s
