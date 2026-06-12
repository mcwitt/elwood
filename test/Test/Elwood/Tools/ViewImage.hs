module Test.Elwood.Tools.ViewImage (tests) where

import Colog.Core (LogAction (..))
import Data.Aeson (object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Elwood.Claude.Types (ToolResultPart (..))
import Elwood.Image (maxImageFileBytes)
import Elwood.Tools.Types (Tool (..), ToolResult (..))
import Elwood.Tools.ViewImage (mkViewImageTool)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Elwood.TestImage (mkPngBytes)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Tools.ViewImage"
    [ testCase "returns text and image parts for a PNG" $
        withWorkspace $ \ws -> do
          let png = mkPngBytes 50 40
          BS.writeFile (ws </> "photo.png") png
          result <- runViewImage ws (ws </> "photo.png")
          case result of
            ToolSuccess [ToolResultText _, ToolResultImage "image/png" b64] ->
              B64.decode (TE.encodeUtf8 b64) @?= Right png
            other -> assertFailure $ "Expected text + image parts, got: " <> show other,
      testCase "resolves relative paths against the workspace" $
        withWorkspace $ \ws -> do
          BS.writeFile (ws </> "rel.png") (mkPngBytes 10 10)
          result <- runViewImage ws "rel.png"
          case result of
            ToolSuccess [_, ToolResultImage "image/png" _] -> pure ()
            other -> assertFailure $ "Expected image part, got: " <> show other,
      testCase "missing file is an error" $
        withWorkspace $ \ws -> do
          result <- runViewImage ws "nope.png"
          case result of
            ToolError _ -> pure ()
            other -> assertFailure $ "Expected error, got: " <> show other,
      testCase "unsupported extension is an error" $
        withWorkspace $ \ws -> do
          BS.writeFile (ws </> "doc.pdf") "%PDF"
          result <- runViewImage ws "doc.pdf"
          case result of
            ToolError _ -> pure ()
            other -> assertFailure $ "Expected error, got: " <> show other,
      testCase "file larger than the read cap is an error" $
        withWorkspace $ \ws -> do
          BS.writeFile (ws </> "huge.png") (BS.replicate (maxImageFileBytes + 1) 0)
          result <- runViewImage ws "huge.png"
          case result of
            ToolError err -> assertBool "mentions too large" (T.isInfixOf "too large" err)
            other -> assertFailure $ "Expected error, got: " <> show other,
      testCase "missing path parameter is an error" $
        withWorkspace $ \ws -> do
          let tool = mkViewImageTool (LogAction (const (pure ()))) ws
          result <- tool.execute (object [])
          case result of
            ToolError _ -> pure ()
            other -> assertFailure $ "Expected error, got: " <> show other
    ]

withWorkspace :: (FilePath -> IO a) -> IO a
withWorkspace = withSystemTempDirectory "view-image-test"

runViewImage :: FilePath -> FilePath -> IO ToolResult
runViewImage ws p = do
  let tool = mkViewImageTool (LogAction (const (pure ()))) ws
  tool.execute (object ["path" .= (p :: String)])
