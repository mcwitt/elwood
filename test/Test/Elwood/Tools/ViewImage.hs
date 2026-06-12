module Test.Elwood.Tools.ViewImage (tests) where

import Codec.Picture (encodePng, generateImage)
import Codec.Picture.Types (PixelRGBA8 (..))
import Colog.Core (LogAction (..))
import Data.Aeson (object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as TE
import Elwood.Claude.Types (ToolResultPart (..))
import Elwood.Tools.Types (Tool (..), ToolResult (..))
import Elwood.Tools.ViewImage (mkViewImageTool)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
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

mkPngBytes :: Int -> Int -> BS.ByteString
mkPngBytes w h =
  let img = generateImage (\x y -> PixelRGBA8 (fromIntegral x) (fromIntegral y) 128 255) w h
   in LBS.toStrict (encodePng img)
