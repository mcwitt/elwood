module Test.Elwood.Image (tests) where

import Codec.Picture (decodeImage)
import Codec.Picture.Types (dynamicMap, imageHeight, imageWidth)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Elwood.Event.Types (Base64Data (..), ImageData (..), MediaType (..))
import Elwood.Image (ResizeResult (..), imageMediaTypeFromPath, perceiveImageBytes, resizeImage, sniffImageMediaType)
import Test.Elwood.TestImage (mkJpegBytes, mkPngBytes)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Image"
    [ resizeTests,
      perceiveTests,
      sniffTests,
      mediaTypeTests
    ]

perceiveTests :: TestTree
perceiveTests =
  testGroup
    "perceiveImageBytes"
    [ testCase "small image passes through unchanged" $ do
        let img = mkPngBytes 100 80
        case perceiveImageBytes (Just 200) img of
          Left err -> assertFailure $ "Expected Right, got: " <> show err
          Right d -> do
            d.mediaType @?= MediaType "image/png"
            B64.decode (TE.encodeUtf8 d.base64Data.unBase64Data) @?= Right img,
      testCase "oversized image is resized within max dimension" $ do
        let img = mkPngBytes 400 200
        case perceiveImageBytes (Just 200) img of
          Left err -> assertFailure $ "Expected Right, got: " <> show err
          Right d -> case B64.decode (TE.encodeUtf8 d.base64Data.unBase64Data) of
            Left err -> assertFailure $ "Invalid base64: " <> err
            Right bytes -> assertResizedWithin 200 bytes,
      testCase "no max dimension leaves bytes unchanged" $ do
        let img = mkPngBytes 400 200
        case perceiveImageBytes Nothing img of
          Left err -> assertFailure $ "Expected Right, got: " <> show err
          Right d -> B64.decode (TE.encodeUtf8 d.base64Data.unBase64Data) @?= Right img,
      testCase "unrecognized bytes are rejected" $
        case perceiveImageBytes (Just 200) (BS.pack [0, 1, 2, 3, 4, 5]) of
          Left err -> assertBool "mentions unrecognized" (T.isInfixOf "Unrecognized" err)
          Right _ -> assertFailure "Expected Left for unrecognizable bytes",
      testCase "media type comes from content, not the declared label" $
        -- caller has no way to declare a type anymore; sniffing decides
        case perceiveImageBytes (Just 200) (mkJpegBytes 50 40) of
          Left err -> assertFailure $ "Expected Right, got: " <> show err
          Right d -> d.mediaType @?= MediaType "image/jpeg",
      testCase "PNG declaring oversized dimensions is rejected before decode" $ do
        -- a real (cheap) 8001px-wide PNG: the header guard must reject it
        let img = mkPngBytes 8001 2
        case perceiveImageBytes (Just 200) img of
          Left err -> assertBool "mentions dimensions" (T.isInfixOf "dimensions" err)
          Right _ -> assertFailure "Expected Left for oversized dimensions",
      testCase "JPEG declaring oversized dimensions is rejected before decode" $ do
        let img = mkJpegBytes 2 8001
        case perceiveImageBytes (Just 200) img of
          Left err -> assertBool "mentions dimensions" (T.isInfixOf "dimensions" err)
          Right _ -> assertFailure "Expected Left for oversized dimensions"
    ]

sniffTests :: TestTree
sniffTests =
  testGroup
    "sniffImageMediaType"
    [ testCase "detects png/jpeg by magic bytes" $ do
        sniffImageMediaType (mkPngBytes 4 4) @?= Just "image/png"
        sniffImageMediaType (mkJpegBytes 4 4) @?= Just "image/jpeg",
      testCase "detects gif and webp prefixes" $ do
        sniffImageMediaType ("GIF89a" <> BS.replicate 16 0) @?= Just "image/gif"
        sniffImageMediaType ("RIFF" <> BS.replicate 4 0 <> "WEBP" <> BS.replicate 8 0) @?= Just "image/webp",
      testCase "returns Nothing for garbage" $ do
        sniffImageMediaType (BS.pack [1, 2, 3]) @?= Nothing
        sniffImageMediaType BS.empty @?= Nothing
    ]

mediaTypeTests :: TestTree
mediaTypeTests =
  testGroup
    "imageMediaTypeFromPath"
    [ testCase "recognizes supported extensions case-insensitively" $ do
        imageMediaTypeFromPath "photo.PNG" @?= Just "image/png"
        imageMediaTypeFromPath "a/b/pic.jpeg" @?= Just "image/jpeg"
        imageMediaTypeFromPath "pic.jpg" @?= Just "image/jpeg"
        imageMediaTypeFromPath "anim.gif" @?= Just "image/gif"
        imageMediaTypeFromPath "modern.webp" @?= Just "image/webp",
      testCase "rejects unsupported extensions" $ do
        imageMediaTypeFromPath "doc.pdf" @?= Nothing
        imageMediaTypeFromPath "noext" @?= Nothing
    ]

resizeTests :: TestTree
resizeTests =
  testGroup
    "resizeImage"
    [ testCase "within bounds returns original bytes unchanged" $ do
        let img = mkJpegBytes 100 80
            (result, mt, status) = resizeImage 200 img (MediaType "image/jpeg")
        result @?= img
        mt @?= MediaType "image/jpeg"
        assertUnchanged status,
      testCase "exactly at boundary returns original bytes unchanged" $ do
        let img = mkJpegBytes 200 150
            (result, _, status) = resizeImage 200 img (MediaType "image/jpeg")
        result @?= img
        assertUnchanged status,
      testCase "landscape image exceeding maxDim is resized" $ do
        let img = mkJpegBytes 400 200
            (result, mt, status) = resizeImage 200 img (MediaType "image/jpeg")
        result /= img @?= True
        mt @?= MediaType "image/jpeg"
        assertResized status
        assertResizedWithin 200 result,
      testCase "portrait image exceeding maxDim is resized" $ do
        let img = mkJpegBytes 200 400
            (result, mt, status) = resizeImage 200 img (MediaType "image/jpeg")
        result /= img @?= True
        mt @?= MediaType "image/jpeg"
        assertResized status
        assertResizedWithin 200 result,
      testCase "aspect ratio is preserved" $ do
        -- 400x200 -> max 100: should be 100x50
        let img = mkJpegBytes 400 200
            (result, _, _) = resizeImage 100 img (MediaType "image/jpeg")
        assertResizedDimensions 100 50 result,
      testCase "garbage bytes returns original unchanged with DecodeFailed" $ do
        let garbage = BS.pack [0, 1, 2, 3, 4, 5]
            (result, mt, status) = resizeImage 100 garbage (MediaType "image/jpeg")
        result @?= garbage
        mt @?= MediaType "image/jpeg"
        assertDecodeFailed status,
      testCase "PNG input re-encoded as PNG when resized" $ do
        let img = mkPngBytes 400 200
            (result, mt, status) = resizeImage 200 img (MediaType "image/png")
        result /= img @?= True
        mt @?= MediaType "image/png"
        assertResized status
        -- Verify it's valid PNG (starts with PNG signature)
        BS.take 4 result @?= BS.pack [0x89, 0x50, 0x4E, 0x47],
      testCase "PNG within bounds returns original bytes unchanged" $ do
        let img = mkPngBytes 100 80
            (result, mt, status) = resizeImage 200 img (MediaType "image/png")
        result @?= img
        mt @?= MediaType "image/png"
        assertUnchanged status
    ]

-- | Assert that the resized image has both dimensions <= maxDim
assertResizedWithin :: Int -> BS.ByteString -> IO ()
assertResizedWithin maxDim bs = do
  let (w, h) = decodeDimensions bs
  assertBool ("width " ++ show w ++ " exceeds " ++ show maxDim) (w <= maxDim)
  assertBool ("height " ++ show h ++ " exceeds " ++ show maxDim) (h <= maxDim)

-- | Assert exact dimensions of a decoded image
assertResizedDimensions :: Int -> Int -> BS.ByteString -> IO ()
assertResizedDimensions expectedW expectedH bs = do
  let (w, h) = decodeDimensions bs
  w @?= expectedW
  h @?= expectedH

-- | Decode image bytes and return dimensions
decodeDimensions :: BS.ByteString -> (Int, Int)
decodeDimensions bs =
  case decodeImage bs of
    Left err -> error ("Failed to decode resized image: " ++ err)
    Right dynImg -> dynamicMap (\img -> (imageWidth img, imageHeight img)) dynImg

assertUnchanged :: ResizeResult -> IO ()
assertUnchanged Unchanged = pure ()
assertUnchanged other = assertFailure ("Expected Unchanged, got " ++ showResult other)

assertResized :: ResizeResult -> IO ()
assertResized Resized = pure ()
assertResized other = assertFailure ("Expected Resized, got " ++ showResult other)

assertDecodeFailed :: ResizeResult -> IO ()
assertDecodeFailed (DecodeFailed _) = pure ()
assertDecodeFailed other = assertFailure ("Expected DecodeFailed, got " ++ showResult other)

showResult :: ResizeResult -> String
showResult Unchanged = "Unchanged"
showResult (DecodeFailed e) = "DecodeFailed " ++ show e
showResult Resized = "Resized"
