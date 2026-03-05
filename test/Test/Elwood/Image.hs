module Test.Elwood.Image (tests) where

import Codec.Picture
  ( Image,
    decodeImage,
    encodeJpegAtQuality,
    encodePng,
    generateImage,
  )
import Codec.Picture.Types
  ( PixelRGB8 (..),
    PixelRGBA8 (..),
    PixelYCbCr8,
    convertImage,
    dynamicMap,
    imageHeight,
    imageWidth,
  )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Elwood.Event.Types (MediaType (..))
import Elwood.Image (ResizeResult (..), resizeImage)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Image"
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

-- | Generate a JPEG image of given dimensions as strict ByteString
mkJpegBytes :: Int -> Int -> BS.ByteString
mkJpegBytes w h =
  let rgb8 = generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128) w h
      ycbcr = convertImage rgb8 :: Image PixelYCbCr8
   in LBS.toStrict (encodeJpegAtQuality 90 ycbcr)

-- | Generate a PNG image of given dimensions as strict ByteString
mkPngBytes :: Int -> Int -> BS.ByteString
mkPngBytes w h =
  let img = generateImage (\x y -> PixelRGBA8 (fromIntegral x) (fromIntegral y) 128 255) w h
   in LBS.toStrict (encodePng img)

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
