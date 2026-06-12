-- | Shared image fixtures for tests.
module Test.Elwood.TestImage
  ( mkPngBytes,
    mkJpegBytes,
  )
where

import Codec.Picture (Image, encodeJpegAtQuality, encodePng, generateImage)
import Codec.Picture.Types (PixelRGB8 (..), PixelRGBA8 (..), PixelYCbCr8, convertImage)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS

-- | Generate a PNG image of given dimensions as strict ByteString
mkPngBytes :: Int -> Int -> BS.ByteString
mkPngBytes w h =
  let img = generateImage (\x y -> PixelRGBA8 (fromIntegral x) (fromIntegral y) 128 255) w h
   in LBS.toStrict (encodePng img)

-- | Generate a JPEG image of given dimensions as strict ByteString
mkJpegBytes :: Int -> Int -> BS.ByteString
mkJpegBytes w h =
  let rgb8 = generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128) w h
      ycbcr = convertImage rgb8 :: Image PixelYCbCr8
   in LBS.toStrict (encodeJpegAtQuality 90 ycbcr)
