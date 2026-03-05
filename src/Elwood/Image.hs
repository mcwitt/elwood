module Elwood.Image
  ( ResizeResult (..),
    resizeImage,
  )
where

import Codec.Picture
  ( DynamicImage (..),
    Image,
    convertRGB8,
    decodeImage,
    encodeJpegAtQuality,
    encodePng,
  )
import Codec.Picture.Extra (scaleBilinear)
import Codec.Picture.Types
  ( PixelRGB8 (..),
    PixelRGBA8 (..),
    PixelYCbCr8,
    convertImage,
    dynamicMap,
    imageHeight,
    imageWidth,
    pixelMap,
  )
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Word (Word8)
import Elwood.Event.Types (MediaType (..))

-- | JPEG encoding quality (0-100)
jpegQuality :: Word8
jpegQuality = 85

-- | Result of attempting to resize an image
data ResizeResult
  = -- | Image was not modified (already within bounds or decode failed)
    Unchanged
  | -- | Decode failed — original bytes returned but caller may want to log
    DecodeFailed String
  | -- | Image was resized
    Resized

-- | Resize image bytes if either dimension exceeds maxDim.
--
-- Preserves aspect ratio using bilinear scaling.
-- If neither dimension exceeds maxDim, returns the original bytes unchanged
-- (no re-encode). On decode failure (e.g. WebP, corrupted data), returns
-- original bytes unchanged with 'DecodeFailed'.
--
-- PNGs are re-encoded as PNG (preserving transparency); others as JPEG.
resizeImage :: Int -> ByteString -> MediaType -> (ByteString, MediaType, ResizeResult)
resizeImage maxDim originalBytes mt =
  case decodeImage originalBytes of
    Left err -> (originalBytes, mt, DecodeFailed err)
    Right dynImg ->
      let (w, h) = dynamicDimensions dynImg
       in if w <= maxDim && h <= maxDim
            then (originalBytes, mt, Unchanged)
            else
              let (newW, newH) = scaledDimensions maxDim w h
                  isPng = mt == MediaType "image/png"
               in if isPng
                    then
                      let resized = scaleBilinear newW newH (toRGBA8 dynImg)
                       in (LBS.toStrict (encodePng resized), MediaType "image/png", Resized)
                    else
                      let resized = scaleBilinear newW newH (convertRGB8 dynImg)
                          ycbcr = convertImage resized :: Image PixelYCbCr8
                       in (LBS.toStrict (encodeJpegAtQuality jpegQuality ycbcr), MediaType "image/jpeg", Resized)

-- | Get dimensions from any DynamicImage
dynamicDimensions :: DynamicImage -> (Int, Int)
dynamicDimensions = dynamicMap (\img -> (imageWidth img, imageHeight img))

-- | Calculate new dimensions preserving aspect ratio
scaledDimensions :: Int -> Int -> Int -> (Int, Int)
scaledDimensions maxDim w h
  | w >= h =
      let newW = maxDim
          newH = max 1 ((h * maxDim) `div` w)
       in (newW, newH)
  | otherwise =
      let newH = maxDim
          newW = max 1 ((w * maxDim) `div` h)
       in (newW, newH)

-- | Convert any DynamicImage to RGBA8 (for PNG output, preserving transparency)
toRGBA8 :: DynamicImage -> Image PixelRGBA8
toRGBA8 (ImageRGBA8 img) = img
toRGBA8 other = pixelMap (\(PixelRGB8 r g b) -> PixelRGBA8 r g b 255) (convertRGB8 other)
