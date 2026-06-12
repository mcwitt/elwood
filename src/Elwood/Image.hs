module Elwood.Image
  ( ResizeResult (..),
    resizeImage,

    -- * Perception
    perceiveImageBytes,
    sniffImageMediaType,
    imageMediaTypeFromPath,
    maxImageFileBytes,
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
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toLower)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Elwood.Event.Types (Base64Data (..), ImageData (..), MediaType (..))
import System.FilePath (takeExtension)

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

-- | Maximum bytes per image accepted by the Anthropic API (5MB).
maxPerceptionBytes :: Int
maxPerceptionBytes = 5 * 1024 * 1024

-- | Maximum file size worth reading for perception. Larger than the API's
-- 5MB per-image limit because oversized originals can still resize below
-- it; bounds memory before any read or decode happens.
maxImageFileBytes :: Int
maxImageFileBytes = 20 * 1024 * 1024

-- | Maximum width/height accepted before decoding. Matches the Anthropic
-- API's own per-dimension limit and bounds decoder memory (a small
-- compressed file can declare enormous dimensions — a decompression bomb).
maxImagePixelDimension :: Int
maxImagePixelDimension = 8000

-- | Media type for an image file path, by extension. 'Nothing' for
-- extensions the model cannot perceive.
imageMediaTypeFromPath :: FilePath -> Maybe Text
imageMediaTypeFromPath fp = case takeExtension (map toLower fp) of
  ".jpg" -> Just "image/jpeg"
  ".jpeg" -> Just "image/jpeg"
  ".png" -> Just "image/png"
  ".gif" -> Just "image/gif"
  ".webp" -> Just "image/webp"
  _ -> Nothing

-- | Detect the actual image format from magic bytes. This is authoritative
-- for perception: declared media types (file extensions, MCP mimeType
-- fields) can lie, and the Anthropic API rejects requests whose media_type
-- does not match the image data.
sniffImageMediaType :: ByteString -> Maybe Text
sniffImageMediaType bs
  | BS.isPrefixOf pngSignature bs = Just "image/png"
  | BS.isPrefixOf jpegSignature bs = Just "image/jpeg"
  | BS.isPrefixOf "GIF87a" bs || BS.isPrefixOf "GIF89a" bs = Just "image/gif"
  | BS.isPrefixOf "RIFF" bs && BS.isPrefixOf "WEBP" (BS.drop 8 bs) = Just "image/webp"
  | otherwise = Nothing
  where
    pngSignature = BS.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]
    jpegSignature = BS.pack [0xFF, 0xD8, 0xFF]

-- | Validate and prepare raw image bytes for model perception: detect the
-- actual format, guard against decompression bombs, resize to the
-- configured max dimension (when set), and enforce the API's per-image
-- size limit. Returns a human-readable error when the image cannot be
-- sent to the model.
perceiveImageBytes :: Maybe Int -> ByteString -> Either Text ImageData
perceiveImageBytes maxDim raw = do
  mt <- case sniffImageMediaType raw of
    Nothing -> Left "Unrecognized image data (supported formats: png, jpeg, gif, webp)"
    Just t -> Right t
  case headerDimensions mt raw of
    Just (w, h)
      | w > maxImagePixelDimension || h > maxImagePixelDimension ->
          Left $
            "Image dimensions too large ("
              <> dims w h
              <> ", max "
              <> dims maxImagePixelDimension maxImagePixelDimension
              <> ")"
    _ -> Right ()
  let mtv = MediaType mt
      (bytes, finalMt, _) = case maxDim of
        Nothing -> (raw, mtv, Unchanged)
        Just d -> resizeImage d raw mtv
  if BS.length bytes > maxPerceptionBytes
    then Left $ "Image is too large to perceive (" <> T.pack (show (BS.length bytes)) <> " bytes, max " <> T.pack (show maxPerceptionBytes) <> ")"
    else Right ImageData {mediaType = finalMt, base64Data = Base64Data (TE.decodeUtf8 (B64.encode bytes))}
  where
    dims w h = T.pack (show w) <> "x" <> T.pack (show h)

-- | Read declared pixel dimensions from image headers without decoding the
-- raster. Used to reject decompression bombs before 'decodeImage'
-- materializes the full bitmap. Returns 'Nothing' when the header cannot
-- be parsed (the subsequent full decode then fails safely for those
-- formats) and for webp (which is never decoded locally).
headerDimensions :: Text -> ByteString -> Maybe (Int, Int)
headerDimensions "image/png" bs = do
  -- 8-byte signature, 4-byte IHDR length, 4-byte "IHDR", then W and H as BE32
  guardBool (BS.length bs >= 24 && BS.take 4 (BS.drop 12 bs) == "IHDR")
  pure (be32 (BS.drop 16 bs), be32 (BS.drop 20 bs))
headerDimensions "image/gif" bs = do
  guardBool (BS.length bs >= 10)
  pure (le16 (BS.drop 6 bs), le16 (BS.drop 8 bs))
headerDimensions "image/jpeg" bs = jpegDimensions (BS.drop 2 bs)
headerDimensions _ _ = Nothing

-- | Walk JPEG segments looking for a start-of-frame marker, which carries
-- the image dimensions. Stops at start-of-scan (entropy data follows).
jpegDimensions :: ByteString -> Maybe (Int, Int)
jpegDimensions bs
  | BS.length bs < 2 || BS.head bs /= 0xFF = Nothing
  | marker == 0xFF = jpegDimensions (BS.drop 1 bs) -- fill byte
  | isStandalone marker = jpegDimensions rest
  | isSOF marker =
      -- segment: length(2) precision(1) height(2) width(2)
      if BS.length rest >= 7
        then Just (be16 (BS.drop 5 rest), be16 (BS.drop 3 rest))
        else Nothing
  | marker == 0xD9 || marker == 0xDA = Nothing -- end of image / start of scan
  | BS.length rest >= 2, len <- be16 rest, len >= 2 = jpegDimensions (BS.drop len rest)
  | otherwise = Nothing
  where
    marker = BS.index bs 1
    rest = BS.drop 2 bs
    isStandalone m = m == 0x01 || (m >= 0xD0 && m <= 0xD7)
    isSOF m =
      m >= 0xC0
        && m <= 0xCF
        && m `notElem` ([0xC4, 0xC8, 0xCC] :: [Word8])

guardBool :: Bool -> Maybe ()
guardBool True = Just ()
guardBool False = Nothing

be16 :: ByteString -> Int
be16 bs = fromIntegral (BS.index bs 0) * 256 + fromIntegral (BS.index bs 1)

le16 :: ByteString -> Int
le16 bs = fromIntegral (BS.index bs 0) + fromIntegral (BS.index bs 1) * 256

be32 :: ByteString -> Int
be32 bs = foldl (\acc i -> acc * 256 + fromIntegral (BS.index bs i)) 0 [0 .. 3]

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
