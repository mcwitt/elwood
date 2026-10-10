module Elwood.Image
  ( ResizeResult (..),
    resizeImage,
    perceiveImageBytes,
    sniffImageMediaType,
    imageMediaTypeFromPath,
    maxImageFileBytes,
  )
where

import Codec.Picture
import Codec.Picture.Extra (scaleBilinear)
import Codec.Picture.Types (convertImage)
import Control.Monad (guard, unless)
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as Bytes
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as Lazy
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Elwood.Event.Types (Base64Data (..), ImageData (..), MediaType (..))
import System.FilePath (takeExtension)

data ResizeResult = Unchanged | DecodeFailed String | Resized

maxImageFileBytes :: Int
maxImageFileBytes = 20 * 1024 * 1024

imageMediaTypeFromPath :: FilePath -> Maybe Text
imageMediaTypeFromPath path =
  lookup
    (map toLower (takeExtension path))
    [(".jpg", "image/jpeg"), (".jpeg", "image/jpeg"), (".png", "image/png"), (".gif", "image/gif"), (".webp", "image/webp")]

sniffImageMediaType :: ByteString -> Maybe Text
sniffImageMediaType bytes
  | Bytes.pack [137, 80, 78, 71, 13, 10, 26, 10] `Bytes.isPrefixOf` bytes = Just "image/png"
  | Bytes.pack [255, 216, 255] `Bytes.isPrefixOf` bytes = Just "image/jpeg"
  | any (`Bytes.isPrefixOf` bytes) ["GIF87a", "GIF89a"] = Just "image/gif"
  | "RIFF" `Bytes.isPrefixOf` bytes, "WEBP" `Bytes.isPrefixOf` Bytes.drop 8 bytes = Just "image/webp"
  | otherwise = Nothing

-- Read headers before materializing a raster. Each read is checked; dimensions
-- and offsets are bounded by the input length, including malformed JPEG segments.
headerDimensions :: Text -> ByteString -> Maybe (Int, Int)
headerDimensions media bytes = case media of
  "image/png" -> do
    guard (Bytes.take 4 (Bytes.drop 12 bytes) == "IHDR")
    (,) <$> number False 4 16 bytes <*> number False 4 20 bytes
  "image/gif" -> (,) <$> number True 2 6 bytes <*> number True 2 8 bytes
  "image/jpeg" -> jpeg (Bytes.drop 2 bytes)
  "image/webp" -> webpDimensions bytes
  _ -> Nothing
  where
    jpeg remaining = do
      (255, rest) <- Bytes.uncons remaining
      (marker, segment) <- Bytes.uncons rest
      case marker of
        255 -> jpeg rest
        1 -> jpeg segment
        m | m >= 208 && m <= 215 -> jpeg segment
        m
          | m >= 192 && m <= 207 && m `notElem` [196, 200, 204] ->
              (,) <$> number False 2 5 segment <*> number False 2 3 segment
        m | m `elem` [217, 218] -> Nothing
        _ -> do
          size <- number False 2 0 segment
          guard (size >= 2 && size <= Bytes.length segment)
          jpeg (Bytes.drop size segment)

-- Container/header verification for the codec JuicyPixels cannot decode.
-- See Google's WebP RIFF and lossless bitstream specifications.
webpDimensions :: ByteString -> Maybe (Int, Int)
webpDimensions bytes = do
  size <- number True 4 4 bytes
  guard (size + 8 == Bytes.length bytes)
  parts <- chunks (Bytes.drop 12 bytes)
  case parts of
    ("VP8X", header) : rest -> do
      guard (Bytes.length header == 10 && any (\(tag, body) -> tag `elem` ["VP8 ", "VP8L", "ANMF"] && not (Bytes.null body)) rest)
      ((,) . (+ 1) <$> number True 3 4 header) <*> ((+ 1) <$> number True 3 7 header)
    ("VP8 ", payload) : _ -> do
      guard (Bytes.length payload > 10 && Bytes.take 3 (Bytes.drop 3 payload) == Bytes.pack [157, 1, 42])
      ((,) . (.&. 16383) <$> number True 2 6 payload) <*> ((.&. 16383) <$> number True 2 8 payload)
    ("VP8L", payload) : _ -> do
      guard (Bytes.length payload > 5 && Bytes.take 1 payload == Bytes.singleton 47)
      dimensions <- number True 4 1 payload
      guard (dimensions `shiftR` 29 == 0)
      pure (1 + (dimensions .&. 16383), 1 + ((dimensions `shiftR` 14) .&. 16383))
    _ -> Nothing
  where
    chunks rest
      | Bytes.null rest = Just []
      | otherwise = do
          size <- number True 4 4 rest
          let padded = size + size `mod` 2
          guard (Bytes.length rest >= 8 + padded)
          ((Bytes.take 4 rest, Bytes.take size (Bytes.drop 8 rest)) :) <$> chunks (Bytes.drop (8 + padded) rest)

number :: Bool -> Int -> Int -> ByteString -> Maybe Int
number little size offset bytes = do
  guard (Bytes.length bytes >= offset + size)
  let slice = Bytes.take size (Bytes.drop offset bytes)
  pure (Bytes.foldl' (\n b -> n * 256 + fromIntegral b) 0 (if little then Bytes.reverse slice else slice))

resizeImage :: Int -> ByteString -> MediaType -> (ByteString, MediaType, ResizeResult)
resizeImage limit bytes media
  | limit <= 0 = (bytes, media, DecodeFailed "image dimension must be positive")
  | otherwise = case decodeImage bytes of
      Left err -> (bytes, media, DecodeFailed err)
      Right raster ->
        let (width, height) = dynamicMap (\i -> (imageWidth i, imageHeight i)) raster
            longest = max width height
            w = max 1 (width * limit `div` longest)
            h = max 1 (height * limit `div` longest)
         in if longest <= limit
              then (bytes, media, Unchanged)
              else
                if media == MediaType "image/png"
                  then (Lazy.toStrict (encodePng (scaleBilinear w h (convertRGBA8 raster))), media, Resized)
                  else (Lazy.toStrict (encodeJpegAtQuality 85 (convertImage (scaleBilinear w h (convertRGB8 raster)))), MediaType "image/jpeg", Resized)

perceiveImageBytes :: Maybe Int -> ByteString -> Either Text ImageData
perceiveImageBytes limit raw = do
  unless (Bytes.length raw <= maxImageFileBytes) (Left "Image file is too large")
  media <- maybe (Left "Unrecognized image data") Right (sniffImageMediaType raw)
  case headerDimensions media raw of
    Just (width, height) -> unless (width > 0 && height > 0 && width <= 8000 && height <= 8000) (Left "Image dimensions too large or invalid")
    Nothing -> Left "Invalid image header"
  unless (maybe True (> 0) limit) (Left "Image dimension must be positive")
  let (bytes, finalMedia, result) = resizeImage (fromMaybe 8000 limit) raw (MediaType media)
  case result of
    DecodeFailed _ | media /= "image/webp" -> Left "Invalid image data"
    _ -> pure ()
  unless (Bytes.length bytes <= 5 * 1024 * 1024) (Left "Image is too large to perceive")
  pure (ImageData finalMedia (Base64Data (Text.decodeUtf8 (Base64.encode bytes))))
