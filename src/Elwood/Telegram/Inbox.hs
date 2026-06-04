-- | Persist inbound Telegram attachments (photo, document, voice, audio) to the
-- workspace inbox and build the perception image for the largest photo.
module Elwood.Telegram.Inbox
  ( AttachmentKind (..),
    InboundAttachment (..),
    InboundResult (..),
    messageAttachments,
    inboxFileName,
    writeInboxFile,
    processInbound,
  )
where

import Control.Exception (SomeException, catch)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as LBS
import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Elwood.Event.Types (Base64Data (..), ImageData (..), MediaType (..), SavedAttachment (..))
import Elwood.Image (ResizeResult (..), resizeImage)
import Elwood.Logging (Logger, logInfo, logWarn)
import Elwood.Telegram.Client (TelegramClient, downloadFile, getFile)
import Elwood.Telegram.Types
  ( Audio (..),
    Document (..),
    Message (..),
    PhotoSize (..),
    TelegramFile (..),
    Voice (..),
  )
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, takeExtension, (</>))

-- | The kind of inbound attachment. Only 'KindPhoto' is perceived by the model.
data AttachmentKind = KindPhoto | KindDocument | KindVoice | KindAudio
  deriving stock (Show, Eq)

-- | A downloadable inbound attachment, normalized across Telegram media types.
data InboundAttachment = InboundAttachment
  { fileId :: Text,
    fileUniqueId :: Text,
    kind :: AttachmentKind,
    fileName :: Maybe Text,
    mimeType :: Maybe Text,
    fileSize :: Maybe Int
  }
  deriving stock (Show, Eq)

-- | The result of archiving a message's attachments.
data InboundResult = InboundResult
  { -- | Attachments successfully archived to disk
    saved :: [SavedAttachment],
    -- | Perception image (largest photo), if any
    perception :: Maybe ImageData
  }
  deriving stock (Show, Eq)

-- | Extract the downloadable attachments from a message (pure). Photos collapse
-- to the single largest size; document/voice/audio map one-to-one.
messageAttachments :: Message -> [InboundAttachment]
messageAttachments msg =
  photoAtt <> docAtt <> voiceAtt <> audioAtt
  where
    photoAtt = case msg.photo of
      Just ps@(_ : _) -> maybe [] (pure . fromPhoto) (largest ps)
      _ -> []
    docAtt = maybe [] (pure . fromDocument) msg.document
    voiceAtt = maybe [] (pure . fromVoice) msg.voice
    audioAtt = maybe [] (pure . fromAudio) msg.audio

    largest :: [PhotoSize] -> Maybe PhotoSize
    largest = listToMaybe . sortOn (Down . (.fileSize))

    fromPhoto p = InboundAttachment p.fileId p.fileUniqueId KindPhoto Nothing Nothing p.fileSize
    fromDocument d = InboundAttachment d.fileId d.fileUniqueId KindDocument d.fileName d.mimeType d.fileSize
    fromVoice v = InboundAttachment v.fileId v.fileUniqueId KindVoice Nothing v.mimeType v.fileSize
    fromAudio a = InboundAttachment a.fileId a.fileUniqueId KindAudio a.fileName a.mimeType a.fileSize

-- | Build the workspace-relative inbox path for an attachment (pure):
-- "inbox/<msgId>-<fileUniqueId><ext>". Extension precedence:
-- Telegram file path > attachment mime type > per-kind default.
inboxFileName :: Int -> InboundAttachment -> Maybe Text -> FilePath
inboxFileName msgId att mFilePath =
  "inbox" </> (show msgId <> "-" <> T.unpack att.fileUniqueId <> ext)
  where
    ext = case extFromPath mFilePath of
      Just e -> e
      Nothing -> case att.mimeType >>= extFromMime of
        Just e -> e
        Nothing -> defaultExt att.kind

extFromPath :: Maybe Text -> Maybe String
extFromPath mfp = do
  fp <- mfp
  let e = takeExtension (T.unpack fp)
  if null e then Nothing else Just e

extFromMime :: Text -> Maybe String
extFromMime = \case
  "image/jpeg" -> Just ".jpg"
  "image/png" -> Just ".png"
  "image/gif" -> Just ".gif"
  "image/webp" -> Just ".webp"
  "application/pdf" -> Just ".pdf"
  "audio/ogg" -> Just ".ogg"
  "audio/mpeg" -> Just ".mp3"
  _ -> Nothing

defaultExt :: AttachmentKind -> String
defaultExt = \case
  KindPhoto -> ".jpg"
  KindVoice -> ".ogg"
  KindAudio -> ".mp3"
  KindDocument -> ".bin"

-- | Write bytes to @\<workspace\>/\<relPath\>@, creating parent directories.
writeInboxFile :: FilePath -> FilePath -> ByteString -> IO ()
writeInboxFile workspace relPath bytes = do
  let absPath = workspace </> relPath
  createDirectoryIfMissing True (takeDirectory absPath)
  BS.writeFile absPath bytes

-- | Resolve the stored MIME type: Telegram-provided mime, else guessed from the
-- file path, else a per-kind default.
resolveMediaType :: InboundAttachment -> Text -> Text
resolveMediaType att fp = case att.mimeType of
  Just m -> m
  Nothing -> fromMaybe (defaultMime att.kind) (guessMimeFromPath fp)

guessMimeFromPath :: Text -> Maybe Text
guessMimeFromPath fp = case takeExtension (T.unpack (T.toLower fp)) of
  ".jpg" -> Just "image/jpeg"
  ".jpeg" -> Just "image/jpeg"
  ".png" -> Just "image/png"
  ".gif" -> Just "image/gif"
  ".webp" -> Just "image/webp"
  ".pdf" -> Just "application/pdf"
  ".ogg" -> Just "audio/ogg"
  ".oga" -> Just "audio/ogg"
  ".mp3" -> Just "audio/mpeg"
  _ -> Nothing

defaultMime :: AttachmentKind -> Text
defaultMime = \case
  KindPhoto -> "image/jpeg"
  KindVoice -> "audio/ogg"
  KindAudio -> "audio/mpeg"
  KindDocument -> "application/octet-stream"

-- | Build a perception image from raw bytes, resizing if a max dimension is set.
-- Returns the resize outcome so callers can log decode failures.
buildPerception :: Maybe Int -> ByteString -> Text -> (ImageData, ResizeResult)
buildPerception maxDim raw mt =
  let mtv = MediaType mt
      (imgBytes, finalMt, rr) = case maxDim of
        Nothing -> (raw, mtv, Unchanged)
        Just d -> resizeImage d raw mtv
      b64 = Base64Data (TE.decodeUtf8 (B64.encode imgBytes))
   in (ImageData {mediaType = finalMt, base64Data = b64}, rr)

-- | Download and archive every attachment in a message. Per-attachment failures
-- are logged and skipped. Returns the saved metadata and the perception image
-- (from the largest photo, if any). Original (un-resized) bytes are archived.
processInbound :: Logger -> TelegramClient -> FilePath -> Maybe Int -> Message -> IO InboundResult
processInbound lgr tg workspace maxDim msg = do
  results <- mapM saveOne (messageAttachments msg)
  pure
    InboundResult
      { saved = mapMaybe fst results,
        perception = listToMaybe (mapMaybe snd results)
      }
  where
    saveOne :: InboundAttachment -> IO (Maybe SavedAttachment, Maybe ImageData)
    saveOne att =
      doSave att `catch` \(e :: SomeException) -> do
        logWarn lgr "Failed to archive inbound attachment" [("file_id", att.fileId), ("error", T.pack (show e))]
        pure (Nothing, Nothing)

    doSave :: InboundAttachment -> IO (Maybe SavedAttachment, Maybe ImageData)
    doSave att = do
      mFile <- getFile tg att.fileId
      case mFile >>= (.filePath) of
        Nothing -> do
          logWarn lgr "No file path for inbound attachment" [("file_id", att.fileId)]
          pure (Nothing, Nothing)
        Just fp -> do
          raw <- LBS.toStrict <$> downloadFile tg fp
          if BS.null raw
            then do
              logWarn lgr "Downloaded inbound attachment was empty" [("file_id", att.fileId)]
              pure (Nothing, Nothing)
            else do
              let relPath = inboxFileName msg.id_ att (Just fp)
                  mt = resolveMediaType att fp
                  isPhoto = att.kind == KindPhoto
              writeInboxFile workspace relPath raw
              logInfo
                lgr
                "Archived inbound attachment"
                [("path", T.pack relPath), ("media_type", mt), ("bytes", T.pack (show (BS.length raw)))]
              mImg <-
                if isPhoto
                  then do
                    let (imgData, rr) = buildPerception maxDim raw mt
                    case rr of
                      DecodeFailed err ->
                        logWarn lgr "Image decode failed, sending original" [("file_id", att.fileId), ("error", T.pack err)]
                      _ -> pure ()
                    pure (Just imgData)
                  else pure Nothing
              let sa =
                    SavedAttachment
                      { path = relPath,
                        mediaType = mt,
                        sizeBytes = BS.length raw,
                        perceivable = isPhoto,
                        originalName = att.fileName
                      }
              pure (Just sa, mImg)
