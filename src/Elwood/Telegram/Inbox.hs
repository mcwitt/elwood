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

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, toLower)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Event.Types (ImageData, SavedAttachment (..))
import Elwood.Exception (catchSync)
import Elwood.Image (imageMediaTypeFromPath, perceiveImageBytes)
import Elwood.Logging (Logger, logWarn)
import Elwood.Telegram.Client (TelegramClient, downloadFile, getFile)
import Elwood.Telegram.Types
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, takeExtension, (</>))

data InboundResult = InboundResult {saved :: [SavedAttachment], perception :: Maybe ImageData} deriving stock (Show, Eq)

messageAttachments :: Message -> [InboundAttachment]
messageAttachments = (.attachments)

formats :: [(Text, String)]
formats = [("image/jpeg", ".jpg"), ("image/png", ".png"), ("image/gif", ".gif"), ("image/webp", ".webp"), ("application/pdf", ".pdf"), ("audio/ogg", ".ogg"), ("audio/mpeg", ".mp3"), ("audio/ogg", ".oga")]

defaultMime :: AttachmentKind -> Text
defaultMime = \case
  KindPhoto -> "image/jpeg"
  KindDocument -> "application/octet-stream"
  KindVoice -> "audio/ogg"
  KindAudio -> "audio/mpeg"

inboxFileName :: Int -> InboundAttachment -> Maybe Text -> FilePath
inboxFileName messageId attachment filePath = "inbox" </> (show messageId <> "-" <> map safe (T.unpack attachment.fileUniqueId) <> extension)
  where
    safe c = if isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ['_', '-'] then c else '_'
    supplied = maybe "" (takeExtension . T.unpack) filePath
    extension = if null supplied then fromMaybe (fromMaybe ".bin" (lookup (defaultMime attachment.kind) formats)) (attachment.mimeType >>= (`lookup` formats)) else supplied

writeInboxFile :: FilePath -> FilePath -> ByteString -> IO ()
writeInboxFile workspace path bytes = do
  let full = workspace </> path
  createDirectoryIfMissing True (takeDirectory full)
  BS.writeFile full bytes

processInbound :: Logger -> TelegramClient -> FilePath -> Maybe Int -> Message -> IO InboundResult
processInbound logger client workspace maxDimension message = do
  results <-
    mapM
      ( \attachment -> catchSync (archive attachment) $ \err -> do
          logWarn logger "Attachment archival failed" [("error", T.pack (show err))]
          pure Nothing
      )
      (messageAttachments message)
  let success = catMaybes results
  pure (InboundResult (map fst success) (listToMaybe (mapMaybe snd success)))
  where
    archive attachment = do
      metadata <- getFile client attachment.fileId
      case metadata >>= (.filePath) of
        Nothing -> pure Nothing
        Just file -> do
          bytes <- LBS.toStrict <$> downloadFile client file
          if BS.null bytes
            then pure Nothing
            else do
              let path = inboxFileName message.id_ attachment (Just file)
                  extension = map toLower (takeExtension (T.unpack file))
                  guessed = imageMediaTypeFromPath (T.unpack file)
                  mime = fromMaybe (fromMaybe (fromMaybe (defaultMime attachment.kind) (lookup extension [(ext, typ) | (typ, ext) <- formats])) guessed) attachment.mimeType
                  image = if attachment.kind == KindPhoto then either (const Nothing) Just (perceiveImageBytes maxDimension bytes) else Nothing
              writeInboxFile workspace path bytes
              pure (Just (SavedAttachment path mime (BS.length bytes) (isJust image) attachment.fileName, image))
