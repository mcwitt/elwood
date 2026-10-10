module Elwood.Tools.Attachment (mkQueueAttachmentTool, isPhotoExtension) where

import Control.Concurrent.STM
import Data.Aeson (encode, object, withObject, (.!=), (.:!), (.=))
import Data.ByteString.Lazy qualified as BL
import Data.Char (toLower)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Elwood.Logging (Logger)
import Elwood.Tools.Builder qualified as B
import Elwood.Tools.Types
import System.Directory (doesFileExist)
import System.FilePath (isAbsolute, takeExtension)

mkQueueAttachmentTool :: Logger -> TVar [Attachment] -> Tool
mkQueueAttachmentTool _ queue = B.tool "queue_attachment" "Queue an existing absolute file path for Telegram delivery after the response." fields parser $ \attachment -> do
  exists <- doesFileExist attachment.path
  if not exists
    then pure (toolError ("File not found: " <> T.pack attachment.path))
    else do
      atomically (modifyTVar' queue (<> [attachment]))
      pure (toolSuccess (T.decodeUtf8 (BL.toStrict (encode (object ["status" .= ("queued" :: T.Text), "path" .= attachment.path])))))
  where
    fields = B.schema [("path", B.field "string" "Absolute file path"), ("type", B.field "string" "photo, document or auto (default)"), ("caption", B.field "string" "Optional caption")] ["path"]
    parser = withObject "queue_attachment" $ \value -> do
      path <- T.unpack <$> B.nonempty value "path"
      if isAbsolute path then pure () else fail "path must be absolute"
      kind <- value .:! "type" .!= ("auto" :: T.Text)
      attachmentType <- case kind of
        "photo" -> pure AttachPhoto
        "document" -> pure AttachDocument
        "auto" -> pure AttachAuto
        _ -> fail "type must be photo, document or auto"
      Attachment path attachmentType <$> value .:! "caption"

isPhotoExtension :: FilePath -> Bool
isPhotoExtension = (`elem` [".png", ".jpg", ".jpeg", ".gif", ".webp"]) . map toLower . takeExtension
