{-# LANGUAGE StrictData #-}

module Elwood.Tools.Attachment
  ( mkQueueAttachmentTool,
    isPhotoExtension,
  )
where

import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Char (toLower)
import Data.IORef (IORef, modifyIORef')
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Logging (Logger, logInfo)
import Elwood.Tools.Types
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)

-- | Construct a tool for queuing file attachments to send after the text response
mkQueueAttachmentTool :: Logger -> IORef [Attachment] -> Tool
mkQueueAttachmentTool logger queue =
  Tool
    { toolName = "queue_attachment",
      toolDescription =
        "Queue a file to be sent as a Telegram attachment after the text response. "
          <> "Supports photos (png, jpg, jpeg, gif, webp) and documents (any file type). "
          <> "The path must be an absolute path to an existing file.",
      toolInputSchema = queueAttachmentSchema,
      toolExecute = \input -> case parseInput input of
        Left err -> pure $ toolError err
        Right (path, attTy, cap) -> do
          exists <- doesFileExist path
          if not exists
            then pure $ toolError $ "File not found: " <> T.pack path
            else do
              let attachment =
                    Attachment
                      { attPath = path,
                        attType = attTy,
                        attCaption = cap
                      }
              modifyIORef' queue (<> [attachment])
              logInfo logger "Attachment queued" [("path", T.pack path)]
              pure $ toolSuccess $ "{\"status\":\"queued\",\"path\":" <> T.pack (show path) <> "}"
    }

-- | JSON Schema for queue_attachment input
queueAttachmentSchema :: Value
queueAttachmentSchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "path"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("Absolute path to the file to send" :: Text)
                ],
            "type"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("How to send: \"photo\", \"document\", or \"auto\" (default: auto)" :: Text),
                  "enum" .= (["photo", "document", "auto"] :: [Text])
                ],
            "caption"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("Optional caption for the attachment" :: Text)
                ]
          ],
      "required" .= (["path"] :: [Text])
    ]

-- | Parse queue_attachment input
parseInput :: Value -> Either Text (FilePath, AttachmentType, Maybe Text)
parseInput (Aeson.Object obj) = do
  path <- case KM.lookup "path" obj of
    Just (Aeson.String p) -> Right (T.unpack p)
    _ -> Left "Missing or invalid 'path' parameter"
  let attTy = case KM.lookup "type" obj of
        Just (Aeson.String "photo") -> AttachPhoto
        Just (Aeson.String "document") -> AttachDocument
        _ -> AttachAuto
      cap = case KM.lookup "caption" obj of
        Just (Aeson.String c) -> Just c
        _ -> Nothing
  Right (path, attTy, cap)
parseInput _ = Left "Expected object input"

-- | Check if a file extension is a photo type supported by Telegram
isPhotoExtension :: FilePath -> Bool
isPhotoExtension p = map toLower (takeExtension p) `elem` [".png", ".jpg", ".jpeg", ".gif", ".webp"]
