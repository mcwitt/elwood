module Elwood.Tools.ViewImage
  ( mkViewImageTool,
  )
where

import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Elwood.Claude.Types (ToolResultPart (..), ToolSchema (..))
import Elwood.Image (imageMediaTypeFromPath)
import Elwood.Logging (Logger, logInfo)
import Elwood.Tools.Types
import System.Directory (doesFileExist)
import System.FilePath (isAbsolute, (</>))

-- | Construct a tool that reads an image file and returns it as a
-- perceivable image part, so the model can see and reason about its
-- content. Perception constraints (resizing, size limits) are enforced
-- centrally by the agent loop.
mkViewImageTool :: Logger -> FilePath -> Tool
mkViewImageTool logger workspace =
  Tool
    { schema =
        ToolSchema
          { name = "view_image",
            description =
              "View an image file so you can see its content. "
                <> "Supports png, jpg, jpeg, gif, and webp. "
                <> "The path may be absolute or relative to the workspace.",
            inputSchema = viewImageSchema
          },
      execute = \input -> case parseInput input of
        Left err -> pure $ toolError err
        Right p -> do
          let resolved = if isAbsolute p then p else workspace </> p
          exists <- doesFileExist resolved
          case (exists, imageMediaTypeFromPath resolved) of
            (False, _) -> pure $ toolError $ "File not found: " <> T.pack resolved
            (True, Nothing) ->
              pure $ toolError $ "Not a supported image type: " <> T.pack resolved <> ". Supported extensions: png, jpg, jpeg, gif, webp."
            (True, Just mt) -> do
              raw <- BS.readFile resolved
              logInfo logger "Viewing image" [("path", T.pack resolved), ("media_type", mt)]
              pure $
                ToolSuccess
                  [ ToolResultText $ "Image " <> T.pack p <> " (" <> mt <> "):",
                    ToolResultImage mt (TE.decodeUtf8 (B64.encode raw))
                  ]
    }

-- | JSON Schema for view_image input
viewImageSchema :: Value
viewImageSchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "path"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("Path to the image file (absolute, or relative to the workspace)" :: Text)
                ]
          ],
      "required" .= (["path"] :: [Text])
    ]

-- | Parse view_image input
parseInput :: Value -> Either Text FilePath
parseInput (Aeson.Object obj) = case KM.lookup "path" obj of
  Just (Aeson.String p) | not (T.null (T.strip p)) -> Right (T.unpack p)
  _ -> Left "Missing or invalid 'path' parameter"
parseInput _ = Left "Expected object input"
