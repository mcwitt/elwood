module Elwood.Tools.ViewImage (mkViewImageTool) where

import Data.Aeson (withObject)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Elwood.Claude.Types (ToolResultPart (..))
import Elwood.Image (imageMediaTypeFromPath, maxImageFileBytes)
import Elwood.Logging (Logger)
import Elwood.Tools.Builder qualified as B
import Elwood.Tools.Types
import System.Directory (doesFileExist, getFileSize)
import System.FilePath (isAbsolute, (</>))

mkViewImageTool :: Logger -> FilePath -> Tool
mkViewImageTool _ workspace = B.tool "view_image" "View PNG, JPEG, GIF or WebP images. Paths may be absolute or relative to the workspace." (B.schema [("path", B.field "string" "Image path")] ["path"]) (withObject "view_image" (fmap T.unpack . (`B.nonempty` "path"))) $ \path -> do
  let resolved = if isAbsolute path then path else workspace </> path
  exists <- doesFileExist resolved
  case (exists, imageMediaTypeFromPath resolved) of
    (False, _) -> pure (toolError ("File not found: " <> T.pack resolved))
    (_, Nothing) -> pure (toolError "Not a supported image type")
    (_, Just mediaType) -> do
      size <- getFileSize resolved
      if size > fromIntegral maxImageFileBytes
        then pure (toolError "Image file is too large to view")
        else do
          bytes <- BS.readFile resolved
          pure (ToolSuccess [ToolResultText ("Image " <> T.pack path <> " (" <> mediaType <> "):"), ToolResultImage mediaType (T.decodeUtf8 (B64.encode bytes))])
