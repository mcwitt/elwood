{-# LANGUAGE StrictData #-}

module Elwood.Tools.FileSystem
  ( readFileTool,
    writeFileTool,
  )
where

import Control.Exception (SomeException, try)
import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Elwood.Logging (logInfo, logWarn)
import Elwood.Permissions (PermissionResult (..), checkPathPermission)
import Elwood.Tools.Types
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (normalise, takeDirectory, (</>))

-- | Tool for reading files
readFileTool :: Tool
readFileTool =
  Tool
    { toolName = "read_file",
      toolDescription =
        "Read the contents of a file. "
          <> "The path should be relative to the workspace directory.",
      toolInputSchema = readFileSchema,
      toolExecute = executeReadFile
    }

-- | Tool for writing files
writeFileTool :: Tool
writeFileTool =
  Tool
    { toolName = "write_file",
      toolDescription =
        "Write content to a file. "
          <> "The path should be relative to the workspace directory. "
          <> "Creates parent directories if they don't exist.",
      toolInputSchema = writeFileSchema,
      toolExecute = executeWriteFile
    }

-- | JSON Schema for read_file input
readFileSchema :: Value
readFileSchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "path"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("Path to the file (relative to workspace)" :: Text)
                ]
          ],
      "required" .= (["path"] :: [Text])
    ]

-- | JSON Schema for write_file input
writeFileSchema :: Value
writeFileSchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "path"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("Path to the file (relative to workspace)" :: Text)
                ],
            "content"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("Content to write to the file" :: Text)
                ]
          ],
      "required" .= (["path", "content"] :: [Text])
    ]

-- | Execute read_file
executeReadFile :: ToolEnv -> Value -> IO ToolResult
executeReadFile env input = do
  case parseReadInput input of
    Left err -> pure $ toolError err
    Right relPath -> do
      let absPath = normalise $ teWorkspaceDir env </> relPath

      -- Check permissions
      case checkPathPermission (tePermissions env) absPath of
        Denied reason -> do
          logWarn (teLogger env) "File read blocked" [("path", T.pack absPath), ("reason", reason)]
          pure $ toolError $ "Permission denied: " <> reason
        Allowed -> do
          logInfo (teLogger env) "Reading file" [("path", T.pack absPath)]
          readFileContents absPath

-- | Execute write_file
executeWriteFile :: ToolEnv -> Value -> IO ToolResult
executeWriteFile env input = do
  case parseWriteInput input of
    Left err -> pure $ toolError err
    Right (relPath, content) -> do
      let absPath = normalise $ teWorkspaceDir env </> relPath

      -- Check permissions
      case checkPathPermission (tePermissions env) absPath of
        Denied reason -> do
          logWarn (teLogger env) "File write blocked" [("path", T.pack absPath), ("reason", reason)]
          pure $ toolError $ "Permission denied: " <> reason
        Allowed -> do
          logInfo (teLogger env) "Writing file" [("path", T.pack absPath)]
          writeFileContents absPath content

-- | Parse read_file input
parseReadInput :: Value -> Either Text FilePath
parseReadInput (Aeson.Object obj) =
  case KM.lookup "path" obj of
    Just (Aeson.String p) -> Right (T.unpack p)
    _ -> Left "Missing or invalid 'path' parameter"
parseReadInput _ = Left "Expected object input"

-- | Parse write_file input
parseWriteInput :: Value -> Either Text (FilePath, Text)
parseWriteInput (Aeson.Object obj) = do
  path <- case KM.lookup "path" obj of
    Just (Aeson.String p) -> Right (T.unpack p)
    _ -> Left "Missing or invalid 'path' parameter"
  content <- case KM.lookup "content" obj of
    Just (Aeson.String c) -> Right c
    _ -> Left "Missing or invalid 'content' parameter"
  Right (path, content)
parseWriteInput _ = Left "Expected object input"

-- | Read file contents
readFileContents :: FilePath -> IO ToolResult
readFileContents path = do
  exists <- doesFileExist path
  if not exists
    then pure $ toolError $ "File not found: " <> T.pack path
    else do
      result <- try $ TIO.readFile path
      case result of
        Left (e :: SomeException) ->
          pure $ toolError $ "Failed to read file: " <> T.pack (show e)
        Right content ->
          -- Limit content size to avoid memory issues
          let limited = limitContent 100000 content
           in pure $ toolSuccess limited

-- | Write file contents
writeFileContents :: FilePath -> Text -> IO ToolResult
writeFileContents path content = do
  result <- try $ do
    -- Create parent directories
    createDirectoryIfMissing True (takeDirectory path)
    TIO.writeFile path content
  case result of
    Left (e :: SomeException) ->
      pure $ toolError $ "Failed to write file: " <> T.pack (show e)
    Right () ->
      pure $ toolSuccess $ "Successfully wrote " <> T.pack (show (T.length content)) <> " characters to " <> T.pack path

-- | Limit content size
limitContent :: Int -> Text -> Text
limitContent maxLen content
  | T.length content <= maxLen = content
  | otherwise = T.take maxLen content <> "\n... (content truncated)"
