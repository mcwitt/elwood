{-# LANGUAGE StrictData #-}

module Elwood.Prompt
  ( PromptInput (..),
    PromptInputFile (..),
    resolvePromptInput,
    assemblePrompt,
  )
where

import Control.Exception (SomeException, catch)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- | Resolved prompt input (ready for assembly)
data PromptInput
  = -- | Read content from a file in the workspace directory
    WorkspaceFile FilePath
  | -- | Inline text content
    InlineText Text
  deriving stock (Show, Eq, Generic)

-- | Prompt input as specified in config files (YAML)
data PromptInputFile
  = PromptInputFileWorkspace FilePath
  | PromptInputFileText Text
  deriving stock (Show, Generic)

instance FromJSON PromptInputFile where
  parseJSON = withObject "PromptInputFile" $ \v -> do
    t <- v .: "type" :: Parser Text
    case T.toLower t of
      "workspacefile" -> PromptInputFileWorkspace <$> v .: "path"
      "text" -> PromptInputFileText <$> v .: "content"
      other -> fail $ "Unknown prompt input type: " <> T.unpack other

-- | Resolve a config-file prompt input to a runtime prompt input.
resolvePromptInput :: PromptInputFile -> PromptInput
resolvePromptInput (PromptInputFileWorkspace p) = WorkspaceFile p
resolvePromptInput (PromptInputFileText t) = InlineText t

-- | Assemble a prompt from a list of inputs by reading workspace files
-- and concatenating all parts with @"\\n\\n"@.
--
-- Returns 'Nothing' if all inputs resolve to empty text.
assemblePrompt :: FilePath -> [PromptInput] -> IO (Maybe Text)
assemblePrompt dir inputs = do
  parts <- catMaybes <$> mapM (resolveInput dir) inputs
  let combined = T.intercalate "\n\n" parts
  pure $
    if T.null combined
      then Nothing
      else Just combined

-- | Resolve a single prompt input to its text content.
resolveInput :: FilePath -> PromptInput -> IO (Maybe Text)
resolveInput dir (WorkspaceFile fp) = do
  let fullPath = dir </> fp
  exists <- doesFileExist fullPath
  if exists
    then do
      c <-
        TIO.readFile fullPath
          `catch` \(_ :: SomeException) -> pure ""
      pure $
        if T.null c
          then Nothing
          else Just c
    else pure Nothing
resolveInput _ (InlineText t) =
  pure $
    if T.null t
      then Nothing
      else Just t
