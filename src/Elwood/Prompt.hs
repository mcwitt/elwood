module Elwood.Prompt (PromptInput (..), assemblePrompt) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory (doesFileExist)
import System.FilePath ((</>))

data PromptInput = WorkspaceFile FilePath | InlineText Text
  deriving stock (Show, Eq)

instance FromJSON PromptInput where
  parseJSON = withObject "PromptInput" $ \o ->
    (T.toLower <$> o .: "type" :: Parser Text) >>= \case
      "workspace_file" -> WorkspaceFile <$> o .: "path"
      "text" -> InlineText <$> o .: "content"
      other -> fail ("Unknown prompt input type: " <> T.unpack other)

assemblePrompt :: FilePath -> [PromptInput] -> IO (Maybe Text)
assemblePrompt directory inputs = do
  text <- T.intercalate "\n\n" . filter (not . T.null) <$> traverse resolve inputs
  pure (if T.null text then Nothing else Just text)
  where
    resolve (InlineText text) = pure text
    resolve (WorkspaceFile path) = do
      let file = directory </> path
      exists <- doesFileExist file
      if exists then T.readFile file else pure ""
