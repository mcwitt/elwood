-- | Read flat conversation files without changing or deleting the source.
module Elwood.Compat.Conversation (load) where

import Data.Aeson (eitherDecodeFileStrict)
import Data.Char (isAlphaNum)
import Data.Text qualified as Text
import Elwood.Claude.Types (Conversation (..))
import System.Directory (doesFileExist)
import System.FilePath ((</>))

load :: FilePath -> Text.Text -> IO (Maybe Conversation)
load directory session = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      conversation <- either (ioError . userError) pure =<< eitherDecodeFileStrict path
      if conversation.sessionId == session
        then pure (Just conversation)
        else ioError (userError "Conversation filename collision: embedded session ID does not match")
  where
    path = directory </> "conversations" </> map encode (Text.unpack session) <> ".json"
    encode char = if isAlphaNum char || char `elem` ['-', '_', '.'] then char else '_'
