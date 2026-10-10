module Elwood.Persistence (readJSON, writeJSON, writeBytes) where

import Control.Exception (bracketOnError, onException)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encode)
import Data.ByteString.Lazy qualified as BL
import System.Directory (doesFileExist, removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (hClose, openBinaryTempFile)

readJSON :: (FromJSON a) => FilePath -> a -> IO a
readJSON path fallback = do
  exists <- doesFileExist path
  if exists then either (ioError . userError) pure =<< eitherDecodeFileStrict path else pure fallback

-- Atomic visibility; callers own serialization and mask publication after rename.
writeJSON :: (ToJSON a) => FilePath -> a -> IO ()
writeJSON path = writeBytes path . encode

writeBytes :: FilePath -> BL.ByteString -> IO ()
writeBytes path bytes =
  bracketOnError
    (openBinaryTempFile (takeDirectory path) (takeFileName path <> ".tmp"))
    (\(temporary, handle) -> hClose handle `onException` removeFile temporary >> removeFile temporary)
    (\(temporary, handle) -> BL.hPut handle bytes >> hClose handle >> renameFile temporary path)
