module Elwood.Command (CommandResult (..), runCommand, runCommandWithTimeout) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (IOException, SomeException, catch, finally, mask)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Elwood.Claude.Pruning (softPrune)
import Elwood.Exception (catchSync)
import System.Exit (ExitCode (..))
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Process (CreateProcess (..), StdStream (..), getPid, proc, waitForProcess, withCreateProcess)
import System.Timeout (timeout)

data CommandResult = CommandResult {exitCode :: ExitCode, output :: Text}
  deriving stock (Show)

runCommandWithTimeout :: Text -> Int -> FilePath -> IO CommandResult
runCommandWithTimeout command seconds directory
  | seconds <= 0 = pure (CommandResult (ExitFailure 124) "Timeout must be positive")
  | otherwise = fromMaybe expired <$> timeout micros (runCommand command directory)
  where
    micros = fromInteger (min (toInteger (maxBound :: Int)) (toInteger seconds * 1000000))
    expired = CommandResult (ExitFailure 124) ("Command timed out after " <> Text.pack (show seconds) <> " seconds")

-- The shell and descendants belong to one process group, including background
-- children. Both pipes are drained concurrently; every exit terminates the group.
runCommand :: Text -> FilePath -> IO CommandResult
runCommand command directory =
  action `catchSync` \(e :: SomeException) ->
    pure (CommandResult (ExitFailure 1) ("Failed to execute command: " <> Text.pack (show e)))
  where
    process =
      (proc "/bin/sh" ["-c", Text.unpack command])
        { cwd = Just directory,
          std_out = CreatePipe,
          std_err = CreatePipe,
          create_group = True
        }
    action = mask $ \restore -> withCreateProcess process $ \_ out err child -> do
      pid <- getPid child
      let cleanup = for_ pid $ \group -> signalProcessGroup sigKILL group `catch` \(_ :: IOException) -> pure ()
      flip finally cleanup $ restore $ case (out, err) of
        (Just stdout, Just stderr) -> do
          (output, errors) <- concurrently (TextIO.hGetContents stdout) (TextIO.hGetContents stderr)
          status <- waitForProcess child
          let clipped n t = fromMaybe t (softPrune n n t)
              parts =
                filter
                  (not . Text.null . Text.strip)
                  [clipped 5000 output, if Text.null errors then "" else "stderr:\n" <> clipped 2500 errors]
          pure (CommandResult status (if null parts then "(no output)" else Text.intercalate "\n\n" parts))
        _ -> fail "Command pipes unavailable"
