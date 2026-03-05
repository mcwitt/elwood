module Elwood.Command
  ( CommandResult (..),
    runCommand,
    runCommandWithTimeout,
  )
where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Claude.Pruning (softPrune)
import System.Exit (ExitCode (..))
import System.IO (hGetContents)
import System.Process
  ( CreateProcess (..),
    StdStream (..),
    createProcess,
    proc,
    waitForProcess,
  )
import System.Timeout (timeout)

-- | Result of running a shell command
data CommandResult = CommandResult
  { exitCode :: ExitCode,
    output :: Text
  }
  deriving stock (Show)

-- | Run a shell command in the given working directory with a timeout (seconds).
runCommandWithTimeout :: Text -> Int -> FilePath -> IO CommandResult
runCommandWithTimeout cmd timeoutSecs workDir = do
  let timeoutMicros = timeoutSecs * 1000000
  result <- timeout timeoutMicros (runCommand cmd workDir)
  case result of
    Nothing -> pure $ CommandResult (ExitFailure 124) $ "Command timed out after " <> T.pack (show timeoutSecs) <> " seconds"
    Just r -> pure r

-- | Run a shell command in the given working directory.
runCommand :: Text -> FilePath -> IO CommandResult
runCommand cmd workDir = do
  result <- try $ do
    let process =
          (proc "/bin/sh" ["-c", T.unpack cmd])
            { cwd = Just workDir,
              std_out = CreatePipe,
              std_err = CreatePipe
            }

    (_, Just hOut, Just hErr, ph) <- createProcess process

    -- Read output strictly
    stdoutStr <- hGetContents hOut
    stderrStr <- hGetContents hErr

    -- Force evaluation before waiting
    let !_ = length stdoutStr
        !_ = length stderrStr

    ec <- waitForProcess ph

    -- Pack to Text and limit size
    let stdoutText = softPrune 5000 5000 (T.pack stdoutStr)
        stderrText = softPrune 2500 2500 (T.pack stderrStr)

    pure (ec, stdoutText, stderrText)

  case result of
    Left (e :: SomeException) ->
      pure $ CommandResult (ExitFailure 1) $ "Failed to execute command: " <> T.pack (show e)
    Right (ec, stdoutText, stderrText) ->
      pure $ CommandResult ec (formatOutput stdoutText stderrText)

-- | Format command output combining stdout and stderr
formatOutput :: Text -> Text -> Text
formatOutput stdoutText stderrText =
  case (T.null (T.strip stdoutText), T.null (T.strip stderrText)) of
    (True, True) -> "(no output)"
    (False, True) -> stdoutText
    (True, False) -> "stderr:\n" <> stderrText
    (False, False) -> stdoutText <> "\n\nstderr:\n" <> stderrText
