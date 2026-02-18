{-# LANGUAGE StrictData #-}

module Elwood.Tools.Command
  ( runCommandTool,
  )
where

import Control.Exception (SomeException, try)
import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Logging (logInfo, logWarn)
import Elwood.Permissions (PermissionResult (..), checkCommandPermission)
import Elwood.Tools.Types
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

-- | Tool for running shell commands
runCommandTool :: Tool
runCommandTool =
  Tool
    { toolName = "run_command",
      toolDescription =
        "Execute a shell command in the workspace directory. "
          <> "Use this for listing files, checking git status, running builds, etc. "
          <> "The command runs with a 30 second timeout.",
      toolInputSchema = commandSchema,
      toolExecute = executeCommand
    }

-- | JSON Schema for command input
commandSchema :: Value
commandSchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "command"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("The shell command to execute" :: Text)
                ],
            "timeout_seconds"
              .= object
                [ "type" .= ("integer" :: Text),
                  "description" .= ("Timeout in seconds (default 30, max 120)" :: Text)
                ]
          ],
      "required" .= (["command"] :: [Text])
    ]

-- | Execute a command
executeCommand :: ToolEnv -> Value -> IO ToolResult
executeCommand env input = do
  case parseInput input of
    Left err -> pure $ toolError err
    Right (cmd, timeoutSecs) -> do
      -- Check permissions
      case checkCommandPermission (tePermissions env) cmd of
        Denied reason -> do
          logWarn (teLogger env) "Command blocked" [("command", cmd), ("reason", reason)]
          pure $ toolError $ "Permission denied: " <> reason
        Allowed -> do
          logInfo (teLogger env) "Executing command" [("command", cmd)]
          runWithTimeout cmd timeoutSecs (teWorkspaceDir env)

-- | Parse input JSON
parseInput :: Value -> Either Text (Text, Int)
parseInput (Aeson.Object obj) = do
  cmd <- case KM.lookup "command" obj of
    Just (Aeson.String c) -> Right c
    _ -> Left "Missing or invalid 'command' parameter"
  let timeoutSecs = case KM.lookup "timeout_seconds" obj of
        Just (Aeson.Number n) -> min 120 (max 1 (round n))
        _ -> 30
  Right (cmd, timeoutSecs)
parseInput _ = Left "Expected object input"

-- | Run command with timeout
runWithTimeout :: Text -> Int -> FilePath -> IO ToolResult
runWithTimeout cmd timeoutSecs workDir = do
  let timeoutMicros = timeoutSecs * 1000000
  result <- timeout timeoutMicros (runCommand cmd workDir)
  case result of
    Nothing -> pure $ toolError $ "Command timed out after " <> T.pack (show timeoutSecs) <> " seconds"
    Just r -> pure r

-- | Actually run the command
runCommand :: Text -> FilePath -> IO ToolResult
runCommand cmd workDir = do
  result <- try $ do
    let process =
          (proc "/bin/sh" ["-c", T.unpack cmd])
            { cwd = Just workDir,
              std_out = CreatePipe,
              std_err = CreatePipe
            }

    (_, Just hOut, Just hErr, ph) <- createProcess process

    -- Read output strictly (limit to avoid memory issues)
    stdout <- limitOutput 50000 <$> hGetContents hOut
    stderr <- limitOutput 10000 <$> hGetContents hErr

    -- Force evaluation before waiting
    let !_ = length stdout
        !_ = length stderr

    exitCode <- waitForProcess ph

    pure (exitCode, stdout, stderr)

  case result of
    Left (e :: SomeException) ->
      pure $ toolError $ "Failed to execute command: " <> T.pack (show e)
    Right (exitCode, stdout, stderr) ->
      let output = formatOutput exitCode stdout stderr
       in case exitCode of
            ExitSuccess -> pure $ toolSuccess output
            ExitFailure code ->
              pure $
                toolError $
                  "Command failed with exit code "
                    <> T.pack (show code)
                    <> ":\n"
                    <> output

-- | Format command output
formatOutput :: ExitCode -> String -> String -> Text
formatOutput _exitCode stdout stderr =
  let stdoutText = T.pack stdout
      stderrText = T.pack stderr
   in case (T.null (T.strip stdoutText), T.null (T.strip stderrText)) of
        (True, True) -> "(no output)"
        (False, True) -> stdoutText
        (True, False) -> "stderr:\n" <> stderrText
        (False, False) -> stdoutText <> "\n\nstderr:\n" <> stderrText

-- | Limit output to avoid memory issues
limitOutput :: Int -> String -> String
limitOutput maxLen s
  | length s <= maxLen = s
  | otherwise = take maxLen s <> "\n... (output truncated)"
