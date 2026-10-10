module Elwood.Tools.Command (mkRunCommandTool) where

import Data.Aeson (withObject)
import Data.Text qualified as T
import Elwood.Command qualified as Command
import Elwood.Logging (Logger, logInfo)
import Elwood.Permissions (PermissionConfig, PermissionResult (..), checkCommandPermission)
import Elwood.Tools.Builder qualified as B
import Elwood.Tools.Types
import System.Exit (ExitCode (..))

mkRunCommandTool :: Logger -> FilePath -> PermissionConfig -> Tool
mkRunCommandTool logger workspace permissions = B.tool "run_command" "Run a shell command in the workspace. Timeout defaults to 30 seconds, maximum 120." fields parser $ \(command, seconds) ->
  case checkCommandPermission permissions command of
    Denied reason -> pure (toolError ("Permission denied: " <> reason))
    Allowed -> do
      logInfo logger "Executing command" [("command", command)]
      result <- Command.runCommandWithTimeout command seconds workspace
      pure $ case result.exitCode of
        ExitSuccess -> toolSuccess result.output
        ExitFailure code -> toolError ("Command failed with exit code " <> T.pack (show code) <> ":\n" <> result.output)
  where
    fields = B.schema [("command", B.field "string" "Shell command"), ("timeout_seconds", B.field "integer" "Timeout: 1–120 seconds")] ["command"]
    parser = withObject "run_command" $ \value -> (,) <$> B.nonempty value "command" <*> B.optionalInteger value "timeout_seconds" 30 1 120
