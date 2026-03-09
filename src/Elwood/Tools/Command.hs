module Elwood.Tools.Command
  ( mkRunCommandTool,
  )
where

import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Claude.Types (ToolSchema (..))
import Elwood.Command qualified as Cmd
import Elwood.Logging (Logger, logInfo, logWarn)
import Elwood.Permissions (PermissionConfig, PermissionResult (..), checkCommandPermission)
import Elwood.Tools.Types
import System.Exit (ExitCode (..))

-- | Construct a tool for running shell commands
mkRunCommandTool :: Logger -> FilePath -> PermissionConfig -> Tool
mkRunCommandTool logger workspace_ perms =
  Tool
    { schema =
        ToolSchema
          { name = "run_command",
            description =
              "Execute a shell command in the workspace directory. "
                <> "Use this for listing files, checking git status, running builds, etc. "
                <> "The command runs with a 30 second timeout.",
            inputSchema = commandSchema
          },
      execute = \input -> case parseInput input of
        Left err -> pure $ toolError err
        Right (cmd, timeoutSecs) ->
          case checkCommandPermission perms cmd of
            Denied reason -> do
              logWarn logger "Command blocked" [("command", cmd), ("reason", reason)]
              pure $ toolError $ "Permission denied: " <> reason
            Allowed -> do
              logInfo logger "Executing command" [("command", cmd)]
              toToolResult <$> Cmd.runCommandWithTimeout cmd timeoutSecs workspace_
    }

-- | Convert a generic command result to a tool result
toToolResult :: Cmd.CommandResult -> ToolResult
toToolResult r = case r.exitCode of
  ExitSuccess -> toolSuccess r.output
  ExitFailure code ->
    toolError $
      "Command failed with exit code "
        <> T.pack (show code)
        <> ":\n"
        <> r.output

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
