{-# LANGUAGE StrictData #-}

module Elwood.Permissions
  ( -- * Permission Configuration
    PermissionConfig (..),
    defaultPermissionConfig,

    -- * Permission Results
    PermissionResult (..),

    -- * Permission Checks
    checkCommandPermission,

    -- * Tool Policies
    ToolPolicy (..),
    getToolPolicy,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Text.Regex.TDFA ((=~))

-- | Policy for tool execution
data ToolPolicy
  = -- | Tool can be executed without approval
    PolicyAllow
  | -- | Tool requires user approval before execution
    PolicyAsk
  | -- | Tool is not allowed to be executed
    PolicyDeny
  deriving stock (Show, Eq)

-- | Configuration for permissions
data PermissionConfig = PermissionConfig
  { -- | Commands that are always allowed (prefix match)
    pcSafeCommands :: [Text],
    -- | Regex patterns that are always blocked
    pcDangerousPatterns :: [Text],
    -- | Per-tool policies (tool name -> policy)
    pcToolPolicies :: Map Text ToolPolicy,
    -- | Default policy for tools not in pcToolPolicies
    pcDefaultPolicy :: ToolPolicy,
    -- | Timeout in seconds for approval requests
    pcApprovalTimeoutSeconds :: Int
  }
  deriving stock (Show, Eq)

-- | Default permission configuration
defaultPermissionConfig :: PermissionConfig
defaultPermissionConfig =
  PermissionConfig
    { pcSafeCommands =
        [ "ls",
          "cat",
          "head",
          "tail",
          "date",
          "whoami",
          "pwd",
          "git status",
          "git log",
          "git diff",
          "echo",
          "wc",
          "find",
          "grep"
        ],
      pcDangerousPatterns =
        [ "\\brm\\b",
          "\\bsudo\\b",
          "\\bchmod\\b",
          "\\bchown\\b",
          "curl.*\\|.*sh",
          "wget.*\\|.*sh",
          "\\bmkfs\\b",
          "\\bdd\\b.*of=/",
          "\\b>\\s*/dev/",
          "\\bformat\\b"
        ],
      pcToolPolicies = Map.empty,
      pcDefaultPolicy = PolicyAllow,
      pcApprovalTimeoutSeconds = 300
    }

-- | Result of a permission check
data PermissionResult
  = -- | Operation is permitted
    Allowed
  | -- | Operation is denied with reason
    Denied Text
  deriving stock (Show, Eq)

-- | Check if running a command is allowed
checkCommandPermission :: PermissionConfig -> Text -> PermissionResult
checkCommandPermission config cmd
  | isSafeCommand = Allowed
  | Just pat <- matchesDangerous = Denied $ "Command matches dangerous pattern: " <> pat
  | otherwise = Allowed
  where
    cmdText = T.strip cmd

    -- Check if command starts with a safe command
    isSafeCommand = any (`T.isPrefixOf` cmdText) (pcSafeCommands config)

    -- Check if command matches any dangerous pattern
    matchesDangerous =
      let patterns = pcDangerousPatterns config
          cmdStr = T.unpack cmdText
       in case filter (\p -> cmdStr =~ T.unpack p) patterns of
            (p : _) -> Just p
            [] -> Nothing

-- | Get the policy for a specific tool
--
-- Looks up the tool in pcToolPolicies, falls back to pcDefaultPolicy
getToolPolicy :: PermissionConfig -> Text -> ToolPolicy
getToolPolicy config toolName =
  Map.findWithDefault (pcDefaultPolicy config) toolName (pcToolPolicies config)
