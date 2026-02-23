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
  { -- | Regex patterns that override dangerousPatterns (always allowed)
    safePatterns :: [Text],
    -- | Regex patterns that are always blocked
    dangerousPatterns :: [Text],
    -- | Per-tool policies (tool name -> policy)
    toolPolicies :: Map Text ToolPolicy,
    -- | Default policy for tools not in toolPolicies
    defaultPolicy :: ToolPolicy,
    -- | Timeout in seconds for approval requests
    approvalTimeoutSeconds :: Int
  }
  deriving stock (Show, Eq)

-- | Default permission configuration
defaultPermissionConfig :: PermissionConfig
defaultPermissionConfig =
  PermissionConfig
    { safePatterns =
        [ "^ls\\b",
          "^cat\\b",
          "^head\\b",
          "^tail\\b",
          "^date\\b",
          "^whoami\\b",
          "^pwd\\b",
          "^git status\\b",
          "^git log\\b",
          "^git diff\\b",
          "^echo\\b",
          "^wc\\b",
          "^find\\b",
          "^grep\\b"
        ],
      dangerousPatterns =
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
      toolPolicies = Map.empty,
      defaultPolicy = PolicyAllow,
      approvalTimeoutSeconds = 300
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
    cmdStr = T.unpack cmdText

    -- Check if command matches a safe pattern
    isSafeCommand = any (\p -> cmdStr =~ T.unpack p) config.safePatterns

    -- Check if command matches any dangerous pattern
    matchesDangerous =
      let patterns = config.dangerousPatterns
       in case filter (\p -> cmdStr =~ T.unpack p) patterns of
            (p : _) -> Just p
            [] -> Nothing

-- | Get the policy for a specific tool
--
-- Looks up the tool in toolPolicies, falls back to defaultPolicy
getToolPolicy :: PermissionConfig -> Text -> ToolPolicy
getToolPolicy config toolName_ =
  Map.findWithDefault config.defaultPolicy toolName_ config.toolPolicies
