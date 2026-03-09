module Elwood.Permissions
  ( -- * Permission Configuration
    PermissionConfig (..),
    defaultPermissionConfig,

    -- * Permission File Configuration (partial/mergeable)
    PermissionConfigFile (..),
    resolvePermissions,
    toPermissionConfigFile,

    -- * Permission Results
    PermissionResult (..),

    -- * Permission Checks
    checkCommandPermission,

    -- * Tool Policies
    ToolPolicy (..),
    getToolPolicy,
  )
where

import Data.Aeson (FromJSON (..), withObject, withText, (.:?))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Aeson (rejectUnknownKeys)
import Elwood.Claude.Types (ToolName)
import Elwood.Positive (Positive)
import GHC.Generics (Generic, Generically (..))
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

instance FromJSON ToolPolicy where
  parseJSON = withText "ToolPolicy" $ \t ->
    case T.toLower t of
      "allow" -> pure PolicyAllow
      "ask" -> pure PolicyAsk
      "deny" -> pure PolicyDeny
      other -> fail $ "Unknown tool policy: " <> T.unpack other

-- | Configuration for permissions
data PermissionConfig = PermissionConfig
  { -- | Regex patterns that override dangerousPatterns (always allowed)
    safePatterns :: [Text],
    -- | Regex patterns that are always blocked
    dangerousPatterns :: [Text],
    -- | Per-tool policies (tool name -> policy)
    toolPolicies :: Map ToolName ToolPolicy,
    -- | Default policy for tools not in toolPolicies
    defaultPolicy :: ToolPolicy,
    -- | Timeout in seconds for approval requests
    approvalTimeoutSeconds :: Positive
  }
  deriving stock (Show, Eq)

-- | Default permission configuration.
defaultPermissionConfig :: PermissionConfig
defaultPermissionConfig = resolvePermissions mempty

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
getToolPolicy :: PermissionConfig -> ToolName -> ToolPolicy
getToolPolicy config toolName_ =
  Map.findWithDefault config.defaultPolicy toolName_ config.toolPolicies

-- | Permission configuration from YAML file (partial, mergeable)
data PermissionConfigFile = PermissionConfigFile
  { safePatterns :: Last [Text],
    dangerousPatterns :: Last [Text],
    toolPolicies :: Last (Map ToolName ToolPolicy),
    defaultPolicy :: Last ToolPolicy,
    approvalTimeoutSeconds :: Last Positive
  }
  deriving stock (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically PermissionConfigFile

instance FromJSON PermissionConfigFile where
  parseJSON = withObject "PermissionConfigFile" $ \v -> do
    rejectUnknownKeys "PermissionConfigFile" ["safe_patterns", "dangerous_patterns", "tool_policies", "default_policy", "approval_timeout_seconds"] v
    PermissionConfigFile . Last
      <$> v .:? "safe_patterns"
      <*> (Last <$> v .:? "dangerous_patterns")
      <*> (Last <$> v .:? "tool_policies")
      <*> (Last <$> v .:? "default_policy")
      <*> (Last <$> v .:? "approval_timeout_seconds")

-- | Resolve a partial permission config against hardcoded defaults.
resolvePermissions :: PermissionConfigFile -> PermissionConfig
resolvePermissions pcf =
  PermissionConfig
    { safePatterns = fromMaybe [] (getLast pcf.safePatterns),
      dangerousPatterns = fromMaybe [] (getLast pcf.dangerousPatterns),
      toolPolicies = fromMaybe Map.empty (getLast pcf.toolPolicies),
      defaultPolicy = fromMaybe PolicyAllow (getLast pcf.defaultPolicy),
      approvalTimeoutSeconds = fromMaybe 300 (getLast pcf.approvalTimeoutSeconds)
    }

-- | Convert a resolved 'PermissionConfig' back to a 'PermissionConfigFile' (all 'Just').
toPermissionConfigFile :: PermissionConfig -> PermissionConfigFile
toPermissionConfigFile pc =
  PermissionConfigFile
    { safePatterns = Last (Just pc.safePatterns),
      dangerousPatterns = Last (Just pc.dangerousPatterns),
      toolPolicies = Last (Just pc.toolPolicies),
      defaultPolicy = Last (Just pc.defaultPolicy),
      approvalTimeoutSeconds = Last (Just pc.approvalTimeoutSeconds)
    }
