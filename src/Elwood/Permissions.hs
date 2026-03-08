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

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), withObject, withText, (.:?))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Elwood.Aeson (rejectUnknownKeys)
import Elwood.Claude.Types (ToolName)
import Elwood.Positive (Positive)
import GHC.Generics (Generic)
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
  { safePatterns :: Maybe [Text],
    dangerousPatterns :: Maybe [Text],
    toolPolicies :: Maybe (Map ToolName ToolPolicy),
    defaultPolicy :: Maybe ToolPolicy,
    approvalTimeoutSeconds :: Maybe Positive
  }
  deriving stock (Show, Eq, Generic)

-- | Right-biased: @a <> b@ picks @b@'s values when 'Just'.
instance Semigroup PermissionConfigFile where
  a <> b =
    PermissionConfigFile
      { safePatterns = b.safePatterns <|> a.safePatterns,
        dangerousPatterns = b.dangerousPatterns <|> a.dangerousPatterns,
        toolPolicies = b.toolPolicies <|> a.toolPolicies,
        defaultPolicy = b.defaultPolicy <|> a.defaultPolicy,
        approvalTimeoutSeconds = b.approvalTimeoutSeconds <|> a.approvalTimeoutSeconds
      }

instance Monoid PermissionConfigFile where
  mempty = PermissionConfigFile Nothing Nothing Nothing Nothing Nothing

instance FromJSON PermissionConfigFile where
  parseJSON = withObject "PermissionConfigFile" $ \v -> do
    rejectUnknownKeys "PermissionConfigFile" ["safe_patterns", "dangerous_patterns", "tool_policies", "default_policy", "approval_timeout_seconds"] v
    PermissionConfigFile
      <$> v .:? "safe_patterns"
      <*> v .:? "dangerous_patterns"
      <*> v .:? "tool_policies"
      <*> v .:? "default_policy"
      <*> v .:? "approval_timeout_seconds"

-- | Resolve a partial permission config against hardcoded defaults.
resolvePermissions :: PermissionConfigFile -> PermissionConfig
resolvePermissions pcf =
  PermissionConfig
    { safePatterns = fromMaybe [] pcf.safePatterns,
      dangerousPatterns = fromMaybe [] pcf.dangerousPatterns,
      toolPolicies = fromMaybe Map.empty pcf.toolPolicies,
      defaultPolicy = fromMaybe PolicyAllow pcf.defaultPolicy,
      approvalTimeoutSeconds = fromMaybe 300 pcf.approvalTimeoutSeconds
    }

-- | Convert a resolved 'PermissionConfig' back to a 'PermissionConfigFile' (all 'Just').
toPermissionConfigFile :: PermissionConfig -> PermissionConfigFile
toPermissionConfigFile pc =
  PermissionConfigFile
    { safePatterns = Just pc.safePatterns,
      dangerousPatterns = Just pc.dangerousPatterns,
      toolPolicies = Just pc.toolPolicies,
      defaultPolicy = Just pc.defaultPolicy,
      approvalTimeoutSeconds = Just pc.approvalTimeoutSeconds
    }
