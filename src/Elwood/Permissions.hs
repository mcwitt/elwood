module Elwood.Permissions
  ( PermissionConfig (..),
    defaultPermissionConfig,
    PermissionConfigFile (..),
    resolvePermissions,
    toPermissionConfigFile,
    PermissionResult (..),
    checkCommandPermission,
    ToolPolicy (..),
    getToolPolicy,
  )
where

import Data.Aeson (FromJSON (..), genericParseJSON)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Elwood.Aeson (enum, recordOptions)
import Elwood.Claude.Types (ToolName)
import Elwood.Pattern (Pattern)
import Elwood.Pattern qualified as Pattern
import Elwood.Positive (Positive, literal)
import GHC.Generics (Generic, Generically (..))

data ToolPolicy = PolicyAllow | PolicyAsk | PolicyDeny
  deriving stock (Show, Eq)

instance FromJSON ToolPolicy where
  parseJSON = enum "tool policy" [("allow", PolicyAllow), ("ask", PolicyAsk), ("deny", PolicyDeny)]

data PermissionConfig = PermissionConfig
  { safePatterns :: [Pattern],
    dangerousPatterns :: [Pattern],
    toolPolicies :: Map ToolName ToolPolicy,
    defaultPolicy :: ToolPolicy,
    approvalTimeoutSeconds :: Positive
  }
  deriving stock (Show, Eq)

data PermissionConfigFile = PermissionConfigFile
  { safePatterns :: Last [Pattern],
    dangerousPatterns :: Last [Pattern],
    toolPolicies :: Last (Map ToolName ToolPolicy),
    defaultPolicy :: Last ToolPolicy,
    approvalTimeoutSeconds :: Last Positive
  }
  deriving stock (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically PermissionConfigFile

instance FromJSON PermissionConfigFile where
  parseJSON = genericParseJSON recordOptions

resolvePermissions :: PermissionConfigFile -> PermissionConfig
resolvePermissions p =
  PermissionConfig
    (fromMaybe [] (getLast p.safePatterns))
    (fromMaybe [] (getLast p.dangerousPatterns))
    (fromMaybe mempty (getLast p.toolPolicies))
    (fromMaybe PolicyAllow (getLast p.defaultPolicy))
    (fromMaybe (literal @300) (getLast p.approvalTimeoutSeconds))

defaultPermissionConfig :: PermissionConfig
defaultPermissionConfig = resolvePermissions mempty

toPermissionConfigFile :: PermissionConfig -> PermissionConfigFile
toPermissionConfigFile p =
  PermissionConfigFile
    (present p.safePatterns)
    (present p.dangerousPatterns)
    (present p.toolPolicies)
    (present p.defaultPolicy)
    (present p.approvalTimeoutSeconds)
  where
    present = Last . Just

data PermissionResult = Allowed | Denied Text
  deriving stock (Show, Eq)

checkCommandPermission :: PermissionConfig -> Text -> PermissionResult
checkCommandPermission config command
  | any matches config.safePatterns = Allowed
  | Just compiled <- find matches config.dangerousPatterns = Denied ("Command matches dangerous pattern: " <> Pattern.source compiled)
  | otherwise = Allowed
  where
    matches compiled = Pattern.matches compiled (Text.strip command)

getToolPolicy :: PermissionConfig -> ToolName -> ToolPolicy
getToolPolicy p name = Map.findWithDefault p.defaultPolicy name p.toolPolicies
