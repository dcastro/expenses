module Config where

import CustomPrelude
import Data.Aeson (FromJSON (..))
import Data.Aeson.TH (defaultOptions, deriveFromJSON)
import Data.HashMap.Strict qualified as HM
import Data.Set qualified as Set
import Data.Yaml qualified as Y
import Deriving.Aeson (CustomJSON (..), FieldLabelModifier, StripSuffix)
import Expenses.NonEmptyText qualified as NET
import Text.Regex.TDFA ((=~))
import Types

cronUser :: Admin
cronUser = Admin $ Username $ NET.unsafeFromText "cron"

data AppConfig = AppConfig
  { accountInfos :: [AccountInfo]
  , admins :: [Text]
  , allTagGroups :: HashMap TagGroupName [TagName]
  , ungroupedTags :: [TagName]
  , cronSchedule :: Text
  , categoryPatterns :: [CategoryPatternEntry]
  , notExpenses :: [Text]
  }
  deriving stock (Eq, Show)

data CategoryPatternEntry = CategoryPatternEntry
  -- Note: `pattern` is a keyword, so we use a suffix and strip it in the JSON instance.
  { pattern_ :: Text
  , tag :: TagName
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripSuffix "_"]] CategoryPatternEntry

$( mconcat
     [ deriveFromJSON defaultOptions ''AppConfig
     ]
 )
loadAppConfig :: (MonadIO m) => FilePath -> m AppConfig
loadAppConfig path = do
  Y.decodeFileThrow path

allGroupedTags :: AppConfig -> Set.Set TagName
allGroupedTags AppConfig{allTagGroups} =
  Set.fromList $ concat $ HM.elems allTagGroups

allKnownTags :: AppConfig -> Set.Set TagName
allKnownTags config =
  allGroupedTags config `Set.union` Set.fromList config.ungroupedTags

tryMkAdmin :: AppConfig -> Username -> Maybe Admin
tryMkAdmin config user =
  if config.admins & any \regex -> user.unUsername.getNonEmptyText =~ regex
    then Just $ Admin user
    else Nothing
