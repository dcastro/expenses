module Config where

import CustomPrelude
import Data.Aeson (FromJSON)
import Data.Aeson.TH (defaultOptions, deriveFromJSON)
import Data.HashMap.Strict qualified as HM
import Data.Set qualified as Set
import Data.Yaml qualified as Y
import Deriving.Aeson (CustomJSON (..), FieldLabelModifier, StripSuffix)
import Expenses.NonEmptyText qualified as NET
import Types

cronUser :: Admin
cronUser = Admin $ Username $ NET.unsafeFromText "cron"

data AppConfig = AppConfig
  { accountInfos :: [AccountInfo]
  , admins :: [Username]
  , allTagGroups :: HashMap TagGroupName [TagName]
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

allKnownTags :: AppConfig -> Set.Set TagName
allKnownTags AppConfig{allTagGroups} =
  Set.fromList $ concat $ HM.elems allTagGroups

tryMkAdmin :: AppConfig -> Username -> Maybe Admin
tryMkAdmin config user =
  if user `elem` config.admins
    then Just $ Admin user
    else Nothing
