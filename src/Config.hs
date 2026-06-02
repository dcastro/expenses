module Config where

import CustomPrelude
import Data.Aeson (FromJSON (..))
import Data.Aeson.TH (defaultOptions, deriveFromJSON)
import Data.HashMap.Strict qualified as HM
import Data.Set qualified as Set
import Data.Yaml qualified as Y
import Expenses.NonEmptyText qualified as NET
import Text.Regex.TDFA ((=~))
import Types

cronUser :: Admin
cronUser = Admin $ Username $ NET.unsafeFromText "cron"

data AppConfig = AppConfig
  { institutions :: [InstitutionInfo]
  , admins :: [Text]
  , allTagGroups :: HashMap TagGroupName [TagName]
  , ungroupedTags :: [TagName]
  , cronSchedule :: Text
  , tagPatterns :: [TagPatternEntry]
  , notExpenses :: [Text]
  }
  deriving stock (Eq, Show)

data TagPatternEntry = TagPatternEntry
  { tag :: TagName
  , -- For this tag to be assigned to a transaction,
    --  all the substrings in at least one of the entries in `contains` must be present in the transaction description.
    contains :: [Substrings]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- The tx description must contain all these substrings for the tag to be assigned.
type Substrings = [Substring]

type Substring = Text

$( mconcat
     [ deriveFromJSON defaultOptions ''AppConfig
     ]
 )
loadAppConfig :: (MonadIO m) => FilePath -> m AppConfig
loadAppConfig path = do
  Y.decodeFileThrow path

allAccountInfos :: AppConfig -> [InstitutionAccountInfo]
allAccountInfos AppConfig{institutions} =
  institutions >>= (.accounts)

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
