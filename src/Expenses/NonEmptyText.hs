module Expenses.NonEmptyText (
  NonEmptyText,
  fromText,
  unsafeFromText,
) where

import CustomPrelude
import Data.Aeson (FromJSON (..), ToJSON)
import Data.Aeson qualified as J
import Data.Text qualified as T
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.FromField qualified as SQL
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.ToField (ToField)
import Fmt (Buildable)
import GHC.Records (HasField (..))

-- | Ensures that a Text value is non-empty AND stripped of surrounding whitespace.
newtype NonEmptyText = NonEmptyText Text
  deriving newtype (Eq, Ord, Show, Hashable, ToJSON, Buildable, ToField)
  deriving newtype (NFData)

instance HasField "getNonEmptyText" NonEmptyText Text where
  getField (NonEmptyText txt) = txt

fromText :: Text -> Maybe NonEmptyText
fromText str = do
  let stripped = T.strip str
  if T.null stripped
    then Nothing
    else Just (NonEmptyText stripped)

unsafeFromText :: (HasCallStack) => Text -> NonEmptyText
unsafeFromText t =
  fromText t
    & fromMaybe (error [i|NonEmptyText.unsafeFromText: Text is empty or whitespace: '#{t}'|])

instance FromField NonEmptyText where
  fromField f = do
    text <- fromField @Text f
    case fromText text of
      Just x -> Ok x
      Nothing -> SQL.returnError SQL.ConversionFailed f $ T.unpack [i|Failed to parse to NonEmptyText: '#{text}'|]

instance FromJSON NonEmptyText where
  parseJSON = J.withText "NonEmptyText" \t ->
    case fromText t of
      Just x -> pure x
      Nothing -> fail $ T.unpack [i|Failed to parse to NonEmptyText: '#{t}'|]
