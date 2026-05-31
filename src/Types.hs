module Types where

import Control.Lens (classIdFields, makeLensesWith)
import CustomPrelude
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Aeson qualified as J
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Text qualified as T
import Data.Time (Day)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.FromField qualified as SQL
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.ToField qualified as SQL
import Expenses.Linear (LinearToJSON)
import Expenses.NonEmptyText (NonEmptyText)
import Expenses.NonEmptyText qualified as NET
import Fmt (Buildable)
import Prelude.Linear qualified as Linear

newtype Username = Username {unUsername :: NonEmptyText}
  deriving newtype (Hashable, Eq, Ord, Show, ToJSON, FromJSON, LinearToJSON)

-- >>> mkUsername "diogo.filipe.acastro@gmail.com"
-- Just "diogo.filipe.acastro"
-- >>> mkUsername "diogo.castro@serokell.io"
-- Just "diogo.castro@serokell.io"
mkUsername :: Text -> Maybe Username
mkUsername raw = do
  let withoutDomain = T.stripSuffix "@gmail.com" raw & fromMaybe raw
  net <- NET.fromText withoutDomain
  pure $ Username net

newtype Admin = Admin {unAdmin :: Username}
  deriving newtype (Hashable, Eq, Ord, Show, ToJSON, FromJSON, LinearToJSON)

newtype TagName = TagName {unTagName :: NonEmptyText}
  deriving newtype (Hashable, Eq, Ord, Show, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Buildable, FromField, ToField, LinearToJSON)
  deriving newtype (NFData)

newtype TagGroupName = TagGroupName {unTagGroupName :: Text}
  deriving newtype (IsString, Hashable, Eq, Ord, Show, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Buildable, FromField, ToField)

-- Amount in cents, for use in front-end communication.
-- Expenses are positive, refunds are negative.
--
-- This purposefully does not have FromField or ToField instances, to avoid
-- accidentally using it in the back-end.
newtype FECents = FECents {getCents :: Int}
  deriving newtype (Eq, Ord, Show, Num, ToJSON, FromJSON, LinearToJSON)
  deriving newtype (Linear.Dupable, Linear.Consumable, Linear.Movable)

-- Amount in cents, for use in back-end communication and storage.
-- Expenses are negative, refunds are positive.
--
-- This purposefully does not have ToJSON or FromJSON instances, to avoid
-- accidentally using it in the front-end.
newtype BECents = BECents {getCents :: Int}
  deriving newtype (Eq, Ord, Show, Num, FromField, ToField)
  deriving newtype (Linear.Dupable, Linear.Consumable, Linear.Movable)
  deriving newtype (NFData)

toBE :: FECents %1 -> BECents
toBE (FECents x) = BECents (Linear.negate x)

toFE :: BECents %1 -> FECents
toFE (BECents x) = FECents (Linear.negate x)

data NewTokenRequest = NewTokenRequest
  { secretId :: Text
  , secretKey :: Text
  }

data SyncStatus
  = SyncSuccess
  | SyncError
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

syncStatusToText :: SyncStatus -> Text
syncStatusToText = \case
  SyncSuccess -> "success"
  SyncError -> "error"

syncStatusFromText :: Text -> Maybe SyncStatus
syncStatusFromText = \case
  "success" -> Just SyncSuccess
  "error" -> Just SyncError
  _ -> Nothing

instance ToJSON SyncStatus where
  toJSON = J.String . syncStatusToText

instance FromJSON SyncStatus where
  parseJSON = J.withText "SyncStatus" \txt ->
    case syncStatusFromText txt of
      Just st -> pure st
      Nothing -> fail $ toString [i|Invalid sync status: #{txt}|]

instance ToField SyncStatus where
  toField = SQL.toField . syncStatusToText

instance FromField SyncStatus where
  fromField f = do
    txt <- SQL.fromField @Text f
    case syncStatusFromText txt of
      Just st -> pure st
      Nothing -> SQL.returnError SQL.ConversionFailed f (toString [i|Invalid sync status in DB: #{txt}|])

data CreateRequisitionRequest = CreateRequisitionRequest
  { redirect :: Text
  , institutionId :: Text
  , reference :: Text
  , userLanguage :: Text
  }
  deriving stock (Show, Eq, Generic)

data CreateRequisitionResponse = CreateRequisitionResponse
  { id :: Text
  , link :: Text
  }
  deriving stock (Show, Eq, Generic)

newtype RequisitionsResponse = RequisitionsResponse
  { results :: [Requisition]
  }
  deriving stock (Show, Eq, Generic)

data Requisition = Requisition
  { id :: Text
  , accounts :: [Text]
  , institutionId :: Text
  }
  deriving stock (Show, Eq, Generic)

newtype TransactionResponse = TransactionResponse
  { transactions :: TransactionObj
  }
  deriving stock (Show, Eq, Generic)

newtype TransactionObj = TransactionObj
  { booked :: [ApiTransaction]
  }
  deriving stock (Show, Eq, Generic)

-- A transaction that came from the Nordigen API
data ApiTransaction = ApiTransaction
  { bookingDate :: Day
  , remittanceInformationUnstructured :: Text
  , transactionAmount :: Amount
  , transactionId :: Maybe Text
  , entryReference :: Maybe Text
  , valueDate :: Day
  }
  deriving stock (Show, Eq, Generic)

data Amount = Amount
  { amount :: Text
  , currency :: Text
  }
  deriving stock (Show, Eq, Generic)

newtype NewTokenResponse = NewTokenResponse
  { access :: Text
  }

newtype BalancesResponse = BalancesResponse
  { balances :: J.Array
  }

newtype DetailsResponse = DetailsResponse
  { account :: J.Object
  }

data AccountInfo = AccountInfo
  { accountName :: Text
  , accountId :: Text
  , institutionId :: Text
  , -- Whether transactions from this account should be treated as expenses.
    isExpenseAccount :: Bool
  , -- Credit accounts show transactions as positive numbers, and debit accounts as negative numbers.($)
    -- For credit accounts, we should set `flip sign = true`.
    flipSign :: Bool
  }
  deriving stock (Eq, Show)

data InstitutionInfo = InstitutionInfo
  { institutionId :: Text
  , accounts :: [InstitutionAccountInfo]
  }
  deriving stock (Eq, Show)

data InstitutionAccountInfo = InstitutionAccountInfo
  { accountName :: Text
  , accountId :: Text
  , -- Whether transactions from this account should be treated as expenses.
    isExpenseAccount :: Bool
  , -- Credit accounts show transactions as positive numbers, and debit accounts as negative numbers.($)
    -- For credit accounts, we should set `flip sign = true`.
    flipSign :: Bool
  }
  deriving stock (Eq, Show)

data TransactionRecord = TransactionRecord
  { transactionId :: Text
  , account :: Text
  , date :: Day
  , desc :: Text
  , totalAmountCents :: BECents
  , items :: [TransactionItemRecord]
  }
  deriving stock (Show, Eq)

data TransactionItemRecord = TransactionItemRecord
  { itemAmountCents :: BECents
  , tag :: Maybe TagName
  , details :: Text
  , isExpense :: Bool
  }
  deriving stock (Show, Eq)

$( mconcat
     [ deriveJSON (aesonDrop 0 snakeCase) ''NewTokenRequest
     , deriveJSON (aesonDrop 0 snakeCase) ''CreateRequisitionRequest
     , deriveJSON (aesonDrop 0 snakeCase) ''CreateRequisitionResponse
     , deriveJSON (aesonDrop 0 snakeCase) ''RequisitionsResponse
     , deriveJSON (aesonDrop 0 snakeCase) ''Requisition
     , deriveJSON defaultOptions ''TransactionResponse
     , deriveJSON defaultOptions ''TransactionObj
     , deriveJSON defaultOptions ''ApiTransaction
     , deriveJSON defaultOptions ''Amount
     , deriveJSON defaultOptions ''NewTokenResponse
     , deriveJSON defaultOptions ''BalancesResponse
     , deriveJSON defaultOptions ''DetailsResponse
     , deriveJSON defaultOptions ''AccountInfo
     , deriveJSON defaultOptions ''InstitutionInfo
     , deriveJSON defaultOptions ''InstitutionAccountInfo
     ]
 )

makeLensesWith classIdFields ''TransactionResponse
makeLensesWith classIdFields ''TransactionObj
makeLensesWith classIdFields ''ApiTransaction
makeLensesWith classIdFields ''Amount
makeLensesWith classIdFields ''NewTokenResponse
makeLensesWith classIdFields ''AccountInfo
makeLensesWith classIdFields ''InstitutionInfo
makeLensesWith classIdFields ''InstitutionAccountInfo
makeLensesWith classIdFields ''TransactionRecord
makeLensesWith classIdFields ''TransactionItemRecord
makeLensesWith classIdFields ''FECents
makeLensesWith classIdFields ''BECents
