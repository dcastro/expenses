module Types where

import Control.Lens (classIdFields, makeLensesWith)
import CustomPrelude
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Text qualified as T
import Data.Time (Day)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import Expenses.Linear (LinearToJSON)
import Expenses.NonEmptyText (NonEmptyText)
import Expenses.NonEmptyText qualified as NET
import Fmt (Buildable)
import Prelude.Linear qualified as Linear

newtype Username = Username {unUsername :: NonEmptyText}
  deriving newtype (Hashable, Eq, Ord, Show, ToJSON, FromJSON, LinearToJSON)

mkUsername :: Text -> Maybe Username
mkUsername raw = do
  let withoutDomain = T.takeWhile (\c -> c /= '@') raw
  net <- NET.fromText withoutDomain
  pure $ Username net

newtype Admin = Admin {unAdmin :: Username}
  deriving newtype (Hashable, Eq, Ord, Show, ToJSON, FromJSON, LinearToJSON)

newtype TagName = TagName {unTagName :: NonEmptyText}
  deriving newtype (Hashable, Eq, Ord, Show, ToJSON, FromJSON, Buildable, FromField, ToField, LinearToJSON)
  deriving newtype (NFData)

newtype TagGroupName = TagGroupName {unTagGroupName :: Text}
  deriving newtype (IsString, Hashable, Eq, Ord, Show, ToJSON, FromJSON, Buildable, FromField, ToField)

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
  , -- Whether transactions from this account should be treated as expenses.
    isExpenseAccount :: Bool
  , -- Credit accounts show transactions as positive numbers, and debit accounts as negative numbers.($)
    -- For credit accounts, we should set `flip sign = true`.
    flipSign :: Bool
  }

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
     , deriveJSON defaultOptions ''TransactionResponse
     , deriveJSON defaultOptions ''TransactionObj
     , deriveJSON defaultOptions ''ApiTransaction
     , deriveJSON defaultOptions ''Amount
     , deriveJSON defaultOptions ''NewTokenResponse
     , deriveJSON defaultOptions ''BalancesResponse
     , deriveJSON defaultOptions ''DetailsResponse
     ]
 )

makeLensesWith classIdFields ''TransactionResponse
makeLensesWith classIdFields ''TransactionObj
makeLensesWith classIdFields ''ApiTransaction
makeLensesWith classIdFields ''Amount
makeLensesWith classIdFields ''NewTokenResponse
makeLensesWith classIdFields ''AccountInfo
makeLensesWith classIdFields ''TransactionRecord
makeLensesWith classIdFields ''TransactionItemRecord
makeLensesWith classIdFields ''FECents
makeLensesWith classIdFields ''BECents
