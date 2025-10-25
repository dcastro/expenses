module Core.APITypes where

import Core.YearMonth
import Prelude

import Core.Display (class Display)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut as J
import Data.Date (Date)
import Data.DateTime (DateTime(..))
import Data.Either (Either(..))
import Data.Formatter.DateTime as F
import Data.Generic.Rep (class Generic)
import Data.JSDate as JS
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Unsafe as Unsafe
import Utils (unsafeFromRight)
import Utils as Utils

-- A date with:
--   * an encoder and decoder for the format "YYYY-MM-DD".
--   * a display instance for the format "DD-MM-YYYY".
newtype JDate = JDate Date

instance DecodeJson JDate where
  decodeJson json = do
    (str :: String) <- J.decodeJson json
    case Unsafe.unsafePerformEffect $ parseJDate str of
      Just jdate -> Right jdate
      Nothing -> Left $ J.UnexpectedValue json

instance EncodeJson JDate where
  encodeJson (JDate date) =
    let
      datetime = DateTime date bottom
      formatted = unsafeFromRight $ F.formatDateTime "YYYY-MM-DD" datetime
    in
      J.fromString formatted

instance Display JDate where
  display (JDate date) = do
    let datetime = DateTime date bottom
    unsafeFromRight $ F.formatDateTime "DD-MM-YYYY" datetime

getJDate :: JDate -> Date
getJDate (JDate date) = date

parseJDate :: String -> Effect (Maybe JDate)
parseJDate str = do
  jsdate <- JS.parse str
  case JS.toDate jsdate of
    Just date -> pure (Just (JDate date))
    Nothing -> pure Nothing

newtype TagName = TagName String

getTagName :: TagName -> String
getTagName (TagName name) = name

newtype TagGroupName = TagGroupName String

getTagGroupName :: TagGroupName -> String
getTagGroupName (TagGroupName name) = name

derive instance Generic TagParams _
derive instance Generic ModifyTransactionType _

derive newtype instance EncodeJson TagName

derive newtype instance DecodeJson TagName
derive newtype instance DecodeJson TagGroupName

derive newtype instance Show JDate
derive newtype instance Show TagName
derive newtype instance Show TagGroupName

derive newtype instance Display TagName
derive newtype instance Display TagGroupName

derive newtype instance Eq JDate
derive newtype instance Eq TagName
derive newtype instance Eq TagGroupName

derive newtype instance Ord JDate
derive newtype instance Ord TagName

----------------------------------------------------------------------------
-- GET /transactions
----------------------------------------------------------------------------

type GetTransactions =
  { transactions :: Array TransactionItem
  , groupsStats :: Array TagGroupStats
  , totalAmountCents :: Int
  }

type TagGroupStats =
  { name :: TagGroupName
  , groupTotalAmountCents :: Int
  , groupPercentage :: Int
  , tags :: Array TagStats
  }

type TagStats =
  { name :: TagName
  , tagTotalAmountCents :: Int
  , tagPercentage :: Int
  , tagPercentageOfTotal :: Int
  , tagAmountPerMonth :: Map YearMonth Int
  }

type TransactionItem =
  { transactionId :: TransactionId
  , itemIndex :: Int
  , account :: String
  , date :: JDate
  , desc :: String
  , totalAmountCents :: Int
  , isExpense :: Boolean
  , itemAmountCents :: Int
  , tag :: Maybe TagName
  , details :: String
  }

type TransactionId = String

type TransactionItemId =
  { txId :: TransactionId
  , itemIndex :: Int
  }

----------------------------------------------------------------------------
-- POST /search
----------------------------------------------------------------------------

type RawSearchParams =
  { allFields :: String
  , date :: String
  , account :: Maybe String
  , desc :: String
  , amount :: String
  , tag :: Maybe TagParams
  , notes :: String
  , isExpense :: Maybe Boolean
  }

data TagParams
  = NoTag
  | SomeTag TagName

derive instance Eq TagParams

instance EncodeJson TagParams where
  encodeJson = Utils.encodeAesonSum

instance Show TagParams where
  show = genericShow

----------------------------------------------------------------------------
-- PUT /transactions
----------------------------------------------------------------------------

type ModifyTransaction =
  { transactionId :: String
  , itemIndex :: Int
  , actionType :: ModifyTransactionType
  }

data ModifyTransactionType
  = ModifyTag ModifyTag
  | ModifyIsExpense ModifyIsExpense
  | ModifyDetails ModifyDetails

type ModifyTag =
  { tag :: TagName
  }

type ModifyDetails =
  { details :: String
  }

type ModifyIsExpense =
  { isExpense :: Boolean
  }

instance EncodeJson ModifyTransactionType where
  encodeJson = Utils.encodeAesonSum

----------------------------------------------------------------------------
-- POST /transactions
----------------------------------------------------------------------------

data NewTransactionItem = NewTransactionItem
  { date :: JDate
  , totalAmountCents :: Int
  , isExpense :: Boolean
  , tag :: TagName
  , details :: String
  , account :: String
  , desc :: String
  }

derive instance Generic NewTransactionItem _

instance EncodeJson NewTransactionItem where
  encodeJson (NewTransactionItem payload) = J.encodeJson payload

----------------------------------------------------------------------------
-- GET /transactions/:id/items
----------------------------------------------------------------------------

type ShortTransactionItem =
  { itemAmountCents :: Int
  , details :: String
  , tag :: Maybe TagName
  , isExpense :: Boolean
  }

----------------------------------------------------------------------------
-- GET /dates
----------------------------------------------------------------------------

type DateRange =
  { minMonth :: YearMonth
  , maxMonth :: YearMonth
  }

----------------------------------------------------------------------------
-- POST /transactions/:id/split
----------------------------------------------------------------------------

type NewShortTransactionItem =
  { itemAmountCents :: Int
  , details :: String
  , tag :: TagName
  , isExpense :: Boolean
  }
