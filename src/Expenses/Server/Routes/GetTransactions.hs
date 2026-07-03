module Expenses.Server.Routes.GetTransactions where

import Config (AppConfig)
import Config qualified
import Control.Lens
import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveFromJSON, deriveToJSON)
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Map.Lazy qualified as Map
import Data.Set qualified as Set
import Data.Time (Day)
import Data.Time.Calendar.Month (Month, pattern MonthDay)
import Data.Time.Calendar.Month qualified as Time
import Database qualified as Db
import Effectful
import Effectful.Reader.Static (asks)
import Expenses.Effects
import Expenses.Server.Utils (MapAsList (..))
import Types

data GetTransactions = GetTransactions
  { transactions :: [TransactionItem]
  , groupsStats :: [TagGroupStats]
  , totalAmountCents :: FECents
  }
  deriving stock (Show, Eq)

data TagGroupStats = TagGroupStats
  { name :: TagGroupName
  , groupTotalAmountCents :: FECents
  , groupPercentage :: Int
  , tags :: [TagStats]
  }
  deriving stock (Show, Eq)

data TagStats = TagStats
  { name :: TagName
  , tagTotalAmountCents :: FECents
  , tagPercentage :: Int
  , tagPercentageOfTotal :: Int
  , tagAmountPerMonth :: MapAsList Month FECents
  }
  deriving stock (Show, Eq)

data TransactionItem = TransactionItem
  { transactionId :: Text
  , itemIndex :: Int
  , account :: Text
  , date :: Day
  , desc :: Text
  , totalAmountCents :: FECents
  , isExpense :: Bool
  , itemAmountCents :: FECents
  , tag :: Maybe TagName
  , details :: Text
  , budgetOverride :: Maybe Bool
  -- ^ Overrides whether this item is part of the budget:
  --   Nothing -> default matching rules; Just True -> always include; Just False -> always exclude.
  }
  deriving stock (Show, Eq)

data NewShortTransactionItem = NewShortTransactionItem
  { itemAmountCents :: FECents
  , details :: Text
  , tag :: TagName
  , isExpense :: Bool
  }
  deriving stock (Show, Eq)

data ShortTransactionItem = ShortTransactionItem
  { itemAmountCents :: FECents
  , details :: Text
  , tag :: Maybe TagName
  , isExpense :: Bool
  }
  deriving stock (Show, Eq)

$( mconcat
     [ deriveToJSON defaultOptions ''GetTransactions
     , deriveToJSON defaultOptions ''TagGroupStats
     , deriveToJSON defaultOptions ''TagStats
     , deriveToJSON defaultOptions ''TransactionItem
     , deriveToJSON defaultOptions ''ShortTransactionItem
     , deriveFromJSON defaultOptions ''NewShortTransactionItem
     ]
 )

makeLensesWith classIdFields ''TransactionItem

getTransactionsHandler ::
  (Reader Env :> es, SQLite :> es) =>
  Month -> Month -> Eff es GetTransactions
getTransactionsHandler monthStart monthEnd = do
  config <- asks @Env (.config)
  let dayStart = MonthDay monthStart 1
  let dayEnd = MonthDay (monthEnd & Time.addMonths 1) 1

  txs <-
    useConnection (\conn -> Db.getTransactionsByDate conn dayStart dayEnd)
      <&> filter (\tx -> tx.isExpense)
      <&> fmap (\row -> convertRowToItem row)
      <&> List.sortBy ((compare `on` (.date)) <> (compare `on` (.transactionId)))

  -- NOTE: transactions without tags are excluded from group stats.
  let txsWithTags :: [(TransactionItem, TagName)] =
        txs
          & mapMaybe \tx -> do
            tag <- tx.tag
            Just (tx, tag)
  let groupStats = mkGroupStats config txsWithTags

  pure
    GetTransactions
      { transactions = txs
      , groupsStats = groupStats
      , totalAmountCents = sumOf (each . itemAmountCents) txs
      }

convertRowToItem :: Db.TransactionJoinedRow %1 -> TransactionItem
convertRowToItem
  Db.TransactionJoinedRow
    { transactionId
    , account
    , date
    , desc
    , totalAmountCents
    , isExpense
    , itemIndex
    , itemAmountCents
    , tag
    , details
    , budgetOverride
    } =
    TransactionItem
      { transactionId = transactionId
      , itemIndex = itemIndex
      , account = account
      , date = date
      , desc = desc
      , totalAmountCents = toFE totalAmountCents
      , isExpense = isExpense
      , itemAmountCents = toFE itemAmountCents
      , tag = tag
      , details = details
      , budgetOverride = budgetOverride
      }

convertItemToRow :: TransactionItem %1 -> Db.TransactionJoinedRow
convertItemToRow TransactionItem{transactionId, itemIndex, account, date, desc, totalAmountCents, isExpense, itemAmountCents, tag, details, budgetOverride} =
  Db.TransactionJoinedRow
    { transactionId = transactionId
    , account = account
    , date = date
    , desc = desc
    , totalAmountCents = toBE totalAmountCents
    , isExpense = isExpense
    , itemIndex = itemIndex
    , itemAmountCents = toBE itemAmountCents
    , tag = tag
    , details = details
    , budgetOverride = budgetOverride
    }

mkTagStats :: FECents -> [(TagName, [TransactionItem])] -> ([TagStats], FECents)
mkTagStats txsTotal tagTxs =
  (tagStats, tagGroupTotal)
 where
  tagGroupTotal = sumOf @FECents (each . _2 . each . itemAmountCents) tagTxs

  tagStats = do
    (tag, txs) <- tagTxs
    let tagTotal = sumOf @FECents (each . itemAmountCents) txs
    let percentage = if tagGroupTotal == 0 then 0 else (tagTotal.getCents * 100) `div` tagGroupTotal.getCents
    let percentageOfTotal = if txsTotal == 0 then 0 else (tagTotal.getCents * 100) `div` txsTotal.getCents
    pure
      TagStats
        { name = tag
        , tagTotalAmountCents = tagTotal
        , tagPercentage = percentage
        , tagPercentageOfTotal = percentageOfTotal
        , tagAmountPerMonth =
            MapAsList $ Map.fromListWith (+) do
              tx <- txs
              pure (dayToMonth tx.date, tx.itemAmountCents)
        }

mkGroupStats' :: [(TagGroupName, [TagStats], FECents)] -> [TagGroupStats]
mkGroupStats' groups = do
  let total = sumOf @FECents (each . _3) groups
  (grpName, tagStats, grpTotal) <- groups
  pure
    TagGroupStats
      { name = grpName
      , groupTotalAmountCents = grpTotal
      , groupPercentage = if total == 0 then 0 else (grpTotal.getCents * 100) `div` total.getCents
      , tags = tagStats
      }

-- NOTE: transactions without tags are excluded from group stats.
mkGroupStats :: AppConfig -> [(TransactionItem, TagName)] -> [TagGroupStats]
mkGroupStats config txs = do
  let totalAmountCents = sumOf (each . _1 . itemAmountCents) txs

  let allTagTxs :: Map TagName [TransactionItem] =
        Map.fromListWith (++) do
          (tx, tag) <- txs
          pure (tag, [tx])

  let tagGroups :: [(TagGroupName, [TagStats], FECents)] =
        config.allTagGroups
          & HM.toList
          & mapMaybe \(tgroup :: TagGroupName, tags :: [TagName]) -> do
            let (tagStats, total) =
                  mkTagStats totalAmountCents $
                    tags
                      & ( mapMaybe \tag -> do
                            txs <- Map.lookup tag allTagTxs
                            Just (tag, txs)
                        )

            if null tagStats
              then
                Nothing
              else do
                let tagStatsSorted =
                      tagStats
                        & List.sortOn (Down . (.tagTotalAmountCents))
                Just (tgroup, tagStatsSorted, total)

  let groupedTags = Config.allGroupedTags config

  -- Find tags in txs that are not in groupedTags
  let otherTagTxs =
        Map.toList allTagTxs
          & filter (\(tag, _) -> not (Set.member tag groupedTags))

  let (otherTagStats, otherTotal) = mkTagStats totalAmountCents otherTagTxs

  let tagGroupsWithOther =
        if null otherTagStats
          then tagGroups
          else
            tagGroups
              ++ [("Other", List.sortOn (Down . (.tagTotalAmountCents)) otherTagStats, otherTotal)]

  mkGroupStats' tagGroupsWithOther
    & List.sortOn (Down . (.groupTotalAmountCents))

dayToMonth :: Day -> Month
dayToMonth (MonthDay month _) = month
