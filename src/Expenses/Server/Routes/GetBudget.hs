module Expenses.Server.Routes.GetBudget where

import Config qualified
import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (DayOfMonth, gregorianMonthLength, toGregorian, utctDay)
import Data.Time qualified as Time
import Data.Time.Calendar.Month (Month, pattern YearMonth)
import Data.Vector.Algorithms qualified as V
import Database (SearchParams (..))
import Database qualified as Db
import Effectful
import Effectful.Reader.Static (asks)
import Effectful.Time qualified as Time
import Expenses.Effects
import Expenses.NonEmptyText qualified as NET
import Expenses.Server.Routes.GetTransactions (TransactionItem (..))
import Expenses.Server.Routes.GetTransactions qualified as GetTransactions
import Expenses.Server.Utils (MapAsList (..))
import Types (FECents (..), TagGroupName (..), TagName)

data BudgetTagGroupStats = BudgetTagGroupStats
  { spentToDateCents :: FECents
  , limitCents :: FECents
  , tags :: [Maybe TagName]
  }
  deriving stock (Show, Eq)

data BudgetInfo = BudgetInfo
  { monthlyLimitCents :: FECents
  , expectedSpendingToDateCents :: FECents
  -- ^ How much we expect to spend by this point in the month.
  , actualSpendingToDateCents :: FECents
  -- ^ How much we've spent so far this month.
  , overUnderCents :: FECents
  -- ^ How much we're over or under the expected spending for this point in the month.
  , transactions :: Vector TransactionItem
  , tagGroupStats :: MapAsList TagGroupName BudgetTagGroupStats
  }
  deriving stock (Show, Eq)

$( mconcat
     [ deriveToJSON defaultOptions ''BudgetTagGroupStats
     , deriveToJSON defaultOptions ''BudgetInfo
     ]
 )

getBudgetHandler ::
  (Reader Env :> es, SQLite :> es, Time :> es, Log :> es) =>
  Eff es BudgetInfo
getBudgetHandler = do
  today <- utctDay <$> Time.currentTime
  let (year, month, todayDayNum) = toGregorian today
  let totalDays = gregorianMonthLength year month

  config <- asks @Env (.config)
  let budgetGroups = config.budget.tagGroups
  let monthlyLimit = budgetGroups <&> (.limitCents) & sum

  txs <- findMatchingTxs (YearMonth year month)

  let actualSpendingToDateCents = txs <&> (.itemAmountCents) & sum
  let expectedSpendingToDateCents =
        round @Double @FECents $
          fromIntegral @FECents @Double monthlyLimit
            * fromIntegral @DayOfMonth @Double todayDayNum
            / fromIntegral @DayOfMonth @Double totalDays
  let overUnderCents = actualSpendingToDateCents - expectedSpendingToDateCents

  let tagGroupStats = mkBudgetTagGroupStats budgetGroups txs

  pure
    BudgetInfo
      { monthlyLimitCents = monthlyLimit
      , expectedSpendingToDateCents
      , overUnderCents
      , transactions = txs
      , actualSpendingToDateCents
      , tagGroupStats = MapAsList tagGroupStats
      }

findMatchingTxs ::
  (Reader Env :> es, SQLite :> es, Time :> es, Log :> es) =>
  Month ->
  Eff es (Vector TransactionItem)
findMatchingTxs month = do
  config <- asks @Env (.config)

  let budgetTags = concatMap (.tags) config.budget.tagGroups

  let thisMonth = month & Time.formatTime Time.defaultTimeLocale "%m-%Y" & toText

  useConnection \conn -> do
    let mkSearchParams account tag =
          Db.SearchParams
            { allFields = Db.StringParams [] []
            , transactionId = Nothing
            , date = Just $ Db.Contains $ NET.unsafeFromText thisMonth
            , account
            , desc = Db.StringParams [] []
            , amount = Nothing
            , tag
            , notes = Db.StringParams [] []
            , isExpense = Just True
            }
    taggedTxs <- Db.search conn (mkSearchParams Nothing (Just (Db.SomeTags budgetTags)))

    extraTxs <- forM (Set.toList config.budget.includeAllTxsFromAccounts) \accountName ->
      Db.search conn (mkSearchParams (Just accountName) Nothing)

    pure $
      mconcat (taggedTxs : extraTxs)
        <&> (\tx -> GetTransactions.convertRowToItem tx)
        & V.nubBy (comparing (.transactionId) <> comparing (.itemIndex))

mkBudgetTagGroupStats :: [Config.BudgetTagGroup] -> Vector TransactionItem -> Map TagGroupName BudgetTagGroupStats
mkBudgetTagGroupStats groups txs =
  let
    -- How much we've spent for each tag in the budget config
    tagMap = Map.fromListWith (+) do
      tx <- toList txs
      tag <- maybeToList tx.tag
      pure (tag, tx.itemAmountCents)

    -- Collect the stats for each tag group declared in the config.
    groupStats =
      Map.fromList $
        groups <&> \group ->
          let groupSpent = sum $ mapMaybe (\tag -> Map.lookup tag tagMap) group.tags
           in ( group.name
              , BudgetTagGroupStats
                  { spentToDateCents = groupSpent
                  , limitCents = group.limitCents
                  , tags = Just <$> group.tags
                  }
              )

    -- Collect the stats for all transactions that don't have any of the tags in the config.
    -- We'll lump these together in an "Other" bucket.
    otherGroupStats =
      let
        -- All tags explicitly called out in the budget config, across all groups.
        budgetTagSet = Set.fromList $ concatMap (.tags) groups
        isOtherTx tx = maybe True (`Set.notMember` budgetTagSet) tx.tag
        otherSpent = sum [tx.itemAmountCents | tx <- toList txs, isOtherTx tx]
        otherTags = ordNub [tx.tag | tx <- toList txs, isOtherTx tx]
       in
        if otherSpent == 0
          then Map.empty
          else
            Map.singleton
              (TagGroupName "Other")
              BudgetTagGroupStats
                { spentToDateCents = otherSpent
                , limitCents = FECents 0
                , tags = otherTags
                }
   in
    Map.unionWith
      ( \otherGroup1 otherGroup2 ->
          BudgetTagGroupStats
            { spentToDateCents = otherGroup1.spentToDateCents + otherGroup2.spentToDateCents
            , tags = otherGroup1.tags <> otherGroup2.tags
            , limitCents = max otherGroup1.limitCents otherGroup2.limitCents
            }
      )
      groupStats
      otherGroupStats
