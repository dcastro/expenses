module Expenses.Server.Routes.GetBudget where

import Config qualified
import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.Map.Strict qualified as Map
import Data.Time (DayOfMonth, gregorianMonthLength, toGregorian, utctDay)
import Data.Time qualified as Time
import Data.Time.Calendar.Month (pattern YearMonth)
import Database (SearchParams (..))
import Database qualified as Db
import Effectful
import Effectful.Reader.Static (asks)
import Effectful.Time qualified as Time
import Expenses.Effects
import Expenses.NonEmptyText qualified as NET
import Expenses.Server.Routes.GetTransactions (TransactionItem (..))
import Expenses.Server.Routes.GetTransactions qualified as GetTransactions
import Types (FECents (..), TagGroupName (..), TagName, toFE)

data BudgetTagGroupStats = BudgetTagGroupStats
  { name :: TagGroupName
  , spentToDateCents :: FECents
  , limitCents :: FECents
  , tags :: [TagName]
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
  , tagGroupStats :: [BudgetTagGroupStats]
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
  let budgetTags = concatMap (.tags) budgetGroups
  let monthlyLimit = toFE (budgetGroups <&> (.limitCents) & sum)

  let thisMonth = YearMonth year month & Time.formatTime Time.defaultTimeLocale "%m-%Y" & toText
  txItems <- useConnection \conn ->
    Db.search
      conn
      Db.SearchParams
        { allFields = Db.StringParams [] []
        , transactionId = Nothing
        , date = Just $ Db.Contains $ NET.unsafeFromText thisMonth
        , account = Nothing
        , desc = Db.StringParams [] []
        , amount = Nothing
        , tag = Just (Db.SomeTags budgetTags)
        , notes = Db.StringParams [] []
        , isExpense = Just True
        }
      <&> fmap (\tx -> GetTransactions.convertRowToItem tx)

  let actualSpendingToDateCents = txItems <&> (.itemAmountCents) & sum
  let expectedSpendingToDateCents =
        round @Double @FECents $
          fromIntegral @FECents @Double monthlyLimit
            * fromIntegral @DayOfMonth @Double todayDayNum
            / fromIntegral @DayOfMonth @Double totalDays
  let overUnderCents = actualSpendingToDateCents - expectedSpendingToDateCents

  let tagGroupStatsResult = mkBudgetTagGroupStats budgetGroups txItems

  pure
    BudgetInfo
      { monthlyLimitCents = monthlyLimit
      , expectedSpendingToDateCents
      , overUnderCents
      , transactions = txItems
      , actualSpendingToDateCents
      , tagGroupStats = tagGroupStatsResult
      }

mkBudgetTagGroupStats :: [Config.BudgetTagGroup] -> Vector TransactionItem -> [BudgetTagGroupStats]
mkBudgetTagGroupStats groups txs =
  let tagMap = Map.fromListWith (+) do
        tx <- toList txs
        tag <- maybeToList tx.tag
        pure (tag, tx.itemAmountCents)
   in groups <&> \group ->
        let groupSpent = sum $ mapMaybe (\tag -> Map.lookup tag tagMap) group.tags
         in BudgetTagGroupStats
              { name = group.name
              , spentToDateCents = groupSpent
              , limitCents = toFE group.limitCents
              , tags = group.tags
              }
