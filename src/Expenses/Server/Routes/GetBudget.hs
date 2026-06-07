module Expenses.Server.Routes.GetBudget where

import Config qualified
import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Time (addGregorianMonthsClip, fromGregorian, gregorianMonthLength, toGregorian, utctDay)
import Database (SearchParams (..))
import Database qualified as Db
import Effectful
import Effectful.Reader.Static (asks)
import Effectful.Time qualified as Time
import Expenses.Effects
import Expenses.Server.Routes.GetTransactions (TransactionItem (..), convertRowToItem)
import Types (BECents (..), FECents (..), TagName)

data BudgetTagStats = BudgetTagStats
  { name :: TagName
  , tagTotalAmountCents :: FECents
  , tagPercentage :: Int
  }
  deriving stock (Show, Eq)

data BudgetInfo = BudgetInfo
  { monthlyLimitCents :: FECents
  , projectedLimitTodayCents :: FECents
  , overUnderTodayCents :: FECents
  , transactions :: [TransactionItem]
  , totalSpentCents :: FECents
  , tagStats :: [BudgetTagStats]
  }
  deriving stock (Show, Eq)

$( mconcat
     [ deriveToJSON defaultOptions ''BudgetTagStats
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
  let firstDay = fromGregorian year month 1
  let nextMonthFirstDay = addGregorianMonthsClip 1 firstDay

  config <- asks @Env (.config)
  let budgetGroups = config.budget.tagGroups
  let budgetTags = concatMap (.tags) budgetGroups
  let totalLimitBE = BECents $ sum $ map (.limitCents.getCents) budgetGroups
  let limitCentsInt = negate totalLimitBE.getCents

  rows <- useConnection \conn ->
    Db.search
      conn
      Db.SearchParams
        { allFields = Db.StringParams [] []
        , transactionId = Nothing
        , date = Nothing
        , account = Nothing
        , desc = Db.StringParams [] []
        , amount = Nothing
        , tag = Just (Db.SomeTags budgetTags)
        , notes = Db.StringParams [] []
        , isExpense = Just True
        }

  let thisMonthRows =
        rows
          & toList
          & filter (\r -> r.date >= firstDay && r.date < nextMonthFirstDay)

  let txItems = thisMonthRows <&> (\row -> convertRowToItem row)
  let totalSpentCents = sum (map (.itemAmountCents) txItems)
  let tagStatsResult = mkBudgetTagStats totalSpentCents txItems

  let projectedInt = round ((fromIntegral limitCentsInt :: Double) * fromIntegral todayDayNum / fromIntegral totalDays)
  let projectedLimitTodayCents = FECents projectedInt
  let overUnderTodayCents = FECents (totalSpentCents.getCents - projectedInt)

  pure
    BudgetInfo
      { monthlyLimitCents = FECents limitCentsInt
      , projectedLimitTodayCents
      , overUnderTodayCents
      , transactions = txItems
      , totalSpentCents
      , tagStats = tagStatsResult
      }

mkBudgetTagStats :: FECents -> [TransactionItem] -> [BudgetTagStats]
mkBudgetTagStats total txs =
  let tagMap = Map.fromListWith (+) do
        tx <- txs
        tag <- maybeToList tx.tag
        pure (tag, tx.itemAmountCents)
   in tagMap
        & Map.toList
        & map
          ( \(tag, tagTotal) ->
              BudgetTagStats
                { name = tag
                , tagTotalAmountCents = tagTotal
                , tagPercentage = if total == 0 then 0 else (tagTotal.getCents * 100) `div` total.getCents
                }
          )
        & List.sortOn (Down . (.tagTotalAmountCents))
