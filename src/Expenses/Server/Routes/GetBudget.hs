module Expenses.Server.Routes.GetBudget where

import Config qualified
import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.HashMap.Strict qualified as HM
import Data.List ((!!))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Time (Day, addGregorianMonthsClip, fromGregorian, gregorianMonthLength, toGregorian, utctDay)
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

data BudgetDayInfo = BudgetDayInfo
  { date :: Day
  , projectedLimitCents :: FECents
  , actualSpentCents :: Maybe FECents
  , overUnderCents :: Maybe FECents
  }
  deriving stock (Show, Eq)

data BudgetInfo = BudgetInfo
  { monthlyLimitCents :: FECents
  , projectedLimitTodayCents :: FECents
  , overUnderTodayCents :: FECents
  , days :: [BudgetDayInfo]
  , transactions :: [TransactionItem]
  , totalSpentCents :: FECents
  , tagStats :: [BudgetTagStats]
  }
  deriving stock (Show, Eq)

$( mconcat
     [ deriveToJSON defaultOptions ''BudgetTagStats
     , deriveToJSON defaultOptions ''BudgetDayInfo
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
  let budgetTags = HM.lookup config.budgetTagGroup config.allTagGroups & fromMaybe []

  limitBE <- useConnection \conn -> Db.getBudgetLimit conn
  let limitCentsInt = negate limitBE.getCents

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

  let spendingMap :: Map.Map Day Int =
        thisMonthRows
          & foldl' (\m r -> Map.insertWith (+) r.date r.itemAmountCents.getCents m) Map.empty

  let txItems = thisMonthRows <&> (\row -> convertRowToItem row)
  let totalSpentCents = sum (map (.itemAmountCents) txItems)
  let tagStatsResult = mkBudgetTagStats totalSpentCents txItems

  let dayInfos = buildDayInfos year month todayDayNum totalDays limitCentsInt spendingMap

  let todayItem = dayInfos !! (todayDayNum - 1)

  pure
    BudgetInfo
      { monthlyLimitCents = FECents limitCentsInt
      , projectedLimitTodayCents = todayItem.projectedLimitCents
      , overUnderTodayCents = fromMaybe (FECents 0) todayItem.overUnderCents
      , days = dayInfos
      , transactions = txItems
      , totalSpentCents = totalSpentCents
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

buildDayInfos :: Integer -> Int -> Int -> Int -> Int -> Map.Map Day Int -> [BudgetDayInfo]
buildDayInfos year month todayDayNum totalDays limitCentsInt spendingMap =
  reverse finalAcc
 where
  (_, finalAcc) = foldl' step (0, []) [1 .. totalDays]

  step :: (Int, [BudgetDayInfo]) -> Int -> (Int, [BudgetDayInfo])
  step (cumInt, acc) dayNum =
    let
      d = fromGregorian year month dayNum
      projected = FECents $ round ((fromIntegral limitCentsInt :: Double) * fromIntegral dayNum / fromIntegral totalDays)
      (newCumInt, actualSpent, overUnderFE) =
        if dayNum <= todayDayNum
          then
            let
              daySpent = Map.findWithDefault 0 d spendingMap
              newCum = cumInt + daySpent
              actual = FECents (negate newCum)
             in
              (newCum, Just actual, Just (FECents (negate newCum - projected.getCents)))
          else (cumInt, Nothing, Nothing)
     in
      ( newCumInt
      , BudgetDayInfo
          { date = d
          , projectedLimitCents = projected
          , actualSpentCents = actualSpent
          , overUnderCents = overUnderFE
          }
          : acc
      )
