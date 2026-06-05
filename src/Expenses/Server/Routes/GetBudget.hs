module Expenses.Server.Routes.GetBudget where

import Config qualified
import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.HashMap.Strict qualified as HM
import Data.List ((!!))
import Data.Map.Strict qualified as Map
import Data.Time (Day, addGregorianMonthsClip, fromGregorian, gregorianMonthLength, toGregorian, utctDay)
import Database qualified as Db
import Effectful
import Effectful.Reader.Static (asks)
import Effectful.Time qualified as Time
import Expenses.Effects
import Types (BECents (..), FECents (..))

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
  }
  deriving stock (Show, Eq)

$(
  mconcat
    [ deriveToJSON defaultOptions ''BudgetDayInfo
    , deriveToJSON defaultOptions ''BudgetInfo
    ]
 )

getBudgetHandler ::
  (Reader Env :> es, SQLite :> es, Time :> es) =>
  Eff es BudgetInfo
getBudgetHandler = do
  today <- utctDay <$> Time.currentTime
  let (year, month, todayDayNum) = toGregorian today
  let totalDays = gregorianMonthLength year month
  let firstDay = fromGregorian year month 1
  let nextMonthFirstDay = addGregorianMonthsClip 1 firstDay

  config <- asks @Env (.config)
  let budgetTags = HM.lookup config.budgetTagGroup config.allTagGroups & fromMaybe []

  limitCentsInt <- useConnection \conn -> Db.getBudgetLimit conn

  spendingByDay <- useConnection \conn ->
    Db.getBudgetSpendingByDay conn firstDay nextMonthFirstDay budgetTags

  let spendingMap :: Map.Map Day Int =
        Map.fromList [(d, x) | (d, BECents x) <- spendingByDay]

  let dayInfos = buildDayInfos year month todayDayNum totalDays limitCentsInt spendingMap

  let todayItem = dayInfos !! (todayDayNum - 1)

  pure
    BudgetInfo
      { monthlyLimitCents = FECents limitCentsInt
      , projectedLimitTodayCents = todayItem.projectedLimitCents
      , overUnderTodayCents = fromMaybe (FECents 0) todayItem.overUnderCents
      , days = dayInfos
      }

buildDayInfos :: Integer -> Int -> Int -> Int -> Int -> Map.Map Day Int -> [BudgetDayInfo]
buildDayInfos year month todayDayNum totalDays limitCentsInt spendingMap =
  reverse finalAcc
 where
  (_, finalAcc) = foldl' step (0, []) [1 .. totalDays]
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
