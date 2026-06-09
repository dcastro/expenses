module Expenses.Server.Routes.GetBudget where

import Config qualified
import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (DayOfMonth, gregorianMonthLength, toGregorian, utctDay)
import Data.Time qualified as Time
import Data.Time.Calendar.Month (pattern YearMonth)
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
  let includeAllTxsFromAccounts = config.budget.includeAllTxsFromAccounts

  let thisMonth = YearMonth year month & Time.formatTime Time.defaultTimeLocale "%m-%Y" & toText
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

  txs <- useConnection \conn -> do
    taggedTxs <- Db.search conn (mkSearchParams Nothing (Just (Db.SomeTags budgetTags)))

    extraTxs <- forM (Set.toList includeAllTxsFromAccounts) \accountName ->
      Db.search conn (mkSearchParams (Just accountName) Nothing)

    pure $
      mconcat (taggedTxs : extraTxs)
        <&> (\tx -> GetTransactions.convertRowToItem tx)
        & V.nubBy (comparing (.transactionId) <> comparing (.itemIndex))

  let actualSpendingToDateCents = txs <&> (.itemAmountCents) & sum
  let expectedSpendingToDateCents =
        round @Double @FECents $
          fromIntegral @FECents @Double monthlyLimit
            * fromIntegral @DayOfMonth @Double todayDayNum
            / fromIntegral @DayOfMonth @Double totalDays
  let overUnderCents = actualSpendingToDateCents - expectedSpendingToDateCents

  let tagGroupStatsResult = mkBudgetTagGroupStats budgetGroups includeAllTxsFromAccounts txs

  pure
    BudgetInfo
      { monthlyLimitCents = monthlyLimit
      , expectedSpendingToDateCents
      , overUnderCents
      , transactions = txs
      , actualSpendingToDateCents
      , tagGroupStats = tagGroupStatsResult
      }

mkBudgetTagGroupStats :: [Config.BudgetTagGroup] -> Set.Set Text -> Vector TransactionItem -> [BudgetTagGroupStats]
mkBudgetTagGroupStats groups includeAllAccounts txs =
  let tagMap = Map.fromListWith (+) do
        tx <- toList txs
        tag <- maybeToList tx.tag
        pure (tag, tx.itemAmountCents)
      otherUntaggedSpent =
        sum
          [ tx.itemAmountCents
          | tx <- toList txs
          , Set.member tx.account includeAllAccounts
          , isNothing tx.tag
          ]
      hasOtherGroup = any (\g -> g.name == TagGroupName "Other") groups
      mkGroupStats group =
        let groupSpent = sum $ mapMaybe (\tag -> Map.lookup tag tagMap) group.tags
            extra = if group.name == TagGroupName "Other" then otherUntaggedSpent else 0
         in BudgetTagGroupStats
              { name = group.name
              , spentToDateCents = groupSpent + extra
              , limitCents = toFE group.limitCents
              , tags = group.tags
              }
      baseStats = mkGroupStats <$> groups
   in if hasOtherGroup || otherUntaggedSpent == 0
        then baseStats
        else
          baseStats
            ++ [ BudgetTagGroupStats
                   { name = TagGroupName "Other"
                   , spentToDateCents = otherUntaggedSpent
                   , limitCents = FECents 0
                   , tags = []
                   }
               ]
