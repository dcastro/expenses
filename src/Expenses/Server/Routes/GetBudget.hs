module Expenses.Server.Routes.GetBudget where

import Config qualified
import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time.Calendar.Month (Month, pattern MonthDay)
import Data.Time.Calendar.Month qualified as Month
import Database qualified as Db
import Effectful
import Effectful.Reader.Static (asks)
import Expenses.Effects
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
  , actualSpendingToDateCents :: FECents
  -- ^ How much we've spent so far this month.
  , remainingCents :: FECents
  -- ^ How much of the monthly budget is left to spend.
  , transactions :: Vector TransactionItem
  , tagGroupStats :: MapAsList TagGroupName BudgetTagGroupStats
  , tagOnlyTransactions :: Vector TransactionItem
  -- ^ Expenses that match a budget tag but not a budget account (candidates to include).
  , accountOnlyTransactions :: Vector TransactionItem
  -- ^ Expenses from a budget account but with no budget tag (candidates to include).
  }
  deriving stock (Show, Eq)

$( mconcat
     [ deriveToJSON defaultOptions ''BudgetTagGroupStats
     , deriveToJSON defaultOptions ''BudgetInfo
     ]
 )

getBudgetHandler ::
  (Reader Env :> es, SQLite :> es) =>
  Month ->
  Eff es BudgetInfo
getBudgetHandler month = do
  config <- asks @Env (.config)
  let budgetGroups = config.budget.tagGroups
  let monthlyLimit = budgetGroups <&> (.limitCents) & sum
  let budgetTags = Set.fromList $ concatMap (.tags) budgetGroups
  let budgetAccounts = config.budget.includeAllTxsFromAccounts

  allTxs <- monthTxs month
  let txs = fromList $ filter (isCountedInBudget budgetAccounts budgetTags) allTxs
  let tagOnlyTransactions = fromList $ filter (isTagOnly budgetAccounts budgetTags) allTxs
  let accountOnlyTransactions = fromList $ filter (isAccountOnly budgetAccounts budgetTags) allTxs

  let actualSpendingToDateCents = txs <&> (.itemAmountCents) & sum
  let remainingCents = monthlyLimit - actualSpendingToDateCents

  let tagGroupStats = mkBudgetTagGroupStats budgetGroups txs

  pure
    BudgetInfo
      { monthlyLimitCents = monthlyLimit
      , remainingCents
      , transactions = txs
      , actualSpendingToDateCents
      , tagGroupStats = MapAsList tagGroupStats
      , tagOnlyTransactions
      , accountOnlyTransactions
      }

-- | All transaction items in the given month, as frontend items.
monthTxs ::
  (SQLite :> es) =>
  Month ->
  Eff es [TransactionItem]
monthTxs month = do
  let dayStart = MonthDay month 1
  let dayEnd = MonthDay (Month.addMonths 1 month) 1
  rows <- useConnection \conn -> Db.getTransactionsByDate conn dayStart dayEnd
  pure $ (\row -> GetTransactions.convertRowToItem row) <$> rows

matchesTag :: Set TagName -> TransactionItem -> Bool
matchesTag budgetTags tx = maybe False (`Set.member` budgetTags) tx.tag

matchesAccount :: Set Text -> TransactionItem -> Bool
matchesAccount budgetAccounts tx = tx.account `Set.member` budgetAccounts

{- | Whether a transaction item counts towards the budget.

Honours the `budgetOverride`: `Just True` always includes, `Just False` always
excludes. When there's no override, the item counts iff it's an expense that
matches both a budget account AND a budget tag.
-}
isCountedInBudget :: Set Text -> Set TagName -> TransactionItem -> Bool
isCountedInBudget budgetAccounts budgetTags tx =
  case tx.budgetOverride of
    Just override -> override
    Nothing -> tx.isExpense && matchesTag budgetTags tx && matchesAccount budgetAccounts tx

{- | An expense that matches a budget tag but not a budget account, with no
override set — a candidate the user might want to include in the budget.
-}
isTagOnly :: Set Text -> Set TagName -> TransactionItem -> Bool
isTagOnly budgetAccounts budgetTags tx =
  isNothing tx.budgetOverride
    && tx.isExpense
    && matchesTag budgetTags tx
    && not (matchesAccount budgetAccounts tx)

{- | An expense from a budget account but with no budget tag, with no override
set — a candidate the user might want to include in the budget.
-}
isAccountOnly :: Set Text -> Set TagName -> TransactionItem -> Bool
isAccountOnly budgetAccounts budgetTags tx =
  isNothing tx.budgetOverride
    && tx.isExpense
    && matchesAccount budgetAccounts tx
    && not (matchesTag budgetTags tx)

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
