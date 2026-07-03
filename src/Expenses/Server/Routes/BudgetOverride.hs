module Expenses.Server.Routes.BudgetOverride where

import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveFromJSON)
import Database qualified as Db
import Effectful
import Effectful.Time qualified as Time
import Expenses.Effects
import Expenses.Effects.EventLog qualified as EventLog
import Expenses.Server.EventLog
import Expenses.Server.Routes.GetTransactions qualified as GetTransactions
import Expenses.Server.Utils (throwJsonError)
import Servant (err500)
import Servant.Server qualified as S
import Types (Admin (..))

newtype SetBudgetOverride = MkSetBudgetOverride
  { budgetOverride :: Bool
  -- ^ True -> always include the item in the budget; False -> always exclude it.
  }

$(deriveFromJSON defaultOptions ''SetBudgetOverride)

-- | Sets the budget override (include/exclude) for a transaction item.
setBudgetOverrideHandler ::
  (Time :> es, EventLog :> es, SQLite :> es, Error S.ServerError :> es) =>
  Admin -> Text -> Int -> SetBudgetOverride -> Eff es GetTransactions.TransactionItem
setBudgetOverrideHandler admin txId itemIndex (MkSetBudgetOverride newOverride) =
  updateBudgetOverride admin txId itemIndex (Just newOverride)

-- | Clears the budget override, reverting to the default matching rules.
unsetBudgetOverrideHandler ::
  (Time :> es, EventLog :> es, SQLite :> es, Error S.ServerError :> es) =>
  Admin -> Text -> Int -> Eff es GetTransactions.TransactionItem
unsetBudgetOverrideHandler admin txId itemIndex =
  updateBudgetOverride admin txId itemIndex Nothing

updateBudgetOverride ::
  (Time :> es, EventLog :> es, SQLite :> es, Error S.ServerError :> es) =>
  Admin -> Text -> Int -> Maybe Bool -> Eff es GetTransactions.TransactionItem
updateBudgetOverride admin txId itemIndex newOverride = do
  now <- Time.currentTime
  useConnection \conn -> do
    desc <- Db.getDescription conn txId
    oldOverride <- Db.getBudgetOverride conn txId itemIndex
    when (oldOverride /= newOverride) do
      Db.setBudgetOverride conn txId itemIndex newOverride
      EventLog.appendEvent
        Action
          { username = admin
          , ts = now
          , transactionId = txId
          , transactionDesc = desc
          , itemIndex
          , actionType = UpdateBudgetOverride MkUpdateBudgetOverride{old = oldOverride, new = newOverride}
          }

    rowMb <- Db.getTransactionItemById conn txId itemIndex
    case rowMb of
      Nothing -> throwJsonError err500 [i|Failed to find tx item after updating its budget override: #{txId} index #{itemIndex}|]
      Just row -> pure $ GetTransactions.convertRowToItem row
