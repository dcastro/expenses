module Expenses.Server.Routes.RunBudgetCheck where

import CustomPrelude
import Effectful
import Effectful.Reader.Static qualified as R
import Expenses.Effects
import Expenses.Server.CronJobs.BudgetCheck qualified as BudgetCheck
import Servant (NoContent (..))

runBudgetCheckHandler ::
  (Reader Env :> es, SQLite :> es, Time :> es, Log :> es, Ntfy :> es) =>
  Eff es NoContent
runBudgetCheckHandler = do
  R.asks @Env (.demoMode) >>= \case
    True ->
      -- Don't send real push notifications from the public demo server.
      pure NoContent
    False -> do
      BudgetCheck.budgetCheckJob
      pure NoContent
