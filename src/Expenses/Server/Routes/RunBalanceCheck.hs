module Expenses.Server.Routes.RunBalanceCheck where

import CustomPrelude
import Effectful
import Effectful.Reader.Static qualified as R
import Expenses.Effects
import Expenses.Server.CronJobs.BalanceCheck qualified as BalanceCheck
import Servant (NoContent (..))

runBalanceCheckHandler ::
  (Reader Env :> es, Time :> es, Log :> es, Nordigen :> es, Ntfy :> es) =>
  Eff es NoContent
runBalanceCheckHandler = do
  R.asks @Env (.demoMode) >>= \case
    True ->
      -- Don't send real push notifications from the public demo server.
      pure NoContent
    False -> do
      BalanceCheck.balanceCheckJob
      pure NoContent
