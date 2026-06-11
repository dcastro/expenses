module Expenses.Server.Routes.RunCron where

import CustomPrelude
import Effectful
import Effectful.Concurrent qualified as Conc
import Effectful.Reader.Static qualified as R
import Expenses.Effects
import Expenses.Server.CronJobs.Sync qualified as CronJob
import Servant (NoContent (..))

runCronHandler ::
  (Reader Env :> es, FileSystem :> es, Nordigen :> es, Log :> es, Time :> es, EventLog :> es, Concurrent :> es, SQLite :> es) =>
  Eff es NoContent
runCronHandler = do
  R.asks @Env (.demoMode) >>= \case
    True -> do
      Conc.threadDelay 2_e6
      pure NoContent
    False -> do
      CronJob.nordigenJob
      pure NoContent
