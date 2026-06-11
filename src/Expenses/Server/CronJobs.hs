module Expenses.Server.CronJobs where

import Config (AppConfig (..))
import CustomPrelude
import Effectful.SQLite.Simple (Connection)
import Expenses.Effects qualified as Eff
import Expenses.Server.CronJobs.Sync qualified as Sync
import Expenses.Server.Env (Env (..))
import Log
import System.Cron.Schedule (addJob, execSchedule)

startCronJobs :: MVar Connection -> Env -> Logger -> LogT IO ()
startCronJobs conn env logger = do
  let schedule = env.config.cronSchedule
  logInfo_ [i|Scheduling Nordigen sync job: #{schedule}.|]
  void $
    liftIO $ execSchedule do
      addJob (Sync.nordigenJob & Eff.runCronM conn env logger) schedule
