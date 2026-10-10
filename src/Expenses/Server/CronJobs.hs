module Expenses.Server.CronJobs where

import Config (AppConfig (..), PushNotificationsConfig (..))
import CustomPrelude
import Effectful.SQLite.Simple (Connection)
import Expenses.Effects qualified as Eff
import Expenses.Server.CronJobs.BalanceCheck qualified as BalanceCheck
import Expenses.Server.CronJobs.Sync qualified as Sync
import Expenses.Server.Env (Env (..))
import Log
import System.Cron.Schedule (addJob, execSchedule)

startCronJobs :: MVar Connection -> Env -> Logger -> LogT IO ()
startCronJobs conn env logger = do
  let syncSchedule = env.config.cronSchedule
  let balanceCheckSchedule = env.config.pushNotifications.cronSchedule
  logInfo_ [i|Scheduling Nordigen sync job: #{syncSchedule}.|]
  logInfo_ [i|Scheduling balance check job: #{balanceCheckSchedule}.|]
  void $
    liftIO $ execSchedule do
      addJob (Sync.nordigenJob & Eff.runCronM conn env logger) syncSchedule
      addJob (BalanceCheck.balanceCheckJob & Eff.runCronM conn env logger) balanceCheckSchedule
