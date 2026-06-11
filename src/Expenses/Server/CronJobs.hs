module Expenses.Server.CronJobs where

import Config (AppConfig (..), BudgetConfig (..), PushNotificationsConfig (..))
import CustomPrelude
import Effectful.SQLite.Simple (Connection)
import Expenses.Effects qualified as Eff
import Expenses.Server.CronJobs.BudgetCheck qualified as BudgetCheck
import Expenses.Server.CronJobs.Sync qualified as Sync
import Expenses.Server.Env (Env (..))
import Log
import System.Cron.Schedule (addJob, execSchedule)

startCronJobs :: MVar Connection -> Env -> Logger -> LogT IO ()
startCronJobs conn env logger = do
  let syncSchedule = env.config.cronSchedule
  let budgetCheckSchedule = env.config.budget.pushNotifications.cronSchedule
  logInfo_ [i|Scheduling Nordigen sync job: #{syncSchedule}.|]
  logInfo_ [i|Scheduling budget check job: #{budgetCheckSchedule}.|]
  void $
    liftIO $ execSchedule do
      addJob (Sync.nordigenJob & Eff.runCronM conn env logger) syncSchedule
      addJob (BudgetCheck.budgetCheckJob & Eff.runCronM conn env logger) budgetCheckSchedule
