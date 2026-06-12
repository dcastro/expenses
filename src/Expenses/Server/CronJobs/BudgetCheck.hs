module Expenses.Server.CronJobs.BudgetCheck where

import Config qualified
import CustomPrelude
import Data.Time (defaultTimeLocale, formatTime, utctDay)
import Effectful
import Effectful.Exception qualified as Eff
import Effectful.Reader.Static (asks)
import Effectful.Time qualified as Time
import Expenses.Effects
import Expenses.Effects.Ntfy qualified as Ntfy
import Expenses.Server.Routes.GetBudget (BudgetInfo (..), getBudgetHandler)
import Log
import Util qualified

budgetCheckJob ::
  forall es.
  (Reader Env :> es, SQLite :> es, Time :> es, Log :> es, Ntfy :> es) =>
  Eff es ()
budgetCheckJob =
  Eff.handleSync logCronFailure do
    budgetCheckJob'
 where
  logCronFailure :: SomeException -> Eff es ()
  logCronFailure err =
    logAttention_ [i|[Cron] Budget check failed: #{displayException err}|]

budgetCheckJob' ::
  (Reader Env :> es, SQLite :> es, Time :> es, Log :> es, Ntfy :> es) =>
  Eff es ()
budgetCheckJob' = do
  logInfo_ "[Cron] Starting budget check job."
  pushConfig <- asks @Env (.config.budget.pushNotifications)
  budgetInfo <- getBudgetHandler
  today <- utctDay <$> Time.currentTime
  let remaining = Util.centsToEuros budgetInfo.remainingCents
  logInfo_ [i|[Cron] Budget check done: #{remaining}€ left to spend this month, sending push notification.|]

  -- Clear notifications and send a new one.
  -- NOTE: instead of "clear + send", we could just update an existing nofication, but that wouldn't make the phone ring/vibrate.
  Ntfy.clearNotifications
  Ntfy.sendNotification
    Ntfy.Notification
      { title = [i|Budget update: #{formatTime defaultTimeLocale "%a, %-d %b" today}|]
      , message = [i|You have #{remaining}€ left to spend this month.|]
      , clickUrl = pushConfig.openUrl
      }
