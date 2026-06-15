module Expenses.Server.CronJobs.BudgetCheck where

import Config qualified
import CustomPrelude
import Data.Time (defaultTimeLocale, formatTime, toGregorian, utctDay)
import Data.Time.Calendar.Month (pattern YearMonth)
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
  today <- utctDay <$> Time.currentTime
  let (year, monthOfYear, _) = toGregorian today
  budgetInfo <- getBudgetHandler (YearMonth year monthOfYear)

  let remainingCents = budgetInfo.remainingCents
  let remaining = Util.centsToEuros remainingCents
  logInfo_ [i|[Cron] Budget check done: #{remaining}€ left to spend this month, sending push notification.|]

  -- Clear notifications and send a new one.
  -- NOTE: instead of "clear + send", we could just update an existing nofication, but that wouldn't make the phone ring/vibrate.
  let message =
        if budgetInfo.remainingCents >= 0
          then
            [i|You have #{Util.centsToEuros remainingCents}€ left to spend this month.|]
          else
            [i|You are #{Util.centsToEuros $ abs remainingCents}€ OVER your budget this month.|]
  Ntfy.clearNotifications
  Ntfy.sendNotification
    Ntfy.Notification
      { title = [i|Budget update: #{formatTime defaultTimeLocale "%a, %-d %b" today}|]
      , message = message
      , clickUrl = pushConfig.openUrl
      }
