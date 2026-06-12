module Expenses.Server.CronJobs.BudgetCheck where

import Config qualified
import CustomPrelude
import Effectful
import Effectful.Exception qualified as Eff
import Effectful.Reader.Static (asks)
import Expenses.Effects
import Expenses.Effects.Ntfy qualified as Ntfy
import Expenses.Server.Routes.GetBudget (BudgetInfo (..), getBudgetHandler)
import Log
import Text.Printf (printf)
import Types (FECents)

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
  let remaining = budgetInfo.monthlyLimitCents - budgetInfo.actualSpendingToDateCents
  logInfo_ [i|[Cron] Budget check done: #{formatEuros remaining}€ left to spend this month, sending push notification.|]
  Ntfy.clearNotifications
  Ntfy.sendNotification
    Ntfy.Notification
      { title = "Budget update"
      , message = [i|You have #{formatEuros remaining}€ left to spend this month.|]
      , clickUrl = pushConfig.openUrl
      }

formatEuros :: FECents -> Text
formatEuros cents =
  toText @String $ printf "%.2f" (fromIntegral @FECents @Double cents / 100)
