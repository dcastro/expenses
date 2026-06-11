module Expenses.Server.CronJobs.BudgetCheck where

import Config qualified
import CustomPrelude
import Data.Time (toGregorian, utctDay)
import Effectful
import Effectful.Exception qualified as Eff
import Effectful.Reader.Static (asks)
import Effectful.Time qualified as Time
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
  today <- utctDay <$> Time.currentTime
  let (_, _, dayOfMonth) = toGregorian today
  if dayOfMonth <= 7
    then logInfo_ [i|[Cron] Budget check skipped: day of the month is #{dayOfMonth}.|]
    else do
      pushConfig <- asks @Env (.config.budget.pushNotifications)
      budgetInfo <- getBudgetHandler
      let overBy = budgetInfo.overUnderCents
      if overBy >= pushConfig.thresholdCents
        then do
          logInfo_ [i|[Cron] Over expected spending by #{formatEuros overBy}€, sending push notification.|]
          Ntfy.clearNotifications
          Ntfy.sendNotification
            Ntfy.Notification
              { title = "Over budget"
              , message = [i|You're #{formatEuros overBy}€ over the expected spending.|]
              , clickUrl = pushConfig.openUrl
              }
        else
          logInfo_ [i|[Cron] Budget check done: over/under expected spending by #{formatEuros overBy}€, no notification sent.|]

formatEuros :: FECents -> Text
formatEuros cents =
  toText @String $ printf "%.2f" (fromIntegral @FECents @Double cents / 100)
