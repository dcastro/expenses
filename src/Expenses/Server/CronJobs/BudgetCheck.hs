module Expenses.Server.CronJobs.BudgetCheck where

import Config qualified
import Control.Lens ((^.))
import CustomPrelude
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Time (defaultTimeLocale, formatTime, utctDay)
import Effectful
import Effectful.Exception qualified as Eff
import Effectful.Reader.Static (asks)
import Effectful.Time qualified as Time
import Expenses.Effects
import Expenses.Effects.Nordigen qualified as N
import Expenses.Effects.Ntfy qualified as Ntfy
import Log
import Types
import Util qualified

budgetCheckJob ::
  forall es.
  (Reader Env :> es, Nordigen :> es, Time :> es, Log :> es, Ntfy :> es) =>
  Eff es ()
budgetCheckJob =
  Eff.handleSync logCronFailure do
    budgetCheckJob'
 where
  logCronFailure :: SomeException -> Eff es ()
  logCronFailure err =
    logAttention_ [i|[Cron] Budget check failed: #{displayException err}|]

budgetCheckJob' ::
  (Reader Env :> es, Nordigen :> es, Time :> es, Log :> es, Ntfy :> es) =>
  Eff es ()
budgetCheckJob' = do
  logInfo_ "[Cron] Starting budget check job."
  config <- asks @Env (.config)
  let pushConfig = config.budget.pushNotifications
  today <- utctDay <$> Time.currentTime

  -- Fetch the current balance of each budget account and sum them up: that's how
  -- much money is actually left to spend this month.
  token <- N.login
  let budgetAccountNames = config.budget.includeAllTxsFromAccounts
  let budgetAccounts =
        Config.allAccountInfos config
          & filter \acc -> (acc ^. accountName) `Set.member` budgetAccountNames

  balances <- forM budgetAccounts \acc -> do
    resp <- N.getBalances token (acc ^. accountId)
    case pickSpendableBalance resp of
      Just amount -> pure $ FECents $ Util.eurosToCents amount.amount
      Nothing -> do
        logAttention_ [i|[Cron] No usable balance found for account #{acc ^. accountName}.|]
        pure $ FECents 0

  let remainingCents = sum balances
  logInfo_ [i|[Cron] Budget accounts hold #{Util.centsToEuros remainingCents}€, sending push notification.|]

  -- Clear notifications and send a new one.
  -- NOTE: instead of "clear + send", we could just update an existing nofication, but that wouldn't make the phone ring/vibrate.
  Ntfy.clearNotifications
  Ntfy.sendNotification
    Ntfy.Notification
      { title = [i|Budget update: #{formatTime defaultTimeLocale "%a, %-d %b" today}|]
      , message = [i|You have #{Util.centsToEuros remainingCents}€ left to spend this month.|]
      , clickUrl = pushConfig.openUrl
      }

{- | Picks the balance that best represents how much is available to spend.

Banks report several balance types; we prefer the ones that reflect currently
available funds, falling back to whatever balance is present.
-}
pickSpendableBalance :: BalancesResponse -> Maybe Amount
pickSpendableBalance resp =
  (.balanceAmount) <$> (preferred <|> safeHead bals)
 where
  bals = resp.balances
  byType t = List.find (\b -> b.balanceType == t) bals
  preferred =
    byType "interimAvailable"
      <|> byType "expected"
      <|> byType "interimBooked"
      <|> byType "closingBooked"
