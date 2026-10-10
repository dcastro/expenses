module Expenses.Server.CronJobs.BalanceCheck where

import Config qualified
import CustomPrelude
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, utctDay)
import Effectful
import Effectful.Exception qualified as Eff
import Effectful.Reader.Static (asks)
import Effectful.Time qualified as Time
import Expenses.Effects
import Expenses.Effects.Nordigen qualified as N
import Expenses.Effects.Ntfy qualified as Ntfy
import Log
import Servant.Auth.Client qualified as SA
import Types
import Util qualified

balanceCheckJob ::
  forall es.
  (Reader Env :> es, Time :> es, Log :> es, Nordigen :> es, Ntfy :> es) =>
  Eff es ()
balanceCheckJob =
  Eff.handleSync logCronFailure do
    balanceCheckJob'
 where
  logCronFailure :: SomeException -> Eff es ()
  logCronFailure err =
    logAttention_ [i|[Cron] Balance check failed: #{displayException err}|]

balanceCheckJob' ::
  (Reader Env :> es, Time :> es, Log :> es, Nordigen :> es, Ntfy :> es) =>
  Eff es ()
balanceCheckJob' = do
  logInfo_ "[Cron] Starting balance check job."
  config <- asks @Env (.config)
  let accounts = Config.allAccountInfos config & filter (.notifyBalance)
  if null accounts
    then logInfo_ "[Cron] No accounts have `notifyBalance` enabled, skipping balance check."
    else do
      today <- utctDay <$> Time.currentTime
      authToken <- N.login
      messageLines <- forM accounts (fetchBalanceLine authToken)
      logInfo_ "[Cron] Balance check done, sending push notification."

      -- Clear notifications and send a new one.
      -- NOTE: instead of "clear + send", we could just update an existing nofication, but that wouldn't make the phone ring/vibrate.
      Ntfy.clearNotifications
      Ntfy.sendNotification
        Ntfy.Notification
          { title = [i|Balance update: #{formatTime defaultTimeLocale "%a, %-d %b" today}|]
          , message = T.intercalate "\n" messageLines
          , clickUrl = config.pushNotifications.openUrl
          }

-- | Fetches the balance of an account, and renders it as a line in the push notification, e.g. "Moey: 928.87€".
fetchBalanceLine ::
  (Log :> es, Nordigen :> es) =>
  SA.Token -> InstitutionAccountInfo -> Eff es Text
fetchBalanceLine authToken acc =
  do
    response <- N.getBalances authToken acc.accountId
    logInfo [i|[Cron] Fetched balances for account: #{name}|] response.balances
    case pickBalance response.balances of
      Just balance -> do
        let amount = formatAmount balance.balanceAmount
        pure [i|#{name}: #{amount}|]
      Nothing -> do
        logAttention_ [i|[Cron] Nordigen did not return a balance for account: #{name}|]
        pure [i|#{name}: no balance available|]
    `Eff.catchSync` \err -> do
      -- Log the error, and move onto the next account
      logAttention_
        [i|
          [Cron] Failed to fetch balance for account: #{name}:
          #{displayException err}|]
      pure [i|#{name}: failed to fetch balance|]
 where
  name = acc.accountName

{- | Banks report different types of balances, and not all banks report all of them.
Pick the first one available, in this order of preference.

https://developer.gocardless.com/bank-account-data/balance
-}
pickBalance :: [Balance] -> Maybe Balance
pickBalance balances =
  asum (preferredBalanceTypes <&> \balanceType -> find (\b -> b.balanceType == balanceType) balances)
    <|> (head <$> nonEmpty balances)
 where
  preferredBalanceTypes :: [Text]
  preferredBalanceTypes =
    [ -- Booked entries plus pending items, i.e. what the bank's app usually shows as the current balance.
      "expected"
    , "interimBooked"
    , "closingBooked"
    , "interimAvailable"
    ]

formatAmount :: Amount -> Text
formatAmount amount = do
  let cents = BECents $ Util.eurosToCents amount.amount
  -- `centsToEuros` uses the frontend convention, where expenses are positive.
  -- A positive balance must show as positive, so we negate it.
  [i|#{Util.centsToEuros (negate $ toFE cents)}€|]
