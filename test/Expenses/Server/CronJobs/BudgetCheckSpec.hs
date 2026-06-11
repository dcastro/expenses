module Expenses.Server.CronJobs.BudgetCheckSpec where

import Config
import Control.Lens
import CustomPrelude
import Data.Aeson qualified as J
import Data.Set qualified as Set
import Data.Time (Day, UTCTime (..), fromGregorian, secondsToDiffTime)
import Database qualified as Db
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Log (runLog)
import Effectful.Reader.Static (runReader)
import Effectful.SQLite.Simple qualified as SQL
import Effectful.Time qualified as Time
import Expenses.Effects.Ntfy (Notification (..), Ntfy (..))
import Expenses.Server.CronJobs.BudgetCheck (budgetCheckJob)
import Expenses.Server.Env
import Expenses.Test.Util qualified as Util
import Log (LogLevel (..), mkBulkLogger)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Types

-- | Records every Ntfy call instead of hitting the real API.
data NtfyCall
  = Cleared
  | Sent Notification
  deriving stock (Eq, Show)

runNtfyMock :: (IOE :> es) => IORef [NtfyCall] -> Eff (Ntfy : es) a -> Eff es a
runNtfyMock callsRef = interpret \_ -> \case
  ClearNotifications -> modifyIORef' callsRef (<> [Cleared])
  SendNotification notification -> modifyIORef' callsRef (<> [Sent notification])

mkRow :: Text -> Day -> Maybe TagName -> FECents -> Db.TransactionJoinedRow
mkRow txId date tag amt =
  Db.TransactionJoinedRow
    { transactionId = txId
    , account = "bank"
    , date
    , desc = "desc"
    , totalAmountCents = toBE amt
    , isExpense = True
    , itemIndex = 0
    , itemAmountCents = toBE amt
    , tag
    , details = ""
    }

testOpenUrl :: Text
testOpenUrl = "http://expenses.example.com/#/budget"

-- | Runs the budget check job at the given (frozen) time,
-- against a db containing the given transactions,
-- and returns the Ntfy calls made.
runBudgetCheckJob :: UTCTime -> [Db.TransactionJoinedRow] -> IO [NtfyCall]
runBudgetCheckJob frozenTime txRows = do
  -- Monthly limit: 600€; push notification threshold: 100€.
  env <-
    Util.mkTestEnv <&> \env ->
      env
        & config . budget
          .~ BudgetConfig
            { tagGroups = [Config.BudgetTagGroup{name = "Groceries", tags = ["groceries"], limitCents = 600_00}]
            , includeAllTxsFromAccounts = Set.empty
            , pushNotifications =
                PushNotificationsConfig
                  { cronSchedule = "30 9 */2 * *"
                  , openUrl = testOpenUrl
                  , thresholdCents = 100_00
                  }
            }
  conn <- Util.mkInMemoryDbConn
  nullLogger <- mkBulkLogger "null" (\_ -> pure ()) (pure ())
  callsRef <- newIORef []

  forM_ txRows \row ->
    SQL.useConnection (\c -> Db.insertTransactionJoinedRow c row)
      & SQL.runSQLiteSync conn
      & runConcurrent
      & runEff

  budgetCheckJob
    & SQL.runSQLiteSync conn
    & Time.runFrozenTime frozenTime
    & runNtfyMock callsRef
    & runReader env
    & runConcurrent
    & runLog "test" nullLogger LogAttention
    & runEff

  readIORef callsRef

spec_budgetCheckJob :: Spec
spec_budgetCheckJob = describe "budgetCheckJob" do
  -- June has 30 days, so on 2026-06-15 we're halfway through the month
  -- and the expected spending is 300€ (half of the 600€ monthly limit).
  let midMonth = UTCTime (fromGregorian 2026 6 15) (secondsToDiffTime 0)

  it "clears notifications and sends a new one when over the expected spending by at least the threshold" do
    -- 600.20€ spent, 300.20€ over the expected spending, above the 100€ threshold.
    calls <- runBudgetCheckJob midMonth [mkRow "tx1" (fromGregorian 2026 6 5) (Just "groceries") 600_20]
    calls
      `shouldBe` [ Cleared
                 , Sent
                    Notification
                      { title = "Over budget"
                      , message = "You're 300.20€ over the expected spending."
                      , clickUrl = testOpenUrl
                      }
                 ]

  it "does not send a notification when over the expected spending by less than the threshold" do
    -- 350€ spent, only 50€ over the expected spending, below the 100€ threshold.
    calls <- runBudgetCheckJob midMonth [mkRow "tx1" (fromGregorian 2026 6 5) (Just "groceries") 350_00]
    calls `shouldBe` []

  it "does not send a notification when under the expected spending" do
    calls <- runBudgetCheckJob midMonth [mkRow "tx1" (fromGregorian 2026 6 5) (Just "groceries") 100_00]
    calls `shouldBe` []

  it "does nothing during the first 7 days of the month" do
    let earlyMonth = UTCTime (fromGregorian 2026 6 7) (secondsToDiffTime 0)
    -- Way over the threshold, but it's still too early in the month to notify.
    calls <- runBudgetCheckJob earlyMonth [mkRow "tx1" (fromGregorian 2026 6 5) (Just "groceries") 600_20]
    calls `shouldBe` []

spec_pushNotificationsConfig :: Spec
spec_pushNotificationsConfig = describe "PushNotificationsConfig" do
  it "parses euros into cents and defaults the cron schedule" do
    let json = [i|{"openUrl": "#{testOpenUrl}", "thresholdEuros": 100.5}|] :: Text
    J.eitherDecode (encodeUtf8 json)
      `shouldBe` Right
        PushNotificationsConfig
          { cronSchedule = "30 9 */2 * *"
          , openUrl = testOpenUrl
          , thresholdCents = 100_50
          }

  it "uses the configured cron schedule when present" do
    let json = [i|{"cronSchedule": "0 0 * * *", "openUrl": "#{testOpenUrl}", "thresholdEuros": 100}|] :: Text
    J.eitherDecode (encodeUtf8 json)
      `shouldBe` Right
        PushNotificationsConfig
          { cronSchedule = "0 0 * * *"
          , openUrl = testOpenUrl
          , thresholdCents = 100_00
          }
