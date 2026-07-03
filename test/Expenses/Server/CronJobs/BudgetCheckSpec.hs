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
import Log (LogLevel (..))
import Test.Syd (Spec, describe, it, shouldBe)
import Types

spec :: Spec
spec = do
  specBudgetCheckJob
  specPushNotificationsConfig

-- | Records every Ntfy call instead of hitting the real API.
data NtfyCall
  = Cleared
  | Sent Notification
  deriving stock (Eq, Show)

runNtfyMock :: (IOE :> es) => IORef [NtfyCall] -> Eff (Ntfy ': es) a -> Eff es a
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
    , budgetOverride = Nothing
    }

testOpenUrl :: Text
testOpenUrl = "http://expenses.example.com/#/budget"

{- | Runs the budget check job at the given (frozen) time,
against a db containing the given transactions,
and returns the Ntfy calls made.
-}
runBudgetCheckJob :: UTCTime -> [Db.TransactionJoinedRow] -> IO [NtfyCall]
runBudgetCheckJob frozenTime txRows = do
  -- Monthly limit: 600€.
  env <-
    Util.mkTestEnv <&> \env ->
      env
        & config . budget
          .~ BudgetConfig
            { tagGroups = [Config.BudgetTagGroup{name = "Groceries", tags = ["groceries"], limitCents = 600_00}]
            , includeAllTxsFromAccounts = Set.fromList ["bank"]
            , pushNotifications =
                PushNotificationsConfig
                  { cronSchedule = ""
                  , openUrl = testOpenUrl
                  }
            }
  conn <- Util.mkInMemoryDbConn
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
    & runLog "test" mempty LogAttention
    & runEff

  readIORef callsRef

specBudgetCheckJob :: Spec
specBudgetCheckJob = describe "budgetCheckJob" do
  let midMonth = UTCTime (fromGregorian 2026 6 15) (secondsToDiffTime 0)

  it "clears notifications and sends a new one with the remaining budget" do
    -- 100.20€ spent out of the 600€ monthly limit.
    calls <- runBudgetCheckJob midMonth [mkRow "tx1" (fromGregorian 2026 6 5) (Just "groceries") 100_20]
    calls
      `shouldBe` [ Cleared
                 , Sent
                     Notification
                       { title = "Budget update: Mon, 15 Jun"
                       , message = "You have 499.80€ left to spend this month."
                       , clickUrl = testOpenUrl
                       }
                 ]

  it "reports a negative amount when the monthly limit has been exceeded" do
    -- 600.20€ spent out of the 600€ monthly limit.
    calls <- runBudgetCheckJob midMonth [mkRow "tx1" (fromGregorian 2026 6 5) (Just "groceries") 600_20]
    calls
      `shouldBe` [ Cleared
                 , Sent
                     Notification
                       { title = "Budget update: Mon, 15 Jun"
                       , message = "You have -0.20€ left to spend this month."
                       , clickUrl = testOpenUrl
                       }
                 ]

specPushNotificationsConfig :: Spec
specPushNotificationsConfig = describe "PushNotificationsConfig" do
  it "parses the cron schedule and open url" do
    let json = [i|{"cronSchedule": "0 0 * * *", "openUrl": "#{testOpenUrl}"}|] :: Text
    J.eitherDecode (encodeUtf8 json)
      `shouldBe` Right
        PushNotificationsConfig
          { cronSchedule = "0 0 * * *"
          , openUrl = testOpenUrl
          }
