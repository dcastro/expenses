module Expenses.Server.CronJobs.BudgetCheckSpec where

import Config
import Control.Lens
import CustomPrelude
import Data.Aeson qualified as J
import Data.Set qualified as Set
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Log (runLog)
import Effectful.Reader.Static (runReader)
import Effectful.Time qualified as Time
import Expenses.Effects.Nordigen (Nordigen (..))
import Expenses.Effects.Ntfy (Notification (..), Ntfy (..))
import Expenses.Server.CronJobs.BudgetCheck (budgetCheckJob, pickSpendableBalance)
import Expenses.Server.Env
import Expenses.Test.Util qualified as Util
import Log (LogLevel (..))
import Servant.Auth.Client qualified as SA
import Test.Syd (Spec, describe, it, shouldBe)
import Types

spec :: Spec
spec = do
  specBudgetCheckJob
  specPickSpendableBalance
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

-- | Answers `getBalances` from the given account-id -> balance map, instead of hitting the real API.
runNordigenMock :: (Text -> BalancesResponse) -> Eff (Nordigen ': es) a -> Eff es a
runNordigenMock balancesFor = interpret \_ -> \case
  Login -> pure $ SA.Token "test-token"
  GetBalances _ accountId -> pure $ balancesFor accountId
  _ -> error "runNordigenMock: unexpected Nordigen call"

mkAccount :: Text -> Text -> InstitutionAccountInfo
mkAccount accountName accountId =
  InstitutionAccountInfo{accountName, accountId, flipSign = False}

mkBalance :: Text -> Text -> Balance
mkBalance amount balanceType =
  Balance{balanceAmount = Amount{amount, currency = "EUR"}, balanceType}

testOpenUrl :: Text
testOpenUrl = "http://expenses.example.com/#/budget"

{- | Runs the budget check job at the given (frozen) time, with the given budget
accounts, resolving each account's balance via the given map. Returns the Ntfy calls made.
-}
runBudgetCheckJob :: UTCTime -> [InstitutionAccountInfo] -> (Text -> BalancesResponse) -> IO [NtfyCall]
runBudgetCheckJob frozenTime budgetAccounts balancesFor = do
  env <-
    Util.mkTestEnv <&> \env ->
      env
        & config . institutions
          .~ [InstitutionInfo{institutionId = "inst", accounts = budgetAccounts}]
        & config . budget
          .~ BudgetConfig
            { tagGroups = []
            , includeAllTxsFromAccounts = Set.fromList $ (.accountName) <$> budgetAccounts
            , pushNotifications =
                PushNotificationsConfig
                  { cronSchedule = ""
                  , openUrl = testOpenUrl
                  }
            }
  callsRef <- newIORef []

  budgetCheckJob
    & runNordigenMock balancesFor
    & Time.runFrozenTime frozenTime
    & runNtfyMock callsRef
    & runReader env
    & runLog "test" mempty LogAttention
    & runEff

  readIORef callsRef

specBudgetCheckJob :: Spec
specBudgetCheckJob = describe "budgetCheckJob" do
  let midMonth = UTCTime (fromGregorian 2026 6 15) (secondsToDiffTime 0)

  it "reports the budget account's balance as the amount left to spend" do
    let balances _ = BalancesResponse [mkBalance "499.80" "interimAvailable"]
    calls <- runBudgetCheckJob midMonth [mkAccount "Moey" "acc-1"] balances
    calls
      `shouldBe` [ Cleared
                 , Sent
                     Notification
                       { title = "Budget update: Mon, 15 Jun"
                       , message = "You have 499.80€ left to spend this month."
                       , clickUrl = testOpenUrl
                       }
                 ]

  it "sums the balances across all budget accounts" do
    let balances = \case
          "acc-1" -> BalancesResponse [mkBalance "100.00" "interimAvailable"]
          "acc-2" -> BalancesResponse [mkBalance "50.20" "interimAvailable"]
          _ -> BalancesResponse []
    calls <- runBudgetCheckJob midMonth [mkAccount "Moey" "acc-1", mkAccount "Revolut" "acc-2"] balances
    calls
      `shouldBe` [ Cleared
                 , Sent
                     Notification
                       { title = "Budget update: Mon, 15 Jun"
                       , message = "You have 150.20€ left to spend this month."
                       , clickUrl = testOpenUrl
                       }
                 ]

specPickSpendableBalance :: Spec
specPickSpendableBalance = describe "pickSpendableBalance" do
  it "prefers the interimAvailable balance" do
    let resp =
          BalancesResponse
            [ mkBalance "10.00" "closingBooked"
            , mkBalance "20.00" "interimAvailable"
            ]
    ((.amount) <$> pickSpendableBalance resp) `shouldBe` Just "20.00"

  it "falls back to whatever balance is present" do
    let resp = BalancesResponse [mkBalance "33.00" "somethingElse"]
    ((.amount) <$> pickSpendableBalance resp) `shouldBe` Just "33.00"

  it "returns Nothing when there are no balances" do
    ((.amount) <$> pickSpendableBalance (BalancesResponse [])) `shouldBe` Nothing

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
