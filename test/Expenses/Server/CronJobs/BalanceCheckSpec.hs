module Expenses.Server.CronJobs.BalanceCheckSpec where

import Config
import Control.Exception (ErrorCall (..))
import Control.Lens
import CustomPrelude
import Data.Aeson qualified as J
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Log (runLog)
import Effectful.Reader.Static (runReader)
import Effectful.Time qualified as Time
import Expenses.Effects.Nordigen (Nordigen (..))
import Expenses.Effects.Ntfy (Notification (..), Ntfy (..))
import Expenses.Server.CronJobs.BalanceCheck (balanceCheckJob, formatAmount, pickBalance)
import Expenses.Server.Env
import Expenses.Test.Util qualified as Util
import Log (LogLevel (..))
import Servant.Auth.Client qualified as SA
import Test.Syd (Spec, describe, it, shouldBe)
import Types

spec :: Spec
spec = do
  specBalanceCheckJob
  specPickBalance
  specFormatAmount
  specConfig

-- | Records every Ntfy call instead of hitting the real API.
data NtfyCall
  = Cleared
  | Sent Notification
  deriving stock (Eq, Show)

runNtfyMock :: (IOE :> es) => IORef [NtfyCall] -> Eff (Ntfy ': es) a -> Eff es a
runNtfyMock callsRef = interpret \_ -> \case
  ClearNotifications -> modifyIORef' callsRef (<> [Cleared])
  SendNotification notification -> modifyIORef' callsRef (<> [Sent notification])

{- | Returns the balances in the given map (keyed by account ID) instead of hitting the real API.
If an account ID is not in the map, the request fails.
-}
runNordigenMock :: Map Text [Balance] -> Eff (Nordigen ': es) a -> Eff es a
runNordigenMock balancesByAccount = interpret \_ -> \case
  Login -> pure $ SA.Token "token"
  GetBalances _ accountId ->
    case Map.lookup accountId balancesByAccount of
      Just balances -> pure BalancesResponse{balances}
      Nothing -> throwM $ ErrorCall $ toString [i|Unknown account: #{accountId}|]
  GetTransactions{} -> error "GetTransactions: not mocked"
  CreateRequisition{} -> error "CreateRequisition: not mocked"
  ListRequisitions{} -> error "ListRequisitions: not mocked"
  DeleteRequisition{} -> error "DeleteRequisition: not mocked"

mkBalance :: Text -> Text -> Balance
mkBalance balanceType amount =
  Balance
    { balanceAmount = Amount{amount, currency = "EUR"}
    , balanceType
    }

mkAccount :: Text -> Bool -> InstitutionAccountInfo
mkAccount name notifyBalance =
  InstitutionAccountInfo
    { accountName = name
    , accountId = name <> "-id"
    , flipSign = False
    , notifyBalance
    }

testOpenUrl :: Text
testOpenUrl = "http://expenses.example.com/#/budget"

{- | Runs the balance check job at a frozen time,
with the given accounts in the config and the given balances returned by Nordigen,
and returns the Ntfy calls made.
-}
runBalanceCheckJob :: [InstitutionAccountInfo] -> Map Text [Balance] -> IO [NtfyCall]
runBalanceCheckJob accounts balancesByAccount = do
  env <-
    Util.mkTestEnv <&> \env ->
      env
        & config . institutions .~ [InstitutionInfo{institutionId = "BANK", accounts}]
        & config . pushNotifications
          .~ PushNotificationsConfig
            { cronSchedule = ""
            , openUrl = testOpenUrl
            }
  callsRef <- newIORef []

  balanceCheckJob
    & Time.runFrozenTime (UTCTime (fromGregorian 2026 6 15) (secondsToDiffTime 0))
    & runNordigenMock balancesByAccount
    & runNtfyMock callsRef
    & runReader env
    & runLog "test" mempty LogAttention
    & runEff

  readIORef callsRef

specBalanceCheckJob :: Spec
specBalanceCheckJob = describe "balanceCheckJob" do
  it "clears notifications and sends a new one with the balance of each account with `notifyBalance` enabled" do
    calls <-
      runBalanceCheckJob
        [mkAccount "Moey" True, mkAccount "Millennium" False, mkAccount "Activo" True]
        ( Map.fromList
            [ ("Moey-id", [mkBalance "expected" "928.87"])
            , ("Millennium-id", [mkBalance "expected" "1.00"])
            , ("Activo-id", [mkBalance "expected" "-12.3"])
            ]
        )
    calls
      `shouldBe` [ Cleared
                 , Sent
                     Notification
                       { title = "Balance update: Mon, 15 Jun"
                       , message = "Moey: 928.87€\nActivo: -12.30€"
                       , clickUrl = testOpenUrl
                       }
                 ]

  it "still notifies the other balances when fetching one of them fails" do
    calls <-
      runBalanceCheckJob
        [mkAccount "Moey" True, mkAccount "Activo" True, mkAccount "Black" True]
        ( Map.fromList
            [ ("Moey-id", [mkBalance "expected" "928.87"])
            , ("Black-id", [])
            ]
        )
    calls
      `shouldBe` [ Cleared
                 , Sent
                     Notification
                       { title = "Balance update: Mon, 15 Jun"
                       , message = "Moey: 928.87€\nActivo: failed to fetch balance\nBlack: no balance available"
                       , clickUrl = testOpenUrl
                       }
                 ]

  it "does not send a notification when no account has `notifyBalance` enabled" do
    calls <- runBalanceCheckJob [mkAccount "Moey" False] mempty
    calls `shouldBe` []

specPickBalance :: Spec
specPickBalance = describe "pickBalance" do
  it "prefers the `expected` balance" do
    pickBalance [mkBalance "interimAvailable" "1.00", mkBalance "expected" "2.00", mkBalance "closingBooked" "3.00"]
      `shouldBe` Just (mkBalance "expected" "2.00")

  it "falls back to the other known balance types, in order of preference" do
    pickBalance [mkBalance "interimAvailable" "1.00", mkBalance "closingBooked" "3.00"]
      `shouldBe` Just (mkBalance "closingBooked" "3.00")

  it "falls back to the first balance when no balance type is known" do
    pickBalance [mkBalance "forwardAvailable" "1.00", mkBalance "nonInvoiced" "2.00"]
      `shouldBe` Just (mkBalance "forwardAvailable" "1.00")

  it "returns nothing when there are no balances" do
    pickBalance [] `shouldBe` Nothing

specFormatAmount :: Spec
specFormatAmount = describe "formatAmount" do
  it "formats positive and negative amounts" do
    formatAmount Amount{amount = "928.87", currency = "EUR"} `shouldBe` "928.87€"
    formatAmount Amount{amount = "1,283.3", currency = "EUR"} `shouldBe` "1283.30€"
    formatAmount Amount{amount = "-5", currency = "EUR"} `shouldBe` "-5.00€"

specConfig :: Spec
specConfig = describe "config" do
  it "parses the push notifications config" do
    let json = [i|{"cronSchedule": "0 0 * * *", "openUrl": "#{testOpenUrl}"}|] :: Text
    J.eitherDecode (encodeUtf8 json)
      `shouldBe` Right
        PushNotificationsConfig
          { cronSchedule = "0 0 * * *"
          , openUrl = testOpenUrl
          }

  it "parses the `notifyBalance` flag of an account" do
    let json = [i|{"accountName": "Moey", "accountId": "123", "flipSign": false, "notifyBalance": true}|] :: Text
    J.eitherDecode (encodeUtf8 json)
      `shouldBe` Right
        InstitutionAccountInfo
          { accountName = "Moey"
          , accountId = "123"
          , flipSign = False
          , notifyBalance = True
          }
