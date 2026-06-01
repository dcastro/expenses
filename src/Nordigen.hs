module Nordigen where

import CustomPrelude
import Data.Aeson as J
import Network.HTTP.Client (Manager)
import Servant.API
import Servant.Auth as S
import Servant.Auth.Client (Token)
import Servant.Client
import Types

{-
  * Named routes cookbook:
      https://docs.servant.dev/en/inserting_doc_namedroutes/cookbook/namedRoutes/NamedRoutes.html

  * GoCardless / Nordigen API docs:
      https://developer.gocardless.com/api-reference/#core-endpoints-requisitions
      https://developer.gocardless.com/bank-account-data/endpoints/
 -}
type Routes :: forall k. k -> Type
data Routes routes = Routes
  { accounts ::
      routes
        :- S.Auth '[JWT] Text
          :> "accounts"
          :> Capture "accountId" Text
          :> NamedRoutes AccountRoutes
  , requisitions ::
      routes
        :- S.Auth '[JWT] Text
          :> "requisitions"
          :> NamedRoutes RequisitionRoutes
  , login ::
      routes
        :- "token"
          :> "new"
          :> ReqBody '[JSON] NewTokenRequest
          :> Post '[JSON] NewTokenResponse
  }
  deriving stock (Generic)

type AccountRoutes :: forall k. k -> Type
data AccountRoutes routes = AccountRoutes
  { transactions :: routes :- "transactions" :> Get '[JSON] Value
  , balances :: routes :- "balances" :> Get '[JSON] BalancesResponse
  , details :: routes :- "details" :> Get '[JSON] DetailsResponse
  }
  deriving stock (Generic)

type RequisitionRoutes :: forall k. k -> Type
data RequisitionRoutes routes = RequisitionRoutes
  { list :: routes :- Get '[JSON] RequisitionsResponse
  , create :: routes :- ReqBody '[JSON] CreateRequisitionRequest :> PostCreated '[JSON] CreateRequisitionResponse
  , delete :: routes :- Capture "requisitionId" Text :> DeleteNoContent
  }
  deriving stock (Generic)

type API = NamedRoutes Routes

api :: Proxy API
api = Proxy

routes :: Routes (AsClientT ClientM)
routes = client api

getTransactions :: Token -> Text -> ClientM Value
getTransactions token accountId =
  routes
    // (.accounts)
    /: token
    /: accountId
    // (.transactions)

getBalances :: Token -> Text -> ClientM BalancesResponse
getBalances token accountId =
  routes
    // (.accounts)
    /: token
    /: accountId
    // (.balances)

getDetails :: Token -> Text -> ClientM DetailsResponse
getDetails token accountId =
  routes
    // (.accounts)
    /: token
    /: accountId
    // (.details)

getNewToken :: NewTokenRequest -> ClientM NewTokenResponse
getNewToken req =
  routes // (.login) /: req

createRequisition :: Token -> CreateRequisitionRequest -> ClientM CreateRequisitionResponse
createRequisition token req =
  routes
    // (.requisitions)
    /: token
    // (.create)
    /: req

listRequisitions :: Token -> ClientM RequisitionsResponse
listRequisitions token =
  routes
    // (.requisitions)
    /: token
    // (.list)

deleteRequisition :: Token -> Text -> ClientM NoContent
deleteRequisition token requisitionId =
  routes
    // (.requisitions)
    /: token
    // (.delete)
    /: requisitionId

runNordigen :: Manager -> ClientM a -> IO a
runNordigen manager act = do
  let env = mkClientEnv manager (BaseUrl Https "bankaccountdata.gocardless.com" 443 "api/v2")
  runClientM act env >>= \case
    Right a -> pure a
    Left err -> throwM err
