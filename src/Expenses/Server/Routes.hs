{-# OPTIONS_GHC -Wno-orphans #-}

module Expenses.Server.Routes where

import Config qualified
import Control.Concurrent.MVar qualified as M
import CustomPrelude
import Data.List qualified as List
import Data.Time.Calendar.Month (Month)
import Database.SQLite.Simple qualified as SQL
import Expenses.Server.AppM (AppM, Env (..), runLogger)
import Expenses.Server.CronJob qualified as CronJob
import Expenses.Server.Options (ServerOptions (..))
import Expenses.Server.Options qualified as Opt
import Expenses.Server.Routes.AllAccounts qualified as AllAccounts
import Expenses.Server.Routes.AllTags qualified as AllTags
import Expenses.Server.Routes.GetAvailableDateRange (DateRange)
import Expenses.Server.Routes.GetAvailableDateRange qualified as GetAvailableDateRange
import Expenses.Server.Routes.GetTransactionItems qualified as GetTransactionItems
import Expenses.Server.Routes.GetTransactions qualified as GetTransactions
import Expenses.Server.Routes.InsertNew qualified as InsertNew
import Expenses.Server.Routes.ModifyTransaction qualified as ModifyTransaction
import Expenses.Server.Routes.RunCron qualified as RunCron
import Expenses.Server.Routes.Search qualified as Search
import Expenses.Server.Routes.SplitTransactionItems qualified as SplitTransactionItems
import Expenses.Server.Utils (throwJsonError)
import Log
import Log.Backend.StandardOutput (withStdOutLogger)
import Network.HTTP.Types (hAuthorization, hContentType)
import Network.Wai qualified as Wai
import Network.Wai.Application.Static qualified as Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors qualified as Cors
import Network.Wai.Middleware.RequestLogger qualified as RequestLogger
import Servant.API
import Servant.Server
import Servant.Server.Experimental.Auth qualified as SS
import Servant.Server.Generic (AsServerT)
import Servant.Server.StaticFiles qualified as S
import System.IO qualified as IO
import Types (Admin (..), TagName, Username (..), mkUsername)
import Util qualified

type RequiredParam = QueryParam' '[Required, Strict]

type MyAPI = NamedRoutes API

-- See:
--  * https://docs.servant.dev/en/inserting_doc_namedroutes/cookbook/namedRoutes/NamedRoutes.html
--  * https://docs.servant.dev/en/latest/cookbook/using-custom-monad/UsingCustomMonad.html
--  * https://docs.servant.dev/en/latest/cookbook/hoist-server-with-context/HoistServerWithContext.html
--  * https://docs.servant.dev/en/latest/tutorial/Authentication.html
--  * https://docs.servant.dev/en/latest/cookbook/jwt-and-basic-auth/JWTAndBasicAuth.html
type API :: forall k. k -> Type
data API mode = API
  { private :: mode :- AuthProtect "cloudflare-auth" :> NamedRoutes PrivateAPI
  , admin :: mode :- AuthProtect "admin" :> NamedRoutes AdminAPI
  , health :: mode :- "health" :> Get '[JSON] Text
  , static :: mode :- Raw
  }
  deriving stock (Generic)

type PrivateAPI :: forall k. k -> Type
data PrivateAPI mode = PrivateAPI
  { transactions ::
      mode
        :- "transactions"
          :> RequiredParam "start" Month
          :> RequiredParam "end" Month
          :> Get '[JSON] GetTransactions.GetTransactions
  , getTransactionItems ::
      mode
        :- "transactions"
          :> Capture "transactionId" Text
          :> "items"
          :> Get '[JSON] [GetTransactions.ShortTransactionItem]
  , isAdmin :: mode :- "is-admin" :> Get '[JSON] Bool
  , search ::
      mode
        :- "search"
          :> ReqBody '[JSON] Search.RawSearchParams
          :> Post '[JSON] (Vector GetTransactions.TransactionItem)
  , allTags :: mode :- "tags" :> Get '[JSON] [TagName]
  , allAccounts :: mode :- "accounts" :> Get '[JSON] [Text]
  , getAvailableDateRange :: mode :- "dates" :> Get '[JSON] DateRange
  }
  deriving stock (Generic)

type AdminAPI :: forall k. k -> Type
data AdminAPI mode = AdminAPI
  { modifyTransaction ::
      mode
        :- "transactions"
          :> ReqBody '[JSON] ModifyTransaction.ModifyTransaction
          :> Put '[JSON] GetTransactions.TransactionItem
  , insertTransaction ::
      mode
        :- "transactions"
          :> ReqBody '[JSON] InsertNew.NewTransactionItem
          :> Post '[JSON] GetTransactions.TransactionItem
  , splitTransactionItems ::
      mode
        :- "transactions"
          :> Capture "transactionId" Text
          :> "split"
          :> ReqBody '[JSON] [GetTransactions.NewShortTransactionItem]
          :> PostNoContent
  , runCronSync :: mode :- "sync" :> PostNoContent
  }
  deriving stock (Generic)

main :: IO ()
main = do
  -- Disable buffering, so output appears immediately in systemd's journal (journalctl).
  IO.hSetBuffering stdout IO.NoBuffering

  withStdOutLogger \stdout -> do
    opts@ServerOptions{port, user, resourcesDir, runCron, isVerbose} <-
      runLogger True stdout $ Opt.mkServerOptions

    runLogger isVerbose stdout do
      let ctx = mkAuthContext user

      env <- mkEnv opts stdout

      when runCron do
        CronJob.startCronJobs env stdout

      liftIO $
        run port $
          mkApp ctx isVerbose env stdout resourcesDir
            & requestLogger isVerbose
            & corsMiddleware
 where
  requestLogger :: Bool -> Application -> Application
  requestLogger isVerbose =
    if isVerbose
      then RequestLogger.logStdout
      else id

  corsMiddleware :: Application -> Application
  corsMiddleware = Cors.cors $ \_req ->
    let
      dflt = Cors.simpleCorsResourcePolicy
     in
      Just
        dflt
          { Cors.corsRequestHeaders = dflt.corsRequestHeaders <> [hContentType, hAuthorization]
          , Cors.corsMethods = dflt.corsMethods <> ["PUT"]
          }

  mkEnv :: (MonadIO m, MonadLog m) => ServerOptions -> Logger -> m Env
  mkEnv ServerOptions{dbPath, eventLogPath, logsDir, isVerbose, nordigenSecretId, nordigenSecretKey} logger = do
    Util.checkDbExists dbPath
    dbConn <- liftIO $ SQL.open dbPath
    liftIO $ SQL.setTrace dbConn $ Just \t ->
      runLogger isVerbose logger do
        logTrace_ [i|SQL:\n#{t}|]
    dbConnMutex <- liftIO $ M.newMVar dbConn
    pure Env{dbConn = dbConnMutex, eventLogPath, logsDir, nordigenSecretId, nordigenSecretKey}

  mkAuthContext :: Maybe Username -> Context '[SS.AuthHandler Wai.Request Username, SS.AuthHandler Wai.Request Admin]
  mkAuthContext fallbackUser =
    userAuthHandler fallbackUser
      :. adminAuthHandler fallbackUser
      :. EmptyContext

  userAuthHandler :: Maybe Username -> SS.AuthHandler Wai.Request Username
  userAuthHandler = SS.mkAuthHandler . authHandler'

  adminAuthHandler :: Maybe Username -> SS.AuthHandler Wai.Request Admin
  adminAuthHandler fallbackUser = SS.mkAuthHandler \request -> do
    user <- authHandler' fallbackUser request
    case tryMkAdmin user of
      Just admin -> pure admin
      Nothing -> throwJsonError err403 [i|User is not an admin: #{user}|]

-- This type instance binds together the `AuthHandler Request Username` and `AuthProtect` endpoints.
-- It tells the `HasServer` instance that our `Context` will supply a `Username` (via `AuthHandler`)
-- and that downstream combinators will have access to this `Username` value.
type instance SS.AuthServerData (AuthProtect "cloudflare-auth") = Username

type instance SS.AuthServerData (AuthProtect "admin") = Admin

-- We're using Cloudflare's Access for authentication.
-- When a user successfully authenticates, Cloudflare Access will pass through:
--  * `Cf-Access-Jwt-Assertion`: the JWT token
--  * `Cf-Access-Authenticated-User-Email`: the user's e-mail address.
--
-- Cloudflare docs:
--  * https://developers.cloudflare.com/cloudflare-one/identity/authorization-cookie/
--  * https://developers.cloudflare.com/cloudflare-one/identity/authorization-cookie/validating-json/
--  * https://developers.cloudflare.com/cloudflare-one/identity/authorization-cookie/application-token/
authHandler' :: Maybe Username -> Wai.Request -> Handler Username
authHandler' fallbackUser request = do
  let username =
        request
          & Wai.requestHeaders
          & List.lookup "Cf-Access-Authenticated-User-Email"

  case username of
    Nothing ->
      fallbackUser
        & throw401 "Missing header: `Cf-Access-Authenticated-User-Email`"
    Just username -> do
      username
        & decodeUtf8
        & mkUsername
        & throw401 [i|Invalid username: '#{username}'|]
 where
  throw401 :: Text -> Maybe a -> Handler a
  throw401 errMsg = \case
    Just a -> pure a
    Nothing -> throwJsonError err401 errMsg

api :: Proxy MyAPI
api = Proxy

mkApp :: Context '[SS.AuthHandler Wai.Request Username, SS.AuthHandler Wai.Request Admin] -> Bool -> Env -> Logger -> FilePath -> Application
mkApp ctx isVerbose env logger resourcesDir =
  mkServer logger resourcesDir
    & serveWithContextT api ctx naturalTransformation
 where
  naturalTransformation :: forall a. AppM a -> Handler a
  naturalTransformation app =
    app
      & flip runReaderT env
      & runLogger isVerbose logger

mkServer :: Logger -> FilePath -> API (AsServerT AppM)
-- mkServer :: Logger -> FilePath -> ServerT MyAPI AppM
mkServer logger resourcesDir =
  API
    { private = \username ->
        PrivateAPI
          { transactions = GetTransactions.getTransactionsHandler
          , getTransactionItems = GetTransactionItems.getTransactionItemsHandler
          , search = Search.searchHandler
          , isAdmin = isAdminHandler username
          , allTags = AllTags.allTagsHandler
          , allAccounts = AllAccounts.allAccountsHandler
          , getAvailableDateRange = GetAvailableDateRange.getAvailableDateRangeHandler
          }
    , admin = \admin ->
        AdminAPI
          { modifyTransaction = ModifyTransaction.modifyTransactionHandler admin
          , insertTransaction = InsertNew.insertTransactionHandler admin
          , splitTransactionItems = SplitTransactionItems.splitTransactionItemsHandler admin
          , runCronSync = RunCron.runCronHandler logger admin
          }
    , health = pure "OK"
    , static = getStaticHandler resourcesDir
    }

getStaticHandler :: FilePath -> Tagged AppM Application
getStaticHandler resourcesDir = do
  S.serveDirectoryWith $
    Wai.defaultFileServerSettings resourcesDir

isAdminHandler :: Username -> AppM Bool
isAdminHandler username =
  pure $ isJust $ tryMkAdmin username

tryMkAdmin :: Username -> Maybe Admin
tryMkAdmin user =
  if user `elem` Config.admins
    then Just $ Admin user
    else Nothing
