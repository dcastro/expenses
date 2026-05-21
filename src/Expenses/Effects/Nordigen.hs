{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Expenses.Effects.Nordigen (
  Nordigen,
  runNordigen,
  getTransactions,
) where

import CustomPrelude
import Data.Aeson (Value)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Effectful.TH (makeEffect)
import Expenses.Server.AppM (Env (..))
import Network.HTTP.Client (Manager)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS qualified as TLS
import Nordigen qualified as N
import Servant.Auth.Client qualified as SA
import Types qualified as N

data Nordigen :: Effect where
  GetTransactions ::
    -- | Account ID
    Text ->
    Nordigen m Value

makeEffect ''Nordigen

{- | Warning: This interpreter will login once and then run the entire action with the same token.

This interpreter is not meant to be used for long-running tasks,
as we'd run the risk of the token expiring in the middle of the action.
-}
runNordigen :: (Reader Env :> es, IOE :> es) => Eff (Nordigen : es) a -> Eff es a
runNordigen action = do
  manager <- liftIO $ Client.newManager TLS.tlsManagerSettings
  env <- ask @Env
  token <- liftIO $ login env manager

  action & interpret \_ -> \case
    GetTransactions accountId -> do
      liftIO $ N.runNordigen manager do
        N.getTransactions token accountId

login :: Env -> Manager -> IO SA.Token
login env manager = do
  ntr <- liftIO do
    N.runNordigen manager do
      N.getNewToken
        N.NewTokenRequest
          { secretId = env.nordigenSecretId
          , secretKey = env.nordigenSecretKey
          }
  pure $ SA.Token $ encodeUtf8 ntr.access
