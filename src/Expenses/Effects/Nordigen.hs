{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Expenses.Effects.Nordigen (
  Nordigen,
  runNordigen,
  getTransactions,
  createRequisition,
  listRequisitions,
  deleteRequisition,
) where

import CustomPrelude
import Data.Aeson (Value)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Effectful.TH (makeEffect)
import Expenses.Server.Env (Env (..))
import Network.HTTP.Client (Manager)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS qualified as TLS
import Nordigen qualified as N
import Servant (NoContent)
import Servant.Auth.Client qualified as SA
import Types qualified as T

data Nordigen :: Effect where
  GetTransactions ::
    -- | Account ID
    Text ->
    Nordigen m Value
  CreateRequisition ::
    T.CreateRequisitionRequest ->
    Nordigen m T.CreateRequisitionResponse
  ListRequisitions ::
    Nordigen m T.RequisitionsResponse
  DeleteRequisition ::
    Text ->
    Nordigen m NoContent

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
    CreateRequisition req -> do
      liftIO $ N.runNordigen manager do
        N.createRequisition token req
    ListRequisitions -> do
      liftIO $ N.runNordigen manager do
        N.listRequisitions token
    DeleteRequisition requisitionId -> do
      liftIO $ N.runNordigen manager do
        N.deleteRequisition token requisitionId

login :: Env -> Manager -> IO SA.Token
login env manager = do
  ntr <- liftIO do
    N.runNordigen manager do
      N.getNewToken
        T.NewTokenRequest
          { secretId = env.nordigenSecretId
          , secretKey = env.nordigenSecretKey
          }
  pure $ SA.Token $ encodeUtf8 ntr.access
