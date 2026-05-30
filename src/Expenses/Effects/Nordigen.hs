{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Expenses.Effects.Nordigen (
  Nordigen,
  runNordigen,
  login,
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
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS qualified as TLS
import Nordigen qualified as N
import Servant (NoContent)
import Servant.Auth.Client qualified as SA
import Types qualified as T

data Nordigen :: Effect where
  Login :: Nordigen m SA.Token
  GetTransactions ::
    SA.Token ->
    -- | Account ID
    Text ->
    Nordigen m Value
  CreateRequisition ::
    SA.Token ->
    T.CreateRequisitionRequest ->
    Nordigen m T.CreateRequisitionResponse
  ListRequisitions ::
    SA.Token ->
    Nordigen m T.RequisitionsResponse
  DeleteRequisition ::
    SA.Token ->
    Text ->
    Nordigen m NoContent

makeEffect ''Nordigen

runNordigen :: (Reader Env :> es, IOE :> es) => Eff (Nordigen : es) a -> Eff es a
runNordigen action = do
  manager <- liftIO $ Client.newManager TLS.tlsManagerSettings
  env <- ask @Env

  action & interpret \_ -> \case
    Login -> do
      ntr <- liftIO do
        N.runNordigen manager do
          N.getNewToken
            T.NewTokenRequest
              { secretId = env.nordigenSecretId
              , secretKey = env.nordigenSecretKey
              }
      pure $ SA.Token $ encodeUtf8 ntr.access
    GetTransactions token accountId -> do
      liftIO $ N.runNordigen manager do
        N.getTransactions token accountId
    CreateRequisition token req -> do
      liftIO $ N.runNordigen manager do
        N.createRequisition token req
    ListRequisitions token -> do
      liftIO $ N.runNordigen manager do
        N.listRequisitions token
    DeleteRequisition token requisitionId -> do
      liftIO $ N.runNordigen manager do
        N.deleteRequisition token requisitionId
