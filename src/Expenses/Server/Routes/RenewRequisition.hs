module Expenses.Server.Routes.RenewRequisition where

import Config (AppConfig (..))
import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveFromJSON, deriveToJSON)
import Data.List qualified as List
import Effectful
import Effectful.Log
import Effectful.Reader.Static qualified as R
import Expenses.Effects
import Expenses.Effects.NextUUID qualified as NextUUID
import Expenses.Effects.Nordigen qualified as N
import Expenses.Server.Utils (throwJsonError)
import Servant (err404)
import Types

data RenewRequisitionBody = RenewRequisitionBody
  { redirect :: Text
  }
  deriving stock (Show, Eq, Generic)

data RenewRequisitionResponse = RenewRequisitionResponse
  { id :: Text
  , link :: Text
  }
  deriving stock (Show, Eq, Generic)

$( mconcat
     [ deriveFromJSON defaultOptions ''RenewRequisitionBody
     , deriveToJSON defaultOptions ''RenewRequisitionResponse
     ]
 )

renewRequisitionHandler ::
  (Reader Env :> es, Nordigen :> es, Error ServerError :> es, NextUUID :> es, Log :> es) =>
  Admin ->
  Text ->
  RenewRequisitionBody ->
  Eff es RenewRequisitionResponse
renewRequisitionHandler _admin accountId body = do
  env <- R.ask @Env
  let AppConfig{accountInfos = accounts} = env.config

  -- Find the institution ID for the account from the config
  institutionId <-
    accounts
      & List.find (\acc -> acc.accountId == accountId)
      & maybe
        (throwJsonError err404 [i|Account not found in config: #{accountId}|])
        pure
      <&> (.institutionId)

  -- Delete any requisitions that may exist for this account.
  logInfo_ [i|Deleting existing requisitions for account #{accountId}...|]
  requisitions <- N.listRequisitions
  let existingForAccount =
        requisitions.results
          & filter (\req -> accountId `elem` req.accounts)
  for_ existingForAccount \req ->
    void $ N.deleteRequisition req.id

  -- Create a new requisition for the institution
  logInfo_ [i|Creating new requisition for account #{accountId} and institution #{institutionId}...|]
  referenceUuid <- NextUUID.nextRandom
  created <-
    N.createRequisition
      CreateRequisitionRequest
        { redirect = body.redirect
        , institutionId
        , reference = NextUUID.toText referenceUuid
        , userLanguage = "EN"
        }

  pure
    RenewRequisitionResponse
      { id = created.id
      , link = created.link
      }
