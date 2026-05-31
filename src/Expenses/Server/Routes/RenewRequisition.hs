module Expenses.Server.Routes.RenewRequisition where

import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveFromJSON, deriveToJSON)
import Effectful
import Effectful.Log
import Expenses.Effects
import Expenses.Effects.NextUUID qualified as NextUUID
import Expenses.Effects.Nordigen qualified as N
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
  (Nordigen :> es, NextUUID :> es, Log :> es) =>
  Admin ->
  Text ->
  RenewRequisitionBody ->
  Eff es RenewRequisitionResponse
renewRequisitionHandler _admin institutionId body = do
  -- Delete any requisitions that may exist for this institution.
  logInfo_ [i|Deleting existing requisitions for institution #{institutionId}...|]
  token <- N.login
  requisitions <- N.listRequisitions token
  let existingForInstitution =
        requisitions.results
          & filter (\req -> institutionId == req.institutionId)
  for_ existingForInstitution \req ->
    void $ N.deleteRequisition token req.id

  -- Create a new requisition for the institution
  logInfo_ [i|Creating new requisition for institution #{institutionId}...|]
  referenceUuid <- NextUUID.nextRandom
  created <-
    N.createRequisition
      token
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
