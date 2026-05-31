module Expenses.Server.Routes.CheckMissingAccounts where

import Config qualified
import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Effectful
import Effectful.Reader.Static qualified as R
import Expenses.Effects
import Expenses.Effects.Nordigen qualified as N
import Types (Requisition (..), RequisitionsResponse (..))

data MissingInstitutionAccounts = MissingInstitutionAccounts
  { institutionId :: Text
  , institutionName :: Text
  , missingAccountIds :: [Text]
  }
  deriving stock (Show, Eq, Generic)

$( mconcat
     [ deriveToJSON defaultOptions ''MissingInstitutionAccounts
     ]
 )

checkMissingAccountsHandler ::
  (Reader Env :> es, Nordigen :> es) =>
  Eff es [MissingInstitutionAccounts]
checkMissingAccountsHandler = do
  env <- R.ask @Env
  token <- N.login
  RequisitionsResponse{results = requisitions} <- N.listRequisitions token

  let configuredAccountIds = Config.configuredAccountIds env.config

  let missingByInstitution =
        requisitions
          & foldl'
            ( \acc Requisition{accounts, institutionId} ->
                let missingForReq =
                      accounts
                        & filter (\accountId -> accountId `Set.notMember` configuredAccountIds)
                        & Set.fromList
                 in if Set.null missingForReq
                      then acc
                      else Map.insertWith Set.union institutionId missingForReq acc
            )
            Map.empty

  pure $
    Map.toAscList missingByInstitution
      <&> \(institutionId, missingIds) ->
        MissingInstitutionAccounts
          { institutionId = institutionId
          , -- No display name is configured, so use the id as a stable fallback label.
            institutionName = institutionId
          , missingAccountIds = missingIds & Set.toList & List.sort
          }
