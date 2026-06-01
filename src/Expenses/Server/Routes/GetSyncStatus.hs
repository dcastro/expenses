module Expenses.Server.Routes.GetSyncStatus where

import Config (AppConfig (..))
import Config qualified
import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime)
import Database qualified as Db
import Effectful
import Effectful.Reader.Static qualified as R
import Expenses.Effects
import Expenses.Effects.Nordigen qualified as N
import Types (InstitutionAccountInfo (..), InstitutionInfo (..), Requisition (..), RequisitionsResponse (..), SyncStatus (..))

data AccountSyncStatus = AccountSyncStatus
  { accountId :: Text
  , accountName :: Text
  , lastSyncFinishedAt :: Maybe UTCTime
  , lastSyncStatus :: Maybe SyncStatus
  , lastSyncError :: Maybe Text
  , lastSyncedTransactionCount :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

data InstitutionSyncStatus = InstitutionSyncStatus
  { institutionId :: Text
  , requisitionStatus :: Maybe Text
  , accountStatuses :: [AccountSyncStatus]
  }
  deriving stock (Show, Eq, Generic)

{- | The accounts that are missing from the configuration, but have an active requisition in Nordigen.
This likely means that the user has given consent for these accounts, but they haven't been added to the configuration yet,
so they won't be synced.

The frontend reminds the user to add them to the config.
-}
data MissingInstitutionAccounts = MissingInstitutionAccounts
  { institutionId :: Text
  , institutionName :: Text
  , missingAccountIds :: [Text]
  }
  deriving stock (Show, Eq, Generic)

data SyncStatusResponse = SyncStatusResponse
  { institutions :: [InstitutionSyncStatus]
  , missingAccounts :: [MissingInstitutionAccounts]
  }
  deriving stock (Show, Eq, Generic)

$( mconcat
     [ deriveToJSON defaultOptions ''AccountSyncStatus
     , deriveToJSON defaultOptions ''InstitutionSyncStatus
     , deriveToJSON defaultOptions ''MissingInstitutionAccounts
     , deriveToJSON defaultOptions ''SyncStatusResponse
     ]
 )

getSyncStatusHandler ::
  (Db :> es, Reader Env :> es, Concurrent :> es, Nordigen :> es) =>
  Eff es SyncStatusResponse
getSyncStatusHandler = do
  env <- R.ask @Env
  if env.demoMode
    then pure demoResponse
    else do
      let AppConfig{institutions} = env.config
      token <- N.login
      RequisitionsResponse{results = requisitions} <- N.listRequisitions token

      -- Get the status of each institution's requisition, indexed by institution ID.
      let requisitionStatusByInstitutionId = mkRequisitionStatusByInstitutionId requisitions

      -- Get the list of connected accounts that are missing from the config
      let missingAccounts = mkMissingInstitutionAccounts env.config requisitions

      -- Get the sync status for all accounts from the database, and index it by account ID for easy lookup.
      syncRows <-
        useConnection \conn ->
          Db.getSyncAccountStatuses conn

      let syncByAccountId =
            syncRows
              <&> (\row -> (row.accountId, row))
              & Map.fromList

      -- Get all configured institutions/accounts, and pair each account with corresponding DB sync status (if it exists).
      let institutionStatuses =
            institutions <&> \InstitutionInfo{institutionId, accounts} ->
              InstitutionSyncStatus
                { institutionId
                , requisitionStatus = Map.lookup institutionId requisitionStatusByInstitutionId
                , accountStatuses =
                    accounts <&> \InstitutionAccountInfo{accountId, accountName} ->
                      case Map.lookup accountId syncByAccountId of
                        Nothing ->
                          AccountSyncStatus
                            { accountId = accountId
                            , accountName = accountName
                            , lastSyncFinishedAt = Nothing
                            , lastSyncStatus = Nothing
                            , lastSyncError = Nothing
                            , lastSyncedTransactionCount = Nothing
                            }
                        Just row ->
                          AccountSyncStatus
                            { accountId = accountId
                            , accountName = accountName
                            , lastSyncFinishedAt = Just row.lastSyncFinishedAt
                            , lastSyncStatus = Just row.lastSyncStatus
                            , lastSyncError = row.lastSyncError
                            , lastSyncedTransactionCount = Just row.lastSyncedTransactionCount
                            }
                }

      pure
        SyncStatusResponse
          { institutions = institutionStatuses
          , missingAccounts = missingAccounts
          }

demoResponse :: SyncStatusResponse
demoResponse =
  SyncStatusResponse
    { institutions =
        [ InstitutionSyncStatus
            { institutionId = "demo-institution-id-1"
            , requisitionStatus = Just "LINKED"
            , accountStatuses =
                [ AccountSyncStatus
                    { accountId = "demo-account-id-1"
                    , accountName = "Demo Account #1"
                    , lastSyncFinishedAt = Nothing
                    , lastSyncStatus = Just SyncSuccess
                    , lastSyncError = Nothing
                    , lastSyncedTransactionCount = Nothing
                    }
                ]
            }
        , InstitutionSyncStatus
            { institutionId = "demo-institution-id-2"
            , requisitionStatus = Just "LINKED"
            , accountStatuses =
                [ AccountSyncStatus
                    { accountId = "demo-account-id-2"
                    , accountName = "Demo Account #2"
                    , lastSyncFinishedAt = Nothing
                    , lastSyncStatus = Just SyncSuccess
                    , lastSyncError = Nothing
                    , lastSyncedTransactionCount = Nothing
                    }
                , AccountSyncStatus
                    { accountId = "demo-account-id-3"
                    , accountName = "Demo Account #3"
                    , lastSyncFinishedAt = Nothing
                    , lastSyncStatus = Just SyncSuccess
                    , lastSyncError = Nothing
                    , lastSyncedTransactionCount = Nothing
                    }
                ]
            }
        ]
    , missingAccounts = []
    }

mkRequisitionStatusByInstitutionId :: [Requisition] -> Map.Map Text Text
mkRequisitionStatusByInstitutionId requisitions =
  requisitions
    & foldl'
      ( \acc Requisition{institutionId, status} ->
          case expandRequisitionStatus status of
            Nothing -> acc
            Just longStatus -> Map.insert institutionId longStatus acc
      )
      Map.empty
 where
  --  See: https://developer.gocardless.com/bank-account-data/statuses/
  expandRequisitionStatus :: Text -> Maybe Text
  expandRequisitionStatus = \case
    "CR" -> Just "CREATED"
    "GC" -> Just "GIVING_CONSENT"
    "UA" -> Just "UNDERGOING_AUTHENTICATION"
    "RJ" -> Just "REJECTED"
    "SA" -> Just "SELECTING_ACCOUNTS"
    "GA" -> Just "GRANTING_ACCESS"
    "LN" -> Just "LINKED"
    "EX" -> Just "EXPIRED"
    _ -> Nothing

mkMissingInstitutionAccounts :: AppConfig -> [Requisition] -> [MissingInstitutionAccounts]
mkMissingInstitutionAccounts config requisitions =
  let
    configuredAccountIds =
      config
        & Config.allAccountInfos
        <&> (.accountId)
        & Set.fromList

    missingByInstitution =
      requisitions
        & foldl'
          ( \acc Requisition{accounts, institutionId} ->
              let
                missingForReq =
                  accounts
                    & filter (\accountId -> accountId `Set.notMember` configuredAccountIds)
                    & Set.fromList
               in
                if Set.null missingForReq
                  then acc
                  else Map.insertWith Set.union institutionId missingForReq acc
          )
          Map.empty
   in
    Map.toAscList missingByInstitution
      <&> \(institutionId, missingIds) ->
        MissingInstitutionAccounts
          { institutionId
          , institutionName = institutionId
          , missingAccountIds = missingIds & Set.toList & List.sort
          }
