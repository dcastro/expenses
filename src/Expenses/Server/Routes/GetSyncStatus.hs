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
  , requisitionStatus :: Maybe Text
  , lastSyncFinishedAt :: Maybe UTCTime
  , lastSyncStatus :: Maybe SyncStatus
  , lastSyncError :: Maybe Text
  , lastSyncedTransactionCount :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

data InstitutionSyncStatus = InstitutionSyncStatus
  { institutionId :: Text
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
    then pure $ demoResponse env.config
    else do
      let AppConfig{institutions} = env.config
      token <- N.login
      RequisitionsResponse{results = requisitions} <- N.listRequisitions token

      -- Get the status of each institution requisition, indexed by account ID.
      let requisitionStatusByAccountId = mkRequisitionStatusByAccountId requisitions

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
                , accountStatuses =
                    accounts <&> \InstitutionAccountInfo{accountId, accountName} ->
                      case Map.lookup accountId syncByAccountId of
                        Nothing ->
                          AccountSyncStatus
                            { accountId = accountId
                            , accountName = accountName
                            , requisitionStatus = Map.lookup accountId requisitionStatusByAccountId
                            , lastSyncFinishedAt = Nothing
                            , lastSyncStatus = Nothing
                            , lastSyncError = Nothing
                            , lastSyncedTransactionCount = Nothing
                            }
                        Just row ->
                          AccountSyncStatus
                            { accountId = accountId
                            , accountName = accountName
                            , requisitionStatus = Map.lookup accountId requisitionStatusByAccountId
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

demoResponse :: AppConfig -> SyncStatusResponse
demoResponse AppConfig{institutions} =
  let
    institutionStatuses =
      institutions <&> \InstitutionInfo{institutionId, accounts} ->
        InstitutionSyncStatus
          { institutionId
          , accountStatuses =
              accounts <&> \InstitutionAccountInfo{accountId, accountName} ->
                AccountSyncStatus
                  { accountId
                  , accountName
                  , requisitionStatus = Just "LINKED"
                  , lastSyncFinishedAt = Nothing
                  , lastSyncStatus = Just SyncSuccess
                  , lastSyncError = Nothing
                  , lastSyncedTransactionCount = Nothing
                  }
          }
   in
    SyncStatusResponse
      { institutions = institutionStatuses
      , missingAccounts = []
      }

mkRequisitionStatusByAccountId :: [Requisition] -> Map.Map Text Text
mkRequisitionStatusByAccountId requisitions =
  requisitions
    & foldl'
      ( \acc Requisition{accounts, status} ->
          case status >>= expandRequisitionStatus of
            Nothing -> acc
            Just longStatus ->
              accounts
                & foldl' (\acc2 accountId -> Map.insertWith (\_ old -> old) accountId longStatus acc2) acc
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
