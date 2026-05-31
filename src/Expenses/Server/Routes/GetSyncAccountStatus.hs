module Expenses.Server.Routes.GetSyncAccountStatus where

import Config (AppConfig (..))
import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.Map qualified as Map
import Data.Time (UTCTime)
import Database qualified as Db
import Effectful
import Effectful.Reader.Static qualified as R
import Expenses.Effects
import Expenses.Effects.Nordigen qualified as N
import Types (InstitutionAccountInfo (..), InstitutionInfo (..), Requisition (..), RequisitionsResponse (..), SyncStatus)

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

$( mconcat
     [ deriveToJSON defaultOptions ''AccountSyncStatus
     , deriveToJSON defaultOptions ''InstitutionSyncStatus
     ]
 )

getSyncAccountStatusHandler ::
  (Db :> es, Reader Env :> es, Concurrent :> es, Nordigen :> es) =>
  Eff es [InstitutionSyncStatus]
getSyncAccountStatusHandler = do
  env <- R.ask @Env
  let AppConfig{institutions} = env.config

  token <- N.login
  RequisitionsResponse{results = requisitions} <- N.listRequisitions token
  let requisitionStatusByAccountId = mkRequisitionStatusByAccountId requisitions

  -- Get the sync status for all accounts from the database, and index it by account ID for easy lookup.
  syncRows <-
    useConnection \conn ->
      Db.getSyncAccountStatuses conn

  let syncByAccountId =
        syncRows
          <&> (\row -> (row.accountId, row))
          & Map.fromList

  -- Get all configured institutions/accounts, and pair each account with corresponding DB sync status (if it exists).
  pure $
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
