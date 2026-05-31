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
import Types (InstitutionAccountInfo (..), InstitutionInfo (..), SyncStatus)

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
  , accountStatuses :: [AccountSyncStatus]
  }
  deriving stock (Show, Eq, Generic)

$( mconcat
     [ deriveToJSON defaultOptions ''AccountSyncStatus
     , deriveToJSON defaultOptions ''InstitutionSyncStatus
     ]
 )

getSyncAccountStatusHandler ::
  (Db :> es, Reader Env :> es, Concurrent :> es) =>
  Eff es [InstitutionSyncStatus]
getSyncAccountStatusHandler = do
  env <- R.ask @Env
  let AppConfig{institutions} = env.config

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
