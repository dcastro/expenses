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
import Types (AccountInfo (..), SyncStatus)

data AccountSyncStatus = AccountSyncStatus
  { accountId :: Text
  , accountName :: Text
  , institutionId :: Text
  , lastSyncFinishedAt :: Maybe UTCTime
  , lastSyncStatus :: Maybe SyncStatus
  , lastSyncError :: Maybe Text
  , lastSyncedTransactionCount :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

$( mconcat
     [ deriveToJSON defaultOptions ''AccountSyncStatus
     ]
 )

getSyncAccountStatusHandler ::
  (Db :> es, Reader Env :> es, Concurrent :> es) =>
  Eff es [AccountSyncStatus]
getSyncAccountStatusHandler = do
  env <- R.ask @Env
  let AppConfig{accountInfos} = env.config

  -- Get the sync status for all accounts from the database, and index it by account ID for easy lookup.
  syncRows <-
    useConnection \conn ->
      Db.getSyncAccountStatuses conn

  let syncByAccountId =
        syncRows
          <&> (\row -> (row.accountId, row))
          & Map.fromList

  --  Get all the accounts from the config, and pair with the corresponding sync status from the database (if it exists).
  pure $
    accountInfos <&> \AccountInfo{accountId, accountName, institutionId} ->
      case Map.lookup accountId syncByAccountId of
        Nothing ->
          AccountSyncStatus
            { accountId = accountId
            , accountName = accountName
            , institutionId = institutionId
            , lastSyncFinishedAt = Nothing
            , lastSyncStatus = Nothing
            , lastSyncError = Nothing
            , lastSyncedTransactionCount = Nothing
            }
        Just row ->
          AccountSyncStatus
            { accountId = accountId
            , accountName = accountName
            , institutionId = institutionId
            , lastSyncFinishedAt = Just row.lastSyncFinishedAt
            , lastSyncStatus = Just row.lastSyncStatus
            , lastSyncError = row.lastSyncError
            , lastSyncedTransactionCount = Just row.lastSyncedTransactionCount
            }
