{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Expenses.Effects.SQLite (
  Db,
  runDb,
  query,
  query_,
  execute,
  withTransaction,
  SQL.Connection,
) where

import CustomPrelude
import Database.SQLite.Simple (Connection, FromRow, ToRow)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromRow (RowParser)
import Effectful
import Effectful.Dispatch.Dynamic

data Db :: Effect where
  QueryWith :: (ToRow q) => RowParser r -> Connection -> SQL.Query -> q -> Db m [r]
  QueryWith_ :: RowParser r -> Connection -> SQL.Query -> Db m [r]
  Execute :: (ToRow q) => Connection -> SQL.Query -> q -> Db m ()
  WithTransaction :: Connection -> m a -> Db m a

type instance DispatchOf Db = Dynamic

runDb :: (IOE :> es) => Eff (Db ': es) a -> Eff es a
runDb = interpret \env -> \case
  QueryWith parser conn sql params -> do
    liftIO $ SQL.queryWith parser conn sql params
  QueryWith_ parser conn sql -> do
    liftIO $ SQL.queryWith_ parser conn sql
  Execute conn sql params -> do
    liftIO $ SQL.execute conn sql params
  WithTransaction conn action -> do
    localSeqUnliftIO env \unlift -> do
      liftIO $ SQL.withTransaction conn (unlift action)

queryWith :: (Db :> es) => (ToRow q) => RowParser r -> Connection -> SQL.Query -> q -> Eff es [r]
queryWith = send ... QueryWith

queryWith_ :: (Db :> es) => RowParser r -> Connection -> SQL.Query -> Eff es [r]
queryWith_ = send ... QueryWith_

execute :: (Db :> es) => (ToRow q) => Connection -> SQL.Query -> q -> Eff es ()
execute = send ... Execute

withTransaction :: (Db :> es) => Connection -> Eff es a -> Eff es a
withTransaction = send ... WithTransaction

query :: forall q r es. (Db :> es) => (ToRow q, FromRow r) => Connection -> SQL.Query -> q -> Eff es [r]
query = queryWith SQL.fromRow

query_ :: forall r es. (Db :> es) => (FromRow r) => Connection -> SQL.Query -> Eff es [r]
query_ = queryWith_ SQL.fromRow
