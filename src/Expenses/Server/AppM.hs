module Expenses.Server.AppM where

import Config (AppConfig)
import CustomPrelude
import Database.SQLite.Simple qualified as SQL
import Effectful
import Effectful.Concurrent.MVar
import Effectful.Reader.Static
import Log
import Servant.Server (Handler)

type AppM = ReaderT Env (LogT Handler)

data Env = Env
  { dbConn :: MVar SQL.Connection
  , eventLogPath :: FilePath
  , logsDir :: FilePath
  , nordigenSecretId :: Text
  , nordigenSecretKey :: Text
  , config :: AppConfig
  }

useConnection :: (Concurrent :> es, Reader Env :> es) => (SQL.Connection -> Eff es a) -> Eff es a
useConnection f = do
  env <- ask @Env
  let mv = env.dbConn :: MVar SQL.Connection
  withMVar mv \dbConn -> do
    f dbConn

runLogger :: Bool -> Logger -> LogT m a -> m a
runLogger isVerbose logger action =
  runLogT
    "expenses-server"
    logger
    (if isVerbose then LogTrace else LogInfo)
    action
