module Expenses.Server.AppM where

import Config (AppConfig)
import Control.Concurrent qualified as M
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)
import CustomPrelude
import Database.SQLite.Simple qualified as SQL
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

useConnection :: (MonadReader Env m, MonadBaseControl IO m) => (SQL.Connection -> m a) -> m a
useConnection f = do
  env <- ask
  -- NOTE: instead of `liftBaseOp` from `monad-control`, we could have also re-implemented
  -- `withMVar` using `resourcet` instead of `bracket`: https://hackage.haskell.org/package/resourcet
  liftBaseOp (M.withMVar env.dbConn) \dbConn -> do
    f dbConn

runLogger :: Bool -> Logger -> LogT m a -> m a
runLogger isVerbose logger action =
  runLogT
    "expenses-server"
    logger
    (if isVerbose then LogTrace else LogInfo)
    action
