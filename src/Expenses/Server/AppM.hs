module Expenses.Server.AppM where

import Config (AppConfig)
import Control.Concurrent qualified as M
import Control.Monad.Reader qualified as R
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)
import CustomPrelude hiding (Reader, ask)
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

useConnection2 :: (Concurrent :> es, Reader Env :> es) => (SQL.Connection -> Eff es a) -> Eff es a
useConnection2 f = do
  env <- ask @Env
  let mv = env.dbConn :: MVar SQL.Connection
  withMVar mv \dbConn -> do
    f dbConn

-- TODO: delete
useConnection :: (MonadReader Env m, MonadBaseControl IO m) => (SQL.Connection -> m a) -> m a
useConnection f = do
  env <- R.ask
  -- NOTE: instead of `liftBaseOp` from `monad-control`, we could have also re-implemented
  -- `withMVar` using `resourcet` instead of `bracket`: https://hackage.haskell.org/package/resourcet
  --
  -- It's safe to lift `withMVar` using `MonadBaseControl` here, the monad state is always restored.
  -- https://lexi-lambda.github.io/blog/2019/09/07/demystifying-monadbasecontrol/
  --
  -- This is exactly how `withMvar` from `lifted-base` is implemented.
  liftBaseOp (M.withMVar env.dbConn) \dbConn -> do
    f dbConn

runLogger :: Bool -> Logger -> LogT m a -> m a
runLogger isVerbose logger action =
  runLogT
    "expenses-server"
    logger
    (if isVerbose then LogTrace else LogInfo)
    action
