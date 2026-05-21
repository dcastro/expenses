module Expenses.Effects (
  AppM,
  naturalTransformation,
  CronM,
  runCronM,

  -- * Effects
  NextUUID,
  Error,
  ServerError,
  Db,
  Time,
  EventLog,
  Nordigen,
  Reader,
  Env (..),
  FileSystem,
  Concurrent,
  Log,

  -- * Utils
  die,
  useConnection,
) where

import Control.Monad.Except (liftEither)
import CustomPrelude
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.MVar qualified as MVar
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Error.Static
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Log
import Effectful.Reader.Static
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Expenses.Effects.EventLog (EventLog)
import Expenses.Effects.EventLog qualified as EventLog
import Expenses.Effects.NextUUID (NextUUID)
import Expenses.Effects.NextUUID qualified as Uuid
import Expenses.Effects.Nordigen (Nordigen)
import Expenses.Effects.Nordigen qualified as N
import Expenses.Effects.SQLite (Db)
import Expenses.Effects.SQLite qualified as SQL
import Expenses.Server.Env (Env (..))
import Servant.Server (Handler, ServerError)
import System.Exit qualified as Exit

type AppM = Eff '[NextUUID, Error ServerError, Db, Time, EventLog, Nordigen, Reader Env, FileSystem, Concurrent, Log, IOE]

naturalTransformation :: forall a. Bool -> Env -> Logger -> AppM a -> Handler a
naturalTransformation isVerbose env logger app = do
  let io :: IO (Either ServerError a) =
        app
          & Uuid.runNextUUID
          & runErrorNoCallStack @ServerError
          & SQL.runDb
          & Time.runTime
          & EventLog.runEventLog
          & N.runNordigen
          & runReader env
          & runFileSystem
          & runConcurrent
          & runLog
            "expenses-server"
            logger
            (if isVerbose then LogTrace else LogInfo)
          & runEff

  either <- liftIO io
  liftEither either

type CronM = Eff '[Db, Time, EventLog, Nordigen, Reader Env, FileSystem, Concurrent, Log, IOE]

runCronM :: forall a. Env -> Logger -> CronM a -> IO a
runCronM env logger app = do
  app
    & SQL.runDb
    & Time.runTime
    & EventLog.runEventLog
    & N.runNordigen
    & runReader env
    & runFileSystem
    & runConcurrent
    & runLog
      "expenses-server"
      logger
      LogTrace
    & runEff

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

die :: Text -> Eff es a
die = unsafeEff_ . Exit.die . T.unpack

useConnection :: (Concurrent :> es, Reader Env :> es) => (SQL.Connection -> Eff es a) -> Eff es a
useConnection f = do
  env <- ask @Env
  let mv = env.dbConn :: MVar SQL.Connection
  MVar.withMVar mv \dbConn -> do
    f dbConn
