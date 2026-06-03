module Expenses.Effects (
  AppM,
  naturalTransformation,
  CronM,
  runCronM,

  -- * Effects
  NextUUID,
  Error,
  ServerError,
  SQLite,
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
  SQL.useConnection,
  loggerName,
) where

import Control.Monad.Except (liftEither)
import CustomPrelude
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Error.Static
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Log
import Effectful.Reader.Static
import Effectful.SQLite.Simple (Connection, SQLite)
import Effectful.SQLite.Simple qualified as SQL
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Expenses.Effects.EventLog (EventLog)
import Expenses.Effects.EventLog qualified as EventLog
import Expenses.Effects.NextUUID (NextUUID)
import Expenses.Effects.NextUUID qualified as Uuid
import Expenses.Effects.Nordigen (Nordigen)
import Expenses.Effects.Nordigen qualified as N
import Expenses.Server.Env (Env (..))
import Servant.Server (Handler, ServerError)
import System.Exit qualified as Exit

loggerName :: Text
loggerName = "expenses-server"

type AppM = Eff '[NextUUID, Error ServerError, SQLite, Time, EventLog, Nordigen, Reader Env, FileSystem, Concurrent, Log, IOE]

naturalTransformation :: forall a. Bool -> MVar Connection -> Env -> Logger -> AppM a -> Handler a
naturalTransformation isVerbose conn env logger app = do
  let io :: IO (Either ServerError a) =
        app
          & Uuid.runNextUUID
          & runErrorNoCallStack @ServerError
          & SQL.runSQLiteSync conn
          & Time.runTime
          & EventLog.runEventLog
          & N.runNordigen
          & runReader env
          & runFileSystem
          & runConcurrent
          & runLog
            loggerName
            logger
            (if isVerbose then LogTrace else LogInfo)
          & runEff

  either <- liftIO io
  liftEither either

type CronM = Eff '[SQLite, Time, EventLog, Nordigen, Reader Env, FileSystem, Concurrent, Log, IOE]

runCronM :: forall a. MVar Connection -> Env -> Logger -> CronM a -> IO a
runCronM conn env logger app = do
  app
    & SQL.runSQLiteSync conn
    & Time.runTime
    & EventLog.runEventLog
    & N.runNordigen
    & runReader env
    & runFileSystem
    & runConcurrent
    & runLog
      loggerName
      logger
      LogTrace
    & runEff

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

die :: Text -> Eff es a
die = unsafeEff_ . Exit.die . T.unpack
