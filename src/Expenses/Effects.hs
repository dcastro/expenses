module Expenses.Effects where

import Control.Monad.Except (liftEither)
import CustomPrelude hiding (Reader, runReader)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Error.Static
import Effectful.Log
import Effectful.Reader.Static
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Expenses.Effects.EventLog (EventLog)
import Expenses.Effects.EventLog qualified as EventLog
import Expenses.Effects.NextUUID (NextUUID)
import Expenses.Effects.NextUUID qualified as Uuid
import Expenses.Effects.SQLite (Db)
import Expenses.Effects.SQLite qualified as SQL
import Expenses.Server.AppM (Env)
import Servant.Server (Handler, ServerError)

type AppM = Eff '[Db, Time, EventLog, NextUUID, Reader Env, Concurrent, Error ServerError, Log, IOE]

naturalTransformation :: forall a. Bool -> Env -> Logger -> AppM a -> Handler a
naturalTransformation isVerbose env logger app = do
  let io :: IO (Either ServerError a) =
        app
          & SQL.runDb
          & Time.runTime
          & EventLog.runEventLog
          & Uuid.runNextUUID
          & runReader env
          & runConcurrent
          & runErrorNoCallStack @ServerError
          & runLog
            "expenses-server"
            logger
            (if isVerbose then LogTrace else LogInfo)
          & runEff

  either <- liftIO io
  liftEither either

die :: Text -> Eff es a
die = unsafeEff_ . CustomPrelude.die . T.unpack
