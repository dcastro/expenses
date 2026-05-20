module Expenses.Effects where

import Control.Monad.Except (liftEither)
import CustomPrelude hiding (Reader, runReader)
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.Log
import Effectful.Reader.Static
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Expenses.Effects.SQLite (Db)
import Expenses.Effects.SQLite qualified as SQL
import Expenses.Server.AppM (Env)
import Servant.Server (Handler, ServerError)

type AppM = Eff '[Reader Env, Db, Time, Concurrent, Error ServerError, Log, IOE]

naturalTransformation :: forall a. Bool -> Env -> Logger -> AppM a -> Handler a
naturalTransformation isVerbose env logger app = do
  let io :: IO (Either ServerError a) =
        app
          & runReader env
          & SQL.runDb
          & Time.runTime
          & runConcurrent
          & runErrorNoCallStack @ServerError
          & runLog
            "expenses-server"
            logger
            (if isVerbose then LogTrace else LogInfo)
          & runEff

  either <- liftIO io
  liftEither either
