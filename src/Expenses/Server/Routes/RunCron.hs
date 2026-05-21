module Expenses.Server.Routes.RunCron where

import CustomPrelude hiding (Reader, ask)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.FileSystem (FileSystem)
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader)
import Effectful.Time (Time)
import Expenses.Effects.EventLog (EventLog)
import Expenses.Effects.Nordigen (Nordigen)
import Expenses.Effects.SQLite (Db)
import Expenses.Server.AppM (Env)
import Expenses.Server.CronJob qualified as CronJob
import Servant (NoContent (..))

runCronHandler :: (Reader Env :> es, FileSystem :> es, Nordigen :> es, Log :> es, Time :> es, EventLog :> es, Concurrent :> es, Db :> es) => Eff es NoContent
runCronHandler = do
  CronJob.nordigenJob
  pure NoContent
