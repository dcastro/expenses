module Expenses.Server.Routes.RunCron where

import CustomPrelude hiding (Reader, ask)
import Effectful
import Expenses.Effects
import Expenses.Server.CronJob qualified as CronJob
import Servant (NoContent (..))

runCronHandler ::
  (Reader Env :> es, FileSystem :> es, Nordigen :> es, Log :> es, Time :> es, EventLog :> es, Concurrent :> es, Db :> es) =>
  Eff es NoContent
runCronHandler = do
  CronJob.nordigenJob
  pure NoContent
