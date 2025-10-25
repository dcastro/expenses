module Expenses.Server.Routes.RunCron where

import CustomPrelude
import Expenses.Server.AppM (AppM)
import Expenses.Server.CronJob qualified as CronJob
import Log (Logger)
import Servant (NoContent (..))
import Types (Admin)

runCronHandler :: Logger -> Admin -> AppM NoContent
runCronHandler logger _admin = do
  env <- ask
  liftIO $ CronJob.nordigenJob env logger
  pure NoContent
