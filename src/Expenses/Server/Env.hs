module Expenses.Server.Env where

import Config (AppConfig)
import CustomPrelude
import Database.SQLite.Simple qualified as SQL
import Log

data Env = Env
  { dbConn :: MVar SQL.Connection
  , eventLogPath :: FilePath
  , logsDir :: FilePath
  , nordigenSecretId :: Text
  , nordigenSecretKey :: Text
  , config :: AppConfig
  }

runLogger :: Bool -> Logger -> LogT m a -> m a
runLogger isVerbose logger action =
  runLogT
    "expenses-server"
    logger
    (if isVerbose then LogTrace else LogInfo)
    action
