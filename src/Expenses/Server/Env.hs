module Expenses.Server.Env where

import Config (AppConfig)
import CustomPrelude
import Database.SQLite.Simple qualified as SQL

data Env = Env
  { dbConn :: MVar SQL.Connection
  , eventLogPath :: FilePath
  , logsDir :: FilePath
  , demoMode :: Bool
  , nordigenSecretId :: Text
  , nordigenSecretKey :: Text
  , config :: AppConfig
  }
