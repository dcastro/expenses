{-# OPTIONS_GHC -Wno-orphans #-}

module Expenses.Test.Util where

import Config (AppConfig)
import Config qualified
import CustomPrelude
import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as J
import Data.Text qualified as T
import Database (Contains (..), DoesNotContain (..), IsGTE (..), IsLT (..))
import Database.SQLite.Simple qualified as SQL
import Expenses.NonEmptyText (NonEmptyText)
import Expenses.NonEmptyText qualified as NET
import Expenses.Server.AppM (Env (..))
import GHC.MVar qualified as M
import Servant (ServerError)
import Types (TagName (..))

instance ToJSON ServerError where
  toJSON = J.toJSON . show @Text

instance IsString NonEmptyText where
  fromString = NET.unsafeFromText . T.pack

deriving newtype instance IsString Contains
deriving newtype instance IsString DoesNotContain
deriving newtype instance Num IsGTE
deriving newtype instance Num IsLT
deriving newtype instance IsString TagName

mkTestEnv :: IO Env
mkTestEnv = do
  config <- mkTestConfig
  dbConn <- liftIO $ SQL.open "./resources/test-app-dir/expenses.db" >>= M.newMVar
  pure
    Env
      { dbConn
      , eventLogPath = "/dev/null"
      , logsDir = "/dev/null"
      , nordigenSecretId = ""
      , nordigenSecretKey = ""
      , config
      }

mkTestConfig :: IO AppConfig
mkTestConfig = do
  Config.loadAppConfig "./resources/test-app-dir/config.yaml"
