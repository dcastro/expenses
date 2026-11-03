{-# OPTIONS_GHC -Wno-orphans #-}

module Expenses.Test.Util where

import Config (AppConfig)
import Config qualified
import CustomPrelude
import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database (Contains (..), DoesNotContain (..), IsGTE (..), IsLT (..))
import Database.SQLite.Simple qualified as SQLiteSimple
import Effectful.SQLite.Simple qualified as SQL
import Expenses.NonEmptyText (NonEmptyText)
import Expenses.NonEmptyText qualified as NET
import Expenses.Server.Env (Env (..))
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
  pure
    Env
      { eventLogPath = "/dev/null"
      , logsDir = "/dev/null"
      , demoMode = False
      , nordigenSecretId = ""
      , nordigenSecretKey = ""
      , ntfyTopic = ""
      , config
      }

mkTestDbConn :: IO (M.MVar SQL.Connection)
mkTestDbConn = do
  conn <- SQL.open "./resources/test-app-dir/expenses.db"
  M.newMVar conn

mkInMemoryDbConn :: IO (M.MVar SQL.Connection)
mkInMemoryDbConn = do
  conn <- SQL.open ":memory:"
  schema <- TIO.readFile "db/schema.sql"
  let stmts = T.splitOn ";" schema & fmap T.strip & filter (not . T.null)
  forM_ stmts \stmt ->
    SQLiteSimple.execute_ conn (SQLiteSimple.Query (stmt <> ";"))
  M.newMVar conn

mkTestConfig :: IO AppConfig
mkTestConfig = do
  Config.loadAppConfig "./resources/test-app-dir/config.yaml"

-- | Reads the `structured_output` field of a `claude -p --output-format json` fixture.
readStructuredOutput :: FilePath -> IO J.Value
readStructuredOutput path = do
  bs <- BSL.readFile path
  case J.eitherDecode bs of
    Right (J.Object o) | Just v <- KM.lookup "structured_output" o -> pure v
    _ -> error [i|No `structured_output` in #{path}|]
