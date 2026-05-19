module Expenses.Server.Routes.AllTags2 where

import Config qualified
import CustomPrelude hiding (Reader, ask, asks)
import Data.Set qualified as Set
import Database qualified as Db
import Expenses.Server.AppM (AppM, Env (..), useConnection, useConnection2)
import Types
import Control.Monad.Reader qualified as R
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Reader.Static
import Expenses.Effects.SQLite (Db)

allTagsHandler :: (Reader Env :> es, Concurrent :> es, Db :> es) => Eff es (Set TagName)
allTagsHandler = do
  dbTags <- useConnection2 \conn -> Db.getAllTags2 conn
  appConfig <- asks @Env (.config)
  let allTags = Set.fromList dbTags `Set.union` Config.allKnownTags appConfig
  pure allTags
