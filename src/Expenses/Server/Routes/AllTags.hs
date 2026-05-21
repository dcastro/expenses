module Expenses.Server.Routes.AllTags where

import Config qualified
import CustomPrelude
import Data.Set qualified as Set
import Database qualified as Db
import Effectful
import Effectful.Reader.Static
import Expenses.Effects
import Expenses.Server.AppM (Env (..), useConnection)
import Types

allTagsHandler ::
  (Reader Env :> es, Concurrent :> es, Db :> es) =>
  Eff es (Set TagName)
allTagsHandler = do
  dbTags <- useConnection \conn -> Db.getAllTags conn
  appConfig <- asks @Env (.config)
  let allTags = Set.fromList dbTags `Set.union` Config.allKnownTags appConfig
  pure allTags
