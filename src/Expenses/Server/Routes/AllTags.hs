module Expenses.Server.Routes.AllTags where

import Config qualified
import CustomPrelude
import Data.Set qualified as Set
import Database qualified as Db
import Effectful
import Effectful.Reader.Static
import Expenses.Effects
import Types

allTagsHandler ::
  (Reader Env :> es, SQLite :> es) =>
  Eff es (Set TagName)
allTagsHandler = do
  -- Combine these tags:
  --  * Tags that belong to groups (from the config section `allTagGroups`)
  --  * Tags the user has added to the config, but don't necessarily exist in the db yet (from the config section `ungroupedTags`)
  --  * Tags that exist in the db, but not necessarily in the config.
  dbTags <- useConnection \conn -> Db.getAllTags conn
  appConfig <- asks @Env (.config)
  let allTags = Set.fromList dbTags `Set.union` Config.allKnownTags appConfig
  pure allTags
