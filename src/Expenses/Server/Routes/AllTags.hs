module Expenses.Server.Routes.AllTags where

import Config qualified
import CustomPrelude
import Data.Set qualified as Set
import Database qualified as Db
import Expenses.Server.AppM (AppM, useConnection)
import Types

allTagsHandler :: AppM (Set TagName)
allTagsHandler = do
  dbTags <- useConnection \conn -> liftIO $ Db.getAllTags conn
  let allTags = Set.fromList dbTags `Set.union` Config.allKnownTags
  pure allTags
