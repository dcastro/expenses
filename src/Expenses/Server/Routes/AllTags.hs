module Expenses.Server.Routes.AllTags where

import Config qualified
import CustomPrelude
import Data.Set qualified as Set
import Database qualified as Db
import Expenses.Server.AppM (AppM, Env (..), useConnection)
import Types

allTagsHandler :: AppM (Set TagName)
allTagsHandler = do
  dbTags <- useConnection \conn -> liftIO $ Db.getAllTags conn
  appConfig <- asks (.config)
  let allTags = Set.fromList dbTags `Set.union` Config.allKnownTags appConfig
  pure allTags
