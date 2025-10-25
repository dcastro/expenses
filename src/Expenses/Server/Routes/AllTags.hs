module Expenses.Server.Routes.AllTags where

import CustomPrelude
import Database qualified as Db
import Expenses.Server.AppM (AppM, useConnection)
import Types

allTagsHandler :: AppM [TagName]
allTagsHandler = do
  useConnection $ \conn -> do
    liftIO $ Db.getAllTags conn
