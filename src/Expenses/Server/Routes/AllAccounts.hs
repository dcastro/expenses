module Expenses.Server.Routes.AllAccounts where

import CustomPrelude
import Database qualified as Db
import Expenses.Server.AppM (AppM, useConnection)

allAccountsHandler :: AppM [Text]
allAccountsHandler = do
  useConnection \conn -> do
    accounts <- liftIO $ Db.getAllAccounts conn
    -- NOTE: moving `Cash` to the front of the list so that it becomes
    -- the default account in the UI form for inserting new transactions.
    pure $ "Cash" : filter (/= "Cash") accounts
