module Expenses.Server.Routes.AllAccounts where

import CustomPrelude
import Database qualified as Db
import Effectful
import Expenses.Effects

allAccountsHandler :: (SQLite :> es) => Eff es [Text]
allAccountsHandler = do
  useConnection \conn -> do
    accounts <- Db.getAllAccounts conn
    -- NOTE: moving `Cash` to the front of the list so that it becomes
    -- the default account in the UI form for inserting new transactions.
    pure $ "Cash" : filter (/= "Cash") accounts
