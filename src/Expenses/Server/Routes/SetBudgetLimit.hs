module Expenses.Server.Routes.SetBudgetLimit where

import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveFromJSON)
import Database qualified as Db
import Effectful
import Expenses.Effects
import Expenses.Server.Utils (throwJsonError)
import Servant (NoContent (..), err400)
import Types (Admin)

newtype SetBudgetLimitBody = SetBudgetLimitBody {limitCents :: Int}
  deriving stock (Show, Eq)

$(deriveFromJSON defaultOptions ''SetBudgetLimitBody)

setBudgetLimitHandler ::
  (SQLite :> es, Error ServerError :> es) =>
  Admin -> SetBudgetLimitBody -> Eff es NoContent
setBudgetLimitHandler _admin (SetBudgetLimitBody limitCents) = do
  when (limitCents <= 0) $ throwJsonError err400 ("limitCents must be positive" :: Text)
  useConnection \conn -> Db.setBudgetLimit conn limitCents
  pure NoContent
