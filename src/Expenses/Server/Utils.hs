module Expenses.Server.Utils where

import Control.Monad.Error.Class (MonadError)
import CustomPrelude
import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as J
import Data.Map.Lazy qualified as Map
import Servant (throwError)
import Servant qualified as S

{- | NOTE: purescript's JSON decoder for `Map` expects a JSON array, but Haskell's encoder produces a JSON object.
So we use this newtype to encode `Map` as a JSON array instead.
-}
newtype MapAsList k v = MapAsList {unMapAsList :: Map.Map k v}
  deriving newtype (Show, Eq)

instance (ToJSON k, ToJSON v) => ToJSON (MapAsList k v) where
  toJSON (MapAsList m) = toJSON (Map.toList m)

throwJsonError :: forall errBody m a. (MonadError S.ServerError m) => (ToJSON errBody) => S.ServerError -> errBody -> m a
throwJsonError err errBody =
  throwError $
    err
      { S.errBody = J.encode errBody
      , S.errHeaders = [("Content-Type", "application/json;charset=utf-8")]
      }
