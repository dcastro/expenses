{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Expenses.Linear where

import CustomPrelude as Prelude
import Data.Aeson qualified as J
import Data.Monoid.Linear as Linear
import Data.Time (Day, UTCTime (..))
import Data.Unrestricted.Linear (Consumable, Movable)
import Prelude.Linear qualified as Linear
import Unsafe.Linear qualified as Unsafe
import Unsafe.Linear qualified as UnsafeLinear

----------------------------------------------------------------------------
-- Linear Aeson
----------------------------------------------------------------------------

pairs :: J.Series %1 -> J.Encoding
pairs = Unsafe.toLinear J.pairs

instance Linear.Semigroup J.Series where
  (<>) = Unsafe.toLinear2 (Prelude.<>)

instance Linear.Monoid J.Series where
  mempty = Prelude.mempty

-- A marker class for types we know are linearly consumed when serialized.
class (J.ToJSON a) => LinearToJSON a
instance LinearToJSON UTCTime
instance LinearToJSON Day
instance LinearToJSON Text
instance LinearToJSON Int
instance LinearToJSON Bool
instance (LinearToJSON a) => LinearToJSON (Maybe a)

(.=) :: (LinearToJSON v) => J.Key -> v %1 -> J.Series
(.=) key = Unsafe.toLinear ((J..=) key)

liftMove :: (Movable a) => a %1 -> (a -> r) %1 -> r
liftMove x f =
  case Linear.move x of
    Linear.Ur x -> f x

liftConsume :: (Consumable a) => a %1 -> r %1 -> r
liftConsume x r =
  case Linear.consume x of
    () -> r

-- I'm not sure why `linear-base`'s `coerce` doesn't have the `Coercible` constraint,
-- so I'm adding this safer version here.
linearCoerce :: (Coercible a b) => a %1 -> b
linearCoerce = UnsafeLinear.coerce
