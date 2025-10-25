module Utils where

import Prelude

import Control.Alt (class Alt, alt)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut as J
import Data.Argonaut.Encode.Generic as J
import Data.Bounded.Generic (class GenericBottom, genericBottom)
import Data.Either (Either(..))
import Data.Enum (class Enum, pred, succ)
import Data.Enum.Generic (class GenericEnum, genericSucc)
import Data.Foldable (class Foldable)
import Data.Foldable as F
import Data.Foldable as Fold
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep as Rep
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Ord (abs)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Foreign (MultipleErrors, renderForeignError)
import Partial.Unsafe (unsafeCrashWith)
import Yoga.JSON (class ReadForeign, class WriteForeign)
import Yoga.JSON as J

{- |
Similar to the function `the` from Idris, used to manually ascribe the type of an expression
in a chain of functions.

https://www.idris-lang.org/docs/idris2/current/prelude_docs/docs/Prelude.Basics.html#Prelude.Basics.the

```
myMap
  # Map.toUnfoldable
  # the @(Array _)
  <#> (\(k /\ v) -> ...)
```
-}
the :: forall @a. a -> a
the = identity

whenJust :: forall @a @m. Monad m => Maybe a -> (a -> m Unit) -> m Unit
whenJust mb action =
  case mb of
    Just a -> action a
    Nothing -> pure unit

throwOnNothing :: forall m a. MonadThrow String m => (Unit -> String) -> Maybe a -> m a
throwOnNothing mkMsg mb =
  case mb of
    Just a -> pure a
    Nothing -> throwError (mkMsg unit)

throwOnLeft :: forall m e a. MonadThrow String m => (e -> String) -> Either e a -> m a
throwOnLeft mkMsg either =
  case either of
    Right a -> pure a
    Left err -> throwError (mkMsg err)

renderJsonErr :: MultipleErrors -> String
renderJsonErr errs =
  Fold.intercalate ", " (renderForeignError <$> errs)

listEnum :: forall a rep. Generic a rep => GenericEnum rep => GenericBottom rep => Array a
listEnum = unfoldr (\b -> b >>= next) $ Just genericBottom
  where
  next a = Just $ Tuple a $ genericSucc a

unsafeFromJust :: forall @a. String -> Maybe a -> a
unsafeFromJust reason mb =
  case mb of
    Just a -> a
    Nothing -> unsafeCrashWith reason

unsafeFromRight :: forall @a. Either String a -> a
unsafeFromRight = case _ of
  Right a -> a
  Left err -> unsafeCrashWith err

newtype MapAsArray k v = MapAsArray (Map k v)

instance WriteForeign (Tuple k v) => WriteForeign (MapAsArray k v) where
  writeImpl (MapAsArray x) = J.writeImpl (Map.toUnfoldable x :: Array (Tuple k v))

instance (ReadForeign (Tuple k v), Ord k) => ReadForeign (MapAsArray k v) where
  readImpl json = do
    arr <- J.readImpl json :: _ (Array (Tuple k v))
    pure $ MapAsArray $ Map.fromFoldable arr

derive instance Newtype (MapAsArray k v) _

newtype SetAsArray a = SetAsArray (Set a)

instance WriteForeign a => WriteForeign (SetAsArray a) where
  writeImpl (SetAsArray x) = J.writeImpl (Set.toUnfoldable x :: Array a)

instance (ReadForeign a, Ord a) => ReadForeign (SetAsArray a) where
  readImpl json = do
    arr <- J.readImpl json :: _ (Array a)
    pure $ SetAsArray $ Set.fromFoldable arr

derive instance Newtype (SetAsArray a) _

succCyclic :: forall a. Bounded a => Enum a => a -> a
succCyclic a =
  case succ a of
    Just b -> b
    Nothing -> bottom

predCyclic :: forall a. Bounded a => Enum a => a -> a
predCyclic a =
  case pred a of
    Just b -> b
    Nothing -> top

-- | Combines a collection of elements using the `Alt` operation.
--
-- Similar to `Data.Foldable.oneOf`, but only requires `Alt` instead of `Plus`.
oneOf' :: forall f g a. Foldable f => Alt g => g a -> f (g a) -> g a
oneOf' = F.foldr alt

centsToEuros :: Int -> String
centsToEuros cents =
  centsToEurosRaw cents <> "€"

centsToEurosRaw :: Int -> String
centsToEurosRaw cents =
  let
    absCents = abs cents
    euros = absCents `div` 100
    centsRemainder = absCents `mod` 100
    eurosStr = show euros
    centsStr = if centsRemainder < 10 then "0" <> show centsRemainder else show centsRemainder
    sign = if cents < 0 then "-" else ""
  in
    sign <> eurosStr <> "." <> centsStr

-- | Encode a sum type in a format compatible with aeson's default options.
encodeAesonSum :: forall a r. Rep.Generic a r => J.EncodeRep r => a -> J.Json
encodeAesonSum = J.genericEncodeJsonWith { tagKey: "tag", unwrapSingleArguments: true, valuesKey: "contents" }
