{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Expenses.Effects.NextUUID (
  NextUUID,
  runNextUUID,
  nextRandom,
  Uuid.toText,
) where

import Data.UUID (UUID)
import Data.UUID qualified as Uuid
import Data.UUID.V4 qualified as Uuid
import Effectful
import Effectful.Dispatch.Dynamic

data NextUUID :: Effect where
  NextRandom :: NextUUID m UUID

type instance DispatchOf NextUUID = Dynamic

runNextUUID :: (IOE :> es) => Eff (NextUUID : es) a -> Eff es a
runNextUUID = interpret \_ -> \case
  NextRandom -> liftIO Uuid.nextRandom

nextRandom :: (NextUUID :> es) => Eff es UUID
nextRandom = send NextRandom
