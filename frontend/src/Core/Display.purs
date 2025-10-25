module Core.Display where

import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Prelude

class Display a where
  display :: a -> String

instance Display NonEmptyString where
  display = NES.toString

instance Display String where
  display = identity
