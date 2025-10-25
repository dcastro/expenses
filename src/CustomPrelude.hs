module CustomPrelude (
  module Export,
  i,
) where

import Data.String.Interpolate qualified as Interpolate
import Data.String.Interpolate.Util qualified as Interpolate
import Data.Text qualified as T
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Universum as Export hiding (view, (%~), (.~), (^.), (^..), (^?), _1, _2, _3)

{- | A string interpolator, equivalent to the `interpolate` package
but builds a `Text` instead of a `String`.

We use `unindent` to strip whitespace common to all lines.
-}
i :: QuasiQuoter
i =
  QuasiQuoter
    { quoteExp = \s -> [e|T.pack $ Interpolate.unindent $(Interpolate.i.quoteExp s)|]
    , quotePat = Interpolate.i.quotePat
    , quoteType = Interpolate.i.quoteType
    , quoteDec = Interpolate.i.quoteDec
    }

----------------------------------------------------------------------------
-- Docs: Interpolation examples
----------------------------------------------------------------------------

-- >>> _example1
-- "Aaa\n  bbb\nccc"
_example1 :: String
_example1 =
  Interpolate.unindent
    [Interpolate.i|
    Aaa
      bbb
    ccc|]

-- >>> _example2
-- "Aaa\n  bbb\nccc\n"
_example2 :: String
_example2 =
  Interpolate.unindent
    [Interpolate.i|
      Aaa
        bbb
      ccc
    |]

-- >>> _example3
-- "Aaa bbb ccc   "
_example3 :: String
_example3 =
  Interpolate.unindent
    [Interpolate.i|    Aaa bbb ccc   |]

-- >>> _example4
-- "Aaa\n      bbb\n    ccc"
_example4 :: String
_example4 =
  Interpolate.unindent
    [Interpolate.i|Aaa
      bbb
    ccc|]
