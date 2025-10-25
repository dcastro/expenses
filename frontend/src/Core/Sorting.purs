module Core.Sorting where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- | Column identifiers used when sorting the search transactions table.
data SortingColumn
  = Date
  | Amount
  | Tag

derive instance Eq SortingColumn
derive instance Generic SortingColumn _

instance Show SortingColumn where
  show = genericShow

data SortedOrder = Asc | Desc

derive instance Eq SortedOrder
derive instance Generic SortedOrder _

instance Show SortedOrder where
  show = genericShow

type Sorted =
  { column :: SortingColumn
  , order :: SortedOrder
  }

-- | Default sorting option used when none is provided.
defaultSorted :: Sorted
defaultSorted =
  { column: Date
  , order: Desc
  }

defaultOrder :: SortedOrder
defaultOrder = defaultSorted.order
