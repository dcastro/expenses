module Charts.BudgetCharts where

import Prelude

import Core.APITypes (TagGroupName)
import Core.APITypes as API
import Data.Nullable (Nullable)
import Effect (Effect)
import Foreign (Foreign)
import Utils as Utils

foreign import _makeChart
  :: String
  -> Array FacetChartItem
  -> (Nullable TagGroupName -> Effect Unit)
  -> Effect Foreign

foreign import _updateChart :: Foreign -> Array FacetChartItem -> Effect Unit

foreign import _clearChart :: Foreign -> Effect Unit

type FacetChartItem =
  { name :: String
  , spent :: String
  , limit :: String
  }

makeChart
  :: String
  -> Array API.BudgetTagGroupStats
  -> (Nullable TagGroupName -> Effect Unit)
  -> Effect Foreign
makeChart containerId stats onSelectionChange =
  _makeChart containerId (makeChartData stats) onSelectionChange

updateChart :: Foreign -> Array API.BudgetTagGroupStats -> Effect Unit
updateChart chart stats = _updateChart chart (makeChartData stats)

clearSelection :: Foreign -> Effect Unit
clearSelection = _clearChart

makeChartData :: Array API.BudgetTagGroupStats -> Array FacetChartItem
makeChartData stats =
  stats <#> \s ->
    { name: API.getTagGroupName s.name
    , spent: Utils.centsToEurosRaw s.spentToDateCents
    , limit: Utils.centsToEurosRaw s.limitCents
    }
