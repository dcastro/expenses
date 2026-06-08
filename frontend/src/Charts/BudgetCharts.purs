module Charts.BudgetCharts where

import Prelude

import Core.APITypes (TagGroupName)
import Core.APITypes as API
import Data.Nullable (Nullable)
import Effect (Effect)
import Foreign (Foreign)
import Utils as Utils

foreign import _makeFacetChart
  :: String
  -> Array FacetChartItem
  -> (Nullable TagGroupName -> Effect Unit)
  -> Effect Foreign

foreign import _updateFacetChart :: Foreign -> Array FacetChartItem -> Effect Unit

foreign import _clearFacetSelection :: Foreign -> Effect Unit

type FacetChartItem =
  { name :: String
  , spent :: String
  , limit :: String
  }

makeBudgetFacetChart
  :: String
  -> Array API.BudgetTagGroupStats
  -> (Nullable TagGroupName -> Effect Unit)
  -> Effect Foreign
makeBudgetFacetChart containerId stats onSelectionChange =
  _makeFacetChart containerId (makeFacetChartData stats) onSelectionChange

updateBudgetFacetChart :: Foreign -> Array API.BudgetTagGroupStats -> Effect Unit
updateBudgetFacetChart chart stats = _updateFacetChart chart (makeFacetChartData stats)

clearFacetSelection :: Foreign -> Effect Unit
clearFacetSelection = _clearFacetSelection

makeFacetChartData :: Array API.BudgetTagGroupStats -> Array FacetChartItem
makeFacetChartData stats =
  stats <#> \s ->
    { name: API.getTagGroupName s.name
    , spent: Utils.centsToEurosRaw s.spentToDateCents
    , limit: Utils.centsToEurosRaw s.limitCents
    }
