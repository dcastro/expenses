module Charts.BudgetCharts where

import Prelude

import Core.APITypes (TagGroupName)
import Core.APITypes as API
import Data.Map (Map)
import Data.Map as Map
import Data.Nullable (Nullable)
import Data.Tuple (Tuple(..))
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
  -> Map TagGroupName API.BudgetTagGroupStats
  -> (Nullable TagGroupName -> Effect Unit)
  -> Effect Foreign
makeChart containerId stats onSelectionChange =
  _makeChart containerId (makeChartData stats) onSelectionChange

updateChart :: Foreign -> Map TagGroupName API.BudgetTagGroupStats -> Effect Unit
updateChart chart stats = _updateChart chart (makeChartData stats)

clearSelection :: Foreign -> Effect Unit
clearSelection = _clearChart

makeChartData :: Map TagGroupName API.BudgetTagGroupStats -> Array FacetChartItem
makeChartData stats =
  Map.toUnfoldable stats <#>
    ( \(Tuple name s) ->
        { name: API.getTagGroupName name
        , spent: Utils.centsToEurosRaw s.spentToDateCents
        , limit: Utils.centsToEurosRaw s.limitCents
        }
    )
