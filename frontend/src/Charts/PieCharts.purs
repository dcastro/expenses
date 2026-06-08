module Charts.PieCharts where

import Prelude

import Core.APITypes (TagGroupName, TagName)
import Core.APITypes as API
import Data.Array as Arr
import Data.Nullable (Nullable)
import Effect (Effect)
import Foreign (Foreign)
import Untagged.Union (type (|+|), asOneOf)
import Utils as Utils

{-
  FFI resources:
  * https://github.com/purescript/documentation/blob/master/guides/FFI-Tips.md
  * https://book.purescript.org/chapter10.html
-}
foreign import _makeChart
  :: forall
       -- The type of the ID for the slices of first level of the chart (e.g. tag group name).
       @first
       -- The type of the ID for the slices of second level of the chart,
       -- i.e. when the user drills down on the pie chart (e.g. tag name).
       @second
   . String
  -> ChartData
  -> (Nullable first -> Effect Unit)
  -> (Nullable first -> Nullable second -> Effect Unit)
  -> Effect Foreign

foreign import _updateChart :: Foreign -> ChartData -> Effect Unit

foreign import _clearSelection :: Foreign -> Effect Unit

foreign import _makeFacetChart
  :: String
  -> Array FacetChartItem
  -> (Nullable TagGroupName -> Effect Unit)
  -> Effect Foreign

foreign import _updateFacetChart :: Foreign -> Array FacetChartItem -> Effect Unit

foreign import _clearFacetSelection :: Foreign -> Effect Unit

foreign import _drilldown :: Foreign -> TagGroupName -> Effect Unit

foreign import _selectSlice :: Foreign -> String -> Effect Unit

type ChartData =
  { subvalues :: Array ChartSlice
  }

-- NOTE: we have to use an untagged union here for JS interoperability.
--
-- The charts library expects there to either be a field `subvalues` of type array, or for the field to not be present at all.
-- Setting it to `null` (using `Nullable`) or `undefined` doesn't work.
--
-- Using a tagged union would break interoperability, so that wouldn't work either.
--
-- https://discourse.purescript.org/t/untagged-union-types/2104/37
type ChartSlice = CharSliceWithoutSubvalues |+| ChartSliceWithSubvalues

type ChartSliceWithSubvalues =
  { name :: String
  , id :: TagGroupName
  , value :: Int
  , subvalues :: Array ChartSubSlice
  }

type CharSliceWithoutSubvalues =
  { name :: String
  , id :: TagGroupName
  , value :: Int
  }

type ChartSubSlice =
  { name :: String
  , id :: TagName
  , value :: Int
  }

-- NOTE: if a group / tag has a negative value (because there were only refunds that month), it won't be shown on the pie chart.
makeChart
  :: String
  -> API.GetTransactions
  -> Int
  -- | This callback is called when the user drills-down into a tag group, or when they go back up.
  -> (Nullable TagGroupName -> Effect Unit)
  -- | This callback is called when the user selects a slice when no subvalues (it doesn't drill down or up).
  -> (Nullable TagGroupName -> Nullable TagName -> Effect Unit)
  -> Effect Foreign
makeChart containerId txs monthsSpan onChartUpdate onSelectionChange = do
  _makeChart @TagGroupName @TagName containerId (makeChartData txs monthsSpan) onChartUpdate onSelectionChange

updateChart :: Foreign -> API.GetTransactions -> Int -> Effect Unit
updateChart chart txs monthsSpan = do
  _updateChart chart (makeChartData txs monthsSpan)

clearSelection :: Foreign -> Effect Unit
clearSelection chart = do
  _clearSelection chart

drilldown :: Foreign -> TagGroupName -> Effect Unit
drilldown chart group = do
  _drilldown chart group

selectSlice :: Foreign -> String -> Effect Unit
selectSlice = _selectSlice

makeChartData :: API.GetTransactions -> Int -> ChartData
makeChartData txs monthsSpan =
  { subvalues: map
      ( \group ->
          -- If the group only contains 1 tag, omit the drill-down in the pie chart.
          if Arr.length group.tags == 1 then asOneOf $
            { name: displayLabel (API.getTagGroupName group.name) (group.groupTotalAmountCents / monthsSpan)
            , id: group.name
            , value: group.groupTotalAmountCents / monthsSpan
            }
          else asOneOf $
            { name: displayLabel (API.getTagGroupName group.name) (group.groupTotalAmountCents / monthsSpan)
            , id: group.name
            , value: group.groupTotalAmountCents / monthsSpan
            , subvalues: group.tags <#>
                \tag ->
                  { name: displayLabel (API.getTagName tag.name) (tag.tagTotalAmountCents / monthsSpan)
                  , id: tag.name
                  , value: tag.tagTotalAmountCents / monthsSpan
                  }
            }
      )
      txs.groupsStats
  }

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

-- | Display a tag or tag group.
-- `displayLabel "groceries" 5000` will return "groceries 50.00€ -".
-- The zoomcharts package will fill the rest of the label with the percentage,
-- e.g. "groceries 50.00€ - 25%".
displayLabel :: String -> Int -> String
displayLabel name value =
  name <> " " <> Utils.centsToEuros value <> " -"
