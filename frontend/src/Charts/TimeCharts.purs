module Charts.TimeCharts where

import Prelude

import Core.APITypes (TagGroupName, TagName)
import Core.APITypes as API
import Core.YearMonth (YearMonth)
import Core.YearMonth as YM
import Data.Array as Array
import Data.Enum as Enum
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Foreign (Foreign)
import Utils as Utils

type TimeChartData =
  { subvalues :: Array TimeChartPoint
  }

type TimeChartPoint =
  { name :: String
  , value :: String
  }

foreign import makeChart :: String -> TimeChartData -> Effect Foreign
foreign import updateChart :: Foreign -> TimeChartData -> Effect Unit

makeChartData :: YearMonth -> YearMonth -> Map YearMonth Int -> TimeChartData
makeChartData from to amountPerMonth =
  { subvalues: (Enum.enumFromTo from to :: Array YearMonth)
      <#> \ym ->
        { name: YM.formatYearMonthPretty ym
        , value: Utils.centsToEurosRaw (Map.lookup ym amountPerMonth # fromMaybe 0)
        }
  }

oneTag :: API.GetTransactions -> YearMonth -> YearMonth -> TagGroupName -> TagName -> TimeChartData
oneTag txs from to tagGroupName tagName =
  txs.groupsStats
    # Array.find (\group -> group.name == tagGroupName)
    <#> _.tags
    # fromMaybe []
    # Array.find (\tag -> tag.name == tagName)
    <#> _.tagAmountPerMonth
    # fromMaybe Map.empty
    # makeChartData from to

oneGroup :: API.GetTransactions -> YearMonth -> YearMonth -> TagGroupName -> TimeChartData
oneGroup txs from to tagGroupName =
  txs.groupsStats
    # Array.find (\group -> group.name == tagGroupName)
    <#> _.tags
    # fromMaybe []
    <#> _.tagAmountPerMonth
    # Array.foldr (Map.unionWith (+)) Map.empty
    # makeChartData from to

allGroups :: API.GetTransactions -> YearMonth -> YearMonth -> TimeChartData
allGroups txs from to =
  txs.groupsStats
    >>= _.tags
    <#> _.tagAmountPerMonth
    # Array.foldr (Map.unionWith (+)) Map.empty
    # makeChartData from to
