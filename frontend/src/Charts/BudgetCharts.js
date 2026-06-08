
require('@dvsl/zoomcharts');

export const _makeFacetChart = containerId => chartData => onSelectionChange => () => {
  let chart = new FacetChart({
    assetsUrlBase: "assets",
    container: document.getElementById(containerId),
    data: [{ preloaded: { subvalues: chartData } }],
    facetAxis: { size: 70 },
    stacks: {
      "s1": {
        // Overlap the 2 series on top of each other
        type: "based",
      }
    },
    // https://zoomcharts.com/developers/en/facet-chart/api-reference/settings/series.html
    // https://zoomcharts.com/developers/en/full-reference/FacetChartSettingsSeriesColumns.html
    series: [
      {
        id: "spent",
        name: "Spent to date",
        data: { field: "spent" },
        // Make this bar narrower
        // https://zoomcharts.com/developers/en/full-reference/FacetChartSettingsSeriesColumnsStyle.html
        style: { widthScale: 0.95 },
        stack: "s1",
        valueLabels: {
          enabled: true,
          position: 'aboveValue',
          // When the 2 bars have similar values, their labels will overlap.
          // This option allows the overlapping labels to stack on top of each other instead of hiding one of them.
          // https://zoomcharts.com/developers/en/full-reference/LinearChartSettingsValueLabels.html
          allowOverlappingLabelStacking: true,
          contentsFunction: function (value) {
            return value.toFixed(2) + '€';
          }
        }
      },
      {
        id: "limit",
        name: "Monthly limit",
        data: { field: "limit" },
        style: {
          fillColor: "transparent",
          lineColor: "black",
          lineWidth: 2,
        },
        stack: "s1",
        valueLabels: {
          enabled: true,
          position: 'aboveValue',
          allowOverlappingLabelStacking: true,
          contentsFunction: function (value) {
            return value.toFixed(2) + '€';
          }
        }
      }
    ],
    items: {
      styleFunction: function (item, data) {
        let spent = item.values[0];
        let limit = item.values[1];
        if (spent.value > limit.value) {
          spent.style.fillColor = "red";
        } else {
          spent.style.fillColor = "green";
        }
      }
    },
    events: {
      onSelectionChange: (event, args) => {
        if (args.origin !== "user") return;
        let selected = event.selection && event.selection.length > 0 ? event.selection[0] : null;
        let selectedName = selected?.data?.name;
        onSelectionChange(selectedName)();
      }
    }
  });

  globalThis[containerId] = chart;
  return chart;
}

export const _updateFacetChart = chart => chartData => () => {
  chart.replaceData({ subvalues: chartData });
}

export const _clearFacetSelection = chart => () => {
  chart.selection([]);
}
