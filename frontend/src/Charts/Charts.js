
require('@dvsl/zoomcharts');

/**
 * Pie charts:
 *  * https://www.highcharts.com/demo/highcharts/pie-drilldown
 *  * https://www.chartjs.org/docs/latest/samples/other-charts/pie.html
 *  * https://zoomcharts.com/en/javascript-charts-library/charts-packages/pie-chart/
 *  * https://developers.google.com/chart/interactive/docs/gallery/piechart
 *  * https://www.apexcharts.com/javascript-chart-demos/pie-charts/simple-pie-chart/
 *  * https://echarts.apache.org/examples/en/index.html#chart-type-pie
 *        * PS bindings: https://pursuit.purescript.org/packages/purescript-echarts/10.1.0
 *
 *
 * Zoomcharts links:
 *    * https://zoomcharts.com/en/javascript-charts-library/gallery/
 *    * https://zoomcharts.com/en/javascript-charts-library/gallery/demo/chart-packages/piechart/piechart-events-tester
 */


export const _makeChart = containerId => categories => onChartUpdate => onSelectionChange => () => {
  let chart = new PieChart({
    assetsUrlBase: "assets",
    container: document.getElementById(containerId),
    area: { height: null },
    interaction: {
      others: {
        // Disable the "others" slice for smaller items
        // https://zoomcharts.com/developers/en/full-reference/PieChartSettingsInteraction.html
        // https://stackoverflow.com/questions/42678191/calling-setpie-shows-other-slice-eventhough-i-disabled-it
        enabled: false,
      }
    },
    data: {
      preloaded: categories,
    },
    // https://zoomcharts.com/en/javascript-charts-library/gallery/demo/chart-packages/piechart/piechart-events-tester
    events: {
      onChartUpdate: (e, args) => {
        // If this event was triggered by purescript code (e.g. `_clearSelection`),
        // then we don't want to bypass the purescript callbacks
        if (args.origin !== "user") {
          return;
        }

        let stack = chart.getPie();
        let last = stack.at(-1);
        let maybeLast = last === "" ? null : last;
        // `f` returns an `Effect`, so we have to add an `()` to run the effect.
        onChartUpdate(maybeLast)()
      },
      onSelectionChange: (event, args) => {
        // If this event was triggered by purescript code (e.g. `_clearSelection`),
        // then we don't want to bypass the purescript callbacks
        if (args.origin !== "user") {
          return;
        }

        let selected = event.selection.at(-1)?.id;
        let maybeSelected = selected || null; // convert undefined (from the optional chaining `?.` operator) to null

        let tagGroupName = event.pie.id;
        let maybeTagGroupName = tagGroupName === "" ? null : tagGroupName;

        // if (maybeTagGroupName === null) {
        //   if (maybeSelected !== null) {
        //     console.log(`Selected group: ${maybeSelected}`)
        //   } else {
        //     console.log("No selection - show all")
        //   }
        // } else {
        //   if (maybeSelected !== null) {
        //     console.log(`Selected tag: ${maybeSelected} from ${tagGroupName}`)
        //   }
        //   else {
        //     console.log(`Deselected tag from ${tagGroupName}`)
        //   }
        // }

        // If `tagGroupName` is NOT the empty string, then we've just selected a subslice for a tag group, i.e. we selected a tag.
        // Otherwise, we selected a tag group.
        if (maybeTagGroupName === null) {
          onSelectionChange(maybeSelected)(null)();
        } else {
          onSelectionChange(maybeTagGroupName)(maybeSelected)();
        }
      }
    }
  });

  // Set a global var so we can manually interact with the chart in the browser's js console
  globalThis[containerId] = chart;

  return chart;
}

export const _updateChart = chart => data => () => {
  chart.replaceData(data)
}

export const _clearSelection = chart => () => {
  // Deselect slices
  chart.selection([]);
  // Undo "drilldowns"
  chart.setPie([""]);
}


export const _drilldown = chart => tagGroupName => () => {
  chart.setPie(["", tagGroupName]);
}

export const _selectSlice = chart => sliceId => () => {
  // `sliceId` could be the name of a tagGroup or a tag.
  chart.selection([sliceId]);
}
