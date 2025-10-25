require('@dvsl/zoomcharts');

/*
    * https://zoomcharts.com/en/javascript-charts-library/gallery/demo/chart-types/area-charts/area-chart-for-categorical-data
    * https://zoomcharts.com/en/javascript-charts-library/gallery/demo/chart-types/area-charts/area-chart

*/

export const makeChart = containerId => (chartData) => () => {
  let chart = new FacetChart({
    assetsUrlBase: "assets",
    series: [
      {
        name: '€',
        data: {
          aggregation: 'sum',
        },
        style: {
          fillColor: '#2fc32f',
        },
        type: 'columns',

        // Show labels above each plot point
        valueLabels: {
          enabled: true,
          position: 'aboveValue',
          style: {
            backgroundStyle: {
              lineColor: '#ccc',
              lineWidth: 1,
              fillColor: 'rgba(200,200,200,0.5)',
            },
            borderRadius: 2,
          },
          contentsFunction: function (value) {
            return value.toFixed(2) + '€';
          },
        },
      },
    ],
    container: containerId,
    data: [
      {
        preloaded: chartData,
      },
    ],
    toolbar: {
      fullscreen: true,
      enabled: true,
    },
    interaction: {
      resizing: {
        enabled: false,
      },
    },
  });


  // Set a global var so we can manually interact with the chart in the browser's js console
  globalThis[containerId] = chart;

  return chart;
};

export const updateChart = (chart) => (data) => () => {
  chart.replaceData(data);
};
