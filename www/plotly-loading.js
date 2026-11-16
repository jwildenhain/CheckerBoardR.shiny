(function () {
  'use strict';
  var timer;
  function status(loading, message) {
    var panel = document.getElementById('plotly-loading');
    var container = document.getElementById('plotly-container');
    if (!panel || !container) return;
    clearTimeout(timer);
    panel.hidden = !loading;
    container.setAttribute('aria-busy', String(loading));
    document.getElementById('plotly-loading-message').textContent =
      message || 'Loading interactive 3D plot…';
    if (loading) timer = setTimeout(function () {
      document.getElementById('plotly-loading-message').textContent =
        'Still loading the 3D plot. You can switch to the 2D heatmap while you wait.';
    }, 15000);
  }
  window.checkerboardPlotReady = function () { status(false); };
  document.addEventListener('DOMContentLoaded', function () {
    $(document).on('shiny:recalculating shiny:value', function (event) {
      if (event.name === 'plotlyPlot' || event.target.id === 'plotlyPlot') {
        status(!(event.type === 'shiny:value' && event.value === null));
      }
    });
    $(document).on('shiny:error', function (event) {
      if (event.name === 'plotlyPlot' || event.target.id === 'plotlyPlot') status(false);
    });
    $(document).on('shiny:disconnected', function () {
      var container = document.getElementById('plotly-container');
      if (container && container.getAttribute('aria-busy') === 'true') {
        status(true, 'Connection lost. Please reload the page to try again.');
        clearTimeout(timer);
      }
    });
  });
}());
