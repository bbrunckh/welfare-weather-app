// Hide the "no results yet" empty-state placeholder on a step's Overview
// tab once additional output tabs have been appended (appendTab() adds new
// <li> nav-links to the same tabsetPanel). Purely cosmetic — does not touch
// any Shiny inputs/outputs.
(function () {
  function refresh(navTabs) {
    var pane = navTabs.closest('.tab-content, .bslib-page');
    if (!pane) pane = document;
    var hasExtraTabs = navTabs.querySelectorAll('a.nav-link').length > 1;
    var container = navTabs.parentElement.querySelector('.tab-content') ||
      document.querySelector('.tab-content');
    if (!container) return;
    var emptyStates = container.querySelectorAll(':scope > .tab-pane .empty-state');
    emptyStates.forEach(function (el) {
      el.style.display = hasExtraTabs ? 'none' : '';
    });
  }

  function scan() {
    document.querySelectorAll('ul.nav-tabs').forEach(refresh);
  }

  document.addEventListener('DOMContentLoaded', function () {
    scan();
    var observer = new MutationObserver(scan);
    observer.observe(document.body, { childList: true, subtree: true });
  });
})();
