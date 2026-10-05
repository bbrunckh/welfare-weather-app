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

// Stop an automatic configuration run when its modal is dismissed via Escape
// or the backdrop. The server cannot interrupt a synchronous stage, but it can
// prevent later stages from starting.
(function () {
  var seq = 0;
  document.addEventListener('hidden.bs.modal', function (e) {
    if (!e.target || e.target.id !== 'shiny-modal') return;
    if (!window.Shiny || !Shiny.setInputValue) return;
    seq += 1;
    Shiny.setInputValue('import_dismissed', seq, { priority: 'event' });
  }, true);
})();

// Submit the overview connection form with Enter from a single-line field.
// Scope this to the Data card so other module inputs keep their own behavior.
(function () {
  document.addEventListener('keydown', function (e) {
    if (e.key !== 'Enter' || e.isComposing) return;
    var field = e.target;
    if (!field.matches('.connect-card input[type="text"], .connect-card input[type="password"], .connect-card select')) return;
    if (field.closest('textarea, [contenteditable="true"]')) return;
    var button = field.closest('.connect-card').querySelector('.connection-action-row .btn');
    if (!button || button.disabled) return;
    e.preventDefault();
    button.click();
  });
})();

// ---- Config flyouts (UI-02) --------------------------------------------------
// Shared behavior for the .config-flyout disclosure panels built by
// config_flyout_block() (utils_ui.R):
//   - exactly one flyout open at a time,
//   - aria-expanded kept in sync on the toggle buttons,
//   - focus moved into the flyout on open and back to its toggle on close,
//   - Escape closes the open flyout,
//   - the panel is positioned beside its own toggle (not a shared viewport
//     spot) and follows it while scrolling.
// Panels are marked data-flyout-for="<toggle id>" and get a stable
// "<toggle id>_panel" id; the toggle button carries aria-expanded/controls.
(function () {
  var wasOpen = new WeakMap();
  // Clicks we dispatch ourselves to close other flyouts must not re-trigger
  // the close-others scan (HTMLElement.click() dispatches synchronously).
  var synthetic = new WeakSet();

  function isVisible(el) {
    return el.getClientRects().length > 0;
  }

  function isFixed(panel) {
    return getComputedStyle(panel).position === 'fixed';
  }

  function positionPanel(panel) {
    var anchor = panel.closest('.config-flyout-anchor');
    if (!anchor) return;
    var r = anchor.getBoundingClientRect();
    if (r.width === 0 && r.height === 0) return;
    var w = panel.offsetWidth;
    var h = panel.offsetHeight;
    var left = r.right + 10;
    if (left + w > window.innerWidth - 10) {
      left = Math.max(10, window.innerWidth - w - 10);
    }
    var top = r.top;
    if (top + h > window.innerHeight - 10) {
      top = Math.max(10, window.innerHeight - h - 10);
    }
    panel.style.left = left + 'px';
    panel.style.top = top + 'px';
  }

  // Re-apply aria-expanded / position; optionally move focus for a
  // hidden->visible (focus the panel) or visible->hidden (focus the toggle)
  // transition. When both happen in one pass, the opened panel wins.
  function sync(moveFocus) {
    var opened = null;
    var closed = null;
    document.querySelectorAll('.config-flyout').forEach(function (panel) {
      var open = isVisible(panel);
      var toggle = document.getElementById(panel.getAttribute('data-flyout-for'));
      if (toggle) toggle.setAttribute('aria-expanded', open ? 'true' : 'false');
      if (open && isFixed(panel)) positionPanel(panel);
      var prev = wasOpen.get(panel) || false;
      if (open && !prev && opened === null) opened = panel;
      if (!open && prev) closed = panel;
      wasOpen.set(panel, open);
    });
    if (!moveFocus) return;
    if (opened) {
      opened.setAttribute('tabindex', '-1');
      opened.focus({ preventScroll: true });
    } else if (closed) {
      var t = document.getElementById(closed.getAttribute('data-flyout-for'));
      if (t) t.focus({ preventScroll: true });
    }
  }

  function closePanel(panel) {
    var owner = document.getElementById(panel.getAttribute('data-flyout-for'));
    if (owner) {
      synthetic.add(owner);
      owner.click();
    }
  }

  document.addEventListener('click', function (e) {
    var btn = e.target.closest('.config-flyout-toggle');
    if (!btn) return;
    if (synthetic.has(btn)) {
      synthetic.delete(btn);
      return;
    }
    // One open at a time: close every other visible flyout via its toggle.
    document.querySelectorAll('.config-flyout').forEach(function (panel) {
      if (!isVisible(panel)) return;
      if (panel.getAttribute('data-flyout-for') !== btn.id) closePanel(panel);
    });
    setTimeout(function () { sync(true); }, 60);
  });

  document.addEventListener('keydown', function (e) {
    if (e.key !== 'Escape') return;
    var lastToggle = null;
    var anyClosed = false;
    document.querySelectorAll('.config-flyout').forEach(function (panel) {
      if (!isVisible(panel)) return;
      closePanel(panel);
      lastToggle = document.getElementById(panel.getAttribute('data-flyout-for'));
      anyClosed = true;
    });
    if (anyClosed) {
      e.preventDefault();
      setTimeout(function () {
        sync(false);
        if (lastToggle) lastToggle.focus({ preventScroll: true });
      }, 60);
    }
  });

  // Keep an open fixed-position flyout beside its toggle while the page or
  // any container scrolls.
  function repositionOpen() {
    document.querySelectorAll('.config-flyout').forEach(function (panel) {
      if (isVisible(panel) && isFixed(panel)) positionPanel(panel);
    });
  }
  window.addEventListener('scroll', repositionOpen, { passive: true, capture: true });
  window.addEventListener('resize', repositionOpen);

  document.addEventListener('DOMContentLoaded', function () {
    sync(false);
  });
})();

// Server-driven disabled state for pill_toggle() radios and plain inputs
// (update_pill_toggle_disabled() / update_input_disabled() in utils_ui.R).
// Inserted controls may not be in the DOM yet when the message arrives, so
// the handler retries briefly.
(function () {
  function apply(msg) {
    if (msg.kind === 'input') {
      var el = document.getElementById(msg.id);
      if (!el) return false;
      el.disabled = !!msg.all;
      if (msg.all && msg.tooltip) el.title = msg.tooltip; else el.removeAttribute('title');
      return true;
    }
    var radios = document.querySelectorAll('input[type="radio"][name="' + msg.id + '"]');
    if (!radios.length) return false;
    var values = [].concat(msg.values || []);
    radios.forEach(function (r) {
      var off = !!msg.all || values.indexOf(r.value) > -1;
      r.disabled = off;
      if (off) r.setAttribute('aria-disabled', 'true'); else r.removeAttribute('aria-disabled');
      var wrap = r.closest('.radio-inline, .form-check-inline, .radio, .form-check') || r.parentElement;
      if (!wrap) return;
      wrap.classList.toggle('pill-disabled', off);
      if (off && msg.tooltip) wrap.title = msg.tooltip; else wrap.removeAttribute('title');
    });
    return true;
  }
  function run(msg, tries) {
    if (apply(msg) || tries <= 0) return;
    setTimeout(function () { run(msg, tries - 1); }, 100);
  }
  function register() {
    Shiny.addCustomMessageHandler('wise_set_disabled', function (msg) { run(msg, 30); });
  }
  if (window.Shiny && Shiny.addCustomMessageHandler) register();
  else document.addEventListener('shiny:connected', register, { once: true });
})();
