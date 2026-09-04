// hexmap.js - MapLibre GL JS + browser-side H3 cell decoding for WISE-APP.
//
// R sends columnar payloads (cell ids + values + ramp stops) over the fixed
// custom-message type "hexmap"; this module decodes each H3 index to a
// polygon with h3-js and paints a single GeoJSON source whose colour is an
// interpolate/match expression. Geometry never leaves the browser as
// anything but H3 strings. This is the pattern proven in the data-insights
// Connect deployment: MapLibre vector basemap, one fill layer, no R-side
// geometry aggregation.
//
// Contract (see R/fct_hexmap.R):
//   { id: <container id>, action: "set"|"clear"|"fit",
//     h3: [...], v: [...], v_log: [...], v_kind: "continuous"|"binned"|"binary",
//     stops: {domain: [lo, hi], colors: [...]} | {levels: [...], colors: [...]},
//     bounds: [xmin, ymin, xmax, ymax], label: "...", unit: "..." }
//
// Guarantees:
//   - boot-once per container; maps boot lazily on the first payload
//   - load-once: the file is served twice (golem::bundle_resources() also
//     serves it in alphabetical order and the explicit htmlDependency loads
//     it again); the second copy is a no-op. Without the guard the two
//     copies keep separate message queues/replay registries and race their
//     MutationObservers: a re-rendered uiOutput's replacement container
//     could boot from the copy holding no replay state and stay blank.
//   - camera persists across "set"; "fit" fires only when R says the data
//     key changed, so wave toggles re-colour without a camera jump
//   - messages arriving before the container exists are queued per id and
//     drained when the container appears (uiOutput flush order); the last
//     set/fit per id is also replayed onto a *replacement* container (a
//     re-rendered uiOutput), so a re-render can never leave a blank map
(function () {
  "use strict";

  // Load-once guard: the first copy of this file owns the message router,
  // queues and replay registry; every later copy must not touch them.
  if (window.__wiseappHexmapLoaded) return;
  window.__wiseappHexmapLoaded = true;

  var CARTO_POSITRON_STYLE =
    "https://tiles.basemaps.cartocdn.com/gl/positron-gl-style/style.json";
  var EMPTY_FC = { type: "FeatureCollection", features: [] };
  // na.color parity with the Leaflet palettes: cells without a value.
  var GREY = "#cccccc";

  // Shiny serialises length-1 R vectors as bare JSON values, so every
  // payload field is normalised through here.
  function asArr(x) {
    if (x === null || x === undefined) return [];
    return Array.isArray(x) ? x : [x];
  }

  function reportInput(name, value) {
    if (!name) return;
    if (window.Shiny && typeof window.Shiny.setInputValue === "function") {
      window.Shiny.setInputValue(name, value, { priority: "event" });
    }
  }

  // ---- Colour expressions --------------------------------------------------
  // Ramp input is the cell property the payload says to read: "v_log" when
  // the ramp is log-scaled, "v" otherwise. Cells without a value (JSON null)
  // paint grey.
  function rampExpr(inputExpr, kind, stops) {
    var colors = asArr(stops && stops.colors);
    if (kind === "binned" || kind === "binary") {
      var levels = asArr(stops && stops.levels);
      var e = ["match", ["to-string", inputExpr]];
      for (var i = 0; i < levels.length && i < colors.length; i++) {
        e.push(String(levels[i]), colors[i]);
      }
      e.push(GREY);
      return e;
    }
    var dom = asArr(stops && stops.domain);
    if (dom.length >= 2 && colors.length >= 2) {
      var e2 = ["interpolate", ["linear"], inputExpr];
      if (dom.length === colors.length) {
        // Quantile-style domain: the caller supplies one value per colour,
        // so stops land at the actual distribution (thin cells spread out,
        // dense-city outliers compressed into the dark end).
        for (var j = 0; j < colors.length; j++) e2.push(dom[j], colors[j]);
      } else {
        // Legacy uniform domain: expand the stops evenly across [lo, hi].
        var lo = dom[0], hi = dom[dom.length - 1];
        var span = hi - lo;
        for (var j = 0; j < colors.length; j++) {
          var t = colors.length === 1 ? 0 : j / (colors.length - 1);
          e2.push(span === 0 ? lo : lo + t * span, colors[j]);
        }
      }
      return e2;
    }
    return GREY;
  }

  function colourExpr(state) {
    var prop = state.ramp && state.ramp.log ? "v_log" : "v";
    var input = ["get", prop];
    var inner = (state.ramp && state.ramp.stops)
      ? rampExpr(input, state.ramp.kind, state.ramp.stops)
      : GREY;
    return ["case", ["==", ["typeof", ["get", "v"]], "null"], GREY, inner];
  }

  // ---- Cell geometry -------------------------------------------------------
  function hexFC(ids, v, vLog, info, dash) {
    var lib = window.h3;
    var feats = [];
    for (var i = 0; i < ids.length; i++) {
      var id = ids[i];
      if (typeof id !== "string" || !id) continue;
      var ring = null;
      // "geojson" forces [lng, lat] pairs: h3-js 4.x defaults to [lat, lng],
      // which mirrors every cell across the equator/prime meridian.
      try { ring = lib.cellToBoundary(id, "geojson"); } catch (err) { ring = null; }
      if (!ring || ring.length < 3) continue;

      // Unwrap antimeridian crossings so cell rings never smear across the
      // map; MapLibre renders longitudes outside [-180, 180] wrapped.
      var coords = [];
      var prev = null;
      for (var k = 0; k < ring.length; k++) {
        var x = ring[k][0];
        if (prev !== null) {
          while (x - prev > 180) x -= 360;
          while (x - prev < -180) x += 360;
        }
        prev = x;
        coords.push([x, ring[k][1]]);
      }
      coords.push(coords[0].slice()); // close the ring

      var props = { h3: id };
      if (v && i < v.length && v[i] !== null && v[i] !== undefined) props.v = v[i];
      if (vLog && i < vLog.length && vLog[i] !== null && vLog[i] !== undefined) {
        props.v_log = vLog[i];
      }
      if (info && i < info.length && info[i] !== null && info[i] !== undefined) {
        props.info = info[i];
      }
      if (dash && i < dash.length && dash[i]) {
        props.dash = true;
      }
      feats.push({
        type: "Feature",
        geometry: { type: "Polygon", coordinates: [coords] },
        properties: props
      });
    }
    return { type: "FeatureCollection", features: feats };
  }

  // ---- Tooltip -------------------------------------------------------------
  function escHtml(s) {
    return String(s)
      .replace(/&/g, "&amp;").replace(/</g, "&lt;")
      .replace(/>/g, "&gt;").replace(/"/g, "&quot;");
  }

  function fmtVal(x) {
    if (x === null || x === undefined) return "no value";
    if (typeof x === "number" && isFinite(x)) {
      if (Math.abs(x) >= 1000) return Math.round(x).toLocaleString("en-US");
      return (Math.round(x * 100) / 100).toLocaleString("en-US",
        { maximumFractionDigits: 2 });
    }
    return String(x);
  }

  function hideTip(state) {
    state.tip.style.display = "none";
    state.map.getCanvas().style.cursor = "";
  }

  function showTip(state, e) {
    var f = e.features && e.features[0];
    if (!f) return;
    state.map.getCanvas().style.cursor = "pointer";
    var p = f.properties || {};
    var unit = state.unit ? " " + state.unit : "";
    state.tip.innerHTML =
      "<strong>" + escHtml(state.label || "Value") + ":</strong> " +
      escHtml(fmtVal(p.v)) + escHtml(unit) +
      (p.info ? "<br>" + escHtml(p.info) : "");
    state.tip.style.display = "block";
    var box = state.map.getContainer();
    var x = e.point.x + 14, y = e.point.y + 14;
    var maxX = box.clientWidth - (state.tip.offsetWidth + 10);
    var maxY = box.clientHeight - (state.tip.offsetHeight + 10);
    state.tip.style.left = Math.max(4, Math.min(x, maxX)) + "px";
    state.tip.style.top = Math.max(4, Math.min(y, maxY)) + "px";
  }

  // ---- Reset-view control --------------------------------------------------
  // Replaces the Leaflet `.add_reset_button()` hook: returns the camera to
  // the last fitted data bounds, so panning and zooming is recoverable.
  function ResetControl(onReset) { this._onReset = onReset; }
  ResetControl.prototype.onAdd = function (map) {
    var btn = document.createElement("button");
    btn.type = "button";
    btn.className = "hexmap-reset";
    btn.title = "Reset view";
    btn.setAttribute("aria-label", "Reset view");
    btn.innerHTML = "&#9678;";
    btn.style.cssText =
      "font-size:15px;line-height:26px;width:29px;height:29px;" +
      "background:transparent;border:0;padding:0;cursor:pointer;color:#333;";
    var self = this;
    btn.addEventListener("click", function () { self._onReset(); });
    var wrap = document.createElement("div");
    wrap.className = "maplibregl-ctrl maplibregl-ctrl-group";
    wrap.appendChild(btn);
    this._wrap = wrap;
    return wrap;
  };
  ResetControl.prototype.onRemove = function () {
    if (this._wrap && this._wrap.parentNode) {
      this._wrap.parentNode.removeChild(this._wrap);
    }
  };

  // ---- Payload actions -----------------------------------------------------
  function fitNow(state) {
    state.map.fitBounds(state.lastBounds, { padding: 30, duration: 0 });
  }

  function applySet(state, msg) {
    state.label = asArr(msg.label)[0] || "";
    state.unit = asArr(msg.unit)[0] || "";
    var vLog = msg.v_log !== null && msg.v_log !== undefined;
    state.ramp = {
      kind: asArr(msg.v_kind)[0] || "continuous",
      stops: msg.stops || null,
      log: vLog
    };
    var fc = hexFC(asArr(msg.h3), asArr(msg.v), vLog ? asArr(msg.v_log) : null,
                   msg.info === null || msg.info === undefined ? null : asArr(msg.info),
                   msg.dash === null || msg.dash === undefined ? null : asArr(msg.dash));
    if (!state.ready) { state.pending = msg; return; }
    state.map.getSource("hex").setData(fc);
    var expr = colourExpr(state);
    state.map.setPaintProperty("hex-fill", "fill-color", expr);
    state.map.setPaintProperty("hex-line", "line-color", expr);
    // Fresh data alone does not always wake a stalled render loop (the
    // first tiles can sit unloaded until some layout event pokes the map -
    // seen on bslib cards whose size settles after boot); repaint now.
    state.map.triggerRepaint();
  }

  function applyClear(state) {
    state.ramp = null;
    state.pending = null;
    state.lastBounds = null;
    state.fitPending = false;
    state.label = "";
    state.unit = "";
    if (state.ready) state.map.getSource("hex").setData(EMPTY_FC);
    hideTip(state);
  }

  function applyFit(state, msg) {
    var b = asArr(msg.bounds).map(Number);
    if (b.length !== 4 || !b.every(isFinite)) return;
    state.lastBounds = [[b[0], b[1]], [b[2], b[3]]];
    if (state.ready) fitNow(state);
    else state.fitPending = true;
  }

  function dispatch(state, msg) {
    var action = asArr(msg.action)[0];
    if (action === "clear") applyClear(state);
    else if (action === "fit") applyFit(state, msg);
    else applySet(state, msg);
  }

  // ---- Boot ----------------------------------------------------------------
  function boot(container) {
    // Boot-once guard per container.
    if (container.__hexmap) return container.__hexmap;
    if (!window.maplibregl) return null;

    var tip = document.createElement("div");
    tip.className = "hexmap-tooltip";
    tip.setAttribute("aria-hidden", "true");
    tip.style.cssText =
      "position:absolute;z-index:25;display:none;pointer-events:none;" +
      "background:#fff;border-radius:4px;box-shadow:0 2px 8px rgba(0,0,0,.35);" +
      "padding:6px 10px;max-width:240px;font-size:12px;line-height:1.45;";
    container.appendChild(tip);

    var state = {
      map: null, ready: false, pending: null, fitPending: false,
      lastBounds: null, ramp: null, label: "", unit: "",
      clickInput: container.getAttribute("data-hexmap-click"),
      tip: tip
    };

    var map;
    try {
      map = new maplibregl.Map({
        container: container,
        style: CARTO_POSITRON_STYLE,
        attributionControl: { compact: true }
      });
    } catch (err) {
      // Context creation failed (no WebGL2 in this browser): no map can
      // boot here. The container stays blank rather than half-working.
      container.__hexmap = { unsupported: true };
      return null;
    }
    state.map = map;
    container.__hexmap = state;

    map.addControl(new maplibregl.NavigationControl({ showCompass: false }),
      "top-left");
    map.addControl(new ResetControl(function () {
      if (state.lastBounds) map.fitBounds(state.lastBounds, { duration: 0 });
    }), "top-left");
    map.dragRotate.disable();
    map.touchZoomRotate.disableRotation();

    map.on("error", function (e) {
      // Style/tile fetch failures are logged by MapLibre; capability is
      // decided above, so these must not bubble into unhandled errors.
      if (window.console && console.debug) console.debug("hexmap:", e && e.error);
    });

    map.on("load", function () {
      map.addSource("hex", { type: "geojson", data: EMPTY_FC, buffer: 0, tolerance: 0 });
      var expr = colourExpr(state);
      map.addLayer({
        id: "hex-fill", type: "fill", source: "hex",
        paint: { "fill-color": expr, "fill-opacity": 0.75 }
      });
      // 1px same-colour line layer: kills the hairline seams between
      // edge-to-edge hexagon fills.
      map.addLayer({
        id: "hex-line", type: "line", source: "hex",
        paint: { "line-color": expr, "line-width": 1, "line-opacity": 0.75 }
      });
      // Dashed outline marking cells whose value summarises several
      // interview months (the payload flags them). Sits above the fill so
      // the dashes read against the cell colour.
      map.addLayer({
        id: "hex-dash", type: "line", source: "hex",
        filter: ["==", ["get", "dash"], true],
        paint: {
          "line-color": "#333333",
          "line-width": 1.5,
          "line-opacity": 0.9,
          "line-dasharray": [1.5, 1.5]
        }
      });
      state.ready = true;
      if (state.pending) {
        var p = state.pending;
        state.pending = null;
        applySet(state, p);
      }
      if (state.fitPending && state.lastBounds) {
        state.fitPending = false;
        fitNow(state);
      }
      // Kick the render loop: the first tiles must load even when the
      // card's layout settles after boot (see applySet).
      map.triggerRepaint();
      map.on("mousemove", "hex-fill", function (e) { showTip(state, e); });
      map.on("mouseleave", "hex-fill", function () { hideTip(state); });
      map.on("click", "hex-fill", function (e) {
        var f = e.features && e.features[0];
        var h = f && f.properties && f.properties.h3;
        if (h) reportInput(state.clickInput, h);
      });
    });

    // Cards expand to full screen; keep the canvas measured. The one-shot
    // kicks below also cover layouts that settle AFTER boot (bslib card
    // entrance animation, tab activation): without them the canvas keeps
    // its boot-time size and the first tile batch never renders.
    var kicks = [400, 1500];
    for (var kk = 0; kk < kicks.length; kk++) {
      setTimeout(function () {
        if (!container.isConnected) return;
        map.resize();
        map.triggerRepaint();
      }, kicks[kk]);
    }
    if (window.ResizeObserver) {
      var ro = new ResizeObserver(function () {
        if (!container.isConnected) { ro.disconnect(); return; }
        if (container.offsetWidth > 0 && container.offsetHeight > 0) map.resize();
      });
      ro.observe(container);
    } else {
      window.addEventListener("resize", function () { map.resize(); });
    }

    return state;
  }

  // ---- Message routing -----------------------------------------------------
  var queued = {}; // container id -> [payloads] not yet deliverable
  var replay = {}; // container id -> {set: msg|null, fit: msg|null} last known
                   // state, re-applied when a re-rendered uiOutput swaps in a
                   // replacement container element

  function record(id, msg) {
    var action = asArr(msg.action)[0];
    var rp = replay[id] || (replay[id] = {});
    if (action === "fit") {
      rp.fit = msg;
    } else {
      rp.set = msg;
      if (action === "clear") rp.fit = null;
    }
  }

  function handleMessage(msg) {
    if (!msg) return;
    var id = asArr(msg.id)[0];
    if (id === null || id === undefined) return;
    id = String(id);

    var el = document.getElementById(id);
    if (el) {
      var st = boot(el);
      if (st) {
        record(id, msg);
        dispatch(st, msg);
      }
      return;
    }
    // Container not rendered yet (renderUI flush order): queue and drain
    // when it appears. Only the most recent few payloads are worth keeping.
    record(id, msg);
    var q = queued[id] || (queued[id] = []);
    q.push(msg);
    if (q.length > 20) q.shift();
  }

  function drain(id, el) {
    var wasFresh = !el.__hexmap;
    var q = queued[id];
    if (q && q.length) delete queued[id];
    var st = boot(el);
    if (!st) return;
    if (q && q.length) {
      for (var i = 0; i < q.length; i++) {
        record(id, q[i]);
        dispatch(st, q[i]);
      }
      return;
    }
    // No fresh messages: this is a replacement container from a re-rendered
    // uiOutput. Re-apply the last known set/fit so the map is never blank.
    if (wasFresh && replay[id]) {
      if (replay[id].set) dispatch(st, replay[id].set);
      if (replay[id].fit) dispatch(st, replay[id].fit);
    }
  }

  function scanContainers() {
    var els = document.querySelectorAll(".hexmap-container");
    for (var i = 0; i < els.length; i++) {
      if (els[i].id) drain(els[i].id, els[i]);
    }
  }

  if (window.MutationObserver && typeof document !== "undefined") {
    var mo = new MutationObserver(scanContainers);
    var startMo = function () {
      if (document.body) mo.observe(document.body, { childList: true, subtree: true });
    };
    if (document.body) startMo();
    else document.addEventListener("DOMContentLoaded", startMo);
  }

  // Register the single custom-message handler. shiny.js always precedes
  // app resources in the document, but retry briefly in case of unusual
  // dependency ordering.
  function register() {
    if (window.Shiny && typeof window.Shiny.addCustomMessageHandler === "function") {
      window.Shiny.addCustomMessageHandler("hexmap", handleMessage);
      return true;
    }
    return false;
  }
  if (!register()) {
    var iv = setInterval(function () {
      if (register()) clearInterval(iv);
    }, 50);
    setTimeout(function () { clearInterval(iv); }, 10000);
  }
})();