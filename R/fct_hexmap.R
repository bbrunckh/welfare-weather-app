# ============================================================================ #
# Hex-map engine: MapLibre GL JS + browser-side H3 cell decoding.              #
#                                                                              #
# Every map in the app renders at H3-cell level. Geometry never leaves the     #
# browser except as H3 index strings: R sends a columnar payload              #
# (cell ids + values + colour-ramp stops), and hexmap.js decodes each cell     #
# to a polygon with h3-js and paints it through a single MapLibre GeoJSON      #
# source whose colour is an `interpolate`/`match` expression - no per-cell     #
# colour serialization, no server-side geometry aggregation. The pattern is    #
# the one proven in the data-insights Connect deployment.                      #
#                                                                              #
# Vendored assets (review 5.3 gate: no CDN at runtime, Connect-safe, no new    #
# R packages). Pins recorded here; upgrade by re-fetching these exact URLs     #
# and updating the pins + hashes:                                              #
#                                                                              #
#   inst/app/www/vendor/maplibre-gl.js    maplibre-gl 5.24.0                   #
#     https://unpkg.com/maplibre-gl@5.24.0/dist/maplibre-gl.js                 #
#     sha256 45a9b07a9189ce56054c620a947ccf41e291e58c95e9b61533b740aaa65ee5cb  #
#                                                                              #
#   inst/app/www/vendor/maplibre-gl.css   maplibre-gl 5.24.0                   #
#     https://unpkg.com/maplibre-gl@5.24.0/dist/maplibre-gl.css                #
#     sha256 ab1e70d59ec40465bae7e7030da2f3ccf28133fd502e62bd598eefbadfd7a732  #
#                                                                              #
#   inst/app/www/vendor/h3-js.umd.js      h3-js 4.1.0 (UMD; global `h3`;       #
#     `cellToBoundary` is the only API hexmap.js uses)                         #
#     https://unpkg.com/h3-js@4.1.0/dist/h3-js.umd.js                          #
#     sha256 0870d94de38503bdf6b63dce8c4812a42dc43565f15ae35694aedf4f97eaf548  #
#                                                                              #
# Basemap: keyless CARTO vector Positron (the pattern proven in the            #
# data-insights Connect deployment). Raster tiles die with the Leaflet maps.   #
#                                                                              #
# Note: `golem::bundle_resources()` also scans inst/app/www and re-attaches    #
# these scripts in alphabetical order (hexmap.js would load before the         #
# libraries). hexmap.js is load-order tolerant, and its load-once guard makes  #
# the second copy a strict no-op: without it the two copies keep separate      #
# message queues/replay registries and race their MutationObservers, so a      #
# re-rendered container could boot from the copy holding no replay state and   #
# stay blank. The explicit dependency below always loads last.                 #
# ============================================================================ #


# ---- Dependency ------------------------------------------------------------ #

#' Attach the vendored hex-map engine (scripts in strict order)
#'
#' maplibre-gl -> h3-js -> hexmap.js. Attached once in `app_ui.R` so the
#' ordering cannot be broken by `bundle_resources()`'s alphabetical scan;
#' also attached by `hexmap_ui()` for standalone module harnesses
#' (htmltools deduplicates the identical dependency, so the scripts still
#' load exactly once).
#'
#' @return An `htmltools::htmlDependency`.
#' @noRd
hexmap_dependency <- function() {
  htmltools::htmlDependency(
    name       = "wiseapp-hexmap",
    # Bump on every engine change: the version is part of the script URL,
    # so browsers re-fetch instead of serving a stale cached engine.
    version    = "1.0.5",
    src        = app_sys("app", "www"),
    script     = c(
      "vendor/maplibre-gl.js",
      "vendor/h3-js.umd.js",
      "hexmap.js"
    ),
    stylesheet = "vendor/maplibre-gl.css",
    all_files  = FALSE
  )
}


# ---- UI -------------------------------------------------------------------- #

#' Hex-map container (UI-36 parity)
#'
#' A `<div>` with `role="region"` + `aria-label` that hexmap.js boots a
#' MapLibre map into on the first payload it receives. The `data-hexmap-click`
#' attribute names the Shiny input the JS writes: `<id>_hex_click` (last
#' clicked cell's H3 index).
#'
#' @param id         Namespaced container id, e.g. `ns("density_map")`. The
#'   same string must be passed as the `id` of the matching `hexmap_*`
#'   senders.
#' @param height     CSS height for the container. Pass `"100%"` inside a
#'   fillable bslib card (wrap the call in `bslib::as_fill_carrier()`).
#' @param aria_label Descriptive text for screen readers.
#' @param legend     Optional tag (e.g. a `uiOutput`) positioned over the
#'   map's top-right corner. The R side rebuilds it from the same palette
#'   state as the payloads (`.compact_legend_html()`), so the legend always
#'   matches what the cells show.
#'
#' @noRd
hexmap_ui <- function(id, height = "400px", aria_label = "Map", legend = NULL) {
  # Percentage heights need flex behaviour: the modules render this container
  # through a uiOutput, which bslib flattens with `display: contents` - so the
  # shell becomes a direct flex child of the card body and `height: 100%`
  # alone would resolve against the body (including the title row) and
  # overflow the card. A flex shell fills exactly the space left over.
  flex <- grepl("%$", height)
  style <- paste0(
    "position: relative; width: 100%; overflow: hidden; height: ", height, ";",
    if (flex) " flex: 1 1 auto; min-height: 120px;" else NULL
  )
  shiny::tags$div(
    class = "hexmap-shell",
    style = style,
    shiny::tags$div(
      id                  = id,
      class               = "hexmap-container",
      role                = "region",
      `aria-label`        = aria_label,
      style               = "position: relative; height: 100%; width: 100%; overflow: hidden;",
      `data-hexmap-click` = paste0(id, "_hex_click")
    ),
    if (!is.null(legend)) {
      # Top-right keeps the legend clear of MapLibre's attribution control
      # (bottom-right); the zoom/reset controls sit top-left. The max-width
      # keeps any legend content inside the map: without it a long note line
      # would inherit the card's font and spill across the map.
      shiny::tags$div(
        class = "hexmap-legend",
        style = paste(
          "position: absolute; right: 8px; top: 8px; z-index: 10;",
          "max-width: calc(100% - 70px);"
        ),
        legend
      )
    }
  )
}


# ---- Payload builders ------------------------------------------------------ #

#' Build a columnar `set` payload for one hex map
#'
#' The wire contract shared by every map surface (see the migration plan):
#' cell ids and values travel as parallel arrays; colour comes from ramp
#' `stops` applied by a MapLibre expression in the browser; bounds come from
#' the per-cell bbox columns. Vectors are unnamed so Shiny's JSON serializer
#' keeps them as arrays.
#'
#' @param h3     Character vector of H3 index strings.
#' @param v      Colour value per cell: numeric for `"continuous"`, level
#'   strings for `"binned"`/`"binary"`. `NA` renders grey in the browser.
#' @param v_kind One of `"continuous"`, `"binned"`, `"binary"`.
#' @param stops  For continuous: `list(domain = c(lo, hi), colors = c(...))`
#'   (ramp stops evenly spaced across the domain - pass the *transformed*
#'   domain when the ramp is log-scaled and carry the transformed values in
#'   `v_log`). For binned/binary: `list(levels = c(...), colors = c(...))`.
#' @param bounds Optional `[xmin, ymin, xmax, ymax]` for the initial fit.
#' @param v_log  Optional numeric vector, same length as `h3`: precomputed
#'   transformed value the ramp reads instead of `v` (e.g. log counts).
#' @param info   Optional character vector, same length as `h3`: a per-cell
#'   extra line for the hover tooltip (e.g. location identifiers from
#'   `cell_map`). `NA` entries are dropped by the browser.
#' @param dash   Optional logical vector, same length as `h3`: cells marked
#'   `TRUE` get a dashed outline in the browser (e.g. values averaged across
#'   several interview months).
#' @param label  Scalar label used by the hover tooltip.
#' @param unit   Scalar unit suffix for the hover tooltip.
#'
#' @return A named list with `action = "set"` plus the payload columns.
#' @noRd
hexmap_payload <- function(h3, v, v_kind = c("continuous", "binned", "binary"),
                           stops = NULL, bounds = NULL, v_log = NULL,
                           info = NULL, dash = NULL, label = "", unit = "") {
  v_kind <- match.arg(v_kind)

  h3 <- as.character(unname(h3))
  v  <- if (v_kind == "continuous") as.numeric(unname(v)) else as.character(unname(v))
  if (length(h3) != length(v)) {
    stop("hexmap_payload: h3 and v must be the same length", call. = FALSE)
  }

  v_log <- if (is.null(v_log)) NULL else as.numeric(unname(v_log))
  if (!is.null(v_log) && length(v_log) != length(h3)) {
    stop("hexmap_payload: v_log must match h3 length", call. = FALSE)
  }
  info <- if (is.null(info)) NULL else as.character(unname(info))
  if (!is.null(info) && length(info) != length(h3)) {
    stop("hexmap_payload: info must match h3 length", call. = FALSE)
  }
  dash <- if (is.null(dash)) NULL else unname(dash)
  if (!is.null(dash) && length(dash) != length(h3)) {
    stop("hexmap_payload: dash must match h3 length", call. = FALSE)
  }

  keep <- !is.na(h3) & nzchar(h3)

  label <- as.character(label)[1]
  unit  <- as.character(unit)[1]
  payload <- list(
    action  = "set",
    h3      = h3[keep],
    v       = v[keep],
    v_kind  = v_kind,
    stops   = stops,
    label   = if (is.na(label)) "" else label,
    unit    = if (is.na(unit)) "" else unit
  )
  if (!is.null(v_log)) payload$v_log <- v_log[keep]
  if (!is.null(info))  payload$info  <- info[keep]
  if (!is.null(dash))  payload$dash  <- as.logical(dash)[keep]

  if (!is.null(bounds)) {
    bounds <- as.numeric(bounds)
    if (length(bounds) != 4L || anyNA(bounds) || any(!is.finite(bounds))) {
      stop("hexmap_payload: bounds must be finite [xmin, ymin, xmax, ymax]",
           call. = FALSE)
    }
    payload$bounds <- unname(bounds)
  }

  payload
}


# ---- Senders --------------------------------------------------------------- #

# Messages ride one fixed custom-message type; the payload carries the
# namespaced container id and hexmap.js routes on it.
.hexmap_send <- function(session, msg) {
  session$sendCustomMessage("hexmap", msg)
  invisible(TRUE)
}

#' Send a payload to one hex-map container
#'
#' @param session Shiny session.
#' @param ns      The module's namespace function.
#' @param id      Local output id (namespaced inside this call).
#' @param payload A `hexmap_payload()` list (or `clear`/`fit` equivalent).
#'
#' @noRd
hexmap_update <- function(session, ns, id, payload) {
  stopifnot(is.list(payload), !is.null(payload$action))
  .hexmap_send(session, c(list(id = ns(id)), payload))
}

#' Clear one hex-map container (INT-06: stale geography must never outlive
#' its microdata)
#'
#' @noRd
hexmap_clear <- function(session, ns, id) {
  .hexmap_send(session, list(id = ns(id), action = "clear"))
}

#' Fit one hex-map container's camera to a bbox
#'
#' Sent only when the data key behind the map changes (PERF-36 view-key
#' semantics); wave toggles send `set` alone, so the user's pan/zoom
#' survives.
#'
#' @param bounds `[xmin, ymin, xmax, ymax]`; silently skipped when `NULL` or
#'   not finite.
#'
#' @noRd
hexmap_fit <- function(session, ns, id, bounds) {
  if (is.null(bounds)) return(invisible(FALSE))
  bounds <- unname(as.numeric(bounds))
  if (length(bounds) != 4L || anyNA(bounds) || any(!is.finite(bounds))) {
    return(invisible(FALSE))
  }
  .hexmap_send(session, list(id = ns(id), action = "fit", bounds = bounds))
}
