# ============================================================================ #
# Pure functions for survey statistics and data preparation logic.             #
# Used by mod_1_02_surveystats_server(). 
# All functions are stateless and testable without a Shiny session.                                            #
# ============================================================================ #


# ---------------------------------------------------------------------------- #
# Time columns                                                                  #
# ---------------------------------------------------------------------------- #

#' Add derived time columns to a survey data frame
#'
#' Constructs `month` (integer 1-12), and `countryyear` (character label) from the integer
#' columns `int_year` and `int_month` that are present in raw survey parquet
#' files.
#'
#' @param df A data frame containing integer columns `int_year`, `int_month`,
#'   `economy`, and `year`.
#'
#' @return `df` with two additional columns: `month` (integer), and `countryyear` (character).
#'
#' @export
add_time_columns <- function(df) {
  df |>
    dplyr::mutate(
      month       = lubridate::month(timestamp),
      countryyear = paste0(economy, ", ", year)
    )
}


# ---------------------------------------------------------------------------- #
# Data level                                                                    #
# ---------------------------------------------------------------------------- #

#' Assign a data level indicator for CPI/PPP joining
#'
#' Produces a `data_level` column used to join the CPI/PPP deflator table.
#' For China (`code == "CHN"`) the level is `"urban"` or `"rural"` based on
#' the `urban` column; for all other countries the level is `"national"`.
#'
#' @param df A data frame containing columns `code` (character ISO3) and
#'   `urban` (integer or logical; 1/TRUE = urban, 0/FALSE = rural).
#'
#' @return `df` with an additional `data_level` column (character).
#'
#' @export
assign_data_level <- function(df) {
  df |>
    dplyr::mutate(
      data_level = dplyr::case_when(
        code == "CHN" & urban == 1 ~ "urban",
        code == "CHN" & urban == 0 ~ "rural",
        .default = "national"
      )
    )
}


# ---------------------------------------------------------------------------- #
# LCU variable identification                                                   #
# ---------------------------------------------------------------------------- #

#' Identify LCU monetary variables present in a data frame
#'
#' Filters a variable list to variables flagged as Local Currency Unit (LCU)
#' that are also present as columns in `df`. These variables should be
#' deflated to 2021 PPP before analysis.
#'
#' @param df A data frame (survey microdata).
#' @param variable_list A data frame with at minimum columns `name` (character)
#'   and `units` (character). Rows with `units == "LCU"` are candidates.
#'
#' @return A character vector of variable names that are both LCU and present
#'   in `df`. Returns `character(0)` when none qualify.
#'
#' @export
get_lcu_vars <- function(df, variable_list) {
  variable_list |>
    dplyr::filter(units == "LCU", name %in% colnames(df)) |>
    dplyr::pull(name)
}


# ---------------------------------------------------------------------------- #
# LCU -> PPP conversion                                                         #
# ---------------------------------------------------------------------------- #

#' Convert LCU monetary variables to 2021 PPP
#'
#' Left-joins the CPI/PPP deflator table to `df` (matching on `code`, `year`,
#' and `data_level`) then divides each LCU variable by `cpi * ppp2021`.
#' `assign_data_level()` must be called on `df` before this function.
#'
#' @param df A data frame with columns `code`, `year`, and `data_level`
#'   (as produced by `assign_data_level()`), plus any columns named in
#'   `lcu_vars`.
#' @param cpi_ppp_data A data frame of CPI/PPP deflators with columns `code`,
#'   `year`, `data_level`, `cpi`, and `ppp2021`.
#' @param lcu_vars A character vector of column names in `df` to convert.
#'   If `character(0)` `df` is returned unchanged.
#'
#' @return `df` with the `lcu_vars` columns rescaled to 2021 PPP and the
#'   deflator join columns (`cpi`, `ppp2021`) attached.
#'
#' @export
convert_lcu_to_ppp <- function(df, cpi_ppp_data, lcu_vars) {
  if (length(lcu_vars) == 0) return(df)
  df |>
    dplyr::left_join(cpi_ppp_data, by = c("code", "year", "data_level")) |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(lcu_vars), ~ .x / cpi / ppp2021)
    )
}

# ---------------------------------------------------------------------------- #
# Bottom code welfare (2021 PPP)                                          #
# ---------------------------------------------------------------------------- #

#' Bottom code welfare (2021 PPP)
#'
#' Replaces any welfare values below `floor_value` $/person/day (2021 PPP) with `floor_value`.
#'
#' @param df A data frame with column `welfare`
#' @param floor_value A numeric value representing the minimum welfare value in $/person/day at 2021 PPP. 
#'
#' @return `df` with the 
#'
#' @export
bottom_code_welfare <- function(df, floor_value = 0.28) {
  if ("welfare" %in% colnames(df)) {
    df |>
      dplyr::mutate(
        welfare = ifelse(welfare < floor_value, floor_value, welfare)
      )
  } else {
    return(df)
  }
}

# ---------------------------------------------------------------------------- #
# Interview date summary                                                        #
# ---------------------------------------------------------------------------- #

#' Summarise interview dates for the timing-of-interviews bar chart
#'
#' Groups survey microdata by `economy`, `countryyear`, and a year-month floor
#' date, counting the number of household records (`hh`) in each month. Rows
#' with missing timestamps are dropped before aggregating.
#'
#' @param df A data frame with columns `economy` (character), `countryyear`
#'   (character), and `timestamp` (Date), as produced by `add_time_columns()`.
#'
#' @return A data frame with columns `economy`, `countryyear`, `month`
#'   (Date, first of month), and `hh` (integer count). Returns a zero-row
#'   data frame when `df` is empty or all timestamps are `NA`.
#'
#' @export
summarise_interview_dates <- function(df) {
  df |>
    dplyr::filter(!is.na(timestamp)) |>
    dplyr::mutate(
      month_num = as.integer(format(timestamp, "%m"))
    ) |>
    dplyr::summarise(
      hh = dplyr::n(),
      .by = c(economy, countryyear, month_num)
    )
}


# ---------------------------------------------------------------------------- #
# Welfare poverty lines                                                         #
# ---------------------------------------------------------------------------- #

#' Standard welfare poverty line thresholds (2021 PPP)
#'
#' Returns the three World Bank international poverty lines expressed in
#' USD per day at 2021 PPP prices, along with display labels. Used to add
#' reference lines to the welfare distribution plot.
#'
#' @return A data frame with columns `value` (numeric, $/day 2021 PPP) and
#'   `label` (character, formatted label for annotation).
#'
#' @export
welfare_poverty_lines <- function() {
  data.frame(
    value = c(3.00, 4.20, 8.30),
    label = c("$3.00", "$4.20", "$8.30"),
    stringsAsFactors = FALSE
  )
}


# ---------------------------------------------------------------------------- #
# Interview date bar chart                                                      #
# ---------------------------------------------------------------------------- #

#' Plot timing of survey interviews as a monthly bar chart, faceted by wave
#'
#' @param plot_data A data frame with columns `month` (Date, first of month),
#'   `hh` (integer count), `economy` (character), and `countryyear`
#'   (character), as returned by `summarise_interview_dates()`.
#'
#' @return A `ggplot` object, or `NULL` invisibly when `plot_data` is
#'   `NULL` or has zero rows.
#'
#' @importFrom ggplot2 ggplot aes geom_col facet_wrap scale_x_date labs
#'   theme_minimal theme element_text
#' @export
plot_interview_dates <- function(plot_data) {
  if (is.null(plot_data) || nrow(plot_data) == 0) return(invisible(NULL))

  month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  plot_data$month_fct <- factor(
    plot_data$month_num,
    levels = 1:12,
    labels = month_labels
  )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data$month_fct,
      y = .data$hh,
      fill = .data$countryyear
    )
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    theme_wise() +
    ggplot2::labs(
      x = NULL, y = "Households", fill = "Survey wave"
    ) +
    ggplot2::theme(
      axis.text.x        = ggplot2::element_text(
        angle = 0, hjust = 0.5, size = 9
      ),
      axis.ticks.x       = ggplot2::element_line(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      legend.position    = "bottom"
    )
}

# ---------------------------------------------------------------------------- #
# Leaflet survey location map                                                  #
# ---------------------------------------------------------------------------- #

#' Extract bounding box from a GeoJSON FeatureCollection
#'
#' Iterates over all Polygon and MultiPolygon coordinate arrays to find the
#' overall lng/lat extent. No spatial libraries required.
#'
#' Features may carry either a parsed `geometry` or a raw `geom_json` (or
#' `geom`) geometry string. The raw string is parsed on demand here and
#' immediately discarded, so feature builders no longer need to retain a
#' parsed copy alongside the string (PERF-10).
#'
#' @param geojson A GeoJSON FeatureCollection list, as produced by
#'   `jsonlite::fromJSON` or assembled manually.
#' @return A named list with `lng1`, `lat1`, `lng2`, `lat2`.
#'
#' @noRd
.geojson_bounds <- function(geojson) {
  all_lng <- numeric()
  all_lat <- numeric()

  # Recursively extract all coordinate pairs from any nesting depth
  extract_coords <- function(coords) {
    if (is.numeric(coords) && length(coords) == 2) {
      # A single [lng, lat] pair
      all_lng <<- c(all_lng, coords[1])
      all_lat <<- c(all_lat, coords[2])
    } else if (is.array(coords) && length(dim(coords)) >= 2L &&
               utils::tail(dim(coords), 1L) == 2L) {
      # jsonlite collapses coordinate nesting into an array whenever the rings
      # are regular: n x 2 (matrix) for a Polygon with one ring, 1 x n x 2 for
      # the same geometry read straight from GeoJSON, and deeper for
      # MultiPolygons. The lng/lat pair is always the last dimension, and R
      # stores it column-major, so the first half of the values are lng and
      # the second half lat regardless of the leading dimensions.
      flat <- matrix(as.numeric(coords), ncol = 2L)
      all_lng <<- c(all_lng, flat[, 1L])
      all_lat <<- c(all_lat, flat[, 2L])
    } else if (is.list(coords)) {
      lapply(coords, extract_coords)
    }
  }

  for (f in geojson$features) {
    coords <- f$geometry$coordinates
    if (is.null(coords)) {
      geom_str <- f$geom_json %||% f$geom
      if (is.null(geom_str)) next
      coords <- tryCatch(
        jsonlite::fromJSON(geom_str)$coordinates,
        error = function(e) NULL
      )
      if (is.null(coords)) next
    }
    extract_coords(coords)
  }

  list(
    lng1 = min(all_lng), lat1 = min(all_lat),
    lng2 = max(all_lng), lat2 = max(all_lat)
  )
}





#' Survey waves present in a data frame, for a wave picker
#'
#' @param df Any frame carrying `code`, `year` and `survname` (and ideally
#'   `economy` for the label).
#'
#' @return A data frame with `code`, `year`, `survname`, `key`
#'   (`"code|year|survname"`) and `label`, ordered by label. `NULL` when the
#'   keys are absent.
#'
#' @export
survey_wave_list <- function(df) {
  keys <- c("code", "year", "survname")
  if (is.null(df) || !all(keys %in% names(df))) return(NULL)

  d <- df
  d$year <- as.character(d$year)
  if (!"economy" %in% names(d)) d$economy <- d$code

  w <- unique(d[, c(keys, "economy")])
  if (nrow(w) == 0) return(NULL)

  w$key   <- paste(w$code, w$year, w$survname, sep = "|")
  w$label <- paste0(w$economy, ", ", w$year)
  w <- w[order(w$label), , drop = FALSE]
  rownames(w) <- NULL
  w
}


#' Keep only the features belonging to one survey wave
#'
#' @param geojson A FeatureCollection whose features carry `code`, `year` and
#'   `survname` properties.
#' @param key A `"code|year|survname"` string, or `"all"` to keep everything.
#'
#' @return The filtered FeatureCollection.
#'
#' @export
filter_features_by_wave <- function(geojson, key = "all") {
  if (is.null(geojson) || is.null(key) || identical(key, "all")) return(geojson)
  if (length(geojson$features) == 0) return(geojson)

  keep <- vapply(geojson$features, function(f) {
    p <- f$properties
    identical(paste(as.character(p$code), as.character(p$year),
                    as.character(p$survname), sep = "|"), key)
  }, logical(1))

  geojson$features <- geojson$features[keep]
  geojson
}


#' Restrict a data frame to one survey wave
#'
#' @param df  A frame with `code`, `year`, `survname`.
#' @param key A `"code|year|survname"` string, or `"all"`.
#'
#' @return The filtered frame.
#'
#' @export
filter_by_wave <- function(df, key = "all") {
  keys <- c("code", "year", "survname")
  if (is.null(df) || identical(key, "all") || is.null(key)) return(df)
  if (!all(keys %in% names(df))) return(df)
  k <- paste(df$code, as.character(df$year), df$survname, sep = "|")
  df[k == key, , drop = FALSE]
}


#' Reset-view control
#'
#' A third button under Leaflet's zoom controls that returns the map to the
#' extent it opened at, so panning and zooming is always recoverable.
#'
#' @param m      A `leaflet` widget.
#' @param bounds List with `lng1`, `lat1`, `lng2`, `lat2`.
#'
#' @return `m` with the button added (unchanged if `bounds` is not finite).
#' @noRd
.add_reset_button <- function(m, bounds) {
  if (!all(vapply(bounds, is.finite, logical(1)))) return(m)

  js <- sprintf(
    "function(btn, map) { map.fitBounds([[%s, %s], [%s, %s]]); }",
    format(bounds$lat1, digits = 10), format(bounds$lng1, digits = 10),
    format(bounds$lat2, digits = 10), format(bounds$lng2, digits = 10)
  )

  m |>
    leaflet::addEasyButton(
      leaflet::easyButton(
        position = "topleft",
        icon     = htmltools::HTML(
          '<span style="font-size: 15px; line-height: 26px;">&#9678;</span>'
        ),
        title   = "Reset view",
        onClick = htmlwidgets::JS(js)
      )
    )
}


#' Remember and restore a leaflet map's view across re-renders
#'
#' Switching a map's view (or reloading its data) rebuilds the widget, which
#' would otherwise snap back to the data bounds and lose wherever the user had
#' panned and zoomed to. This records the view Leaflet reports through Shiny
#' and reapplies it to the next build of the same output.
#'
#' @param input,session Shiny `input` and `session` objects.
#' @param output_id Character. The `outputId` of the leaflet output, without
#'   the module namespace.
#'
#' @return A list with `remember()` - call once to start tracking - and
#'   `restore(m)`, which applies the stored view to a widget.
#'
#' @noRd
map_view_memory <- function(input, session, output_id) {
  stored <- shiny::reactiveVal(NULL)

  list(
    remember = function() {
      shiny::observeEvent(
        list(input[[paste0(output_id, "_center")]],
             input[[paste0(output_id, "_zoom")]]),
        {
          ctr <- input[[paste0(output_id, "_center")]]
          zm  <- input[[paste0(output_id, "_zoom")]]
          if (!is.null(ctr) && !is.null(zm) &&
              is.finite(ctr$lng) && is.finite(ctr$lat)) {
            stored(list(lng = ctr$lng, lat = ctr$lat, zoom = zm))
          }
        },
        ignoreInit = TRUE, ignoreNULL = TRUE
      )
    },
    restore = function(m) {
      v <- shiny::isolate(stored())
      if (is.null(m) || is.null(v)) return(m)
      m$x$fitOnResize <- FALSE
      leaflet::setView(m, lng = v$lng, lat = v$lat, zoom = v$zoom)
    },
    get = stored
  )
}

#' JS hook keeping a map correct when its container resizes
#'
#' Leaflet caches its container size, so a map whose card is expanded to full
#' screen keeps drawing at the old size (grey gutters, controls in the wrong
#' place) until something nudges it. This watches the container and
#' re-measures.
#'
#' Re-measuring alone keeps the old zoom, which leaves the locations marooned
#' in the middle of a much bigger map, so the data bounds are re-fitted too -
#' expanding then magnifies the sample rather than its surroundings. Reading
#' the pre-resize bounds instead would not work: Leaflet has usually
#' re-measured by the time the callback runs, so they already describe the new
#' size. Once the user has panned or zoomed, their view is left alone.
#'
#' With `dashes = TRUE` the stroke width of dashed sub-layers is also tapered
#' as the map zooms out - stroke width is in screen pixels, so an outline that
#' reads well when zoomed in swamps the shapes once they shrink.
#'
#' @param bounds List with `lng1`, `lat1`, `lng2`, `lat2`, from
#'   `.geojson_bounds()`.
#' @param dashes Logical. Also scale dashed stroke widths with zoom.
#'
#' @return An `htmlwidgets::JS()` string for `htmlwidgets::onRender()`.
#' @noRd
.map_autofit_js <- function(bounds, dashes = FALSE) {
  fit_ok <- all(vapply(bounds, is.finite, logical(1)))

  # `dashes` is retained for callers that once drew per-area markers; there
  # are none now, so the hook is a no-op.
  dash_fn <- "
        var scaleDashes = function() {};
  "

  htmlwidgets::JS(sprintf("
      function(el, x) {
        var map = this;
        // The caller can suppress the fit when it has restored a remembered
        // view - refitting would throw that view away on the first resize.
        var fit = %s && (x.fitOnResize !== false);
        var b   = [[%s, %s], [%s, %s]];
        var userMoved = false;
        var ro;
        ['mousedown', 'wheel', 'dblclick', 'touchstart'].forEach(function(ev) {
          el.addEventListener(ev, function() { userMoved = true; },
                              { passive: true });
        });
        // Expanding a card usually changes width far more than height, so a
        // whole zoom level rarely fits and integer snapping would leave the
        // sample as small as before. Quarter steps let the re-fit actually
        // use the new space; the +/- buttons still step by whole levels.
        map.options.zoomSnap = 0.25;
        %s
        // Re-rendering the output (switching view, reloading data) destroys
        // this map while the observer on its container is still live, so bail
        // out once the map is gone rather than reaching into dead panes.
        var alive = function() {
          // el.isConnected catches the window between Shiny detaching the old
          // output node and Leaflet firing 'unload'; touching the map in that
          // window schedules a redraw against a canvas that no longer exists.
          return el.isConnected !== false && map._loaded &&
                 map._container && map._container.parentNode;
        };
        var refit = function() {
          if (!alive()) { if (ro) ro.disconnect(); return; }
          window.requestAnimationFrame(function() {
            if (!el.offsetWidth || !el.offsetHeight || !alive()) return;
            try {
              map.invalidateSize();
              if (fit && !userMoved) map.fitBounds(b, { animate: false });
              scaleDashes();
            } catch (err) { /* map torn down mid-resize */ }
          });
        };
        if (window.ResizeObserver) {
          ro = new ResizeObserver(refit);
          ro.observe(el);
        } else {
          window.addEventListener('resize', refit);
        }
        map.on('unload', function() {
          if (ro) ro.disconnect();
          // Leaflet leaves a queued canvas redraw behind when a map is
          // replaced (switching view rebuilds the widget). It then runs
          // against a destroyed 2d context and throws. Cancel it.
          try {
            var rs = map._renderer ? [map._renderer] : [];
            map.eachLayer(function(l) {
              if (l._renderer && rs.indexOf(l._renderer) < 0) rs.push(l._renderer);
            });
            rs.forEach(function(r) {
              if (r._redrawRequest && window.cancelAnimationFrame) {
                window.cancelAnimationFrame(r._redrawRequest);
                r._redrawRequest = null;
              }
            });
          } catch (err) { /* nothing queued */ }
        });
      }",
    if (fit_ok) "true" else "false",
    format(bounds$lat1, digits = 10), format(bounds$lng1, digits = 10),
    format(bounds$lat2, digits = 10), format(bounds$lng2, digits = 10),
    dash_fn
  ))
}


#' Build a leaflet map of survey interview locations
#'
#' Renders a `leaflet` widget from a GeoJSON FeatureCollection of H3-aggregated
#' survey locations. Polygons are coloured by economy code and the viewport is
#' fitted to the bounding box. Accepts both Polygon and MultiPolygon geometries.
#'
#' @param loc A GeoJSON FeatureCollection list with a `code` property on each
#'   feature, as produced by the H3-to-polygon aggregation step in
#'   `mod_1_02_surveystats_server()`.
#'
#' @return A `leaflet` widget, or `NULL` invisibly when `loc` is `NULL` or
#'   has no features.
#'
#' @export
plot_survey_map <- function(loc) {
  if (is.null(loc) || length(loc$features) == 0) return(invisible(NULL))

  codes      <- sapply(loc$features, function(f) f$properties$code)
  u_codes    <- unique(codes)
  code_color <- setNames(scales::hue_pal()(length(u_codes)), u_codes)
  bounds     <- .geojson_bounds(loc)

  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)

  # One addGeoJSON call per code so each gets a static colour.
  # Geometry is embedded as a raw JSON string to avoid a jsonlite named-vector
  # warning that fires when R matrices (fromJSON default) are re-serialised.
  for (cd in u_codes) {
    features_for_code <- loc$features[codes == cd]
    features_json <- vapply(features_for_code, function(f) {
      sprintf('{"type":"Feature","geometry":%s,"properties":%s}',
              f$geom_json,
              jsonlite::toJSON(f$properties, auto_unbox = TRUE))
    }, character(1L))
    sub_geojson_str <- sprintf('{"type":"FeatureCollection","features":[%s]}',
                               paste(features_json, collapse = ","))

    m <- m |>
      leaflet::addGeoJSON(
        geojson     = sub_geojson_str,
        color       = code_color[[cd]],
        opacity     = 0.5,
        fillOpacity = 0,
        weight      = 1
      )
  }

  # Hover highlight via vanilla Leaflet JS.
  #
  # The un-highlight uses Leaflet's own `resetStyle()` rather than style values
  # captured in JS variables at render time. Captured values are wrong (or
  # undefined) for any sub-layer Leaflet builds as a group instead of a single
  # path, and writing an undefined colour/weight back makes the stroke invalid.
  # These polygons are drawn with `fillOpacity = 0`, so a broken stroke means
  # the shape vanishes completely and stays gone - every later mouseout writes
  # the same invalid style again.
  #
  # Handlers are bound on the GeoJSON layer itself, not on each sub-layer:
  # L.GeoJSON is a FeatureGroup, so child events propagate up, and the binding
  # then also covers sub-layers regardless of the geometry type they came from.
  hover_js <- htmlwidgets::JS("
    function(el, x) {
      this.eachLayer(function(layer) {
        if (typeof layer.resetStyle !== 'function') return;  // not GeoJSON

        layer.on('mouseover', function(e) {
          var target = e.propagatedFrom || e.layer;
          if (target && target.setStyle) {
            target.setStyle({ weight: 2, color: '#FF0000' });
            if (target.bringToFront) target.bringToFront();
          }
        });

        layer.on('mouseout', function(e) {
          var target = e.propagatedFrom || e.layer;
          if (target) layer.resetStyle(target);
        });
      });
    }
  ")

  m |>
    leaflet::fitBounds(
      lng1 = bounds$lng1, lat1 = bounds$lat1,
      lng2 = bounds$lng2, lat2 = bounds$lat2
    ) |>
    .add_reset_button(bounds) |>
    htmlwidgets::onRender(hover_js) |>
    htmlwidgets::onRender(.map_autofit_js(bounds))
}


# ---------------------------------------------------------------------------- #
# Sample density heatmap                                                        #
# ---------------------------------------------------------------------------- #

#' Spread each location's sampled units across the H3 cells it covers
#'
#' Survey locations overlap - in a capital, dozens of `loc_id`s can cover the
#' same ground - so a map of outlined location polygons says little about how
#' many households sit where. H3 cells, by contrast, tile the plane without
#' gaps or overlaps, which makes them the natural unit for a heatmap.
#'
#' A location's units are divided evenly over the cells it covers, and cells
#' are then summed across locations, so a cell reached by several overlapping
#' locations accumulates a share from each. The result is smooth and the
#' totals reconcile with the sample exactly. Values are therefore fractional:
#' a location of a dozen households spread over two dozen cells contributes
#' half a household to each, and only the accumulation across overlapping
#' locations builds that back up.
#'
#' @param cell_map    Data frame with one row per location-cell pair: `code`,
#'   `year`, `survname`, `loc_id`, `h3`.
#' @param survey_data Loaded survey observations, one row per sampled unit,
#'   with `code`, `year`, `survname` and `loc_id`.
#'
#' @return A data frame with one row per `h3` cell holding a positive share:
#'   `h3` and `n_units`. `NULL` when the inputs cannot be combined.
#'
#' @export
allocate_units_to_cells <- function(cell_map, survey_data) {
  keys <- c("code", "year", "survname", "loc_id")
  if (is.null(cell_map) || is.null(survey_data)) return(NULL)
  if (!all(c(keys, "h3") %in% names(cell_map))) return(NULL)
  if (!all(keys %in% names(survey_data))) return(NULL)

  cm <- cell_map
  cm$year <- as.character(cm$year)

  sd <- survey_data
  sd$year <- as.character(sd$year)

  # Sampled units per location.
  n_loc <- sd |>
    dplyr::count(.data$code, .data$year, .data$survname, .data$loc_id,
                 name = "n_units")

  cm <- cm |>
    dplyr::inner_join(n_loc, by = keys)
  if (nrow(cm) == 0) return(NULL)

  cm |>
    dplyr::group_by(.data$code, .data$year, .data$survname, .data$loc_id) |>
    dplyr::mutate(.alloc = .data$n_units / dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$h3) |>
    dplyr::summarise(n_units = sum(.data$.alloc, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::filter(.data$n_units > 0) |>
    as.data.frame()
}


#' Merge per-location values onto the H3 cells they cover
#'
#' Survey locations overlap: several `loc_id`s can cover the same ground, so
#' drawing one semi-transparent polygon per location stacks fills on top of
#' each other and invents colours that are in neither the data nor the legend.
#' Aggregating onto H3 cells - which tile without overlapping - gives one
#' unambiguous value per patch of ground.
#'
#' Each contributing location is weighted by how many of its sampled units sit
#' in that cell (its unit count times the cell's share of the location's
#' `pop_2020`). Continuous values are averaged with those weights; categorical
#' values (weather bins) take the highest-weight category.
#'
#' @param cell_map Data frame with one row per location-cell pair: `code`,
#'   `year`, `survname`, `loc_id`, `h3`, `pop_2020`.
#' @param loc_vals Data frame with one row per location: `code`, `year`,
#'   `survname`, `loc_id`, `value` and `n_hh`, optionally `n_months`. A
#'   `binned` attribute marks `value` as categorical.
#'
#' @param by_wave Keep waves separate (the default), giving one row per wave
#'   and cell. Set `FALSE` to pool every selected wave into a single value per
#'   cell - needed wherever all waves are drawn on one map, since a cell
#'   sampled by two waves would otherwise be painted twice.
#'
#' @return A data frame with one row per wave-cell: `code`, `year`,
#'   `survname`, `loc_id` (the H3 index, so the result drops straight into the
#'   location-map plotters), `value`, `n_hh`, `n_months` and `n_locs`. Carries
#'   the `binned` and `levels` attributes of `loc_vals`. `NULL` when the
#'   inputs cannot be combined.
#'
#' @export
merge_loc_values_to_cells <- function(cell_map, loc_vals, by_wave = TRUE) {
  keys <- c("code", "year", "survname", "loc_id")
  if (is.null(cell_map) || is.null(loc_vals) || nrow(loc_vals) == 0) return(NULL)
  if (!all(c(keys, "h3") %in% names(cell_map))) return(NULL)
  if (!all(c(keys, "value") %in% names(loc_vals))) return(NULL)

  binned <- isTRUE(attr(loc_vals, "binned"))
  lvls   <- attr(loc_vals, "levels")

  cm <- cell_map
  cm$year <- as.character(cm$year)
  if (!"pop_2020" %in% names(cm)) cm$pop_2020 <- 1
  cm$pop_2020[!is.finite(cm$pop_2020) | cm$pop_2020 < 0] <- 0

  lv <- loc_vals
  lv$year <- as.character(lv$year)
  if (!"n_hh" %in% names(lv))     lv$n_hh <- 1
  if (!"n_months" %in% names(lv)) lv$n_months <- 1L

  carry <- intersect(c("economy"), names(lv))
  j <- cm |>
    dplyr::inner_join(
      lv[, c(keys, "value", "n_hh", "n_months", carry)], by = keys
    )
  if (nrow(j) == 0) return(NULL)

  # Units of this location that sit in this cell.
  j <- j |>
    dplyr::group_by(.data$code, .data$year, .data$survname, .data$loc_id) |>
    dplyr::mutate(
      .tot = sum(.data$pop_2020, na.rm = TRUE),
      .w   = .data$n_hh * dplyr::if_else(.data$.tot > 0,
                                         .data$pop_2020 / .data$.tot,
                                         1 / dplyr::n())
    ) |>
    dplyr::ungroup()

  grp <- if (by_wave) {
    c("code", "year", "survname", "h3")
  } else {
    "h3"
  }

  # `split()` dropped rows with missing grouping keys, so they are removed
  # before the grouping is built (PERF-05).
  j <- j[complete.cases(j[grp]), , drop = FALSE]
  if (nrow(j) == 0) return(NULL)

  g <- collapse::GRP(j, by = grp)
  n_g <- g$N.groups
  first_idx <- match(seq_len(n_g), g$group.id)
  w <- j$.w

  out <- data.frame(
    code     = j$code[first_idx],
    year     = j$year[first_idx],
    survname = j$survname[first_idx],
    loc_id   = j$h3[first_idx],
    value    = if (binned) rep(NA_character_, n_g) else rep(NA_real_, n_g),
    # The colour summarises more than one number when several locations
    # meet here, or when a contributing location spans several months.
    n_hh     = { nh <- collapse::fsum(w, g = g, na.rm = TRUE); nh[is.na(nh)] <- 0; nh },
    n_months = collapse::fmax(j$n_months, g = g, na.rm = TRUE),
    n_locs   = as.integer(collapse::fndistinct(j$loc_id, g = g, na.rm = FALSE)),
    stringsAsFactors = FALSE
  )
  if (length(carry)) out$economy <- j$economy[first_idx]

  if (binned) {
    # Modal bin per cell by summed weight; ties broken by the alphabetical
    # order `tapply()` used before, and NA values carry no vote (PERF-05).
    vv <- as.character(j$value)
    ok <- !is.na(vv)
    if (any(ok)) {
      gv <- collapse::GRP(
        list(gid = g$group.id[ok], value = vv[ok]),
        group.sizes = TRUE
      )
      wsum <- collapse::fsum(w[ok], g = gv, na.rm = TRUE)
      wsum[is.na(wsum)] <- 0
      ord  <- order(gv$groups$gid, -wsum, match(gv$groups$value, sort(unique(vv[ok]))))
      take <- ord[!duplicated(gv$groups$gid[ord])]
      out$value[gv$groups$gid[take]] <- gv$groups$value[take]
    }
  } else {
    vn <- suppressWarnings(as.numeric(j$value))
    ok <- is.finite(vn) & is.finite(w) & (w > 0)
    # Invalid rows are NA-ed out of both value and weight so the grouped mean
    # skips them, as the old per-cell mask did.
    vn2 <- vn; w2 <- w
    vn2[!ok] <- NA_real_
    w2[!ok] <- NA_real_
    val <- suppressWarnings(collapse::fmean(vn2, g = g, w = w2, na.rm = TRUE))
    val[is.nan(val)] <- NA_real_
    out$value <- unname(val)
  }

  # `interaction()` ordered its levels with `code` varying fastest (by_wave)
  # and `factor(h3)` sorted alphabetically otherwise; restore that order.
  out <- if (by_wave) {
    out[order(out$loc_id, out$survname, out$year, out$code), ]
  } else {
    out[order(out$loc_id), ]
  }
  rownames(out) <- NULL
  attr(out, "binned") <- binned
  attr(out, "levels") <- lvls
  out
}


#' GeoJSON features for H3 cells, tagged with their survey wave
#'
#' Produces features whose `loc_id` property is the H3 index, so cell-level
#' values from `merge_loc_values_to_cells()` can be drawn by the same plotters
#' that draw location polygons.
#'
#' @param cell_geo Data frame with `h3` and `geom` (a GeoJSON geometry
#'   string).
#' @param cell_map Data frame with `code`, `year`, `survname` and `h3`.
#' @param by_wave Tag features with their wave (the default), so per-wave maps
#'   can filter to their own. Set `FALSE` to emit each cell exactly once -
#'   maps that draw all waves together must not paint a shared cell twice.
#'
#' @return A GeoJSON FeatureCollection list, or `NULL`.
#'
#' @export
build_cell_features <- function(cell_geo, cell_map, by_wave = TRUE) {
  if (is.null(cell_geo) || is.null(cell_map)) return(NULL)
  if (!all(c("h3", "geom") %in% names(cell_geo))) return(NULL)
  if (!all(c("code", "year", "survname", "h3") %in% names(cell_map))) return(NULL)

  waves <- if (by_wave) {
    cell_map |>
      dplyr::distinct(.data$code, .data$year, .data$survname, .data$h3)
  } else {
    cell_map |>
      dplyr::distinct(.data$h3) |>
      dplyr::mutate(code = NA_character_, year = NA_character_,
                    survname = NA_character_)
  }
  waves$year <- as.character(waves$year)

  d <- waves |>
    dplyr::inner_join(cell_geo, by = "h3") |>
    dplyr::filter(!is.na(.data$geom), nchar(.data$geom) > 2)
  if (nrow(d) == 0) return(NULL)

  features <- lapply(seq_len(nrow(d)), function(i) {
    list(
      type      = "Feature",
      geom_json = d$geom[i],
      properties = list(
        code     = d$code[i],
        year     = d$year[i],
        survname = d$survname[i],
        loc_id   = d$h3[i]
      )
    )
  })

  list(type = "FeatureCollection", features = features)
}


#' Heatmap of sampled units per H3 cell
#'
#' Draws one filled hexagon per cell, shaded by the number of sampled units
#' allocated to it by `allocate_units_to_cells()`. Cells tile the plane, so
#' unlike the location-outline map nothing overlaps and dense areas read
#' directly off the colour.
#'
#' @param cells     Data frame with `h3`, `geom` (a GeoJSON geometry string)
#'   and `n_units`.
#' @param unit_label Plural noun for what a row of the survey is, e.g.
#'   `"households"`.
#'
#' @return A `leaflet` widget, or `NULL` invisibly when there is nothing to
#'   draw.
#'
#' @export
plot_sample_density_map <- function(cells, unit_label = "households") {
  if (is.null(cells) || nrow(cells) == 0) return(invisible(NULL))
  if (!all(c("geom", "n_units") %in% names(cells))) return(invisible(NULL))

  cells <- cells[!is.na(cells$geom) & nchar(cells$geom) > 2 &
                   is.finite(cells$n_units), , drop = FALSE]
  if (nrow(cells) == 0) return(invisible(NULL))

  # Counts are heavily skewed - a handful of urban cells dwarf everything - so
  # the colour scale runs on log, over the range actually present. Shares are
  # fractional where a location's units are split across its cells, so the
  # bottom of the ramp can sit below one.
  pos <- cells$n_units[cells$n_units > 0]
  rng <- range(pos, na.rm = TRUE)
  if (!all(is.finite(rng)) || rng[1] <= 0) rng <- c(0.5, 1)
  if (diff(rng) <= 0) rng <- c(rng[1], rng[1] * 2)
  # Mako (viridis family: perceptually uniform, colour-blind safe, and still
  # readable in greyscale) running light to dark, with the palest stops
  # trimmed off - near-white yellows and creams disappear against the light
  # basemap, which is what made the old Inferno ramp hard to read at the
  # bottom end.
  ramp <- rev(grDevices::hcl.colors(12, "Mako"))[3:11]
  pal  <- leaflet::colorNumeric(ramp, domain = log(rng), na.color = "#cccccc")
  col_of <- function(v) pal(log(pmin(pmax(v, rng[1]), rng[2])))

  feats <- lapply(seq_len(nrow(cells)), function(i) {
    list(
      geom_json = cells$geom[i],
      props     = list(
        h3    = cells$h3[i] %||% "",
        popup = paste0(
          "<b>", format(round(cells$n_units[i], 1), big.mark = ","), " ",
          htmltools::htmlEscape(unit_label), "</b><br/><small>area ",
          htmltools::htmlEscape(cells$h3[i] %||% ""), "</small>"
        )
      )
    )
  })

  bounds <- .geojson_bounds(list(features = feats))
  cols   <- col_of(cells$n_units)

  # One addGeoJSON call carrying a per-feature style, rather than one call per
  # distinct colour. With a continuous ramp almost every cell has its own
  # colour, so grouping by colour would create a thousand separate Leaflet
  # layers - enough event and pane bookkeeping to make dragging stutter.
  fj <- vapply(seq_along(feats), function(i) {
    props <- feats[[i]]$props
    props$style <- list(fillColor = cols[i], fillOpacity = 0.75,
                        stroke = FALSE, weight = 0)
    sprintf('{"type":"Feature","geometry":%s,"properties":%s}',
            feats[[i]]$geom_json, jsonlite::toJSON(props, auto_unbox = TRUE))
  }, character(1L))

  # Canvas rather than SVG: a thousand-plus filled hexagons as individual SVG
  # paths makes dragging stutter, because the browser re-rasterises every one.
  # Canvas draws them in a single pass and pans smoothly; popups still work.
  m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::addGeoJSON(
      geojson     = sprintf('{"type":"FeatureCollection","features":[%s]}',
                            paste(fj, collapse = ",")),
      stroke      = FALSE,
      weight      = 0,
      fillOpacity = 0.75
    )

  # The legend ramp samples this palette across the domain, so both are given
  # on the count scale; `col_of()` applies the log internally. `mid` is the
  # count that actually lands halfway along the ramp.
  pal_info <- list(
    pal    = function(v) col_of(v),
    domain = rng,
    mid    = exp(mean(log(rng))),
    # Evenly spaced along the log ramp, so the legend bar shows the same
    # colour progression the cells do.
    stops  = exp(seq(log(rng[1]), log(rng[2]), length.out = 9))
  )

  m |>
    leaflet::fitBounds(
      lng1 = bounds$lng1, lat1 = bounds$lat1,
      lng2 = bounds$lng2, lat2 = bounds$lat2
    ) |>
    .add_reset_button(bounds) |>
    htmlwidgets::onRender(.map_autofit_js(bounds)) |>
    leaflet::addControl(
      position = "bottomright",
      html = .compact_legend_html(
        pal_info = pal_info,
        binned   = FALSE,
        title    = paste0(.capitalise(unit_label), " per area"),
        info     = paste0(
          "Number of sampled ", unit_label, " in each hexagon. Every ",
          sub("s$", "", unit_label), " is placed in exactly one hexagon: a",
          " location's ", unit_label, " are assigned across the cells it",
          " covers in proportion to their 2020 population, then rolled up to",
          " this display grid - so an area covered by many overlapping survey",
          " locations accumulates all of them, and the totals match the",
          " sample exactly. Colour runs on a log scale because a handful of",
          " urban areas would otherwise flatten everything else."
        )
      )
    )
}


# Upper-case the first letter only; `toupper()` on the whole word would shout.
#' @noRd
.capitalise <- function(x) {
  if (!nzchar(x %||% "")) return(x)
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}


# ---------------------------------------------------------------------------- #
# Summary stats tables                                                         #
# ---------------------------------------------------------------------------- #

#' Build a formatted DT summary table by variable group flag
#'
#' Creates a `DT::renderDT()` expression for survey summary statistics of the
#' variables flagged in `variable_list[[flag_col]] == 1` and present in
#' `survey_data()`.
#'
#' The returned table includes:
#' \itemize{
#'   \item Weighted summary statistics from `weighted_summary_long()`
#'   \item Wave-specific missingness (`% Missing`) by `countryyear` and variable
#'   \item Readable variable labels (from `variable_list`) shown in a single
#'     `Variable` column, falling back to the raw name when no label exists
#'   \item Standardized column names (capitalized first letter)
#'   \item Basic display formatting (numeric columns to 2 decimals except `N`)
#'   \item Soft text wrapping for long character/factor fields
#' }
#'
#' @param survey_data A reactive expression returning a survey `data.frame`.
#'   The data should include `countryyear` when wave-specific missingness is
#'   required.
#' @param variable_list A reactive expression or `data.frame` containing at least
#'   columns `name`, `label`, and the grouping flag column given in `flag_col`.
#' @param flag_col Character scalar naming the grouping flag column in
#'   `variable_list` (e.g., `"outcome"`, `"ind"`, `"hh"`, `"firm"`, `"area"`).
#'   Ignored if `vars` is supplied.
#' @param vars Optional character vector of variable names to summarise. When
#'   supplied, takes precedence over `flag_col`.
#'
#' @return A `shiny.render.function` (from `DT::renderDT`) that renders the
#'   formatted summary statistics table.
#' @export
make_stats_dt <- function(survey_data, variable_list, flag_col = NULL, vars = NULL) {
  DT::renderDT({
    shiny::req(survey_data())
    df <- survey_data()
    vl <- if (is.function(variable_list)) variable_list() else variable_list

    target <- if (!is.null(vars)) vars else vl$name[vl[[flag_col]] == 1]
    vars   <- intersect(target, names(df))
    if (length(vars) == 0) {
      tag <- flag_col %||% "specified"
      return(data.frame(Note = paste("No", tag, "variables found")))
    }

    tab <- weighted_summary_long(df, vars = vars)

    # Add missingness by survey wave (countryyear) and variable
    if ("variable" %in% names(tab) && "countryyear" %in% names(tab)) {
      if (!"countryyear" %in% names(df)) {
        stop("countryyear column is required in survey_data() to compute wave-specific missingness.")
      }

      # Wave-specific missingness by countryyear and variable, in one
      # grouped pass (PERF-09)
      fill_df <- survey_missingness_long(df, vars)

      tab <- tab |>
        dplyr::left_join(fill_df, by = c("countryyear", "variable"))
    }

    # Show only the readable variable label, falling back to the raw name
    if ("variable" %in% names(tab)) {
      lab_map <- vl[, c("name", "label"), drop = FALSE]
      tab <- tab |>
        dplyr::left_join(lab_map, by = c("variable" = "name")) |>
        dplyr::mutate(variable = dplyr::coalesce(.data$label, .data$variable)) |>
        dplyr::select(variable, dplyr::everything(), -dplyr::any_of("label"))
    }

    # Sort by variable label, then wave (countryyear) where available
    if (all(c("variable", "countryyear") %in% names(tab))) {
      tab <- tab |>
        dplyr::arrange(.data$variable, .data$countryyear)
    }

    # ---- Column renaming ----------------------------------------------------
    if ("countryyear" %in% names(tab))    names(tab)[names(tab) == "countryyear"]    <- "Country, Year"

    names(tab) <- vapply(names(tab), function(nm) {
      if (!nzchar(nm)) return(nm)
      paste0(toupper(substr(nm, 1, 1)), substr(nm, 2, nchar(nm)))
    }, character(1))

    wrap_width <- 28
    text_cols <- names(tab)[vapply(tab, function(x) is.character(x) || is.factor(x), logical(1))]
    if (length(text_cols) > 0) {
      tab[text_cols] <- lapply(tab[text_cols], function(x) {
        x_chr <- as.character(x)
        vapply(x_chr, function(s) {
          if (is.na(s)) return(NA_character_)
          # HTML-escape each wrapped line before joining with the literal
          # <br> markup below (the table is rendered with escape = FALSE,
          # so any unescaped data-derived text would render as raw HTML;
          # see SEC-05).
          lines <- strwrap(s, width = wrap_width)
          paste(htmltools::htmlEscape(lines), collapse = "<br>")
        }, character(1))
      })
    }

    dt <- DT::datatable(
      tab,
      rownames = FALSE,
      escape = FALSE,
      options = list(
        autoWidth = TRUE,
        pageLength = 10,
        columnDefs = list(list(className = "dt-wrap", targets = "_all"))
      )
    )

    # 2 decimals for numeric columns except N
    num_cols <- names(tab)[vapply(tab, is.numeric, logical(1))]
    num_cols <- setdiff(num_cols, "N")
    if (length(num_cols) > 0) {
      dt <- DT::formatRound(dt, columns = num_cols, digits = 2)
    }

    dt
  })
}
