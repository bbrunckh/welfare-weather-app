library(testthat)

# ============================================================================ #
# Helpers                                                                      #
# ============================================================================ #

# A merged survey-weather frame covering two waves of one country, three
# locations each, with a continuous weather variable.
make_survey_weather <- function(waves = c(2018, 2021)) {
  do.call(rbind, lapply(waves, function(y) {
    data.frame(
      code      = "TST",
      economy   = "Testland",
      year      = as.character(y),
      survname  = "SRV",
      loc_id    = rep(c("L1", "L2", "L3"), each = 4),
      timestamp = as.Date(paste0(y, "-06-01")),
      weight    = 1,
      # 25/30/35 in 2018, 28/33/38 in 2021 — distinct enough that a rendered
      # map can be told apart by the values in its popups.
      tx        = rep(c(25, 30, 35), each = 4) + (y - 2018),
      stringsAsFactors = FALSE
    )
  }))
}

make_selected_weather <- function(n = 1) {
  data.frame(
    name           = c("tx", "pr")[seq_len(n)],
    label          = c("Max temp", "Precipitation")[seq_len(n)],
    cont_binned    = rep("Continuous", n),
    transformation = rep("None", n),
    stringsAsFactors = FALSE
  )
}

# Boilerplate the module needs but that these tests do not exercise.
weatherstats_args <- function(sw, swd, cell_data = NULL) {
  list(
    connection_params = shiny::reactive(list(type = "local", path = ".")),
    variable_list     = shiny::reactive(NULL),
    selected_surveys  = shiny::reactive(NULL),
    selected_outcome  = shiny::reactive(NULL),
    selected_weather  = shiny::reactive(sw),
    hist_years        = shiny::reactive(c(from = 1991L, to = 2020L)),
    survey_data       = shiny::reactive(NULL),
    cell_data         = shiny::reactive(cell_data),
    tabset_id         = "tabs"
  )
}

# An H3 cell fixture shaped like what mod_1_02 hands over: one `geom` row per
# cell (geometry string + DuckDB bbox) and a `map` of location-to-cell pairs.
# The merged per-cell values differ per wave, so a rendered map can be told
# apart by its fill colours.
make_cell_data <- function(waves = c(2018, 2021)) {
  locs  <- c("L1", "L2", "L3")
  h3    <- c("879754048ffffff", "87975404affffff", "87975404bffffff")
  vals  <- c(25, 30, 35)
  cell_geo <- data.frame(
    h3   = h3,
    geom = sprintf(
      '{"type":"Polygon","coordinates":[[[%f,%f],[%f,%f],[%f,%f],[%f,%f],[%f,%f]]]}',
      -19 - seq_along(h3), 27, -18 - seq_along(h3), 27,
      -18 - seq_along(h3), 28, -19 - seq_along(h3), 28,
      -19 - seq_along(h3), 27
    ),
    xmin = -20 - seq_along(h3), ymin = 27,
    xmax = -18 - seq_along(h3), ymax = 28,
    stringsAsFactors = FALSE
  )
  cell_map <- do.call(rbind, lapply(waves, function(y) {
    data.frame(
      code     = "TST",
      year     = as.character(y),
      survname = "SRV",
      loc_id   = locs,
      h3       = h3,
      pop_2020 = c(10, 20, 30),
      stringsAsFactors = FALSE
    )
  }))
  list(geom = cell_geo, map = cell_map)
}


# ============================================================================ #
# Weather-by-location maps                                                     #
# ============================================================================ #

test_that("one map output is created per weather variable, not per wave", {
  skip_if_not_installed("leaflet")

  sw <- make_selected_weather(2)
  # Two variables x two waves used to stand up four leaflet widgets.
  swd <- make_survey_weather()
  swd$pr <- swd$tx * 2

  shiny::testServer(
    mod_1_05_weatherstats_server,
    args = weatherstats_args(sw, swd, make_cell_data()),
    {
      survey_weather(swd)
      wx_spec(list(sw = sw, so = NULL))
      session$flushReact()

      expect_equal(nrow(wave_list()), 2L)
      # One widget per variable — two, where the per-wave layout would have
      # stood up four.
      expect_true(nchar(as.character(output$wxmap_1)) > 0)
      expect_true(nchar(as.character(output$wxmap_2)) > 0)

      # Both cards are laid out, each headed by its variable and the wave.
      html <- as.character(output$weather_map_layout$html)
      expect_true(grepl("Max temp - Testland, 2018", html, fixed = TRUE))
      expect_true(grepl("Precipitation - Testland, 2018", html, fixed = TRUE))
      expect_equal(lengths(regmatches(html, gregexpr("wxmap_", html))), 2L)
    }
  )
})

test_that("the wave picker selects which wave the maps draw", {
  skip_if_not_installed("leaflet")

  sw  <- make_selected_weather(1)
  swd <- make_survey_weather()

  shiny::testServer(
    mod_1_05_weatherstats_server,
    args = weatherstats_args(sw, swd, make_cell_data()),
    {
      survey_weather(swd)
      wx_spec(list(sw = sw, so = NULL))
      session$flushReact()

      # Defaults to the first wave rather than to nothing.
      expect_equal(wxmap_wave(), "TST|2018|SRV")
      # The wave's values are encoded as the per-feature fill colours (no
      # popups): read them out of the widget's addGeoJSON call. The FC
      # string's properties carry a nested `style.fillColor` since the
      # Leaflet rollback (the mapgl experiment used a flat `__fill`).
      fill_colors <- function(widget) {
        j <- jsonlite::fromJSON(widget, simplifyVector = FALSE)
        call <- Filter(function(k) identical(k$method, "addGeoJSON"),
                       j$x$calls)[[1]]
        fc <- jsonlite::fromJSON(call$args[[1]], simplifyVector = FALSE)
        vapply(fc$features,
               function(f) f$properties$style$fillColor, character(1))
      }
      cols_2018 <- fill_colors(output$wxmap_1)
      expect_true(all(nzchar(cols_2018)))

      # Switching the picker redraws the same widget with the other wave's
      # values rather than adding a second one.
      session$setInputs(wxmap_wave = "TST|2021|SRV")
      expect_equal(wxmap_wave(), "TST|2021|SRV")
      cols_2021 <- fill_colors(output$wxmap_1)
      expect_true(all(nzchar(cols_2021)))
      expect_false(identical(cols_2018, cols_2021))

      # The card header follows the picker.
      expect_true(grepl("Max temp - Testland, 2021",
                        as.character(output$weather_map_layout$html),
                        fixed = TRUE))

      # A wave that is no longer in the data falls back to the first.
      wxmap_wave_val("TST|1999|SRV")
      expect_equal(wxmap_wave(), "TST|2018|SRV")
    }
  )
})

test_that("the wave picker is hidden when there is only one wave", {
  skip_if_not_installed("leaflet")

  sw  <- make_selected_weather(1)
  swd <- make_survey_weather(waves = 2018)

  shiny::testServer(
    mod_1_05_weatherstats_server,
    args = weatherstats_args(sw, swd),
    {
      survey_weather(swd)
      wx_spec(list(sw = sw, so = NULL))
      session$flushReact()

      expect_equal(nrow(wave_list()), 1L)
      expect_null(output$wxmap_wave_ui$html)
    }
  )
})

test_that("the map colour scale spans every wave, not just the one shown", {
  skip_if_not_installed("leaflet")

  sw  <- make_selected_weather(1)
  swd <- make_survey_weather()
  # Push the 2021 wave well above the 2018 range: a scale built from the
  # displayed wave alone would change when the picker moves.
  swd$tx[swd$year == "2021"] <- swd$tx[swd$year == "2021"] + 20

  shiny::testServer(
    mod_1_05_weatherstats_server,
    args = weatherstats_args(sw, swd),
    {
      survey_weather(swd)
      wx_spec(list(sw = sw, so = NULL))
      session$flushReact()

      lv  <- weather_loc_vals()[[1]]
      pal <- .weather_map_palette(lv$value, FALSE, NULL, "None")
      expect_equal(pal$domain, range(swd$tx))
    }
  )
})
