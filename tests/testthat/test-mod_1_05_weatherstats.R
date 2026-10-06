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
weatherstats_args <- function(sw, swd, cell_data = NULL,
                              survey_data = NULL, survey_version = 0L,
                              survey_data_generation = survey_version) {
  survey_data_rx <- if (is.function(survey_data)) survey_data else shiny::reactiveVal(survey_data)
  survey_version_rx <- if (is.function(survey_version)) survey_version else shiny::reactiveVal(survey_version)
  survey_generation_rx <- if (is.function(survey_data_generation)) survey_data_generation else shiny::reactiveVal(survey_data_generation)
  list(
    connection_params = shiny::reactive(list(type = "local", path = ".")),
    variable_list     = shiny::reactive(NULL),
    selected_surveys  = shiny::reactive(NULL),
    selected_outcome  = shiny::reactive(NULL),
    selected_weather  = shiny::reactive(sw),
    hist_years        = shiny::reactive(c(from = 1991L, to = 2020L)),
    survey_data       = survey_data_rx,
    cell_data         = shiny::reactive(cell_data),
    survey_version    = survey_version_rx,
    survey_data_generation = survey_generation_rx,
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

  sw <- make_selected_weather(2)
  # Two variables x two waves used to stand up four leaflet widgets; the
  # payload stream sends one hex map per variable instead.
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
      # One hex-map surface per variable, each holding the engine container.
      expect_true(all(nzchar(as.character(output$wxmap_1_surface))))
      expect_true(all(nzchar(as.character(output$wxmap_2_surface))))

      # Both cards are laid out, each headed by its variable and the wave.
      html <- as.character(output$weather_map_layout$html)
      expect_true(grepl("Max temp - Testland, 2018", html, fixed = TRUE))
      expect_true(grepl("Precipitation - Testland, 2018", html, fixed = TRUE))
      expect_equal(lengths(regmatches(html, gregexpr("wxmap_", html))), 2L)
    }
  )
})

test_that("the wave picker selects which wave the maps draw", {
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
      # The wave's values feed the payload stream; check the merged rows the
      # payloads are built from differ between waves.
      v2018 <- wxmap_sub(1)$value
      expect_true(all(nzchar(as.character(v2018))))

      session$setInputs(wxmap_wave = "TST|2021|SRV")
      expect_equal(wxmap_wave(), "TST|2021|SRV")
      v2021 <- wxmap_sub(1)$value
      expect_false(identical(v2018, v2021))

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

test_that("an unmet weather prerequisite releases the load guard", {
  selected <- shiny::reactiveVal(NULL)
  args <- weatherstats_args(selected, NULL)
  args$selected_weather <- selected

  shiny::testServer(mod_1_05_weatherstats_server, args = args, {
    session$setInputs(weather_stats = 0L)
    session$setInputs(weather_stats = 1L)
    session$flushReact()
    expect_false(load_guard$is_running())
    expect_equal(load_status(), "failure")

    selected(make_selected_weather())
    session$setInputs(weather_stats = 2L)
    session$flushReact()
    expect_false(load_guard$is_running())
    expect_equal(load_done(), 2L)
  })
})

test_that("P3: weather map fit keys do not self-invalidate the payload observer", {
  sw <- make_selected_weather(1)
  swd <- make_survey_weather(waves = 2018)
  fit_calls <- 0L
  update_calls <- 0L
  local_mocked_bindings(
    hexmap_update = function(...) { update_calls <<- update_calls + 1L; invisible(TRUE) },
    hexmap_fit = function(...) { fit_calls <<- fit_calls + 1L; invisible(TRUE) },
    hexmap_clear = function(...) invisible(TRUE)
  )
  shiny::testServer(
    mod_1_05_weatherstats_server,
    args = weatherstats_args(sw, swd, make_cell_data(waves = 2018)),
    {
      survey_weather(swd)
      wx_spec(list(sw = sw, so = NULL))
      session$flushReact()
      expect_equal(fit_calls, 1L)
      expect_equal(update_calls, 1L)
      session$flushReact(); session$flushReact()
      expect_equal(fit_calls, 1L)
      expect_equal(update_calls, 1L)
      wx_spec(list(sw = sw, so = NULL, revision = 2L))
      session$flushReact()
      expect_equal(fit_calls, 2L)
      expect_equal(update_calls, 2L)
    }
  )
})

test_that("P8: survey-wave metadata follows survey-data generation", {
  calls <- 0L
  real_metadata <- survey_wave_metadata
  local_mocked_bindings(survey_wave_metadata = function(df) {
    calls <<- calls + 1L
    real_metadata(df)
  })
  svy <- shiny::reactiveVal(make_survey_weather(waves = 2018))
  version <- shiny::reactiveVal(1L)
  generation <- shiny::reactiveVal(1L)
  swd <- make_survey_weather(waves = 2018)
  shiny::testServer(
    mod_1_05_weatherstats_server,
    args = weatherstats_args(make_selected_weather(1), swd,
                             survey_data = svy, survey_version = version,
                             survey_data_generation = generation),
    {
      first <- survey_wave_meta()
      expect_equal(calls, 1L)
      expect_identical(survey_wave_meta(), first)
      expect_equal(calls, 1L)
      svy(make_survey_weather(waves = c(2018, 2021)))
      expect_identical(survey_wave_meta(), first)
      expect_equal(calls, 1L)
      version(2L)
      expect_identical(survey_wave_meta(), first)
      expect_equal(calls, 1L)
      generation(2L)
      expect_equal(nrow(survey_wave_meta()$waves), 2L)
      expect_equal(calls, 2L)
    }
  )
})

test_that("P7: weather plotting frames are reused until their source changes", {
  swd <- make_survey_weather(waves = 2018)
  shiny::testServer(
    mod_1_05_weatherstats_server,
    args = weatherstats_args(make_selected_weather(1), swd),
    {
      survey_weather(swd)
      survey_weather_cont(swd)
      expect_identical(weather_plot_frame(), weather_plot_frame())
      expect_identical(weather_plot_cont_frame(), weather_plot_cont_frame())
      expect_equal(attr(weather_plot_frame, "observable")$.execCount, 1L)
      expect_equal(attr(weather_plot_cont_frame, "observable")$.execCount, 1L)
      expect_identical(weather_plot_frame()$countryyear,
                       paste0(swd$economy, ", ", swd$year))
      existing <- swd
      existing$countryyear <- paste0("Existing ", seq_len(nrow(existing)))
      survey_weather(existing)
      expect_identical(weather_plot_frame()$countryyear, existing$countryyear)
      changed <- swd
      changed$tx <- changed$tx + 1
      survey_weather(changed)
      expect_identical(weather_plot_frame()$tx, changed$tx)
      expect_equal(attr(weather_plot_frame, "observable")$.execCount, 3L)
      expect_equal(attr(weather_plot_cont_frame, "observable")$.execCount, 1L)
    }
  )
})

test_that("R2-BUG-21: the load short-circuit does not keep a stale outcome", {
  sw <- make_selected_weather(1)
  swd <- make_survey_weather(waves = 2018)
  loads <- 0L
  local_mocked_bindings(
    get_weather = function(...) {
      loads <<- loads + 1L
      list(historical = swd)
    },
    merge_survey_weather = function(svy, wd) swd,
    join_hist_sample_cells = function(...) NULL
  )
  outcome <- shiny::reactiveVal(data.frame(name = "welfare", label = "A"))
  args <- weatherstats_args(sw, NULL, survey_data = swd)
  args$selected_surveys <- shiny::reactive(data.frame(code = "TST", year = "2018"))
  args$selected_outcome <- outcome

  shiny::testServer(mod_1_05_weatherstats_server, args = args, {
    session$setInputs(weather_stats = 0L)
    session$setInputs(weather_stats = 1L)
    session$flushReact()
    expect_equal(loads, 1L)
    expect_equal(wx_spec()$so$label, "A")

    # Same weather selection, same outcome: served from state.
    session$setInputs(weather_stats = 2L)
    session$flushReact()
    expect_equal(loads, 1L)

    # A new outcome must not be answered with "already loaded".
    outcome(data.frame(name = "poor", label = "B"))
    session$setInputs(weather_stats = 3L)
    session$flushReact()
    expect_equal(wx_spec()$so$label, "B")
    expect_equal(load_status(), "success")
  })
})
