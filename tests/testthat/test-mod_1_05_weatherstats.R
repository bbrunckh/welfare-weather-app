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

# A FeatureCollection shaped like the one mod_1_02 hands over: parsed geometry
# for the bounds plus the raw `geom_json` string each layer is built from.
make_map_data <- function(waves = c(2018, 2021)) {
  square <- function(x0, y0) {
    list(c(x0, y0), c(x0 + 1, y0), c(x0 + 1, y0 + 1), c(x0, y0 + 1),
         c(x0, y0))
  }
  feats <- list()
  for (y in waves) {
    for (i in seq_along(c("L1", "L2", "L3"))) {
      ring <- square(i, i)
      feats <- c(feats, list(list(
        properties = list(code = "TST", year = as.character(y),
                          survname = "SRV",
                          loc_id = c("L1", "L2", "L3")[i]),
        geometry   = list(type = "Polygon", coordinates = list(ring)),
        geom_json  = jsonlite::toJSON(
          list(type = "Polygon", coordinates = list(ring)),
          auto_unbox = TRUE
        ) |> as.character()
      )))
    }
  }
  list(type = "FeatureCollection", features = feats)
}

# Boilerplate the module needs but that these tests do not exercise.
weatherstats_args <- function(sw, swd, md) {
  list(
    connection_params = shiny::reactive(list(type = "local", path = ".")),
    variable_list     = shiny::reactive(NULL),
    selected_surveys  = shiny::reactive(NULL),
    selected_outcome  = shiny::reactive(NULL),
    selected_weather  = shiny::reactive(sw),
    hist_years        = shiny::reactive(c(from = 1991L, to = 2020L)),
    survey_data       = shiny::reactive(NULL),
    map_data          = shiny::reactive(md),
    cell_data         = shiny::reactive(NULL),
    tabset_id         = "tabs"
  )
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
    args = weatherstats_args(sw, swd, make_map_data()),
    {
      survey_weather(swd)
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
    args = weatherstats_args(sw, swd, make_map_data()),
    {
      survey_weather(swd)
      session$flushReact()

      # Defaults to the first wave rather than to nothing.
      expect_equal(wxmap_wave(), "TST|2018|SRV")
      map_2018 <- as.character(output$wxmap_1)
      expect_true(grepl("25.00", map_2018, fixed = TRUE))
      expect_false(grepl("28.00", map_2018, fixed = TRUE))

      # Switching the picker redraws the same widget with the other wave's
      # values rather than adding a second one.
      session$setInputs(wxmap_wave = "TST|2021|SRV")
      expect_equal(wxmap_wave(), "TST|2021|SRV")
      map_2021 <- as.character(output$wxmap_1)
      expect_true(grepl("28.00", map_2021, fixed = TRUE))
      expect_false(grepl("25.00", map_2021, fixed = TRUE))

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
    args = weatherstats_args(sw, swd, make_map_data(waves = 2018)),
    {
      survey_weather(swd)
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
    args = weatherstats_args(sw, swd, make_map_data()),
    {
      survey_weather(swd)
      session$flushReact()

      lv  <- weather_loc_vals()[[1]]
      pal <- .weather_map_palette(lv$value, FALSE, NULL, "None")
      expect_equal(pal$domain, range(swd$tx))
    }
  )
})
