# ============================================================================ #
# tests/testthat/test-mod_1_02_surveystats.R                                   #
# INT-06: map/cell state is cleared when a survey (re)load starts and on the   #
# inner H3 load failure, so the map can never show the previous survey's      #
# geography next to new microdata.                                            #
# ============================================================================ #

library(testthat)
library(shiny)

# A raw survey frame that survives the load pipeline untouched:
# add_time_columns (timestamp/economy/year) -> get_lcu_vars (no LCU vars,
# no-op) -> assign_data_level (code/urban) -> convert_lcu_to_ppp (no-op with
# no LCU vars) -> bottom_code_welfare (no welfare col) ->
# apply_policy_derivations (derived sources missing, skipped).
make_raw_survey <- function() {
  data.frame(
    code      = "TST",
    economy   = "Testland",
    year      = "2021",
    survname  = "SRV",
    source    = "NAT",
    loc_id    = c("L1", "L2", "L3"),
    timestamp = as.Date("2021-06-01"),
    weight    = 1,
    urban     = 0L,
    stringsAsFactors = FALSE
  )
}

make_raw_survey_year <- function(year) {
  df <- make_raw_survey()
  df$year <- as.character(year)
  df$timestamp <- as.Date(sprintf("%s-06-01", year))
  df
}

make_selected_surveys_fixture <- function() {
  data.frame(
    code     = "TST",
    year     = "2021",
    survname = "SRV",
    source   = "NAT",
    fname    = "microdata/TST/TST_2021_SRV_NAT.parquet",
    stringsAsFactors = FALSE
  )
}

test_that("INT-06: reload clears stale map/cell state; H3 failure leaves it clear", {
  # load_data dispatches on the requested files: the survey request resolves
  # to the fixture frame, the H3 request fails (inner load-failure path).
  local_mocked_bindings(load_data = function(fnames, ...) {
    if (any(grepl("/h3/", fnames))) stop("h3 boom")
    make_raw_survey()
  })

  shiny::testServer(
    mod_1_02_surveystats_server,
    args = list(
      id                = "ss",
      connection_params = shiny::reactiveVal(list()),
      variable_list     = shiny::reactiveVal(
        data.frame(name = character(0), units = character(0))
      ),
      selected_surveys  = shiny::reactiveVal(make_selected_surveys_fixture()),
      cpi_ppp           = shiny::reactiveVal(data.frame()),
      tabset_id         = "step1_tabs"
    ),
    {
      # Pre-seed the state a previous survey would have left behind.
      # (Location-level map_data no longer carries geography - all maps
      # render H3 cells - so the INT-06 contract is about cell state.)
      cell_data(list(geom = data.frame(h3 = "stale", geom = "stale"),
                     map = data.frame(h3 = "stale")))
      survey_data(NULL)

      # ignoreInit = TRUE swallows the first input event (session-init
      # semantics), so prime the counter before the real click.
      session$setInputs(survey_stats = 0L)
      session$setInputs(survey_stats = 1L)
      session$flushReact()

      # Microdata published; cell state cleared and NOT repopulated by the
      # failed H3 build - the map goes blank instead of showing stale
      # geography (INT-06).
      expect_false(is.null(survey_data()))
      expect_null(cell_data())
      expect_equal(survey_version(), 0L)

      # A second load starts from the same cleared baseline.
      session$setInputs(survey_stats = 3L)
      session$flushReact()
      expect_null(cell_data())
    }
  )
})

test_that("P8: failed downstream loads invalidate cached wave metadata", {
  load_number <- 0L
  local_mocked_bindings(
    load_data = function(fnames, ...) {
      if (any(grepl("/h3/", fnames))) stop("h3 boom")
      load_number <<- load_number + 1L
      make_raw_survey_year(c("2021", "2022")[[load_number]])
    }
  )

  shiny::testServer(
    mod_1_02_surveystats_server,
    args = list(
      id = "ss", connection_params = shiny::reactiveVal(list()),
      variable_list = shiny::reactiveVal(data.frame(name = character(0), units = character(0))),
      selected_surveys = shiny::reactiveVal(make_selected_surveys_fixture()),
      cpi_ppp = shiny::reactiveVal(data.frame()), tabset_id = "step1_tabs"
    ),
    {
      session$setInputs(survey_stats = 0L)
      session$setInputs(survey_stats = 1L)
      session$flushReact()
      first <- survey_wave_meta()
      expect_identical(first$waves$year, "2021")
      expect_equal(survey_data_generation(), 1L)
      expect_equal(survey_version(), 0L)

      session$setInputs(survey_stats = 2L)
      session$flushReact()
      second <- survey_wave_meta()
      expect_identical(second$waves$year, "2022")
      expect_equal(survey_data_generation(), 2L)
      expect_equal(survey_version(), 0L)
      expect_false(identical(first, second))
    }
  )
})

test_that("P3: density map fit state does not self-invalidate its observer", {
  fit_calls <- 0L
  update_calls <- 0L
  local_mocked_bindings(
    load_data = function(fnames, ...) {
      if (any(grepl("/h3/", fnames))) stop("h3 unavailable")
      make_raw_survey()
    },
    hexmap_update = function(...) { update_calls <<- update_calls + 1L; invisible(TRUE) },
    hexmap_fit = function(...) { fit_calls <<- fit_calls + 1L; invisible(TRUE) },
    hexmap_clear = function(...) invisible(TRUE)
  )

  shiny::testServer(
    mod_1_02_surveystats_server,
    args = list(
      id = "ss", connection_params = shiny::reactiveVal(list()),
      variable_list = shiny::reactiveVal(data.frame(name = character(0), units = character(0))),
      selected_surveys = shiny::reactiveVal(make_selected_surveys_fixture()),
      cpi_ppp = shiny::reactiveVal(data.frame()), tabset_id = "step1_tabs"
    ),
    {
      session$setInputs(survey_stats = 0L)
      session$setInputs(survey_stats = 1L)
      session$flushReact()
      current_survey <- make_raw_survey()
      current_survey$loc_id_panel <- current_survey$loc_id
      survey_data(current_survey)
      cell_data(list(
        geom = data.frame(h3 = "h1", geom = "{x}", xmin = 0, ymin = 0, xmax = 1, ymax = 1),
        map = data.frame(code = "TST", year = "2021", survname = "SRV", loc_id = "L1", h3 = "h1", pop_2020 = 1)
      ))
      map_data_version(1L)
      expect_false(is.null(density_cells()))
      session$flushReact()
      expect_equal(fit_calls, 1L)
      expect_equal(update_calls, 1L)
      session$flushReact(); session$flushReact()
      expect_equal(fit_calls, 1L)
      expect_equal(update_calls, 1L)
      session$setInputs(map_wave = "all")
      session$flushReact()
      expect_equal(fit_calls, 1L)
      expect_equal(update_calls, 2L)
      map_data_version(2L)
      session$flushReact()
      expect_equal(fit_calls, 2L)
      expect_equal(update_calls, 3L)
    }
  )
})

test_that("PERF-40: stats tables render from the shared union pass", {
  # The fixture must survive the load pipeline untouched and carry one
  # numeric hh-flagged variable (see make_raw_survey() above).
  raw <- make_raw_survey()
  raw$x1 <- c(1, 2, 3)
  local_mocked_bindings(load_data = function(fnames, ...) raw)

  vl <- data.frame(
    name  = "x1",
    label = "X one",
    units = "x",
    hh    = 1,
    stringsAsFactors = FALSE
  )

  shiny::testServer(
    mod_1_02_surveystats_server,
    args = list(
      id                = "ss",
      connection_params = shiny::reactiveVal(list()),
      variable_list     = shiny::reactiveVal(vl),
      selected_surveys  = shiny::reactiveVal(make_selected_surveys_fixture()),
      cpi_ppp           = shiny::reactiveVal(data.frame()),
      tabset_id         = "step1_tabs"
    ),
    {
      session$setInputs(survey_stats = 0L)
      session$setInputs(survey_stats = 1L)
      session$flushReact()

      # Rendering the hh table evaluates stats_base() and filters its rows;
      # the server-side payload carries the column headers.
      payload <- paste(jsonlite::toJSON(output$hh_stats, auto_unbox = TRUE),
                       collapse = "")
      expect_match(payload, "Variable", fixed = TRUE)
      expect_match(payload, "Country, Year", fixed = TRUE)
      expect_match(payload, "Mean", fixed = TRUE)
    }
  )
})

test_that("density cells share mapped-location counts with the payload", {
  shiny::testServer(
    mod_1_02_surveystats_server,
    args = list(
      id                = "ss",
      connection_params = shiny::reactiveVal(list()),
      variable_list     = shiny::reactiveVal(data.frame()),
      selected_surveys  = shiny::reactiveVal(make_selected_surveys_fixture()),
      cpi_ppp            = shiny::reactiveVal(data.frame()),
      tabset_id         = "step1_tabs"
    ),
    {
      survey_data(data.frame(
        code = "TST", year = "2021", survname = "SRV",
        loc_id = c("L1", "L1", "L2", "unmapped")
      ))
      cell_data(list(
        geom = data.frame(h3 = c("a", "b"), geom = c("{a}", "{b}")),
        map = data.frame(
          code = "TST", year = 2021L, survname = "SRV",
          loc_id = c("L1", "L1", "L2"), h3 = c("a", "b", "a"),
          pop_2020 = c(3, 1, 1)
        )
      ))

      density <- density_cells("all")
      expect_identical(names(density), c("cells", "n_locations"))
      expect_identical(density$n_locations, 2L)
      expect_identical(density$cells$h3, c("a", "b"))
      expect_equal(density$cells$n_units, c(2.5, 0.5))
    }
  )
})

test_that("PERF-43: wave toggles re-slice the density prep without recompute", {
  prep_calls <- 0L
  local_mocked_bindings(
    .density_wave_summary = function(cell_map, survey_data) {
      prep_calls <<- prep_calls + 1L
      list(
        cells = data.frame(
          wave = "TST|2021|SRV", h3 = c("a", "b"), n_units = c(2.5, 0.5)
        ),
        n_locations = stats::setNames(2L, "TST|2021|SRV")
      )
    }
  )

  shiny::testServer(
    mod_1_02_surveystats_server,
    args = list(
      id                = "ss",
      connection_params = shiny::reactiveVal(list()),
      variable_list     = shiny::reactiveVal(data.frame()),
      selected_surveys  = shiny::reactiveVal(make_selected_surveys_fixture()),
      cpi_ppp            = shiny::reactiveVal(data.frame()),
      tabset_id         = "step1_tabs"
    ),
    {
      survey_data(data.frame(
        code = "TST", year = "2021", survname = "SRV",
        loc_id = c("L1", "L1", "L2")
      ))
      cell_data(list(
        geom = data.frame(
          h3 = c("a", "b"), geom = c("{a}", "{b}"),
          xmin = 0, ymin = 0, xmax = 1, ymax = 1
        ),
        map = data.frame(
          code = "TST", year = 2021L, survname = "SRV",
          loc_id = "L1", h3 = "a", pop_2020 = 1
        )
      ))

      d_all <- density_cells("all")
      expect_equal(d_all$cells$h3, c("a", "b"))
      expect_equal(d_all$n_locations, 2L)
      d_wave <- density_cells("TST|2021|SRV")
      expect_equal(d_wave$cells$n_units, c(2.5, 0.5))
      # The prep ran once for both reads.
      expect_equal(prep_calls, 1L)

      # A new published load invalidates the prep (survey_data value change).
      sd2 <- survey_data()
      sd2$probe <- 1L
      publish_new_survey_data(sd2)
      density_cells("all")
      expect_equal(prep_calls, 2L)
    }
  )
})

test_that("an unmet survey prerequisite releases the load guard", {
  selected <- shiny::reactiveVal(NULL)
  shiny::testServer(
    mod_1_02_surveystats_server,
    args = list(
      id                = "ss",
      connection_params = shiny::reactiveVal(list()),
      variable_list     = shiny::reactiveVal(data.frame()),
      selected_surveys  = selected,
      cpi_ppp           = shiny::reactiveVal(data.frame()),
      tabset_id         = "step1_tabs"
    ),
    {
      session$setInputs(survey_stats = 0L)
      session$setInputs(survey_stats = 1L)
      session$flushReact()
      expect_false(load_guard$is_running())
      expect_equal(load_status(), "failure")

      selected(make_selected_surveys_fixture())
      session$setInputs(survey_stats = 2L)
      session$flushReact()
      expect_false(load_guard$is_running())
      expect_equal(load_done(), 2L)
    }
  )
})
