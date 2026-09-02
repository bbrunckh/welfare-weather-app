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
      map_data(list(type = "FeatureCollection",
                    features = list(list(properties = list(stale = TRUE)))))
      cell_data(list(geom = data.frame(h3 = "stale", geom = "stale"),
                     map = data.frame(h3 = "stale")))
      survey_data(NULL)

      # ignoreInit = TRUE swallows the first input event (session-init
      # semantics), so prime the counter before the real click.
      session$setInputs(survey_stats = 0L)
      session$setInputs(survey_stats = 1L)
      session$flushReact()

      # Microdata published; map/cell cleared and NOT repopulated by the
      # failed H3 build - the map goes blank instead of showing stale
      # geography (INT-06).
      expect_false(is.null(survey_data()))
      expect_null(map_data())
      expect_null(cell_data())

      # A second load starts from the same cleared baseline.
      session$setInputs(survey_stats = 3L)
      session$flushReact()
      expect_null(map_data())
      expect_null(cell_data())
    }
  )
})
