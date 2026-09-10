# ============================================================================ #
# tests/testthat/test-run-signals.R                                            #
# UI-75: a dynamically rendered button registers as 0 when its renderUI       #
#        appears. Wrapping the button in a list for the pipeline defeated      #
#        ignoreNULL, so that registration counted as an event - the survey     #
#        loaded the instant surveys were selected, before anyone clicked.      #
#        And a load that short-circuits as "already loaded" publishes no new   #
#        frame, so a runner watching the data waited for ever.                 #
# ============================================================================ #

library(testthat)
library(shiny)

btn_val <- function(n) {
  structure(as.integer(n), class = c("shinyActionButtonValue", "integer"))
}

survey_args <- function(ext = reactiveVal(NULL)) list(
  id = "ss",
  connection_params = reactiveVal(list(type = "local")),
  variable_list     = reactiveVal(data.frame(name = "welfare", label = "W")),
  selected_surveys  = reactiveVal(data.frame(
    code = "ABC", year = 2015L, fname = "f", survname = "S",
    economy = "X", level = "hh", stringsAsFactors = FALSE)),
  cpi_ppp = reactiveVal(NULL), tabset_id = "t", run_trigger = ext
)

test_that("a Survey stats button registering at 0 does not load anything", {
  loads <- 0L
  local_mocked_bindings(load_data = function(...) { loads <<- loads + 1L; stop("stub") })
  testServer(mod_1_02_surveystats_server, args = survey_args(), {
    session$flushReact()
    # This is what happens the moment surveys are selected: the button's
    # renderUI appears and it reports 0. It must be inert.
    session$setInputs(survey_stats = btn_val(0)); session$flushReact()
    expect_equal(loads, 0L)
    # A real click loads.
    session$setInputs(survey_stats = btn_val(1)); session$flushReact()
    expect_equal(loads, 1L)
  })
})

test_that("an external run request loads with the button never clicked", {
  loads <- 0L
  local_mocked_bindings(load_data = function(...) { loads <<- loads + 1L; stop("stub") })
  ext <- reactiveVal(NULL)
  testServer(mod_1_02_surveystats_server, args = survey_args(ext), {
    session$flushReact()
    ext(list(n = 1L, at = Sys.time())); session$flushReact()
    expect_equal(loads, 1L)
  })
})

test_that("the completion tick advances on a short-circuited load", {
  # The REACT-03 short-circuit returns before publishing, so survey_data()
  # never changes on a repeat request. The tick must move anyway - it is what
  # the pipeline runner waits on. Seed the stored signature with the digest
  # the handler computes, so the click takes the "already loaded" branch
  # without depending on the full load pipeline succeeding on a fixture.
  loads <- 0L
  local_mocked_bindings(load_data = function(...) { loads <<- loads + 1L; stop("stub") })
  testServer(mod_1_02_surveystats_server, args = survey_args(), {
    session$flushReact()
    last_load_sig(digest::digest(list(
      selected_surveys(), connection_params(), variable_list(), cpi_ppp()
    )))
    expect_equal(isolate(load_done()), 0L)

    session$setInputs(survey_stats = btn_val(1)); session$flushReact()
    # Short-circuited: no I/O ran...
    expect_equal(loads, 0L)
    # ...but the tick moved, so a runner waiting on it is released.
    expect_equal(isolate(load_done()), 1L)
  })
})

test_that("a load that fails partway does not tick", {
  # The runner must not advance over a failure: a load that aborts leaves
  # survey_data() unchanged and must leave the tick unchanged too.
  local_mocked_bindings(load_data = function(...) stop("connection refused"))
  testServer(mod_1_02_surveystats_server, args = survey_args(), {
    session$flushReact()
    session$setInputs(survey_stats = btn_val(1)); session$flushReact()
    expect_equal(isolate(load_done()), 0L)
  })
})

test_that("the loaders expose their completion ticks", {
  parent <- function(input, output, session) {
    session$userData$s <- mod_1_02_surveystats_server(
      "ss", connection_params = reactiveVal(list()),
      variable_list = reactiveVal(data.frame(name = "w", label = "W")),
      selected_surveys = reactiveVal(data.frame(code = "A", year = 1L)),
      cpi_ppp = reactiveVal(NULL), tabset_id = "t")
  }
  testServer(parent, {
    expect_true(is.function(session$userData$s$load_done))
    expect_equal(isolate(session$userData$s$load_done()), 0L)
  })
})

test_that("every merged run signal is NULL until a real click or request", {
  # The same guard on all five: NULL is what ignoreNULL blocks.
  mk <- function(btn, ext) {
    if (!shiny::isTruthy(btn) && is.null(ext)) NULL else list(btn = btn, ext = ext)
  }
  expect_null(mk(NULL, NULL))
  expect_null(mk(btn_val(0), NULL))
  expect_false(is.null(mk(btn_val(1), NULL)))
  expect_false(is.null(mk(NULL, list(n = 1L))))
  expect_false(is.null(mk(btn_val(0), list(n = 1L))))
})
