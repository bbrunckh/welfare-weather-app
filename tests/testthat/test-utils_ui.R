# ============================================================================ #
# tests/testthat/test-utils_ui.R                                               #
# REACT-02: .busy_guard() double-click protection - second entry is refused    #
# while a guarded action is still running, and the flag clears on end().       #
# INT-01: .restore_selection() / .restore_numeric() dynamic-input restores.    #
# ============================================================================ #

library(testthat)
library(shiny)

# ---- INT-01: selection restore helpers ---------------------------------------

test_that("restore_selection clips invalid values and falls back on empty", {
  # First render: no previous selection -> fallback
  expect_identical(.restore_selection(NULL, c("a", "b"), fallback = "a"), "a")
  expect_identical(.restore_selection(character(0), c("a", "b"), fallback = NULL), NULL)

  # Full survival: previous selection kept, order preserved
  expect_identical(.restore_selection(c("b", "a"), c("a", "b", "c"), "x"),
                   c("b", "a"))

  # Partial clip: only values still present survive
  expect_identical(.restore_selection(c("a", "zzz"), c("a", "b"), fallback = "b"),
                   "a")

  # Nothing survives -> historical default
  expect_identical(.restore_selection(c("zz"), c("a", "b"), fallback = c("a", "b")),
                   c("a", "b"))

  # Named inputs/choices are matched on values, names stripped
  expect_identical(
    .restore_selection(c(k = "a"), stats::setNames(c("a", "b"), c("A", "B")), "a"),
    "a"
  )
})

test_that("restore_numeric keeps in-range values and falls back otherwise", {
  expect_identical(.restore_numeric(5.5, 0.01, Inf, fallback = 3), 5.5)
  expect_identical(.restore_numeric(7, 5, 20, fallback = 10), 7)
  expect_identical(.restore_numeric(NULL, 5, 20, fallback = 10), 10)
  expect_identical(.restore_numeric(NA_real_, 5, 20, fallback = 10), 10)
  expect_identical(.restore_numeric(numeric(0), 5, 20, fallback = 10), 10)
  # Out of range -> fallback
  expect_identical(.restore_numeric(25, 5, 20, fallback = 10), 10)
  expect_identical(.restore_numeric(0, 0.1, 1, fallback = 0.5), 0.5)
  # Boundary values are kept
  expect_identical(.restore_numeric(5, 5, 20, fallback = 10), 5)
  expect_identical(.restore_numeric(20, 5, 20, fallback = 10), 20)
})

# ---- INT-05: fit-time label snapshot -----------------------------------------

test_that("label_lookup binds labels to a fixed variable frame", {
  vl <- data.frame(name  = c("tx", "welfare"),
                   label = c("Max temp", "Welfare"))
  lab <- .label_lookup(vl)
  expect_identical(lab("tx"), "Max temp")
  expect_identical(lab("unknown"), "unknown")
  expect_identical(.label_lookup(NULL)("tx"), "tx")
})

# ---- INT-08: run-signature canonicalisation ----------------------------------

test_that("sig_plain makes independently-built values identical", {
  df1 <- data.frame(name = c("a", "b"), flag = c(1L, 0L))
  df2 <- data.frame(name = c("a", "b"), flag = c(1L, 0L))
  expect_true(identical(.sig_plain(df1), .sig_plain(df2)))

  lst1 <- list(x = 1, df = df1, sub = list("s", 2.5))
  lst2 <- list(x = 1, df = df2, sub = list("s", 2.5))
  expect_true(identical(.sig_plain(lst1), .sig_plain(lst2)))

  # Differing content must differ
  df3 <- df2
  df3$flag <- c(1L, 1L)
  expect_false(identical(.sig_plain(df1), .sig_plain(df3)))

  # Atoms and NULL pass through
  expect_identical(.sig_plain("ssp3_7_0"), "ssp3_7_0")
  expect_null(.sig_plain(NULL))
})


test_that("busy guard admits one concurrent entry and refuses re-entry", {
  shiny::testServer(
    function(input, output, session) {
      guard <- .busy_guard(session, btn1)
      # expose for assertions inside the test block
      expose <- guard
      list(guard = guard)
    },
    {
      expect_false(guard$is_running())

      # First entry wins
      expect_true(guard$begin())
      expect_true(guard$is_running())

      # Re-entry while running is refused
      expect_false(guard$begin())
      expect_true(guard$is_running())

      # Ending clears the flag and admits a new run
      guard$end()
      expect_false(guard$is_running())
      expect_true(guard$begin())
      expect_true(guard$is_running())
      guard$end()
 expect_false(guard$is_running())
    }
  )
})

test_that("on.exit(guard$end()) releases the guard on error", {
  shiny::testServer(
    function(input, output, session) {
      guard <- .busy_guard(session)
      list(guard = guard)
    },
    {
      guarded <- function() {
        guard$begin()
        on.exit(guard$end(), add = TRUE)
        stop("boom")
      }
      expect_error(guarded(), "boom")
      expect_false(guard$is_running())
      expect_true(guard$begin())
      guard$end()
    }
  )
})

test_that("Step 2-style run observer refuses re-entry during the run", {
  shiny::testServer(
    function(input, output, session) {
      # Minimal replica of the Step 2 run-guard contract: the observer
      # begins the guard synchronously, so a re-entrant event during the
      # same flush is refused.
      sim_guard <- .busy_guard(session, run_sim)
      runs <- shiny::reactiveVal(0L)
      nested_attempt <- shiny::reactiveVal(NULL)

      observeEvent(input$run_sim, {
        if (!sim_guard$begin()) {
          nested_attempt("refused")
          return(invisible(NULL))
        }
        on.exit(sim_guard$end(), add = TRUE)
        runs(runs() + 1L)
      }, ignoreInit = TRUE)

      list(sim_guard = sim_guard, runs = runs,
           nested_attempt = nested_attempt)
    },
    {
      # Prime: with ignoreInit = TRUE the first input value is treated as
      # the session-init event and ignored (matches production behaviour).
      session$setInputs(run_sim = 0L); session$flushReact()
      expect_equal(runs(), 0L)

      session$setInputs(run_sim = 1L); session$flushReact()
      expect_equal(runs(), 1L)
      expect_false(sim_guard$is_running())
      expect_null(nested_attempt())

      # Sequential re-run is allowed once the previous finished.
      session$setInputs(run_sim = 2L); session$flushReact()
      expect_equal(runs(), 2L)
      expect_false(sim_guard$is_running())
    }
  )
})

# ---- Wave toggle-style slider helpers ----------------------------------------

test_that("wave_slider_choices formats single and multi-country choices correctly", {
  # Empty or NULL input
  expect_identical(wave_slider_choices(NULL), character(0))
  expect_identical(wave_slider_choices(data.frame()), character(0))

  # Single country: years directly
  df_single <- data.frame(
    key      = c("MWI|2010|IHS3", "MWI|2016|IHS4"),
    year     = c("2010", "2016"),
    code     = c("MWI", "MWI"),
    economy  = c("Malawi", "Malawi"),
    stringsAsFactors = FALSE
  )
  choices_all <- wave_slider_choices(df_single, include_all = TRUE)
  expect_equal(choices_all, c("All" = "all", "2010" = "MWI|2010|IHS3", "2016" = "MWI|2016|IHS4"))

  choices_no_all <- wave_slider_choices(df_single, include_all = FALSE)
  expect_equal(choices_no_all, c("2010" = "MWI|2010|IHS3", "2016" = "MWI|2016|IHS4"))

  # Multi-country: prefixed with country code
  df_multi <- data.frame(
    key      = c("MWI|2010|IHS3", "TZA|2012|NPS"),
    year     = c("2010", "2012"),
    code     = c("MWI", "TZA"),
    economy  = c("Malawi", "Tanzania"),
    stringsAsFactors = FALSE
  )
  choices_multi <- wave_slider_choices(df_multi, include_all = TRUE)
  expect_equal(choices_multi, c("All" = "all", "MWI 2010" = "MWI|2010|IHS3", "TZA 2012" = "TZA|2012|NPS"))
})

test_that("wave_toggle_slider builds radio group with toggle-slider classes", {
  choices <- c("All" = "all", "2010" = "MWI|2010", "2016" = "MWI|2016")
  tag <- wave_toggle_slider("test_wave", choices = choices, selected = "all")
  tag_html <- as.character(tag)

  expect_true(grepl("toggle-slider", tag_html, fixed = TRUE))
  expect_true(grepl("wave-toggle-slider", tag_html, fixed = TRUE))
  expect_true(grepl("type=\"radio\"", tag_html, fixed = TRUE))
  expect_true(grepl("value=\"all\"", tag_html, fixed = TRUE))
  expect_true(grepl("value=\"MWI|2010\"", tag_html, fixed = TRUE))
})
