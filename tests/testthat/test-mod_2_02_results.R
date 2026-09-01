# ============================================================================ #
# tests/testthat/test-mod_2_02_results.R                                       #
# Regression tests for the Step 2 results module: aggregation-cache keying     #
# (PERF-30) and the resolve_band_q contract (DUP-01).                          #
# ============================================================================ #

library(testthat)
library(shiny)

# ---- DUP-01: single authoritative resolve_band_q ----------------------------

test_that("resolve_band_q maps every UI band key to its quantile pair", {
  expect_identical(resolve_band_q("p25_p75"),   c(lo = 0.25,  hi = 0.75))
  expect_identical(resolve_band_q("p20_p80"),   c(lo = 0.20,  hi = 0.80))
  expect_identical(resolve_band_q("p10_p90"),   c(lo = 0.10,  hi = 0.90))
  expect_identical(resolve_band_q("p05_p95"),   c(lo = 0.05,  hi = 0.95))
  expect_identical(resolve_band_q("p025_p975"), c(lo = 0.025, hi = 0.975))
  expect_identical(resolve_band_q("p005_p995"), c(lo = 0.005, hi = 0.995))
  # minmax is the full observed range, not a winsorised pair (the deleted
  # fct_aggregation.R duplicate winsorised to 0.001/0.999 - DUP-01).
  expect_identical(resolve_band_q("minmax"),    c(lo = 0.00,  hi = 1.00))
})

test_that("resolve_band_q falls back to p10_p90 for unknown keys", {
  expect_identical(resolve_band_q("bogus"), c(lo = 0.10, hi = 0.90))
})

# ---- PERF-30: aggregation cache keyed only by value-affecting inputs -------

make_hist_sim_fixture <- function() {
  n <- 400
  set.seed(7)
  pl <- data.frame(
    sim_year = rep(2020:2021, each = n / 2),
    y_point  = rnorm(n, 1.2, 0.4),
    weight   = rep(c(1, 2), length.out = n)
  )
  list(
    so          = list(type = "numeric", name = "welfare", transform = "log"),
    residuals   = "none",
    has_weights = TRUE,
    pipeline    = list(
      sim_year  = pl$sim_year,
      y_point   = pl$y_point,
      weight    = pl$weight,
      # Tiny loadings keep the auto-tuned kernel bandwidth below the user
      # bandwidth, so bandwidth_p0 changes must visibly alter headcount SEs.
      F_loading = matrix(rnorm(2 * n) * 0.01, nrow = n),
      train_aug = NULL, id_vec = NULL, id_col = NULL
    )
  )
}

test_that("agg cache: display-only controls do not invalidate unaffected methods", {
  skip_if_not_installed("shiny")

  hist_sim <- shiny::reactiveVal(make_hist_sim_fixture())

  shiny::testServer(
    mod_2_02_results_server,
    args = list(
      id              = "results",
      hist_sim        = hist_sim,
      saved_scenarios = shiny::reactiveVal(list()),
      selected_hist   = shiny::reactiveVal(NULL),
      tabset_id       = "step2_output_tabs"
    ),
    {
      settle <- function() { session$elapse(500); session$flushReact() }
      ws <- function() agg_workspace()

      session$flushReact()
      h1 <- .get_hist_agg("mean")
      stopifnot(length(ls(envir = ws()$cache)) == 1L)

      # Poverty-line move: mean does not read it -> entry survives untouched
      session$setInputs(pov_line = 5.50); settle()
      expect_true(isTRUE(all.equal(pov_line_val(), 5.5)))
      h2 <- .get_hist_agg("mean")
      expect_identical(h1, h2)
      expect_length(ls(envir = ws()$cache), 1L)

      # gap at first line, then at a new one: new key, recompute, old kept
      g1 <- .get_hist_agg("gap")
      expect_length(ls(envir = ws()$cache), 2L)
      session$setInputs(pov_line = 7.25); settle()
      g2 <- .get_hist_agg("gap")
      expect_false(identical(g1, g2))
      expect_length(ls(envir = ws()$cache), 3L)
      m2 <- .get_hist_agg("mean")
      expect_identical(h1, m2)
      expect_true(all(g2$unweighted$gap$value > g1$unweighted$gap$value))

      # headcount reads both pl and bandwidth; gap ignores bandwidth
      session$setInputs(bandwidth_p0 = 0.10); settle()
      hc1 <- .get_hist_agg("headcount_ratio")
      expect_length(ls(envir = ws()$cache), 4L)
      hc2 <- .get_hist_agg("headcount_ratio")
      expect_identical(hc1, hc2)
      g3 <- .get_hist_agg("gap")
      expect_identical(g2, g3)
      m3 <- .get_hist_agg("mean")
      expect_identical(h1, m3)
      expect_length(ls(envir = ws()$cache), 4L)

      # New bandwidth invalidates only headcount
      session$setInputs(bandwidth_p0 = 0.20); settle()
      hc3 <- .get_hist_agg("headcount_ratio")
      expect_false(identical(hc2, hc3))
      expect_length(ls(envir = ws()$cache), 5L)
      g4 <- .get_hist_agg("gap")
      expect_identical(g2, g4)
      m4 <- .get_hist_agg("mean")
      expect_identical(h1, m4)
    }
  )
})
