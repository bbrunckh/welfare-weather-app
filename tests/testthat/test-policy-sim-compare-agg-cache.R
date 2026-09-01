# ============================================================================ #
# tests/testthat/test-policy-sim-compare-agg-cache.R                           #
# PERF-31: Step 3 aggregation cache. The aggregation workspace is keyed by     #
# (source, method, poverty line) only - the deviation control is applied       #
# downstream, so changing it must not re-aggregate anything. Cached results    #
# are identical to freshly computed ones (aggregate_pipeline_per_year is       #
# RNG-deterministic per seed + year, see Batch C tests).                       #
#                                                                              #
# .wire_results_pane() invisibly returns its aggregation internals so these    #
# tests can drive the real reactives.                                          #
# ============================================================================ #

library(testthat)
library(shiny)

make_step3_pipe_fixture <- function(n = 300L, yrs = 2020:2021) {
  set.seed(11)
  expand <- length(yrs)
  list(
    sim_year  = rep(yrs, each = n),
    y_point   = rnorm(n * expand, 1.2, 0.4),
    weight    = rep(c(1, 2), length.out = n * expand),
    # Tiny loadings keep SEs small and deterministic under the delta method
    F_loading = matrix(rnorm(2 * n * expand) * 0.01, nrow = n * expand),
    train_aug = NULL, id_vec = NULL, id_col = NULL
  )
}

make_step3_hist_fixture <- function() {
  list(
    so        = list(type = "numeric", name = "welfare", transform = "log"),
    residuals = "none",
    pipeline  = make_step3_pipe_fixture()
  )
}

make_step3_scenarios_fixture <- function() {
  pipe <- make_step3_pipe_fixture()
  list("SSP2-4.5 / 2030-2040" = list(
    scenario_name = "SSP2-4.5 / 2030-2040",
    so            = list(type = "numeric", name = "welfare", transform = "log"),
    pipelines     = list(ensemble_mean = pipe, ensemble_hi = pipe)
  ))
}

test_that("Step 3 agg cache: deviation changes reuse cache; method/pov-line key entries", {
  skip_if_not_installed("shiny")

  bh  <- shiny::reactiveVal(make_step3_hist_fixture())
  ph  <- shiny::reactiveVal(make_step3_hist_fixture())
  bsc <- shiny::reactiveVal(make_step3_scenarios_fixture())
  psc <- shiny::reactiveVal(make_step3_scenarios_fixture())

  internals <- NULL
  shiny::testServer(
    function(input, output, session) {
      internals <<- .wire_results_pane(
        input, output, session,
        baseline_hist_sim        = bh,
        baseline_saved_scenarios = bsc,
        policy_hist_sim          = ph,
        policy_saved_scenarios   = psc,
        selected_hist            = shiny::reactiveVal(NULL),
        residuals                = shiny::reactiveVal("none")
      )
      NULL
    },
    {
      settle <- function() { session$elapse(500); session$flushReact() }
      # Count only the baseline_hist cache entries; the pane's other
      # consumers (threshold tables, policy arm) populate other keys.
      bh_keys <- function() {
        grep("^baseline_hist\\r", ls(envir = internals$agg_cache_ws()), value = TRUE)
      }
      session$setInputs(cmp_agg_method = "mean", cmp_deviation = "none")
      session$setInputs(cmp_pov_line = 3.00); settle()

      # First pass populates one baseline_hist entry
      h1 <- internals$baseline_agg_hist()$out
      expect_length(bh_keys(), 1L)

      # Deviation change: NO new entries, identical object served from cache
      session$setInputs(cmp_deviation = "mean"); settle()
      h2 <- internals$baseline_agg_hist()$out
      expect_length(bh_keys(), 1L)
      expect_identical(h1, h2)

      # Method change: separate key, old entry retained
      session$setInputs(cmp_agg_method = "median"); settle()
      m1 <- internals$baseline_agg_hist()$out
      expect_false(identical(h1, m1))
      expect_length(bh_keys(), 2L)

      # Back to mean: served from the original cache entry, bit-identical
      session$setInputs(cmp_agg_method = "mean"); settle()
      h3 <- internals$baseline_agg_hist()$out
      expect_identical(h1, h3)
      expect_length(bh_keys(), 2L)

      # Poverty line (poverty methods only): new keys per line, old kept
      session$setInputs(cmp_agg_method = "gap"); settle()
      g1 <- internals$baseline_agg_hist()$out
      expect_length(bh_keys(), 3L)
      session$setInputs(cmp_pov_line = 5.50); settle()
      g2 <- internals$baseline_agg_hist()$out
      expect_false(identical(g1, g2))
      expect_length(bh_keys(), 4L)
      # Step 3 schema: `out` carries a scalar `value` per year directly
      expect_true(all(g2$value > g1$value))
    }
  )
})

test_that("Step 3 agg cache is invalidated on simulation republish; recompute identical", {
  skip_if_not_installed("shiny")

  bh  <- shiny::reactiveVal(make_step3_hist_fixture())
  ph  <- shiny::reactiveVal(make_step3_hist_fixture())
  bsc <- shiny::reactiveVal(make_step3_scenarios_fixture())
  psc <- shiny::reactiveVal(make_step3_scenarios_fixture())

  internals <- NULL
  shiny::testServer(
    function(input, output, session) {
      internals <<- .wire_results_pane(
        input, output, session,
        baseline_hist_sim        = bh,
        baseline_saved_scenarios = bsc,
        policy_hist_sim          = ph,
        policy_saved_scenarios   = psc,
        selected_hist            = shiny::reactiveVal(NULL),
        residuals                = shiny::reactiveVal("none")
      )
      NULL
    },
    {
      session$setInputs(cmp_agg_method = "mean", cmp_deviation = "none")
      session$setInputs(cmp_pov_line = 3.00)
      session$elapse(500); session$flushReact()

      h1 <- internals$baseline_agg_hist()$out
      expect_length(ls(envir = internals$agg_cache_ws()), 4L)

      # Re-publish the simulation (same content): fresh cache env, entries
      # are recomputed deterministically and stay bit-identical (seeded
      # residual substreams - Batch C).
      bh(make_step3_hist_fixture())
      session$elapse(500); session$flushReact()

      h2 <- internals$baseline_agg_hist()$out
      expect_length(ls(envir = internals$agg_cache_ws()), 4L)
      expect_identical(h1, h2)
    }
  )
})

test_that("cached scenario aggregation is identical to uncached recomputation", {
  skip_if_not_installed("shiny")

  hist <- make_step3_hist_fixture()
  sc   <- make_step3_scenarios_fixture()

  bh  <- shiny::reactiveVal(hist)
  ph  <- shiny::reactiveVal(hist)
  bsc <- shiny::reactiveVal(sc)
  psc <- shiny::reactiveVal(sc)

  internals <- NULL
  shiny::testServer(
    function(input, output, session) {
      internals <<- .wire_results_pane(
        input, output, session,
        baseline_hist_sim        = bh,
        baseline_saved_scenarios = bsc,
        policy_hist_sim          = ph,
        policy_saved_scenarios   = psc,
        selected_hist            = shiny::reactiveVal(NULL),
        residuals                = shiny::reactiveVal("none")
      )
      NULL
    },
    {
      session$setInputs(cmp_agg_method = "gap", cmp_deviation = "none",
                        cmp_pov_line = 3.00)
      session$elapse(500); session$flushReact()

      cached_hist <- internals$baseline_agg_hist()$out
      cached_scn  <- internals$baseline_agg_scenarios()[["SSP2-4.5 / 2030-2040"]]$out
      n_after_first <- length(ls(envir = internals$agg_cache_ws()))
      expect_true(n_after_first >= 4L)

      # Invalidate by re-publishing; deterministic recompute must match
      bh(make_step3_hist_fixture())
      bsc(make_step3_scenarios_fixture())
      session$elapse(500); session$flushReact()

      fresh_hist <- internals$baseline_agg_hist()$out
      fresh_scn  <- internals$baseline_agg_scenarios()[["SSP2-4.5 / 2030-2040"]]$out

      expect_identical(cached_hist, fresh_hist)
      expect_identical(cached_scn,  fresh_scn)
      expect_length(ls(envir = internals$agg_cache_ws()), n_after_first)
    }
  )
})
