# W3-A characterization tests. These tests deliberately compare the shared
# pipeline wrappers with their pre-change per-year direct calls so the lazy-arm
# and preparation-cache changes cannot alter values, gradients, or RNG streams.

library(testthat)
library(wiseapp)

make_w3a_pipeline <- function(with_weights = TRUE) {
  y_point <- c(
    log(2.0), log(3.0), NA_real_, log(5.0),
    log(2.5), log(3.5), log(4.5), log(6.0),
    log(1.5), log(2.5), log(4.0), log(7.0)
  )
  list(
    sim_year  = rep(c(2030L, 2031L, 2032L), each = 4L),
    y_point   = y_point,
    weight    = if (with_weights) c(1, 2, 3, 4, 2, 1, 4, 3, 1, 3, 2, 5) else NULL,
    F_loading = cbind(
      seq(-0.03, 0.03, length.out = length(y_point)),
      rep(c(-0.02, 0.01, 0.03, -0.01), length.out = length(y_point))
    ),
    id_vec    = c("a", "b", "missing", "d", "a", "b", "c", "d",
                  "a", "b", "c", "d"),
    id_col    = "hhid",
    train_aug = data.frame(
      hhid = c("a", "b", "c", "d"),
      .resid = c(-0.2, 0.1, 0.3, -0.1),
      stringsAsFactors = FALSE
    )
  )
}

w3a_methods <- c(
  "mean", "median", "total", "headcount_ratio", "gap", "fgt2",
  "gini", "prosperity_gap", "avg_poverty"
)

w3a_pov_line <- function(method) {
  if (method %in% c("headcount_ratio", "gap", "fgt2")) 3.25 else NULL
}

# This is the old aggregate_pipeline_per_year body expressed as a test oracle:
# it intentionally repeats the setup that W3-A is allowed to share, then calls
# the public delta aggregator once per year.
w3a_direct_per_year <- function(pipe, method, weighted, residuals, is_log, seed) {
  yrs <- sort(unique(pipe$sim_year))
  lapply(yrs, function(yr) {
    idx <- pipe$sim_year == yr
    valid <- idx & !is.na(pipe$y_point)
    f_idx <- pipe$F_loading[valid, , drop = FALSE]
    w_idx <- if (isTRUE(weighted) && !is.null(pipe$weight)) pipe$weight[valid] else NULL
    id_idx <- pipe$id_vec[valid]
    out <- wiseapp::aggregate_with_uncertainty_delta(
      y_point = pipe$y_point[valid],
      F_loading = f_idx,
      method = method,
      weights = w_idx,
      pov_line = w3a_pov_line(method),
      residuals = residuals,
      train_aug = pipe$train_aug,
      id_vec = id_idx,
      id_col = pipe$id_col,
      is_log = is_log,
      seed = wiseapp:::wise_seed(seed, "residual", yr)
    )
    out$sim_year <- yr
    out
  })
}

test_that("every method and weighting arm preserves the pre-change yearly contract", {
  pipe <- make_w3a_pipeline()

  for (method in w3a_methods) {
    for (weighted in c(FALSE, TRUE)) {
      observed <- wiseapp::aggregate_pipeline_per_year(
        pipe,
        method = method,
        weighted = weighted,
        pov_line = w3a_pov_line(method),
        residuals = "none",
        is_log = TRUE,
        seed = 991L
      )
      expected <- w3a_direct_per_year(
        pipe, method, weighted, residuals = "none", is_log = TRUE, seed = 991L
      )
      expect_identical(
        observed,
        expected,
        info = paste(method, if (weighted) "weighted" else "unweighted")
      )
    }
  }
})

test_that("characterization covers non-finite filtering and log back-transformation", {
  welfare <- c(1, Inf, -Inf, NA_real_, 4)
  weights <- c(1, 2, 3, 4, 5)

  expect_equal(
    wiseapp::resolve_agg_fn("avg_poverty")(welfare, weights, NULL),
    stats::weighted.mean(c(1, 1 / 4), c(1, 5))
  )
  expect_equal(
    wiseapp::resolve_agg_fn("gini")(c(1, NA_real_, 3), NULL, NULL),
    wiseapp::resolve_agg_fn("gini")(c(1, 3), NULL, NULL)
  )

  pipe <- make_w3a_pipeline()
  pipe$y_point <- log(c(2, 4, 8, 16, 3, 6, 9, 12, 2, 5, 10, 20))
  log_result <- wiseapp::aggregate_pipeline_per_year(
    pipe, method = "mean", weighted = FALSE, residuals = "none", is_log = TRUE
  )
  level_result <- wiseapp::aggregate_pipeline_per_year(
    pipe, method = "mean", weighted = FALSE, residuals = "none", is_log = FALSE
  )
  expect_equal(
    vapply(log_result, `[[`, numeric(1L), "value"),
    c(mean(c(2, 4, 8, 16)), mean(c(3, 6, 9, 12)), mean(c(2, 5, 10, 20)))
  )
  expect_equal(
    vapply(level_result, `[[`, numeric(1L), "value"),
    c(mean(log(c(2, 4, 8, 16))), mean(log(c(3, 6, 9, 12))),
      mean(log(c(2, 5, 10, 20))))
  )
})

test_that("original residuals preserve ID matching and deterministic fallback", {
  pipe <- make_w3a_pipeline()
  observed <- wiseapp::aggregate_pipeline_per_year(
    pipe, method = "mean", weighted = FALSE, residuals = "original",
    is_log = TRUE, seed = 123L
  )
  expected <- w3a_direct_per_year(
    pipe, "mean", FALSE, residuals = "original", is_log = TRUE, seed = 123L
  )
  expect_identical(observed, expected)

  first_year_ids <- pipe$id_vec[pipe$sim_year == 2030L]
  residuals <- wiseapp:::draw_residuals_vec(
    "original", pipe$train_aug, length(first_year_ids), first_year_ids,
    pipe$id_col, seed = wiseapp:::wise_seed(123L, "residual", 2030L)
  )
  expect_equal(residuals[first_year_ids == "a"], -0.2)
  expect_equal(residuals[first_year_ids == "b"], 0.1)
  expect_equal(residuals[first_year_ids == "d"], -0.1)
  expect_true(is.finite(residuals[first_year_ids == "missing"]))
})

test_that("normal and resample residual streams remain per-year and arm-independent", {
  pipe <- make_w3a_pipeline()
  for (residuals in c("normal", "resample")) {
    for (weighted in c(FALSE, TRUE)) {
      observed <- wiseapp::aggregate_pipeline_per_year(
        pipe, method = "mean", weighted = weighted, residuals = residuals,
        is_log = TRUE, seed = 456L
      )
      expected <- w3a_direct_per_year(
        pipe, "mean", weighted, residuals = residuals, is_log = TRUE, seed = 456L
      )
      expect_identical(observed, expected,
                       info = paste(residuals, weighted))
    }
    mean_result <- wiseapp::aggregate_pipeline_per_year(
      pipe, method = "mean", weighted = TRUE, residuals = residuals,
      is_log = TRUE, seed = 456L
    )
    total_result <- wiseapp::aggregate_pipeline_per_year(
      pipe, method = "total", weighted = TRUE, residuals = residuals,
      is_log = TRUE, seed = 456L
    )
    expect_equal(
      vapply(total_result, `[[`, numeric(1L), "value"),
      vapply(mean_result, `[[`, numeric(1L), "value") * vapply(
        seq_along(mean_result),
        function(i) {
          yr <- mean_result[[i]]$sim_year
          idx <- pipe$sim_year == yr & !is.na(pipe$y_point)
          sum(pipe$weight[idx])
        },
        numeric(1L)
      ),
      tolerance = 1e-12
    )
  }
})

test_that("Results weighting arms remain lazy until a consumer requests them", {
  skip_if_not_installed("shiny")
  n <- 400L
  set.seed(7)
  hist_sim <- shiny::reactiveVal(list(
    so = list(type = "numeric", name = "welfare", transform = "log"),
    residuals = "none",
    has_weights = TRUE,
    pipeline = list(
      sim_year = rep(2020:2021, each = n / 2L),
      y_point = rnorm(n, 1.2, 0.4),
      weight = rep(c(1, 2), length.out = n),
      F_loading = matrix(rnorm(2L * n) * 0.01, nrow = n),
      train_aug = NULL, id_vec = NULL, id_col = NULL
    )
  ))

  shiny::testServer(
    wiseapp:::mod_2_02_results_server,
    args = list(
      id = "results", hist_sim = hist_sim,
      saved_scenarios = shiny::reactiveVal(list()),
      selected_hist = shiny::reactiveVal(NULL),
      tabset_id = "step2_output_tabs"
    ),
    {
      session$flushReact()
      value <- .get_hist_agg("median")
      expect_false(attr(value$unweighted, "state", exact = TRUE)$built)
      expect_false(attr(value$weighted, "state", exact = TRUE)$built)

      unweighted <- value$unweighted[["median"]]
      expect_true(attr(value$unweighted, "state", exact = TRUE)$built)
      expect_false(attr(value$weighted, "state", exact = TRUE)$built)
      weighted <- value$weighted[["median"]]
      expect_true(attr(value$weighted, "state", exact = TRUE)$built)
      expect_identical(names(unweighted), names(weighted))
    }
  )
})

test_that("bounded aggregation preparation and method caches evict oldest entries", {
  cache <- wiseapp:::.new_aggregation_preparation_cache(max_entries = 2L)
  pipe <- make_w3a_pipeline()
  for (seed in 1:3) {
    wiseapp:::.aggregation_prepare_pipeline(
      pipe = pipe, train_aug = pipe$train_aug, id_col = pipe$id_col,
      residuals = "none", seed = seed, is_log = TRUE, cache = cache
    )
  }
  expect_length(cache$keys, 2L)
  expect_length(cache$entries, 2L)

  expect_error(
    wiseapp:::.aggregation_prepare_pipeline(
      pipe = pipe, train_aug = pipe$train_aug, id_col = pipe$id_col,
      residuals = "none", seed = 1L, is_log = TRUE, cache = cache
    ),
    NA
  )
})

test_that("aggregation preparation reuses residual variance across years", {
  pipe <- make_w3a_pipeline()
  prepared <- wiseapp:::.aggregation_prepare_pipeline(
    pipe = pipe, train_aug = pipe$train_aug, id_col = pipe$id_col,
    residuals = "normal", seed = 1L, is_log = TRUE
  )

  expect_identical(prepared$resid_sigma2,
                   stats::var(pipe$train_aug$.resid, na.rm = TRUE))

  observed <- aggregate_pipeline_per_year(
    pipe, method = "mean", residuals = "normal", seed = 1L
  )
  expected <- lapply(observed, function(x) x$var_resid)
  expect_true(all(is.finite(unlist(expected))))
})
