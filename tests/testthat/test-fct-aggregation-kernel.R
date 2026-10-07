# Single-pass welfare kernel parity + integration tests ----
# The kernel (src/welfare_stats.cpp) computes the nine resolve_agg_fn()
# statistics in one scan + one stable sort per group. Parity with the
# per-method resolver is enforced to float tolerance (determinism-tolerance
# style), and the multi-method suite path must stay within the same tolerance
# of the single-method oracle path.

test_that("welfare_stats_suite matches resolve_agg_fn on random draws", {
  set.seed(20260918)
  methods <- c(
    "mean", "median", "total", "headcount_ratio", "gap", "fgt2",
    "gini", "prosperity_gap", "avg_poverty"
  )
  for (rep in seq_len(40)) {
    n <- sample(c(2, 3, 7, 50, 500, 5000, 40000), 1)
    y <- exp(rnorm(n, rnorm(1, 1, 1), runif(1, 0.2, 2)))
    # occasional missing values in welfare (never weights: resolver
    # propagates NA weights into NA results; covered separately below)
    if (runif(1) < 0.3) y[sample(n, max(1L, n %/% 50L))] <- NA_real_
    w <- if (runif(1) < 0.7) runif(n, 0.1, 4) else NULL
    pov <- sample(c(3, 3.25, 0.5, 6.85, 1.9), 1)
    st <- welfare_stats_suite(y, w, pov_line = pov)
    for (m in methods) {
      ref <- resolve_agg_fn(m)(y, w, pov)
      expect_equal(unname(st[[m]]), ref, tolerance = 1e-10,
        info = sprintf("rep=%d n=%d method=%s weighted=%s", rep, n, m, !is.null(w)))
    }
    expect_identical(as.integer(attr(st, "order")), as.integer(order(y)))
  }
})

test_that("welfare_stats_suite order attribute is deterministic", {
  set.seed(1)
  y <- c(3.2, 1.1, 1.1, NA_real_, 7.7, 0.9)
  w <- c(1, 2, 0.5, 1, 1.5, 1)
  a <- welfare_stats_suite(y, w, pov_line = 3)
  b <- welfare_stats_suite(y, w, pov_line = 3)
  expect_identical(a, b)
  # ascending values, ties keep original row order, NA sorts last
  expect_identical(as.integer(attr(a, "order")), c(6L, 2L, 3L, 1L, 5L, 4L))
})

test_that("multi-method suite path matches the single-method oracle", {
  set.seed(7)
  n_per_year <- 4000
  years <- 2015:2017
  n_total <- n_per_year * length(years)
  y_point <- log(exp(rnorm(n_total, 1.2, 0.7)))
  w <- runif(n_per_year, 0.2, 3)
  sim_year <- rep(years, each = n_per_year)
  F_loading <- matrix(rnorm(n_total * 8, sd = 0.01), nrow = n_total)
  pipe <- list(
    y_point = y_point,
    sim_year = sim_year,
    weight = rep(w, length(years)),
    F_loading = F_loading,
    id_vec = rep(seq_len(n_per_year), length(years)),
    id_col = "hhid",
    train_aug = data.frame(hhid = seq_len(n_per_year),
      .resid = rnorm(n_per_year, 0, 0.15))
  )
  methods <- c(
    "mean", "median", "total", "headcount_ratio", "gap", "fgt2",
    "gini", "prosperity_gap", "avg_poverty"
  )
  pov_lines <- setNames(rep(3, length(methods)), methods)

  for (skip_coef in c(FALSE, TRUE)) {
    for (residuals in c("none", "original")) {
      multi <- aggregate_pipeline_per_year_multi(
        pipe, methods, weighted = TRUE, pov_lines = pov_lines,
        residuals = residuals, is_log = TRUE, skip_coef = skip_coef,
        seed = 42
      )
      for (m in methods) {
        single <- aggregate_pipeline_per_year(
          pipe, m, weighted = TRUE, pov_line = 3,
          residuals = residuals, is_log = TRUE, skip_coef = skip_coef,
          seed = 42
        )
        for (i in seq_along(years)) {
          a <- multi[[m]][[i]]
          b <- single[[i]]
          expect_equal(a$value, b$value, tolerance = 1e-12,
            info = sprintf("method=%s year=%d skip_coef=%s residuals=%s",
              m, years[[i]], skip_coef, residuals))
          expect_equal(a$value_lo, b$value_lo, tolerance = 1e-12)
          expect_equal(a$value_hi, b$value_hi, tolerance = 1e-12)
          expect_equal(a$var_coef, b$var_coef, tolerance = 1e-12)
          expect_equal(a$var_resid, b$var_resid, tolerance = 1e-12)
          if (!skip_coef && !is.null(b$F_agg)) {
            expect_equal(a$F_agg, b$F_agg, tolerance = 1e-12)
          }
        }
      }
    }
  }
})

test_that("multi-method point-estimate path matches the oracle", {
  set.seed(11)
  n_per_year <- 3000
  years <- 2020:2021
  y_point <- rnorm(n_per_year * length(years), 1.0, 0.5)
  w <- runif(n_per_year, 0.2, 3)
  sim_year <- rep(years, each = n_per_year)
  pipe <- list(
    y_point = y_point, sim_year = sim_year, weight = rep(w, length(years))
  )
  methods <- c("mean", "median", "gini", "gap", "headcount_ratio")
  pov_lines <- setNames(rep(2, length(methods)), methods)
  multi <- aggregate_pipeline_per_year_multi(
    pipe, methods, weighted = TRUE, pov_lines = pov_lines,
    residuals = "none", skip_coef = TRUE, seed = 5
  )
  for (m in methods) {
    single <- aggregate_pipeline_per_year(
      pipe, m, weighted = TRUE, pov_line = 2,
      residuals = "none", skip_coef = TRUE, seed = 5
    )
    for (i in seq_along(multi[[m]])) {
      expect_equal(multi[[m]][[i]]$value, single[[i]]$value, tolerance = 1e-12)
    }
  }
})

test_that("kernel handles pathological groups without crashing", {
  # all-NA welfare
  st <- welfare_stats_suite(c(NA_real_, NA_real_), c(1, 1), pov_line = 3)
  expect_true(all(is.na(st[c("mean", "median", "headcount_ratio")])))
  # single row
  st1 <- welfare_stats_suite(2.5, 1, pov_line = 3)
  expect_equal(unname(st1[["mean"]]), 2.5)
  expect_equal(unname(st1[["total"]]), 2.5)
  expect_true(is.na(st1[["gini"]]))
  # zeros and negatives (28/0 = Inf feeds the prosperity mean, as in R)
  st0 <- welfare_stats_suite(c(0, -1, 2, 3), c(1, 1, 1, 1), pov_line = 3)
  expect_equal(unname(st0[["headcount_ratio"]]), 0.75)
  expect_equal(unname(st0[["prosperity_gap"]]), Inf)
  expect_equal(unname(st0[["avg_poverty"]]), mean(1 / c(2, 3)))
  # infinite welfare keeps Inf semantics of weighted.mean
  st_inf <- welfare_stats_suite(c(1, Inf, 3), c(1, 1, 1), pov_line = 3)
  expect_equal(unname(st_inf[["mean"]]), Inf)
  # zero weights: weighted.mean denominator zero -> NaN, as in R
  st_zw <- welfare_stats_suite(c(1, 2, 3), c(0, 0, 0), pov_line = 3)
  expect_equal(unname(st_zw[["mean"]]), 0 / 0)
  # NA weights poison weighted stats (resolver behaviour)
  st_naw <- welfare_stats_suite(c(1, 2, 3), c(1, NA_real_, 1), pov_line = 3)
  expect_true(is.na(st_naw[["mean"]]))
  expect_true(is.na(st_naw[["headcount_ratio"]]))
  expect_true(is.na(st_naw[["gini"]]))
  # empty group: NaN/NA non-values, no crash
  st_e <- welfare_stats_suite(numeric(0), numeric(0), pov_line = 3)
  expect_true(length(st_e) == 9L)
})

test_that("uncovered methods and missing poverty lines keep resolver errors", {
  pipe <- list(
    y_point = c(1, 2, 3), sim_year = c(2020, 2020, 2020),
    weight = c(1, 1, 1)
  )
  expect_error(
    aggregate_pipeline_per_year_multi(pipe, "made_up_method"),
    "Unknown method"
  )
  expect_error(
    aggregate_pipeline_per_year_multi(pipe, "headcount_ratio", pov_line = NULL),
    "pov_line required"
  )
  expect_error(
    aggregate_pipeline_per_year_multi(pipe, "gap", pov_line = NULL),
    "pov_line required"
  )
})

test_that("one Gini definition across resolver, kernel and aggregate_outcome (CR-BUG-10)", {
  # Standard sample Gini for 1:4 is 0.25; the old unweighted rank form
  # returned 0.25 + 1/n = 0.5.
  y <- c(4, 1, 3, 2)
  expect_equal(resolve_agg_fn("gini")(y, NULL, NULL), 0.25)
  expect_equal(unname(welfare_stats_suite(y)[["gini"]]), 0.25)
  expect_equal(resolve_agg_fn("gini")(y, rep(2, 4), NULL), 0.25)
  df <- data.frame(sim_year = 1L, y = y, wt = c(1, 2, 3, 4))
  expect_equal(aggregate_outcome(df, "y", aggregate = "gini")$value, 0.25)
  expect_equal(
    aggregate_outcome(df, "y", aggregate = "gini", weights = "wt")$value,
    resolve_agg_fn("gini")(y, df$wt, NULL)
  )
})

test_that("weighted median ignores the weight of missing welfare rows (CR-BUG-10)", {
  # Without the NA row the weighted median of 1:4 (weights 1) is 2. The NA
  # row carried weight 10 into the denominator, so the 0.5 crossing fell on
  # the missing row and the result was NA before the fix.
  y <- c(1, 2, NA, 3, 4)
  w <- c(1, 1, 10, 1, 1)
  expect_equal(resolve_agg_fn("median")(y, w, NULL), 2)
  expect_equal(unname(welfare_stats_suite(y, w)[["median"]]), 2)
})
