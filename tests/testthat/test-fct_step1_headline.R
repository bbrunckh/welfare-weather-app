# ============================================================================ #
# tests/testthat/test-fct_step1_headline.R                                     #
# RIF heterogeneity screen: clustering of the stacked fit and disclosure of    #
# the stacked-row cap (R2-BUG-10).                                             #
# ============================================================================ #

library(testthat)

.het_fixture <- function(n, cluster = NULL, seed = 10) {
  set.seed(seed)
  loc <- sample(sprintf("L%02d", 1:40), n, replace = TRUE)
  temp <- stats::rnorm(n)
  df <- data.frame(
    welfare = 1 + 0.3 * temp * stats::rnorm(n, 1, 0.5) + stats::rnorm(n),
    temp = temp,
    loc_id_panel = loc,
    stringsAsFactors = FALSE
  )
  sm <- build_selected_model(
    model_type = "Unconditional quantile regression (RIF)",
    engine     = "rif",
    cluster    = cluster
  )
  so <- list(name = "welfare", type = "numeric")
  sw <- data.frame(name = "temp", cont_binned = "Continuous",
                   stringsAsFactors = FALSE)
  mf <- fit_model(df, so, sw, sm)
  list(mf = mf, snap = list(model = sm))
}

.capture_feols_cluster <- function(mf, snap) {
  seen <- NULL
  real_feols <- fixest::feols
  local_mocked_bindings(
    feols = function(...) {
      args <- list(...)
      if (!is.null(args$cluster)) seen <<- args$cluster
      real_feols(...)
    },
    .package = "fixest"
  )
  p <- step1_rif_heterogeneity_p(mf, snap, "temp")
  list(p = p, cluster = seen)
}

test_that("stacked RIF Wald test clusters on the model cluster variable", {
  fx <- .het_fixture(600L, cluster = "loc_id_panel")
  res <- .capture_feols_cluster(fx$mf, fx$snap)
  expect_true(is.numeric(res$p) && is.finite(res$p))
  expect_s3_class(res$cluster, "formula")
  expect_identical(all.vars(res$cluster), "loc_id_panel")
})

test_that("stacked RIF Wald test clusters on the household row without a model cluster", {
  fx <- .het_fixture(600L)
  res <- .capture_feols_cluster(fx$mf, fx$snap)
  expect_true(is.numeric(res$p) && is.finite(res$p))
  expect_identical(all.vars(res$cluster), ".row_id")
})

test_that("stacked-row cap is disclosed instead of returning NULL silently", {
  fx <- .het_fixture(28000L)
  p <- step1_rif_heterogeneity_p(fx$mf, fx$snap, "temp")
  expect_false(is.null(p))
  expect_true(is.na(p))
  expect_match(attr(p, "note"), "250,000 rows")
  expect_match(attr(p, "note"), "28,000 observations x 9 quantiles", fixed = TRUE)
})

test_that("the who-is-affected card shows the cap note", {
  scen <- list(list(
    list(tau = 0.1, estimate = -0.10, se = 0.02, label = NULL),
    list(tau = 0.9, estimate = -0.04, se = 0.02, label = NULL)
  ))
  card <- .s1_who_card(
    mf = list(engine = "rif", model_type = "linear"),
    snap = list(outcome = data.frame(type = "numeric", transform = "log")),
    var = "temp", engine = "rif", scale = "pct", label_fun = identity,
    rif_scenarios = scen, rif_scenarios_cached = TRUE,
    rif_p = structure(NA_real_, note = "CAP NOTE"), rif_p_cached = TRUE
  )
  expect_match(card$info, "CAP NOTE", fixed = TRUE)
})
