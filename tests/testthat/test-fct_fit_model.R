# ============================================================================ #
# tests/testthat/test-fct_fit_model.R                                          #
# fit_model() / run_lasso_selection() sample and role-flag regressions.        #
# ============================================================================ #

library(testthat)

test_that("RIF is computed on the complete-case estimation sample (CR-BUG-03)", {

  set.seed(303)
  n <- 4000L
  welfare <- stats::rnorm(n)
  ctrl <- stats::rnorm(n)
  # Welfare-correlated missingness: richer households lose the control more
  # often, so the complete-case sample has a lower welfare distribution.
  ctrl[stats::runif(n) < stats::plogis(2 * welfare - 1)] <- NA
  df <- data.frame(welfare = welfare, temp = stats::rnorm(n), ctrl = ctrl)

  so <- list(name = "welfare", type = "numeric")
  sw <- data.frame(name = "temp", cont_binned = "Continuous",
                   stringsAsFactors = FALSE)
  sm <- build_selected_model(
    model_type    = "Unconditional quantile regression (RIF)",
    engine        = "rif",
    hh_covariates = "ctrl"
  )

  mf <- fit_model(df, so, sw, sm)
  td <- mf$train_data
  expect_false(anyNA(td$ctrl))
  expect_lt(nrow(td), n)

  for (tau in c(0.1, 0.5, 0.9)) {
    col <- paste0("rif_", formatC(tau * 100, format = "d"))
    q_est <- unname(stats::quantile(td$welfare, tau, type = 7))
    # E[RIF] = q_tau on the sample the RIF was built from; only the
    # discreteness term (tau - F_n(q)) / f(q) remains, which is O(1/n).
    expect_equal(mean(td[[col]]), q_est, tolerance = 5e-3)
  }
})
