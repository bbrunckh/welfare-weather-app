# ============================================================================ #
# tests/testthat/test-fct_fit_model_fallback.R                                 #
# REACT-14: silent specification fallbacks (logistic -> linear, clustered ->   #
# unclustered VCV) are recorded as structured metadata on the fit result so    #
# the Step 1 results banner can disclose them.                                 #
# ============================================================================ #

library(testthat)

# Same fixture as test-determinism.R (defined there, file-local)
make_lasso_fixture <- function(n = 180L) {
  set.seed(71)
  signal <- stats::rnorm(n)
  data.frame(
    welfare = 2 + 2 * signal + 0.2 * stats::rnorm(n),
    temp = stats::rnorm(n),
    signal = signal,
    noise = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
}

test_that("logistic request on a non-logical outcome records a model-family fallback", {
  skip_if_not_installed("fixest")

  df <- make_lasso_fixture(120L)
  so <- list(name = "welfare", type = "numeric")
  sw <- data.frame(name = "temp", cont_binned = "Continuous",
                   stringsAsFactors = FALSE)
  sm <- build_selected_model(model_type = "Logistic regression",
                             engine = "fixest")

  expect_warning(mf <- fit_model(df, so, sw, sm), "falling back to linear")

  fb <- mf$fallbacks
  expect_length(fb, 1)
  expect_identical(fb[[1]]$kind, "model_family")
  expect_identical(fb[[1]]$requested, "logistic")
  expect_identical(fb[[1]]$used, "linear")
  # The fallback also drives the fitted model type (not just metadata)
  expect_identical(mf$model_type, "linear")
})

test_that("missing cluster variable records a VCV fallback", {
  skip_if_not_installed("fixest")

  df <- make_lasso_fixture(120L)
  df$loc_id_panel <- rep(letters[1:8], length.out = nrow(df))
  so <- list(name = "welfare", type = "numeric")
  sw <- data.frame(name = "temp", cont_binned = "Continuous",
                   stringsAsFactors = FALSE)
  sm <- build_selected_model(
    model_type    = "Linear regression",
    engine        = "fixest",
    hh_covariates = c("signal", "noise"),
    cluster       = c("loc_id_panel", "not_in_data")
  )

  expect_warning(mf <- fit_model(df, so, sw, sm),
                 "fitting without clustered SEs")

  fb <- mf$fallbacks
  expect_length(fb, 1)
  expect_identical(fb[[1]]$kind, "vcv")
  expect_match(fb[[1]]$requested, "loc_id_panel")
  expect_match(fb[[1]]$reason, "not_in_data")
  expect_match(fb[[1]]$used, "default")
})

test_that("a clean fit records no fallbacks", {
  skip_if_not_installed("fixest")

  df <- make_lasso_fixture(120L)
  so <- list(name = "welfare", type = "numeric")
  sw <- data.frame(name = "temp", cont_binned = "Continuous",
                   stringsAsFactors = FALSE)
  sm <- build_selected_model(
    model_type    = "Linear regression",
    engine        = "fixest",
    hh_covariates = c("signal", "noise")
  )

  mf <- fit_model(df, so, sw, sm)
  expect_length(mf$fallbacks, 0)
})
