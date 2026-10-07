# ============================================================================ #
# tests/testthat/test-fct-sim-diag-ridges.R                                    #
# Fixed-grid KDE preparation for simulation diagnostic ridges.                 #
# ============================================================================ #

library(testthat)

make_ridge_sim_inputs <- function(n = 300L) {
  set.seed(31)
  hist <- data.frame(
    year = rep(c(2018L, 2019L), each = n),
    welfare = c(rnorm(n, 2, 0.4), rnorm(n, 2.3, 0.45)),
    stringsAsFactors = FALSE
  )
  scenario <- list(
    SSP2_2030 = list(
      preds = data.frame(
        year = rep(c(2018L, 2019L), each = n),
        welfare = c(rnorm(n, 2.1, 0.4), rnorm(n, 2.5, 0.45)),
        stringsAsFactors = FALSE
      ),
      so = list()
    )
  )
  list(hist = hist, scenario = scenario)
}

test_that("simulation ridge data precomputes bounded curves", {
  x <- make_ridge_sim_inputs()
  out <- build_ridge_kde_data(
    x$hist, x$scenario, "welfare", actual_vals = rnorm(500, 2, 0.4)
  )

  expect_type(out, "list")
  expect_true(is.finite(out$global_bw))
  expect_length(out$ridge_curves$hist, 2L)
  expect_length(out$ridge_curves$scenario[["SSP2_2030"]], 2L)
  expect_equal(nrow(out$ridge_curves$hist[[1]]), 512L)
  expect_equal(nrow(out$ridge_curves$predicted), 512L)
  expect_true(all(is.finite(out$ridge_curves$hist[[1]]$density_raw)))
})
