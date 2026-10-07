# ============================================================================ #
# CR-BUG-02: one definition of the model scale for LCU outcomes.               #
#                                                                              #
# Survey welfare is stored in 2021 PPP. A log-transformed LCU outcome is       #
# trained on log(welfare * ppp2021), but the RIF quantile assignment, the      #
# decomposition channels and the SP transfer used the stored PPP values, so    #
# every household was ranked against the wrong end of the training ECDF.       #
# ============================================================================ #

library(testthat)

so_ppp <- list(name = "welfare", transform = "log", units = "PPP")
so_lcu <- list(name = "welfare", transform = "log", units = "LCU")


# ---- helpers ----------------------------------------------------------------

test_that("an LCU log outcome moves to the outcome currency, others do not", {
  y <- c(1, 2, 4)
  ppp <- c(200, 220, 240)

  expect_equal(outcome_level_scale(y, so_lcu, ppp), y * ppp)
  expect_equal(outcome_to_model_scale(y, so_lcu, ppp), log(y * ppp))

  # PPP outcomes, missing deflators and non-log outcomes pass through.
  expect_identical(outcome_level_scale(y, so_ppp, ppp), y)
  expect_equal(outcome_to_model_scale(y, so_ppp, ppp), log(y))
  expect_identical(outcome_level_scale(y, so_lcu, NULL), y)
  expect_identical(
    outcome_level_scale(y, list(units = "LCU", transform = "none"), ppp), y
  )
  expect_identical(outcome_level_scale(y, NULL, ppp), y)
  expect_identical(outcome_to_model_scale(y, list(transform = "none")), y)
})

test_that("the floor guards the log and a length mismatch is refused", {
  expect_equal(outcome_to_model_scale(0, so_ppp, floor = 1e-10), log(1e-10))
  expect_equal(outcome_to_model_scale(-1, so_lcu, 200, floor = 1e-10), log(1e-10))
  expect_error(outcome_level_scale(1:3, so_lcu, c(200, 210)), "one value per")
})

test_that("an SP transfer is brought onto the outcome scale with the same helper", {
  base <- data.frame(welfare = c(2, 4), ppp2021 = c(200, 300))
  pol <- base
  pol[[SP_TRANSFER_COL]] <- c(0.5, 1)
  expect_equal(.policy_sp_transfer(pol, base, so_lcu), c(100, 300))
  expect_equal(.policy_sp_transfer(pol, base, so_ppp), c(0.5, 1))
  # No transfer column: zeros.
  expect_equal(.policy_sp_transfer(base, base, so_lcu), c(0, 0))
})

test_that("training preparation and the helper agree", {
  df <- data.frame(welfare = c(1.5, 3, 6), ppp2021 = c(210, 210, 250))
  prepared <- prepare_outcome_df(df, as.data.frame(so_lcu))
  expect_equal(prepared$welfare, outcome_to_model_scale(df$welfare, so_lcu, df$ppp2021))
  # PPP and no-deflator inputs keep their previous behaviour.
  expect_equal(prepare_outcome_df(df, as.data.frame(so_ppp))$welfare, log(df$welfare))
  nod <- df["welfare"]
  expect_equal(prepare_outcome_df(nod, as.data.frame(so_lcu))$welfare, log(nod$welfare))
})

test_that("a baseline on the wrong scale is refused, a plausible one is not", {
  train <- log(c(4, 5, 6, 8) * 200)
  expect_error(
    .assert_outcome_scales_match(log(c(4, 5, 6, 8)), train, TRUE, "x"),
    "different scales"
  )
  expect_silent(.assert_outcome_scales_match(log(c(3, 5, 7, 9) * 200), train, TRUE, "x"))
  expect_silent(.assert_outcome_scales_match(c(NA, NA), train, TRUE, "x"))
  # Level outcomes compare by ratio.
  expect_error(.assert_outcome_scales_match(c(1, 2), c(500, 900), FALSE, "x"))
  expect_silent(.assert_outcome_scales_match(c(400, 800), c(500, 900), FALSE, "x"))
})


# ---- RIF quantile assignment (predict_rif) ----------------------------------

rif_fixture <- function(currency, ppp = 200, n = 240, n_svy = 60) {
  set.seed(20261007)
  w_ppp <- stats::rlnorm(n, log(4), 0.6)
  df <- data.frame(
    temp = rnorm(n), rain = rnorm(n),
    loc = factor(sample(letters[1:4], n, TRUE)),
    year = factor(sample(2010:2015, n, TRUE))
  )
  # Training outcome on the model scale: log of the outcome-currency level.
  df$welfare <- log(w_ppp * if (currency == "LCU") ppp else 1)
  taus <- seq(0.1, 0.9, by = 0.1)
  rif_cols <- paste0("rif_", formatC(taus * 100, format = "d"))
  for (i in seq_along(taus)) df[[rif_cols[i]]] <- compute_rif(df$welfare, taus[i])
  fml <- stats::as.formula(paste0(
    "c(", paste(rif_cols, collapse = ", "), ") ~ temp + rain | loc + year"
  ))
  fit_multi <- fixest::feols(fml, data = df, warn = FALSE)
  # The survey keeps the stored (PPP) outcome, as in the app.
  svy <- df[seq_len(n_svy), c("temp", "rain", "loc", "year")]
  svy$welfare <- w_ppp[seq_len(n_svy)]
  svy$ppp2021 <- ppp
  svy$.svy_row_id <- seq_len(n_svy)
  newdata <- svy
  newdata$temp <- svy$temp + 1
  list(fit_multi = fit_multi, train = df, svy = svy, newdata = newdata, taus = taus)
}

run_predict_rif <- function(fx, so) {
  predict_rif(
    fit_multi = fx$fit_multi, newdata = fx$newdata, svy = fx$svy,
    train_data = fx$train, taus = fx$taus, outcome = "welfare",
    weather_cols = c("temp", "rain"), so = so
  )
}

test_that("an LCU RIF model assigns the same quantiles as the PPP model", {
  skip_if_not_installed("fixest")
  ppp_fx <- rif_fixture("PPP")
  lcu_fx <- rif_fixture("LCU")
  res_ppp <- run_predict_rif(ppp_fx, so_ppp)
  res_lcu <- run_predict_rif(lcu_fx, so_lcu)

  # Same households, same ranks, so the same weather effect; the LCU
  # prediction is the PPP one shifted by log(ppp2021).
  # (Tolerance reflects the kernel density inside the RIF, whose grid shifts
  # with the outcome.)
  expect_equal(res_lcu$.fitted - log(200), res_ppp$.fitted, tolerance = 1e-4)
  expect_equal(
    res_lcu$.fitted - log(lcu_fx$svy$welfare * 200),
    res_ppp$.fitted - log(ppp_fx$svy$welfare),
    tolerance = 1e-4
  )
})

test_that("an LCU model against a PPP-scale baseline fails loudly", {
  skip_if_not_installed("fixest")
  lcu_fx <- rif_fixture("LCU")
  # The old behaviour: the stored baseline is used with an outcome that says PPP.
  expect_error(run_predict_rif(lcu_fx, so_ppp), "different scales")
})


# ---- Decomposition channels -------------------------------------------------

decomp_fixture <- function(currency, ppp = 200, n = 40) {
  # Irregular values: with an evenly spaced grid, a household plus its transfer
  # can land exactly on another household's value, and one ulp of floating
  # point then decides an ECDF tie.
  set.seed(31)
  w <- sort(stats::runif(n, 1, 10))
  baseline <- data.frame(
    welfare = w, temp = rep(c(20, 30), each = n / 2),
    electricity = rep(c(0, 1), n / 2)
  )
  baseline$ppp2021 <- ppp
  policy <- baseline
  policy$electricity <- 1L
  # The transfer is stored on the PPP welfare scale in both worlds.
  policy[[SP_TRANSFER_COL]] <- stats::runif(n, 0, 1)
  taus <- c(0.1, 0.5, 0.9)
  terms <- c("temp", "electricity", "temp:electricity")
  grid <- expand.grid(
    model = 3L, term = terms, tau = taus,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  grid$estimate <- with(grid, c(
    temp = 0.01, electricity = 0.25, `temp:electricity` = 0.02
  )[term] * (1 + tau))
  grid$std.error <- 0.05
  train <- data.frame(
    welfare = log(w * if (currency == "LCU") ppp else 1)
  )
  list(
    baseline = baseline, policy = policy, weather = data.frame(temp = c(15, 35)),
    model_fit = list(
      engine = "rif", weather_terms = "temp", rif_grid = grid, taus = taus,
      train_data = train
    )
  )
}

test_that("RIF policy channels are the same in PPP and LCU, with the transfer converted", {
  ppp_fx <- decomp_fixture("PPP")
  lcu_fx <- decomp_fixture("LCU")

  d_ppp <- .policy_central_delta(
    ppp_fx$baseline, ppp_fx$policy, ppp_fx$model_fit, so_ppp,
    weather_raw = ppp_fx$weather
  )
  d_lcu <- .policy_central_delta(
    lcu_fx$baseline, lcu_fx$policy, lcu_fx$model_fit, so_lcu,
    weather_raw = lcu_fx$weather
  )
  expect_true(all(is.finite(d_lcu)))
  # Ratios of effect to welfare do not depend on the currency.
  expect_equal(d_lcu, d_ppp, tolerance = 1e-10)

  # The full decomposition agrees with the central kernel in LCU too.
  full <- decompose_policy_effect(
    lcu_fx$baseline, lcu_fx$policy, lcu_fx$model_fit, so_lcu,
    weather_raw = lcu_fx$weather
  )
  expect_identical(d_lcu, full$delta_total)
})

test_that("the decomposition context carries model-scale and level baselines", {
  lcu_fx <- decomp_fixture("LCU")
  ctx <- .build_decomposition_context(
    lcu_fx$baseline, lcu_fx$policy, lcu_fx$model_fit, so_lcu,
    skip_coef = TRUE, run_identity = "scale-test"
  )
  expect_equal(ctx$y_baseline, log(lcu_fx$baseline$welfare * 200))
  expect_equal(ctx$y_level_baseline, lcu_fx$baseline$welfare * 200)
  expect_equal(ctx$sp_transfer, lcu_fx$policy[[SP_TRANSFER_COL]] * 200)
  expect_identical(ctx$so$units, "LCU")

  ppp_fx <- decomp_fixture("PPP")
  ctx_ppp <- .build_decomposition_context(
    ppp_fx$baseline, ppp_fx$policy, ppp_fx$model_fit, so_ppp,
    skip_coef = TRUE, run_identity = "scale-test"
  )
  expect_equal(ctx_ppp$y_baseline, log(ppp_fx$baseline$welfare))
  expect_equal(ctx_ppp$y_level_baseline, ppp_fx$baseline$welfare)
})

test_that("the decomposition context refuses a baseline on the wrong scale", {
  lcu_fx <- decomp_fixture("LCU")
  # Model trained in LCU, outcome metadata says PPP: the old silent mismatch.
  expect_error(
    .build_decomposition_context(
      lcu_fx$baseline, lcu_fx$policy, lcu_fx$model_fit, so_ppp,
      skip_coef = TRUE, run_identity = "scale-test"
    ),
    "different scales"
  )
})

test_that("level effects are reported in the outcome currency", {
  ppp_fx <- decomp_fixture("PPP")
  lcu_fx <- decomp_fixture("LCU")
  d_ppp <- decompose_policy_effect(
    ppp_fx$baseline, ppp_fx$policy, ppp_fx$model_fit, so_ppp,
    weather_raw = ppp_fx$weather
  )
  d_lcu <- decompose_policy_effect(
    lcu_fx$baseline, lcu_fx$policy, lcu_fx$model_fit, so_lcu,
    weather_raw = lcu_fx$weather
  )
  lvl_ppp <- .decomposition_outcome_channels(d_ppp, so_ppp, ppp_fx$baseline)
  lvl_lcu <- .decomposition_outcome_channels(d_lcu, so_lcu, lcu_fx$baseline)
  expect_gt(nrow(lvl_lcu), 0L)
  # Before the fix the LCU effects were multiplied by the PPP baseline.
  expect_equal(lvl_lcu$total, lvl_ppp$total * 200, tolerance = 1e-8)
  expect_equal(lvl_lcu$main, lvl_ppp$main * 200, tolerance = 1e-8)

  expect_equal(
    .decomposition_baseline_mean(so_lcu, lcu_fx$baseline),
    .decomposition_baseline_mean(so_ppp, ppp_fx$baseline) * 200
  )
})

test_that("the OLS channels are currency-invariant too", {
  set.seed(7)
  n <- 80
  w <- exp(rnorm(n, log(4), 0.3))
  mk <- function(ppp) {
    b <- data.frame(welfare = w, temp = rnorm(n, 25, 2),
                    electricity = rbinom(n, 1, 0.4))
    b$ppp2021 <- ppp
    p <- b
    p$electricity <- 1L
    p[[SP_TRANSFER_COL]] <- seq(0, 0.5, length.out = n)
    b$.y <- NULL
    list(b = b, p = p)
  }
  set.seed(7)
  a <- mk(1)
  fit_ppp <- lm(log(welfare) ~ temp * electricity, data = a$b)
  fit_lcu <- lm(I(log(welfare * 200)) ~ temp * electricity, data = a$b)
  mf <- function(fit) list(engine = "fixest", fit3 = fit, weather_terms = "temp",
                           train_data = a$b)
  b200 <- a$b; b200$ppp2021 <- 200
  p200 <- a$p; p200$ppp2021 <- 200
  d_ppp <- .policy_central_delta(a$b, a$p, mf(fit_ppp), so_ppp,
                                 weather_raw = data.frame(temp = c(22, 28)))
  d_lcu <- .policy_central_delta(b200, p200, mf(fit_lcu), so_lcu,
                                 weather_raw = data.frame(temp = c(22, 28)))
  expect_equal(d_lcu, d_ppp, tolerance = 1e-10)
})

test_that("PPP outcomes are unchanged by the scale helpers (bit-identical)", {
  ppp_fx <- decomp_fixture("PPP")
  no_ppp_col <- ppp_fx
  no_ppp_col$baseline$ppp2021 <- NULL
  no_ppp_col$policy$ppp2021 <- NULL
  a <- .policy_central_delta(
    ppp_fx$baseline, ppp_fx$policy, ppp_fx$model_fit, so_ppp,
    weather_raw = ppp_fx$weather
  )
  b <- .policy_central_delta(
    no_ppp_col$baseline, no_ppp_col$policy, no_ppp_col$model_fit,
    list(name = "welfare", transform = "log"), weather_raw = no_ppp_col$weather
  )
  expect_identical(a, b)
})

test_that("LCU channels are deterministic for the same inputs", {
  lcu_fx <- decomp_fixture("LCU")
  run <- function() .policy_central_delta(
    lcu_fx$baseline, lcu_fx$policy, lcu_fx$model_fit, so_lcu,
    weather_raw = lcu_fx$weather
  )
  expect_identical(run(), run())
})
