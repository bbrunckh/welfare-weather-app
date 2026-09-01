# Tests for the additive-decomposition active-coefficient mask
# Covers build_active_coef_mask(), detect_modified_cols(), attach_active_mask()
# and their integration with compute_factor_loading() / interpolate_F_loading().

# ---- build_active_coef_mask ------------------------------------------------ #

test_that("build_active_coef_mask flags weather, polynomials, and interactions", {
  coef_names <- c("(Intercept)", "tx", "I(tx^2)", "tx:urban", "urban", "log_age")
  mask <- build_active_coef_mask(coef_names, "tx")
  expect_equal(unname(mask), c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_named(mask, coef_names)
})

test_that("build_active_coef_mask keeps policy variable and its weather interaction", {
  coef_names <- c("(Intercept)", "tx", "tx:electricity", "electricity",
                  "internet", "log_age")
  mask <- build_active_coef_mask(coef_names, c("tx", "electricity"))
  expect_equal(unname(mask), c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))
})

test_that("build_active_coef_mask forces intercept to FALSE", {
  coef_names <- c("(Intercept)", "tx")
  mask <- build_active_coef_mask(coef_names, "tx")
  expect_false(mask[["(Intercept)"]])
  expect_true(mask[["tx"]])
})

test_that("build_active_coef_mask warns and returns NULL on empty active_terms", {
  expect_warning(
    mask <- build_active_coef_mask(c("tx", "urban"), character()),
    "no active terms"
  )
  expect_null(mask)
})

test_that("build_active_coef_mask matches fixest factor expansion via ::", {
  # fixest writes factor levels as "var::level" — boundary match must work
  coef_names <- c("tx_bin::1", "tx_bin::2", "tx_bin::3:urban", "urban")
  mask <- build_active_coef_mask(coef_names, "tx_bin")
  expect_equal(unname(mask), c(TRUE, TRUE, TRUE, FALSE))
})


# ---- detect_modified_cols -------------------------------------------------- #

test_that("detect_modified_cols returns empty when frames are identical", {
  df <- data.frame(hhid = 1:5, electricity = c(0, 1, 0, 1, 0),
                   internet = c(1, 0, 1, 1, 0))
  expect_length(detect_modified_cols(df, df, id_col = "hhid"), 0L)
})

test_that("detect_modified_cols flags exactly the changed columns", {
  train <- data.frame(hhid = 1:5, electricity = c(0, 1, 0, 1, 0),
                      internet = c(1, 0, 1, 1, 0), age = c(30, 40, 50, 60, 70))
  modified <- train
  modified$electricity <- c(1, 1, 1, 1, 1)  # policy flips electricity
  changed <- detect_modified_cols(modified, train, id_col = "hhid")
  expect_equal(changed, "electricity")
})

test_that("detect_modified_cols honours exclude_cols", {
  train <- data.frame(hhid = 1:3, tx = c(20, 22, 24), urban = c(0, 1, 0))
  modified <- train
  modified$tx <- c(30, 32, 34)
  changed <- detect_modified_cols(modified, train,
                                  id_col = "hhid",
                                  exclude_cols = "tx")
  expect_length(changed, 0L)
})

test_that("detect_modified_cols falls back to row-order when id_col missing", {
  train <- data.frame(electricity = c(0, 1, 0), urban = c(0, 1, 0))
  modified <- train
  modified$electricity <- c(1, 1, 1)
  changed <- detect_modified_cols(modified, train, id_col = NULL)
  expect_equal(changed, "electricity")
})


# ---- compute_factor_loading subsetting ------------------------------------- #

test_that("compute_factor_loading subsets columns when active_mask is supplied", {
  set.seed(1)
  K <- 4L
  L <- matrix(c(1, 0, 0, 0,
                0.1, 1, 0, 0,
                0.2, 0.1, 1, 0,
                0.3, 0.2, 0.1, 1),
              nrow = K, byrow = TRUE)
  beta <- setNames(rnorm(K), c("(Intercept)", "tx", "urban", "tx:urban"))
  X <- matrix(rnorm(20), ncol = K,
               dimnames = list(NULL, names(beta)))

  chol_obj <- list(L = L, K = K, beta = beta, spec = "iid")

  # No mask: full K columns
  F_full <- compute_factor_loading(X, chol_obj)
  expect_equal(ncol(F_full), K)

  # With mask: only "tx" and "tx:urban" active.
  # Caller MUST also supply L_active = Cholesky of the active block of
  # Sigma. compute_factor_loading() returns X[, mask] %*% L_active.
  mask <- c(FALSE, TRUE, FALSE, TRUE)
  Sigma <- L %*% t(L)
  L_active <- t(chol(Sigma[mask, mask, drop = FALSE]))
  chol_obj$active_mask <- mask
  chol_obj$L_active    <- L_active
  F_active <- compute_factor_loading(X, chol_obj)
  expect_equal(ncol(F_active), 2L)
  expect_equal(F_active, X[, mask, drop = FALSE] %*% L_active)
})

test_that("active mask gives correct (block-Cholesky) additive-decomposition variance", {
  # Regression test for the column-subset bug: when Sigma has off-diagonal
  # terms between active and inactive coefficients, naively subsetting
  # columns of F = X %*% L gives a DIFFERENT (incorrect) variance than the
  # true additive-decomposition variance x_w' Σ_ww x_w. The correct
  # quantity is computed via X_w %*% L_active where L_active = chol(Σ_ww).
  set.seed(2)
  K <- 5L
  N <- 20L
  Sigma <- crossprod(matrix(rnorm(K * K), K)) + diag(K) * 0.1
  L <- t(chol(Sigma))
  beta <- setNames(rnorm(K), c("(Intercept)", "tx", "tx:urban", "urban", "age"))
  X <- matrix(rnorm(N * K), ncol = K, dimnames = list(NULL, names(beta)))

  chol_obj_full <- list(L = L, K = K, beta = beta, spec = "iid")
  mask <- c(FALSE, TRUE, TRUE, FALSE, FALSE)  # tx + tx:urban
  L_active <- t(chol(Sigma[mask, mask, drop = FALSE]))
  chol_obj_masked <- list(L = L, K = K, beta = beta, spec = "iid",
                           active_mask = mask, L_active = L_active)

  F_full   <- compute_factor_loading(X, chol_obj_full)
  F_masked <- compute_factor_loading(X, chol_obj_masked)
  expect_equal(ncol(F_masked), sum(mask))

  h <- rnorm(N)
  var_masked <- sum(as.numeric(crossprod(F_masked, h))^2)
  # Truth: var = h' X_w Σ_ww X_w' h
  var_truth <- as.numeric(t(h) %*% X[, mask] %*% Sigma[mask, mask] %*% t(X[, mask]) %*% h)
  expect_equal(var_masked, var_truth, tolerance = 1e-10)

  # NB: masked variance is NOT guaranteed to be smaller than full variance.
  # The full level variance is h' X Σ X' h, which decomposes into
  #   active-block + inactive-block + 2 * cross-terms.
  # Cross-terms can be negative when Σ has off-diagonals of opposite sign
  # to X[:, a] X[:, b]', so the masked (active-only) variance can exceed
  # the full level variance. The two answer *different questions*:
  #   - full   = Var(prediction | ALL coefs perturbed)
  #   - masked = Var(prediction | only active coefs perturbed, others fixed)
  # Both are valid SEs of different counterfactual designs.
  var_full <- sum(as.numeric(crossprod(F_full, h))^2)
  expect_true(is.finite(var_full) && is.finite(var_masked))
  expect_gt(var_masked, 0)
})


# ---- attach_active_mask ---------------------------------------------------- #

test_that("attach_active_mask is a no-op when residuals != 'original' (linear engine)", {
  chol_obj <- list(L = matrix(0, 2, 2),
                   beta = setNames(c(1, 1), c("tx", "urban")))
  svy   <- data.frame(hhid = 1:3, tx = c(20, 22, 24), urban = c(0, 1, 0))
  train <- svy
  out <- attach_active_mask(
    chol_obj                            = chol_obj,
    svy_modified                        = svy,
    train_data                          = train,
    weather_terms                       = "tx",
    residuals                           = "normal",
    propagate_all_covariate_uncertainty = FALSE
  )
  expect_null(out$active_mask)
})

test_that("attach_active_mask applies mask for RIF shape even under residuals='none'", {
  # RIF chol_obj is a list of per-tau lists; the prediction y_baseline + delta
  # plays the same role as the held-fixed residual, so masking is valid.
  beta_names <- c("(Intercept)", "tx", "urban")
  per_tau <- list(L = diag(3), beta = setNames(c(1, 1, 1), beta_names))
  chol_obj <- list(per_tau, per_tau, per_tau)
  svy <- data.frame(hhid = 1:3, tx = c(20, 22, 24), urban = c(0, 1, 0))
  out <- attach_active_mask(
    chol_obj                            = chol_obj,
    svy_modified                        = svy,
    train_data                          = svy,
    weather_terms                       = "tx",
    residuals                           = "none",
    propagate_all_covariate_uncertainty = FALSE
  )
  expect_equal(unname(attr(out, "active_mask")),
               c(FALSE, TRUE, FALSE))
})

test_that("attach_active_mask is a no-op when propagate_all_covariate_uncertainty = TRUE", {
  chol_obj <- list(L = matrix(0, 2, 2),
                   beta = setNames(c(1, 1), c("tx", "urban")))
  out <- attach_active_mask(
    chol_obj                            = chol_obj,
    svy_modified                        = data.frame(tx = 1, urban = 1),
    train_data                          = data.frame(tx = 1, urban = 1),
    weather_terms                       = "tx",
    residuals                           = "original",
    propagate_all_covariate_uncertainty = TRUE
  )
  expect_null(out$active_mask)
})

test_that("attach_active_mask builds weather-only mask in Module 2 (no policy diffs)", {
  chol_obj <- list(
    L = diag(3),
    beta = setNames(c(1, 1, 1),
                    c("(Intercept)", "tx", "urban"))
  )
  svy   <- data.frame(hhid = 1:3, tx = c(20, 22, 24), urban = c(0, 1, 0))
  train <- svy
  out <- attach_active_mask(
    chol_obj                            = chol_obj,
    svy_modified                        = svy,
    train_data                          = train,
    weather_terms                       = "tx",
    residuals                           = "original",
    propagate_all_covariate_uncertainty = FALSE
  )
  expect_equal(unname(out$active_mask), c(FALSE, TRUE, FALSE))
})

test_that("attach_active_mask unions weather + policy-modified columns (Module 3)", {
  chol_obj <- list(
    L = diag(4),
    beta = setNames(c(1, 1, 1, 1),
                    c("(Intercept)", "tx", "electricity", "urban"))
  )
  train <- data.frame(hhid = 1:3, tx = c(20, 22, 24),
                      electricity = c(0, 0, 0), urban = c(0, 1, 0))
  svy   <- train
  svy$electricity <- c(1, 1, 1)  # policy flips electricity
  out <- attach_active_mask(
    chol_obj                            = chol_obj,
    svy_modified                        = svy,
    svy_reference                       = train,
    train_data                          = train,
    weather_terms                       = "tx",
    residuals                           = "original",
    propagate_all_covariate_uncertainty = FALSE
  )
  expect_equal(unname(out$active_mask),
               c(FALSE, TRUE, TRUE, FALSE))
})

test_that("attach_active_mask excludes outcome column from auto-detection", {
  # The outcome is log-transformed in train_data but on level scale in svy.
  # Diff would erroneously flag it as "modified" — defensively excluded.
  chol_obj <- list(
    L = diag(3),
    beta = setNames(c(1, 1, 1), c("(Intercept)", "tx", "urban"))
  )
  train <- data.frame(hhid = 1:3, welfare = log(c(100, 200, 300)),
                      tx = c(20, 22, 24), urban = c(0, 1, 0))
  svy <- train
  svy$welfare <- c(100, 200, 300)  # level scale (untransformed)
  out <- attach_active_mask(
    chol_obj                            = chol_obj,
    svy_modified                        = svy,
    svy_reference                       = svy,
    train_data                          = train,
    weather_terms                       = "tx",
    outcome_col                         = "welfare",
    residuals                           = "original",
    propagate_all_covariate_uncertainty = FALSE
  )
  # Only "tx" should be active; "welfare" must not appear as a "modified"
  # column even though its values differ between svy and train_data.
  expect_equal(unname(out$active_mask), c(FALSE, TRUE, FALSE))
})

test_that("similarly-named covariates do not bleed into the weather mask", {
  # Regression test for the regex: "tx" must not match "tax", "context",
  # "tx_old", etc. (only word-boundary matches).
  coef_names <- c("(Intercept)", "tx", "I(tx^2)", "tx:urban",
                  "tax", "context", "tx_old", "ttx", "urban")
  mask <- build_active_coef_mask(coef_names, "tx")
  expect_equal(unname(mask),
               c(FALSE, TRUE, TRUE, TRUE,
                 FALSE, FALSE, FALSE, FALSE, FALSE))
})

test_that("RIF end-to-end: full production chain shrinks var_coef", {
  # Reproduces production flow: fixest_multi fit → compute_chol_vcov →
  # attach_active_mask → run_sim_pipeline-style chol_list extraction →
  # predict_rif → aggregate. Locks in that adding non-weather covariates
  # to a RIF model does NOT inflate var_coef when the additive-decomp
  # gate is on. Regression test for a reported bug where the mask
  # appeared not to reach interpolate_F_loading().
  skip_if_not_installed("fixest")
  set.seed(42)
  n <- 300
  df <- data.frame(
    y = rnorm(n, 10, 2), temp = rnorm(n),
    urban = sample(0:1, n, TRUE), age = rnorm(n, 40, 10),
    hh_size = sample(1:8, n, TRUE),
    loc = factor(sample(letters[1:4], n, TRUE))
  )
  taus <- seq(0.1, 0.9, by = 0.2)
  for (i in seq_along(taus))
    df[[paste0("rif_", i)]] <- compute_rif(df$y, taus[i])
  rif_lhs <- paste0("c(", paste(paste0("rif_", seq_along(taus)),
                                 collapse = ", "), ")")
  fit_multi <- fixest::feols(
    stats::as.formula(paste(rif_lhs, "~ temp + urban + age + hh_size | loc")),
    data = df, warn = FALSE
  )
  chol_obj <- compute_chol_vcov(fit_multi)
  svy <- df[1:50, ]
  svy$.svy_row_id <- seq_len(50)
  newdata <- svy
  newdata$temp <- svy$temp + 1

  # Helper that mimics run_sim_pipeline's chol_list extraction: when an
  # active mask is set, extract L_active (Cholesky of Σ_active block);
  # otherwise extract the full L.
  extract_cl <- function(chol_obj) {
    am <- attr(chol_obj, "active_mask")
    use_active <- !is.null(am) &&
                   all(vapply(chol_obj,
                              function(x) "L_active" %in% names(x),
                              logical(1)))
    tmp <- lapply(chol_obj, function(x)
      if (use_active) x$L_active else x$L)
    if (use_active) attr(tmp, "active_mask") <- am
    tmp
  }

  # Legacy: propagate_all = TRUE → no mask
  chol_legacy <- suppressMessages(attach_active_mask(
    chol_obj = chol_obj, svy_modified = svy, svy_reference = svy,
    train_data = df, weather_terms = "temp", outcome_col = "y",
    residuals = "none", propagate_all_covariate_uncertainty = TRUE
  ))
  expect_null(attr(chol_legacy, "active_mask"))

  # Additive: propagate_all = FALSE → mask {temp}
  chol_adit <- suppressMessages(attach_active_mask(
    chol_obj = chol_obj, svy_modified = svy, svy_reference = svy,
    train_data = df, weather_terms = "temp", outcome_col = "y",
    residuals = "none", propagate_all_covariate_uncertainty = FALSE
  ))
  am <- attr(chol_adit, "active_mask")
  expect_equal(unname(am), c(TRUE, FALSE, FALSE, FALSE))   # temp only

  res_legacy <- predict_rif(
    fit_multi = fit_multi, newdata = newdata, svy = svy,
    train_data = df, taus = taus, outcome = "y", weather_cols = "temp",
    chol_list = extract_cl(chol_legacy)
  )
  res_adit <- predict_rif(
    fit_multi = fit_multi, newdata = newdata, svy = svy,
    train_data = df, taus = taus, outcome = "y", weather_cols = "temp",
    chol_list = extract_cl(chol_adit)
  )
  F_legacy <- attr(res_legacy, "F_loading")
  F_adit   <- attr(res_adit,   "F_loading")
  expect_equal(ncol(F_legacy), 4L)  # all coefs
  expect_equal(ncol(F_adit),   1L)  # temp only

  agg_legacy <- wiseapp:::aggregate_with_uncertainty_delta(
    y_point = res_legacy$.fitted, F_loading = F_legacy,
    method = "mean", weights = rep(1, 50), residuals = "none",
    train_aug = NULL, is_log = FALSE
  )
  agg_adit <- wiseapp:::aggregate_with_uncertainty_delta(
    y_point = res_adit$.fitted, F_loading = F_adit,
    method = "mean", weights = rep(1, 50), residuals = "none",
    train_aug = NULL, is_log = FALSE
  )
  # Additive var_coef must be strictly smaller (non-weather coefs were
  # contributing under legacy). With this fixture we see ~30x reduction.
  expect_lt(agg_adit$var_coef, agg_legacy$var_coef)
  expect_lt(agg_adit$var_coef / agg_legacy$var_coef, 0.5)
})

test_that("end-to-end: masked F_loading shrinks var_coef vs unmasked", {
  # Simulates the full path: build chol_obj with active_mask, run
  # compute_factor_loading, then aggregate_with_uncertainty_delta.
  # Asserts var_coef strictly decreases when the mask drops covariates.
  set.seed(99)
  K <- 6L
  N <- 500L
  Sigma <- crossprod(matrix(rnorm(K * K, 0, 0.05), K, K)) + diag(K) * 0.01
  L <- t(chol(Sigma))
  beta_names <- c("(Intercept)", "tx", "I(tx^2)", "tx:urban", "urban", "age")
  beta <- setNames(rnorm(K, 0, 0.2), beta_names)
  X <- matrix(rnorm(N * K, 0, 0.1), N, K,
              dimnames = list(NULL, beta_names))
  X[, "(Intercept)"] <- 1
  y_point <- as.numeric(X %*% beta) + log(3.0)

  chol_obj_full <- list(L = L, K = K, beta = beta, spec = "iid")
  chol_obj_msk  <- chol_obj_full
  mask <- build_active_coef_mask(beta_names, "tx")
  chol_obj_msk$active_mask <- mask
  chol_obj_msk$L_active <- t(chol(Sigma[mask, mask, drop = FALSE]))

  F_full <- compute_factor_loading(X, chol_obj_full)
  F_msk  <- compute_factor_loading(X, chol_obj_msk)
  expect_equal(ncol(F_full), K)
  expect_equal(ncol(F_msk),  3L)  # tx + I(tx^2) + tx:urban

  res_full <- wiseapp:::aggregate_with_uncertainty_delta(
    y_point = y_point, F_loading = F_full,
    method = "mean", weights = rep(1, N), is_log = TRUE
  )
  res_msk <- wiseapp:::aggregate_with_uncertainty_delta(
    y_point = y_point, F_loading = F_msk,
    method = "mean", weights = rep(1, N), is_log = TRUE
  )

  # Masked SE must be strictly smaller than unmasked (dropping urban + age
  # uncertainty contributions can only shrink var_coef).
  expect_lt(res_msk$var_coef, res_full$var_coef)
  expect_gt(res_msk$var_coef, 0)  # but not zero — weather coefs still active
})
