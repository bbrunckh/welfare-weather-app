# Tests for predict_rif() and interpolate_delta()

test_that("interpolate_delta works correctly", {
  taus <- seq(0.1, 0.9, by = 0.1)
  K <- length(taus)
  n <- 5
  delta_mat <- matrix(2.0, nrow = n, ncol = K)
  tau_i <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  result <- interpolate_delta(delta_mat, taus, tau_i)
  expect_equal(result, rep(2.0, n))
  delta_mat2 <- matrix(rep(taus, each = n), nrow = n, ncol = K)
  tau_i2 <- c(0.1, 0.15, 0.5, 0.85, 0.9)
  result2 <- interpolate_delta(delta_mat2, taus, tau_i2)
  expect_equal(result2, tau_i2, tolerance = 1e-10)
})

test_that("predict_rif returns correct structure", {
  skip_if_not_installed("fixest")
  set.seed(42)
  n <- 200
  df <- data.frame(y = rnorm(n, 10, 2), temp = rnorm(n), rain = rnorm(n), loc = factor(sample(letters[1:4], n, replace = TRUE)), year = factor(sample(2010:2015, n, replace = TRUE)))
  taus <- seq(0.1, 0.9, by = 0.1)
  rif_cols <- paste0("rif_", formatC(taus * 100, format = "d"))
  for (i in seq_along(taus)) df[[rif_cols[i]]] <- compute_rif(df$y, taus[i])
  lhs <- paste0("c(", paste(rif_cols, collapse = ", "), ")")
  fml <- stats::as.formula(paste(lhs, "~ temp + rain | loc + year"))
  fit_multi <- fixest::feols(fml, data = df, warn = FALSE)
  svy <- df[1:50, ]
  svy$.svy_row_id <- seq_len(nrow(svy))
  newdata <- svy
  newdata$temp <- svy$temp + 1
  result <- predict_rif(fit_multi = fit_multi, newdata = newdata, svy = svy, train_data = df, taus = taus, outcome = "y", weather_cols = c("temp", "rain"))
  expect_true(is.data.frame(result))
  expect_true(".fitted" %in% names(result))
  expect_true(".residual" %in% names(result))
  expect_true("y" %in% names(result))
  expect_equal(nrow(result), 50)
  expect_true(all(is.na(result$.residual)))
  deltas <- result$.fitted - svy$y[1:50]
  expect_true(any(abs(deltas) > 0.01))
})

test_that("predict_rif delta is ~0 when scenario == baseline weather", {
  skip_if_not_installed("fixest")
  set.seed(123)
  n <- 100
  df <- data.frame(y = rnorm(n, 10, 2), temp = rnorm(n), rain = rnorm(n), loc = factor(sample(letters[1:3], n, replace = TRUE)), year = factor(sample(2010:2012, n, replace = TRUE)))
  taus <- seq(0.1, 0.9, by = 0.1)
  rif_cols <- paste0("rif_", formatC(taus * 100, format = "d"))
  for (i in seq_along(taus)) df[[rif_cols[i]]] <- compute_rif(df$y, taus[i])
  lhs <- paste0("c(", paste(rif_cols, collapse = ", "), ")")
  fml <- stats::as.formula(paste(lhs, "~ temp + rain | loc + year"))
  fit_multi <- fixest::feols(fml, data = df, warn = FALSE)
  svy <- df[1:30, ]
  svy$.svy_row_id <- seq_len(nrow(svy))
  newdata <- svy
  result <- predict_rif(fit_multi = fit_multi, newdata = newdata, svy = svy, train_data = df, taus = taus, outcome = "y", weather_cols = c("temp", "rain"))
  deltas <- result$.fitted - svy$y[1:30]
  expect_true(all(abs(deltas) < 1e-8))
})
