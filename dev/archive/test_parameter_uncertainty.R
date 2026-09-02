# =============================================================================
# Parameter Uncertainty in Counterfactual Welfare Simulations
# =============================================================================
# Two methods for propagating coefficient uncertainty into simulated welfare
# distributions when holding the population fixed and varying only weather.
#
# Method A: Parametric  — draw from the multivariate normal using the VCV matrix
# Method B: Non-parametric — bootstrap (resample-reestimate-predict)
# =============================================================================

library(fixest)
library(MASS)
library(dplyr)
library(ggplot2)

set.seed(42)


# =============================================================================
# 1. SIMULATE TRAINING DATA
# =============================================================================

n <- 1000

data <- data.frame(
  hh_id  = 1:n,
  income = rnorm(n, mean = 50, sd = 10),
  size   = sample(1:6, n, replace = TRUE),
  temp   = rnorm(n, mean = 22, sd = 5)
)

# True data-generating process: welfare is a linear function of covariates
# beta_temp = -1.5 (the "damage function" coefficient we care most about)
data$welfare <- 100 +
  (2.0 * data$income) +
  (-1.5 * data$temp) +
  rnorm(n, mean = 0, sd = 10)


# =============================================================================
# 2. FIT THE MODEL
# =============================================================================

model <- feols(welfare ~ temp + income + size, data = data)
summary(model)


# =============================================================================
# 3. CONSTRUCT COUNTERFACTUAL DATA
# =============================================================================
# The population is held fixed. Only weather changes.
# Here: a heatwave scenario where temperature rises uniformly by 5°C.

data_cf       <- data
data_cf$temp  <- data_cf$temp + 5

# Design matrix for the counterfactual (N x K)
# model.matrix ensures the intercept and factor encoding match the fitted model
X_cf <- model.matrix(model, data = data_cf)


# =============================================================================
# 4. METHOD A: PARAMETRIC SIMULATION (VCV)
# =============================================================================
# Under standard OLS assumptions, the coefficient vector is asymptotically:
#   beta_hat ~ N(mu, Sigma)
# where mu = point estimates and Sigma = variance-covariance matrix.
#
# We draw S coefficient vectors from this distribution and compute one set of
# welfare predictions per draw. This propagates uncertainty from ALL coefficients
# into the simulated welfare distribution — not just beta_temp.

n_sims <- 1000

mu    <- coef(model)   # K-vector of point estimates
Sigma <- vcov(model)   # K x K variance-covariance matrix

# Draw S plausible coefficient vectors (S x K matrix)
beta_draws_vcv <- mvrnorm(n = n_sims, mu = mu, Sigma = Sigma)

# For each draw, compute predicted welfare for all N households (N x S matrix)
# X_cf %*% t(beta_draws_vcv): each column is one simulated welfare vector
sim_matrix_vcv <- X_cf %*% t(beta_draws_vcv)

# Posterior mean prediction per household (collapsing across draws)
welfare_vcv_mean <- rowMeans(sim_matrix_vcv)

# 95% prediction interval per household (from parameter uncertainty alone)
welfare_vcv_lo   <- apply(sim_matrix_vcv, 1, quantile, probs = 0.025)
welfare_vcv_hi   <- apply(sim_matrix_vcv, 1, quantile, probs = 0.975)

cat("--- Method A: VCV ---\n")
cat("Mean welfare (counterfactual):", round(mean(welfare_vcv_mean), 2), "\n")
cat("Mean welfare (baseline)      :", round(mean(data$welfare), 2), "\n")
cat("Implied welfare loss          :", round(mean(welfare_vcv_mean) - mean(data$welfare), 2), "\n\n")


# =============================================================================
# 5. METHOD B: NON-PARAMETRIC SIMULATION (BOOTSTRAP)
# =============================================================================
# Instead of assuming a multivariate normal sampling distribution, we build it
# empirically by repeatedly:
#   (i)  resampling the training data with replacement
#   (ii) re-estimating the model on the bootstrap sample
#   (iii) predicting welfare for the FIXED counterfactual population
#
# The spread of predictions across S iterations reflects joint uncertainty in
# all coefficients without any distributional assumption.

boot_preds <- matrix(NA, nrow = n, ncol = n_sims)

for (s in 1:n_sims) {
  
  # Resample training data with replacement (same size as original)
  boot_idx    <- sample(1:n, size = n, replace = TRUE)
  boot_sample <- data[boot_idx, ]
  
  # Re-estimate model on bootstrap sample
  boot_mod <- feols(welfare ~ temp + income + size, data = boot_sample)
  
  # Predict for the COUNTERFACTUAL population using this draw's coefficients
  # The prediction target is always data_cf — the population does not change
  boot_preds[, s] <- predict(boot_mod, newdata = data_cf)
}

# Posterior mean and interval from bootstrap
welfare_boot_mean <- rowMeans(boot_preds)
welfare_boot_lo   <- apply(boot_preds, 1, quantile, probs = 0.025)
welfare_boot_hi   <- apply(boot_preds, 1, quantile, probs = 0.975)

cat("--- Method B: Bootstrap ---\n")
cat("Mean welfare (counterfactual):", round(mean(welfare_boot_mean), 2), "\n")
cat("Mean welfare (baseline)      :", round(mean(data$welfare), 2), "\n")
cat("Implied welfare loss          :", round(mean(welfare_boot_mean) - mean(data$welfare), 2), "\n\n")


# =============================================================================
# 6. COMPARE INTERVAL WIDTHS
# =============================================================================
# Both methods should produce similar mean predictions. The key difference is
# whether the interval shape reflects a normal or empirical sampling distribution.

cat("--- Interval width comparison (mean across households) ---\n")
cat("VCV  interval width:", round(mean(welfare_vcv_hi  - welfare_vcv_lo),  2), "\n")
cat("Boot interval width:", round(mean(welfare_boot_hi - welfare_boot_lo), 2), "\n\n")


# =============================================================================
# 7. VISUALISE: WELFARE DISTRIBUTIONS
# =============================================================================

plot_df <- data.frame(
  Type    = rep(c("Baseline (observed)",
                  "Counterfactual — VCV",
                  "Counterfactual — Bootstrap"), each = n),
  Welfare = c(data$welfare, welfare_vcv_mean, welfare_boot_mean)
)

ggplot(plot_df, aes(x = Welfare, fill = Type)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("grey40", "#E69F00", "#56B4E9")) +
  theme_minimal(base_size = 13) +
  labs(
    title    = "Welfare Distribution: Baseline vs. Counterfactual (+5°C Heatwave)",
    subtitle = "Parameter uncertainty propagated across all coefficients via VCV and Bootstrap",
    x        = "Welfare Score",
    y        = "Density",
    fill     = NULL
  )


# =============================================================================
# 8. VISUALISE: COEFFICIENT UNCERTAINTY
# =============================================================================
# Compare the sampling distribution of beta_temp across both methods.
# These should be centred on the same point estimate; the shape may differ.

# VCV draws for temp coefficient
vcv_temp_draws  <- beta_draws_vcv[, "temp"]

# Bootstrap draws for temp coefficient (extract from each refit)
# Re-run bootstrap storing coefficients this time
boot_coefs <- matrix(NA, nrow = n_sims, ncol = length(coef(model)),
                     dimnames = list(NULL, names(coef(model))))

for (s in 1:n_sims) {
  boot_idx        <- sample(1:n, size = n, replace = TRUE)
  boot_coefs[s, ] <- coef(feols(welfare ~ temp + income + size,
                                data = data[boot_idx, ]))
}

coef_df <- data.frame(
  Method = rep(c("VCV", "Bootstrap"), each = n_sims),
  beta_temp = c(vcv_temp_draws, boot_coefs[, "temp"])
)

ggplot(coef_df, aes(x = beta_temp, fill = Method)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 60) +
  geom_vline(xintercept = coef(model)["temp"], linetype = "dashed") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  theme_minimal(base_size = 13) +
  labs(
    title    = "Sampling Distribution of beta_temp",
    subtitle = "Dashed line = point estimate. Both methods should agree closely under standard OLS.",
    x        = "Coefficient on Temperature",
    y        = "Count",
    fill     = NULL
  )
