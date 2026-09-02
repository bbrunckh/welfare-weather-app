library(MASS)
library(tidyverse)

# =============================================================
# DATA SETUP
# 
# Survey data: 2 years (2020, 2021), same N households observed
# Weather data: 10 years (2020-2029), same locations
# Goal: simulate welfare for the same survey sample under
#       each year's weather conditions
# =============================================================

set.seed(123)

N          <- 500        # number of households
locations  <- paste0("loc_", 1:10)
survey_yrs <- c(2020, 2021)
all_yrs    <- 2020:2029
n_sim      <- 2000

# -------------------------------------------------------
# 1. Generate household characteristics (fixed over time)
#    These are the X_i and E_j covariates — observed and
#    held constant across all weather scenarios
# -------------------------------------------------------

households <- tibble(
  i         = 1:N,
  j         = sample(locations, N, replace = TRUE),
  X_assets  = rnorm(N, mean = 50, sd = 15),    # household asset index
  E_market  = rnorm(N, mean = 5,  sd = 2)      # location market access
)

# -------------------------------------------------------
# 2. Generate weather data for all 10 years by location
#    In the real application this comes from your weather dataset.
#    The 2020-2021 values are also used in training.
# -------------------------------------------------------

weather <- expand_grid(
  j    = locations,
  year = all_yrs
) %>%
  mutate(
    Haz = rnorm(n(), mean = 2 + 0.1 * (year - 2020), sd = 1)
    # Slight upward trend in hazard severity over time
  )

# -------------------------------------------------------
# 3. Generate survey data for 2020 and 2021
#    True DGP: W = 200 - 30*Haz + 4*X + 10*E + epsilon
#    epsilon ~ N(0, 50): individual persistent shock +
#    transitory noise. We simulate a persistent component
#    to make the reuse-residuals approach meaningful.
# -------------------------------------------------------

# Persistent unobserved individual component (stable across years)
households <- households %>%
  mutate(
    u_i = rnorm(N, mean = 0, sd = 35)   # persistent shock: large relative to transitory
  )

survey_data <- expand_grid(
  i    = 1:N,
  year = survey_yrs
) %>%
  left_join(households, by = "i") %>%
  left_join(weather, by = c("j", "year")) %>%
  mutate(
    epsilon_transitory = rnorm(n(), mean = 0, sd = 35),   # year-specific noise
    W = 200 + (-30 * Haz) + (4 * X_assets) + (10 * E_market) +
        u_i + epsilon_transitory                           # total residual = u_i + transitory
  )

# -------------------------------------------------------
# 4. Fit model on survey data (pooled 2020-2021)
#    Note: model does NOT include individual fixed effects,
#    so the residual absorbs both persistent and transitory
#    components — this is the setting where reusing
#    residuals is most defensible.
# -------------------------------------------------------

model <- lm(W ~ Haz + X_assets + E_market, data = survey_data)
summary(model)

cat("\nTrue coefficients: Intercept=200, Haz=-30, X_assets=4, E_market=10\n")
cat("Residual SD (sigma):", round(sigma(model), 2), "\n")
cat("True total residual SD:", round(sd(survey_data$u_i + survey_data$epsilon_transitory), 2), "\n\n")

# -------------------------------------------------------
# 5. Compute fitted residuals for each household
#    Use the average residual across the 2 survey years
#    per household — more stable than a single year
# -------------------------------------------------------

survey_data <- survey_data %>%
  mutate(resid_fitted = residuals(model))

# Average residual per household across survey years
household_residuals <- survey_data %>%
  group_by(i) %>%
  summarise(resid_avg = mean(resid_fitted), .groups = "drop")

# Full training residual vector (for empirical bootstrap pool)
train_resids <- residuals(model)

# -------------------------------------------------------
# 6. Build prediction dataset
#    Same N households × 10 weather years
#    Fixed household characteristics, varying Haz by year
# -------------------------------------------------------

pred_data <- expand_grid(
  i    = 1:N,
  year = all_yrs
) %>%
  left_join(households %>% dplyr::select(i, j, X_assets, E_market), by = "i") %>%
  left_join(weather, by = c("j", "year")) %>%
  left_join(household_residuals, by = "i")   # attach matched residual

# Design matrix for predictions
X_pred <- model.matrix(~ Haz + X_assets + E_market, data = pred_data)

# -------------------------------------------------------
# 7. Draw coefficient vectors (parameter uncertainty)
#    Shared across all three residual approaches —
#    this component is identical regardless of which
#    residual strategy is used
# -------------------------------------------------------

coef_draws <- mvrnorm(
  n     = n_sim,
  mu    = coef(model),
  Sigma = vcov(model)
)
# Dimensions: n_sim x n_coefficients

# Conditional mean predictions: n_obs x n_sim
# Each column is one plausible set of conditional mean predictions
W_mean_pred <- X_pred %*% t(coef_draws)

sigma_resid <- sigma(model)
n_obs       <- nrow(pred_data)

# =============================================================
# APPROACH 1: Parametric residuals — draw from N(0, sigma_hat)
#
# Assumes: residuals are normally distributed, purely transitory,
# and independent across individuals and weather. Ignores any
# persistent individual component.
# =============================================================

set.seed(1)
resid_parametric <- matrix(
  rnorm(n_obs * n_sim, mean = 0, sd = sigma_resid),
  nrow = n_obs, ncol = n_sim
)

W_parametric <- W_mean_pred + resid_parametric

# =============================================================
# APPROACH 2: Empirical bootstrap — sample from training residuals
#
# Assumes: residuals are transitory and randomly distributed
# across individuals each draw. Preserves observed distributional
# shape (skew, kurtosis) but discards individual identity.
# =============================================================

set.seed(2)
resid_empirical <- matrix(
  sample(train_resids, size = n_obs * n_sim, replace = TRUE),
  nrow = n_obs, ncol = n_sim
)

W_empirical <- W_mean_pred + resid_empirical

# =============================================================
# APPROACH 3: Reuse fitted residuals (matched by household ID)
#
# Assumes: residuals reflect persistent unobserved individual
# characteristics, stable across the simulation period.
# Preserves cross-sectional correlation structure — the same
# households that are above/below the model in training remain
# so in all simulated years.
#
# The residual is FIXED (not drawn), so it contributes no
# additional simulation variance. Only parameter uncertainty
# is being simulated here. The prediction intervals are
# narrower and conditional on each household's observed position.
# =============================================================

# Fixed residual vector: one value per observation, matched by i
# Broadcasting: add the same residual column across all n_sim draws
resid_fixed <- pred_data$resid_avg   # length n_obs

W_reuse <- W_mean_pred + resid_fixed   # vector recycles across columns

# =============================================================
# 8. Aggregate to population-level summaries by year
#    For each approach: mean welfare and poverty share per year,
#    with 95% uncertainty intervals
# =============================================================

threshold <- 300   # welfare poverty line

summarise_by_year <- function(W_matrix, pred_data, label) {
  years <- sort(unique(pred_data$year))
  
  map_dfr(years, function(yr) {
    idx <- which(pred_data$year == yr)
    W_yr <- W_matrix[idx, ]    # n_households x n_sim for this year
    
    pop_mean    <- colMeans(W_yr)
    pop_poverty <- colMeans(W_yr < threshold)
    
    bind_rows(
      tibble(
        year      = yr,
        approach  = label,
        statistic = "Mean welfare",
        estimate  = mean(pop_mean),
        lwr_95    = quantile(pop_mean, 0.025),
        upr_95    = quantile(pop_mean, 0.975)
      ),
      tibble(
        year      = yr,
        approach  = label,
        statistic = "Poverty share",
        estimate  = mean(pop_poverty),
        lwr_95    = quantile(pop_poverty, 0.025),
        upr_95    = quantile(pop_poverty, 0.975)
      )
    )
  })
}

results <- bind_rows(
  summarise_by_year(W_parametric, pred_data, "1. Parametric N(0,sigma)"),
  summarise_by_year(W_empirical,  pred_data, "2. Empirical bootstrap"),
  summarise_by_year(W_reuse,      pred_data, "3. Reuse fitted residuals")
)

# -------------------------------------------------------
# 9. Print results table
# -------------------------------------------------------

results %>%
  filter(statistic == "Mean welfare") %>%
  mutate(across(c(estimate, lwr_95, upr_95), round, 1)) %>%
  arrange(year, approach) %>%
  print(n = Inf)

# -------------------------------------------------------
# 10. Compare interval widths across approaches
#     Key diagnostic: how much wider are parametric/empirical
#     intervals vs. reuse? The difference reflects residual
#     variance contribution to population-level uncertainty.
# -------------------------------------------------------

interval_widths <- results %>%
  mutate(width = upr_95 - lwr_95) %>%
  group_by(approach, statistic) %>%
  summarise(mean_width = round(mean(width), 3), .groups = "drop") %>%
  arrange(statistic, approach)

cat("\n--- Average 95% interval width by approach ---\n")
print(interval_widths)

# -------------------------------------------------------
# 11. Plot: mean welfare over time with uncertainty bands
# -------------------------------------------------------

results %>%
  filter(statistic == "Mean welfare") %>%
  ggplot(aes(x = year, y = estimate, colour = approach, fill = approach)) +
  geom_ribbon(aes(ymin = lwr_95, ymax = upr_95), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2021.5, linetype = "dashed", colour = "grey40") +
  annotate("text", x = 2021.2, y = Inf, label = "Training period",
           hjust = 1, vjust = 1.5, size = 3, colour = "grey40") +
  annotate("text", x = 2021.8, y = Inf, label = "Simulation period",
           hjust = 0, vjust = 1.5, size = 3, colour = "grey40") +
  scale_x_continuous(breaks = all_yrs) +
  labs(
    title    = "Simulated mean welfare under 10 years of weather — three residual approaches",
    subtitle = "Shaded bands = 95% uncertainty intervals. Reuse approach intervals reflect parameter uncertainty only.",
    x        = "Year", y = "Mean welfare",
    colour   = "Approach", fill = "Approach"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# -------------------------------------------------------
# 12. Plot: poverty share over time
# -------------------------------------------------------

results %>%
  filter(statistic == "Poverty share") %>%
  ggplot(aes(x = year, y = estimate, colour = approach, fill = approach)) +
  geom_ribbon(aes(ymin = lwr_95, ymax = upr_95), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2021.5, linetype = "dashed", colour = "grey40") +
  scale_x_continuous(breaks = all_yrs) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title    = "Poverty share under 10 years of weather — three residual approaches",
    subtitle = "Poverty threshold = 300. Distributional shape of residuals matters most here.",
    x        = "Year", y = "Poverty share",
    colour   = "Approach", fill = "Approach"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# -------------------------------------------------------
# 13. Diagnostic: check residual distribution
#     Use this to justify your choice of approach
# -------------------------------------------------------

tibble(resid = train_resids) %>%
  ggplot(aes(sample = resid)) +
  stat_qq() + stat_qq_line(colour = "red") +
  labs(
    title    = "QQ plot of training residuals",
    subtitle = "Departure from the line favours empirical bootstrap over parametric"
  ) +
  theme_minimal()

tibble(resid = train_resids) %>%
  ggplot(aes(x = resid)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_density(aes(y = after_stat(count) * (max(train_resids) - min(train_resids)) / 50),
               colour = "red", linewidth = 0.8) +
  labs(
    title = "Distribution of training residuals",
    x = "Residual", y = "Count"
  ) +
  theme_minimal()

# -------------------------------------------------------
# 14. Diagnostic: persistence of individual residuals
#     If residuals are persistent across the 2 training years,
#     reusing them is more defensible. Check correlation of
#     individual residuals across years.
# -------------------------------------------------------

resid_by_year <- survey_data %>%
  dplyr::select(i, year, resid_fitted) %>%
  pivot_wider(names_from = year, values_from = resid_fitted,
              names_prefix = "resid_")

resid_correlation <- cor(
  resid_by_year$resid_2020,
  resid_by_year$resid_2021,
  use = "complete.obs"
)

cat("\nCorrelation of individual residuals across 2020 and 2021:", round(resid_correlation, 3), "\n")
cat("Higher correlation (>0.4) supports reusing fitted residuals.\n")

resid_by_year %>%
  ggplot(aes(x = resid_2020, y = resid_2021)) +
  geom_point(alpha = 0.4, colour = "steelblue") +
  geom_smooth(method = "lm", colour = "red", se = TRUE) +
  labs(
    title    = "Individual residual persistence across training years",
    subtitle = paste0("Correlation = ", round(resid_correlation, 3),
                      " — higher values support reusing fitted residuals"),
    x        = "Residual 2020", y = "Residual 2021"
  ) +
  theme_minimal()