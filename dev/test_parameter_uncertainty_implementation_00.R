# =============================================================================
# Implementation test: coefficient uncertainty pipeline
# =============================================================================
# Tests draw_coefs(), predict_outcome(beta_override), and run_sim_pipeline()
# with coef_draws on synthetic data that mimics the app's expected schema.
#
# Run interactively section by section. Each section has an explicit PASS/FAIL
# check. Fix failures before proceeding to the next section.
#
# Usage:
#   1. Open this file in Positron
#   2. Run devtools::load_all() first (Section 0)
#   3. Run each section in order
# =============================================================================

# =============================================================================
# 0. LOAD PACKAGE
# =============================================================================

devtools::load_all()

# Expected: no errors, all fct_* functions available in search path


# =============================================================================
# 1. BUILD MINIMAL SYNTHETIC DATA
# =============================================================================
# Mimics the app's survey + weather joined data structure.
# Column names match what predict_outcome() / run_sim_pipeline() expect.

set.seed(42)
n <- 500

survey_data <- data.frame(
  hh_id     = 1:n,
  loc_id    = sample(paste0("loc_", 1:20), n, replace = TRUE),
  int_month = sample(1:12, n, replace = TRUE),
  income    = rnorm(n, 50, 10),
  size      = sample(1:6, n, replace = TRUE),
  haz       = rnorm(n, 0, 1),             # weather hazard — varies at loc x month
  welfare   = NA_real_
)

# True DGP: welfare is linear in covariates
survey_data$welfare <- 100 +
  2.0  * survey_data$income  +
  -1.5 * survey_data$haz     +
  rnorm(n, 0, 10)


# =============================================================================
# 2. FIT MODEL
# =============================================================================

library(fixest)

fit <- feols(
  welfare ~ haz + income + size | loc_id + int_month,
  data    = survey_data,
  cluster = ~loc_id
)

summary(fit)

# Sanity check: haz coefficient should be close to -1.5
cat("haz coefficient:", round(coef(fit)["haz"], 3), " (expect ~ -1.5)\n")


# =============================================================================
# 3. TEST draw_coefs()
# =============================================================================

draws <- draw_coefs(fit, S = 500, method = "vcov", vcov_spec = ~loc_id, seed = 42)

# -- Dimension check
stopifnot(nrow(draws) == 500)
stopifnot(ncol(draws) == length(coef(fit)))
stopifnot(all(colnames(draws) == names(coef(fit))))
cat("PASS: draw matrix dimensions correct\n")

# -- Column means should be close to point estimates
mean_diff <- max(abs(colMeans(draws) - coef(fit)))
stopifnot(mean_diff < 0.05)
cat("PASS: colMeans(draws) ≈ coef(fit), max diff =", round(mean_diff, 5), "\n")

# -- Covariance should be close to vcov(fit, vcov = ~loc_id)
cov_diff <- max(abs(cov(draws) - vcov(fit, vcov = ~loc_id)))
stopifnot(cov_diff < 0.01)
cat("PASS: cov(draws) ≈ vcov(fit), max diff =", round(cov_diff, 6), "\n")

# -- Bootstrap stub should error cleanly
tryCatch(
  draw_coefs(fit, S = 10, method = "bootstrap"),
  error = function(e) cat("PASS: bootstrap stub errors with:", conditionMessage(e), "\n")
)


# =============================================================================
# 4. TEST predict_outcome() — beta_override sanity check
# =============================================================================

preds_point    <- predict_outcome(fit, newdata = survey_data)
preds_override <- predict_outcome(fit, newdata = survey_data,
                                  beta_override = coef(fit))

fitted_col <- ".fitted"   # adjust if names(preds_point) shows a different column

preds_point_vec    <- preds_point[[fitted_col]]
preds_override_vec <- preds_override[[fitted_col]]

max_pred_diff <- max(abs(preds_point_vec - preds_override_vec), na.rm = TRUE)
stopifnot(max_pred_diff < 1e-6)
cat("PASS: beta_override = coef(fit) reproduces point estimates, max diff =",
    round(max_pred_diff, 10), "\n")

# -- With a perturbed beta, predictions should differ
beta_perturbed        <- coef(fit)
beta_perturbed["haz"] <- coef(fit)["haz"] + 1.0

preds_perturbed <- predict_outcome(fit, newdata = survey_data,
                                   beta_override = beta_perturbed)

cat("Mean welfare shift from +1 on haz coef:",
    round(mean(preds_perturbed[[fitted_col]] - preds_point_vec, na.rm = TRUE), 3),
    " (expect ~ mean(haz) * 1.0 =", round(mean(survey_data$haz), 3), ")\n")

# =============================================================================
# 5. TEST run_sim_pipeline() — draw_id propagation
# =============================================================================
# prepare_hist_weather() (called inside run_sim_pipeline) requires:
#   weather_raw  : timestamp (POSIXct) → derives year (factor), int_month, sim_year
#                  + code, survname, loc_id for inner_join with svy
#   svy          : code, survname, loc_id, int_month, year (factor) for the join
#                  + all covariate columns the model needs
#
# Our original survey_data was missing code/survname/timestamp, so we rebuild
# minimal versions here. fit2 is refit on survey_data2.

S_dev      <- 500   # draws for dev/testing -- set to 500 for full run
n_locs   <- 10
n_years  <- 3
n_months <- 4
n_per_cell <- 3   # households per (loc x month x year) cell

grid <- expand.grid(
  loc_id    = paste0("loc_", seq_len(n_locs)),
  int_month = seq_len(n_months),
  year      = 2005:(2005 + n_years - 1),
  rep       = seq_len(n_per_cell),
  stringsAsFactors = FALSE
)
grid$year <- factor(grid$year)

set.seed(99)
survey_data2 <- data.frame(
  hh_id    = seq_len(nrow(grid)),
  code     = "SYN",
  survname = "synth_survey",
  loc_id   = grid$loc_id,
  int_month = grid$int_month,
  year     = grid$year,
  income   = rnorm(nrow(grid), 50, 10),
  size     = sample(1:6, nrow(grid), replace = TRUE),
  haz      = rnorm(nrow(grid), 0, 1),
  welfare  = NA_real_,
  stringsAsFactors = FALSE
)
survey_data2$welfare <- 100 +
  2.0  * survey_data2$income +
  -1.5 * survey_data2$haz   +
  rnorm(nrow(survey_data2), 0, 10)

# weather_raw: one row per (loc_id x month x year), timestamp encodes year+month
weather_grid <- expand.grid(
  loc_id    = paste0("loc_", seq_len(n_locs)),
  int_month = seq_len(n_months),
  year_int  = 2005:(2005 + n_years - 1),
  stringsAsFactors = FALSE
)
weather_raw2 <- data.frame(
  code      = "SYN",
  survname  = "synth_survey",
  loc_id    = weather_grid$loc_id,
  year      = weather_grid$year_int,    # integer — prepare_hist_weather() calls as.factor(year)
  haz       = rnorm(nrow(weather_grid), 0, 1),
  timestamp = as.POSIXct(paste0(
    weather_grid$year_int, "-", sprintf("%02d", weather_grid$int_month), "-01"
  )),
  stringsAsFactors = FALSE
)

# Fit model on balanced panel survey_data2
fit2 <- feols(
  welfare ~ haz + income + size | loc_id + int_month,
  data    = survey_data2,
  cluster = ~loc_id
)
cat("fit2 haz coefficient:", round(coef(fit2)["haz"], 3), " (expect ~ -1.5)\n")
draws2 <- draw_coefs(fit2, S = 500, method = "vcov", vcov_spec = ~loc_id, seed = 42)

sw_obj <- data.frame(name = "haz",     stringsAsFactors = FALSE)
so_obj <- data.frame(
  name      = "welfare",
  transform = "none",
  type      = "continuous",   # aggregate_sim_preds() checks so$type == "logical" for binary outcomes
  stringsAsFactors = FALSE
)

result_draws <- run_sim_pipeline(
  weather_raw = weather_raw2,
  svy         = survey_data2,
  sw          = sw_obj,
  so          = so_obj,
  model       = fit2,
  residuals   = "none",
  train_data  = survey_data2,
  engine      = "fixest",
  coef_draws  = draws2[1:S_dev, ]  # controlled by S_dev above
)

preds_draws <- result_draws$preds

# -- draw_id column present
stopifnot("draw_id" %in% names(preds_draws))
cat("PASS: draw_id column present\n")

# -- Correct number of unique draws
stopifnot(length(unique(preds_draws$draw_id)) == S_dev)
cat("PASS:", S_dev, "unique draw_id values\n")

# -- Row count = N households x S draws (may be < nrow(survey_data2) * S_dev if inner_join drops rows)
cat("Row count:", nrow(preds_draws), "(expect", nrow(survey_data2) * S_dev, "if join is 1:1)\n")

# -- Point-estimate run (coef_draws = NULL) gives draw_id = NA
result_point2 <- run_sim_pipeline(
  weather_raw = weather_raw2,
  svy         = survey_data2,
  sw          = sw_obj,
  so          = so_obj,
  model       = fit2,
  residuals   = "none",
  train_data  = survey_data2,
  engine      = "fixest",
  coef_draws  = NULL
)

stopifnot(all(is.na(result_point2$preds$draw_id)))
cat("PASS: coef_draws = NULL produces draw_id = NA throughout\n")


# =============================================================================
# 6. TEST aggregate_sim_preds() — two-stage order
# =============================================================================
# Stage 1: aggregate within (draw_id) to scalar
# Stage 2: percentiles across draw_id
# Check that value_p05 / value_p50 / value_p95 are present and ordered correctly

agg <- aggregate_sim_preds(
  preds      = preds_draws,
  so         = so_obj,
  agg_method = "mean",
  deviation  = "none",   # FALSE causes match.arg() error; "none" skips deviation_from_centre()
  loss_frame = FALSE
)

# aggregate_sim_preds() returns a named list: $out (tibble) and $x_label (string)
# names(agg) == c("out", "x_label") — must check names(agg$out) for the columns
agg_out <- agg$out

stopifnot(all(c("value_p05", "value_p50", "value_p95") %in% names(agg_out)))
cat("PASS: value_p05 / value_p50 / value_p95 present in agg$out\n")

stopifnot(all(agg_out$value_p05 <= agg_out$value_p50, na.rm = TRUE))
stopifnot(all(agg_out$value_p50 <= agg_out$value_p95, na.rm = TRUE))
cat("PASS: p05 <= p50 <= p95 ordering correct\n")


# =============================================================================
# 7. VISUAL SANITY CHECK
# =============================================================================
# Plot the coefficient uncertainty band on mean welfare across draws.
# Should show a tight band — this is pure coefficient uncertainty, no weather variation.

library(ggplot2)

ggplot(agg_out, aes(x = sim_year)) +
  geom_ribbon(aes(ymin = value_p05, ymax = value_p95), alpha = 0.3, fill = "#56B4E9") +
  geom_line(aes(y = value_p50), colour = "#56B4E9") +
  geom_hline(yintercept = mean(survey_data2$welfare), linetype = "dashed", colour = "grey40") +
  theme_minimal(base_size = 13) +
  labs(
    title    = "Coefficient uncertainty band on mean welfare",
    subtitle = paste0("Band = 5th/95th percentile across S=", S_dev, " VCV draws. Dashed = observed mean."),
    x        = "Simulated year",
    y        = "Mean welfare"
  )

