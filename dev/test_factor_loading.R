# dev/test_factor_loading.R
# =============================================================================
# Verification suite for Cholesky uncertainty propagation functions.
# Per spec Section 13: all sections must pass before run_sim_pipeline()
# rewrite begins.
#
# Run with: source("dev/test_factor_loading.R")
# Expected: all sections print PASS
# =============================================================================

devtools::load_all()
set.seed(42L)

cat("=== test_factor_loading.R ===\n\n")

# -----------------------------------------------------------------------------
# Synthetic data setup
# -----------------------------------------------------------------------------
# Small synthetic fixest model — N households, K covariates, one FE
# Using feols() so compute_chol_vcov() gets a real fixest object.

N_train <- 500L
K       <- 4L    # non-FE covariates (excl. intercept absorbed by FE)

syn_train <- data.frame(
  y      = rnorm(N_train, mean = 8, sd = 0.5),
  x1     = rnorm(N_train),
  x2     = rnorm(N_train),
  x3     = rnorm(N_train),
  x4     = rnorm(N_train),
  loc_id = sample(paste0("loc_", 1:20), N_train, replace = TRUE)
)

fit_syn <- fixest::feols(y ~ x1 + x2 + x3 + x4 | loc_id,
                         data    = syn_train,
                         vcov    = ~loc_id)

# Synthetic counterfactual newdata — same structure, different rows
N_new <- 200L
syn_new <- data.frame(
  x1     = rnorm(N_new),
  x2     = rnorm(N_new),
  x3     = rnorm(N_new),
  x4     = rnorm(N_new),
  loc_id = sample(paste0("loc_", 1:20), N_new, replace = TRUE)
)

# Non-FE design matrix for counterfactual data
X_nonFE <- model.matrix(~ x1 + x2 + x3 + x4, data = syn_new)[, -1L, drop = FALSE] #Dropped intercept

# Point estimate predictions (log-scale for testing)
y_point <- as.numeric(stats::predict(fit_syn, newdata = syn_new))

# Training augmentation for residual tests
train_aug <- dplyr::mutate(
  syn_train,
  .fitted = as.numeric(stats::predict(fit_syn, newdata = syn_train)),
  .resid  = y - .fitted,
  hh_id   = seq_len(N_train)
)

cat("Synthetic data and model ready.\n\n")

# =============================================================================
# Section 1 — compute_chol_vcov() + compute_factor_loading()
# F %*% t(F) ≈ X %*% Σ %*% t(X) within tolerance
# =============================================================================
cat("Section 1: F %*% t(F) ≈ X %*% Σ %*% t(X) ...\n")

chol_obj  <- compute_chol_vcov(fit_syn, vcov_spec = ~loc_id)
F_loading <- compute_factor_loading(X_nonFE, chol_obj)

stopifnot(
  "chol_obj has L, K, beta, spec" =
    all(c("L", "K", "beta", "spec") %in% names(chol_obj)),
  "L is lower triangular K x K" =
    is.matrix(chol_obj$L) && nrow(chol_obj$L) == ncol(chol_obj$L),
  "F_loading is N x K matrix" =
    is.matrix(F_loading) && nrow(F_loading) == N_new && ncol(F_loading) == chol_obj$K
)

# Verify F %*% t(F) ≈ X %*% Σ %*% t(X)
Sigma   <- vcov(fit_syn, vcov = ~loc_id)
lhs     <- F_loading %*% t(F_loading)          # N × N
rhs     <- X_nonFE %*% Sigma %*% t(X_nonFE)   # N × N
max_err <- max(abs(lhs - rhs))
stopifnot("F %*% t(F) ≈ X %*% Σ %*% t(X) within 1e-8" = max_err < 1e-8)

cat(sprintf("  PASS — max deviation from X Σ X': %.2e\n\n", max_err))

# =============================================================================
# Section 2 — aggregate_with_uncertainty() point estimate consistency
# Point estimate must match direct aggregation on exp(y_point)
# =============================================================================
cat("Section 2: Point estimate consistency ...\n")

agg_fn    <- resolve_agg_fn("mean")
result_pt <- aggregate_with_uncertainty(
  y_point   = y_point,
  F_loading = NULL,     # no uncertainty — point estimate only
  agg_fn    = agg_fn,
  S         = 0L,
  residuals = "none",
  is_log    = TRUE
)

direct_pt <- mean(exp(y_point))
stopifnot(
  "point estimate matches direct computation" =
    abs(result_pt$value - direct_pt) < 1e-10,
  "value_lo == value_hi == value when S=0" =
    result_pt$value_lo == result_pt$value &&
    result_pt$value_hi == result_pt$value
)

cat(sprintf("  PASS — point estimate: %.6f (direct: %.6f)\n\n",
            result_pt$value, direct_pt))

# =============================================================================
# Section 3 — Monte Carlo vs analytic agreement (non-log, linear aggregate)
# For non-log outcome with mean aggregate, MC mean should converge to
# analytic mean within ~1-2% at S=10000
# =============================================================================
cat("Section 3: Monte Carlo vs analytic (non-log mean, S=10000) ...\n")

result_mc <- aggregate_with_uncertainty(
  y_point   = y_point,
  F_loading = F_loading,
  agg_fn    = resolve_agg_fn("mean"),
  S         = 10000L,
  residuals = "none",
  is_log    = FALSE,    # non-log — mean is linear, analytic formula applies
  band_q    = c(lo = 0.10, hi = 0.90),
  seed      = 42L
)

# Analytic mean: E[mean(y + F z)] = mean(y_point) since E[z] = 0
analytic_mean <- mean(y_point)
pct_diff      <- abs(result_mc$value - analytic_mean) / abs(analytic_mean) * 100

stopifnot(
  "MC mean within 1% of analytic mean (non-log)" = pct_diff < 1,
  "draw_values has length S"                      = length(result_mc$draw_values) == 10000L,
  "value_lo <= value_p50 <= value_hi"             =
    result_mc$value_lo <= result_mc$value_p50 &&
    result_mc$value_p50 <= result_mc$value_hi
)

cat(sprintf("  PASS — MC mean: %.6f | analytic: %.6f | diff: %.3f%%\n\n",
            result_mc$value, analytic_mean, pct_diff))

# =============================================================================
# Section 4 — Non-linear aggregates produce valid ordering
# p05 <= p50 <= p95 for headcount_ratio and fgt2
# =============================================================================
cat("Section 4: Non-linear aggregate ordering (headcount_ratio, fgt2) ...\n")

pov_line <- median(exp(y_point)) * 0.6   # 60% of median welfare

result_hcr <- aggregate_with_uncertainty(
  y_point   = y_point,
  F_loading = F_loading,
  agg_fn    = resolve_agg_fn("headcount_ratio"),
  S         = 500L,
  residuals = "none",
  is_log    = TRUE,
  pov_line  = pov_line,
  band_q    = c(lo = 0.10, hi = 0.90),
  seed      = 42L
)

result_fgt2 <- aggregate_with_uncertainty(
  y_point   = y_point,
  F_loading = F_loading,
  agg_fn    = resolve_agg_fn("fgt2"),
  S         = 500L,
  residuals = "none",
  is_log    = TRUE,
  pov_line  = pov_line,
  band_q    = c(lo = 0.10, hi = 0.90),
  seed      = 42L
)

stopifnot(
  "headcount_ratio: value_lo <= value_p50 <= value_hi" =
    result_hcr$value_lo  <= result_hcr$value_p50 &&
    result_hcr$value_p50 <= result_hcr$value_hi,
  "fgt2: value_lo <= value_p50 <= value_hi" =
    result_fgt2$value_lo  <= result_fgt2$value_p50 &&
    result_fgt2$value_p50 <= result_fgt2$value_hi,
  "headcount_ratio in [0, 1]" =
    result_hcr$value >= 0 && result_hcr$value <= 1,
  "fgt2 in [0, 1]" =
    result_fgt2$value >= 0 && result_fgt2$value <= 1
)

cat(sprintf(
  "  PASS — HCR: [%.3f, %.3f, %.3f] | FGT2: [%.3f, %.3f, %.3f]\n\n",
  result_hcr$value_lo, result_hcr$value_p50, result_hcr$value_hi,
  result_fgt2$value_lo, result_fgt2$value_p50, result_fgt2$value_hi
))

# =============================================================================
# Section 5 — Log path: exp(y + F z + resid) correct
# Resample residuals from train_aug — result must differ from no-residual path
# =============================================================================
cat("Section 5: Log path with resample residuals ...\n")

result_resamp <- aggregate_with_uncertainty(
  y_point   = y_point,
  F_loading = F_loading,
  agg_fn    = resolve_agg_fn("mean"),
  S         = 500L,
  residuals = "resample",
  train_aug = train_aug,
  is_log    = TRUE,
  band_q    = c(lo = 0.10, hi = 0.90),
  seed      = 42L
)

result_none <- aggregate_with_uncertainty(
  y_point   = y_point,
  F_loading = F_loading,
  agg_fn    = resolve_agg_fn("mean"),
  S         = 500L,
  residuals = "none",
  is_log    = TRUE,
  band_q    = c(lo = 0.10, hi = 0.90),
  seed      = 42L
)

stopifnot(
  "resample result differs from no-residual result" =
    abs(result_resamp$value - result_none$value) > 0,
  "value_lo <= value_p50 <= value_hi with residuals" =
    result_resamp$value_lo  <= result_resamp$value_p50 &&
    result_resamp$value_p50 <= result_resamp$value_hi,
  "draw_values length S" = length(result_resamp$draw_values) == 500L
)

cat(sprintf(
  "  PASS — with resid: %.4f | without: %.4f\n\n",
  result_resamp$value, result_none$value
))

# =============================================================================
# Section 6 — combine_ensemble_results()
# Inner band narrower than outer band when M > 1
# =============================================================================
cat("Section 6: combine_ensemble_results() inner < outer band ...\n")

# Simulate 3 ensemble representative results with different point estimates
make_result <- function(value_offset, seed_val) {
  aggregate_with_uncertainty(
    y_point   = y_point + value_offset,
    F_loading = F_loading,
    agg_fn    = resolve_agg_fn("mean"),
    S         = 200L,
    residuals = "none",
    is_log    = TRUE,
    band_q    = c(lo = 0.10, hi = 0.90),
    seed      = seed_val
  )
}

ensemble_results <- list(
  ssp3_2025_2035_ensemble_mean = make_result(0,     42L),
  ssp3_2025_2035_ensemble_lo   = make_result(-0.05, 43L),
  ssp3_2025_2035_ensemble_hi   = make_result( 0.05, 44L)
)

combined <- combine_ensemble_results(ensemble_results)

stopifnot(
  "combined has required fields" =
    all(c("value", "value_lo", "value_hi", "model_lo",
          "model_hi", "draw_values", "n_members") %in% names(combined)),
  "n_members == 3" = combined$n_members == 3L,
  "inner band lo <= value <= inner band hi" =
    combined$value_lo <= combined$value &&
    combined$value    <= combined$value_hi,
  "outer band wider than or equal to inner band" =
    combined$model_lo <= combined$value_lo &&
    combined$model_hi >= combined$value_hi,
  "draw_values is numeric vector" =
    is.numeric(combined$draw_values) && length(combined$draw_values) > 0
)

cat(sprintf(
  "  PASS — central: %.4f | inner: [%.4f, %.4f] | outer: [%.4f, %.4f]\n\n",
  combined$value,
  combined$value_lo, combined$value_hi,
  combined$model_lo, combined$model_hi
))

# =============================================================================
# Section 7 — compute_cluster_counts() valid VCV
# No singular matrix errors at ~loc_id with synthetic data
# =============================================================================
cat("Section 7: compute_cluster_counts() + VCV validity ...\n")

counts <- compute_cluster_counts(syn_train)

stopifnot(
  "counts has loc_id"           = !is.na(counts$loc_id),
  "counts has loc_id_int_month" = is.na(counts$loc_id_int_month) ||
                                  is.numeric(counts$loc_id_int_month),
  "G at loc_id > 0"             = counts$loc_id > 0L
)

# Confirm VCV is positive definite (no Cholesky failure)
vcv_valid <- tryCatch({
  chol(vcov(fit_syn, vcov = ~loc_id))
  TRUE
}, error = function(e) FALSE)

stopifnot("VCV is positive definite at ~loc_id" = vcv_valid)

cat(sprintf(
  "  PASS — G(loc_id): %d | VCV positive definite: %s\n\n",
  counts$loc_id, vcv_valid
))

# =============================================================================
# Summary
# =============================================================================
cat("=== ALL SECTIONS PASSED ===\n")
cat(sprintf("  Timestamp: %s\n", format(Sys.time())))
cat(sprintf("  N_train=%d | N_new=%d | K=%d | S_max=10000\n",
            N_train, N_new, K))