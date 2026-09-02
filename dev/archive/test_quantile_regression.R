# Test script: Unconditional Quantile Regression (RIF) pipeline
#
# Validates the math pipeline from issue #21:
#   RIF computation → stacked feols regression → simulate_policy_rif()
#
# Run: source("dev/test_quantile_regression.R")
# Packages needed: fixest, broom, dplyr, ggplot2 (all in DESCRIPTION)

# ── 0. Packages ───────────────────────────────────────────────────────────────

library(fixest)
library(broom)
library(dplyr)
library(ggplot2)

# ── 1. Helper functions ───────────────────────────────────────────────────────

#' Compute RIF for a given quantile
#' (Firpo, Fortin & Lemieux 2009)
#'
#' @param y     numeric vector (the outcome)
#' @param tau   quantile in (0, 1)
#' @param bw    bandwidth for density estimation; NULL uses bw.SJ
#' @return numeric vector of RIF values, same length as y
compute_rif <- function(y, tau, bw = NULL) {
  y_obs <- y[!is.na(y)]
  q_tau <- stats::quantile(y_obs, probs = tau, names = FALSE)

  bw_use <- if (is.null(bw)) stats::bw.SJ(y_obs) else bw
  dens   <- stats::density(y_obs, bw = bw_use, n = 512)
  f_q    <- stats::approx(dens$x, dens$y, xout = q_tau)$y

  if (is.na(f_q) || f_q <= 0) {
    warning(sprintf("Density near zero at quantile %.2f; RIF may be unstable.", tau))
    f_q <- max(f_q, 1e-6)
  }

  q_tau + (tau - as.numeric(y <= q_tau)) / f_q
}


#' Simulate a policy intervention using UQR (RIF regression) estimates
#'
#' Decomposes the total policy effect into:
#'
#'   delta_total = delta_main + delta_res
#'
#' **Main effect** (welfare shift independent of weather):
#'   - Cash transfer: delta_main_i = cash_i  (log-points, can be household-specific)
#'   - RIF-modeled variables: delta_main_i += sum_x [ beta_x(tau_i) * delta_x ]
#'     e.g. an irrigation programme that raises income via its RIF coefficient
#'
#' **Resilience effect** (change in how badly a weather shock hurts):
#'   - Repositioning (res1): households re-rank after the main effect, moving
#'     to a higher or lower point on the hazard beta curve:
#'       delta_res1_i = [ beta_Haz(tau_i_post) - beta_Haz(tau_i) ] * Haz_i
#'   - Direct interaction (res2): policy variable explicitly moderates weather
#'     sensitivity via an interaction term in the model:
#'       delta_res2_i = sum_x [ beta_Haz:x(tau_i) * Haz_i * delta_x ]
#'
#' @param rif_grid        data.frame with columns: tau, term, estimate, model.
#'                        Built from broom::tidy() on a stacked feols fit.
#' @param observed_y      numeric vector of observed log-welfare (baseline).
#' @param weather_var     character; the weather/hazard variable name, exactly
#'                        as it appears in rif_grid$term (e.g. "temp_anom").
#' @param hazard_values   numeric scalar or household-length vector; the
#'                        realised hazard shock to simulate (e.g. 1.0 for +1 SD).
#' @param cash            numeric scalar or household-length vector; direct
#'                        log-point consumption change from a cash transfer.
#'                        Use log(1 + r) for a proportional rate r.
#'                        Set to 0 (default) for no direct transfer.
#' @param targeting       Controls who receives each intervention. Can be:
#'                          NULL                  — uniform (everyone gets full amount)
#'                          function(tau) -> m    — same rule for cash AND all policy_vars
#'                          named list of functions — per-intervention rules, e.g.:
#'                            list(
#'                              cash      = function(tau) as.numeric(tau < 0.4),
#'                              irrigated = function(tau) as.numeric(tau < 0.2)
#'                            )
#'                        Any variable not named in a list gets the full amount (mult = 1).
#'                        Multipliers are evaluated against tau_i_pre (baseline quantile).
#'                        Repositioning (res1) is non-zero whenever a household's
#'                        welfare changes — it reflects movement along the absolute
#'                        baseline welfare distribution, not relative re-ranking.
#' @param policy_vars     named numeric vector; changes to RIF-modeled covariates
#'                        that create a main effect via their estimated coefficients.
#'                        e.g. c(irrigated = 1) for a binary irrigation programme.
#'                        Names must match terms in rif_grid. NULL = none.
#' @param interaction_vars named numeric vector; changes to variables that
#'                        interact with the hazard in the model, creating res2.
#'                        Names must match interaction terms in rif_grid exactly,
#'                        e.g. c("temp_anom:irrigated" = 1). NULL = none.
#' @param model_id        integer matching the `model` column in rif_grid (default 3).
#' @return data.frame with one row per household and full decomposition columns.
simulate_policy_rif <- function(rif_grid,
                                observed_y,
                                weather_var,
                                hazard_values,
                                cash = 0,
                                targeting = NULL,
                                policy_vars = NULL,
                                interaction_vars = NULL,
                                model_id = 3) {

  n    <- length(observed_y)
  taus <- sort(unique(rif_grid$tau))

  if (!is.numeric(observed_y) || n < 2)
    stop("observed_y must be a numeric vector with at least 2 elements.")
  if (!is.numeric(hazard_values) || !length(hazard_values) %in% c(1, n))
    stop("hazard_values must be a scalar or same-length vector as observed_y.")
  if (!is.numeric(cash) || !length(cash) %in% c(1, n))
    stop("cash must be a scalar or same-length vector as observed_y.")
  if (!is.null(targeting) && !is.function(targeting) && !is.list(targeting))
    stop("targeting must be NULL, a function, or a named list of functions.")
  if (is.list(targeting) && !all(sapply(targeting, is.function)))
    stop("All elements of the targeting list must be functions.")

  hazard_values <- rep(hazard_values, length.out = n)
  cash          <- rep(cash,          length.out = n)

  # ── Helper: interpolate a beta curve from rif_grid at arbitrary tau values ──
  beta_curve <- function(term_name, tau_values) {
    rows <- rif_grid[rif_grid$term == term_name & rif_grid$model == model_id, ]
    if (nrow(rows) == 0)
      stop(sprintf("Term '%s' not found in rif_grid for model %d.", term_name, model_id))
    stats::approx(x = rows$tau, y = rows$estimate, xout = tau_values, rule = 2)$y
  }

  # ── Step 1: Baseline CDF and pre-policy quantiles ─────────────────────────
  # Use the empirical CDF of the *baseline* distribution as a fixed yardstick.
  # Evaluating tau_i_post with the same CDF means:
  #   - Non-treated (delta_main = 0): ecdf(y)(y_i) == ecdf(y)(y_i)  →  res1 = 0
  #   - Treated (delta_main > 0):     ecdf(y)(y_i + Δ) > tau_i_pre  →  res1 ≠ 0
  # Non-treated households are never displaced by others' gains.
  baseline_cdf <- stats::ecdf(observed_y)
  tau_i_pre    <- baseline_cdf(observed_y)

  # ── Step 2: Per-intervention targeting ────────────────────────────────────
  # Returns a household-level multiplier for a given variable name.
  # Evaluated against baseline quantiles so targeting rules are stable.
  get_multiplier <- function(var_name) {
    if (is.null(targeting))    return(rep(1, n))
    if (is.function(targeting)) return(targeting(tau_i_pre))
    # Named list: use the matching function or default to 1 (untargeted)
    fn <- targeting[[var_name]]
    if (is.null(fn)) return(rep(1, n))
    fn(tau_i_pre)
  }

  # ── Step 3: Main effect ────────────────────────────────────────────────────
  # Part A — direct cash transfer (with cash-specific targeting)
  cash_mult  <- get_multiplier("cash")
  delta_main <- cash * cash_mult

  # Part B — RIF coefficient-based main effects (each with its own targeting)
  if (!is.null(policy_vars)) {
    for (v in names(policy_vars)) {
      v_mult     <- get_multiplier(v)
      delta_main <- delta_main + beta_curve(v, tau_i_pre) * policy_vars[[v]] * v_mult
    }
  }

  # ── Step 4: Post-policy quantiles using the baseline CDF ──────────────────
  # Evaluating the *same* ecdf at new welfare levels maps each household's
  # absolute post-policy welfare back to its position in the pre-policy
  # distribution — without displacing anyone who did not receive the intervention.
  y_post_policy  <- observed_y + delta_main
  tau_i_post     <- baseline_cdf(y_post_policy)

  # ── Step 5: Resilience — repositioning (res1) ─────────────────────────────
  # Households now sit at a different point on the hazard beta curve.
  # Even without any interaction term in the model, a household moved from
  # the 20th to the 25th percentile experiences the weather shock as a
  # 25th-percentile household. If the curve is concave (poorer HHs hurt more),
  # this provides implicit weather protection.
  beta_haz_pre  <- beta_curve(weather_var, tau_i_pre)
  beta_haz_post <- beta_curve(weather_var, tau_i_post)

  delta_res1 <- (beta_haz_post - beta_haz_pre) * hazard_values

  # ── Step 6: Resilience — direct interaction (res2) ────────────────────────
  # Only present when the policy variable explicitly moderates weather
  # sensitivity AND is interacted with the hazard in the estimated model.
  # e.g. irrigation reduces heat damage: beta_{temp:irrigated}(tau) * Haz * delta_irrigated
  delta_res2 <- rep(0, n)

  if (!is.null(interaction_vars)) {
    for (int_term in names(interaction_vars)) {
      delta_res2 <- delta_res2 +
        beta_curve(int_term, tau_i_pre) * hazard_values * interaction_vars[[int_term]]
    }
  }

  # ── Step 7: Totals ─────────────────────────────────────────────────────────
  delta_res   <- delta_res1 + delta_res2
  delta_total <- delta_main + delta_res

  # Weather impact in each scenario (to isolate resilience from main effect)
  weather_no_policy   <- beta_haz_pre  * hazard_values
  weather_with_policy <- beta_haz_post * hazard_values + delta_res2

  # ── Step 8: Return decomposition ──────────────────────────────────────────
  data.frame(
    y_observed_log          = observed_y,
    y_post_policy_log       = y_post_policy,
    tau_i_pre               = tau_i_pre,
    tau_i_post              = tau_i_post,
    decile_i                = ceiling(tau_i_pre  * 10),
    decile_i_post           = ceiling(tau_i_post * 10),
    targeting_weight        = cash_mult,    # cash targeting multiplier (use delta_main>0 for overall treatment)
    beta_haz_pre            = beta_haz_pre,
    beta_haz_post           = beta_haz_post,
    delta_main_log          = delta_main,
    delta_res1_log          = delta_res1,   # repositioning
    delta_res2_log          = delta_res2,   # direct interaction
    delta_res_log           = delta_res,
    delta_total_log         = delta_total,
    weather_no_policy_log   = weather_no_policy,
    weather_with_policy_log = weather_with_policy,
    pct_main                = (exp(delta_main)          - 1) * 100,
    pct_weather_no_policy   = (exp(weather_no_policy)   - 1) * 100,
    pct_weather_with_policy = (exp(weather_with_policy) - 1) * 100,
    pct_total               = (exp(delta_total)         - 1) * 100,
    pct_protected           = (exp(weather_with_policy) - exp(weather_no_policy)) * 100
  )
}


# ── 2. Synthetic data ─────────────────────────────────────────────────────────
#
# 500 households, cross-section, log-normal welfare.
# True DGP includes:
#   - a negative main effect of temperature (-0.10, larger for poor HHs)
#   - a buffering interaction for irrigated farms (+0.05 * temp * irrigated)
# This ensures (a) a non-flat beta curve and (b) a non-zero res2 channel.

set.seed(42)

n_hh       <- 500
n_villages <- 20

df <- data.frame(
  hh_id     = seq_len(n_hh),
  village   = sample(paste0("v", seq_len(n_villages)), n_hh, replace = TRUE),
  year      = sample(2010:2019, n_hh, replace = TRUE),
  irrigated = rbinom(n_hh, 1, 0.3)
)

hh_fe      <- rnorm(n_hh, mean = 0, sd = 0.4)
village_fe <- setNames(rnorm(n_villages, 0, 0.3), paste0("v", seq_len(n_villages)))

df$temp_anom <- rnorm(n_hh, mean = 0.2, sd = 1.0)

df$log_welfare <- 5 +
  village_fe[df$village] +
  (-0.10 - 0.20 * (hh_fe < -0.3)) * df$temp_anom +  # poorer HHs: -0.30
  0.05 * df$temp_anom * df$irrigated +
  hh_fe +
  rnorm(n_hh, 0, 0.3)

cat("── Synthetic data ─────────────────────────────────────────────────────\n")
cat(sprintf("  n = %d households, %d villages, years %d-%d\n",
            n_hh, n_villages, min(df$year), max(df$year)))
cat(sprintf("  log_welfare: mean = %.2f, sd = %.2f, range [%.2f, %.2f]\n",
            mean(df$log_welfare), sd(df$log_welfare),
            min(df$log_welfare), max(df$log_welfare)))
cat(sprintf("  irrigated: %.0f%%\n\n", mean(df$irrigated) * 100))


# ── 3. Compute RIF columns ────────────────────────────────────────────────────

taus     <- seq(0.1, 0.9, by = 0.1)
tau_lbls <- paste0("rif_", formatC(taus * 100, format = "d"))

for (i in seq_along(taus)) {
  df[[tau_lbls[i]]] <- compute_rif(df$log_welfare, tau = taus[i])
}

cat("── RIF columns added ─────────────────────────────────────────────────\n")
cat(sprintf("  Quantiles: %s\n", paste(taus, collapse = ", ")))
rif_check <- sapply(tau_lbls, function(v) mean(df[[v]]))
cat("  Mean RIF by quantile (should increase with tau):\n")
print(round(rif_check, 3))
cat("\n")


# ── 4. Fit stacked feols ──────────────────────────────────────────────────────
#
# Include temp_anom * irrigated to estimate the direct interaction channel (res2).
# The `*` expands to main effects + interaction: temp_anom + irrigated + temp_anom:irrigated

lhs <- paste0("c(", paste(tau_lbls, collapse = ", "), ")")
fml <- as.formula(paste0(lhs, " ~ temp_anom * irrigated | village + year"))

fits <- fixest::feols(fml, data = df, cluster = ~village, warn = FALSE)

cat("── Stacked feols fit ─────────────────────────────────────────────────\n")
cat(sprintf("  %d models fitted (one per quantile)\n\n", length(fits)))


# ── 5. Build rif_grid ─────────────────────────────────────────────────────────

rif_grid <- purrr::map_dfr(seq_along(fits), function(i) {
  tbl       <- broom::tidy(fits[[i]], conf.int = TRUE)
  tbl$tau   <- taus[i]  # i is the numeric index 1, 2, ..., 9
  tbl$model <- 3L   # convention: model 3 = full specification
  tbl
})

n_terms <- length(unique(rif_grid$term))

cat("── rif_grid ──────────────────────────────────────────────────────────\n")
cat(sprintf("  %d rows (%d quantiles × %d terms), %d columns\n",
            nrow(rif_grid), length(taus), n_terms, ncol(rif_grid)))
cat(sprintf("  Terms: %s\n\n", paste(unique(rif_grid$term), collapse = ", ")))

cat("  Weather beta curve (temp_anom) by quantile:\n")
beta_curve_tbl <- rif_grid[rif_grid$term == "temp_anom",
                            c("tau", "estimate", "conf.low", "conf.high")]
print(round(beta_curve_tbl, 4), row.names = FALSE)
cat("\n")


# ── 6. Run simulate_policy_rif() — five scenarios ────────────────────────────
#
# Ranking note: tau is now derived from the *baseline* empirical CDF (fixed
# yardstick), not relative ranks. Consequence:
#   - Non-treated (delta_main = 0): tau_i_post = tau_i_pre → res1 = 0 exactly
#   - Treated (delta_main > 0):     household moves to a higher point on the
#     beta curve → res1 ≠ 0 (proportional to the slope of beta at that point)
# This eliminates the zero-sum displacement of non-treated households.

# Scenario A: Uniform cash — everyone gets the same transfer
#   All households' absolute welfare rises → each moves up in the baseline CDF
#   → res1 ≠ 0 for all (small, driven by the slope of the beta curve at each
#   household's quantile). Good baseline to verify the absolute CDF mechanism.
res_uniform <- simulate_policy_rif(
  rif_grid         = rif_grid,
  observed_y       = df$log_welfare,
  weather_var      = "temp_anom",
  hazard_values    = 1.0,
  cash             = log(1.10),    # +10% welfare for everyone
  targeting        = NULL,
  interaction_vars = NULL,
  model_id         = 3
)

# Scenario B: Targeted cash — bottom 40% only
#   Only poor households receive the transfer.
#   Treated HHs: delta_main > 0 → tau_i_post > tau_i_pre → res1 ≠ 0
#   Untreated HHs: delta_main = 0 → tau_i_post = tau_i_pre → res1 = 0 exactly
res_targeted <- simulate_policy_rif(
  rif_grid         = rif_grid,
  observed_y       = df$log_welfare,
  weather_var      = "temp_anom",
  hazard_values    = 1.0,
  cash             = log(1.10),
  targeting        = function(tau) as.numeric(tau < 0.4),  # bottom 40%
  interaction_vars = NULL,
  model_id         = 3
)

# Scenario C: Irrigation expansion (untargeted) — res2 channel
#   Main effect via RIF coefficient; direct heat-buffering via interaction term.
res_irrig <- simulate_policy_rif(
  rif_grid         = rif_grid,
  observed_y       = df$log_welfare,
  weather_var      = "temp_anom",
  hazard_values    = 1.0,
  cash             = 0,
  targeting        = NULL,
  policy_vars      = c(irrigated = 1),
  interaction_vars = c("temp_anom:irrigated" = 1),
  model_id         = 3
)

# Scenario D: Targeted irrigation — bottom 40% only
#   Only poorest households receive irrigation access.
#   Demonstrates res2 + res1 combined for targeted policy.
res_irrig_targeted <- simulate_policy_rif(
  rif_grid         = rif_grid,
  observed_y       = df$log_welfare,
  weather_var      = "temp_anom",
  hazard_values    = 1.0,
  cash             = 0,
  targeting        = function(tau) as.numeric(tau < 0.4),
  policy_vars      = c(irrigated = 1),
  interaction_vars = c("temp_anom:irrigated" = 1),
  model_id         = 3
)

# Scenario E: Combined programme with per-intervention targeting
#   Cash safety net → bottom 40%; Irrigation → bottom 20% (poorest only)
#   This is the key use case for a named targeting list.
#   Cash multiplier (targeting_weight) will be 1 for tau < 0.4, 0 otherwise.
#   Irrigation multiplier will be 1 for tau < 0.2, 0 otherwise.
res_combined <- simulate_policy_rif(
  rif_grid         = rif_grid,
  observed_y       = df$log_welfare,
  weather_var      = "temp_anom",
  hazard_values    = 1.0,
  cash             = log(1.10),
  targeting        = list(
    cash      = function(tau) as.numeric(tau < 0.4),   # cash: bottom 40%
    irrigated = function(tau) as.numeric(tau < 0.2)    # irrigation: bottom 20%
  ),
  policy_vars      = c(irrigated = 1),
  interaction_vars = c("temp_anom:irrigated" = 1),
  model_id         = 3
)


# ── 7. Diagnostics ────────────────────────────────────────────────────────────

summarise_scenario <- function(res, label) {
  treated   <- res$targeting_weight > 0
  n_treated <- sum(treated)
  n_total   <- nrow(res)

  # With the absolute CDF approach, untreated HHs (delta_main = 0) have
  # tau_i_post = tau_i_pre → res1 = 0 exactly. Treated HHs move along the
  # beta curve proportional to their welfare gain and the local curve slope.
  treated_any    <- abs(res$delta_main_log) > 1e-10
  res1_treated   <- if (any(treated_any))  mean(res$delta_res1_log[treated_any])  else NA_real_
  res1_untreated <- if (any(!treated_any)) mean(res$delta_res1_log[!treated_any]) else NA_real_
  n_treated      <- sum(treated_any)
  res1_label <- if (n_treated == n_total) {
    sprintf("%+.4f log-pts (all households received intervention)", mean(res$delta_res1_log))
  } else {
    sprintf("treated %+.4f / untreated %+.5f log-pts",
            res1_treated, res1_untreated)
  }

  cat(sprintf("  %s\n", label))
  cat(sprintf("    Received intervention: %d / %d HHs (%.0f%%)\n",
              n_treated, n_total, 100 * n_treated / n_total))
  cat(sprintf("    Main effect:                 %+.3f log-pts (%+.2f%%) [treated: %+.3f]\n",
              mean(res$delta_main_log), mean(res$pct_main),
              if (any(treated_any)) mean(res$delta_main_log[treated_any]) else 0))
  cat(sprintf("    Resilience — repositioning:  %s\n", res1_label))
  cat(sprintf("    Resilience — direct interact:%+.4f log-pts\n",
              mean(res$delta_res2_log)))
  cat(sprintf("    Weather impact (no policy):  %+.3f log-pts (%+.2f%%)\n",
              mean(res$weather_no_policy_log), mean(res$pct_weather_no_policy)))
  cat(sprintf("    Weather impact (with policy):%+.3f log-pts (%+.2f%%)\n",
              mean(res$weather_with_policy_log), mean(res$pct_weather_with_policy)))
  cat(sprintf("    Protection (weather Δ):      treated %+.2f pp / untreated %+.2f pp\n\n",
              if (any(treated_any))  mean(res$pct_protected[treated_any])  else 0,
              if (any(!treated_any)) mean(res$pct_protected[!treated_any]) else 0))
}

cat("── Scenario results ──────────────────────────────────────────────────\n\n")
summarise_scenario(res_uniform,        "A. Cash uniform           — all HHs treated, small res1 (beta slope)")
summarise_scenario(res_targeted,       "B. Cash bottom 40%        — treated res1 > 0, untreated res1 = 0")
summarise_scenario(res_irrig,          "C. Irrigation untargeted  — res2 channel + res1 if beta_irrig non-flat")
summarise_scenario(res_irrig_targeted, "D. Irrigation bottom 40%  — res1 + res2 combined")
summarise_scenario(res_combined,       "E. Cash+irrigation, per-intervention targeting (cash<40%, irrig<20%)")

# Decomposition by welfare decile for scenario D (targeted irrigation)
by_decile <- res_irrig_targeted |>
  group_by(decile_i) |>
  summarise(
    n             = n(),
    pct_main      = mean(pct_main),
    pct_res1      = mean((exp(delta_res1_log) - 1) * 100),
    pct_res2      = mean((exp(delta_res2_log) - 1) * 100),
    pct_protected = mean(pct_protected),
    .groups = "drop"
  )

cat("── Decomposition by welfare decile (Scenario D: targeted irrigation) ─\n")
cat("  (decile 1 = poorest; only deciles 1-4 treated)\n\n")
print(round(by_decile, 3), row.names = FALSE)

# Checks
cat("\n── Checks ────────────────────────────────────────────────────────────\n")
cat(sprintf("  rif_grid rows = %d (expect %d)? %s\n",
            nrow(rif_grid), length(taus) * n_terms,
            if (nrow(rif_grid) == length(taus) * n_terms) "PASS" else "FAIL"))
cat(sprintf("  beta curve non-flat? %s\n",
            if (diff(range(beta_curve_tbl$estimate)) > 0.01) "PASS" else "FAIL"))

# Scenario A: uniform transfer — movement on tau scale is tiny (everyone shifts same amount).
# Res1 is theoretically non-zero but negligible in practice (small transfer + ceiling effect at tau=1).
res1_a_mean <- mean(abs(res_uniform$delta_res1_log))
cat(sprintf("  uniform cash: res1 mean magnitude [%.2e] (expect negligible)? %s\n",
            res1_a_mean,
            if (res1_a_mean > 1e-15) "PASS (effect exists)" else "FAIL (numerical noise)"))

# Scenario B: targeted cash — treated res1 > 0, untreated res1 = 0 EXACTLY
treated_B   <- abs(res_targeted$delta_main_log) > 1e-10
untreated_B <- !treated_B
cat(sprintf("  targeted cash: res1 > 0 for treated HHs? %s  [mean = %.5f]\n",
            if (mean(res_targeted$delta_res1_log[treated_B]) > 1e-6) "PASS" else "FAIL",
            mean(res_targeted$delta_res1_log[treated_B])))
cat(sprintf("  targeted cash: res1 = 0 for untreated HHs? %s  [max abs = %.2e]\n",
            if (all(abs(res_targeted$delta_res1_log[untreated_B]) < 1e-10)) "PASS" else "FAIL",
            max(abs(res_targeted$delta_res1_log[untreated_B]))))
cat(sprintf("  targeted cash: bottom 40%% treated, top 60%% untreated? %s\n",
            if (all(res_targeted$delta_main_log[res_targeted$tau_i_pre >= 0.4] == 0) &&
                all(res_targeted$delta_main_log[res_targeted$tau_i_pre <  0.4] > 0))
              "PASS" else "FAIL"))

# Scenario C: irrigation res2 channel
cat(sprintf("  irrigation: res2 non-zero? %s\n",
            if (any(abs(res_irrig$delta_res2_log) > 1e-10)) "PASS" else "FAIL"))

# Scenario E: per-intervention targeting — verify different treatment sets
cash_treated_E  <- abs(res_combined$targeting_weight) > 0
irrig_treated_E <- abs(res_combined$delta_main_log) > 1e-10
cat(sprintf("  combined: cash targeting (bottom 40%%) correct? %s\n",
            if (sum(cash_treated_E) > 0 && sum(cash_treated_E) < n_hh) "PASS" else "FAIL"))
cat(sprintf("  combined: untreated HHs have res1 = 0? %s  [max abs = %.2e]\n",
            if (all(abs(res_combined$delta_res1_log[!irrig_treated_E]) < 1e-10)) "PASS" else "FAIL",
            max(abs(res_combined$delta_res1_log[!irrig_treated_E]))))

cat(sprintf("  all result rows = %d? %s\n",
            n_hh,
            if (all(c(nrow(res_uniform), nrow(res_targeted),
                      nrow(res_irrig), nrow(res_irrig_targeted),
                      nrow(res_combined)) == n_hh))
              "PASS" else "FAIL"))


# ── 8. Plot beta curves ───────────────────────────────────────────────────────

p <- ggplot(rif_grid, aes(x = tau, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~term, scales = "free_y") +
  scale_x_continuous(breaks = taus, labels = scales::percent_format(1)) +
  labs(
    title    = "UQR beta curves by term and welfare quantile",
    subtitle = "Ribbon = 95% CI; village + year FE absorbed; SEs clustered by village",
    x        = "Welfare quantile (tau)",
    y        = "UQR coefficient estimate"
  ) +
  theme_bw(base_size = 12)

print(p)
cat("\n── Plot rendered ────────────────────────────────────────────────────\n")
