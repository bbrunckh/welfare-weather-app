# ============================================================================ #
# Pure functions for Unconditional Quantile Regression (RIF).                  #
#                                                                              #
# Implements the Recentered Influence Function approach of                     #
# Firpo, Fortin & Lemieux (2009) for estimating distributional impacts.        #
#                                                                              #
# Used by:                                                                     #
#   Module 1 - fct_fit_model.R (compute_rif, build_rif_grid)                   #
#   Module 2 - fct_simulations.R (predict_rif)                                 #
# ============================================================================ #


# ---------------------------------------------------------------------------- #
# RIF computation                                                               #
# ---------------------------------------------------------------------------- #

#' Compute the Recentered Influence Function for a given quantile
#'
#' Transforms the outcome \code{y} into its RIF representation at quantile
#' \code{tau}, following Firpo, Fortin & Lemieux (2009). The resulting vector
#' can be used as a dependent variable in OLS to estimate unconditional
#' quantile partial effects.
#'
#' @param y   Numeric vector (the outcome).
#' @param tau Scalar in (0, 1) specifying the quantile.
#' @param bw  Optional bandwidth for kernel density estimation. When
#'   \code{NULL} (default), uses \code{stats::bw.SJ()}. Ignored when
#'   \code{dens} is supplied.
#' @param dens Optional pre-built \code{stats::density()} object for
#'   \code{y[is.finite(y)]}. Bandwidth selection and the KDE depend only on
#'   \code{y}, not on \code{tau}, so callers that evaluate several taus for
#'   the same \code{y} (e.g. \code{prepare_outcome()}'s 9-tau RIF grid) can
#'   build this once and pass it in to avoid repeating \code{bw.SJ()}/
#'   \code{density()} per tau (see PERF-03). When \code{NULL} (default), it
#'   is built from \code{y}/\code{bw} exactly as before.
#'
#' @return Numeric vector of RIF values, same length as \code{y}.
#'
#' @export
compute_rif <- function(y, tau, bw = NULL, dens = NULL) {
  na_mask <- !is.finite(y)
  y_obs   <- y[!na_mask]
  q_tau   <- stats::quantile(y_obs, probs = tau, names = FALSE)

  # Robust bandwidth: SJ can fail on large/multimodal data

  if (is.null(dens)) {
    bw_use <- bw
    if (is.null(bw_use)) {
      bw_use <- tryCatch(stats::bw.SJ(y_obs), error = function(e) stats::bw.nrd0(y_obs))
    }

    # Standard KDE with interpolation at the quantile

    dens <- stats::density(y_obs, bw = bw_use, n = 1024)
  }
  f_q  <- stats::approx(dens$x, dens$y, xout = q_tau)$y

  # Scale-aware floor: fraction of peak density

  if (is.na(f_q) || f_q <= 0) {
    f_q <- max(dens$y) * 0.01
    warning(sprintf("Density near zero at quantile %.2f; using floor.", tau))
  }
  f_q <- max(f_q, max(dens$y) * 0.001)

  rif <- rep(NA_real_, length(y))
  rif[!na_mask] <- q_tau + (tau - as.numeric(y_obs <= q_tau)) / f_q
  rif
}

# Direct prediction is deliberately limited to ordinary fixed-effect OLS
# models. Unsupported fixest features return NULL so callers retain the exact
# fixest::predict() fallback.
.direct_fixest_metadata_one <- function(fit) {
  if (!inherits(fit, "fixest") || !identical(fit$method_type, "feols") ||
      isTRUE(fit$iv) || !is.null(fit$NL.fml) || !is.null(fit$call$offset) ||
      !is.null(fit$fixef_terms)) return(NULL)
  beta <- tryCatch(stats::coef(fit), error = function(e) NULL)
  fixefs <- tryCatch(fixest::fixef(fit, notes = FALSE),
                     error = function(e) NULL)
  if (is.null(beta) || is.null(fixefs)) return(NULL)
  fe_vars <- fit$fixef_vars %||% character()
  fe_names <- lapply(fe_vars, function(fe_var) {
    vars <- all.vars(tryCatch(str2lang(fe_var), error = function(e) NULL))
    if (length(vars) != 1L) return(NULL)
    vars
  })
  if (length(fe_vars) && any(vapply(fe_names, is.null, logical(1L)))) return(NULL)
  rhs_vars <- tryCatch(all.vars(fit$fml[[3L]]), error = function(e) NULL)
  if (is.null(rhs_vars)) return(NULL)
  list(beta = beta, fixefs = fixefs, fe_vars = fe_vars, fe_names = fe_names,
       rhs_vars = rhs_vars)
}

build_direct_rif_metadata <- function(fits) {
  metadata <- lapply(fits, .direct_fixest_metadata_one)
  if (!length(metadata) || any(vapply(metadata, is.null, logical(1L)))) return(NULL)
  metadata
}

.direct_fixest_design <- function(fit, data) {
  X <- tryCatch(stats::model.matrix(fit, data = data, type = "rhs"),
                error = function(e) NULL)
  if (is.null(X) || !is.numeric(X)) return(NULL)
  list(X = X, data = data)
}

.direct_rif_baseline_signature <- function(fit, metadata, base) {
  rhs_vars <- metadata[[1L]]$rhs_vars %||% character()
  fe_names <- metadata[[1L]]$fe_names %||% list()
  fe_vars <- if (length(fe_names)) {
    vapply(fe_names, `[[`, character(1L), 1L)
  } else character()
  relevant <- unique(c(rhs_vars, fe_vars))
  if (any(!relevant %in% names(base))) return(NULL)

  # The cache is scoped to one model run. Retaining factor levels in the
  # signature prevents unsafe reuse when a caller changes their encoding.
  digest::digest(list(
    formula = paste(deparse(fit$fml), collapse = ""),
    contrasts = getOption("contrasts"),
    beta_names = lapply(metadata, function(meta) names(meta$beta)),
    fe_levels = lapply(metadata[[1L]]$fixefs, names),
    n = nrow(base),
    data = base[, relevant, drop = FALSE]
  ), serialize = TRUE, algo = "xxhash64")
}

.direct_rif_baseline_design <- function(fits, metadata, base,
                                        signature = NULL) {
  if (!length(fits) || !length(metadata)) return(NULL)
  base_design <- .direct_fixest_design(fits[[1L]], base)
  if (is.null(base_design)) return(NULL)

  fe_vars <- metadata[[1L]]$fe_vars %||% character()
  fe_names <- metadata[[1L]]$fe_names %||% list()
  if (length(fe_vars) != length(fe_names)) return(NULL)
  level_values <- lapply(seq_along(fe_vars), function(i) {
    var <- fe_names[[i]][[1L]]
    if (!var %in% names(base)) return(NULL)
    names(metadata[[1L]]$fixefs[[fe_vars[[i]]]])
  })
  if (length(level_values) && any(vapply(level_values, is.null, logical(1))))
    return(NULL)
  level_index <- function(data, i) {
    if (!length(level_values)) return(integer(nrow(data)))
    match(as.character(data[[fe_names[[i]][[1L]]]]), level_values[[i]])
  }
  base_fe_index <- lapply(seq_along(fe_vars), function(i) level_index(base, i))
  if (any(vapply(base_fe_index, anyNA, logical(1)))) return(NULL)

  beta_columns <- lapply(metadata, function(meta) {
    match(names(meta$beta), colnames(base_design$X))
  })
  if (any(vapply(beta_columns, function(x) anyNA(x), logical(1)))) return(NULL)

  list(base_X = base_design$X, base_fe_index = base_fe_index,
       beta_columns = beta_columns, fe_levels = level_values,
       signature = signature)
}

.direct_rif_baseline_get <- function(fits, metadata, base, cache = NULL) {
  signature <- .direct_rif_baseline_signature(fits[[1L]], metadata, base)
  if (is.environment(cache) && !is.null(signature) &&
      exists("entry", envir = cache, inherits = FALSE) &&
      identical(cache$signature, signature)) {
    cache$hits <- (cache$hits %||% 0L) + 1L
    return(cache$entry)
  }
  baseline <- .direct_rif_baseline_design(fits, metadata, base, signature)
  if (is.environment(cache) && !is.null(baseline) && !is.null(signature)) {
    cache$entry <- baseline
    cache$signature <- signature
    cache$misses <- (cache$misses %||% 0L) + 1L
  }
  baseline
}

.direct_rif_design_cache <- function(fits, metadata, base, scenario,
                                     baseline_cache = NULL) {
  if (!length(fits) || !length(metadata)) return(NULL)
  baseline <- .direct_rif_baseline_get(fits, metadata, base, baseline_cache)
  if (is.null(baseline)) return(NULL)
  scen_design <- .direct_fixest_design(fits[[1L]], scenario)
  if (is.null(scen_design) ||
      !identical(colnames(baseline$base_X), colnames(scen_design$X))) return(NULL)

  fe_vars <- metadata[[1L]]$fe_vars %||% character()
  fe_names <- metadata[[1L]]$fe_names %||% list()
  level_index <- function(data, i) {
    if (!length(baseline$fe_levels)) return(integer(nrow(data)))
    match(as.character(data[[fe_names[[i]][[1L]]]]), baseline$fe_levels[[i]])
  }
  scen_fe_index <- lapply(seq_along(fe_vars), function(i) level_index(scenario, i))
  if (any(vapply(scen_fe_index, anyNA, logical(1)))) return(NULL)

  list(base_X = baseline$base_X, scenario_X = scen_design$X,
       beta_columns = baseline$beta_columns,
       fe_indices = list(base = baseline$base_fe_index,
                         scenario = scen_fe_index),
       fe_levels = baseline$fe_levels)
}

.direct_rif_prediction_pair <- function(fits, base, scenario,
                                        metadata = NULL,
                                        baseline_cache = NULL) {
  if (!length(fits)) return(NULL)
  metadata <- metadata %||% build_direct_rif_metadata(fits)
  if (is.null(metadata) || length(metadata) != length(fits)) return(NULL)
  design_cache <- .direct_rif_design_cache(
    fits, metadata, base, scenario, baseline_cache = baseline_cache
  )
  if (is.null(design_cache)) return(NULL)

  direct_one <- function(meta, k) {
    beta <- meta$beta
    add_fixed_effects <- function(which, n) {
      if (!length(meta$fe_vars)) return(numeric(n))
      indices <- design_cache$fe_indices[[which]]
      Reduce(`+`, lapply(seq_along(meta$fe_vars), function(i) {
        values <- meta$fixefs[[meta$fe_vars[[i]]]]
        values <- as.numeric(values[match(design_cache$fe_levels[[i]], names(values))])
        values[indices[[i]]]
      }))
    }
    base_fe <- add_fixed_effects("base", nrow(design_cache$base_X))
    scen_fe <- add_fixed_effects("scenario", nrow(design_cache$scenario_X))
    list(
      base = as.numeric(design_cache$base_X[, design_cache$beta_columns[[k]], drop = FALSE] %*% beta) + base_fe,
      scenario = as.numeric(design_cache$scenario_X[, design_cache$beta_columns[[k]], drop = FALSE] %*% beta) + scen_fe
    )
  }

  out <- lapply(seq_along(metadata), function(k) direct_one(metadata[[k]], k))
  if (any(vapply(out, is.null, logical(1L)))) return(NULL)
  attr(out, "design_cache") <- design_cache
  out
}


#' Compute RIF outcomes for several quantiles with shared preparation
#'
#' This is the fit-time counterpart to \code{compute_rif()}. It preserves that
#' function's per-quantile output while sharing the finite-value scan, type-7
#' quantile calculation, and density interpolation across the quantile grid.
#' Each RIF vector is still built separately to avoid an \code{N x K} logical
#' comparison matrix.
#'
#' @param y Numeric outcome vector.
#' @param taus Numeric vector of quantiles in the required output order.
#' @param bw Optional bandwidth, as in \code{compute_rif()}.
#' @param dens Optional pre-built density object, as in \code{compute_rif()}.
#'
#' @return An unnamed list of numeric vectors, one for each value of
#'   \code{taus}, in the same order.
#'
#' @keywords internal
compute_rif_multi <- function(y, taus, bw = NULL, dens = NULL) {
  na_mask <- !is.finite(y)
  y_obs   <- y[!na_mask]
  q_taus  <- stats::quantile(y_obs, probs = taus, names = FALSE, type = 7)

  if (is.null(dens)) {
    bw_use <- bw
    if (is.null(bw_use)) {
      bw_use <- tryCatch(stats::bw.SJ(y_obs), error = function(e) stats::bw.nrd0(y_obs))
    }
    dens <- stats::density(y_obs, bw = bw_use, n = 1024)
  }

  f_taus   <- stats::approx(dens$x, dens$y, xout = q_taus)$y
  dens_max <- max(dens$y)

  lapply(seq_along(taus), function(i) {
    f_q <- f_taus[i]
    if (is.na(f_q) || f_q <= 0) {
      f_q <- dens_max * 0.01
      warning(sprintf("Density near zero at quantile %.2f; using floor.", taus[i]))
    }
    f_q <- max(f_q, dens_max * 0.001)

    rif <- rep(NA_real_, length(y))
    rif[!na_mask] <- q_taus[i] +
      (taus[i] - as.numeric(y_obs <= q_taus[i])) / f_q
    rif
  })
}


# ---------------------------------------------------------------------------- #
# Grid construction                                                             #
# ---------------------------------------------------------------------------- #

#' Build a tidy data frame of RIF regression coefficients ("beta curves")
#'
#' Extracts coefficients from a \code{fixest_multi} object (stacked feols
#' result) and arranges them into a long-format grid with one row per
#' (quantile x term) combination.
#'
#' @param fits_multi A \code{fixest_multi} object returned by
#'   \code{fixest::feols()} with a stacked LHS.
#' @param taus Numeric vector of quantile values corresponding to each
#'   element in \code{fits_multi}.
#' @param model_id Integer model identifier (1 = weather only, 2 = + FE,
#'   3 = + FE + controls).
#'
#' @return A data frame with columns: \code{tau}, \code{term},
#'   \code{estimate}, \code{std.error}, \code{conf.low}, \code{conf.high},
#'   \code{model}.
#'
#' @export
build_rif_grid <- function(fits_multi, taus, model_id) {
  purrr::map_dfr(seq_along(taus), function(i) {
    fit_i <- fits_multi[[i]]
    # Try fit-time VCV first (respects cluster= passed at estimation)
    tbl <- tryCatch(broom::tidy(fit_i, conf.int = TRUE), error = function(e) NULL)
    if (is.null(tbl)) {
      for (spec in list(COEF_VCOV_SPEC, ~loc_id, "HC1", "iid")) {
        tbl <- tryCatch(
          broom::tidy(fit_i, conf.int = TRUE, vcov = spec),
          error = function(e) NULL
        )
        if (!is.null(tbl)) break
      }
    }
    if (is.null(tbl)) tbl <- broom::tidy(fit_i, conf.int = TRUE)
    tbl$tau   <- taus[i]
    tbl$model <- model_id
    tbl
  })
}


# ---------------------------------------------------------------------------- #
# RIF Simulation: Delta Method Prediction                                      #
# ---------------------------------------------------------------------------- #

#' Predict welfare outcomes using RIF delta method
#'
#' For each household, assigns a quantile position via ecdf, predicts
#' the change in RIF values between baseline and scenario weather at that
#' quantile, then adds the delta to the observed baseline welfare.
#'
#' @param fit_multi A \code{fixest_multi} object (9 sub-models, one per tau).
#' @param newdata Data frame from \code{prepare_hist_weather()} - has scenario
#'   weather columns and \code{.svy_row_id}.
#' @param svy The raw survey data passed to \code{prepare_hist_weather()}.
#' @param train_data Training data used for ecdf quantile assignment.
#' @param taus Numeric vector of quantiles (e.g. \code{seq(0.1, 0.9, 0.1)}).
#' @param outcome Character; name of the outcome column.
#' @param weather_cols Character vector of weather variable column names.
#' @param so Selected outcome metadata (list with \code{$transform}).
#'   When \code{so$transform == "log"}, the baseline outcome is log-transformed
#'   to match the scale used during model fitting.
#' @param chol_list Optional list of Cholesky factor matrices (one per tau),
#'   as returned by \code{compute_chol_vcov(fit_multi)}. When provided,
#'   an \code{F_loading} matrix is attached as an attribute of the result for
#'   use in \code{aggregate_with_uncertainty_delta()}.
#' @param ecdf_train Optional pre-built \code{stats::ecdf()} of
#'   \code{train_data[[outcome]]}. \code{train_data} is identical across
#'   simulation keys for a given model fit, so callers that invoke
#'   \code{predict_rif()} in a per-key loop (e.g. \code{run_sim_pipeline()})
#'   can build this once and pass it in to avoid rebuilding the same
#'   empirical CDF on every key. When \code{NULL} (default), it is built
#'   from \code{train_data} as before.
#'
#' @return \code{newdata} augmented with \code{.fitted}, \code{.residual}, and outcome.
#'   When \code{chol_list} is non-NULL, also carries \code{attr(., "F_loading")}.
#'
#' @export
predict_rif <- function(fit_multi, newdata, svy, train_data, taus, outcome,
                        weather_cols, so = NULL, chol_list = NULL,
                        ecdf_train = NULL, batch_predictions = FALSE,
                        direct_predictions = FALSE,
                        direct_metadata = NULL,
                        direct_baseline_cache = NULL) {
  stopifnot(
    ".svy_row_id must be present in newdata" = ".svy_row_id" %in% names(newdata),
    "taus must be non-empty" = length(taus) > 0,
    "fit_multi must have same length as taus" = length(fit_multi) == length(taus)
  )

  svy_row    <- newdata$.svy_row_id
  y_raw      <- svy[[outcome]][svy_row]
  n          <- nrow(newdata)
  K          <- length(taus)

  # Transform y_baseline to model scale (log if applicable)
  # train_data[[outcome]] is already in model scale (log-transformed by
  # prepare_outcome_df before fitting), so ecdf and predictions are in log scale.
  is_log     <- isTRUE(so$transform == "log")
  y_baseline <- if (is_log) log(y_raw) else y_raw

  # Assign quantile position via ecdf of training data (in model scale).
  # Reuse a caller-supplied ecdf when available (see PERF-27); otherwise
  # build it here as before.
  F_hat <- if (!is.null(ecdf_train)) ecdf_train else stats::ecdf(train_data[[outcome]])
  tau_i <- pmin(pmax(F_hat(y_baseline), min(taus)), max(taus))

  # Pre-build baseline and scenario data frames ONCE (avoid K*2 column copies)
  newdata_base <- newdata
  for (wc in weather_cols) {
    newdata_base[[wc]] <- svy[[wc]][svy_row]
  }
  newdata_scen <- newdata  # already has scenario weather

  # Predict at each quantile for baseline and scenario weather
  # Store deltas in a matrix: rows = observations, cols = quantiles
  delta_mat <- matrix(NA_real_, nrow = n, ncol = K)

  direct_pairs <- if (isTRUE(direct_predictions)) {
    .direct_rif_prediction_pair(
      fit_multi, newdata_base, newdata_scen, metadata = direct_metadata,
      baseline_cache = direct_baseline_cache
    )
  } else NULL

  predict_pair <- function(fit, base, scenario) {
    if (isTRUE(batch_predictions)) {
      combined <- tryCatch(rbind(base, scenario), error = function(e) NULL)
      if (!is.null(combined)) {
        pair <- tryCatch(
          as.numeric(stats::predict(fit, newdata = combined,
                                    type = "response")),
          error = function(e) NULL
        )
        if (length(pair) == 2L * n) {
          return(list(base = pair[seq_len(n)],
                      scenario = pair[n + seq_len(n)]))
        }
      }
    }
    list(
      base = as.numeric(stats::predict(fit, newdata = base, type = "response")),
      scenario = as.numeric(stats::predict(fit, newdata = scenario, type = "response"))
    )
  }

  # For F_loading: the scenario design matrix X_scenario %*% L_k gives the
  # delta-method gradient of the *predicted welfare level* under the RIF
  # regression at quantile k, consistent with the OLS path's F_loading
  # construction. Downstream:
  #   - Level mode: ||F_loading||^2 = level-CI of the predicted welfare
  #     under the RIF model at this household's quantile position tau_i.
  #     Comparable in width to OLS level bands.
  #   - Deviation/contrast mode: the aggregation layer subtracts the
  #     historical reference F_agg, so F_agg_s - F_agg_h reduces (per
  #     household i) to (X_scenario_i - X_hist_i) %*% t(L_tau_i) - the
  #     paired-contrast variance, matching the X_diff construction we
  #     previously used. Deviation bands stay tight.
  #
  # The displayed point estimate remains y_observed + delta_i; the level
  # SE refers to the predicted level under the regression, not to that
  # observed-anchored displayed value (analogous to how OLS's level SE
  # refers to its predicted, not "true," welfare).
  #
  # Memory: rather than materialising all K full N*P scenario design matrices
  # up front (~ K * N * p * 8 bytes - ~8 GB for a large-N country with K=9),
  # interpolate_F_loading() builds each quantile's design matrix on demand for
  # only the rows that need it (it processes rows grouped by their (lo, hi)
  # quantile pair). We hand it a lazy accessor that calls model.matrix() on a
  # row subset of newdata_scen. Bit-identical to the all-at-once form because
  # matrix multiply distributes over row subsetting.
  compute_loading <- !is.null(chol_list) && length(chol_list) == K

  for (k in seq_len(K)) {
    # Baseline weather prediction (needed for the climate-delta point
    # estimate; not used for F_loading any more).
    pair <- direct_pairs[[k]] %||%
      predict_pair(fit_multi[[k]], newdata_base, newdata_scen)
    pred_base <- pair$base
    pred_new  <- pair$scenario

    delta_mat[, k] <- pred_new - pred_base
  }

  # Clean up the baseline frame. newdata_scen is retained until after F_loading
  # is built below, since the lazy design-matrix accessor reads from it.
  rm(newdata_base)

  # Interpolate delta at each household's tau_i position
  delta_i <- interpolate_delta(delta_mat, taus, tau_i)

  # Assemble output
  newdata$.fitted    <- y_baseline + delta_i
  newdata$.residual  <- NA_real_
  newdata[[outcome]] <- y_baseline + delta_i

  # Compute F_loading by interpolating X_scenario %*% L_k at each tau_i.
  # X_scen_fn(k, rows) builds the quantile-k scenario design matrix for the
  # given rows only - model.matrix() on a row subset of newdata_scen.
  if (compute_loading) {
    active_mask <- attr(chol_list, "active_mask")
    direct_design_cache <- attr(direct_pairs, "design_cache")
    X_scen_fn <- if (!is.null(direct_design_cache)) {
      function(k, rows) direct_design_cache$scenario_X[rows, , drop = FALSE]
    } else function(k, rows) {
      stats::model.matrix(fit_multi[[k]], data = newdata_scen[rows, , drop = FALSE],
                          type = "rhs")
    }
    F_loading <- tryCatch({
      interpolate_F_loading(X_scen_fn, chol_list, taus, tau_i,
                             active_mask = active_mask)
    }, error = function(e) {
      warning("[predict_rif] F_loading interpolation failed: ", conditionMessage(e))
      NULL
    })
    if (!is.null(F_loading)) {
      attr(newdata, "F_loading") <- F_loading
      # Diagnostic: confirm the additive-decomposition mask reached predict_rif.
      # Compare ncol(F_loading) against the per-tau design width - if the mask
      # is active, ncol should be < length(active_mask).
      if (!is.null(active_mask) && ncol(F_loading) < length(active_mask)) {
        message(sprintf(
          "[predict_rif] additive-decomposition mask applied: F_loading is %d x %d (out of %d coefficients).",
          nrow(F_loading), ncol(F_loading), length(active_mask)))
      }
    }
  }

  rm(newdata_scen)

  newdata
}


#' Interpolate factor loadings at arbitrary quantile positions
#'
#' For each household i, linearly interpolates between the two adjacent
#' quantile factor-loading rows to produce a single N x P F_loading matrix.
#'
#' Each per-quantile loading is \eqn{F_k = X\_diff_k \%*\% t(L_k)}, where
#' \eqn{X\_diff_k = X_{scenario} - X_{baseline}} at quantile k.
#'
#' @param X_diff_fn  Function \code{(k, rows)} returning the quantile-k design
#'   matrix (\code{length(rows) x p}) for the requested rows only - built on
#'   demand so all K full N*p matrices never coexist in memory. (For backward
#'   compatibility, a list of K full \code{n x p} matrices is also accepted and
#'   wrapped automatically.)
#' @param chol_list  List of K Cholesky factor matrices (p x p), one per quantile.
#' @param taus       Numeric vector of length K (sorted quantile grid).
#' @param tau_i      Numeric vector of length n (household quantile positions).
#' @param active_mask Optional logical vector of length p. When supplied,
#'   each per-quantile loading is subset to the active columns before the
#'   interpolation blend (additive-decomposition SE). NULL = no masking.
#'
#' @return Numeric n x p matrix of interpolated factor loadings (or
#'   n x sum(active_mask) when masking is applied).
#'
#' @keywords internal
interpolate_F_loading <- function(X_diff_fn, chol_list, taus, tau_i,
                                   active_mask = NULL) {
  K <- length(taus)

  # Accept a prebuilt list of K matrices (legacy callers / tests) by wrapping
  # it in the same (k, rows) accessor the streaming path expects.
  if (is.list(X_diff_fn) && !is.function(X_diff_fn)) {
    X_diff_list <- X_diff_fn
    n <- nrow(X_diff_list[[1]])
    X_diff_fn <- function(k, rows) X_diff_list[[k]][rows, , drop = FALSE]
  } else {
    n <- length(tau_i)
  }

  # Find interval: taus[idx] <= tau_i < taus[idx+1]
  idx <- findInterval(tau_i, taus, all.inside = TRUE)
  idx_hi <- pmin(idx + 1L, K)

  # Interpolation weights
  tau_lo <- taus[idx]
  tau_hi <- taus[idx_hi]
  w      <- ifelse(tau_hi > tau_lo, (tau_i - tau_lo) / (tau_hi - tau_lo), 0)

  # F = X %*% L (lower triangular L with LL' = Sigma) so that
  # F F' = X L L' X' = X Sigma X' - the correct level variance.
  # Earlier versions used t(L) here, which gives X L' L X' (not Sigma)
  # and inflated SEs by ~25% on realistic Sigma. The linear path
  # (compute_factor_loading) was fixed for this; this matches.
  #
  # When active_mask is set, `chol_list[[k]]` is expected to be the
  # K_active x K_active Cholesky of the active block of Sigma (built by
  # attach_active_mask), and the design matrix is the full N x K design.
  # We subset X to active columns before multiplying.
  #
  # Memory: each output row i reads from exactly two quantile loadings,
  # at idx[i] and idx_hi[i]. Rather than materialising all K full N*P design
  # matrices (~ K * N * p * 8 bytes), we process rows grouped by their
  # (lo, hi) quantile pair and build (via X_diff_fn) only that group's rows of
  # the at-most-two quantiles it needs. Matrix multiply distributes over row
  # subsetting exactly - (X[rows, ] %*% C) is row-for-row identical to
  # (X %*% C)[rows, ] - so the result is bit-equivalent to the all-at-once
  # form, at a fraction of the peak allocation (one N*P result + small
  # per-group scratch).

  # Subset a group's design matrix to active columns when masking is on.
  apply_mask <- function(Xk) {
    if (!is.null(active_mask) && length(active_mask) == ncol(Xk)) {
      Xk[, active_mask, drop = FALSE]
    } else {
      Xk
    }
  }
  p <- ncol(chol_list[[idx[1]]])

  F <- matrix(NA_real_, nrow = n, ncol = p)

  # Group rows by their (lo, hi) quantile-index pair. Each group needs at
  # most two per-quantile matmuls, restricted to the group's rows.
  pair_key <- idx + idx_hi * (K + 1L)  # unique per (lo, hi) combination
  pair_groups <- split(seq_len(n), pair_key, drop = TRUE)
  for (rows in pair_groups) {
    a <- idx[rows[1]]
    b <- idx_hi[rows[1]]
    w_g <- w[rows]

    F_a <- apply_mask(X_diff_fn(a, rows)) %*% chol_list[[a]]
    F_b <- if (b == a) F_a else apply_mask(X_diff_fn(b, rows)) %*% chol_list[[b]]

    F[rows, ] <- (1 - w_g) * F_a + w_g * F_b
  }

  F
}


#' Interpolate delta values at arbitrary quantile positions
#'
#' @param delta_mat Matrix (n x K) of delta values at each quantile.
#' @param taus Numeric vector of length K (sorted quantile grid).
#' @param tau_i Numeric vector of length n (household quantile positions).
#'
#' @return Numeric vector of length n with interpolated deltas.
#'
#' @keywords internal
interpolate_delta <- function(delta_mat, taus, tau_i) {
  n <- length(tau_i)
  K <- length(taus)

  # Find interval: idx such that taus[idx] <= tau_i < taus[idx+1]
  idx <- findInterval(tau_i, taus, all.inside = TRUE)

  # Linear interpolation weights
  tau_lo <- taus[idx]
  tau_hi <- taus[pmin(idx + 1L, K)]
  w      <- ifelse(tau_hi > tau_lo, (tau_i - tau_lo) / (tau_hi - tau_lo), 0)

  # Interpolated delta
  delta_lo <- delta_mat[cbind(seq_len(n), idx)]
  delta_hi <- delta_mat[cbind(seq_len(n), pmin(idx + 1L, K))]

  delta_lo * (1 - w) + delta_hi * w
}
