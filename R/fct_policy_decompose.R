# Policy effect decomposition ----
# Harmonized policy effect decomposition for RIF and OLS/fixest engines.
#
# Decomposes total policy effect into:
#   - Main effect (direct welfare shift from policy)
#   - Resilience effect:
#       - Repositioning (RIF only): movement along weather beta curve
#       - Interaction: weather x policy interaction terms
#
# The shared helper .compute_rif_channels() is the single source of truth
# for all RIF channel calculations. Both the decomposition display AND the
# RIF simulation pipeline (.compute_rif_policy_correction below, invoked
# from run_sim_pipeline in fct_simulations.R) call it, so the Results pane
# and the Decomposition pane always agree numerically.
#
# Used by: mod_3_06_policy_sim.R (decomposition display)
#          fct_simulations.R     (run_sim_pipeline RIF policy correction)


# Shared helpers ----

#' Compute covariate deltas between baseline and policy survey data
#'
#' @param svy_baseline Baseline survey data frame.
#' @param svy_policy Policy-modified survey data frame.
#' @param outcome Outcome variable name.
#' @param weather_vars Weather variable names to exclude from deltas.
#' @param candidate_cols Optional known policy candidate columns. NULL retains
#'   the generic all-shared-columns behavior.
#' @return Named list of numeric delta vectors for changed covariates.
#' @keywords internal
.compute_policy_deltas <- function(svy_baseline, svy_policy, outcome, weather_vars,
                                   candidate_cols = NULL) {
  exclude_cols <- c(
    outcome, SP_TRANSFER_COL, ".svy_row_id", weather_vars,
    "year", "sim_year", "int_month", "code", "survname", "loc_id",
    grep("^weight$|^hhweight$|^wgt$|^pw$", names(svy_baseline),
      value = TRUE, ignore.case = TRUE
    )
  )
  check_cols <- intersect(names(svy_baseline), names(svy_policy))
  if (!is.null(candidate_cols)) {
    check_cols <- check_cols[check_cols %in% candidate_cols]
  }
  check_cols <- setdiff(check_cols, exclude_cols)

  deltas <- list()
  for (col in check_cols) {
    b <- svy_baseline[[col]]
    p <- svy_policy[[col]]
    # Coerce factors to numeric for comparison (suppress warnings for non-numeric levels)
    if (is.factor(b)) b <- suppressWarnings(as.numeric(as.character(b)))
    if (is.factor(p)) p <- suppressWarnings(as.numeric(as.character(p)))
    if (is.numeric(b) && is.numeric(p) && !all(is.na(b)) && !all(is.na(p))) {
      d <- p - b
      if (any(abs(d) > 1e-10, na.rm = TRUE)) {
        deltas[[col]] <- d
      }
    }
  }
  deltas
}

.build_rif_curve_index <- function(grid3) {
  if (is.null(grid3) || !nrow(grid3) ||
    !all(c("term", "tau", "estimate") %in% names(grid3))) {
    return(list())
  }
  groups <- split(seq_len(nrow(grid3)), as.character(grid3$term), drop = TRUE)
  lapply(groups, function(rows) {
    list(
      tau = grid3$tau[rows],
      estimate = grid3$estimate[rows],
      std.error = if ("std.error" %in% names(grid3)) grid3$std.error[rows] else NULL
    )
  })
}


#' Compute all RIF policy channels (single source of truth)
#'
#' Returns a list with all decomposition channels and (optionally) per-channel
#' analytic standard errors via the delta method. Both the decomposition
#' display and the simulation correction call this function to ensure
#' the numbers are identical.
#'
#' **Uncertainty assumption (v1):** per-channel SEs are computed under a
#' diagonal coefficient covariance - each `(tau, term)` beta draws from
#' `rif_grid$std.error` with zero off-diagonal covariance. Under that
#' assumption channels are independent, so
#' `Var(Delta_total) = Var(Delta_main) + Var(Delta_res1) + Var(Delta_res2)` exactly. This
#' is conservative for channels using *different* tau slots (Delta_main at tau_pre,
#' Delta_res2 at tau_post) and slightly understates total variance for channels
#' sharing tau (Delta_main / Delta_res1 at tau_pre, Delta_res1 / Delta_res2 at tau_post).
#'
#' @param svy_baseline Baseline survey data frame.
#' @param deltas Named list of covariate delta vectors (from .compute_policy_deltas).
#' @param sp_transfer Numeric vector of SP cash transfer amounts (level scale).
#' @param hazard_values Named list of hazard value vectors (one per weather var).
#' @param weather_vars Character vector of weather variable names.
#' @param rif_grid RIF coefficient grid data frame (must have model, term, tau, estimate).
#' @param taus Numeric vector of quantile grid points.
#' @param train_data Training data (for ecdf).
#' @param outcome Character, outcome variable name.
#' @param is_log Logical, whether outcome is log-transformed.
#' @param skip_coef Logical. When TRUE, all per-channel SEs are returned as 0.
#' @param central_only Logical. When TRUE, omit all coefficient-SE work and
#'   return only the channel values needed to form `delta_total`.
#' @param F_hat Optional pre-built empirical CDF of the training outcome
#'   (\code{stats::ecdf(train_data[[outcome]])}). The ecdf is weather- and
#'   policy-independent, so per-year/per-scenario callers can build it once
#'   and pass it in instead of rebuilding it on every call (PERF-22). When
#'   NULL it is computed here (backward compatible).
#' @return Named list with delta_* and sd_* vectors of length n, plus
#'   tau_i_pre / tau_i_post / has_interactions. In central-only mode, returns
#'   only `delta_total` and `has_interactions`. NULL if inputs are invalid.
#' @keywords internal
.compute_rif_channels <- function(svy_baseline, deltas, sp_transfer,
                                  hazard_values, weather_vars,
                                  rif_grid, taus, train_data,
                                  outcome, is_log,
                                  skip_coef = FALSE,
                                  central_only = FALSE,
                                  F_hat = NULL,
                                  context = NULL) {
  n <- nrow(svy_baseline)

  # Only use model 3 coefficients
  grid3 <- context$grid3 %||% rif_grid[rif_grid$model == 3L, ]
  if (nrow(grid3) == 0) {
    return(NULL)
  }
  all_terms <- context$all_terms %||% unique(grid3$term)
  curve_index <- context$rif_curve_index %||% .build_rif_curve_index(grid3)

  # Beta curve interpolation helper
  beta_at <- function(term_name, tau_values) {
    if (!is.null(context$beta_at)) {
      return(context$beta_at(term_name, tau_values))
    }
    curve <- curve_index[[term_name]]
    if (is.null(curve)) {
      return(rep(0, length(tau_values)))
    }
    if (length(curve$tau) == 1L) {
      return(rep(curve$estimate[[1L]], length(tau_values)))
    }
    stats::approx(
      x = curve$tau, y = curve$estimate,
      xout = tau_values, rule = 2
    )$y
  }
  # SE curve interpolation helper (same shape; returns 0 if SE absent)
  se_at <- function(term_name, tau_values) {
    if (isTRUE(skip_coef) || isTRUE(central_only)) {
      return(rep(0, length(tau_values)))
    }
    curve <- curve_index[[term_name]]
    if (is.null(curve) || is.null(curve$std.error)) {
      return(rep(0, length(tau_values)))
    }
    if (length(curve$tau) == 1L) {
      return(rep(curve$std.error[[1L]], length(tau_values)))
    }
    stats::approx(
      x = curve$tau, y = curve$std.error,
      xout = tau_values, rule = 2
    )$y
  }

  # Baseline welfare in model scale
  y_baseline <- context$y_baseline %||% {
    y_raw <- svy_baseline[[outcome]]
    if (is_log) log(pmax(y_raw, 1e-10)) else y_raw
  }

  # Baseline quantile position via ecdf of training outcome
  # PERF-22: reuse a caller-supplied ecdf when available - it depends only
  # on train_data and the outcome, never on weather or policy scenario.
  F_hat <- F_hat %||% context$F_hat %||% stats::ecdf(train_data[[outcome]])
  tau_i_pre <- context$tau_i_pre %||%
    pmin(pmax(F_hat(y_baseline), min(taus)), max(taus))

  # --- Main effect: SP cash (log-scale) ---
  delta_sp <- if (is_log) {
    log(pmax(exp(y_baseline) + sp_transfer, 1e-10)) - y_baseline
  } else {
    sp_transfer
  }

  # --- Main effect: covariate shifts beta_x(tau_pre) * delta_x ---
  # Accumulate per-channel variance in parallel under the diagonal-Sigma
  # approximation:  Var = Sigma_term (Deltax * SE_term(tau))^2
  delta_main_covar <- rep(0, n)
  var_main <- if (isTRUE(central_only)) NULL else rep(0, n)
  for (v in names(deltas)) {
    .decomposition_context_counter_add(context, "term_map_reuses")
    term_v <- context$rif_term_map[[v]] %||% if (v %in% all_terms) {
      v
    } else {
      matches <- grep(paste0("^", v), all_terms, value = TRUE)
      if (length(matches) > 0) matches[1] else NULL
    }
    if (!is.null(term_v)) {
      beta_v <- beta_at(term_v, tau_i_pre)
      delta_main_covar <- delta_main_covar + beta_v * deltas[[v]]
      if (!isTRUE(central_only)) {
        se_v <- se_at(term_v, tau_i_pre)
        var_main <- var_main + (deltas[[v]] * se_v)^2
      }
    }
  }

  delta_main <- context$delta_main %||% (delta_sp + delta_main_covar)
  # delta_sp is a function of policy and observed welfare only - no beta
  # dependence - so it adds 0 to var_main.

  # --- Repositioning (res1): tau shift from main effect changes weather beta ---
  # Post-main-effect position on the SAME CDF used for tau_i_pre - i.e. the
  # training-outcome ecdf the RIF coefficient grid was estimated against.
  # Using a different CDF here (e.g. ecdf(svy_baseline)) would make
  # tau_i_post != tau_i_pre even when delta_main == 0, producing a spurious
  # repositioning channel when no policy is selected. Indexing both against
  # F_hat keeps the link to the beta curve intact and makes delta_res1
  # identically zero whenever delta_main is zero.
  y_post_main <- y_baseline + delta_main
  tau_i_post <- context$tau_i_post %||%
    pmin(pmax(F_hat(y_post_main), min(taus)), max(taus))

  delta_res1 <- rep(0, n)
  var_res1 <- if (isTRUE(central_only)) NULL else rep(0, n)
  for (wv in weather_vars) {
    haz <- hazard_values[[wv]]
    if (wv %in% all_terms) {
      # Continuous weather: repositioning via change in beta along the curve
      beta_pre <- beta_at(wv, tau_i_pre)
      beta_post <- beta_at(wv, tau_i_post)
      delta_res1 <- delta_res1 + (beta_post - beta_pre) * haz
      # Independent-tau assumption: Var((beta_post - beta_pre) * haz) = haz^2 (SE_pre^2 + SE_post^2)
      if (!isTRUE(central_only)) {
        se_pre <- se_at(wv, tau_i_pre)
        se_post <- se_at(wv, tau_i_post)
        var_res1 <- var_res1 + (haz^2) * (se_pre^2 + se_post^2)
      }
    } else if (wv %in% names(svy_baseline) && is.factor(svy_baseline[[wv]])) {
      # Binned (factor) weather: use year-specific bin from hazard_values[[wv]],
      # derived from that year's weather_raw via loc_id join - this varies by year.
      # Falls back to svy_baseline[[wv]] when weather_raw is NULL (historical run).
      fac_col <- if (is.factor(hazard_values[[wv]])) hazard_values[[wv]] else svy_baseline[[wv]]
      for (lv in levels(fac_col)) {
        term_name <- paste0(wv, lv)
        if (!term_name %in% all_terms) next # reference level - contribution is 0
        active <- !is.na(fac_col) & fac_col == lv
        if (!any(active)) next
        beta_pre_b <- beta_at(term_name, tau_i_pre[active])
        beta_post_b <- beta_at(term_name, tau_i_post[active])
        delta_res1[active] <- delta_res1[active] + (beta_post_b - beta_pre_b)
        if (!isTRUE(central_only)) {
          se_pre_b <- se_at(term_name, tau_i_pre[active])
          se_post_b <- se_at(term_name, tau_i_post[active])
          var_res1[active] <- var_res1[active] +
            (se_pre_b^2 + se_post_b^2)
        }
      }
    }
  }

  # --- Interaction (res2): beta_Haz:x(tau_post) * Haz * delta_x ---
  # Evaluated at tau_i_post (not tau_i_pre) because the household's welfare

  # level has shifted due to the main effect (SP + covariate changes), and
  # the interaction beta should reflect the vulnerability at the new position.
  # Continuous weather: beta_int(tau_post) * haz_mean * delta_x
  # Binned weather:     beta_int_bin_k(tau_post) * 1 * delta_x  (only for active bin)
  delta_res2 <- rep(0, n)
  var_res2 <- if (isTRUE(central_only)) NULL else rep(0, n)
  has_interactions <- FALSE

  for (wv in weather_vars) {
    haz_int <- hazard_values[[wv]]
    is_binned <- is.factor(haz_int)

    for (v in names(deltas)) {
      if (is_binned) {
        for (lv in levels(haz_int)) {
          cand1 <- paste0(wv, lv, ":", v)
          cand2 <- paste0(v, ":", wv, lv)
          candidates <- if (!is.null(context)) {
            context$rif_interaction_map[[wv]][[v]] %||% character(0)
          } else {
            .resolve_interaction_terms(all_terms, weather_vars, deltas)[[wv]][[v]] %||%
              character(0)
          }
          matched <- c(cand1, cand2)[c(cand1, cand2) %in% candidates]
          it_name <- if (length(matched)) matched[[1L]] else NULL
          if (is.null(it_name)) next
          has_interactions <- TRUE
          active <- !is.na(haz_int) & haz_int == lv
          if (!any(active)) next
          beta_int <- beta_at(it_name, tau_i_post[active])
          delta_res2[active] <- delta_res2[active] + beta_int * deltas[[v]][active]
          if (!isTRUE(central_only)) {
            se_int <- se_at(it_name, tau_i_post[active])
            var_res2[active] <- var_res2[active] +
              (deltas[[v]][active] * se_int)^2
          }
        }
      } else {
        .decomposition_context_counter_add(context, "term_map_reuses")
        candidates <- if (!is.null(context)) {
          context$rif_interaction_map[[wv]][[v]] %||% character(0)
        } else {
          .resolve_interaction_terms(all_terms, weather_vars, deltas)[[wv]][[v]] %||%
            character(0)
        }
        int_term <- if (length(candidates)) candidates[[1L]] else NULL
        if (!is.null(int_term)) {
          has_interactions <- TRUE
          beta_int <- beta_at(int_term, tau_i_post)
          delta_res2 <- delta_res2 + beta_int * haz_int * deltas[[v]]
          if (!isTRUE(central_only)) {
            se_int <- se_at(int_term, tau_i_post)
            var_res2 <- var_res2 +
              (haz_int * deltas[[v]] * se_int)^2
          }
        }
      }
    }
  }

  delta_total <- delta_main + delta_res1 + delta_res2

  if (isTRUE(central_only)) {
    return(list(
      delta_sp         = delta_sp,
      delta_main_covar = delta_main_covar,
      delta_main       = delta_main,
      delta_res1       = delta_res1,
      delta_res2       = delta_res2,
      delta_total      = delta_total,
      has_interactions = has_interactions
    ))
  }

  # Total variance under the channel-independence approximation. Holds exactly
  # when channels use disjoint (tau, term) sets (the common case once SP/cov
  # vs. weather effects are separated); approximate otherwise.
  var_total <- var_main + var_res1 + var_res2

  list(
    delta_sp         = delta_sp,
    delta_main_covar = delta_main_covar,
    delta_main       = delta_main,
    delta_res1       = delta_res1,
    delta_res2       = delta_res2,
    delta_total      = delta_total,
    sd_main          = sqrt(var_main),
    sd_res1          = sqrt(var_res1),
    sd_res2          = sqrt(var_res2),
    sd_total         = sqrt(var_total),
    tau_i_pre        = tau_i_pre,
    tau_i_post       = tau_i_post,
    has_interactions = has_interactions
  )
}


#' Compute per-household hazard values for each weather variable
#'
#' Continuous weather: per-location mean across `weather_raw` rows, joined to
#' each household via `loc_id`. Binned (factor) weather: per-location modal
#' bin. Both fall back to the household's own survey value when `weather_raw`
#' or `loc_id` is unavailable, so the function is safe to call from any
#' year-slice context (historical run, single-year scenario slice, etc.).
#'
#' @param svy_baseline Baseline survey data frame (provides loc_id and the
#'   fallback weather columns).
#' @param weather_raw Data frame of weather values for the period being
#'   evaluated. May be NULL.
#' @param weather_vars Character vector of weather variable names.
#' @return Named list (length `length(weather_vars)`) of hazard vectors, each
#'   of length `nrow(svy_baseline)`.
#' @keywords internal
.compute_hazard_values <- function(svy_baseline, weather_raw, weather_vars,
                                   location_group = NULL) {
  n <- nrow(svy_baseline)
  has_location_keys <- !is.null(weather_raw) &&
    "loc_id" %in% names(weather_raw) && "loc_id" %in% names(svy_baseline)
  if (is.null(location_group) && has_location_keys) {
    loc_id <- weather_raw[["loc_id"]]
    ok_loc <- !is.na(loc_id)
    if (any(ok_loc)) {
      location_group <- collapse::GRP(
        data.frame(loc_id = loc_id[ok_loc]),
        by = "loc_id"
      )
    }
  }
  hazard_values <- lapply(weather_vars, function(wv) {
    if (!is.null(weather_raw) && wv %in% names(weather_raw)) {
      vals <- weather_raw[[wv]]
      has_loc <- "loc_id" %in% names(weather_raw) &&
        "loc_id" %in% names(svy_baseline)

      if (is.factor(vals) || is.character(vals)) {
        if (has_loc) {
          # Modal bin per location: unweighted counts of the non-NA values,
          # ties broken alphabetically like table() did (PERF-05 follow-up).
          loc_id <- weather_raw[["loc_id"]]
          chr_vals <- as.character(vals)
          ok <- !is.na(loc_id) & !is.na(chr_vals)
          modal_bin <- if (any(ok)) {
            gv <- collapse::GRP(
              list(loc_id = loc_id[ok], value = chr_vals[ok]),
              group.sizes = TRUE
            )
            cnt <- as.integer(gv$group.sizes)
            ord <- order(
              gv$groups$loc_id, -cnt,
              match(gv$groups$value, sort(unique(chr_vals[ok])))
            )
            take <- ord[!duplicated(gv$groups$loc_id[ord])]
            setNames(
              gv$groups$value[take],
              as.character(gv$groups$loc_id[take])
            )
          } else {
            setNames(character(0), character(0))
          }
          hh_bin <- modal_bin[as.character(svy_baseline[["loc_id"]])]
          orig_levels <- levels(svy_baseline[[wv]])
          return(factor(hh_bin, levels = orig_levels))
        }
        return(svy_baseline[[wv]])
      }

      if (!is.numeric(vals)) {
        return(rep(0, n))
      }

      if (has_loc) {
        loc_id <- weather_raw[["loc_id"]]
        ok <- !is.na(loc_id)
        g_loc <- if (!is.null(location_group)) {
          location_group
        } else {
          collapse::GRP(data.frame(loc_id = loc_id[ok]), by = "loc_id")
        }
        loc_means <- collapse::fmean(vals[ok], g = g_loc, na.rm = TRUE)
        loc_means <- setNames(
          as.numeric(loc_means),
          as.character(g_loc$groups$loc_id)
        )
        hh_vals <- loc_means[as.character(svy_baseline[["loc_id"]])]
        grand_mean <- mean(vals, na.rm = TRUE)
        hh_vals[is.na(hh_vals)] <- grand_mean
        return(as.numeric(hh_vals))
      }
      return(rep(mean(vals, na.rm = TRUE), n))
    } else if (wv %in% names(svy_baseline)) {
      vals <- svy_baseline[[wv]]
      if (is.factor(vals) || is.character(vals)) {
        return(vals)
      }
      if (!is.numeric(vals)) {
        return(rep(0, n))
      }
      return(vals)
    }
    rep(0, n)
  })
  names(hazard_values) <- weather_vars
  hazard_values
}

.decomposition_context_signature <- function(svy_baseline, svy_policy,
                                             model_fit, so, run_identity = NULL,
                                             skip_coef = FALSE) {
  model_signature <- tryCatch(.sig_plain(model_fit), error = function(e) {
    list(
      engine = model_fit$engine,
      weather_terms = model_fit$weather_terms,
      taus = model_fit$taus,
      rif_grid = tryCatch(.sig_plain(model_fit$rif_grid), error = function(e) NULL),
      train_shape = if (is.null(model_fit$train_data)) NULL else dim(model_fit$train_data)
    )
  })
  digest::digest(list(
    run_identity = run_identity, skip_coef = isTRUE(skip_coef),
    baseline = list(
      dim = dim(svy_baseline), names = names(svy_baseline),
      values = .sig_plain(svy_baseline)
    ),
    policy = list(
      dim = dim(svy_policy), names = names(svy_policy),
      values = .sig_plain(svy_policy)
    ),
    model = model_signature, outcome = .sig_plain(so)
  ), algo = "xxhash64")
}

.decomposition_context_owner <- new.env(parent = emptyenv())

.validate_decomposition_context <- function(context, svy_baseline, svy_policy,
                                            model_fit, so, run_identity = NULL,
                                            skip_coef = context$skip_coef) {
  if (is.null(context) || !is.environment(context) ||
    !isTRUE(context$context_version == 2L)) {
    return(FALSE)
  }
  identity_matches <- identical(context$run_identity, run_identity)
  if (!identity_matches) return(FALSE)
  identical(
    context$signature,
    .decomposition_context_signature(
      svy_baseline, svy_policy, model_fit, so, run_identity,
      skip_coef
    )
  )
}

.decomposition_context_counter_add <- function(context, name, amount = 1L) {
  counters <- if (!is.null(context) && is.environment(context)) {
    context$reuse_counters
  } else {
    NULL
  }
  if (is.environment(counters)) {
    counters[[name]] <- as.integer(counters[[name]] %||% 0L) + as.integer(amount)
  }
  invisible(NULL)
}

.decomposition_context_counter <- function(context, name, default = 0L) {
  counters <- if (!is.null(context) && is.environment(context)) {
    context$reuse_counters
  } else {
    NULL
  }
  if (is.environment(counters)) {
    return(as.integer(counters[[name]] %||% default))
  }
  default
}

.decomposition_context_baseline_deciles <- function(context) {
  if (is.null(context) || !is.environment(context)) {
    return(NULL)
  }
  value <- context$baseline_deciles
  if (!is.null(value)) {
    .decomposition_context_counter_add(context, "fixed_decile_reuses")
  }
  value
}

.decomposition_context_adverse_basis <- function(context, basis) {
  if (is.null(context) || !is.environment(context)) {
    return(NULL)
  }
  value <- context$adverse_bases[[basis]]
  if (!is.null(value)) {
    .decomposition_context_counter_add(context, "adverse_basis_reuses")
  }
  value
}

.decomposition_context_adverse_result <- function(context, basis) {
  if (is.null(context) || !is.environment(context)) {
    return(NULL)
  }
  value <- context$adverse_decompositions[[basis]]
  if (!is.null(value)) {
    .decomposition_context_counter_add(context, "adverse_result_cache_hits")
  }
  value
}

.decomposition_context_hazard_values <- function(context, svy_baseline,
                                                 weather_raw, weather_vars,
                                                 resolved = FALSE, key = NULL) {
  if (!isTRUE(resolved)) weather_raw <- step2_resolve_weather(weather_raw)
  key <- key %||% .weather_context_key(weather_raw, resolved = TRUE)
  if (!is.null(context) && is.environment(context)) {
    cached <- if (is.null(key)) NULL else context$hazard_products[[key]]
    if (!is.null(cached)) {
      .decomposition_context_counter_add(context, "hazard_cache_hits")
      return(cached)
    }
    .decomposition_context_counter_add(context, "hazard_cache_misses")
  }
  .compute_hazard_values(svy_baseline, weather_raw, weather_vars)
}

.weather_context_key <- function(weather_raw, resolved = FALSE) {
  if (is.null(weather_raw)) {
    return("<baseline>")
  }
  if (!isTRUE(resolved)) weather_raw <- step2_resolve_weather(weather_raw)
  digest::digest(.sig_plain(weather_raw), algo = "xxhash64")
}

.resolve_interaction_terms <- function(coef_names, weather_vars, deltas) {
  setNames(lapply(weather_vars, function(wv) {
    setNames(lapply(names(deltas), function(v) {
      pattern <- paste0("^", wv, "[^:]*:", v, "|^", v, "[^:]*:", wv)
      unique(c(
        intersect(c(paste0(wv, ":", v), paste0(v, ":", wv)), coef_names),
        grep(pattern, coef_names, value = TRUE)
      ))
    }), names(deltas))
  }), weather_vars)
}


#' Build the immutable state shared by all panels in one policy run
#'
#' Weather hazards are deliberately not part of this object: a historical
#' mean, a future member, and each simulated year are different bases.  Every
#' other decomposition input is fixed after the policy survey is published.
#' Keeping this as a plain list also makes the run boundary explicit and
#' prevents a reactive cache from being mutated by a later failed run.
#' @keywords internal
.build_decomposition_context <- function(svy_baseline, svy_policy, model_fit, so,
                                         deltas = NULL, skip_coef = FALSE,
                                         F_hat = NULL, baseline_deciles = NULL,
                                         run_identity = NULL,
                                         weather_panels = list(),
                                         adverse_bases = list(),
                                         adverse_decompositions = list()) {
  if (is.null(svy_baseline) || is.null(svy_policy) || is.null(model_fit) ||
    is.null(so)) {
    return(NULL)
  }
  engine <- model_fit$engine
  if (!engine %in% c("rif", "fixest")) {
    return(NULL)
  }
  weather_vars <- model_fit$weather_terms %||% character(0)
  if (!length(weather_vars) || !so$name %in% names(svy_baseline)) {
    return(NULL)
  }
  weather_panels <- Filter(Negate(is.null), lapply(
    weather_panels, step2_resolve_weather
  ))
  weather_keys <- if (length(weather_panels)) {
    vapply(weather_panels, function(panel) {
      .weather_context_key(panel, resolved = TRUE)
    }, character(1L))
  } else {
    character(0)
  }
  unique_weather <- !duplicated(weather_keys)
  weather_panels <- weather_panels[unique_weather]
  weather_keys <- weather_keys[unique_weather]

  outcome <- so$name
  is_log <- isTRUE(so$transform == "log")
  n <- nrow(svy_baseline)
  deltas <- deltas %||% .compute_policy_deltas(
    svy_baseline, svy_policy, outcome, weather_vars
  )
  sp_transfer <- if (SP_TRANSFER_COL %in% names(svy_policy)) {
    svy_policy[[SP_TRANSFER_COL]]
  } else {
    rep(0, n)
  }
  y_raw <- svy_baseline[[outcome]]
  y_baseline <- if (is_log) log(pmax(y_raw, 1e-10)) else y_raw

  reuse_counters <- new.env(parent = emptyenv())
  for (counter in c(
    "hazard_cache_hits", "hazard_cache_misses", "adverse_basis_reuses",
    "adverse_result_cache_hits", "fixed_decile_reuses", "rif_invariant_reuses",
    "term_map_reuses", "delta_reuses"
  )) {
    reuse_counters[[counter]] <- 0L
  }
  cache_entries <- new.env(parent = emptyenv())

  # Context-owned snapshots are never shared with callers that may still mutate
  # their original frames by reference (for example, with data.table).
  snapshot <- function(x) unserialize(serialize(x, NULL, version = 3L))
  weight_cols <- grep("^weight$|^hhweight$|^wgt$|^pw$",
    names(svy_baseline), value = TRUE, ignore.case = TRUE
  )
  baseline_cols <- unique(c(
    outcome, "loc_id", weather_vars, weight_cols
  ))
  baseline_snapshot <- snapshot(as.data.frame(svy_baseline)[
    intersect(baseline_cols, names(svy_baseline))
  ])
  model_snapshot <- if (engine == "rif") {
    list(
      engine = engine,
      weather_terms = snapshot(weather_vars),
      taus = snapshot(model_fit$taus),
      rif_grid = snapshot(model_fit$rif_grid),
      train_data = if (!is.null(model_fit$train_data) &&
        outcome %in% names(model_fit$train_data)) {
        snapshot(as.data.frame(model_fit$train_data)[outcome])
      } else NULL
    )
  } else {
    list(engine = engine, weather_terms = snapshot(weather_vars), fit3 = TRUE)
  }
  ctx <- list(
    context_version = 2L,
    context_owner = .decomposition_context_owner,
    context_owned = TRUE,
    svy_baseline = baseline_snapshot,
    model_fit = model_snapshot,
    so = snapshot(so[c("name", "transform")]),
    run_identity = run_identity,
    signature = .decomposition_context_signature(
      svy_baseline, svy_policy, model_fit, so, run_identity,
      skip_coef = skip_coef
    ),
    engine = engine, outcome = outcome, is_log = is_log, n = n,
    weather_vars = snapshot(weather_vars), deltas = snapshot(deltas),
    sp_transfer = snapshot(sp_transfer),
    y_baseline = snapshot(y_baseline),
    skip_coef = isTRUE(skip_coef), F_hat = F_hat,
    baseline_deciles = snapshot(baseline_deciles %||% if (exists("weighted_baseline_deciles",
      mode = "function"
    )) {
      weight_col <- if (exists("baseline_weight_column", mode = "function")) {
        baseline_weight_column(baseline_snapshot)
      } else {
        NULL
      }
      weighted_baseline_deciles(baseline_snapshot, outcome, weight_col)
    } else {
      NULL
    }),
    hazard_products = setNames(lapply(weather_panels, function(panel) {
      .compute_hazard_values(baseline_snapshot, panel, weather_vars)
    }), weather_keys),
    adverse_bases = snapshot(adverse_bases),
    adverse_decompositions = adverse_decompositions,
    reuse_counters = reuse_counters,
    cache_entries = cache_entries
  )
  ctx$cache_entries$hazard_prepared <- length(weather_panels)
  ctx$cache_entries$hazard_cache_entries <- length(weather_keys)
  ctx$cache_entries$adverse_basis_entries <- length(adverse_bases)
  ctx$cache_entries$fixed_decile_entries <- 1L
  ctx$cache_entries$rif_invariant_entries <- 0L
  if (engine == "rif") {
    ctx$rif_grid <- model_snapshot$rif_grid
    ctx$taus <- model_snapshot$taus
    ctx$train_data <- model_snapshot$train_data
    if (is.null(ctx$F_hat) && !is.null(ctx$train_data) &&
      outcome %in% names(ctx$train_data)) {
      ctx$F_hat <- stats::ecdf(ctx$train_data[[outcome]])
    }
    ctx$grid3 <- if (is.null(ctx$rif_grid)) {
      NULL
    } else {
      ctx$rif_grid[ctx$rif_grid$model == 3L, , drop = FALSE]
    }
    ctx$all_terms <- unique(ctx$grid3$term)
    ctx$rif_curve_index <- .build_rif_curve_index(ctx$grid3)
    ctx$rif_term_map <- setNames(lapply(names(deltas), function(v) {
      if (v %in% ctx$all_terms) {
        v
      } else {
        hit <- grep(paste0("^", v), ctx$all_terms, value = TRUE)
        if (length(hit)) hit[[1L]] else NULL
      }
    }), names(deltas))
    ctx$rif_interaction_map <- .resolve_interaction_terms(
      ctx$all_terms, weather_vars, deltas
    )
    beta_at <- function(term, tau) {
      curve <- ctx$rif_curve_index[[term]]
      if (is.null(curve)) {
        return(rep(0, length(tau)))
      }
      if (length(curve$tau) == 1L) {
        return(rep(curve$estimate[[1L]], length(tau)))
      }
      stats::approx(curve$tau, curve$estimate, xout = tau, rule = 2)$y
    }
    ctx$beta_at <- beta_at
    ctx$tau_i_pre <- pmin(pmax(ctx$F_hat(ctx$y_baseline), min(ctx$taus)), max(ctx$taus))
    delta_sp <- if (is_log) {
      log(pmax(exp(ctx$y_baseline) + sp_transfer, 1e-10)) -
        ctx$y_baseline
    } else {
      sp_transfer
    }
    delta_cov <- rep(0, n)
    for (v in names(deltas)) {
      term <- ctx$rif_term_map[[v]]
      if (!is.null(term)) delta_cov <- delta_cov + beta_at(term, ctx$tau_i_pre) * deltas[[v]]
    }
    ctx$delta_main <- delta_sp + delta_cov
    ctx$tau_i_post <- pmin(pmax(ctx$F_hat(ctx$y_baseline + ctx$delta_main), min(ctx$taus)), max(ctx$taus))
    ctx$cache_entries$rif_invariant_entries <- 1L
  } else {
    fit <- model_fit$fit3
    ctx$coefs <- tryCatch(stats::coef(fit), error = function(e) NULL)
    ctx$se_vec <- if (is.null(fit)) {
      setNames(rep(0, length(ctx$coefs %||% numeric(0))), names(ctx$coefs))
    } else {
      vc <- tryCatch(stats::vcov(fit), error = function(e) NULL)
      if (is.null(vc)) {
        setNames(rep(0, length(ctx$coefs)), names(ctx$coefs))
      } else {
        d <- diag(vc)
        d[d < 0 | is.na(d)] <- 0
        setNames(sqrt(d), names(ctx$coefs))
      }
    }
    ctx$coef_names <- names(ctx$coefs)
    ctx$ols_term_map <- setNames(lapply(names(deltas), function(v) {
      hit <- c(v, grep(paste0("^", v), ctx$coef_names, value = TRUE))
      hit <- hit[hit %in% ctx$coef_names]
      if (length(hit)) hit[[1L]] else NULL
    }), names(deltas))
    ctx$ols_interaction_map <- .resolve_interaction_terms(
      ctx$coef_names, weather_vars, deltas
    )
    delta_sp <- if (is_log) {
      log(pmax(exp(y_baseline) + sp_transfer, 1e-10)) -
        y_baseline
    } else {
      sp_transfer
    }
    delta_cov <- rep(0, n)
    for (v in names(deltas)) {
      term <- ctx$ols_term_map[[v]]
      if (!is.null(term)) delta_cov <- delta_cov + ctx$coefs[[term]] * deltas[[v]]
    }
    ctx$delta_main <- delta_sp + delta_cov
  }
  # A context belongs to one successful policy run.  Locking its bindings
  # catches accidental cross-run reuse during development and keeps the
  # cached representation observationally immutable to all consumers.
  ctx <- list2env(ctx, parent = emptyenv())
  lockEnvironment(ctx, bindings = TRUE)
  ctx
}

.validate_run_decomposition_context <- function(context, run_identity) {
  if (is.null(context) || !is.environment(context) ||
    !identical(context$context_version, 2L) ||
    !identical(context$context_owner, .decomposition_context_owner) ||
    !environmentIsLocked(context)) {
    stop("Invalid run-owned decomposition context.", call. = FALSE)
  }
  if (is.null(run_identity)) {
    stop("Current run identity is required.", call. = FALSE)
  }
  if (!identical(context$run_identity, run_identity)) {
    stop("Decomposition context run identity mismatch.", call. = FALSE)
  }
  invisible(context)
}

.decompose_policy_effect_run <- function(context, run_identity, weather_raw = NULL,
                                         central_only = FALSE) {
  .validate_run_decomposition_context(context, run_identity)
  svy_baseline <- context$svy_baseline
  model_fit <- context$model_fit
  so <- context$so
  n <- context$n
  weather_vars <- context$weather_vars
  if (!length(weather_vars)) return(NULL)

  weather_raw <- step2_resolve_weather(weather_raw)
  hazard_key <- .weather_context_key(weather_raw, resolved = TRUE)
  hazard_values <- .decomposition_context_hazard_values(
    context, svy_baseline, weather_raw, weather_vars,
    resolved = TRUE, key = hazard_key
  )
  if (isTRUE(central_only)) {
    if (identical(context$engine, "rif")) {
      channels <- .compute_rif_channels(
        svy_baseline = svy_baseline,
        deltas = context$deltas,
        sp_transfer = context$sp_transfer,
        hazard_values = hazard_values,
        weather_vars = weather_vars,
        rif_grid = model_fit$rif_grid,
        taus = model_fit$taus,
        train_data = model_fit$train_data,
        outcome = context$outcome,
        is_log = context$is_log,
        skip_coef = TRUE,
        central_only = TRUE,
        F_hat = context$F_hat,
        context = context
      )
      if (is.null(channels)) return(NULL)
      if (!channels$has_interactions && length(context$deltas)) {
        warning(
          "[decompose_policy_effect] No weather\u00d7policy interaction terms found ",
          "in the model. The interaction channel (res2) will be zero. ",
          "Consider including interaction terms in Step 1 model specification.",
          call. = FALSE
        )
      }
    } else {
      channels <- .decompose_ols(
        svy_baseline, model_fit, so, context$deltas, context$sp_transfer,
        hazard_values, weather_vars, n, central_only = TRUE, context = context
      )
      if (is.null(channels)) return(NULL)
    }
    return(channels)
  }

  if (identical(context$engine, "rif")) {
    .decompose_rif(
      svy_baseline, model_fit, so, context$deltas, context$sp_transfer,
      hazard_values, weather_vars, n, skip_coef = context$skip_coef,
      F_hat = context$F_hat, context = context
    )
  } else if (identical(context$engine, "fixest")) {
    .decompose_ols(
      svy_baseline, model_fit, so, context$deltas, context$sp_transfer,
      hazard_values, weather_vars, n, skip_coef = context$skip_coef,
      context = context
    )
  } else {
    NULL
  }
}

.compact_run_future_decomposition <- function(context, run_identity, weather_raw,
                                              scenario, sim_year,
                                              year_start = NA_integer_,
                                              year_end = NA_integer_) {
  channels <- .decompose_policy_effect_run(
    context, run_identity, weather_raw, central_only = TRUE
  )
  if (is.null(channels)) return(NULL)
  n <- context$n
  svy_baseline <- context$svy_baseline
  weight_col <- grep("^weight$|^hhweight$|^wgt$|^pw$",
    names(svy_baseline), value = TRUE, ignore.case = TRUE
  )[1L]
  compact <- data.frame(
    id = seq_len(n),
    weight = if (!is.na(weight_col)) svy_baseline[[weight_col]] else rep(1, n),
    delta_sp = channels$delta_sp,
    delta_main_covar = channels$delta_main_covar,
    delta_main = channels$delta_main,
    delta_res1 = channels$delta_res1,
    delta_res2 = channels$delta_res2,
    delta_total = channels$delta_total
  )
  .compact_future_decomposition(
    compact, scenario, sim_year, year_start, year_end,
    baseline_deciles = context$baseline_deciles,
    is_rif = identical(context$engine, "rif"), engine = context$engine
  )
}

.policy_central_delta_run <- function(context, run_identity, weather_raw = NULL) {
  channels <- .decompose_policy_effect_run(
    context, run_identity, weather_raw, central_only = TRUE
  )
  if (is.null(channels)) NULL else channels$delta_total
}

.finalize_decomposition_context <- function(context, adverse_decompositions = list()) {
  if (is.null(context) || !is.environment(context)) {
    return(NULL)
  }
  values <- as.list(context, all.names = TRUE)
  values$adverse_decompositions <- adverse_decompositions
  values$cache_entries$adverse_result_entries <- length(adverse_decompositions)
  out <- list2env(values, parent = emptyenv())
  lockEnvironment(out, bindings = TRUE)
  out
}


#' Compute the net policy effect on welfare level for RIF, in model scale
#'
#' Single source of truth for the RIF policy adjustment used by the
#' simulation pipeline. Sums all decomposition channels:
#'   delta_total = delta_main (SP + covariate level) + delta_res1
#'                 (repositioning) + delta_res2 (weather x policy interaction).
#'
#' `run_sim_pipeline()` calls this in policy mode and adds the returned
#' vector to the baseline-x RIF prediction so that the Results pane's
#' policy y_point reflects the same net effect the Decomposition pane
#' reports.
#'
#' @param svy_baseline Baseline survey data frame.
#' @param svy_policy   Policy-adjusted survey data frame.
#' @param weather_raw  Period weather data frame (passed to hazard helper).
#' @param weather_cols Character vector of weather variable names from the
#'   fitted RIF model (`model_fit$weather_terms`).
#' @param rif_grid     RIF coefficient grid (`model_fit$rif_grid`).
#' @param taus         Numeric vector of quantile grid points
#'   (`model_fit$taus`).
#' @param train_data   Training data used for the ecdf
#'   (`model_fit$train_data`).
#' @param outcome      Character outcome variable name.
#' @param is_log       Logical; whether the outcome is log-transformed.
#' @param deltas       Optional pre-computed covariate deltas from
#'   `.compute_policy_deltas()`. The deltas are weather- and period-
#'   independent, so per-year/per-scenario callers can compute them once and
#'   pass them in (PERF-22). When NULL they are computed here.
#' @param F_hat        Optional pre-built ecdf of the training outcome (see
#'   `.compute_rif_channels()`). When NULL it is computed here.
#' @return Numeric vector of length `nrow(svy_baseline)` giving delta_total
#'   in model scale, or NULL if any required input is missing.
#' @keywords internal
.compute_rif_policy_correction <- function(svy_baseline, svy_policy,
                                           weather_raw, weather_cols,
                                           rif_grid, taus, train_data,
                                           outcome, is_log,
                                           deltas = NULL, F_hat = NULL) {
  if (is.null(svy_baseline) || is.null(svy_policy) ||
    is.null(rif_grid) || is.null(taus) || is.null(train_data) ||
    length(weather_cols) == 0L) {
    return(NULL)
  }

  n <- nrow(svy_baseline)
  deltas <- deltas %||% .compute_policy_deltas(
    svy_baseline, svy_policy,
    outcome, weather_cols
  )
  sp_transfer <- if (SP_TRANSFER_COL %in% names(svy_policy)) {
    svy_policy[[SP_TRANSFER_COL]]
  } else {
    rep(0, n)
  }
  hazard_values <- .compute_hazard_values(
    svy_baseline, weather_raw,
    weather_cols
  )

  channels <- .compute_rif_channels(
    svy_baseline  = svy_baseline,
    deltas        = deltas,
    sp_transfer   = sp_transfer,
    hazard_values = hazard_values,
    weather_vars  = weather_cols,
    rif_grid      = rif_grid,
    taus          = taus,
    train_data    = train_data,
    outcome       = outcome,
    is_log        = is_log,
    # SEs are not propagated into y_point; the sim pipeline expresses
    # uncertainty through coefficient draws / F_loading, not channel SDs.
    skip_coef     = TRUE,
    F_hat         = F_hat
  )
  if (is.null(channels)) {
    return(NULL)
  }
  channels$delta_total
}


#' Compute the central-value policy correction without decomposition summaries
#'
#' This is the active analytic simulation kernel. It deliberately omits
#' coefficient-uncertainty work and data-frame assembly while sharing the same
#' channel helpers as `decompose_policy_effect()`, so `delta_total` remains
#' exactly aligned with the full decomposition path.
#'
#' @return Numeric vector in baseline survey row order, or NULL when the model
#'   cannot produce an analytic correction.
#' @keywords internal
.policy_central_delta <- function(svy_baseline, svy_policy, model_fit, so,
                                  weather_raw = NULL, deltas = NULL,
                                  F_hat = NULL, context = NULL,
                                  run_identity = NULL) {
  if (is.null(svy_baseline) || is.null(svy_policy) || is.null(model_fit) ||
    is.null(so)) {
    return(NULL)
  }

  engine <- model_fit$engine
  if (!engine %in% c("rif", "fixest")) {
    return(NULL)
  }

  weather_vars <- model_fit$weather_terms
  if (is.null(weather_vars) || length(weather_vars) == 0L) {
    return(NULL)
  }

  outcome <- so$name
  is_log <- isTRUE(so$transform == "log")

  # Match the public decomposition path for Step 2 snapshots that omit a
  # derivable outcome such as `poor`.
  svy_baseline <- ensure_outcome_column(svy_baseline, so)
  svy_policy <- ensure_outcome_column(svy_policy, so)
  if (!outcome %in% names(svy_baseline) || !outcome %in% names(svy_policy)) {
    warning(
      "[decompose_policy_effect] Outcome column `", outcome,
      "` is unavailable in the baseline or policy survey.",
      call. = FALSE
    )
    return(NULL)
  }
  context_reused <- !is.null(context)
  if (!is.null(context)) {
    if (is.null(run_identity)) {
      stop("Current run identity is required when reusing a decomposition context.",
        call. = FALSE
      )
    }
    if (!identical(context$run_identity, run_identity)) {
      stop("Decomposition context run identity mismatch.", call. = FALSE)
    }
    if (!.validate_decomposition_context(
      context, svy_baseline, svy_policy, model_fit, so, run_identity,
      context$skip_coef
    )) {
      stop("Incompatible or stale decomposition context.", call. = FALSE)
    }
    .decomposition_context_counter_add(context, "delta_reuses")
    if (identical(engine, "rif")) {
      .decomposition_context_counter_add(context, "rif_invariant_reuses")
    }
  }

  n <- nrow(svy_baseline)
  deltas <- deltas %||% .compute_policy_deltas(
    svy_baseline, svy_policy, outcome, weather_vars
  )

  sp_transfer <- if (SP_TRANSFER_COL %in% names(svy_policy)) {
    svy_policy[[SP_TRANSFER_COL]]
  } else {
    rep(0, n)
  }
  hazard_values <- .decomposition_context_hazard_values(
    context, svy_baseline, weather_raw, weather_vars
  )

  if (identical(engine, "rif")) {
    channels <- .compute_rif_channels(
      svy_baseline = svy_baseline,
      deltas = deltas,
      sp_transfer = sp_transfer,
      hazard_values = hazard_values,
      weather_vars = weather_vars,
      rif_grid = model_fit$rif_grid,
      taus = model_fit$taus,
      train_data = model_fit$train_data,
      outcome = outcome,
      is_log = is_log,
      skip_coef = TRUE,
      central_only = TRUE,
      F_hat = F_hat,
      context = context
    )
    if (is.null(channels)) {
      return(NULL)
    }
    if (!channels$has_interactions && length(deltas) > 0L) {
      warning(
        "[decompose_policy_effect] No weather\u00d7policy interaction terms found ",
        "in the model. The interaction channel (res2) will be zero. ",
        "Consider including interaction terms in Step 1 model specification.",
        call. = FALSE
      )
    }
    return(channels$delta_total)
  }

  central <- .decompose_ols(
    svy_baseline, model_fit, so, deltas, sp_transfer,
    hazard_values, weather_vars, n,
    central_only = TRUE, context = context
  )
  if (is.null(central)) {
    return(NULL)
  }
  central$delta_total
}


# Public API ----

#' Decompose policy effects into main and resilience channels
#'
#' Harmonized function that works for both RIF and fixest/OLS engines.
#' For RIF: full decomposition (main + repositioning + interaction).
#' For fixest: simplified (main + interaction only, no repositioning).
#'
#' @param svy_baseline Data frame of baseline survey-weather covariates.
#' @param svy_policy Data frame of policy-adjusted survey-weather covariates.
#' @param model_fit Model fit list from Step 1 (contains $fit3, $engine,
#'   $taus, $weather_terms, $rif_grid, $train_data).
#' @param so Selected outcome metadata (list with $name, $transform).
#' @param weather_raw Data frame of weather values for the simulation period.
#'   Used to extract realised hazard values. If NULL, uses baseline survey
#'   weather columns as hazard.
#' @param skip_coef Logical. When TRUE, per-channel analytic standard errors
#'   are forced to 0 (use this when the user has turned coefficient
#'   uncertainty off in Step 2). Default FALSE.
#' @param deltas Optional pre-computed covariate deltas from
#'   `.compute_policy_deltas()`. The deltas depend only on the baseline and
#'   policy surveys - never on the weather panel - so per-year/per-scenario
#'   callers can compute them once and pass them in (PERF-22). When NULL
#'   they are computed here (backward compatible).
#' @param F_hat Optional pre-built empirical CDF of the training outcome
#'   (RIF path only; see `.compute_rif_channels()`). When NULL it is
#'   computed here.
#'
#' @return A data frame with one row per household and decomposition columns,
#'   or NULL if decomposition is not possible.
#' @export
decompose_policy_effect <- function(svy_baseline,
                                    svy_policy,
                                    model_fit,
                                    so,
                                    weather_raw = NULL,
                                    skip_coef = FALSE,
                                    deltas = NULL,
                                    F_hat = NULL,
                                    context = NULL,
                                    hazard_values = NULL,
                                    run_identity = NULL) {
  if (is.null(svy_baseline) || is.null(svy_policy) || is.null(model_fit)) {
    return(NULL)
  }
  outcome <- so$name

  # Step 2 snapshots may omit the synthetic `poor` outcome even though the
  # fitted model and selected outcome metadata refer to it.
  svy_baseline <- ensure_outcome_column(svy_baseline, so)
  svy_policy <- ensure_outcome_column(svy_policy, so)
  if (!outcome %in% names(svy_baseline) || !outcome %in% names(svy_policy)) {
    warning(
      "[decompose_policy_effect] Outcome column `", outcome,
      "` is unavailable in the baseline or policy survey.",
      call. = FALSE
    )
    return(NULL)
  }

  context_reused <- !is.null(context)
  if (!is.null(context)) {
    if (is.null(run_identity)) {
      stop("Current run identity is required when reusing a decomposition context.",
        call. = FALSE
      )
    }
    if (!identical(context$run_identity, run_identity)) {
      stop("Decomposition context run identity mismatch.", call. = FALSE)
    }
    if (!.validate_decomposition_context(
      context, svy_baseline, svy_policy, model_fit, so, run_identity, skip_coef
    )) {
      stop("Incompatible or stale decomposition context.", call. = FALSE)
    }
  }
  context <- context %||% .build_decomposition_context(
    svy_baseline, svy_policy, model_fit, so, deltas, skip_coef, F_hat,
    run_identity = run_identity
  )
  if (is.null(context)) {
    return(NULL)
  }
  engine <- context$engine
  if (context_reused) {
    .decomposition_context_counter_add(context, "delta_reuses")
  }
  if (context_reused && identical(engine, "rif")) {
    .decomposition_context_counter_add(context, "rif_invariant_reuses")
  }
  if (!engine %in% c("rif", "fixest")) {
    return(NULL)
  }

  n <- context$n
  outcome <- context$outcome
  is_log <- context$is_log

  # Identify weather hazard variable(s) and their realised values
  weather_vars <- context$weather_vars
  if (is.null(weather_vars) || length(weather_vars) == 0) {
    return(NULL)
  }

  hazard_values <- hazard_values %||% .decomposition_context_hazard_values(
    context, svy_baseline, weather_raw, weather_vars
  )

  # Compute covariate deltas using shared helper
  # PERF-22: accept a caller-supplied, weather-independent delta list.
  deltas <- context$deltas

  # SP transfer component
  sp_transfer <- context$sp_transfer

  if (engine == "rif") {
    .decompose_rif(svy_baseline, model_fit, so, deltas, sp_transfer,
      hazard_values, weather_vars, n,
      skip_coef = skip_coef,
      F_hat = context$F_hat, context = context
    )
  } else {
    .decompose_ols(svy_baseline, model_fit, so, deltas, sp_transfer,
      hazard_values, weather_vars, n,
      skip_coef = skip_coef,
      context = context
    )
  }
}


# RIF decomposition (delegates to .compute_rif_channels) ----

.decompose_rif <- function(svy_baseline, model_fit, so, deltas, sp_transfer,
                           hazard_values, weather_vars, n,
                           skip_coef = FALSE, F_hat = NULL, context = NULL) {
  outcome <- so$name
  is_log <- isTRUE(so$transform == "log")
  rif_grid <- model_fit$rif_grid
  taus <- model_fit$taus
  train_data <- model_fit$train_data

  if (is.null(rif_grid) || is.null(taus)) {
    return(NULL)
  }

  # Delegate to single source of truth
  channels <- .compute_rif_channels(
    svy_baseline  = svy_baseline,
    deltas        = deltas,
    sp_transfer   = sp_transfer,
    hazard_values = hazard_values,
    weather_vars  = weather_vars,
    rif_grid      = rif_grid,
    taus          = taus,
    train_data    = train_data,
    outcome       = outcome,
    is_log        = is_log,
    skip_coef     = skip_coef,
    F_hat         = F_hat,
    context       = context
  )
  if (is.null(channels)) {
    return(NULL)
  }

  if (!channels$has_interactions && length(deltas) > 0) {
    warning(
      "[decompose_policy_effect] No weather\u00d7policy interaction terms found ",
      "in the model. The interaction channel (res2) will be zero. ",
      "Consider including interaction terms in Step 1 model specification.",
      call. = FALSE
    )
  }

  # Assemble output data frame
  weight_col <- grep("^weight$|^hhweight$|^wgt$|^pw$",
    names(svy_baseline),
    value = TRUE, ignore.case = TRUE
  )[1]

  data.frame(
    id               = seq_len(n),
    weight           = if (!is.na(weight_col)) svy_baseline[[weight_col]] else rep(1, n),
    tau_i_pre        = channels$tau_i_pre,
    tau_i_post       = channels$tau_i_post,
    decile           = pmin(pmax(ceiling(channels$tau_i_pre * 10), 1L), 10L),
    sp_eligible      = sp_transfer > 0,
    delta_main       = channels$delta_main,
    delta_sp         = channels$delta_sp,
    delta_main_covar = channels$delta_main_covar,
    delta_res1       = channels$delta_res1,
    delta_res2       = channels$delta_res2,
    delta_res        = channels$delta_res1 + channels$delta_res2,
    delta_total      = channels$delta_total,
    sd_main          = channels$sd_main,
    sd_res1          = channels$sd_res1,
    sd_res2          = channels$sd_res2,
    sd_total         = channels$sd_total,
    pct_main         = (exp(channels$delta_main) - 1) * 100,
    pct_res1         = (exp(channels$delta_res1) - 1) * 100,
    pct_res2         = (exp(channels$delta_res2) - 1) * 100,
    pct_total        = (exp(channels$delta_total) - 1) * 100,
    stringsAsFactors = FALSE
  )
}


# OLS/fixest decomposition (simplified: main + interaction only) ----

.decompose_ols <- function(svy_baseline, model_fit, so, deltas, sp_transfer,
                           hazard_values, weather_vars, n,
                           skip_coef = FALSE, central_only = FALSE,
                           context = NULL) {
  outcome <- so$name
  is_log <- isTRUE(so$transform == "log")
  fit <- model_fit$fit3

  if (is.null(fit)) {
    return(NULL)
  }

  coefs <- context$coefs %||% tryCatch(stats::coef(fit), error = function(e) NULL)
  if (is.null(coefs)) {
    return(NULL)
  }

  # Per-coefficient SE under diagonal-Sigma approximation. Central-only calls
  # bypass vcov extraction entirely; a run context reuses the prepared vector.
  se_vec <- if (isTRUE(central_only) || isTRUE(skip_coef)) {
    setNames(rep(0, length(coefs)), names(coefs))
  } else {
    context$se_vec %||% {
      vc <- tryCatch(stats::vcov(fit), error = function(e) NULL)
      if (is.null(vc)) {
        setNames(rep(0, length(coefs)), names(coefs))
      } else {
        d <- diag(vc)
        d[d < 0 | is.na(d)] <- 0
        setNames(sqrt(d), names(coefs))
      }
    }
  }
  se_of <- if (isTRUE(central_only)) {
    NULL
  } else {
    function(term) {
      if (isTRUE(skip_coef)) {
        return(0)
      }
      s <- se_vec[term]
      if (is.na(s)) 0 else as.numeric(s)
    }
  }

  # Baseline welfare
  y_baseline <- context$y_baseline %||% {
    y_raw <- svy_baseline[[outcome]]
    if (is_log) log(pmax(y_raw, 1e-10)) else y_raw
  }

  # --- Main effect ---
  delta_sp <- if (is_log) {
    log(pmax(exp(y_baseline) + sp_transfer, 1e-10)) - y_baseline
  } else {
    sp_transfer
  }
  delta_main_covar <- rep(0, n)
  var_main <- if (isTRUE(central_only)) NULL else rep(0, n)

  coef_names <- names(coefs)
  for (v in names(deltas)) {
    .decomposition_context_counter_add(context, "term_map_reuses")
    term_used <- context$ols_term_map[[v]] %||% v
    if (!term_used %in% coef_names) {
      matches <- grep(paste0("^", v), coef_names, value = TRUE)
      if (length(matches)) term_used <- matches[[1L]]
    }
    beta_v <- coefs[term_used]
    if (!is.na(beta_v)) {
      delta_main_covar <- delta_main_covar + beta_v * deltas[[v]]
      if (!isTRUE(central_only)) {
        var_main <- var_main + (deltas[[v]] * se_of(term_used))^2
      }
    }
  }
  delta_main <- context$delta_main %||% (delta_sp + delta_main_covar)

  # --- Interaction (res2) --- OLS has no repositioning
  # Continuous weather: coef_int * haz_mean * delta_x
  # Binned weather:     coef_int_bin_k * 1 * delta_x  (only for active bin)
  delta_res2 <- rep(0, n)
  var_res2 <- if (isTRUE(central_only)) NULL else rep(0, n)
  has_interactions <- FALSE

  for (wv in weather_vars) {
    haz_int <- hazard_values[[wv]]
    is_binned <- is.factor(haz_int)

    for (v in names(deltas)) {
      if (is_binned) {
        for (lv in levels(haz_int)) {
          cand1 <- paste0(wv, lv, ":", v)
          cand2 <- paste0(v, ":", wv, lv)
          candidates <- context$ols_interaction_map[[wv]][[v]] %||% character(0)
          matched <- c(cand1, cand2)[c(cand1, cand2) %in% candidates]
          it_name <- if (length(matched)) matched[[1L]] else if (cand1 %in% coef_names) cand1 else if (cand2 %in% coef_names) cand2 else NULL
          if (is.null(it_name)) next
          has_interactions <- TRUE
          active <- !is.na(haz_int) & haz_int == lv
          if (!any(active)) next
          delta_res2[active] <- delta_res2[active] + coefs[it_name] * deltas[[v]][active]
          if (!isTRUE(central_only)) {
            var_res2[active] <- var_res2[active] +
              (deltas[[v]][active] * se_of(it_name))^2
          }
        }
      } else {
        .decomposition_context_counter_add(context, "term_map_reuses")
        candidates <- context$ols_interaction_map[[wv]][[v]] %||% character(0)
        int_term <- if (length(candidates)) candidates[[1L]] else NULL
        if (is.null(int_term)) {
          exact <- c(paste0(wv, ":", v), paste0(v, ":", wv))
          exact_matches <- exact[exact %in% coef_names]
          int_term <- if (length(exact_matches)) exact_matches[[1L]] else NULL
          if (is.null(int_term)) {
            pattern <- paste0("^", wv, "[^:]*:", v, "|^", v, "[^:]*:", wv)
            matches <- grep(pattern, coef_names, value = TRUE)
            if (length(matches)) int_term <- matches[[1L]]
          }
        }
        if (!is.null(int_term)) {
          has_interactions <- TRUE
          delta_res2 <- delta_res2 + coefs[int_term] * haz_int * deltas[[v]]
          if (!isTRUE(central_only)) {
            var_res2 <- var_res2 +
              (haz_int * deltas[[v]] * se_of(int_term))^2
          }
        }
      }
    }
  }

  if (!has_interactions && length(deltas) > 0) {
    warning(
      "[decompose_policy_effect] No weather\u00d7policy interaction terms found ",
      "in the model. The interaction channel will be zero. ",
      "Consider including interaction terms in Step 1 model specification.",
      call. = FALSE
    )
  }

  delta_total <- delta_main + delta_res2
  if (isTRUE(central_only)) {
    return(list(
      delta_sp         = delta_sp,
      delta_main_covar = delta_main_covar,
      delta_main       = delta_main,
      delta_res1       = rep(0, n),
      delta_res2       = delta_res2,
      delta_total      = delta_total,
      has_interactions = has_interactions
    ))
  }
  var_total <- var_main + var_res2 # OLS has no res1 channel

  weight_col <- grep("^weight$|^hhweight$|^wgt$|^pw$",
    names(svy_baseline),
    value = TRUE, ignore.case = TRUE
  )[1]

  data.frame(
    id               = seq_len(n),
    weight           = if (!is.na(weight_col)) svy_baseline[[weight_col]] else rep(1, n),
    tau_i_pre        = NA_real_,
    tau_i_post       = NA_real_,
    decile           = pmin(pmax(ceiling(stats::ecdf(y_baseline)(y_baseline) * 10), 1L), 10L),
    sp_eligible      = sp_transfer > 0,
    delta_main       = delta_main,
    delta_sp         = delta_sp,
    delta_main_covar = delta_main_covar,
    delta_res1       = rep(0, n),
    delta_res2       = delta_res2,
    delta_res        = delta_res2,
    delta_total      = delta_total,
    sd_main          = sqrt(var_main),
    sd_res1          = rep(0, n),
    sd_res2          = sqrt(var_res2),
    sd_total         = sqrt(var_total),
    pct_main         = (exp(delta_main) - 1) * 100,
    pct_res1         = rep(0, n),
    pct_res2         = (exp(delta_res2) - 1) * 100,
    pct_total        = (exp(delta_total) - 1) * 100,
    stringsAsFactors = FALSE
  )
}
