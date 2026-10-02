# Row-aligned central policy channels shared by production and attribution.
# Unit-exposure evaluations use the existing kernels, not a second formula map.

.policy_explicit_channels <- function(context, hazards, weather_vars = context$weather_vars) {
  if (identical(context$engine, "rif")) {
    .compute_rif_channels(
      context$svy_baseline, context$deltas, context$sp_transfer,
      hazards, weather_vars, context$rif_grid, context$taus,
      context$train_data, context$outcome, context$is_log,
      skip_coef = TRUE, central_only = TRUE, context = context
    )
  } else {
    suppressWarnings(.decompose_ols(
      context$svy_baseline, context$model_fit, context$so,
      context$deltas, context$sp_transfer, hazards, weather_vars,
      context$n, skip_coef = TRUE, central_only = TRUE, context = context
    ))
  }
}

.policy_annual_channel_status <- function(context) {
  endpoint <- .policy_endpoint_status(context$so, context)
  if (!identical(endpoint$status, "ok")) return(endpoint)
  if (is.null(context$engine) || !context$engine %in% c("rif", "fixest")) {
    return(list(status = "unsupported", reason = "Annual channels require RIF or linear fixest."))
  }
  transform <- context$so$transform
  if (!is.null(transform) && !is.na(transform) &&
    !transform %in% c("log", "none", "identity")) {
    return(list(status = "unsupported", reason = "Annual channels require identity or log outcomes."))
  }
  list(status = "ok", reason = NULL)
}

.prepare_policy_annual_channels <- function(context, run_identity) {
  .validate_run_decomposition_context(context, run_identity)
  endpoint <- .policy_annual_channel_status(context)
  if (!identical(endpoint$status, "ok")) return(endpoint)
  vars <- context$weather_vars
  n <- context$n
  zero <- setNames(lapply(vars, function(v) {
    x <- context$svy_baseline[[v]]
    if (is.factor(x)) factor(rep(levels(x)[1L], n), levels = levels(x)) else rep(0, n)
  }), vars)
  # Main/ranks are invariant to weather. The full call also records whether
  # fitted interactions are present, including categories absent in this run.
  base <- .policy_explicit_channels(context, zero)
  if (is.null(base) || any(!is.finite(base$delta_main))) {
    return(list(status = "unavailable", reason = "Canonical main channel is unavailable or nonfinite."))
  }
  products <- setNames(lapply(vars, function(v) {
    x <- context$svy_baseline[[v]]
    categories <- if (is.factor(x)) levels(x) else NULL
    values <- if (is.null(categories)) 1 else categories
    r1 <- r2 <- matrix(0, nrow = n, ncol = length(values))
    for (j in seq_along(values)) {
      hazards <- zero
      hazards[[v]] <- if (is.null(categories)) rep(1, n) else {
        factor(rep(values[j], n), levels = categories)
      }
      ch <- .policy_explicit_channels(context, hazards, weather_vars = v)
      r1[, j] <- ch$delta_res1
      r2[, j] <- ch$delta_res2
    }
    list(categories = categories, repositioning = r1, interaction = r2)
  }), vars)
  # Each category stores its own fitted-reference contrast. No assumption about
  # the first factor level or division by observed hazards is needed.
  if (any(!is.finite(c(base$delta_res1, base$delta_res2,
    unlist(lapply(products, function(p) c(p$repositioning, p$interaction))))))) {
    return(list(status = "unavailable", reason = "Canonical sensitivity channels are nonfinite."))
  }
  prepared <- list2env(list(
    status = "ok", context = context, run_identity = run_identity,
    delta_sp = base$delta_sp, delta_main_covar = base$delta_main_covar,
    delta_main = base$delta_main, products = products,
    repositioning_modeled = identical(context$engine, "rif"),
    interaction_included = base$has_interactions,
    tau_i_pre = context$tau_i_pre, tau_i_post = context$tau_i_post,
    correction_version = "row_aligned_annual_v1"
  ), parent = emptyenv())
  lockEnvironment(prepared, bindings = TRUE)
  prepared
}

.validate_policy_annual_exposure <- function(pipeline, context) {
  exposure <- pipeline$weather_exposure
  if (is.null(exposure) || !identical(exposure$status, "ok")) {
    stop("Exact prediction-row weather exposure mapping unavailable.", call. = FALSE)
  }
  n <- length(pipeline$y_point)
  if (!n) stop("Empty baseline prediction pipeline.", call. = FALSE)
  fields <- c("svy_row_id", "sim_year", "weight", "id_vec")
  for (field in fields) {
    if (!identical(pipeline[[field]], exposure[[field]])) {
      stop("Prediction/exposure ordering mismatch: ", field, call. = FALSE)
    }
  }
  ids <- pipeline$svy_row_id
  idx <- exposure$row_index
  ordinal <- exposure$prediction_row_id
  valid_index <- function(x, upper) is.numeric(x) && length(x) == n &&
    !anyNA(x) && all(x == as.integer(x) & x >= 1 & x <= upper)
  if (!valid_index(ids, context$n) || length(pipeline$sim_year) != n ||
    anyNA(pipeline$sim_year) || !is.data.frame(exposure$table) ||
    !valid_index(idx, nrow(exposure$table)) || !valid_index(ordinal, Inf) ||
    anyDuplicated(ordinal)) {
    stop("Invalid prediction-row exposure identifiers.", call. = FALSE)
  }
  required <- c("code", "year", "survname", "loc_id", "int_month", "timestamp",
    context$weather_vars)
  absent <- setdiff(required, names(exposure$table))
  missing <- if (!length(absent)) required[vapply(required,
    function(key) anyNA(exposure$table[[key]][idx]), logical(1))] else absent
  if (length(missing)) {
    stop("Missing exact weather exposure or anchor identity: ",
      paste(missing, collapse = ", "), call. = FALSE)
  }
  join_keys <- c("code", "year", "survname", "loc_id", "int_month")
  if (!all(join_keys %in% names(context$svy_baseline))) {
    stop("Survey join identity unavailable in run context.", call. = FALSE)
  }
  for (key in join_keys) {
    if (!identical(as.character(exposure$table[[key]][idx]),
      as.character(context$svy_baseline[[key]][ids]))) {
      stop("Weather exposure/survey identity mismatch: ", key, call. = FALSE)
    }
  }
  if (!is.null(pipeline$weight) && "weight" %in% names(context$svy_baseline) &&
    !identical(pipeline$weight, context$svy_baseline$weight[ids])) {
    stop("Prediction/survey weight mismatch.", call. = FALSE)
  }
  ts <- as.POSIXlt(exposure$table$timestamp[idx])
  if (!all(as.integer(ts$year + 1900L) == pipeline$sim_year) ||
    !all(as.integer(ts$mon + 1L) == exposure$table$int_month[idx])) {
    stop("Weather anchor does not match simulation year/month.", call. = FALSE)
  }
  for (v in context$weather_vars) {
    x <- exposure$table[[v]][idx]
    baseline <- context$svy_baseline[[v]]
    if (is.factor(baseline)) {
      if (!is.factor(x) || any(!as.character(x) %in% levels(baseline))) {
        stop("Unknown or invalid weather category: ", v, call. = FALSE)
      }
    } else if (!is.numeric(x) || any(!is.finite(x))) {
      stop("Nonfinite or nonnumeric weather exposure: ", v, call. = FALSE)
    }
  }
  exposure
}

.policy_annual_channels <- function(pipeline, prepared, run_identity,
                                    rows = seq_along(pipeline$y_point)) {
  if (!is.environment(prepared) || !environmentIsLocked(prepared)) {
    stop("Invalid prepared annual channel source.", call. = FALSE)
  }
  context <- prepared$context
  .validate_run_decomposition_context(context, run_identity)
  if (!identical(prepared$run_identity, run_identity)) {
    stop("Annual channel run identity mismatch.", call. = FALSE)
  }
  exposure <- .validate_policy_annual_exposure(pipeline, context)
  if (!is.numeric(rows) || anyNA(rows) || any(rows != as.integer(rows)) ||
    any(rows < 1 | rows > length(pipeline$y_point)) || anyDuplicated(rows)) {
    stop("Invalid annual channel row selection.", call. = FALSE)
  }
  .policy_annual_channel_block(pipeline, prepared, exposure, rows)
}

# Internal hot loop: callers validate the immutable source and whole mapping
# once before consuming bounded blocks. Public adapter calls remain strict.
.policy_annual_channel_block <- function(pipeline, prepared, exposure, rows) {
  ids <- pipeline$svy_row_id[rows]
  idx <- exposure$row_index[rows]
  r1 <- r2 <- numeric(length(rows))
  for (v in names(prepared$products)) {
    p <- prepared$products[[v]]
    x <- exposure$table[[v]][idx]
    if (is.null(p$categories)) {
      r1 <- r1 + p$repositioning[ids, 1L] * x
      r2 <- r2 + p$interaction[ids, 1L] * x
    } else {
      lookup <- cbind(ids, match(as.character(x), p$categories))
      r1 <- r1 + p$repositioning[lookup]
      r2 <- r2 + p$interaction[lookup]
    }
  }
  main <- prepared$delta_main[ids]
  list(status = "ok", delta_sp = prepared$delta_sp[ids],
    delta_main_covar = prepared$delta_main_covar[ids], delta_main = main,
    delta_res1 = r1, delta_res2 = r2, delta_total = main + r1 + r2,
    prediction_row_id = exposure$prediction_row_id[rows],
    repositioning_modeled = prepared$repositioning_modeled,
    interaction_included = prepared$interaction_included,
    correction_version = prepared$correction_version)
}

.apply_policy_annual_pipeline <- function(pipeline, prepared, run_identity,
                                          scenario = NULL, member = NULL,
                                          year_range = c(NA_integer_, NA_integer_),
                                          chunk_size = 100000L) {
  .validate_run_decomposition_context(prepared$context, run_identity)
  if (!identical(prepared$run_identity, run_identity)) {
    stop("Annual channel run identity mismatch.", call. = FALSE)
  }
  if (is.null(pipeline$y_point)) stop("Missing baseline prediction pipeline.", call. = FALSE)
  exposure <- .validate_policy_annual_exposure(pipeline, prepared$context)
  n <- length(pipeline$y_point)
  years <- sort(unique(pipeline$sim_year))
  channel_stats <- decile_stats <- NULL
  stat_names <- as.vector(rbind(paste0("sum_", .compact_decomp_channels),
    paste0("weight_", .compact_decomp_channels)))
  accumulate <- function(current, values, group, n_groups) {
    if (is.null(current)) current <- matrix(0, n_groups, ncol(values))
    grouped <- rowsum(values, group, reorder = FALSE)
    keys <- as.integer(rownames(grouped))
    current[keys, ] <- current[keys, , drop = FALSE] + grouped
    current
  }
  out <- pipeline
  for (start in seq.int(1L, n, by = chunk_size)) {
    rows <- seq.int(start, min(n, start + chunk_size - 1L))
    ch <- .policy_annual_channel_block(pipeline, prepared, exposure, rows)
    if (any(!is.finite(ch$delta_total))) {
      stop("Nonfinite annual policy correction.", call. = FALSE)
    }
    out$y_point[rows] <- pipeline$y_point[rows] + ch$delta_total
    if (!is.null(scenario)) {
      weights <- if (is.null(pipeline$weight)) rep(1, length(rows)) else pipeline$weight[rows]
      valid <- is.finite(weights) & weights > 0
      weights[!valid] <- 0
      values <- cbind(ch$delta_total, ch$delta_main, ch$delta_sp,
        ch$delta_main_covar, ch$delta_res1 + ch$delta_res2, ch$delta_res1, ch$delta_res2)
      stats <- matrix(0, length(rows), length(stat_names))
      stats[, seq.int(1L, length(stat_names), 2L)] <- values * weights
      stats[, seq.int(2L, length(stat_names), 2L)] <- weights
      year_id <- match(pipeline$sim_year[rows], years)
      channel_stats <- accumulate(channel_stats, stats, year_id, length(years))
      deciles <- prepared$context$baseline_deciles[pipeline$svy_row_id[rows]]
      keep <- which(is.finite(deciles) & deciles >= 1 & deciles <= 10)
      if (length(keep)) {
        decile_stats <- accumulate(decile_stats,
          cbind(stats[keep, , drop = FALSE], 1, weights[keep]),
          (year_id[keep] - 1L) * 10L + deciles[keep], length(years) * 10L)
      }
    }
  }
  make_table <- function(stats, decile = FALSE) {
    if (is.null(stats)) return(data.frame())
    if (decile) {
      keys <- which(stats[, ncol(stats) - 1L] > 0)
      stats <- stats[keys, , drop = FALSE]
      sim_year <- years[(keys - 1L) %/% 10L + 1L]
    } else sim_year <- years
    colnames(stats) <- c(stat_names, if (decile) c("n_households", "weighted_population"))
    # Empty positive-weight groups match the technical reducer's unavailable state.
    for (j in seq.int(2L, length(stat_names), 2L)) {
      zero <- stats[, j] == 0
      stats[zero, c(j - 1L, j)] <- NA_real_
    }
    tbl <- data.frame(scenario = scenario, sim_year = sim_year,
      year_start = year_range[[1L]], year_end = year_range[[2L]], stats,
      engine = prepared$context$engine, is_rif = prepared$repositioning_modeled,
      member = member, correction_version = prepared$correction_version,
      scope = "production_prediction_rows", uncertainty = "central_only")
    if (decile) {
      tbl$decile <- as.integer((keys - 1L) %% 10L + 1L)
      tbl$n_households <- as.integer(tbl$n_households)
    }
    tbl
  }
  compact <- if (!is.null(scenario)) list(
    channel = make_table(channel_stats), decile = make_table(decile_stats, TRUE),
    metadata = list(scenario = scenario, year_start = year_range[[1L]], year_end = year_range[[2L]]),
    engine = prepared$context$engine, is_rif = prepared$repositioning_modeled
  ) else NULL
  out$policy_correction <- list(version = prepared$correction_version,
    run_identity = run_identity, exposure_source = "step2_prediction_row_mapping",
    n_prediction_rows = n, n_exposure_anchors = nrow(exposure$table),
    scope = "production_prediction_rows", uncertainty = "baseline_X_gradient")
  list(pipeline = out, compact = compact)
}

# Deliberately slow reference for tests/benchmarks only: evaluate the original
# central kernels against each row's exact exposures, then select its household.
.policy_annual_channels_reference <- function(pipeline, context, run_identity) {
  .validate_run_decomposition_context(context, run_identity)
  endpoint <- .policy_annual_channel_status(context)
  if (!identical(endpoint$status, "ok")) return(endpoint)
  exposure <- .validate_policy_annual_exposure(pipeline, context)
  columns <- c("delta_sp", "delta_main_covar", "delta_main", "delta_res1", "delta_res2", "delta_total")
  out <- setNames(lapply(columns, function(x) numeric(length(pipeline$y_point))), columns)
  for (i in seq_along(pipeline$y_point)) {
    hazards <- setNames(lapply(context$weather_vars, function(v) {
      rep(exposure$table[[v]][exposure$row_index[i]], context$n)
    }), context$weather_vars)
    ch <- .policy_explicit_channels(context, hazards)
    for (v in columns) out[[v]][i] <- ch[[v]][pipeline$svy_row_id[i]]
  }
  c(list(status = "ok"), out)
}

.policy_metric_fields <- c("baseline", "after_main", "after_repositioning", "policy",
  "main", "repositioning", "interaction", "resilience", "total")
.policy_metric_tolerance <- 1e-8

.policy_metric_contributions <- function(levels) {
  levels$main <- levels$after_main - levels$baseline
  levels$repositioning <- levels$after_repositioning - levels$after_main
  levels$interaction <- levels$policy - levels$after_repositioning
  levels$resilience <- levels$repositioning + levels$interaction
  levels$total <- levels$policy - levels$baseline
  levels
}

# One summary operator for every cumulative state and contribution. Models with
# unequal year counts still receive equal weight; survey weights act upstream.
.policy_metric_summary <- function(annual, scenario) {
  models <- split(annual, annual$model_id)
  means <- lapply(models, function(x) colMeans(x[, .policy_metric_fields, drop = FALSE]))
  values <- as.list(colMeans(do.call(rbind, means)))
  data.frame(scenario = scenario, values, n_models = length(models),
    n_model_years = nrow(annual), n_years = min(vapply(models, nrow, integer(1))),
    center_method = "equal_model_mean", scope = "production_prediction_rows")
}

.policy_metric_tails <- function(annual, scenario, adverse_tail) {
  states <- .policy_metric_fields[1:4]
  models <- split(annual, annual$model_id)
  rows <- list()
  for (p in unname(RP_LOW)) {
    eligible <- vapply(models, function(x) nrow(x) >= ceiling(1 / p), logical(1))
    # Never silently change the model ensemble to manufacture a sparse tail.
    if (!all(eligible)) {
      rows[[length(rows) + 1L]] <- data.frame(scenario = scenario,
        probability = p, return_period = 1 / p, scope = "equal_probability",
        status = "unavailable", reason = "Insufficient years in one or more matched models.")
      next
    }
    per_model <- lapply(models, function(x) vapply(states, function(s) {
      rank_interp(sort(x[[s]]), if (identical(adverse_tail, "high")) p else 1 - p)
    }, numeric(1)))
    levels <- as.list(apply(do.call(rbind, per_model), 2, stats::median))
    quantile_row <- data.frame(scenario = scenario, .policy_metric_contributions(levels),
      probability = p, return_period = 1 / p, scope = "equal_probability",
      center_method = "median_model_quantile", quantile_method = "rank_interp_n_p_plus_half",
      n_models = length(models), n_model_years = nrow(annual), status = "ok", reason = "")
    rows <- c(rows, list(quantile_row))
  }
  dplyr::bind_rows(rows)
}

.policy_metric_pipeline <- function(baseline, policy, prepared, method, pov_line,
                                    requested_residuals, shared_baseline, shared_policy,
                                    scenario, member) {
  context <- prepared$context
  exposure <- .validate_policy_annual_exposure(baseline, context)
  for (field in c("svy_row_id", "sim_year", "weight", "id_vec", "weather_exposure")) {
    if (!identical(baseline[[field]], policy[[field]])) {
      stop("Baseline/policy row alignment mismatch: ", field, call. = FALSE)
    }
  }
  if (length(baseline$y_point) != length(policy$y_point) ||
    !identical(is.na(baseline$y_point), is.na(policy$y_point))) {
    stop("Baseline/policy prediction missingness mismatch.", call. = FALSE)
  }
  correction <- policy$policy_correction
  if (!identical(correction$version, prepared$correction_version) ||
    !identical(correction$run_identity, prepared$run_identity)) {
    stop("Policy correction version/run identity mismatch.", call. = FALSE)
  }
  bctx <- step2_pipeline_context(baseline, shared_baseline)
  pctx <- step2_pipeline_context(policy, shared_policy)
  if (!identical(bctx$train_aug, pctx$train_aug) || !identical(bctx$id_col, pctx$id_col)) {
    stop("Baseline/policy residual context mismatch.", call. = FALSE)
  }
  effective <- requested_residuals
  if (is.null(bctx$train_aug) || !".resid" %in% names(bctx$train_aug)) effective <- "none"
  lookup <- .residual_lookup(bctx$train_aug, bctx$id_col)
  sigma2 <- .residual_sigma2(bctx$train_aug)
  is_log <- isTRUE(context$so$transform == "log")
  aggregate <- resolve_agg_fn(method)
  years <- sort(unique(baseline$sim_year))
  annual <- mechanisms <- list()
  for (year in years) {
    all_rows <- which(baseline$sim_year == year)
    rows <- all_rows[!is.na(baseline$y_point[all_rows])]
    if (!length(rows)) stop("No valid prediction rows in a simulated year.", call. = FALSE)
    ch <- .policy_annual_channel_block(baseline, prepared, exposure, rows)
    y <- baseline$y_point[rows]
    target <- policy$y_point[rows]
    reconstructed <- y + ch$delta_total
    error <- max(abs(reconstructed - target))
    if (!all(is.finite(c(y, target, reconstructed))) ||
      !is.finite(error) || error > .policy_metric_tolerance * max(1, abs(target))) {
      stop("Final cumulative state does not match production policy predictions.", call. = FALSE)
    }
    # Same row mask, draw helper and year seed as .aggregation_prepare_pipeline;
    # reuse its invariant lookup/variance without hashing training data per year.
    residual <- draw_residuals_vec(effective, bctx$train_aug, length(rows),
      baseline$id_vec[rows], bctx$id_col,
      seed = wise_seed(WISEAPP_DEFAULT_SEED, "residual", year),
      resid_lookup = lookup, resid_sigma2 = sigma2)
    w <- if (!is.null(baseline$weight)) as.numeric(baseline$weight[rows]) else NULL
    excluded <- integer(4)
    deltas <- list(rep(0, length(rows)), ch$delta_main, ch$delta_res1, ch$delta_res2)
    values <- numeric(4)
    for (j in seq_len(4)) {
      if (j > 1L) y <- y + deltas[[j]]
      mu <- if (is_log) exp(y + residual) else y + residual
      values[j] <- aggregate(mu, w, pov_line)
      excluded[j] <- sum(!is.finite(mu) | (method == "avg_poverty" & mu <= 0))
    }
    names(values) <- .policy_metric_fields[1:4]
    annual[[length(annual) + 1L]] <- data.frame(scenario = scenario, member = member,
      model_id = member, sim_year = year, .policy_metric_contributions(as.list(values)),
      n_prediction_rows = length(all_rows), n_retained_rows = length(rows),
      n_excluded_rows = length(all_rows) - length(rows),
      excluded_baseline = excluded[1L], excluded_after_main = excluded[2L],
      excluded_after_repositioning = excluded[3L], excluded_policy = excluded[4L],
      parity_error = error, requested_residuals = requested_residuals, effective_residuals = effective)
    ids <- baseline$svy_row_id[rows]
    weighted_mean <- function(x) resolve_agg_fn("mean")(x, w, NULL)
    for (hazard in names(prepared$products)) {
      product <- prepared$products[[hazard]]
      for (j in seq_len(ncol(product$interaction))) {
        r1 <- product$repositioning[ids, j]
        r2 <- product$interaction[ids, j]
        mechanisms[[length(mechanisms) + 1L]] <- data.frame(scenario = scenario,
          member = member, model_id = member, sim_year = year, hazard = hazard,
          category = if (is.null(product$categories)) NA_character_ else product$categories[j],
          contrast = if (is.null(product$categories)) "continuous_coefficient_change" else "fitted_reference_category_contrast",
          repositioning = if (prepared$repositioning_modeled) weighted_mean(r1) else NA_real_,
          interaction = if (prepared$interaction_included) weighted_mean(r2) else NA_real_,
          positive_repositioning_share = if (prepared$repositioning_modeled) weighted_mean(as.numeric(r1 > 0)) else NA_real_,
          negative_repositioning_share = if (prepared$repositioning_modeled) weighted_mean(as.numeric(r1 < 0)) else NA_real_,
          positive_interaction_share = if (prepared$interaction_included) weighted_mean(as.numeric(r2 > 0)) else NA_real_,
          negative_interaction_share = if (prepared$interaction_included) weighted_mean(as.numeric(r2 < 0)) else NA_real_,
          tau_pre = if (prepared$repositioning_modeled) weighted_mean(prepared$tau_i_pre[ids]) else NA_real_,
          tau_post = if (prepared$repositioning_modeled) weighted_mean(prepared$tau_i_post[ids]) else NA_real_,
          n_retained_rows = length(rows), weighted = !is.null(w),
          model_units = if (is_log) "log outcome units" else "model outcome units",
          weather_units = "exact fitted weather-input units; unit label unavailable",
          scope = "production_prediction_rows", scale = "model_scale")
      }
    }
  }
  list(annual = dplyr::bind_rows(annual), mechanisms = dplyr::bind_rows(mechanisms))
}

# Pure run-owned calculation. Only small annual/summary tables escape; cumulative
# household vectors and residual preparations are temporary, one member/year.
.policy_metric_decomposition <- function(baseline_hist, policy_hist,
                                         baseline_scenarios, policy_scenarios,
                                         prepared, method, pov_line = NULL,
                                         requested_residuals = "original",
                                         endpoint_series_baseline = NULL,
                                         endpoint_series_policy = NULL,
                                         focus_scenario = NULL, analysis_unit = NULL) {
  so <- baseline_hist$so
  hist_name <- baseline_hist$hist_label %||% "Historical"
  focus_scenario <- focus_scenario %||% if (length(baseline_scenarios)) names(baseline_scenarios)[1L] else hist_name
  metadata <- metric_metadata(method, so, pov_line, analysis_unit,
    weighted = !is.null(baseline_hist$pipeline$weight))
  metadata$focus_scenario <- focus_scenario
  metadata$requested_residuals <- requested_residuals
  metadata$scale <- "metric_aware"
  metadata$uncertainty <- "central_only"
  metadata$component_order <- "main -> repositioning -> interaction"
  metadata$correction_version <- "row_aligned_annual_v1"
  metadata$exposure_source <- "step2_prediction_row_mapping"
  metadata$population_scope <- "fixed survey rows; canonical state-specific metric eligibility retained"
  metadata$eligibility_caveat <- if (method == "avg_poverty") {
    "Positive finite welfare eligibility is evaluated by the canonical metric in each state; this is not a fixed eligible subpopulation."
  } else metadata$caveat
  metadata$parity_tolerance <- .policy_metric_tolerance
  endpoint <- list()
  for (nm in intersect(names(endpoint_series_baseline), names(endpoint_series_policy))) {
    endpoint[[nm]] <- paired_model_year_effects(endpoint_series_baseline[[nm]]$out,
      endpoint_series_policy[[nm]]$out)
  }
  endpoint_summary <- dplyr::bind_rows(lapply(names(endpoint), function(nm) {
    paired_effect_summary(endpoint[[nm]], scenario = nm, center = "equal_model_mean")
  }))
  result <- list(status = "unavailable", reason = "Annual channel source unavailable.",
    annual = data.frame(), summary = data.frame(), return_period = data.frame(),
    mechanisms = list(), metadata = metadata, endpoint_summary = endpoint_summary, scenarios = list())
  status <- .policy_endpoint_status(so, if (is.environment(prepared)) prepared$context else NULL)
  if (!identical(status$status, "ok")) {
    result$status <- status$status; result$reason <- status$reason
    result$endpoint_summary <- data.frame()
    return(result)
  }
  if (!is.environment(prepared) || !environmentIsLocked(prepared)) return(result)
  result$metadata$run_identity <- prepared$run_identity
  result$metadata$repositioning_modeled <- prepared$repositioning_modeled
  result$metadata$interaction_included <- prepared$interaction_included
  validation <- tryCatch({
    .validate_run_decomposition_context(prepared$context, prepared$run_identity)
    .policy_annual_channel_status(prepared$context)
  }, error = function(e) list(status = "unavailable", reason = conditionMessage(e)))
  if (!identical(validation$status, "ok")) {
    result$status <- validation$status; result$reason <- validation$reason
    return(result)
  }
  owners_b <- c(setNames(list(list(pipelines = list(Historical = baseline_hist$pipeline),
    shared_context = baseline_hist$shared_context)), hist_name), baseline_scenarios)
  owners_p <- c(setNames(list(list(pipelines = list(Historical = policy_hist$pipeline),
    shared_context = policy_hist$shared_context)), hist_name), policy_scenarios)
  for (nm in names(owners_b)) {
    calculated <- tryCatch({
      b <- owners_b[[nm]]; p <- owners_p[[nm]]
      if (!length(b$pipelines) || !identical(names(b$pipelines), names(p$pipelines))) {
        stop("Baseline/policy member identity mismatch.", call. = FALSE)
      }
      members <- lapply(names(b$pipelines), function(id) .policy_metric_pipeline(
        b$pipelines[[id]], p$pipelines[[id]], prepared, method, pov_line,
        requested_residuals, b$shared_context, p$shared_context, nm, id))
      annual <- dplyr::bind_rows(lapply(members, `[[`, "annual"))
      ep <- endpoint[[nm]]
      dropped <- 0L
      if (!is.null(ep)) {
        ep <- ep[is.finite(ep$baseline) & is.finite(ep$policy) & is.finite(ep$effect), , drop = FALSE]
        key <- function(x) paste(x$model_id, x$sim_year, sep = "\r")
        if (anyDuplicated(key(annual)) || anyDuplicated(key(ep))) stop("Duplicate model/year aggregate keys.")
        index <- match(key(ep), key(annual))
        if (anyNA(index)) stop("Channel summary cannot cover Results endpoint support.")
        dropped <- nrow(annual) - nrow(ep)
        annual <- annual[index, , drop = FALSE]
        for (field in c("baseline", "policy")) {
          if (any(!is.finite(annual[[field]])) || any(abs(annual[[field]] - ep[[field]]) >
            .policy_metric_tolerance * pmax(1, abs(ep[[field]])))) stop("Results endpoint aggregate parity mismatch.")
        }
      }
      if (!nrow(annual) || any(!is.finite(as.matrix(annual[, .policy_metric_fields])))) {
        stop("Nonfinite cumulative aggregates would change Results endpoint support.")
      }
      summary <- .policy_metric_summary(annual, nm)
      summary$n_dropped_model_years <- dropped
      diagnostic <- dplyr::bind_rows(lapply(members, `[[`, "mechanisms"))
      diagnostic <- diagnostic[paste(diagnostic$model_id, diagnostic$sim_year, sep = "\r") %in%
        paste(annual$model_id, annual$sim_year, sep = "\r"), , drop = FALSE]
      weather <- baseline_hist$sim_summary$weather
      if (is.data.frame(weather) && all(c("name", "units") %in% names(weather))) {
      units <- as.character(weather$units[match(diagnostic$hazard, weather$name)])
      known <- !is.na(units) & nzchar(units)
      continuous <- diagnostic$contrast == "continuous_coefficient_change"
      diagnostic$weather_units[known & continuous] <- units[known & continuous]
    }
    diagnostic$weather_units[diagnostic$contrast != "continuous_coefficient_change"] <-
      "category contrast; no per-unit slope"
      tails <- .policy_metric_tails(annual, nm, metadata$adverse_tail)
      # Results thresholds can use different marginal support from the matched
      # expected-effect headline. Do not assert tail parity merely by relabeling.
      if (!is.null(ep)) {
        bmatrix <- by_model_matrix(endpoint_series_baseline[[nm]]$out)
        pmatrix <- by_model_matrix(endpoint_series_policy[[nm]]$out)
        for (i in which(tails$scope == "equal_probability" & tails$status == "ok")) {
          probability <- tails$probability[i]
          endpoint_threshold <- function(mm) {
            if (is.null(mm) || probability < 1 / ncol(mm$vals)) return(NA_real_)
            thresholds <- by_model_rp_matrix(mm$vals, mm$sds, probability,
              metadata$adverse_tail)$rp
            stats::median(thresholds, na.rm = TRUE)
          }
          levels <- c(endpoint_threshold(bmatrix), endpoint_threshold(pmatrix))
          observed <- c(tails$baseline[i], tails$policy[i])
          parity <- all(is.finite(levels)) && all(abs(levels - observed) <=
            .policy_metric_tolerance * pmax(1, abs(levels)))
          if (!parity) {
            tails$status[i] <- "unavailable"
            tails$reason[i] <- "Matched channel scope does not reproduce Results marginal threshold endpoints."
            tails[i, .policy_metric_fields] <- NA_real_
          }
        }
      }
      list(status = "ok", reason = NULL, annual = annual, summary = summary,
        return_period = tails,
        mechanisms = diagnostic)
    }, error = function(e) list(status = "unavailable", reason = conditionMessage(e)))
    result$scenarios[[nm]] <- calculated
  }
  good <- Filter(function(x) identical(x$status, "ok"), result$scenarios)
  result$annual <- dplyr::bind_rows(lapply(good, `[[`, "annual"))
  result$summary <- dplyr::bind_rows(lapply(good, `[[`, "summary"))
  result$return_period <- dplyr::bind_rows(lapply(good, `[[`, "return_period"))
  mechanism_annual <- dplyr::bind_rows(lapply(good, `[[`, "mechanisms"))
  mechanism_summary <- data.frame()
  if (nrow(mechanism_annual)) {
    numeric_fields <- c("repositioning", "interaction", "positive_repositioning_share",
      "negative_repositioning_share", "positive_interaction_share", "negative_interaction_share", "tau_pre", "tau_post")
    mechanism_summary <- mechanism_annual |>
      dplyr::group_by(.data$scenario, .data$hazard, .data$category, .data$contrast,
        .data$model_units, .data$weather_units, .data$model_id) |>
      dplyr::summarise(dplyr::across(dplyr::all_of(numeric_fields), mean), .groups = "drop") |>
      dplyr::group_by(.data$scenario, .data$hazard, .data$category, .data$contrast,
        .data$model_units, .data$weather_units) |>
      dplyr::summarise(dplyr::across(dplyr::all_of(numeric_fields), mean), n_models = dplyr::n(), .groups = "drop")
  }
  result$mechanisms <- list(annual = mechanism_annual, summary = mechanism_summary,
    fitted_curve = if (prepared$repositioning_modeled) prepared$context$rif_grid else NULL,
    curve_scope = "unchanged Step 1 fitted curve; main-derived pre/post ranks",
    repositioning_status = if (prepared$repositioning_modeled) "modeled" else "Not modeled by this engine",
    interaction_status = if (prepared$interaction_included) "included" else "Interaction not included in fitted model")
  result$mechanisms$metadata <- list(scope = "production_prediction_rows",
    center_method = "equal_model_mean", scale = "model_scale", uncertainty = "central_only",
    model_units = if (isTRUE(so$transform == "log")) "log outcome units" else "model outcome units",
    weather_units = "exact fitted weather-input units; unit label unavailable",
    rank_convention = "fixed main-derived pre/post ranks; interaction evaluated at post-main rank",
    included_terms = "canonical repositioning and weather-policy interaction channel changes only",
    excluded_terms = "not a complete derivative of nonlinear fitted model terms")
  result$metadata$effective_residuals <- if (nrow(result$annual)) unique(result$annual$effective_residuals) else character()
  result$metadata$mixed_effective_residuals <- length(result$metadata$effective_residuals) > 1L
  focus <- result$scenarios[[focus_scenario]]
  result$status <- focus$status %||% "unavailable"
  result$reason <- focus$reason %||% if (is.null(focus)) "Results focus scenario unavailable." else NULL
  result
}
