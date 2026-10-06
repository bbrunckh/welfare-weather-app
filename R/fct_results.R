# Model results helpers ----
# Outcome preparation, coefficient helpers, and plot/table builders.
# Outcome preparation, coefficient helpers, and plot/table builders.
# Used by mod_1_07_results_server(). Stateless and testable without Shiny.    #


# Outcome preparation ----

#' Prepare the outcome column in survey_weather before model fitting
#'
#' Applies three sequential transformations in order:
#' 1. **LCU back-conversion** - multiply by `ppp2021` when `units == "LCU"`.
#'    Applies only to an existing continuous (log-transformed) monetary
#'    outcome column, e.g. `welfare`. Constructed indicators such as `poor`
#'    are unitless and are never back-converted.
#' 2. **Log transform** - `log()` when `transform == "log"`.
#' 3. **Binary poor indicator** - `welfare < povline` when `name == "poor"`
#'    and `povline` is non-NA. The stored `welfare` column is in 2021 PPP,
#'    so a poverty line expressed in LCU (`units == "LCU"`) is first divided
#'    by the per-observation `ppp2021` factor to place the comparison on a
#'    common scale.
#'
#' @param df A data frame containing the outcome column and optionally
#'   `welfare` and `ppp2021`.
#' @param so A one-row data frame as returned by `build_selected_outcome()`.
#'   Must contain columns `name`, `units`, `transform`, and `povline`.
#'
#' @return `df` with the outcome column mutated in place.
#'
#' @export
prepare_outcome_df <- function(df, so) {
  name <- as.character(so$name[1])
  units <- as.character(so$units[1])
  trans <- as.character(so$transform[1])
  povline <- so$povline[1]

  if (isTRUE(units == "LCU") && isTRUE(trans == "log") &&
    "ppp2021" %in% names(df) && name %in% names(df)) {
    df <- df |> dplyr::mutate(!!name := .data[[name]] * .data$ppp2021)
  }
  if (isTRUE(trans == "log")) {
    df <- df |> dplyr::mutate(!!name := log(.data[[name]]))
  }
  if (isTRUE(name == "poor") && !is.na(povline) && "welfare" %in% names(df)) {
    line <- .povline_to_ppp(povline, df, units == "LCU")
    df <- df |>
      dplyr::mutate(!!name := as.numeric(.data[["welfare"]] < line))
  }

  df
}


#' Ensure a derived outcome column exists without applying transforms
#'
#' The `poor` outcome is synthesized from `welfare` and the selected poverty
#' line. Step 2 survey snapshots can omit that derived column, so callers that
#' operate on those snapshots must restore it before reading the outcome.
#' Existing columns are left untouched. Unlike `prepare_outcome_df()`, this
#' helper never logs or currency-converts values.
#'
#' @param df A survey data frame.
#' @param so Outcome metadata containing `name`, `units`, and `povline`.
#' @return `df`, with a derivable missing outcome column added.
#' @export
ensure_outcome_column <- function(df, so) {
  if (is.null(df) || !is.data.frame(df) || is.null(so)) {
    return(df)
  }

  name <- as.character(so$name %||% NA_character_)[1]
  if (is.na(name) || !nzchar(name) || name %in% names(df)) {
    return(df)
  }
  if (!identical(name, "poor") || !"welfare" %in% names(df)) {
    return(df)
  }

  povline <- suppressWarnings(as.numeric(so$povline %||% NA_real_)[1])
  if (!is.finite(povline)) {
    return(df)
  }

  units <- as.character(so$units %||% NA_character_)[1]
  line <- .povline_to_ppp(povline, df, identical(units, "LCU"))
  df[[name]] <- as.numeric(df[["welfare"]] < line)
  df
}


# Scale a user-specified poverty line to match the stored welfare column
# (2021 PPP). LCU lines are divided by the per-observation ppp2021 factor;
# PPP lines - and data loaded without deflators, where no load-time
# conversion took place - are compared directly.
.povline_to_ppp <- function(pl, df, is_lcu) {
  if (isTRUE(is_lcu) && "ppp2021" %in% names(df)) pl / df$ppp2021 else pl
}


# Coefficient helpers ----

#' Extract the Variance-Covariance Matrix from a fixest Fit
#'
#' Tries the fit-time VCV first (respecting the cluster/VCV specification
#' passed at estimation), then progressively simpler alternatives
#' (`COEF_VCOV_SPEC`, one-way `~loc_id` clusters, `"HC1"`, `"iid"`), and
#' finally plain `stats::vcov()`.
#'
#' @param fit A native `fixest` model object.
#'
#' @return A variance-covariance matrix, or `NULL` when extraction fails.
#'
#' @export
.fixest_vcov <- function(fit) {
  # Try the fit-time VCV first (respects cluster= arg passed at estimation)
  v <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  if (!is.null(v)) {
    return(v)
  }
  for (spec in list(COEF_VCOV_SPEC, ~loc_id, "HC1", "iid")) {
    v <- tryCatch(stats::vcov(fit, vcov = spec), error = function(e) NULL)
    if (!is.null(v)) {
      return(v)
    }
  }
  stats::vcov(fit)
}

# RIF sub-fit (one fixest per tau) for `tau`, or NULL. `taus` is the full
# quantile grid in the order the fixest_multi was estimated.
.rif_subfit <- function(fit_multi, taus, tau) {
  if (is.null(fit_multi) || !length(taus)) {
    return(NULL)
  }
  if (inherits(fit_multi, "fixest")) {
    return(fit_multi)
  }
  i <- which.min(abs(taus - tau))
  tryCatch(fit_multi[[i]], error = function(e) NULL)
}

# Standard error of a weighted sum of coefficients, sqrt(w' V w), from the
# fit's full VCV so polynomial terms keep their covariance (R2-BUG-19). Falls
# back to the independent-terms formula when the VCV or a term is unavailable.
.rif_combined_se <- function(fit, terms, weights, se) {
  keep <- weights != 0
  terms <- terms[keep]
  w <- weights[keep]
  V <- if (is.null(fit)) NULL else tryCatch(.fixest_vcov(fit), error = function(e) NULL)
  if (!is.null(V) && length(terms) && all(terms %in% colnames(V))) {
    v <- as.numeric(t(w) %*% V[terms, terms, drop = FALSE] %*% w)
    return(sqrt(max(v, 0)))
  }
  sqrt(sum((weights * se)^2, na.rm = TRUE))
}

.fixest_vcov_spec <- function(fit) {
  # Try the fit-time VCV first (respects cluster= arg passed at estimation)
  ok <- tryCatch(
    {
      summary(fit)
      TRUE
    },
    error = function(e) FALSE
  )
  if (ok) {
    return(NULL)
  } # NULL signals "use default"
  for (spec in list(COEF_VCOV_SPEC, ~loc_id, "HC1", "iid")) {
    ok <- tryCatch(
      {
        summary(fit, vcov = spec)
        TRUE
      },
      error = function(e) FALSE
    )
    if (ok) {
      return(spec)
    }
  }
  "iid"
}

.fixest_coeftable <- function(fit) {
  # Try the fit-time VCV first (respects cluster= arg passed at estimation)
  ct <- tryCatch(as.data.frame(fixest::coeftable(fit)), error = function(e) NULL)
  if (!is.null(ct)) {
    return(ct)
  }
  for (spec in list(COEF_VCOV_SPEC, ~loc_id, "HC1", "iid")) {
    ct <- tryCatch(
      as.data.frame(fixest::coeftable(fit, vcov = spec)),
      error = function(e) NULL
    )
    if (!is.null(ct)) {
      return(ct)
    }
  }
  as.data.frame(fixest::coeftable(fit))
}


weather_coef_names <- function(fit, weather_terms) {
  all_coefs <- names(stats::coef(fit))

  # Build safe word-boundary pattern
  pattern <- paste0("\\b(", paste(weather_terms, collapse = "|"), ")\\b")

  # Return matching coefficient names
  all_coefs[grepl(pattern, all_coefs)]
}


#' Detect survey columns modified between training data and a counterfactual
#'
#' Compares two household-level data frames column-by-column. Returns the names
#' of columns whose values differ for at least one household. Used to identify
#' policy-modified variables in Module 3 simulations (`apply_policy_to_svy()`
#' flips e.g. `electricity`, `internet`, `employed`) so the coefficient-
#' uncertainty mask can be extended beyond weather terms.
#'
#' Both inputs are expected at the household grain (one row per household).
#' Joins on `id_col` if present in both; otherwise falls back to a row-order
#' comparison clipped to the shorter frame.
#'
#' @param svy_modified Data frame. Survey after counterfactual modification.
#' @param svy_train Data frame. Reference training data.
#' @param id_col Character or NULL. Optional household ID column for joining.
#' @param exclude_cols Character vector. Columns to skip (outcome, weights, FE,
#'   metadata, SP transfer column).
#'
#' @return Character vector of modified column names (possibly empty).
#'
#' @export
detect_modified_cols <- function(svy_modified, svy_train,
                                 id_col = NULL,
                                 exclude_cols = character()) {
  if (is.null(svy_modified) || is.null(svy_train)) {
    return(character())
  }

  common <- intersect(names(svy_modified), names(svy_train))
  candidates <- setdiff(common, c(id_col, exclude_cols))
  if (length(candidates) == 0L) {
    return(character())
  }

  if (!is.null(id_col) && id_col %in% names(svy_modified) &&
    id_col %in% names(svy_train)) {
    keep <- intersect(svy_modified[[id_col]], svy_train[[id_col]])
    if (length(keep) == 0L) {
      return(character())
    }
    m <- svy_modified[match(keep, svy_modified[[id_col]]), candidates,
      drop = FALSE
    ]
    t <- svy_train[match(keep, svy_train[[id_col]]), candidates, drop = FALSE]
  } else {
    n <- min(nrow(svy_modified), nrow(svy_train))
    if (n == 0L) {
      return(character())
    }
    m <- svy_modified[seq_len(n), candidates, drop = FALSE]
    t <- svy_train[seq_len(n), candidates, drop = FALSE]
  }

  changed <- vapply(candidates, function(col) {
    a <- m[[col]]
    b <- t[[col]]
    if (length(a) != length(b)) {
      return(TRUE)
    }
    any((a != b) | (is.na(a) != is.na(b)), na.rm = TRUE)
  }, logical(1))

  candidates[changed]
}


#' Attach an active-coefficient mask to a Cholesky vcov object
#'
#' Wrapper that builds the additive-decomposition active mask
#' (\code{\link{build_active_coef_mask}}) and attaches it to `chol_obj` for
#' downstream consumption by \code{\link{compute_factor_loading}} (linear
#' engine) and \code{interpolate_F_loading()} (RIF engine).
#'
#' Active terms = `weather_terms` plus any columns whose values differ
#' between `svy_modified` and `train_data` (excluding weather, outcome,
#' weights, FE columns, and `SP_TRANSFER_COL`).
#'
#' The mask is only built when `residuals == "original"` and
#' `propagate_all_covariate_uncertainty == FALSE`. Otherwise the function
#' returns `chol_obj` unchanged (legacy full propagation).
#'
#' Handles both linear shape (`chol_obj` is a list with `$L`, `$beta`, ...)
#' and RIF shape (`chol_obj` is a list of per-tau lists). For RIF, the mask
#' is attached via `attr(chol_obj, "active_mask")`; per-tau attachment is
#' not needed because all tau fits share coefficient ordering.
#'
#' @param chol_obj Output of `compute_chol_vcov()` or NULL.
#' @param svy_modified Household-level survey (possibly policy-modified).
#'   Compared against `svy_reference` to detect which covariates changed
#'   between baseline and counterfactual.
#' @param svy_reference Household-level survey to diff against. For Module 2
#'   this is the (unmodified) baseline survey, so the diff is empty and
#'   `active_terms = weather_terms`. For Module 3 this is the pre-policy
#'   baseline `svy_baseline`, so the diff returns exactly the policy-modified
#'   columns. If NULL, falls back to `train_data` (legacy comparison).
#' @param train_data Training-data data frame. Used as the reference when
#'   `svy_reference` is NULL.
#' @param weather_terms Character vector of weather variable names.
#' @param outcome_col Character or NULL. Outcome column name to exclude from
#'   the diff (e.g. `"welfare"`). The outcome is log-transformed in
#'   `train_data` but not in `svy`, so it would otherwise be flagged as
#'   "modified" - defensively excluded even though no coefficient is named
#'   after it.
#' @param residuals Character. Residual mode (`"original"`, `"resample"`, ...).
#' @param propagate_all_covariate_uncertainty Logical. TRUE disables the
#'   mask (legacy behaviour).
#'
#' @return `chol_obj` with `$active_mask` (or `attr(., "active_mask")` for
#'   RIF) set when appropriate; otherwise unchanged.
#'
#' @export
attach_active_mask <- function(chol_obj,
                               svy_modified,
                               train_data,
                               weather_terms,
                               residuals,
                               svy_reference = NULL,
                               outcome_col = NULL,
                               propagate_all_covariate_uncertainty = FALSE) {
  if (is.null(chol_obj)) {
    return(chol_obj)
  }
  if (isTRUE(propagate_all_covariate_uncertainty)) {
    return(chol_obj)
  }

  # Determine coefficient names from either shape. RIF chol_obj is a list of
  # per-tau lists (each with $L, $beta); linear chol_obj is a single list with
  # $L and $beta at the top level.
  is_rif_shape <- is.list(chol_obj) && !("L" %in% names(chol_obj)) &&
    length(chol_obj) > 0L &&
    is.list(chol_obj[[1]]) && "beta" %in% names(chol_obj[[1]])

  # Gating: the cancellation argument applies under (a) "original" residuals
  # for the linear engine - the per-household residual is held fixed across beta
  # draws and absorbs uncertainty on unchanged coefficients - or (b) any
  # residuals mode for the RIF engine, because the RIF prediction is
  # y_baseline + delta_i and the level y_baseline plays the same role as the
  # fixed residual on the linear path. Under "resample"/"normal" with the
  # linear engine, residuals are drawn independently of beta and the
  # cancellation does not hold.
  if (!is_rif_shape && !identical(residuals, "original")) {
    return(chol_obj)
  }
  coef_names <- if (is_rif_shape) {
    names(chol_obj[[1]]$beta)
  } else if (is.list(chol_obj) && "beta" %in% names(chol_obj)) {
    names(chol_obj$beta)
  } else {
    NULL
  }
  if (is.null(coef_names)) {
    return(chol_obj)
  }

  # Prefer comparing against the pre-counterfactual baseline survey
  # (svy_reference) - that is *exactly* what we want to diff against.
  # Fall back to train_data when the baseline is not available; in that
  # case the comparison is correct iff the user simulates on the same
  # underlying survey they trained on (common case).
  reference <- svy_reference %||% train_data

  id_col <- intersect(
    c("pid", "hhid", "fid"),
    intersect(names(svy_modified), names(reference))
  )
  id_col <- if (length(id_col) > 0L) id_col[[1L]] else NULL

  weight_cols <- grep("^weight$|^hhweight$|^wgt$|^pw$",
    union(names(svy_modified), names(reference)),
    value = TRUE, ignore.case = TRUE
  )
  exclude_cols <- c(
    SP_TRANSFER_COL, ".svy_row_id",
    "year", "sim_year", "int_month",
    "code", "survname", "loc_id",
    weather_terms, weight_cols, outcome_col
  )

  modified <- detect_modified_cols(svy_modified, reference,
    id_col = id_col,
    exclude_cols = exclude_cols
  )
  active_terms <- unique(c(weather_terms, modified))
  if (length(active_terms) == 0L) {
    return(chol_obj)
  }

  mask <- tryCatch(
    build_active_coef_mask(coef_names, active_terms),
    error = function(e) {
      warning(
        "[attach_active_mask] mask construction failed: ",
        conditionMessage(e)
      )
      NULL
    }
  )
  if (is.null(mask)) {
    return(chol_obj)
  }

  # Build L_active: Cholesky factor of the active block of Sigma. For the
  # lower-triangular factor emitted by compute_chol_vcov(),
  # Sigma[mask, mask] = L[mask, ] %*% t(L[mask, ]), so the full K x K
  # covariance reconstruction is unnecessary. Legacy or differently oriented
  # factors use the old route after this validation fails.
  cholesky_active_block <- function(L_full) {
    valid_factor <- is.matrix(L_full) && nrow(L_full) == ncol(L_full) &&
      all(is.finite(L_full))
    if (valid_factor) {
      upper <- L_full[upper.tri(L_full)]
      scale <- max(1, max(abs(L_full)))
      valid_factor <- !length(upper) ||
        max(abs(upper)) <= sqrt(.Machine$double.eps) * scale
    }

    active_rows <- L_full[mask, , drop = FALSE]
    active_covariance <- if (valid_factor) {
      tcrossprod(active_rows)
    } else {
      Sigma_full <- L_full %*% t(L_full)
      Sigma_full[mask, mask, drop = FALSE]
    }

    tryCatch(t(chol(active_covariance)),
      error = function(e) {
        warning(
          "[attach_active_mask] Cholesky of active block failed: ",
          conditionMessage(e),
          " - falling back to no masking."
        )
        NULL
      }
    )
  }

  if (is_rif_shape) {
    L_active_list <- lapply(chol_obj, function(x) cholesky_active_block(x$L))
    if (any(vapply(L_active_list, is.null, logical(1)))) {
      return(chol_obj)
    }
    for (k in seq_along(chol_obj)) {
      chol_obj[[k]]$L_active <- L_active_list[[k]]
    }
    attr(chol_obj, "active_mask") <- mask
  } else {
    L_active <- cholesky_active_block(chol_obj$L)
    if (is.null(L_active)) {
      return(chol_obj)
    }
    chol_obj$L_active <- L_active
    chol_obj$active_mask <- mask
  }

  # Diagnostic so users can verify the additive-decomposition SE is in
  # effect. Printed once per simulation run.
  kept <- names(mask)[mask]
  message(sprintf(
    "[active_mask] additive-decomposition SE active: keeping %d/%d coefficients (%s engine). Active: %s",
    sum(mask), length(mask),
    if (is_rif_shape) "RIF" else "linear",
    paste(kept, collapse = ", ")
  ))
  chol_obj
}


#' Build a logical mask over coefficient names for the active variable set
#'
#' Returns a length-K logical vector flagging coefficients whose names involve
#' any term in `active_terms` (via word-boundary regex). Used by the additive-
#' decomposition SE: when residuals are held fixed per household ("original"),
#' uncertainty on coefficients for variables that do not change between
#' baseline and counterfactual cancels through the residual term, so only the
#' active subset contributes to `var_coef`.
#'
#' The intercept (if present) is forced to FALSE.
#'
#' @param coef_names Character vector. Names from `coef(fit)` /
#'   `names(chol_obj$beta)`, in the same order as the design-matrix columns.
#' @param active_terms Character vector. Raw variable names whose coefficients
#'   (and any interactions involving them) should remain active.
#'
#' @return Named logical vector of length `length(coef_names)`, or NULL if
#'   `active_terms` is empty.
#'
#' @export
build_active_coef_mask <- function(coef_names, active_terms) {
  active_terms <- active_terms[nzchar(active_terms)]
  if (length(active_terms) == 0L) {
    warning(
      "[build_active_coef_mask] no active terms supplied; ",
      "returning NULL (caller will fall back to full propagation)."
    )
    return(NULL)
  }

  # Escape regex metacharacters in term names (e.g. dots in column names).
  esc <- gsub("([][{}().+*^$|?\\\\])", "\\\\\\1", active_terms)

  # Word-boundary match plus a fallback for fixest factor expansions that use
  # "::" between variable and level (e.g. "tx::level1:urban").
  pattern <- paste0(
    "(\\b(", paste(esc, collapse = "|"), ")\\b)",
    "|((^|[^A-Za-z0-9_])(", paste(esc, collapse = "|"),
    ")(::|$))"
  )

  mask <- grepl(pattern, coef_names)
  names(mask) <- coef_names

  if ("(Intercept)" %in% coef_names) mask[["(Intercept)"]] <- FALSE
  mask
}


#' Build a human-readable label for a coefficient name
#'
#' Splits on `":"` and applies `label_fun` to each component, joining
#' with `" * "`.
#'
#' @param coef_name Scalar character coefficient name, e.g. `"tx:urban"`.
#' @param label_fun Function mapping a variable name to a readable label.
#'   Defaults to `identity`.
#'
#' @return Scalar character label.
#'
#' @export
coef_label <- function(coef_name, label_fun = identity) {
  parts <- strsplit(coef_name, ":")[[1]]
  paste(vapply(parts, label_fun, character(1)), collapse = " \u00d7 ")
}

# Pretty polynomial term name: I(I(t^2)) / I(t^2) -> "<label of t>²".
.pretty_poly_label <- function(term, label_fun = identity) {
  m <- regmatches(term, regexec(
    "^I\\((?:I\\()?([^\\^]+)\\^([23])\\)\\)?$",
    term
  ))[[1]]
  if (length(m) != 3) {
    return(term)
  }
  lab <- tryCatch(label_fun(m[2]), error = function(e) m[2])
  if (is.null(lab) || is.na(lab) || !nzchar(lab)) lab <- m[2]
  paste0(lab, if (m[3] == "2") "\u00b2" else "\u00b3")
}


#' Build a named coefficient map for jtools
#'
#' Returns a named vector where names are human-readable labels and values
#' are raw coefficient names, suitable for the `coefs` argument of
#' `jtools::plot_summs()` / `jtools::export_summs()`.
#'
#' @param coef_names Character vector of raw coefficient names.
#' @param label_fun  Function mapping variable names to readable labels.
#'
#' @return Named character vector.
#'
#' @export
make_coef_map <- function(coef_names, label_fun = identity) {
  one_label <- function(term) {
    parts <- strsplit(as.character(term), ":", fixed = TRUE)[[1]]
    parts <- vapply(parts, function(part) {
      poly <- .pretty_poly_label(part, label_fun)
      if (!identical(poly, part)) return(poly)
      m <- regexec("^([^\\[\\(]+)([\\[\\(].*)$", part)
      hit <- regmatches(part, m)[[1]]
      if (length(hit) == 3L) {
        return(.cut_bin_label(hit[[3]]))
      }
      tryCatch(label_fun(part), error = function(e) part)
    }, character(1))
    paste(parts, collapse = " \u00d7 ")
  }
  readable <- vapply(coef_names, one_label, character(1))
  stats::setNames(coef_names, readable)
}


# Engine helpers ----

#' Extract the native model object from a fit_model result
#'
#' For `"fixest"`, `"ranger"`, and `"xgboost"` engines the object stored by
#' `fit_model()` is already a native R model object - no unwrapping needed.
#' For the `"rif"` engine, each fit is a `fixest_multi` (list of 9 models).
#' This function returns the object as-is; use `extract_rif_median()` to
#' get a single representative model for diagnostics.
#'
#' @param fit    A model object as stored in `fit_model()$fit1` etc.
#' @param engine Scalar character engine key (e.g. `"fixest"`).
#'
#' @return The native model object.
#'
#' @export
extract_native_fit <- function(fit, engine = "fixest") {
  fit
}


#' Resolve a fitted model's design matrix, preferring the cached copy
#'
#' `fit_model()` strips the embedded data from each fixest fit's `$call` to
#' save memory, after which `stats::model.matrix(fit)` errors (fixest tries to
#' re-fetch the now-removed data). Before slimming, `fit_model()` caches fit3's
#' design matrix in `attr(fit, "wise_mm")`. This helper returns that cached
#' matrix when present and otherwise recomputes (for unslimmed fits, or
#' non-fit3 fits that were never cached).
#'
#' @param model A fitted model object (typically `fixest`).
#'
#' @return A data frame design matrix, or `NULL` if it cannot be resolved.
#'
#' @export
resolve_model_matrix <- function(model) {
  cached <- attr(model, "wise_mm")
  if (!is.null(cached)) {
    return(as.data.frame(cached))
  }
  tryCatch(
    as.data.frame(stats::model.matrix(model)),
    error = function(e) NULL
  )
}


#' Extract the median quantile model from a RIF fixest_multi
#'
#' For diagnostic functions that require a single fixest model, this extracts
#' the median quantile (tau = 0.5, index 5) from the 9-quantile stack.
#' Returns the input unchanged for non-RIF engines.
#'
#' @param fit    A model object (fixest_multi for RIF, or single model).
#' @param engine Scalar character engine key.
#'
#' @return A single fixest model object.
#'
#' @export
extract_rif_median <- function(fit, engine = "fixest") {
  if (identical(engine, "rif") && (inherits(fit, "fixest_multi") || is.list(fit))) {
    # Index 5 = tau = 0.5 (median)
    idx <- min(5L, length(fit))
    fit[[idx]]
  } else {
    fit
  }
}


#' Test whether a fit_model result represents a logistic model
#'
#' Checks `model_type` in the list returned by `fit_model()`.
#'
#' @param fit_list Named list returned by `fit_model()`, containing at least
#'   `$model_type` and `$engine`.
#'
#' @return Scalar logical.
#'
#' @export
is_logistic_fit <- function(fit_list) {
  mt <- tolower(fit_list$model_type %||% "")
  isTRUE(grepl("logistic|logit|binary", mt))
}


#' Plot standard diagnostic panels for a fixest fitted model
#'
#' Produces a residual-vs-fitted ggplot. Returns a blank plot on error.
#'
#' @param model  A native model object (output of `extract_native_fit()`).
#' @param engine Scalar character engine key from `fit_model()$engine`.
#'   Kept for backward compatibility.
#'
#' @return A `ggplot` object.
#'
#' @export
plot_residual_panels <- function(model, is_logistic = FALSE) {
  if (is_logistic) {
    # Binary outcomes: raw residuals vs fitted are unreadable (all points on
    # two curves), so show binned residual means by decile of predicted risk
    # (Gelman & Hill). Bins should scatter around zero without a trend.
    return(tryCatch(
      {
        p <- as.numeric(stats::fitted(model))
        res <- tryCatch(as.numeric(stats::residuals(model, type = "response")),
          error = function(e) as.numeric(stats::residuals(model))
        )
        n <- min(length(p), length(res))
        p <- p[seq_len(n)]
        res <- res[seq_len(n)]

        k <- max(3L, min(10L, floor(n / 20)))
        ord <- order(p)
        brks <- unique(floor(seq(0, n, length.out = k + 1)))
        if (length(brks) < 3) {
          return(blank_plot("Too few observations for binned residuals."))
        }
        grp <- cut(seq_len(n), breaks = brks, include.lowest = TRUE)

        bdf <- data.frame(p = p[ord], res = res[ord], grp = grp)
        agg <- stats::aggregate(cbind(pred = p, mean_res = res) ~ grp,
          data = bdf, FUN = mean
        )
        cnt <- as.data.frame(table(bdf$grp))
        agg$n <- cnt$Freq[match(as.character(agg$grp), as.character(cnt$Var1))]
        agg$se <- vapply(split(bdf$res, bdf$grp), function(r) {
          if (length(r) > 1) stats::sd(r) / sqrt(length(r)) else NA_real_
        }, numeric(1))

        ggplot2::ggplot(agg, ggplot2::aes(x = .data$pred, y = .data$mean_res)) +
          ggplot2::geom_ribbon(
            ggplot2::aes(
              ymin = .data$mean_res - 2 * .data$se,
              ymax = .data$mean_res + 2 * .data$se
            ),
            fill = .wise_blue, alpha = 0.15
          ) +
          ggplot2::geom_hline(
            yintercept = 0, color = .wise_zero,
            linetype = "dashed"
          ) +
          ggplot2::geom_line(color = .wise_blue, linewidth = 0.6) +
          ggplot2::geom_point(color = .wise_blue, size = 2) +
          theme_wise() +
          ggplot2::labs(
            subtitle = "Binned residuals by predicted risk",
            x = "Predicted risk (bin mean)",
            y = "Mean residual in bin"
          )
      },
      error = function(e) {
        blank_plot(paste(
          "Diagnostic plot error:",
          conditionMessage(e)
        ))
      }
    ))
  }

  # Linear / LPM / RIF: residuals vs fitted next to a normal QQ plot.
  tryCatch(
    {
      res <- as.numeric(stats::residuals(model))
      fitted <- as.numeric(stats::fitted(model))
      n <- min(length(fitted), length(res))
      df <- data.frame(fitted = fitted[seq_len(n)], residuals = res[seq_len(n)])

      p1 <- ggplot2::ggplot(df, ggplot2::aes(x = .data$fitted, y = .data$residuals)) +
        ggplot2::geom_point(alpha = 0.15) +
        ggplot2::geom_hline(
          yintercept = 0, color = .wise_zero,
          linetype = "dashed"
        ) +
        ggplot2::geom_smooth(
          method = "loess", se = FALSE, color = .wise_blue,
          linewidth = 0.8, formula = y ~ x
        ) +
        theme_wise(base_size = 14) +
        ggplot2::labs(
          subtitle = "Residuals vs fitted",
          x = "Fitted values", y = "Residuals"
        )

      p2 <- ggplot2::ggplot(df, ggplot2::aes(sample = .data$residuals)) +
        ggplot2::stat_qq(alpha = 0.15, size = 1) +
        ggplot2::stat_qq_line(color = .wise_blue, linewidth = 0.6) +
        theme_wise(base_size = 14) +
        ggplot2::labs(
          subtitle = "Normal Q-Q",
          x = "Theoretical quantiles", y = "Sample quantiles"
        )

      p1 + p2 + patchwork::plot_layout(ncol = 2)
    },
    error = function(e) {
      blank_plot(paste(
        "Diagnostic plot error:",
        conditionMessage(e)
      ))
    }
  )
}


#' Get first displayed bin label for a binned weather variable
#'
#' Uses the same ordering logic as `plot_weather_dist()`: factor level order
#' if factor, otherwise sorted unique character values.
#'
#' @param df A data frame containing column `hv`.
#' @param hv Scalar character. Weather variable column name.
#'
#' @return Character scalar first bin label, or `NA_character_`.
#' @export
get_first_bin_label <- function(df, hv) {
  if (is.null(df) || is.na(hv) || !(hv %in% names(df))) {
    return(NA_character_)
  }

  x <- df[[hv]]
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA_character_)
  }

  labels <- if (is.factor(x)) levels(x) else sort(unique(as.character(x)))
  if (length(labels) == 0) {
    return(NA_character_)
  }

  labels[[1]]
}

# Plot / table builders ----

#' Build a coefficient plot across three progressive model fits
#'
#' Uses the fits' stored `fixest` SEs and plots all three models side-by-side,
#' replicating the `jtools::plot_summs()` style. For RIF engines, produces
#' beta-curve plots (coefficient vs quantile, faceted by term).
#'
#' @param fit1,fit2,fit3    Native fixest model objects (or fixest_multi for RIF).
#' @param weather_terms     Character vector of base weather variable names.
#' @param interaction_terms Character vector of interaction term strings.
#' @param outcome_label     Scalar character label for the x-axis.
#' @param label_fun         Function mapping variable names to readable labels.
#' @param engine            Scalar character engine key.
#' @param rif_grid          Optional tidy data frame of RIF beta curves (from
#'   \code{fit_model()$rif_grid}). Used only when \code{engine = "rif"}.
#' @param pred_var          Optional scalar character. Weather predictor used
#'   to filter RIF curves; when `NULL`, all weather terms are shown.
#' @param x_label           Optional scalar character. When non-NULL, replaces
#'   the default x-axis title ("Effect on <outcome_label>").
#'
#' @return A `ggplot` object.
#'
#' @export
make_coefplot <- function(fit1, fit2, fit3,
                          weather_terms,
                          interaction_terms,
                          outcome_label = "outcome",
                          label_fun = identity,
                          engine = "fixest",
                          rif_grid = NULL,
                          pred_var = NULL,
                          x_label = NULL,
                          has_controls = TRUE) {
  # --- RIF branch: beta curve plot -------------------------------------------
  if (identical(engine, "rif") && !is.null(rif_grid)) {
    return(tryCatch(
      {
        taus <- sort(unique(rif_grid$tau))

        # Filter terms: by pred_var if supplied, otherwise all weather terms
        filter_terms <- if (!is.null(pred_var)) pred_var else weather_terms
        term_esc <- gsub("([\\[\\]\\(\\)\\^\\$\\.\\*\\+\\?])", "\\\\\\1", filter_terms)
        weather_pattern <- paste0("\\b(", paste(term_esc, collapse = "|"), ")\\b")

        all_terms <- unique(rif_grid$term)
        keep <- grepl(weather_pattern, all_terms)
        if (!any(keep)) {
          if (!is.null(pred_var)) {
            return(blank_plot(paste0("No RIF terms found for '", pred_var, "'.")))
          }
          keep <- rep(TRUE, length(all_terms))
        }
        plot_terms <- all_terms[keep]

        plot_data <- rif_grid[rif_grid$term %in% plot_terms, ]
  lab3 <- "FE + covariates"
      plot_data$model_label <- factor(
        dplyr::case_when(
          plot_data$model == 1L ~ "No FE or covariates",
          plot_data$model == 2L ~ "No covariates",
          TRUE ~ lab3
        ),
        levels = c("No FE or covariates", "No covariates", lab3)
      )
        plot_data$term_label <- vapply(
          plot_data$term, function(t) coef_label(t, label_fun), character(1)
        )

        # Order facets so each main is followed by its interactions, grouped
        # by whichever chunk contains the weather variable (handles both
        # `weather:modx` and `modx:weather` orderings).
        protected <- gsub("::", "", plot_data$term, fixed = TRUE)
        parts <- strsplit(protected, ":", fixed = TRUE)
        main_part <- vapply(parts, function(p) {
          hit <- p[grepl(weather_pattern, p)]
          if (length(hit) == 0) p[1] else hit[1]
        }, character(1))
        is_int <- lengths(parts) > 1
        term_levels <- unique(plot_data$term_label[
          order(match(main_part, unique(main_part)), is_int)
        ])
        plot_data$term_label <- factor(plot_data$term_label, levels = term_levels)

        ggplot2::ggplot(plot_data, ggplot2::aes(
          x = tau, y = estimate,
          colour = model_label,
          fill = model_label
        )) +
          ggplot2::geom_hline(
            yintercept = 0, linetype = "dashed",
            colour = .wise_zero
          ) +
          ggplot2::geom_ribbon(
            ggplot2::aes(ymin = conf.low, ymax = conf.high),
            alpha = 0.10, colour = NA
          ) +
          ggplot2::geom_line(linewidth = 0.8) +
          ggplot2::geom_point(size = 2) +
          ggplot2::facet_wrap(~term_label, scales = "free_y", ncol = 2) +
          ggplot2::scale_x_continuous(
            breaks = taus,
            labels = scales::percent_format(1)
          ) +
          wise_scale_colour_cat(name = NULL) +
          wise_scale_fill_cat(name = NULL) +
          ggplot2::labs(
            subtitle = paste("UQR coefficients for", label_fun(pred_var)),
            x = "Welfare quantile",
            y = stringr::str_wrap(paste0("Effect on ", outcome_label), 50),
            caption = "Ribbon = 95% CI"
          ) +
          theme_wise() +
          ggplot2::theme(
            legend.position  = "bottom",
            panel.border     = ggplot2::element_blank(),
            strip.background = ggplot2::element_blank()
          )
      },
      error = function(e) blank_plot(paste0("RIF coefficient plot error: ", conditionMessage(e)))
    ))
  }

  if (!requireNamespace("fixest", quietly = TRUE)) {
    return(blank_plot("Package 'fixest' is required."))
  }

  # Spec (3) equals spec (2) when no controls are selected - say so instead
  # of labelling an identical column "FE + controls".
  lab3 <- "FE + covariates"

  model_list <- list("No FE or covariates" = fit1, "No covariates" = fit2)
  model_list[[lab3]] <- fit3

  p <- tryCatch(
    {
      coef_data <- purrr::imap_dfr(model_list, function(fit, model_name) {
        ct <- tryCatch(
          .fixest_coeftable(fit),
          error = function(e) NULL
        )
        if (is.null(ct)) {
          return(NULL)
        }
        ct$term <- rownames(ct)
        ct$model <- model_name
        ct
      })

      filter_terms <- if (!is.null(pred_var)) pred_var else weather_terms
      term_esc <- gsub("([\\[\\]\\(\\)\\^\\$\\.\\*\\+\\?])", "\\\\\\1", filter_terms)
      weather_pattern <- paste0("\\b(", paste(term_esc, collapse = "|"), ")\\b")
      keep_terms <- weather_coef_names(fit3, filter_terms)
      coef_data <- coef_data[coef_data$term %in% keep_terms, ]

      if (nrow(coef_data) == 0) {
        return(blank_plot("No weather coefficients found to plot."))
      }

      coef_map <- make_coef_map(keep_terms, label_fun)
      # make_coef_map() is keyed for jtools (names = readable labels, values =
      # raw terms); index it by position in the term list, not by term name,
      # which silently fell back to raw coefficient labels.
      coef_data$label <- names(coef_map)[match(coef_data$term, coef_map)]
      coef_data$label <- ifelse(is.na(coef_data$label), coef_data$term, coef_data$label)
      coef_data$conf.low <- coef_data$Estimate - 1.96 * coef_data$`Std. Error`
      coef_data$conf.high <- coef_data$Estimate + 1.96 * coef_data$`Std. Error`
      coef_data$model <- factor(coef_data$model,
        levels = c("No FE or covariates", "No covariates", lab3)
      )

      # Order y-axis labels: each main effect followed by its interaction(s),
      # in model order. Reversed so the first main appears at the TOP of the plot.
      coef_data$label_wrap <- stringr::str_wrap(coef_data$label, 25)
      protected <- gsub("::", "", coef_data$term, fixed = TRUE)
      parts <- strsplit(protected, ":", fixed = TRUE)
      main_part <- vapply(parts, function(p) {
        hit <- p[grepl(weather_pattern, p)]
        if (length(hit) == 0) p[1] else hit[1]
      }, character(1))
      is_int <- lengths(parts) > 1
      ord <- order(match(main_part, unique(main_part)), is_int)
      label_levels <- unique(coef_data$label_wrap[ord])
      coef_data$label_wrap <- factor(coef_data$label_wrap, levels = rev(label_levels))

      ggplot2::ggplot(
        coef_data,
        ggplot2::aes(
          x      = Estimate,
          y      = label_wrap,
          colour = model,
          shape  = model
        )
      ) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = .wise_zero) +
        ggplot2::geom_pointrange(
          ggplot2::aes(xmin = conf.low, xmax = conf.high),
          position = ggplot2::position_dodge(width = 0.5)
        ) +
        ggplot2::scale_colour_manual(
          # Progressively stronger specification: greys for simpler specs, the
          # categorical lead blue (#0072B2) for the preferred full specification.
          values = c(
            "No FE" = "grey72", "FE" = "grey58",
            setNames("#0072B2", lab3)
          ),
          name = NULL
        ) +
        ggplot2::scale_shape_discrete(name = NULL) +
        ggplot2::labs(
          x = x_label %||% stringr::str_wrap(paste0("Effect on ", outcome_label), 50),
          y = NULL
        ) +
        theme_wise() +
        ggplot2::theme(
          legend.position = "bottom",
          panel.border = ggplot2::element_blank()
        )
    },
    error = function(e) blank_plot(paste0("Coefficient plot error: ", conditionMessage(e)))
  )
  p
}

#' Generate weather effect plots (continuous or binned) with readable labels
#'
#' Builds effect plots for a selected weather predictor from a fitted `fixest`
#' model. For continuous predictors, predictions are computed manually over a
#' grid of `pred_var` (and moderator values when interactions are present).
#' For binned predictors, coefficient paths are plotted by bin, optionally
#' overlaid by moderator levels. When `weather_df` is supplied for binned
#' predictors, the plot caption reports the first configured bin as the omitted
#' reference category.
#'
#' @param fit A native `fixest` model object.
#' @param pred_var Scalar character. Weather predictor name.
#' @param interaction_terms Character vector of interaction term strings.
#' @param is_binned Scalar logical. Whether `pred_var` is binned.
#' @param label_fun Function mapping variable names to human-readable labels.
#' @param engine Scalar character engine key (kept for compatibility).
#' @param selected_weather Optional data frame of selected weather metadata.
#'   Kept for compatibility.
#' @param weather_df Optional data frame used to recover configured bin labels
#'   (via `get_first_bin_label()`), for omitted-reference caption text.
#' @param rif_grid Optional tidy data frame of RIF beta curves (from
#'   \code{fit_model()$rif_grid}). Used only on the RIF branch
#'   (\code{engine = "rif"}).
#' @param mode Scalar character: \code{"auto"} (default) keeps the historical
#'   behaviour; \code{"main"} forces the no-moderator branch even when a
#'   moderator exists (interaction columns are recomputed at sample means, so
#'   the plotted slope is correct); \code{"moderated"} forces the
#'   moderator-overlay branch and returns an informative blank plot when no
#'   moderator is specified for \code{pred_var}.
#' @param is_logistic Scalar logical. When \code{TRUE}, manual predictions on
#'   the continuous paths are transformed to the response scale
#'   (\code{plogis(eta)} with delta-method SEs) instead of plotting raw
#'   log-odds, and the default y-axis label becomes "Predicted poverty
#'   probability".
#' @param x_label Optional scalar character. When non-NULL, replaces the
#'   constructed x-axis title (\code{"<pred_var> (<label>)"}) on continuous
#'   and binned plots; callers pass unit-complete labels.
#' @param y_label Optional scalar character. When non-NULL, replaces the
#'   y-axis title ("Predicted <y>" / "Effect on <y>") in all branches.
#' @param caption Optional scalar character. Continuous paths: replaces the
#'   default marginal-effect note ("Line = marginal effect (95% CI); ...";
#'   the logistic default adds the median-risk household qualifier). Binned
#'   paths: prepended to the omitted-reference note.
#' @param show_rug Scalar logical (default \code{TRUE}). Continuous paths:
#'   rug of the observed \code{pred_var} values along the bottom axis.
#' @param show_mean_ref Scalar logical (default \code{TRUE}). Continuous
#'   paths: dashed vertical reference line at the mean of \code{pred_var}.
#' @param mark_taus Optional numeric vector (RIF branch only). Draws dashed
#'   vertical grey lines at those tau values with small top labels.
#'
#' @return A `ggplot` object. Returns an informative blank plot on error.
#'
#' @export
make_weather_effect_plot <- function(fit, pred_var, interaction_terms, is_binned,
                                     label_fun, engine, selected_weather = NULL,
                                     weather_df = NULL, rif_grid = NULL,
                                     mode = "auto", is_logistic = FALSE,
                                     x_label = NULL, y_label = NULL,
                                     caption = NULL,
                                     show_rug = TRUE, show_mean_ref = TRUE,
                                     mark_taus = NULL,
                                     effect_scale = "model",
                                     profile_eta = NULL) {
  mode <- match.arg(mode, c("auto", "main", "moderated"))
  effect_scale <- match.arg(effect_scale, c("model", "pp", "pp100", "pct"))

  # Moderator level labels: raw 0/1 codes read as developer output, so binary
  # moderators become "<label>: no / <label>: yes".
  modx_level_label <- function(lab, v) {
    v_chr <- as.character(v)
    if (length(v_chr) != 1) v_chr <- v_chr[[1]]
    num <- suppressWarnings(as.numeric(v_chr))
    if (!is.na(num) && num %in% c(0, 1)) {
      paste0(lab, ": ", if (num == 1) "yes" else "no")
    } else if (!is.na(num)) {
      paste0(lab, " = ", round(num, 2))
    } else {
      paste0(lab, " = ", v_chr)
    }
  }
  # Scale transform for binned contrast effects (applied to estimate + CI
  # endpoints together, so the interval stays valid):
  #   "pp"    - logistic link contrasts mapped to percentage points at the
  #             reference profile (monotone map, interval stays honest);
  #   "pp100" - linear-probability contrasts expressed in pp (x 100);
  #   "model" - unchanged (log points / level).
  .apply_effect_scale <- function(df, est_col = "Estimate",
                                  lo_col = "conf.low", hi_col = "conf.high") {
    if (identical(effect_scale, "pp")) {
      if (!is.finite(profile_eta)) {
        return(df)
      }
      pp_at <- function(b) {
        100 * (stats::plogis(profile_eta + b) -
          stats::plogis(profile_eta))
      }
      df[[est_col]] <- pp_at(df[[est_col]])
      df[[lo_col]] <- pp_at(df[[lo_col]])
      df[[hi_col]] <- pp_at(df[[hi_col]])
    } else if (identical(effect_scale, "pp100")) {
      df[[est_col]] <- 100 * df[[est_col]]
      df[[lo_col]] <- 100 * df[[lo_col]]
      df[[hi_col]] <- 100 * df[[hi_col]]
    }
    df
  }

  .t2_bin_label <- function(term, pred_var) {
    term <- as.character(term)[[1]]
    pred_esc <- gsub("([\\[\\]\\(\\)\\^\\$\\.\\*\\+\\?])", "\\\\\\1", pred_var)
    s <- sub(paste0("^", pred_esc, "[\\[\\(]"), "", term)
    if (identical(s, term)) {
      return(term)
    }
    s <- sub("[])]$", "", s)
    parts <- trimws(strsplit(s, ",", fixed = TRUE)[[1]])
    if (length(parts) != 2L || any(!nzchar(parts))) {
      return(term)
    }
    paste0(parts[[1]], "\u2013", parts[[2]])
  }

  tau_layers <- NULL
  if (!is.null(mark_taus)) {
    # The tau marks ride in their own data frame (not annotate()): in
    # facetted plots annotate()'s literal label mapping is not replicated
    # with the facet-expanded data, which breaks the aesthetic-length check.
    tau_df <- data.frame(
      x = mark_taus,
      label = paste0("\u03c4 = ", formatC(mark_taus, format = "f", digits = 1))
    )
    tau_layers <- list(
      ggplot2::geom_vline(
        xintercept = mark_taus,
        linetype = "dashed", colour = .wise_zero
      ),
      ggplot2::geom_text(
        data = tau_df,
        ggplot2::aes(x = x, y = Inf, label = label),
        inherit.aes = FALSE,
        vjust = 1.4, size = 3.2, colour = .wise_slate
      )
    )
  }

  # Design matrix for the linear (non-RIF) effect plot. Prefers the cached
  # copy stashed by fit_model() before slimming (see resolve_model_matrix);
  # falls back to model.frame() only if neither cache nor model.matrix() works.
  mm_of <- function(fit) {
    mm <- resolve_model_matrix(fit)
    if (!is.null(mm)) {
      return(mm)
    }
    tryCatch(stats::model.frame(fit), error = function(e) NULL)
  }

  # --- RIF branch: weather beta curve across quantiles -----------------------
  if (identical(engine, "rif") && !is.null(rif_grid)) {
    return(tryCatch(
      {
        pred_lab <- label_fun(pred_var)

        # Filter rif_grid to model 3, terms containing pred_var
        grid3 <- rif_grid[rif_grid$model == 3L, ]
        pred_esc <- gsub("([\\[\\]\\(\\)\\^\\$\\.\\*\\+\\?])", "\\\\\\1", pred_var)
        mask <- grepl(paste0("\\b", pred_esc, "\\b"), grid3$term)
        if (!any(mask)) {
          return(blank_plot(paste0("No RIF terms found for '", pred_var, "'.")))
        }
        plot_data <- grid3[mask, ]

        taus <- sort(unique(plot_data$tau))
        plot_data$term_label <- vapply(
          plot_data$term, function(t) coef_label(t, label_fun), character(1)
        )

        n_terms <- length(unique(plot_data$term))
        has_int_terms <- any(grepl(":", plot_data$term, fixed = TRUE))
        rif_y_lab <- "Effect size (log points)"
        is_bin_terms <- all(grepl(
          paste0("^", pred_esc, "[\\[\\(]"),
          unique(plot_data$term)
        ))
        if (n_terms > 1 && !has_int_terms && is_bin_terms) {
          # Binned predictor without interactions: one beta(tau) curve per bin,
          # one facet per bin in numeric bin order. (The moderated branch below
          # is for interaction terms and would invent 0/1 moderator levels.)
          # Facet strips show the prettified bin range, ordered numerically.
          bin_lo <- function(tm) {
            s <- sub(paste0("^", pred_esc, "[\\[\\(]"), "", tm)
            suppressWarnings(as.numeric(sub("^([^,]+),.*", "\\1", s)))
          }
          tu <- unique(plot_data$term)
          tu <- tu[order(suppressWarnings(bin_lo(tu)))]
          lab_map <- stats::setNames(
            vapply(tu, function(t) .t2_bin_label(t, pred_var), character(1)), tu
          )
          plot_data$term_label <- factor(plot_data$term,
            levels = tu,
            labels = lab_map
          )
          p <- ggplot2::ggplot(
            plot_data,
            ggplot2::aes(x = tau, y = estimate, ymin = conf.low, ymax = conf.high)
          ) +
            ggplot2::geom_hline(
              yintercept = 0, linetype = "dashed",
              colour = .wise_zero
            ) +
            ggplot2::geom_ribbon(alpha = 0.15, fill = .wise_blue) +
            ggplot2::geom_line(colour = .wise_blue, linewidth = 0.9) +
            ggplot2::geom_point(colour = .wise_blue, size = 2) +
            ggplot2::facet_wrap(~term_label) +
            ggplot2::scale_x_continuous(
              breaks = taus,
              labels = scales::percent_format(1)
            ) +
            ggplot2::labs(
              x       = "Welfare quantile",
              y       = rif_y_lab,
              caption = "Ribbon = 95% CI"
            ) +
            theme_wise(base_size = 14) +
            ggplot2::theme(
              legend.position    = "none",
              panel.border       = ggplot2::element_blank(),
              strip.background   = ggplot2::element_blank()
            )
          if (!is.null(tau_layers)) p <- p + tau_layers
          return(p)
        }
        if (n_terms == 1) {
          # Single term: simple beta curve
          p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = tau, y = estimate)) +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = .wise_zero) +
            ggplot2::geom_ribbon(
              ggplot2::aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.15, fill = .wise_blue
            ) +
            ggplot2::geom_line(colour = .wise_blue, linewidth = 0.9) +
            ggplot2::geom_point(colour = .wise_blue, size = 2.5) +
            ggplot2::scale_x_continuous(breaks = taus, labels = scales::percent_format(1)) +
            ggplot2::labs(
              x = "Welfare quantile",
              y = rif_y_lab,
              caption = "Ribbon = 95% CI"
            ) +
            theme_wise() +
            ggplot2::theme(
              legend.position    = "none",
              panel.border       = ggplot2::element_blank(),
              strip.background   = ggplot2::element_blank()
            )
          if (!is.null(tau_layers)) p <- p + tau_layers
          p
        } else {
          # Multiple terms (main + interactions): evaluate the combined effect
          # at each moderator level so the plot has one line per modx value in
          # a single panel (or per-bin facet for binned predictors), matching
          # the style of the linear-regression moderated effect plot.

          protected <- gsub("::", "", plot_data$term, fixed = TRUE)
          parts <- strsplit(protected, ":", fixed = TRUE)
          weather_pat <- paste0("\\b", pred_esc, "\\b")
          is_int_row <- lengths(parts) > 1
          main_part <- vapply(parts, function(p) {
            hit <- p[grepl(weather_pat, p)]
            if (length(hit) == 0) p[1] else hit[1]
          }, character(1))

          # Identify moderator variable from interaction_terms. Use a word-
          # boundary regex (not fixed substring) so short pred_var names like
          # "r" don't accidentally match terms like "tx:urban" via the "r" in
          # "urban", which would pick the wrong moderator.
          modx_var <- NULL
          modx_lab <- NULL
          if (length(interaction_terms) > 0) {
            pv_pat <- paste0("\\b", pred_esc, "\\b")
            mt <- interaction_terms[grepl(pv_pat, interaction_terms)]
            if (length(mt) > 0) {
              mp <- strsplit(mt[1], ":", fixed = TRUE)[[1]]
              modx_var <- mp[mp != pred_var][1]
              if (!is.na(modx_var) && nzchar(modx_var)) {
                modx_lab <- label_fun(modx_var)
              }
            }
          }

          # Moderator evaluation points: binary 0/1, small set of unique
          # numeric values, or mean +/- sd for continuous.
          modx_vals <- c(0, 1)
          if (!is.null(weather_df) && !is.null(modx_var) &&
            modx_var %in% names(weather_df)) {
            mx <- weather_df[[modx_var]]
            mx <- mx[!is.na(mx)]
            if (length(mx) > 0) {
              if (is.numeric(mx)) {
                u <- sort(unique(mx))
                if (length(u) <= 5) {
                  modx_vals <- u
                } else {
                  m <- mean(mx)
                  s <- stats::sd(mx)
                  modx_vals <- c(m - s, m, m + s)
                }
              } else {
                lvls <- if (is.factor(mx)) {
                  levels(droplevels(mx))
                } else {
                  sort(unique(as.character(mx)))
                }
                num_try <- suppressWarnings(as.numeric(lvls))
                modx_vals <- if (all(!is.na(num_try))) {
                  num_try
                } else {
                  seq_along(lvls) - 1L
                }
              }
            }
          }

          # Pair main rows with their matching interaction rows by bin id + tau.
          plot_data$.bin_id <- main_part
          main_rows <- plot_data[!is_int_row, , drop = FALSE]
          int_rows <- plot_data[is_int_row, , drop = FALSE]

          combined <- do.call(rbind, lapply(modx_vals, function(v) {
            do.call(rbind, lapply(seq_len(nrow(main_rows)), function(j) {
              mr <- main_rows[j, , drop = FALSE]
              ir <- int_rows[int_rows$.bin_id == mr$.bin_id &
                int_rows$tau == mr$tau, , drop = FALSE]
              ie <- if (nrow(ir) > 0) ir$estimate[1] else 0
              ise <- if (nrow(ir) > 0) ir$std.error[1] else 0
              effect <- mr$estimate + v * ie
              se <- sqrt(mr$std.error^2 + v^2 * ise^2)
              data.frame(
                tau = mr$tau,
                bin_id = mr$.bin_id,
                bin_label = coef_label(mr$.bin_id, label_fun),
                modx_val = v,
                estimate = effect,
                std.error = se,
                conf.low = effect - 1.96 * se,
                conf.high = effect + 1.96 * se,
                stringsAsFactors = FALSE
              )
            }))
          }))

          modx_lab_print <- modx_lab %||% (modx_var %||% "moderator")
          combined$modx_label <- vapply(
            combined$modx_val,
            function(v) modx_level_label(modx_lab_print, v),
            character(1)
          )
          combined$modx_label <- factor(
            combined$modx_label,
            levels = unique(combined$modx_label[order(combined$modx_val)])
          )

          if (identical(mode, "main")) {
            # The main relationship plot is the population-level RIF profile.
            # Keep the moderator-specific curves for the heterogeneity plot, but
            # average their estimates at each quantile and weather-bin panel here.
            combined <- combined |>
              dplyr::group_by(.data$tau, .data$bin_id, .data$bin_label) |>
              dplyr::summarise(
                estimate = mean(.data$estimate, na.rm = TRUE),
                std.error = sqrt(mean(.data$std.error^2, na.rm = TRUE)),
                conf.low = mean(.data$conf.low, na.rm = TRUE),
                conf.high = mean(.data$conf.high, na.rm = TRUE),
                .groups = "drop"
              ) |>
              dplyr::mutate(modx_label = "Average across moderator levels")
          }

          # coef_label() is scalar - vectorise over each unique bin id so that
          # multi-bin (binned) predictors produce one facet per bin. Sort by
          # parsed numeric lower bound so negative ranges aren't ordered
          # lexicographically (e.g. -1.2 must come before -0.4).
          bin_ids_raw <- unique(main_rows$.bin_id)
          .bin_lower <- function(b) {
            s <- sub(paste0("^", pred_esc, "[\\[\\(]"), "", b)
            suppressWarnings(as.numeric(sub("^([^,]+),.*", "\\1", s)))
          }
          ord <- order(.bin_lower(bin_ids_raw))
          # NA lowers (non-binned / unparseable terms) keep their first-seen
          # order at the front.
          bin_ids_ordered <- bin_ids_raw[ord]
          bin_levels <- vapply(
            bin_ids_ordered,
            function(b) coef_label(b, label_fun),
            character(1)
          )
          combined$bin_label <- factor(combined$bin_label, levels = bin_levels)
          n_bins <- length(bin_levels)

          p <- ggplot2::ggplot(
            combined,
            ggplot2::aes(
              x = tau, y = estimate,
              colour = modx_label, fill = modx_label
            )
          ) +
            ggplot2::geom_hline(
              yintercept = 0, linetype = "dashed",
              colour = .wise_zero
            ) +
            ggplot2::geom_ribbon(
              ggplot2::aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.15, colour = NA
            ) +
            ggplot2::geom_line(linewidth = 0.9) +
            ggplot2::geom_point(size = 2) +
            ggplot2::scale_x_continuous(
              breaks = taus,
              labels = scales::percent_format(1)
            ) +
            # Legend keys already carry the moderator name ("Urban: no").
            wise_scale_colour_cat(name = NULL) +
            wise_scale_fill_cat(name = NULL) +
            ggplot2::labs(
              x = "Welfare quantile",
              y = rif_y_lab,
              caption = if (identical(mode, "main")) {
                "Line and ribbon average the estimated effect across moderator levels; ribbon = 95% CI (cov(main, interaction) omitted)."
              } else {
                "Ribbon = 95% CI (cov(main, interaction) omitted)"
              }
            ) +
            theme_wise() +
            ggplot2::theme(
              legend.position    = "bottom",
              panel.border       = ggplot2::element_blank(),
              strip.background   = ggplot2::element_blank()
            )

          if (n_bins > 1) {
            p <- p + ggplot2::facet_wrap(~bin_label,
              scales = "free_y",
              ncol = 2
            )
          }
          if (!is.null(tau_layers)) p <- p + tau_layers
          p
        }
      },
      error = function(e) blank_plot(paste0("RIF effect plot error: ", conditionMessage(e)))
    ))
  }

  pred_lab <- label_fun(pred_var)
  pred_x_lab <- x_label %||% paste0(pred_var, " (", pred_lab, ")")
  y_var_name <- tryCatch(
    as.character(stats::formula(fit)[[2]]),
    error = function(e) "outcome"
  )
  y_lab <- label_fun(y_var_name)
  cap_text <- caption %||% (
    if (isTRUE(is_logistic)) {
      paste(
        "Line = marginal effect (95% CI); curved with polynomial terms,",
        "flat for linear ones. pp at the median-risk household."
      )
    } else {
      paste(
        "Line = marginal effect (95% CI); curved with polynomial terms,",
        "flat for linear ones."
      )
    }
  )

  mf <- mm_of(fit)

  # --- Resolve pred columns (exact or binned prefix match) ------------------
  pred_esc <- gsub("([\\[\\]\\(\\)\\^\\$\\.\\*\\+\\?])", "\\\\\\1", pred_var)

  if (!is.null(mf) && pred_var %in% names(mf) && !is_binned) {
    # Continuous: exact column exists
    pred_cols <- pred_var
  } else if (!is.null(mf) && is_binned) {
    # Binned: match columns containing pred_var[
    pred_cols <- grep(paste0("^", pred_esc, "[\\[\\(]"), names(mf), value = TRUE)
  } else {
    pred_cols <- character(0)
  }

  if (!length(pred_cols)) {
    return(blank_plot(paste0("'", pred_var, "' not found in model frame.")))
  }

  # ========================================================================= #
  # BINNED PATH                                                               #
  # ========================================================================= #
  if (is_binned) {
    p <- tryCatch(
      {
        if (!requireNamespace("fixest", quietly = TRUE)) {
          return(blank_plot("Package 'fixest' is required."))
        }

        mm <- mm_of(fit)
        if (is.null(mm)) {
          return(blank_plot("Model matrix unavailable."))
        }
        ct <- .fixest_coeftable(fit)
        ct$term <- rownames(ct)

        bin_cols <- grep(paste0("^", pred_esc, "[\\[\\(]"), names(mm), value = TRUE)
        bin_cols <- bin_cols[!grepl(":", bin_cols)]
        if (length(bin_cols) == 0) {
          return(blank_plot("No binned columns found in model matrix."))
        }

        ct_main <- ct[
          grepl(paste0("^", pred_esc, "[\\[\\(]"), ct$term) & !grepl(":", ct$term),
          c("term", "Estimate", "Std. Error"),
          drop = FALSE
        ]

        # Sort bin columns by parsed numeric lower bound. Alphabetical sort
        # breaks for negative ranges (e.g. "(-0.4," < "(-0.6," < "(-1.2,"
        # lexicographically but the desired numeric order is the reverse).
        .bin_lower <- function(b) {
          s <- sub(paste0("^", pred_esc, "[\\[\\(]"), "", b)
          suppressWarnings(as.numeric(sub("^([^,]+),.*", "\\1", s)))
        }
        bin_cols <- bin_cols[order(.bin_lower(bin_cols))]

        # Omitted note from first bin label in configured weather data
        omitted_note <- NULL
        if (!is.null(weather_df)) {
          first_bin <- get_first_bin_label(weather_df, pred_var)
          if (!is.na(first_bin) && nzchar(first_bin)) {
            omitted_note <- paste0("Omitted reference bin: ", .cut_bin_label(first_bin), " at y = 0.")
          }
        }

        # Build full table with all bins; missing coefficient => omitted reference (0 effect)
        bins_df <- data.frame(term = bin_cols, stringsAsFactors = FALSE)
        bins_df <- dplyr::left_join(bins_df, ct_main, by = "term")
        bins_df$Estimate[is.na(bins_df$Estimate)] <- 0
        bins_df$`Std. Error`[is.na(bins_df$`Std. Error`)] <- 0
        bins_df$bin_index <- seq_len(nrow(bins_df))
        bins_df$bin_label <- vapply(
          bins_df$term, .t2_bin_label, character(1),
          pred_var = pred_var
        )

        # Detect moderator (if any). Use word-boundary regex so short pred_var
        # names (e.g. "r") aren't matched as substrings inside other variable
        # names like "urban" - which would pick the wrong moderator.
        modx_var <- NULL
        modx_lab <- NULL
        if (length(interaction_terms) > 0) {
          pv_pat <- paste0("\\b", pred_esc, "\\b")
          mt <- interaction_terms[grepl(pv_pat, interaction_terms)]
          if (length(mt) > 0) {
            parts <- strsplit(mt[1], ":")[[1]]
            modx_var <- parts[parts != pred_var][1]
            if (!is.na(modx_var) && nzchar(modx_var)) modx_lab <- label_fun(modx_var)
          }
        }

        # No moderator: single line
        if (identical(mode, "main")) {
          modx_var <- NULL
          modx_lab <- NULL
        } else if (identical(mode, "moderated") && is.null(modx_var)) {
          return(blank_plot(paste0("No moderator specified for '", pred_var, "'.")))
        }

        if (is.null(modx_var)) {
          bins_df$conf.low <- bins_df$Estimate - 1.96 * bins_df$`Std. Error`
          bins_df$conf.high <- bins_df$Estimate + 1.96 * bins_df$`Std. Error`
          bins_df <- .apply_effect_scale(bins_df)

          # Figure note: the omitted reference bin (the dashed line at 0) plus
          # any caller caption; no in-plot title (the section heading covers it).
          cap_binned <- paste(c(omitted_note, caption), collapse = " ")
          cap_binned <- if (is.null(cap_binned) || !nzchar(cap_binned)) {
            NULL
          } else {
            cap_binned
          }

          return(
            ggplot2::ggplot(
              bins_df,
              ggplot2::aes(x = bin_index, y = Estimate, ymin = conf.low, ymax = conf.high)
            ) +
              ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = .wise_zero) +
              ggplot2::geom_pointrange(colour = .wise_blue, size = 0.65) +
              ggplot2::geom_line(ggplot2::aes(group = 1), colour = .wise_blue, linewidth = 0.6) +
              ggplot2::scale_x_continuous(breaks = bins_df$bin_index, labels = bins_df$bin_label) +
              ggplot2::labs(
                x = pred_x_lab,
                y = y_label %||% paste("Effect on", y_lab),
                caption = cap_binned
              ) +
              theme_wise()
          )
        }

        # Moderator present: overlay lines (same plot, different colors)
        ct_int <- ct[
          grepl(paste0(pred_esc, "[\\[\\(]"), ct$term) &
            grepl(":", ct$term) &
            grepl(modx_var, ct$term, fixed = TRUE),
          c("term", "Estimate", "Std. Error"),
          drop = FALSE
        ]
        int_est <- stats::setNames(ct_int$Estimate, ct_int$term)
        int_se <- stats::setNames(ct_int$`Std. Error`, ct_int$term)

        # Moderator values
        if (modx_var %in% names(mm)) {
          modx_vals <- sort(unique(mm[[modx_var]]))
          if (length(modx_vals) > 5 && is.numeric(modx_vals)) {
            mu <- mean(mm[[modx_var]], na.rm = TRUE)
            sd <- stats::sd(mm[[modx_var]], na.rm = TRUE)
            modx_vals <- c(mu - sd, mu, mu + sd)
          }
        } else {
          modx_vals <- c(0, 1)
        }

        # Build predictions/effects for every bin x moderator value
        plot_df <- do.call(
          rbind,
          lapply(seq_len(nrow(bins_df)), function(i) {
            bin_term <- bins_df$term[i]
            b0 <- bins_df$Estimate[i]
            v0 <- bins_df$`Std. Error`[i]^2

            t1 <- paste0(bin_term, ":", modx_var)
            t2 <- paste0(modx_var, ":", bin_term)
            iterm <- if (t1 %in% names(int_est)) t1 else if (t2 %in% names(int_est)) t2 else NA_character_
            has_int <- !is.na(iterm)

            do.call(rbind, lapply(modx_vals, function(mv) {
              b <- b0 + if (has_int) int_est[[iterm]] * mv else 0
              s <- sqrt(v0 + if (has_int) (mv^2) * int_se[[iterm]]^2 else 0)
              data.frame(
                bin_index = bins_df$bin_index[i],
                bin_label = bins_df$bin_label[i],
                est = b,
                conf.low = b - 1.96 * s,
                conf.high = b + 1.96 * s,
                modx = as.character(mv),
                stringsAsFactors = FALSE
              )
            }))
          })
        )

        plot_df <- plot_df[order(plot_df$modx, plot_df$bin_index), , drop = FALSE]
        plot_df <- .apply_effect_scale(plot_df, est_col = "est")
        modx_u <- sort(unique(plot_df$modx))
        plot_df$modx <- factor(
          plot_df$modx,
          levels = modx_u,
          labels = vapply(
            modx_u, function(v) modx_level_label(modx_lab, v),
            character(1)
          )
        )

        # Legend keys already carry the moderator name ("Urban: no"); repeating
        # it as the legend title is redundant.
        cap_binned <- paste(c(omitted_note, caption), collapse = " ")
        cap_binned <- if (is.null(cap_binned) || !nzchar(cap_binned)) {
          NULL
        } else {
          cap_binned
        }

        ggplot2::ggplot(
          plot_df,
          ggplot2::aes(
            x = bin_index, y = est, ymin = conf.low, ymax = conf.high,
            colour = modx, group = modx
          )
        ) +
          ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = .wise_zero) +
          ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 0.2), size = 0.5) +
          ggplot2::geom_line(position = ggplot2::position_dodge(width = 0.2), linewidth = 0.6) +
          wise_scale_colour_cat(name = NULL) +
          ggplot2::scale_x_continuous(breaks = bins_df$bin_index, labels = bins_df$bin_label) +
          ggplot2::labs(
            x = pred_x_lab,
            y = y_label %||% paste("Effect on", y_lab),
            caption = cap_binned
          ) +
          theme_wise() +
          ggplot2::theme(legend.position = "bottom")
      },
      error = function(e) blank_plot(paste0("Binned effect plot error: ", conditionMessage(e)))
    )

    return(p)
  }

  # ========================================================================= #
  # CONTINUOUS PATH: marginal effect of weather vs weather level               #
  # ========================================================================= #
  if (!is_binned) {
    pred_vals <- mf[[pred_var]]

    if (!any(is.finite(pred_vals))) {
      return(blank_plot(paste0("No finite values for '", pred_var, "' - cannot build effect plot.")))
    }

    if (!requireNamespace("fixest", quietly = TRUE)) {
      return(blank_plot("Package 'fixest' is required."))
    }

    # Resolve moderator
    modx_var <- NULL
    modx_lab <- NULL
    if (length(interaction_terms) > 0) {
      match_term <- grep(paste0("^", pred_var, ":"), interaction_terms, value = TRUE)
      if (length(match_term) > 0) {
        modx_var <- strsplit(match_term[1], ":")[[1]][2]
        modx_lab <- label_fun(modx_var)
      }
    }

    if (identical(mode, "main")) {
      modx_var <- NULL
      modx_lab <- NULL
    } else if (identical(mode, "moderated") &&
      (is.null(modx_var) || !modx_var %in% names(mf))) {
      return(blank_plot(paste0("No moderator specified for '", pred_var, "'.")))
    }

    p <- tryCatch(
      {
        mm <- mm_of(fit)
        if (is.null(mm)) {
          return(blank_plot("Model matrix unavailable."))
        }
        betas <- stats::coef(fit)
        vcov_m <- .fixest_vcov(fit)
        n_grid <- 100L

        # --- Marginal effect: d(response)/dx evaluated along the grid -------
        # d/dx [b1*x + b2*I(x^2) + b3*I(x^3) + b_int*(x^k)*modx] =
        #   b1 + 2*b2*x + 3*b3*x^2 + k*b_k*x^(k-1)*modx. Polynomial terms are
        #   matched against fixest's double-wrapped "I(I(x^2))" spelling as
        #   well as the plain one. This replaces the old centred prediction
        #   curve, which froze polynomial columns at their sample means (flat
        #   lines) and read as a nonsensical "predicted welfare" level.
        grad_w <- function(nm, x, mv) {
          if (identical(nm, pred_var)) {
            return(1)
          }
          if (.s1_is_poly_term(nm, pred_var, 2)) {
            return(2 * x)
          }
          if (.s1_is_poly_term(nm, pred_var, 3)) {
            return(3 * x^2)
          }
          if (grepl(":", nm, fixed = TRUE)) {
            parts <- strsplit(nm, ":", fixed = TRUE)[[1]]
            w <- 1
            has_x <- FALSE
            for (pp in parts) {
              if (identical(pp, pred_var)) {
                has_x <- TRUE
              } else if (.s1_is_poly_term(pp, pred_var, 2)) {
                w <- w * (2 * x)
                has_x <- TRUE
              } else if (.s1_is_poly_term(pp, pred_var, 3)) {
                w <- w * (3 * x^2)
                has_x <- TRUE
              } else {
                w <- w * mv
              }
            }
            return(if (has_x) w else 0)
          }
          0
        }

        slope_grid <- function(mv) {
          W <- matrix(0,
            nrow = length(x_seq), ncol = length(betas),
            dimnames = list(NULL, names(betas))
          )
          for (nm in colnames(mm)) {
            if (!nm %in% colnames(W)) next
            W[, nm] <- vapply(x_seq, function(xx) grad_w(nm, xx, mv), numeric(1))
          }
          ok <- !is.na(betas)
          est <- as.numeric(W[, ok, drop = FALSE] %*% betas[ok])
          se <- sqrt(pmax(0, rowSums((W[, ok, drop = FALSE] %*%
            vcov_m[ok, ok, drop = FALSE]) * W[, ok, drop = FALSE])))
          data.frame(x = x_seq, est = est, se = se)
        }

        # Reporting-scale transform of the slope and its CI:
        #   "pct"   - log outcome: % change per +1 unit = 100*(exp(slope)-1)
        #   "pp"    - logit: dp/dx = p(1-p)*slope at the reference profile, in pp
        #   "pp100" - linear probability: slope in probability units -> pp
        #   "model" - raw slope (level outcomes)
        .slope_scale <- function(d) {
          if (identical(effect_scale, "pct")) {
            f <- function(v) 100 * (exp(v) - 1)
            data.frame(
              x = d$x, fit = f(d$est),
              lo = f(d$est - 1.96 * d$se), hi = f(d$est + 1.96 * d$se)
            )
          } else if (identical(effect_scale, "pp")) {
            if (is.finite(profile_eta)) {
              f <- 100 * stats::plogis(profile_eta) * (1 - stats::plogis(profile_eta))
            } else {
              f <- 1
            }
            data.frame(
              x = d$x, fit = f * d$est,
              lo = f * (d$est - 1.96 * d$se), hi = f * (d$est + 1.96 * d$se)
            )
          } else if (identical(effect_scale, "pp100")) {
            data.frame(
              x = d$x, fit = 100 * d$est,
              lo = 100 * (d$est - 1.96 * d$se), hi = 100 * (d$est + 1.96 * d$se)
            )
          } else {
            data.frame(
              x = d$x, fit = d$est,
              lo = d$est - 1.96 * d$se, hi = d$est + 1.96 * d$se
            )
          }
        }

        mean_x <- mean(mm[[pred_var]], na.rm = TRUE)
        extra_layers <- list()
        if (isTRUE(show_mean_ref) && is.finite(mean_x)) {
          extra_layers <- c(extra_layers, list(
            ggplot2::geom_vline(
              xintercept = mean_x, colour = .wise_slate,
              linetype = "dashed"
            ),
            ggplot2::annotate("text",
              x = mean_x, y = Inf, label = "mean",
              vjust = 1.4, size = 3.2, colour = .wise_slate
            )
          ))
        }
        if (isTRUE(show_rug)) {
          rug_x <- mm[[pred_var]]
          rug_x <- rug_x[is.finite(rug_x)]
          if (length(rug_x) > 0) {
            extra_layers <- c(extra_layers, list(
              ggplot2::geom_rug(
                data = data.frame(x = rug_x),
                ggplot2::aes(x = x),
                sides = "b", alpha = 0.12, colour = .wise_slate,
                inherit.aes = FALSE
              )
            ))
          }
        }

        x_seq <- seq(min(mm[[pred_var]], na.rm = TRUE),
          max(mm[[pred_var]], na.rm = TRUE),
          length.out = n_grid
        )

        if (!is.null(modx_var) && modx_var %in% names(mm)) {
          modx_col <- mm[[modx_var]]
          modx_uniq <- sort(unique(modx_col))
          is_cat_modx <- is.factor(modx_col) || is.character(modx_col) ||
            length(modx_uniq) <= 5

          modx_vals <- if (is_cat_modx) {
            modx_uniq
          } else {
            modx_mean <- mean(modx_col, na.rm = TRUE)
            modx_sd <- stats::sd(modx_col, na.rm = TRUE)
            c(modx_mean - modx_sd, modx_mean, modx_mean + modx_sd)
          }

          plot_df <- do.call(rbind, lapply(modx_vals, function(mv) {
            d <- .slope_scale(slope_grid(mv))
            d$.modx_label <- modx_level_label(modx_lab, mv)
            d
          }))
          plot_df$.modx_label <- factor(
            plot_df$.modx_label,
            levels = vapply(
              modx_vals, function(v) modx_level_label(modx_lab, v),
              character(1)
            )
          )

          p <- ggplot2::ggplot(
            plot_df,
            ggplot2::aes(
              x      = x,
              y      = fit,
              colour = .data$.modx_label,
              fill   = .data$.modx_label
            )
          ) +
            ggplot2::geom_hline(
              yintercept = 0, linetype = "dashed",
              colour = .wise_zero
            ) +
            ggplot2::geom_ribbon(
              ggplot2::aes(ymin = lo, ymax = hi),
              alpha = 0.15, colour = NA
            ) +
            ggplot2::geom_line(linewidth = 0.9) +
            # Legend keys already carry the moderator name ("Urban: no").
            wise_scale_colour_cat(name = NULL) +
            wise_scale_fill_cat(name = NULL) +
            ggplot2::labs(
              x = pred_x_lab,
              y = y_label %||% paste("Change in", y_lab, "per +1 unit"),
              caption = cap_text
            ) +
            theme_wise() +
            ggplot2::theme(legend.position = "bottom")
          if (length(extra_layers)) p <- p + extra_layers
          p
        } else {
          d <- .slope_scale(slope_grid(0))

          p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = fit)) +
            ggplot2::geom_hline(
              yintercept = 0, linetype = "dashed",
              colour = .wise_zero
            ) +
            ggplot2::geom_ribbon(
              ggplot2::aes(ymin = lo, ymax = hi),
              alpha = 0.2, fill = .wise_blue
            ) +
            ggplot2::geom_line(colour = .wise_blue, linewidth = 0.9) +
            ggplot2::labs(
              x = pred_x_lab,
              y = y_label %||% paste("Change in", y_lab, "per +1 unit"),
              caption = cap_text
            ) +
            theme_wise()
          if (length(extra_layers)) p <- p + extra_layers
          p
        }
      },
      error = function(e) blank_plot(paste0("fixest effect plot error: ", conditionMessage(e)))
    )
    return(p)
  }
}


#' Tidy regression results behind the Step 1 coefficient table
#'
#' Returns the estimates behind the coefficient table as one long data frame
#' so the "Download CSV" link under the table hands over usable numbers.
#'
#' @param fit1,fit2,fit3 fixest model objects (ignored on the RIF path).
#' @param engine    Estimation engine; `"rif"` reads `rif_grid` instead.
#' @param rif_grid  RIF coefficient grid (term, tau, model, estimate, ...).
#' @param label_fun Function mapping variable names to human labels.
#'
#' @return A data.frame, or NULL when nothing can be extracted.
#' @noRd
make_regtable_df <- function(fit1, fit2, fit3,
                             engine = "fixest",
                             rif_grid = NULL,
                             label_fun = identity) {
  # --- RIF: one row per (term, quantile) from the full specification --------
  if (identical(engine, "rif") && !is.null(rif_grid)) {
    g <- rif_grid[rif_grid$model == 3L, , drop = FALSE]
    if (nrow(g) == 0) {
      return(NULL)
    }
    keep <- intersect(
      c("term", "tau", "estimate", "std.error", "statistic", "p.value"),
      names(g)
    )
    out <- g[order(g$term, g$tau), keep, drop = FALSE]
    out$term <- vapply(as.character(out$term), function(x) {
      lbl <- tryCatch(label_fun(x), error = function(e) x)
      if (length(lbl) == 1 && !is.na(lbl) && nzchar(lbl)) lbl else x
    }, character(1))
    # Same column names as the fixest branch below, so both engines export a
    # CSV with the same header.
    rename <- c(
      term = "Variable", tau = "Quantile", estimate = "Estimate",
      std.error = "Std. error", statistic = "Statistic",
      p.value = "p value"
    )
    hit <- names(out) %in% names(rename)
    names(out)[hit] <- unname(rename[names(out)[hit]])
    out$Model <- "(3) FE + Controls"
    front <- intersect(c("Model", "Variable", "Quantile"), names(out))
    return(out[, c(front, setdiff(names(out), front)), drop = FALSE])
  }

  # --- fixest: one row per (specification, term) ----------------------------
  specs <- list(
    "(1) No FE"           = fit1,
    "(2) FE"              = fit2,
    "(3) FE + Controls"   = fit3
  )
  rows <- lapply(names(specs), function(nm) {
    fit <- specs[[nm]]
    if (!inherits(fit, "fixest")) {
      return(NULL)
    }
    ct <- tryCatch(.fixest_coeftable(fit), error = function(e) NULL)
    if (is.null(ct) || nrow(ct) == 0) {
      return(NULL)
    }
    terms <- rownames(ct)
    data.frame(
      Model = nm,
      Variable = vapply(terms, function(x) {
        lbl <- tryCatch(label_fun(x), error = function(e) x)
        if (length(lbl) == 1 && !is.na(lbl) && nzchar(lbl)) lbl else x
      }, character(1)),
      Term = terms,
      Estimate = unname(ct[, 1]),
      `Std. error` = unname(ct[, 2]),
      `p value` = unname(ct[, 4]),
      Observations = tryCatch(stats::nobs(fit), error = function(e) NA_integer_),
      check.names = FALSE,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(NULL)
  }
  do.call(rbind, rows)
}


# Model fit diagnostic plots ----

# Clean label for a cut()-style bin level: "[22.1, 26.6]" or "t(26.6, 28.4]"
# -> "22.1 – 26.6". Falls back to the trimmed input for odd shapes.
.cut_bin_label <- function(lvl) {
  s <- trimws(lvl)
  if (startsWith(s, "t(") || startsWith(s, "t[")) s <- substr(s, 2, nchar(s))
  if (startsWith(s, "[") || startsWith(s, "(")) s <- substr(s, 2, nchar(s))
  if (endsWith(s, "]") || endsWith(s, ")")) s <- substr(s, 1, nchar(s) - 1)
  parts <- strsplit(s, ",", fixed = TRUE)[[1]]
  if (length(parts) == 2) {
    paste0(trimws(parts[1]), " \u2013 ", trimws(parts[2]))
  } else {
    trimws(s)
  }
}


#' Plot calibration curve for a binary model
#'
#' Groups observations into deciles (rank-based bins, robust to ties) of
#' predicted risk and plots the observed outcome rate against the mean
#' predicted risk per bin, with a diagonal reference and a +/- 2-SE binomial
#' band. Bins close to the diagonal indicate calibrated predictions.
#'
#' @param model  A fitted binary model (e.g. `glm`/`fixest` feglm with
#'   `family = binomial`) for which `fitted()` returns predicted probabilities.
#' @param n_bins Scalar integer, maximum number of bins (fewer when the
#'   sample is small; at least 3).
#'
#' @return A `ggplot` object.
#'
#' @export
plot_calibration <- function(model, n_bins = 10) {
  predicted <- tryCatch(
    stats::fitted(model),
    error = function(e) {
      tryCatch(stats::predict(model, type = "response"),
        error = function(e2) NULL
      )
    }
  )
  # stats::model.frame() errors on fixest objects; recover actual = fitted +
  # residuals (response residuals of a binomial model reproduce the outcome).
  actual <- tryCatch(stats::model.frame(model)[[1]],
    error = function(e) {
      f <- tryCatch(stats::fitted(model),
        error = function(e2) NULL
      )
      r <- tryCatch(stats::residuals(model, type = "response"),
        error = function(e2) NULL
      )
      if (is.null(f) || is.null(r)) NULL else f + r
    }
  )

  if (is.null(predicted) || is.null(actual)) {
    return(blank_plot("Could not recover fitted values from model."))
  }

  n <- min(length(actual), length(predicted))
  k <- max(3L, min(as.integer(n_bins), floor(n / 20)))
  ord <- order(as.numeric(predicted[seq_len(n)]))
  brks <- unique(floor(seq(0, n, length.out = k + 1)))
  if (length(brks) < 3) {
    return(blank_plot("Too few observations for calibration bins."))
  }
  grp <- cut(seq_len(n), breaks = brks, include.lowest = TRUE)

  bdf <- data.frame(
    pred = as.numeric(predicted[seq_len(n)])[ord],
    obs = as.numeric(actual)[ord],
    grp = grp,
    stringsAsFactors = FALSE
  )
  cal <- stats::aggregate(cbind(pred, obs) ~ grp, data = bdf, FUN = mean)
  names(cal) <- c("grp", "pred", "obs")
  cnt <- as.data.frame(table(bdf$grp))
  cal$n <- cnt$Freq[match(as.character(cal$grp), as.character(cnt$Var1))]
  cal$se <- sqrt(pmax(cal$obs * (1 - cal$obs), 0) / pmax(cal$n, 1))

  ggplot2::ggplot(cal, ggplot2::aes(x = .data$pred, y = .data$obs)) +
    ggplot2::geom_abline(
      slope = 1, intercept = 0,
      color = .wise_zero, linetype = "dashed"
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = pmax(0, .data$obs - 2 * .data$se),
        ymax = pmin(1, .data$obs + 2 * .data$se)
      ),
      fill = .wise_blue, alpha = 0.15
    ) +
    ggplot2::geom_line(color = .wise_blue, linewidth = 0.6) +
    ggplot2::geom_point(color = .wise_blue, size = 2) +
    theme_wise() +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(
      subtitle = "Observed vs predicted rate by decile of predicted risk",
      x = "Predicted risk (bin mean)",
      y = "Observed rate in bin"
    )
}


#' Plot predicted vs actual distribution
#'
#' For linear models: overlaid histogram of actual vs predicted values.
#' For logistic models: calibration curve (observed vs predicted rate by
#' decile of predicted risk) instead of a threshold-dependent confusion matrix.
#'
#' @param model        A native `lm`/`glm` object.
#' @param is_logistic  Scalar logical.
#' @param outcome_label Scalar character label for the x-axis (linear only).
#'
#' @return A `ggplot` object.
#'
#' @export
plot_pred_vs_actual <- function(model, is_logistic, outcome_label = "outcome") {
  # stats::model.frame()[[1]] throws 'subscript out of bounds' on fixest objects.
  # Recover actual = fitted + residuals, which works for both lm and fixest.
  actual <- tryCatch(
    stats::model.frame(model)[[1]],
    error = function(e) {
      f <- tryCatch(stats::fitted(model), error = function(e) NULL)
      r <- tryCatch(stats::residuals(model), error = function(e) NULL)
      if (!is.null(f) && !is.null(r)) f + r else NULL
    }
  )

  if (is.null(actual)) {
    return(blank_plot("Could not recover outcome values from model."))
  }

  if (!is_logistic) {
    predicted <- tryCatch(stats::fitted(model), error = function(e) stats::predict(model))
    n <- min(length(actual), length(predicted))
    plot_data <- data.frame(
      Type   = rep(c("Actual", "Predicted"), each = n),
      Values = c(actual[seq_len(n)], predicted[seq_len(n)])
    )
    ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$Values, fill = .data$Type)) +
      ggplot2::geom_histogram(
        ggplot2::aes(y = 100 * ggplot2::after_stat(count) / sum(ggplot2::after_stat(count))),
        position = "dodge", alpha = 0.7, bins = 30
      ) +
      ggplot2::scale_fill_manual(
        # Observed data in the neutral slate; model output in brand blue.
        values = c("Actual" = .wise_slate, "Predicted" = .wise_blue)
      ) +
      ggplot2::labs(
        x = stringr::str_wrap(outcome_label, 40),
        y = "Share of households (%)"
      ) +
      theme_wise() +
      ggplot2::theme(legend.title = ggplot2::element_blank())
  } else {
    plot_calibration(model)
  }
}


#' Compute model fit statistics as a data frame
#'
#' Returns a two-column data frame (`Statistic`, `Value`) using
#' `fixest::fitstat()` / `fixest::r2()`. R-squared style statistics use the
#' same formatting as the At-a-glance fit snippet (`<0.01` below 0.005,
#' an em dash when unavailable) so the two never appear to disagree.
#'
#' @param model       A native fixest model object (or a list / fixest_multi
#'   of per-quantile models for the RIF engine).
#' @param is_logistic Scalar logical.
#' @param engine      Scalar character engine key (`"rif"` switches to the
#'   per-quantile table).
#' @param taus        Optional numeric vector of RIF quantiles; defaults to
#'   `seq(0.1, 0.9, by = 0.1)` when `engine = "rif"`.
#'
#' @return A data frame with columns `Statistic` and `Value` (RIF: one row
#'   per quantile with columns `Quantile`, `N`, `R²`, `Within R²`).
#'
#' @export
calc_fit_stats <- function(model, is_logistic, engine = "fixest", taus = NULL) {
  fmt_r2 <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (!is.finite(x)) {
      "\u2014"
    } else if (x < 0.005) {
      "<0.01"
    } else {
      sprintf("%.2f", x)
    }
  }
  fmt_int <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (!is.finite(x)) "\u2014" else formatC(x, format = "f", digits = 0, big.mark = ",")
  }

  # RIF: per-quantile R^2 table
  if (identical(engine, "rif") && (inherits(model, "fixest_multi") || is.list(model))) {
    taus <- taus %||% seq(0.1, 0.9, by = 0.1)
    n <- min(length(model), length(taus))
    rows <- lapply(seq_len(n), function(i) {
      m <- model[[i]]
      row_i <- data.frame(
        Quantile = paste0("\u03c4 = ", sprintf("%g", taus[i])),
        N = tryCatch(fmt_int(stats::nobs(m)), error = function(e) "\u2014"),
        R2 = tryCatch(fmt_r2(fixest::r2(m, "r2")), error = function(e) "\u2014"),
        stringsAsFactors = FALSE
      )
      row_i[["Within R\u00b2"]] <- tryCatch(fmt_r2(fixest::r2(m, "wr2")),
        error = function(e) "\u2014"
      )
      names(row_i) <- c("Quantile", "N", "R\u00b2", "Within R\u00b2")
      row_i
    })
    return(do.call(rbind, rows))
  }

  nobs_val <- tryCatch(fmt_int(stats::nobs(model)), error = function(e) "\u2014")

  if (!is_logistic) {
    r2_val <- tryCatch(fmt_r2(fixest::r2(model, "r2")), error = function(e) "\u2014")
    ar2_val <- tryCatch(fmt_r2(fixest::r2(model, "ar2")), error = function(e) "\u2014")
    wr2_val <- tryCatch(fmt_r2(fixest::r2(model, "wr2")), error = function(e) "\u2014")
    data.frame(
      Statistic = c("Observations", "R\u00b2", "Adj. R\u00b2", "Within R\u00b2"),
      Value = c(nobs_val, r2_val, ar2_val, wr2_val),
      stringsAsFactors = FALSE
    )
  } else {
    aic_val <- tryCatch(fmt_int(stats::AIC(model)), error = function(e) "\u2014")
    pr2_val <- tryCatch(fmt_r2(fixest::r2(model, "pr2")), error = function(e) {
      # fixest::r2() only knows fixest models; fall back to McFadden from
      # the log-likelihoods so stats::glm fits are covered too.
      ll <- tryCatch(as.numeric(stats::logLik(model)), error = function(e2) NA_real_)
      ll0 <- tryCatch(
        {
          y <- stats::model.response(stats::model.frame(model))
          p0 <- mean(y)
          if (p0 <= 0 || p0 >= 1) {
            NA_real_
          } else {
            sum(y * log(p0) + (1 - y) * log(1 - p0))
          }
        },
        error = function(e2) NA_real_
      )
      fmt_r2(1 - ll / ll0)
    })
    data.frame(
      Statistic = c("Observations", "McFadden R\u00b2", "AIC"),
      Value = c(nobs_val, pr2_val, aic_val),
      stringsAsFactors = FALSE
    )
  }
}


#' Plot each term's approximate contribution to explained variation
#'
#' Standardized-coefficient decomposition: each term's squared standardized
#' coefficient (|beta| * sd(x))^2 expressed as a share of their sum, as an
#' indicative ranking of how much each term contributes to the model's fit.
#' Collinearity is ignored and fixed effects are excluded, so shares are
#' approximate.
#'
#' @param model     A native fitted model (`fixest` feols/feglm, incl. a single
#'   RIF quantile model).
#' @param label_fun Optional function mapping term names to readable labels
#'   (polynomial terms are prettified automatically).
#'
#' @return A `ggplot` object.
#'
#' @export
plot_importance <- function(model, label_fun = identity) {
  mm <- resolve_model_matrix(model)
  if (is.null(mm)) {
    return(blank_plot("Model matrix unavailable."))
  }

  coefs <- stats::coef(model)
  keep <- names(coefs) != "(Intercept)" & names(coefs) %in% names(mm)
  beta <- coefs[keep]
  if (!length(beta)) {
    return(blank_plot("No estimable terms."))
  }

  X <- mm[, names(beta), drop = FALSE]
  sd_x <- apply(X, 2, stats::sd, na.rm = TRUE)
  sd_x[is.na(sd_x)] <- 0

  imp <- abs(as.numeric(beta)) * as.numeric(sd_x)
  tot <- sum(imp^2)
  if (!is.finite(tot) || tot <= 0) {
    return(blank_plot("No variation to decompose."))
  }

  df <- data.frame(
    term = names(beta),
    share = 100 * imp^2 / tot,
    stringsAsFactors = FALSE
  )
  coef_map <- make_coef_map(df$term, label_fun)
  df$label <- unname(names(coef_map)[match(df$term, unname(coef_map))])
  df$label[is.na(df$label) | !nzchar(df$label)] <- df$term[is.na(df$label) | !nzchar(df$label)]
  df <- df[order(-df$share), , drop = FALSE]
  df <- utils::head(df, 15)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$share, y = stats::reorder(.data$label, .data$share))) +
    ggplot2::geom_col(fill = .wise_blue, width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.0f%%", .data$share)),
      hjust = -0.15, size = 3.2, colour = .wise_charcoal
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()) +
    ggplot2::labs(
      subtitle = "Squared standardized coefficients, as a share of their sum",
      x = "Share of explained variation (%)",
      y = ""
    ) +
    theme_wise()
}


# Echarts counterparts of the Results / Model fit figures (guidelines §7) ----
# The ggplot builders above stay the canonical static renderers (tests and the
# export fallback use them); these draw the same statistics, computed with the
# same parameters, as interactive echarts4r widgets.

# Transparent polygon band (lo..hi) plus an estimate line: the echarts idiom
# replacing ggplot's geom_ribbon. Returns three series (dummy, polygon band, line)
# to preserve 3-series index compatibility for all callers.
.e_ribbon_series <- function(nm, x, est, lo, hi, fill, line,
                             line_width = 2, show_points = FALSE,
                             point_size = 5, z = 2) {
  stopifnot(length(x) == length(est), length(lo) == length(x),
            length(hi) == length(x))
  ok <- is.finite(x) & is.finite(est) & is.finite(lo) & is.finite(hi)
  x <- x[ok]; est <- est[ok]; lo <- lo[ok]; hi <- hi[ok]
  if (!length(x)) {
    return(NULL)
  }
  ord <- order(x)
  x <- x[ord]; est <- est[ord]; lo <- lo[ord]; hi <- hi[ord]

  js_ribbon <- htmlwidgets::JS("function(params, api) {
    if (params.dataIndex !== 0) return;
    var count = params.dataInsideLength || 0;
    if (!count) return;
    var pts = [];
    for (var i = 0; i < count; i++) {
      pts.push(api.coord([api.value(0, i), api.value(2, i)]));
    }
    for (var i = count - 1; i >= 0; i--) {
      pts.push(api.coord([api.value(0, i), api.value(3, i)]));
    }
    return {
      type: 'polygon',
      shape: { points: pts },
      style: { fill: api.visual('color'), opacity: 0.18 }
    };
  }")

  band_data <- lapply(seq_along(x), function(i) list(x[i], est[i], hi[i], lo[i]))
  line_data <- lapply(seq_along(x), function(i) list(
    value = list(x[i], est[i]), confLow = lo[i], confHigh = hi[i]
  ))

  list(
    list(
      name = nm, type = "line", data = list(), symbol = "none", silent = TRUE, z = z,
      lineStyle = list(opacity = 0), itemStyle = list(color = fill, opacity = 1),
      tooltip = list(show = FALSE), legendHoverLink = FALSE
    ),
    list(
      name = nm, type = "custom",
      renderItem = js_ribbon,
      data = band_data,
      itemStyle = list(color = fill),
      areaStyle = list(color = fill, opacity = 0.18),
      silent = TRUE, z = z,
      tooltip = list(show = FALSE), legendHoverLink = FALSE
    ),
    list(
      name = nm, type = "line", data = line_data, symbol = if (show_points) "circle" else "none", z = z + 1,
      lineStyle = list(color = line, width = line_width),
      itemStyle = list(color = line),
      symbolSize = if (show_points) point_size else 1,
      showSymbol = show_points
    )
  )
}

.e_effect_tooltip <- function(percent = FALSE, trigger = "axis", label_prefix = NULL) {
  fmt <- if (percent) {
    "function(v){var n=Number(v); return isFinite(n) ? n.toLocaleString('en-US',{minimumFractionDigits:1,maximumFractionDigits:1})+'%' : '';}"
  } else {
    "function(v){var n=Number(v); return isFinite(n) ? n.toLocaleString('en-US',{minimumFractionDigits:2,maximumFractionDigits:2}) : '';}"
  }
  prefix <- if (is.null(label_prefix)) {
    "p.seriesName||'Effect'"
  } else {
    paste0("'", gsub("'", "\\\\'", label_prefix), " ' + (p.seriesName||'Effect')")
  }
  html <- sprintf(
    "function(params){var rows=[];(params||[]).forEach(function(p){var d=p.data||{};var v=Array.isArray(p.value)?p.value[1]:p.value;var lo=d.confLow,hi=d.confHigh;var text=(%s)+': <b>'+((%s)(v))+'</b>';if(isFinite(Number(lo))&&isFinite(Number(hi)))text+='<br/>95%% CI: ['+(%s)(lo)+', '+(%s)(hi)+']';rows.push(text);});return rows.join('<br/>');}",
    prefix, fmt, fmt, fmt
  )
  list(trigger = trigger, formatter = htmlwidgets::JS(html))
}

#' Echarts coefficient plot across three progressive model fits
#'
#' Interactive coefficient stability plot (guidelines §7), with 95% CI
#' whiskers, wrapped labels and specification colours. RIF coefficients are
#' filtered to one selected welfare quantile.
#'
#' @inheritParams make_coefplot
#' @param height Widget height; a CSS length or a number of pixels.
#' @param tau Scalar quantile used for RIF coefficient stability plots.
#'
#' @return An `echarts4r` widget, or `NULL`.
#'
#' @export
echart_make_coefplot <- function(fit1, fit2, fit3,
                                 weather_terms,
                                 interaction_terms,
                                 outcome_label = "outcome",
                                 label_fun = identity,
                                 engine = "fixest",
                                 rif_grid = NULL,
                                 pred_var = NULL,
                                 x_label = NULL,
                                 has_controls = TRUE,
                                 height = "600px",
                                 tau = NULL,
                                 train_data = NULL) {
  lab3 <- "FE + covariates"
  if (identical(engine, "rif")) {
    if (is.null(rif_grid) || is.null(pred_var)) {
      return(echart_blank("No RIF coefficients found to plot.", height = height))
    }
    term_has_predictor <- function(term) {
      parts <- strsplit(as.character(term), ":", fixed = TRUE)[[1L]]
      any(vapply(parts, function(part) {
        tokens <- regmatches(part, gregexpr("[[:alnum:]_.]+", part))[[1L]]
        pred_var %in% tokens
      }, logical(1)))
    }
    term_match <- vapply(rif_grid$term, term_has_predictor, logical(1))
    grid <- rif_grid[term_match, , drop = FALSE]
    if (!nrow(grid)) {
      return(echart_blank(paste0("No RIF coefficients found for '", pred_var, "'."), height = height))
    }
    taus <- sort(unique(grid$tau))
    tau <- suppressWarnings(as.numeric(tau)[1L])
    selected_tau <- if (is.null(tau) || !length(tau) || !is.finite(tau)) {
      if (0.5 %in% taus) 0.5 else taus[[which.min(abs(taus - 0.5))]]
    } else {
      taus[[which.min(abs(taus - tau))]]
    }
    grid <- grid[grid$tau == selected_tau, , drop = FALSE]
    grid$model <- dplyr::case_when(
      grid$model == 1L ~ "No FE or covariates",
      grid$model == 2L ~ "No covariates",
      TRUE ~ lab3
    )
    grid$Estimate <- grid$estimate
    grid$.term <- grid$term
    if (!"conf.low" %in% names(grid)) grid$conf.low <- grid$Estimate - 1.96 * grid$std.error
    if (!"conf.high" %in% names(grid)) grid$conf.high <- grid$Estimate + 1.96 * grid$std.error
    combined_poly <- FALSE
    poly_terms <- unique(grid$term[
      vapply(grid$term, term_has_predictor, logical(1)) &
        grepl(paste0("I(", pred_var, "^"), grid$term, fixed = TRUE)
    ])
    if (pred_var %in% grid$term && length(poly_terms)) {
      x_mean <- NA_real_
      if (!is.null(train_data) && pred_var %in% names(train_data)) {
        x_mean <- mean(as.numeric(train_data[[pred_var]]), na.rm = TRUE)
      }
      fit3_model <- tryCatch({
        if (inherits(fit3, "fixest_multi") && length(fit3) >= 5L) fit3[[5L]] else fit3
      }, error = function(e) NULL)
      mm <- if (!is.null(fit3_model)) resolve_model_matrix(fit3_model) else NULL
      if (!is.finite(x_mean) && !is.null(mm) && pred_var %in% names(mm)) {
        x_mean <- mean(as.numeric(mm[[pred_var]]), na.rm = TRUE)
      }
      if (!is.finite(x_mean)) x_mean <- 0
      poly_power <- function(term) {
        power <- regmatches(term, regexpr("[0-9]+", term))
        suppressWarnings(as.integer(power))
      }
      related <- c(pred_var, poly_terms)
      selected <- grid[grid$.term %in% related, , drop = FALSE]
      groups <- split(selected, interaction(selected$model, selected$tau, drop = TRUE))
      model_fits <- list(fit1, fit2, fit3)
      names(model_fits) <- c("No FE or covariates", "No covariates", lab3)
      combined <- lapply(groups, function(d) {
        weights <- vapply(d$.term, function(term) {
          if (identical(term, pred_var)) return(1)
          p <- poly_power(term)
          p * x_mean^(p - 1L)
        }, numeric(1))
        estimate <- sum(weights * d$Estimate, na.rm = TRUE)
        se <- .rif_combined_se(
          .rif_subfit(model_fits[[d$model[1L]]], taus, d$tau[1L]),
          d$.term, weights, d$std.error
        )
        row <- d[1L, , drop = FALSE]
        row$Estimate <- estimate
        row$std.error <- se
        row$conf.low <- estimate - 1.96 * se
        row$conf.high <- estimate + 1.96 * se
        row$term <- pred_var
        row
      })
      grid <- do.call(rbind, combined)
      grid$term <- pred_var
      combined_poly <- TRUE
    }
    coef_map <- make_coef_map(unique(grid$term), label_fun)
    grid$label <- unname(names(coef_map)[match(grid$term, unname(coef_map))])
    grid$label[is.na(grid$label)] <- grid$term[is.na(grid$label)]
    term_order <- unique(grid$term[order(grepl(":", grid$term), grid$term)])
    grid$label_wrap <- factor(
      stringr::str_wrap(grid$label, 25),
      levels = rev(unique(stringr::str_wrap(grid$label[match(term_order, grid$term)], 25)))
    )
    coef_data <- grid
  } else {
    if (!requireNamespace("fixest", quietly = TRUE)) {
      return(echart_blank("Package 'fixest' is required.", height = height))
    }
  model_list <- list("No FE or covariates" = fit1, "No covariates" = fit2)
  model_list[[lab3]] <- fit3

  coef_data <- tryCatch({
    d <- do.call(rbind, lapply(names(model_list), function(model_name) {
      ct <- tryCatch(.fixest_coeftable(model_list[[model_name]]),
        error = function(e) NULL
      )
      if (is.null(ct)) {
        return(NULL)
      }
      ct$term <- rownames(ct)
      ct$model <- model_name
      ct
    }))
    if (is.null(d)) {
      return(NULL)
    }
    filter_terms <- if (!is.null(pred_var)) pred_var else weather_terms
    keep_terms <- weather_coef_names(fit3, filter_terms)
    d <- d[d$term %in% keep_terms, , drop = FALSE]
    if (nrow(d) == 0) {
      return(NULL)
    }
    coef_map <- make_coef_map(keep_terms, label_fun)
    d$label <- names(coef_map)[match(d$term, coef_map)]
    d$label <- ifelse(is.na(d$label), d$term, d$label)
    d$conf.low <- d$Estimate - 1.96 * d$`Std. Error`
    d$conf.high <- d$Estimate + 1.96 * d$`Std. Error`
    d$label_wrap <- stringr::str_wrap(d$label, 25)

    term_esc <- gsub("([\\[\\]\\(\\)\\^\\$\\.\\*\\+\\?])", "\\\\\\1", filter_terms)
    weather_pattern <- paste0("\\b(", paste(term_esc, collapse = "|"), ")\\b")
    protected <- gsub("::", "", d$term, fixed = TRUE)
    parts <- strsplit(protected, ":", fixed = TRUE)
    main_part <- vapply(parts, function(p) {
      hit <- p[grepl(weather_pattern, p)]
      if (length(hit) == 0) p[1] else hit[1]
    }, character(1))
    is_int <- lengths(parts) > 1
    ord <- order(match(main_part, unique(main_part)), is_int)
    label_levels <- unique(d$label_wrap[ord])
    d$label_wrap <- factor(d$label_wrap, levels = rev(label_levels))
    d
  }, error = function(e) NULL)
  }

  if (is.null(coef_data) || !nrow(coef_data)) {
    return(echart_blank("No weather coefficients found to plot.", height = height))
  }

  model_levels <- c("No FE or covariates", "No covariates", lab3)
  model_cols <- c(
    "No FE or covariates" = "#8C8C8C",
    "No covariates" = .wise_charcoal,
    setNames(.wise_blue, lab3)
  )
  dodges <- c(-0.28, 0, 0.28)

  labels <- rev(levels(coef_data$label_wrap))
  y_cats <- labels
  e <- .e_new(height)
  series <- lapply(seq_along(model_levels), function(i) {
    nm <- model_levels[i]
    d <- coef_data[coef_data$model == nm, , drop = FALSE]
    if (!nrow(d)) {
      return(NULL)
    }
    col <- unname(model_cols[[nm]])
    dy <- dodges[i]
      pts <- lapply(seq_len(nrow(d)), function(j) {
        cat_idx <- match(as.character(d$label_wrap[j]), y_cats)
        list(value = list(d$Estimate[j], cat_idx + dy),
          confLow = d$conf.low[j], confHigh = d$conf.high[j])
    })
    whiskers <- lapply(seq_len(nrow(d)), function(j) {
      cat_idx <- match(as.character(d$label_wrap[j]), y_cats)
      list(
        list(coord = list(d$conf.low[j], cat_idx + dy)),
        list(coord = list(d$conf.high[j], cat_idx + dy))
      )
    })
    ml_data <- whiskers
    if (i == 1L) {
      ml_data <- c(
        list(list(
          xAxis = 0,
          lineStyle = list(color = .wise_zero, type = "dashed", width = 1),
          symbol = list("none", "none")
        )),
        whiskers
      )
    }
    list(
      name = nm, type = "scatter",
      data = pts,
       symbol = "circle", symbolSize = 8,
      itemStyle = list(color = col),
         markLine = list(
         symbol = list("none", "none"),
         symbolSize = list(0, 0),
        silent = TRUE, animation = FALSE,
        lineStyle = list(color = col, width = 1.5, type = "solid"),
        label = list(show = FALSE),
        data = ml_data
      )
    )
  })
  series <- purrr::compact(series)
  e$x$opts$series <- series

  x_min_val <- min(c(0, coef_data$conf.low), na.rm = TRUE)
  x_max_val <- max(c(0, coef_data$conf.high), na.rm = TRUE)
  x_pad <- max((x_max_val - x_min_val) * 0.08, 0.02)

  e$x$opts$xAxis <- list(
    type = "value", scale = TRUE,
    min = round(x_min_val - x_pad, 2),
    max = round(x_max_val + x_pad, 2),
    name = x_label %||% stringr::str_wrap(paste0("Effect on ", outcome_label), 50),
    nameLocation = "middle", nameGap = 30,
           nameTextStyle = wise_eaxis_name(align = "center"),
    axisLabel = wise_eaxis_label(showMinLabel = FALSE, showMaxLabel = FALSE),
    splitLine = wise_esplit_line()
  )
  e$x$opts$yAxis <- list(
    type = "value", min = 0, max = length(y_cats) + 1,
    interval = 1, inverse = TRUE,
    axisLabel = wise_eaxis_label(
      fontSize = 11, formatter = .e_index_formatter(y_cats),
      showMinLabel = TRUE, showMaxLabel = TRUE
    ),
    axisLine = list(lineStyle = list(color = .wise_grid)),
    splitLine = wise_esplit_line()
  )
  e$x$opts$legend <- wise_elegend_style(left = "center", top = 0, orient = "horizontal")
  e$x$opts$grid <- list(containLabel = TRUE, left = 8, right = 20, top = 42, bottom = 28)
  if (identical(engine, "rif") && combined_poly) {
    e <- .e_caption(e, paste(
      "Polynomial weather terms combined to total marginal effect at sample mean;",
      "95% CI assumes zero covariance across RIF coefficients."
    ))
    e$x$opts$grid$bottom <- e$x$opts$grid$bottom + 24
  }
  e$x$opts$tooltip <- list(
    trigger = "item",
    formatter = htmlwidgets::JS(
      "function(p){var d=p.data||{};var v=Array.isArray(d.value)?Number(d.value[0]):NaN;if(!isFinite(v))return '';var f=function(x){return Number(x).toLocaleString('en-US',{minimumFractionDigits:2,maximumFractionDigits:2});};var ci=isFinite(Number(d.confLow))&&isFinite(Number(d.confHigh))?'<br/>95% CI: ['+f(d.confLow)+', '+f(d.confHigh)+']':'';return (p.seriesName||'Effect')+': <b>'+f(v)+'</b>'+ci;}"
    )
  )
  wise_echart_theme(e)
}


# Shared echarts fragments for the effect / diagnostics builders (guidelines
# §7). Keep these tiny and generic; branch-specific layout stays in the
# builders below.

# Horizontal dashed y = 0 reference (geom_hline(yintercept = 0) counterpart).
.e_zero_line <- function() {
  list(
    symbol = "none", silent = TRUE, animation = FALSE,
    label = list(show = FALSE),
    lineStyle = list(color = .wise_zero, type = "dashed", width = 1),
    data = list(list(yAxis = 0))
  )
}

# Percent-share axis labels (scales::percent_format counterpart).
.e_percent_formatter <- function() {
  htmlwidgets::JS("function(v){return Math.round(100*v)+'%';}")
}

# Numeric-axis label formatter backed by an R-side lookup table (index ->
# label), the echarts counterpart of scale_x_continuous(breaks, labels).
.e_index_formatter <- function(labels) {
  htmlwidgets::JS(paste0(
    "function(v){var m=",
    jsonlite::toJSON(stats::setNames(as.list(labels), as.character(seq_along(labels)))),
    ";return m[String(Math.round(v))]||'';}"
  ))
}

# Bottom-anchored, left-aligned sub-text carrying the ggplot caption. ECharts
# titles do not reserve layout space, so callers give the grid extra room.
.e_caption <- function(e, caption) {
  if (is.null(caption) || !nzchar(caption)) {
    return(e)
  }
  e$x$opts$title <- c(e$x$opts$title %||% list(), list(
    list(
      text = "", subtext = caption, left = 8, bottom = 0,
      textAlign = "left",
      subtextStyle = list(
        color = .wise_slate, fontSize = 11, fontWeight = "normal"
      )
      )
  ))

  get_axis_types <- function(axes) {
    if (is.null(axes)) return(character(0))
    axis_fields <- c("type", "axisLabel", "axisTick", "data", "gridIndex", "show")
    single_axis <- !is.null(names(axes)) && any(names(axes) %in% axis_fields)
    axis_list <- if (single_axis) list(axes) else axes
    vapply(axis_list, function(axis) {
      if (!is.list(axis)) return("value")
      axis[["type"]] %||% "value"
    }, character(1))
  }
  axis_type <- c(get_axis_types(e$x$opts$xAxis), get_axis_types(e$x$opts$yAxis))
  if (length(axis_type) && all(axis_type %in% c("value", "log", "time"))) {
    grids <- e$x$opts$grid
    if (!is.null(grids)) {
      grid_fields <- c("left", "right", "top", "bottom", "width", "height",
        "containLabel", "show", "backgroundColor", "borderColor", "borderWidth",
        "shadowBlur", "shadowColor", "shadowOffsetX", "shadowOffsetY")
      single_grid <- !is.null(names(grids)) && any(names(grids) %in% grid_fields)
      grid_list <- if (single_grid) list(grids) else grids
      grid_list <- lapply(grid_list, function(grid) {
        left <- suppressWarnings(as.numeric(sub("px$", "", as.character(grid$left))))
        if (length(left) == 1L && is.finite(left)) {
          grid$left <- max(left, 64)
          grid$containLabel <- FALSE
        }
        grid
      })
      e$x$opts$grid <- if (single_grid) grid_list[[1L]] else grid_list
    }
  }
  e
}

# Dashed vertical tau reference marks with small top labels (mark_taus arg).
.e_tau_mark_data <- function(taus) {
  lapply(taus, function(t) {
    list(
      xAxis = t,
      lineStyle = list(color = .wise_zero, type = "dashed", width = 1),
      label = list(
        show = TRUE, position = "end",
        formatter = paste0("\u03c4 = ", formatC(t, format = "f", digits = 1)),
        color = .wise_slate, fontSize = 10
      )
    )
  })
}


#' Echarts weather effect plot (continuous, binned and RIF branches)
#'
#' Interactive counterpart of [make_weather_effect_plot()] (guidelines §7):
#' the ggplot builder's data preparation is re-used verbatim and drawn as an
#' `echarts4r` widget. Branch map (same dispatch as the ggplot builder):
#'
#' \itemize{
#'   \item RIF, binned predictor without interactions: one beta(tau) curve per
#'     bin, faceted one panel per bin (echarts grids) when there is more than
#'     one bin.
#'   \item RIF, single term: a single beta(tau) curve panel.
#'   \item RIF, main + interactions: combined effect per moderator level
#'     (or the across-moderator average in `mode = "main"`), one panel per
#'     bin when the predictor is binned.
#'   \item Binned without moderator: bin pointrange + connecting line.
#'   \item Binned with moderator: per-moderator overlay with dodged
#'     pointranges.
#'   \item Continuous: marginal-effect line with 95% CI ribbon, observed-
#'     value rug along the bottom edge and a dashed mean reference.
#'   \item Continuous with moderator: per-moderator marginal-effect curves.
#' }
#'
#' Captions render as a small slate sub-text anchored bottom-left (echarts
#' has no plot.caption slot); call-side captions pass through unchanged.
#'
#' @inheritParams make_weather_effect_plot
#' @param height Widget height; a CSS length or a number of pixels.
#'
#' @return An `echarts4r` widget.
#'
#' @export
echart_weather_effect_plot <- function(fit, pred_var, interaction_terms, is_binned,
                                       label_fun, engine, selected_weather = NULL,
                                       weather_df = NULL, rif_grid = NULL,
                                       mode = "auto", is_logistic = FALSE,
                                       x_label = NULL, y_label = NULL,
                                       caption = NULL,
                                       show_rug = TRUE, show_mean_ref = TRUE,
                                       mark_taus = NULL,
                                       effect_scale = "model",
                                       profile_eta = NULL,
                                       height = "500px") {
  tryCatch(
    {
      mode <- match.arg(mode, c("auto", "main", "moderated"))
      effect_scale <- match.arg(effect_scale, c("model", "pp", "pp100", "pct"))

      # --- data preparation copied verbatim from make_weather_effect_plot() --
      modx_level_label <- function(lab, v) {
        v_chr <- as.character(v)
        if (length(v_chr) != 1) v_chr <- v_chr[[1]]
        num <- suppressWarnings(as.numeric(v_chr))
        if (!is.na(num) && num %in% c(0, 1)) {
          paste0(lab, ": ", if (num == 1) "yes" else "no")
        } else if (!is.na(num)) {
          paste0(lab, " = ", round(num, 2))
        } else {
          paste0(lab, " = ", v_chr)
        }
      }
      .apply_effect_scale <- function(df, est_col = "Estimate",
                                      lo_col = "conf.low", hi_col = "conf.high") {
        if (identical(effect_scale, "pp")) {
          if (!is.finite(profile_eta)) {
            return(df)
          }
          pp_at <- function(b) {
            100 * (stats::plogis(profile_eta + b) -
              stats::plogis(profile_eta))
          }
          df[[est_col]] <- pp_at(df[[est_col]])
          df[[lo_col]] <- pp_at(df[[lo_col]])
          df[[hi_col]] <- pp_at(df[[hi_col]])
        } else if (identical(effect_scale, "pp100")) {
          df[[est_col]] <- 100 * df[[est_col]]
          df[[lo_col]] <- 100 * df[[lo_col]]
          df[[hi_col]] <- 100 * df[[hi_col]]
        }
        df
      }
      .t2_bin_label <- function(term, pred_var) {
        term <- as.character(term)[[1]]
        pred_esc <- gsub("([\\[\\]\\(\\)\\^\\$\\.\\*\\+\\?])", "\\\\\\1", pred_var)
        s <- sub(paste0("^", pred_esc, "[\\[\\(]"), "", term)
        if (identical(s, term)) {
          return(term)
        }
        s <- sub("[])]$", "", s)
        parts <- trimws(strsplit(s, ",", fixed = TRUE)[[1]])
        if (length(parts) != 2L || any(!nzchar(parts))) {
          return(term)
        }
        parts <- gsub("[\\[\\]()]", "", parts)
        paste0(parts[[1]], "\u2013", parts[[2]])
      }
      mm_of <- function(fit) {
        mm <- resolve_model_matrix(fit)
        if (!is.null(mm)) {
          return(mm)
        }
        tryCatch(stats::model.frame(fit), error = function(e) NULL)
      }

      # --- widget plumbing ----------------------------------------------------
      tau_x_axis <- function(taus) {
        list(
          type = "value", min = min(taus), max = max(taus),
          name = "Welfare quantile",
          nameLocation = "middle", nameGap = 28,
            nameTextStyle = wise_eaxis_name(),
          axisLabel = modifyList(
            wise_eaxis_label(),
            list(formatter = .e_percent_formatter())
          ),
          splitLine = wise_esplit_line()
        )
      }
      value_y_axis <- function(name, min_val = NULL, max_val = NULL,
                               vertical_name = FALSE) {
        list(
          type = "value", scale = is.null(min_val), name = name,
          min = min_val, max = max_val,
          nameLocation = if (vertical_name) "middle" else "end",
          nameRotate = if (vertical_name) 90 else 0,
          nameGap = if (vertical_name) 44 else 8,
          nameTextStyle = if (vertical_name) wise_eaxis_name() else wise_eyaxis_name(),
           axisLabel = wise_eaxis_label(),
          splitLine = wise_esplit_line()
        )
      }
      rif_y_bounds <- function(d) {
        lo <- min(c(0, d$conf.low), na.rm = TRUE)
        hi <- max(c(0, d$conf.high), na.rm = TRUE)
        pad <- max((hi - lo) * 0.08, 0.02)
        c(lo - pad, hi + pad)
      }
      omitted_bin_note <- function(bin_ids) {
        observed <- unique(vapply(bin_ids, function(b) .t2_bin_label(b, pred_var), character(1)))
        observed <- observed[nzchar(observed)]
        all_bins <- character(0)
        if (!is.null(weather_df) && pred_var %in% names(weather_df)) {
          values <- weather_df[[pred_var]]
          raw_levels <- if (is.factor(values)) levels(values) else sort(unique(as.character(values)))
          all_bins <- vapply(raw_levels, .cut_bin_label, character(1))
        }
        omitted <- setdiff(all_bins, observed)
        label <- if (length(omitted)) omitted[[1L]] else "reference bin"
        paste0("Omitted weather bin: ", label, ".")
      }
      # Bin category axis matching ggplot factor bins.
      bin_x_axis <- function(bin_labels, name) {
        list(
          type = "category",
          data = as.character(bin_labels),
          name = name,
          nameLocation = "middle", nameGap = 34,
          nameTextStyle = wise_eaxis_name(align = "center"),
          axisLabel = wise_eaxis_label(rotate = 0),
          axisTick = list(alignWithLabel = TRUE),
          axisLine = list(lineStyle = list(color = .wise_grid)),
          splitLine = wise_esplit_line()
        )
      }
      markline_of <- function(extra_data = list()) {
        ml <- .e_zero_line()
        if (length(extra_data)) ml$data <- c(ml$data, extra_data)
        ml
      }
      grid_pad <- function(legend, cap) {
        8 + 22 * legend + 24 * cap
      }

      # --- RIF branch: weather beta curve across quantiles --------------------
      if (identical(engine, "rif") && !is.null(rif_grid)) {
        pred_esc <- gsub("([\\[\\]\\(\\)\\^\\$\\.\\*\\+\\?])", "\\\\\\1", pred_var)

        grid3 <- rif_grid[rif_grid$model == 3L, ]
        mask <- grepl(paste0("\\b", pred_esc, "\\b"), grid3$term)
        if (!any(mask)) {
          return(echart_blank(paste0("No RIF terms found for '", pred_var, "'."),
            height = height
          ))
        }
        plot_data <- grid3[mask, ]

        taus <- sort(unique(plot_data$tau))
        plot_data$term_label <- vapply(
          plot_data$term, function(t) coef_label(t, label_fun), character(1)
        )

        n_terms <- length(unique(plot_data$term))
        has_int_terms <- any(grepl(":", plot_data$term, fixed = TRUE))
      rif_y_lab <- "Effect size (log points)"

      # Facet panels: shared panel setup (echarts grids).
      panel_ribbon <- function(d, col, panel_idx, point_size, nm = "Effect") {
        d <- d[order(d$tau), ]
        tri <- .e_ribbon_series(
          nm, d$tau, d$estimate, d$conf.low, d$conf.high,
          fill = col, line = col, line_width = 2,
          show_points = TRUE, point_size = point_size
        )
        lapply(tri, function(s) {
          s$xAxisIndex <- panel_idx - 1L
          s$yAxisIndex <- panel_idx - 1L
          s
        })
      }

         is_bin_terms <- all(grepl(
           paste0("^", pred_esc, "[\\[\\(]"),
           unique(plot_data$term)
         ))
         if (n_terms > 1 && !has_int_terms && is_bin_terms) {
          # Binned predictor without interactions: one beta(tau) curve per bin,
          # one facet per bin in numeric bin order (verbatim prep).
          bin_lo <- function(tm) {
            s <- sub(paste0("^", pred_esc, "[\\[\\(]"), "", tm)
            suppressWarnings(as.numeric(sub("^([^,]+),.*", "\\1", s)))
          }
          tu <- unique(plot_data$term)
          tu <- tu[order(suppressWarnings(bin_lo(tu)))]
          lab_map <- stats::setNames(
            vapply(tu, function(t) .t2_bin_label(t, pred_var), character(1)), tu
          )

          e <- .e_new(height)
          e <- .e_multi_grids(
            e, length(tu), titles = paste0("Bin: ", unname(lab_map)), height = height,
            contain_label = FALSE
          )
          series <- unlist(lapply(seq_along(tu), function(i) {
            panel_ribbon(plot_data[plot_data$term == tu[i], ], .wise_blue, i, 7)
          }), recursive = FALSE)
           for (i in seq_along(tu)) {
             idx <- i * 3L
             if (length(series) >= idx) {
               series[[idx]]$markLine <- markline_of(.e_tau_mark_data(mark_taus))
             }
          }
          e$x$opts$series <- series
          e$x$opts$xAxis <- lapply(seq_along(tu), function(i) {
            axis <- tau_x_axis(taus)
            axis$gridIndex <- i - 1L
            axis
          })
           bounds <- rif_y_bounds(plot_data[plot_data$term == tu[[1L]], , drop = FALSE])
           yax <- value_y_axis(rif_y_lab, bounds[[1L]], bounds[[2L]], vertical_name = TRUE)
           e$x$opts$yAxis <- lapply(seq_along(tu), function(i) {
             d_i <- plot_data[plot_data$term == tu[[i]], , drop = FALSE]
             b_i <- rif_y_bounds(d_i)
             axis <- if (i == 1L) yax else {
                y <- value_y_axis("", b_i[[1L]], b_i[[2L]])
                y
              }
              if (i == 1L) axis <- value_y_axis(
                rif_y_lab, b_i[[1L]], b_i[[2L]], vertical_name = TRUE
              )
              axis$gridIndex <- i - 1L
              axis
            })
           e$x$opts$tooltip <- .e_effect_tooltip(
             percent = effect_scale %in% c("pp", "pp100", "pct")
           )
           observed_bins <- unname(lab_map)
           omitted_bins <- character(0)
           if (!is.null(weather_df) && pred_var %in% names(weather_df)) {
             omitted_bins <- setdiff(
               vapply(levels(weather_df[[pred_var]]), .cut_bin_label, character(1)),
               observed_bins
             )
           }
           omitted_label <- if (length(omitted_bins)) omitted_bins[[1L]] else "reference bin"
           e <- .e_caption(e, paste("Ribbon = 95% CI. Omitted weather bin:", omitted_label))
           e$x$opts$grid <- lapply(seq_along(e$x$opts$grid), function(i) {
             modifyList(e$x$opts$grid[[i]], list(bottom = grid_pad(FALSE, TRUE) + 44))
          })
          return(wise_echart_theme(e))
        }

        if (n_terms == 1) {
          # Single term: simple beta curve.
          e <- .e_new(height)
          tri <- panel_ribbon(plot_data, .wise_blue, 1L, 8)
           if (length(tri) >= 3L) {
             tri[[3]]$markLine <- markline_of(.e_tau_mark_data(mark_taus))
           }
          e$x$opts$series <- tri
          e$x$opts$xAxis <- list(tau_x_axis(taus))
           bounds <- rif_y_bounds(plot_data)
           e$x$opts$yAxis <- list(value_y_axis(rif_y_lab, bounds[[1L]], bounds[[2L]]))
          e$x$opts$grid <- list(
            containLabel = TRUE, left = 8, right = 20, top = 14,
            bottom = grid_pad(FALSE, TRUE)
          )
           e$x$opts$tooltip <- .e_effect_tooltip(
             percent = effect_scale %in% c("pp", "pp100", "pct")
           )
          e <- .e_caption(e, "Ribbon = 95% CI")
          return(wise_echart_theme(e))
        }

        if (!is_bin_terms && !has_int_terms) {
          # Polynomial RIF terms describe one continuous weather effect. Plot
          # their mean marginal effect as a single quantile curve rather than
          # treating each polynomial coefficient as a separate bin panel.
          mm <- mm_of(fit)
          x_mean <- if (!is.null(mm) && pred_var %in% names(mm)) {
            mean(as.numeric(mm[[pred_var]]), na.rm = TRUE)
          } else {
            0
          }
          term_weight <- function(term) {
            if (identical(term, pred_var)) return(1)
            power <- suppressWarnings(as.numeric(sub(
              paste0("^I\\(", pred_esc, "\\^([0-9]+)\\)$"),
              "\\1", term
            )))
            if (is.finite(power) && power > 1) return(power * x_mean^(power - 1))
            0
          }
          plot_data$weight <- vapply(plot_data$term, term_weight, numeric(1))
          combined <- do.call(rbind, lapply(
            split(plot_data, plot_data$tau),
            function(d) {
              data.frame(
                tau = d$tau[1L],
                estimate = sum(d$estimate * d$weight, na.rm = TRUE),
                std.error = .rif_combined_se(
                  .rif_subfit(fit, taus, d$tau[1L]),
                  d$term, d$weight, d$std.error
                )
              )
            }
          ))
          combined$conf.low <- combined$estimate - 1.96 * combined$std.error
          combined$conf.high <- combined$estimate + 1.96 * combined$std.error
          tri <- panel_ribbon(combined, .wise_blue, 1L, 8)
          if (length(tri) >= 3L) tri[[3L]]$markLine <- markline_of(.e_tau_mark_data(mark_taus))
          bounds <- rif_y_bounds(combined)
          e <- .e_new(height)
          e$x$opts$series <- tri
          e$x$opts$xAxis <- list(tau_x_axis(taus))
          e$x$opts$yAxis <- list(value_y_axis(rif_y_lab, bounds[[1L]], bounds[[2L]]))
          e$x$opts$grid <- list(
            containLabel = TRUE, left = 8, right = 20, top = 14,
            bottom = grid_pad(FALSE, TRUE)
          )
          e$x$opts$grid <- list(
            containLabel = TRUE, left = 8, right = 20, top = 14,
            bottom = grid_pad(FALSE, TRUE)
          )
          e$x$opts$tooltip <- .e_effect_tooltip(
            percent = effect_scale %in% c("pp", "pp100", "pct")
          )
          e <- .e_caption(e, "Ribbon = 95% CI")
          return(wise_echart_theme(e))
        }

        # Multiple terms (main + interactions): combined effect per moderator
        # level (verbatim prep below through the bin factor).
        protected <- gsub("::", "", plot_data$term, fixed = TRUE)
        parts <- strsplit(protected, ":", fixed = TRUE)
        weather_pat <- paste0("\\b", pred_esc, "\\b")
        is_int_row <- lengths(parts) > 1
        main_part <- vapply(parts, function(p) {
          hit <- p[grepl(weather_pat, p)]
          if (length(hit) == 0) p[1] else hit[1]
        }, character(1))

        modx_var <- NULL
        modx_lab <- NULL
        if (length(interaction_terms) > 0) {
          pv_pat <- paste0("\\b", pred_esc, "\\b")
          mt <- interaction_terms[grepl(pv_pat, interaction_terms)]
          if (length(mt) > 0) {
            mp <- strsplit(mt[1], ":", fixed = TRUE)[[1]]
            modx_var <- mp[mp != pred_var][1]
            if (!is.na(modx_var) && nzchar(modx_var)) {
              modx_lab <- label_fun(modx_var)
            }
          }
        }

        modx_vals <- c(0, 1)
        if (!is.null(weather_df) && !is.null(modx_var) &&
          modx_var %in% names(weather_df)) {
          mx <- weather_df[[modx_var]]
          mx <- mx[!is.na(mx)]
          if (length(mx) > 0) {
            if (is.numeric(mx)) {
              u <- sort(unique(mx))
              if (length(u) <= 5) {
                modx_vals <- u
              } else {
                m <- mean(mx)
                s <- stats::sd(mx)
                modx_vals <- c(m - s, m, m + s)
              }
            } else {
              lvls <- if (is.factor(mx)) {
                levels(droplevels(mx))
              } else {
                sort(unique(as.character(mx)))
              }
              num_try <- suppressWarnings(as.numeric(lvls))
              modx_vals <- if (all(!is.na(num_try))) {
                num_try
              } else {
                seq_along(lvls) - 1L
              }
            }
          }
        }

        plot_data$.bin_id <- main_part
        main_rows <- plot_data[!is_int_row, , drop = FALSE]
        int_rows <- plot_data[is_int_row, , drop = FALSE]

        combined <- do.call(rbind, lapply(modx_vals, function(v) {
          do.call(rbind, lapply(seq_len(nrow(main_rows)), function(j) {
            mr <- main_rows[j, , drop = FALSE]
            ir <- int_rows[int_rows$.bin_id == mr$.bin_id &
              int_rows$tau == mr$tau, , drop = FALSE]
            ie <- if (nrow(ir) > 0) ir$estimate[1] else 0
            ise <- if (nrow(ir) > 0) ir$std.error[1] else 0
            effect <- mr$estimate + v * ie
            se <- sqrt(mr$std.error^2 + v^2 * ise^2)
            data.frame(
              tau = mr$tau,
              bin_id = mr$.bin_id,
              bin_label = .t2_bin_label(mr$.bin_id, pred_var),
              modx_val = v,
              estimate = effect,
              std.error = se,
              conf.low = effect - 1.96 * se,
              conf.high = effect + 1.96 * se,
              stringsAsFactors = FALSE
            )
          }))
        }))

        modx_lab_print <- modx_lab %||% (modx_var %||% "moderator")
        combined$modx_label <- vapply(
          combined$modx_val,
          function(v) modx_level_label(modx_lab_print, v),
          character(1)
        )
        combined$modx_label <- factor(
          combined$modx_label,
          levels = unique(combined$modx_label[order(combined$modx_val)])
        )

        if (identical(mode, "main")) {
          combined <- combined |>
            dplyr::group_by(.data$tau, .data$bin_id, .data$bin_label) |>
            dplyr::summarise(
              estimate = mean(.data$estimate, na.rm = TRUE),
              std.error = sqrt(mean(.data$std.error^2, na.rm = TRUE)),
              conf.low = mean(.data$conf.low, na.rm = TRUE),
              conf.high = mean(.data$conf.high, na.rm = TRUE),
              .groups = "drop"
            ) |>
            dplyr::mutate(modx_label = "Average across moderator levels")
        }

        bin_ids_raw <- unique(main_rows$.bin_id)
        .bin_lower <- function(b) {
          s <- sub(paste0("^", pred_esc, "[\\[\\(]"), "", b)
          suppressWarnings(as.numeric(sub("^([^,]+),.*", "\\1", s)))
        }
        ord <- order(.bin_lower(bin_ids_raw))
        bin_ids_ordered <- bin_ids_raw[ord]
        bin_levels <- vapply(
          bin_ids_ordered,
          function(b) .t2_bin_label(b, pred_var),
          character(1)
        )
        combined$bin_label <- factor(combined$bin_label, levels = bin_levels)
        n_bins <- length(bin_levels)

        modx_levels <- levels(combined$modx_label) %||%
          unique(as.character(combined$modx_label))
        cols <- stats::setNames(.wise_cat[seq_along(modx_levels)], modx_levels)
        omitted_note <- omitted_bin_note(main_rows$.bin_id)
        rif_cap <- if (identical(mode, "main")) {
          paste(
            "Line and ribbon average the estimated effect across moderator levels;",
            "ribbon = 95% CI (cov(main, interaction) omitted).", omitted_note
          )
        } else {
          paste("Ribbon = 95% CI (cov(main, interaction) omitted).", omitted_note)
        }
        has_legend <- length(modx_levels) > 1L

        e <- .e_new(height)
        if (n_bins > 1) {
          panel_titles <- paste0("Bin: ", bin_levels)
          e <- .e_multi_grids(
            e, n_bins, titles = panel_titles, height = height,
            contain_label = FALSE
          )
          if (has_legend) {
            e$x$opts$title <- lapply(e$x$opts$title, function(title) {
              modifyList(title, list(top = 34))
            })
          }
          panel_trios <- lapply(seq_len(n_bins), function(bi) {
            trios <- lapply(seq_along(modx_levels), function(mi) {
              d <- combined[combined$bin_label == bin_levels[bi] &
                combined$modx_label == modx_levels[mi], , drop = FALSE]
              if (!nrow(d)) {
                return(NULL)
              }
              panel_ribbon(d, unname(cols[mi]), bi, 7, nm = modx_levels[mi])
            })
            trios <- purrr::compact(trios)
            if (length(trios)) trios[[1]][[3]]$markLine <-
              markline_of(.e_tau_mark_data(mark_taus))
            trios
          })
          series <- unlist(unlist(panel_trios, recursive = FALSE),
            recursive = FALSE
          )
           e$x$opts$xAxis <- lapply(seq_len(n_bins), function(i) {
             axis <- tau_x_axis(taus)
             axis$gridIndex <- i - 1L
             axis
           })
            e$x$opts$yAxis <- lapply(seq_len(n_bins), function(i) {
              d_i <- combined[combined$bin_label == bin_levels[[i]], , drop = FALSE]
              b_i <- rif_y_bounds(d_i)
              axis <- if (i == 1L) value_y_axis(
                rif_y_lab, b_i[[1L]], b_i[[2L]], vertical_name = TRUE
              ) else {
                y <- value_y_axis("", b_i[[1L]], b_i[[2L]])
               y
             }
              axis$gridIndex <- i - 1L
              axis
           })
          e$x$opts$grid <- lapply(seq_along(e$x$opts$grid), function(i) {
            modifyList(e$x$opts$grid[[i]], list(
              top = if (has_legend) 58 else e$x$opts$grid[[i]]$top,
              bottom = grid_pad(has_legend, TRUE) + 44
            ))
           })
        } else {
          series <- unlist(lapply(seq_along(modx_levels), function(mi) {
            d <- combined[combined$modx_label == modx_levels[mi], , drop = FALSE]
            if (!nrow(d)) {
              return(NULL)
            }
            panel_ribbon(d, unname(cols[mi]), 1L, 7, nm = modx_levels[mi])
          }), recursive = FALSE)
          if (length(series) >= 3L) {
            series[[3]]$markLine <- markline_of(.e_tau_mark_data(mark_taus))
          }
          e$x$opts$xAxis <- list(tau_x_axis(taus))
           bounds <- rif_y_bounds(combined)
           e$x$opts$yAxis <- list(value_y_axis(rif_y_lab, bounds[[1L]], bounds[[2L]]))
          e$x$opts$grid <- list(
            containLabel = TRUE, left = 8, right = 20, top = 14,
            bottom = grid_pad(has_legend, TRUE)
          )
        }
        if (has_legend) {
          e$x$opts$legend <- wise_elegend_style(
            data = lapply(seq_along(modx_levels), function(i) list(
              name = unname(modx_levels[i]), icon = "roundRect",
              itemStyle = list(color = unname(cols[i]))
            )),
            left = "center", top = 4, orient = "horizontal"
          )
        }
        e$x$opts$series <- series
         e$x$opts$tooltip <- .e_effect_tooltip(
           percent = effect_scale %in% c("pp", "pp100", "pct")
         )
        e <- .e_caption(e, rif_cap)
        return(wise_echart_theme(e))
      }

      pred_lab <- label_fun(pred_var)
      pred_x_lab <- x_label %||% paste0(pred_var, " (", pred_lab, ")")
      y_var_name <- tryCatch(
        as.character(stats::formula(fit)[[2]]),
        error = function(e) "outcome"
      )
      y_lab <- label_fun(y_var_name)
      cap_text <- caption %||% (
        if (isTRUE(is_logistic)) {
          paste(
            "Line = marginal effect (95% CI); curved with polynomial terms,",
            "flat for linear ones. pp at the median-risk household."
          )
        } else {
          paste(
            "Line = marginal effect (95% CI); curved with polynomial terms,",
            "flat for linear ones."
          )
        }
      )

      mf <- mm_of(fit)

      pred_esc <- gsub("([\\[\\]\\(\\)\\^\\$\\.\\*\\+\\?])", "\\\\\\1", pred_var)

      if (!is.null(mf) && pred_var %in% names(mf) && !is_binned) {
        pred_cols <- pred_var
      } else if (!is.null(mf) && is_binned) {
        pred_cols <- grep(paste0("^", pred_esc, "[\\[\\(]"), names(mf), value = TRUE)
      } else {
        pred_cols <- character(0)
      }

      if (!length(pred_cols)) {
        return(echart_blank(paste0("'", pred_var, "' not found in model frame."),
          height = height
        ))
      }

      # ===================================================================== #
      # BINNED PATH                                                           #
      # ===================================================================== #
      if (is_binned) {
        return(tryCatch(
          {
            if (!requireNamespace("fixest", quietly = TRUE)) {
              return(echart_blank("Package 'fixest' is required.", height = height))
            }

            mm <- mm_of(fit)
            if (is.null(mm)) {
              return(echart_blank("Model matrix unavailable.", height = height))
            }
            ct <- .fixest_coeftable(fit)
            ct$term <- rownames(ct)

            bin_cols <- grep(paste0("^", pred_esc, "[\\[\\(]"), names(mm), value = TRUE)
            bin_cols <- bin_cols[!grepl(":", bin_cols)]
            if (length(bin_cols) == 0) {
              return(echart_blank("No binned columns found in model matrix.",
                height = height
              ))
            }

            ct_main <- ct[
              grepl(paste0("^", pred_esc, "[\\[\\(]"), ct$term) & !grepl(":", ct$term),
              c("term", "Estimate", "Std. Error"),
              drop = FALSE
            ]

            .bin_lower <- function(b) {
              s <- sub(paste0("^", pred_esc, "[\\[\\(]"), "", b)
              suppressWarnings(as.numeric(sub("^([^,]+),.*", "\\1", s)))
            }
            bin_cols <- bin_cols[order(.bin_lower(bin_cols))]

            omitted_note <- NULL
            if (!is.null(weather_df)) {
              first_bin <- get_first_bin_label(weather_df, pred_var)
              if (!is.na(first_bin) && nzchar(first_bin)) {
                omitted_note <- paste0("Omitted reference bin: ", .cut_bin_label(first_bin), " at y = 0.")
              }
            }

            bins_df <- data.frame(term = bin_cols, stringsAsFactors = FALSE)
            bins_df <- dplyr::left_join(bins_df, ct_main, by = "term")
            bins_df$Estimate[is.na(bins_df$Estimate)] <- 0
            bins_df$`Std. Error`[is.na(bins_df$`Std. Error`)] <- 0
            bins_df$bin_index <- seq_len(nrow(bins_df))
            bins_df$bin_label <- vapply(
              bins_df$term, .t2_bin_label, character(1),
              pred_var = pred_var
            )

            modx_var <- NULL
            modx_lab <- NULL
            if (length(interaction_terms) > 0) {
              pv_pat <- paste0("\\b", pred_esc, "\\b")
              mt <- interaction_terms[grepl(pv_pat, interaction_terms)]
              if (length(mt) > 0) {
                parts <- strsplit(mt[1], ":")[[1]]
                modx_var <- parts[parts != pred_var][1]
                if (!is.na(modx_var) && nzchar(modx_var)) modx_lab <- label_fun(modx_var)
              }
            }

            if (identical(mode, "main")) {
              modx_var <- NULL
              modx_lab <- NULL
            } else if (identical(mode, "moderated") && is.null(modx_var)) {
              return(echart_blank(paste0("No moderator specified for '", pred_var, "'."),
                height = height
              ))
            }

            cap_binned <- paste(c(omitted_note, caption), collapse = " ")
            cap_binned <- if (is.null(cap_binned) || !nzchar(cap_binned)) {
              NULL
            } else {
              cap_binned
            }

            e <- .e_new(height)

            if (is.null(modx_var)) {
              bins_df$conf.low <- bins_df$Estimate - 1.96 * bins_df$`Std. Error`
              bins_df$conf.high <- bins_df$Estimate + 1.96 * bins_df$`Std. Error`
              bins_df <- .apply_effect_scale(bins_df)

              band <- .e_ribbon_series(
                "Effect", seq_len(nrow(bins_df)) - 1L,
                bins_df$Estimate, bins_df$conf.low, bins_df$conf.high,
                fill = .wise_blue, line = .wise_blue,
                line_width = 1.5, show_points = TRUE, point_size = 8
              )
              band[[3]]$markLine <- list(
                symbol = list("none", "none"), silent = TRUE,
                data = list(list(
                  yAxis = 0,
                  lineStyle = list(color = .wise_zero, type = "dashed", width = 1)
                ))
              )
              band[[3]]$tooltip <- .e_effect_tooltip(
                percent = effect_scale %in% c("pp", "pp100", "pct")
              )
              y_min_val <- min(c(0, bins_df$conf.low), na.rm = TRUE)
              y_max_val <- max(c(0, bins_df$conf.high), na.rm = TRUE)
              y_pad <- max((y_max_val - y_min_val) * 0.12, 0.02)

              e$x$opts$series <- band
              e$x$opts$xAxis <- list(bin_x_axis(bins_df$bin_label, pred_x_lab))
              e$x$opts$yAxis <- list(value_y_axis(
                y_label %||% paste("Effect on", y_lab),
                min_val = round(y_min_val - y_pad, 3),
                max_val = round(y_max_val + y_pad, 3)
              ))
              e$x$opts$grid <- list(
                containLabel = TRUE, left = 8, right = 20, top = 36,
                bottom = grid_pad(FALSE, !is.null(cap_binned))
              )
              e$x$opts$tooltip <- .e_effect_tooltip(
                percent = effect_scale %in% c("pp", "pp100", "pct")
              )
              e <- .e_caption(e, cap_binned)
              return(wise_echart_theme(e))
            }

            # Moderator present: overlay lines (verbatim prep).
            ct_int <- ct[
              grepl(paste0(pred_esc, "[\\[\\(]"), ct$term) &
                grepl(":", ct$term) &
                grepl(modx_var, ct$term, fixed = TRUE),
              c("term", "Estimate", "Std. Error"),
              drop = FALSE
            ]
            int_est <- stats::setNames(ct_int$Estimate, ct_int$term)
            int_se <- stats::setNames(ct_int$`Std. Error`, ct_int$term)

            if (modx_var %in% names(mm)) {
              modx_vals <- sort(unique(mm[[modx_var]]))
              if (length(modx_vals) > 5 && is.numeric(modx_vals)) {
                mu <- mean(mm[[modx_var]], na.rm = TRUE)
                sd <- stats::sd(mm[[modx_var]], na.rm = TRUE)
                modx_vals <- c(mu - sd, mu, mu + sd)
              }
            } else {
              modx_vals <- c(0, 1)
            }

            plot_df <- do.call(
              rbind,
              lapply(seq_len(nrow(bins_df)), function(i) {
                bin_term <- bins_df$term[i]
                b0 <- bins_df$Estimate[i]
                v0 <- bins_df$`Std. Error`[i]^2

                t1 <- paste0(bin_term, ":", modx_var)
                t2 <- paste0(modx_var, ":", bin_term)
                iterm <- if (t1 %in% names(int_est)) t1 else if (t2 %in% names(int_est)) t2 else NA_character_
                has_int <- !is.na(iterm)

                do.call(rbind, lapply(modx_vals, function(mv) {
                  b <- b0 + if (has_int) int_est[[iterm]] * mv else 0
                  s <- sqrt(v0 + if (has_int) (mv^2) * int_se[[iterm]]^2 else 0)
                  data.frame(
                    bin_index = bins_df$bin_index[i],
                    bin_label = bins_df$bin_label[i],
                    est = b,
                    conf.low = b - 1.96 * s,
                    conf.high = b + 1.96 * s,
                    modx = as.character(mv),
                    stringsAsFactors = FALSE
                  )
                }))
              })
            )

            plot_df <- plot_df[order(plot_df$modx, plot_df$bin_index), , drop = FALSE]
            plot_df <- .apply_effect_scale(plot_df, est_col = "est")
            modx_u <- sort(unique(plot_df$modx))
            modx_labels <- vapply(
              modx_u, function(v) modx_level_label(modx_lab, v),
              character(1)
            )
            plot_df$modx <- factor(plot_df$modx, levels = modx_u, labels = modx_labels)

            # Dodge in data units so lines, points and whiskers share offsets.
            dodge <- 0.16
            offs <- stats::setNames(
              (seq_along(modx_u) - (length(modx_u) + 1) / 2) * dodge,
              modx_labels
            )
            cols <- stats::setNames(
              .wise_cat[seq_along(modx_labels)], modx_labels
            )

            series <- unlist(lapply(seq_along(modx_labels), function(mi) {
              lab <- unname(modx_labels[mi])
              d <- plot_df[plot_df$modx == lab, , drop = FALSE]
              d <- d[order(d$bin_index), , drop = FALSE]
              if (!nrow(d)) {
                return(NULL)
              }
              dx <- unname(offs[[lab]])
              col <- unname(cols[mi])
              tri <- .e_ribbon_series(
                lab, seq_len(nrow(d)) - 1L + dx, d$est,
                d$conf.low, d$conf.high, fill = col, line = col,
                line_width = 1.5, show_points = TRUE, point_size = 7
              )
              if (length(tri) >= 3L && mi == 1L) {
                tri[[3]]$markLine <- list(
                  symbol = list("none", "none"), silent = TRUE,
                  data = list(list(yAxis = 0, lineStyle = list(
                    color = .wise_zero, type = "dashed", width = 1
                  )))
                )
              }
              if (length(tri) >= 3L) {
                tri[[3]]$tooltip <- .e_effect_tooltip(
                  percent = effect_scale %in% c("pp", "pp100", "pct")
                )
              }
              tri
            }), recursive = FALSE)
            series <- Filter(Negate(is.null), series)

            y_min_val <- min(c(0, plot_df$conf.low), na.rm = TRUE)
            y_max_val <- max(c(0, plot_df$conf.high), na.rm = TRUE)
            y_pad <- max((y_max_val - y_min_val) * 0.12, 0.02)

            e$x$opts$series <- series
            e$x$opts$xAxis <- list(bin_x_axis(bins_df$bin_label, pred_x_lab))
            e$x$opts$yAxis <- list(value_y_axis(
              y_label %||% paste("Effect on", y_lab),
              min_val = round(y_min_val - y_pad, 3),
              max_val = round(y_max_val + y_pad, 3)
            ))
            e$x$opts$legend <- wise_elegend_style(
              data = lapply(seq_along(modx_labels), function(i) list(
                name = unname(modx_labels[i]),
                icon = "roundRect",
                itemStyle = list(color = unname(cols[i]))
              )),
              left = "center", top = 4, width = "92%", height = "32%",
              orient = "horizontal"
            )
            e$x$opts$grid <- list(
               containLabel = TRUE, left = 8, right = 20, top = 82,
              bottom = grid_pad(TRUE, !is.null(cap_binned))
            )
            e$x$opts$tooltip <- .e_effect_tooltip(
              percent = effect_scale %in% c("pp", "pp100", "pct")
            )
            e <- .e_caption(e, cap_binned)
            return(wise_echart_theme(e))
          },
          error = function(e) echart_blank(paste0("Binned effect plot error: ", conditionMessage(e)),
            height = height
          )
        ))
      }

      # ===================================================================== #
      # CONTINUOUS PATH: marginal effect of weather vs weather level          #
      # ===================================================================== #
      pred_vals <- mf[[pred_var]]

      if (!any(is.finite(pred_vals))) {
        return(echart_blank(paste0(
          "No finite values for '", pred_var, "' - cannot build effect plot."
        ), height = height))
      }

      if (!requireNamespace("fixest", quietly = TRUE)) {
        return(echart_blank("Package 'fixest' is required.", height = height))
      }

      modx_var <- NULL
      modx_lab <- NULL
      if (length(interaction_terms) > 0) {
        match_term <- grep(paste0("^", pred_var, ":"), interaction_terms, value = TRUE)
        if (length(match_term) > 0) {
          modx_var <- strsplit(match_term[1], ":")[[1]][2]
          modx_lab <- label_fun(modx_var)
        }
      }

      if (identical(mode, "main")) {
        modx_var <- NULL
        modx_lab <- NULL
      } else if (identical(mode, "moderated") &&
        (is.null(modx_var) || !modx_var %in% names(mf))) {
        return(echart_blank(paste0("No moderator specified for '", pred_var, "'."),
          height = height
        ))
      }

      tryCatch(
        {
          mm <- mm_of(fit)
          if (is.null(mm)) {
            return(echart_blank("Model matrix unavailable.", height = height))
          }
          betas <- stats::coef(fit)
          vcov_m <- .fixest_vcov(fit)
          n_grid <- 100L

          # Marginal effect gradient (verbatim from the ggplot builder).
          grad_w <- function(nm, x, mv) {
            if (identical(nm, pred_var)) {
              return(1)
            }
            if (.s1_is_poly_term(nm, pred_var, 2)) {
              return(2 * x)
            }
            if (.s1_is_poly_term(nm, pred_var, 3)) {
              return(3 * x^2)
            }
            if (grepl(":", nm, fixed = TRUE)) {
              parts <- strsplit(nm, ":", fixed = TRUE)[[1]]
              w <- 1
              has_x <- FALSE
              for (pp in parts) {
                if (identical(pp, pred_var)) {
                  has_x <- TRUE
                } else if (.s1_is_poly_term(pp, pred_var, 2)) {
                  w <- w * (2 * x)
                  has_x <- TRUE
                } else if (.s1_is_poly_term(pp, pred_var, 3)) {
                  w <- w * (3 * x^2)
                  has_x <- TRUE
                } else {
                  w <- w * mv
                }
              }
              return(if (has_x) w else 0)
            }
            0
          }

          slope_grid <- function(mv) {
            W <- matrix(0,
              nrow = length(x_seq), ncol = length(betas),
              dimnames = list(NULL, names(betas))
            )
            for (nm in colnames(mm)) {
              if (!nm %in% colnames(W)) next
              W[, nm] <- vapply(x_seq, function(xx) grad_w(nm, xx, mv), numeric(1))
            }
            ok <- !is.na(betas)
            est <- as.numeric(W[, ok, drop = FALSE] %*% betas[ok])
            se <- sqrt(pmax(0, rowSums((W[, ok, drop = FALSE] %*%
              vcov_m[ok, ok, drop = FALSE]) * W[, ok, drop = FALSE])))
            data.frame(x = x_seq, est = est, se = se)
          }

          .slope_scale <- function(d) {
            if (identical(effect_scale, "pct")) {
              f <- function(v) 100 * (exp(v) - 1)
              data.frame(
                x = d$x, fit = f(d$est),
                lo = f(d$est - 1.96 * d$se), hi = f(d$est + 1.96 * d$se)
              )
            } else if (identical(effect_scale, "pp")) {
              if (is.finite(profile_eta)) {
                f <- 100 * stats::plogis(profile_eta) * (1 - stats::plogis(profile_eta))
              } else {
                f <- 1
              }
              data.frame(
                x = d$x, fit = f * d$est,
                lo = f * (d$est - 1.96 * d$se), hi = f * (d$est + 1.96 * d$se)
              )
            } else if (identical(effect_scale, "pp100")) {
              data.frame(
                x = d$x, fit = 100 * d$est,
                lo = 100 * (d$est - 1.96 * d$se), hi = 100 * (d$est + 1.96 * d$se)
              )
            } else {
              data.frame(
                x = d$x, fit = d$est,
                lo = d$est - 1.96 * d$se, hi = d$est + 1.96 * d$se
              )
            }
          }

          mean_x <- mean(mm[[pred_var]], na.rm = TRUE)
          rug_x <- if (isTRUE(show_rug)) {
            rx <- mm[[pred_var]]
            rx <- rx[is.finite(rx)]
            rx
          } else {
            numeric(0)
          }

          x_seq <- seq(min(mm[[pred_var]], na.rm = TRUE),
            max(mm[[pred_var]], na.rm = TRUE),
            length.out = n_grid
          )

          curves <- if (!is.null(modx_var) && modx_var %in% names(mm)) {
            modx_col <- mm[[modx_var]]
            modx_uniq <- sort(unique(modx_col))
            is_cat_modx <- is.factor(modx_col) || is.character(modx_col) ||
              length(modx_uniq) <= 5

            modx_vals <- if (is_cat_modx) {
              modx_uniq
            } else {
              modx_mean <- mean(modx_col, na.rm = TRUE)
              modx_sd <- stats::sd(modx_col, na.rm = TRUE)
              c(modx_mean - modx_sd, modx_mean, modx_mean + modx_sd)
            }

            stats::setNames(
              lapply(modx_vals, function(mv) .slope_scale(slope_grid(mv))),
              vapply(
                modx_vals, function(v) modx_level_label(modx_lab, v),
                character(1)
              )
            )
          } else {
            list("Marginal effect" = .slope_scale(slope_grid(0)))
          }

          y_r <- range(unlist(lapply(curves, function(d) c(d$lo, d$hi))),
            na.rm = TRUE
          )
          pad <- 0.06 * (diff(y_r) %||% 1)
          zero_pad <- max(diff(y_r) * 0.05, 0.02)
          rug_y <- y_r[1] - pad
          y_min <- min(0, rug_y, y_r[1] - pad)
          y_max <- max(y_r[2] + pad, zero_pad)
          zero_is_boundary <- y_r[1] >= 0 || y_r[2] <= 0
          rug_series <- if (length(rug_x)) {
            list(list(
              name = "Observed", type = "scatter",
              data = lapply(rug_x, function(v) list(v, rug_y)),
              symbol = "rect", symbolSize = list(1.5, 9),
              itemStyle = list(color = .wise_slate, opacity = 0.15),
              silent = TRUE, z = 1, tooltip = list(show = FALSE),
              xAxisIndex = 0, yAxisIndex = 0
            ))
          } else {
            list()
          }

          mean_mark <- if (isTRUE(show_mean_ref) && is.finite(mean_x)) {
            list(list(
              xAxis = mean_x,
              lineStyle = list(color = .wise_slate, type = "dashed", width = 1),
              label = list(
                show = TRUE, position = "insideEndTop", formatter = "mean",
                color = .wise_slate, fontSize = 10
              )
            ))
          } else {
            list()
          }

          has_legend <- length(curves) > 1L
          cols <- stats::setNames(
            if (has_legend) .wise_cat[seq_along(curves)] else .wise_blue,
            names(curves)
          )

          ribbon_trios <- lapply(seq_along(curves), function(i) {
            d <- curves[[i]]
            col <- unname(cols[i])
            .e_ribbon_series(names(curves)[i], d$x, d$fit, d$lo, d$hi,
              fill = col, line = col, line_width = 2
            )
          })
          series <- unlist(ribbon_trios, recursive = FALSE)
          if (length(rug_series)) series <- c(series, rug_series)
          last_curve <- max(which(vapply(series, function(s) identical(s$type, "line") &&
            !isTRUE(s$silent), logical(1))), 0L)
          if (last_curve > 0L) {
            series[[last_curve]]$markLine <- markline_of(mean_mark)
            if (zero_is_boundary) {
              series[[last_curve]]$markLine$data[[1L]]$label <- list(
                show = TRUE, position = "start", formatter = "0",
                color = .wise_slate, fontSize = 11
              )
            }
          }

          e <- .e_new(height)
          e$x$opts$series <- series
          e$x$opts$xAxis <- list(list(
            type = "value", name = pred_x_lab,
            nameLocation = "middle", nameGap = 30,
            nameTextStyle = wise_eyaxis_name(),
           axisLabel = wise_eaxis_label(showMinLabel = FALSE, showMaxLabel = FALSE),
            splitLine = wise_esplit_line()
          ))
          e$x$opts$yAxis <- list(value_y_axis(
            y_label %||% paste("Change in", y_lab, "per +1 unit")
          ))
            e$x$opts$yAxis[[1]]$min <- y_min
            e$x$opts$yAxis[[1]]$max <- y_max
           x_sample <- mm[[pred_var]]
           x_sample <- x_sample[is.finite(x_sample)]
           if (length(x_sample)) {
             e$x$opts$xAxis[[1]]$min <- min(x_sample)
             e$x$opts$xAxis[[1]]$max <- max(x_sample)
           }
          if (has_legend) {
            e$x$opts$legend <- wise_elegend_style(
              data = lapply(seq_along(curves), function(i) list(
                name = names(curves)[i],
                icon = "roundRect",
                itemStyle = list(color = unname(cols[i]))
              )),
              left = "center", top = 0, orient = "horizontal"
            )
          }
          e$x$opts$grid <- list(
            containLabel = TRUE, left = 8, right = 20,
            top = if (has_legend) 54 else 36,
            bottom = grid_pad(FALSE, TRUE)
          )
          e$x$opts$tooltip <- .e_effect_tooltip(
            percent = effect_scale %in% c("pp", "pp100", "pct")
          )
          e <- .e_caption(e, cap_text)
          wise_echart_theme(e)
        },
        error = function(e) echart_blank(paste0("fixest effect plot error: ", conditionMessage(e)),
          height = height
        )
      )
    },
    error = function(e) echart_blank(paste0("Effect plot error: ", conditionMessage(e)),
      height = height
    )
  )
}



# Echarts counterparts of the Model fit figures (guidelines §7) ---------------

# Shared-bin histogram shares: counts of `x` in `breaks` as % of the pooled
# two-sample total, with bin midpoints as category labels. Mirrors the
# ggplot histogram prep (bins = 30, y = 100 * count / sum(count)).
.e_hist_shares <- function(x, breaks) {
  h <- graphics::hist(x, breaks = breaks, plot = FALSE)
  counts <- h$counts
  total <- sum(counts)
  data.frame(
    mid = (head(breaks, -1) + tail(breaks, -1)) / 2,
    share = if (is.finite(total) && total > 0) 100 * counts / total else rep(0, length(counts)),
    stringsAsFactors = FALSE
  )
}


#' Echarts importance plot (squared standardized coefficient shares)
#'
#' Interactive counterpart of [plot_importance()] (guidelines §7): horizontal
#' bars of each term's share of the sum of squared standardized coefficients,
#' same data preparation, largest share at the top. The ggplot subtitle is
#' carried by the section heading, so it is not repeated in the widget.
#'
#' @inheritParams plot_importance
#' @param height Widget height; a CSS length or a number of pixels.
#'
#' @return An `echarts4r` widget.
#'
#' @export
echart_importance <- function(model, label_fun = identity, height = "400px") {
  mm <- resolve_model_matrix(model)
  if (is.null(mm)) {
    return(echart_blank("Model matrix unavailable.", height = height))
  }

  coefs <- stats::coef(model)
  keep <- names(coefs) != "(Intercept)" & names(coefs) %in% names(mm)
  beta <- coefs[keep]
  if (!length(beta)) {
    return(echart_blank("No estimable terms.", height = height))
  }

  X <- mm[, names(beta), drop = FALSE]
  sd_x <- apply(X, 2, stats::sd, na.rm = TRUE)
  sd_x[is.na(sd_x)] <- 0

  imp <- abs(as.numeric(beta)) * as.numeric(sd_x)
  tot <- sum(imp^2)
  if (!is.finite(tot) || tot <= 0) {
    return(echart_blank("No variation to decompose.", height = height))
  }

  df <- data.frame(
    term = names(beta),
    share = 100 * imp^2 / tot,
    stringsAsFactors = FALSE
  )
  coef_map <- make_coef_map(df$term, label_fun)
  df$label <- unname(names(coef_map)[match(df$term, unname(coef_map))])
  df$label[is.na(df$label) | !nzchar(df$label)] <-
    df$term[is.na(df$label) | !nzchar(df$label)]
  df <- df[order(-df$share), , drop = FALSE]
  df <- utils::head(df, 15)

  # Category axes draw the first item at the bottom: feed ascending order so
  # the largest share lands on top, like the ggplot reorder().
  df <- df[order(df$share), , drop = FALSE]

  e <- .e_new(height)
  e$x$opts$series <- list(list(
    name = "Share", type = "bar",
    data = lapply(seq_len(nrow(df)), function(i) {
      list(
        value = df$share[i],
        label = list(
          show = TRUE, position = "right",
          formatter = sprintf("%.0f%%", df$share[i]),
          color = .wise_charcoal, fontSize = 11
        )
      )
    }),
    itemStyle = list(color = .wise_blue),
    barMaxWidth = 16
  ))
  e$x$opts$xAxis <- list(list(
    type = "value", name = "Share of explained variation (%)",
    nameLocation = "middle", nameGap = 30,
    nameTextStyle = wise_eaxis_name(align = "center"),
    axisLabel = wise_eaxis_label(
      formatter = htmlwidgets::JS("function(v){ return v + '%'; }")
    ),
    splitLine = wise_esplit_line()
  ))
  e$x$opts$yAxis <- list(list(
    type = "category", data = df$label,
    axisLabel = wise_eaxis_label(fontSize = 11),
    axisLine = list(lineStyle = list(color = .wise_grid)),
    splitLine = list(show = FALSE)
  ))
  e$x$opts$grid <- list(
    containLabel = TRUE, left = 8, right = 56, top = 20, bottom = 44,
    width = "auto", height = "auto"
  )
  e$x$opts$tooltip <- list(trigger = "item")
  wise_echart_theme(e)
}


#' Echarts residual diagnostic panels
#'
#' Interactive counterpart of [plot_residual_panels()] (guidelines §7).
#' Linear / LPM / RIF models: residuals-vs-fitted (with the same loess smooth)
#' beside a normal Q-Q plot in one widget with two echarts grids; the QQ
#' quantiles are precomputed in R with the same methods as `stat_qq`
#' (`ppoints` / `qnorm`) and the reference line through the quartiles.
#' Binary outcomes: binned residual means by decile of predicted risk with a
#' +/- 2-SE band, as in the ggplot builder.
#'
#' @inheritParams plot_residual_panels
#' @param height Widget height; a CSS length or a number of pixels.
#'
#' @return An `echarts4r` widget.
#'
#' @export
echart_residual_panels <- function(model, is_logistic = FALSE, height = "400px") {
  if (is_logistic) {
    # Binned residual means by decile of predicted risk (Gelman & Hill);
    # data preparation copied verbatim from plot_residual_panels().
    return(tryCatch(
      {
        p <- as.numeric(stats::fitted(model))
        res <- tryCatch(as.numeric(stats::residuals(model, type = "response")),
          error = function(e) as.numeric(stats::residuals(model))
        )
        n <- min(length(p), length(res))
        p <- p[seq_len(n)]
        res <- res[seq_len(n)]

        k <- max(3L, min(10L, floor(n / 20)))
        ord <- order(p)
        brks <- unique(floor(seq(0, n, length.out = k + 1)))
        if (length(brks) < 3) {
          return(echart_blank("Too few observations for binned residuals.",
            height = height
          ))
        }
        grp <- cut(seq_len(n), breaks = brks, include.lowest = TRUE)

        bdf <- data.frame(p = p[ord], res = res[ord], grp = grp)
        agg <- stats::aggregate(cbind(pred = p, mean_res = res) ~ grp,
          data = bdf, FUN = mean
        )
        cnt <- as.data.frame(table(bdf$grp))
        agg$n <- cnt$Freq[match(as.character(agg$grp), as.character(cnt$Var1))]
        agg$se <- vapply(split(bdf$res, bdf$grp), function(r) {
          if (length(r) > 1) stats::sd(r) / sqrt(length(r)) else NA_real_
        }, numeric(1))

        e <- .e_new(height)
        tri <- .e_ribbon_series(
          "Binned residuals", agg$pred, agg$mean_res,
          agg$mean_res - 2 * agg$se, agg$mean_res + 2 * agg$se,
          fill = .wise_blue, line = .wise_blue, line_width = 1.5,
          show_points = TRUE, point_size = 7
        )
        tri[[3]]$markLine <- .e_zero_line()
        tri[[3]]$tooltip <- .e_effect_tooltip()
        e$x$opts$series <- tri
        e$x$opts$xAxis <- list(list(
          type = "value", name = "Predicted risk (bin mean)",
          nameLocation = "middle", nameGap = 30,
          nameTextStyle = wise_eaxis_name(align = "center"),
          axisLabel = wise_eaxis_label(), splitLine = wise_esplit_line()
        ))
        e$x$opts$yAxis <- list(list(
          type = "value", scale = TRUE, name = "Mean residual in bin",
          nameLocation = "end",
          nameTextStyle = wise_eyaxis_name(),
          axisLabel = wise_eaxis_label(), splitLine = wise_esplit_line()
        ))
        e$x$opts$grid <- list(
          containLabel = TRUE, left = 8, right = 20, top = 36, bottom = 44
        )
        e$x$opts$tooltip <- .e_effect_tooltip()
        wise_echart_theme(e)
      },
      error = function(e) echart_blank(paste0(
        "Diagnostic plot error: ", conditionMessage(e)
      ), height = height)
    ))
  }

  # Linear / LPM / RIF: residuals vs fitted next to a normal QQ plot.
  tryCatch(
    {
      res <- as.numeric(stats::residuals(model))
      fitted <- as.numeric(stats::fitted(model))
      n <- min(length(fitted), length(res))
      df <- data.frame(fitted = fitted[seq_len(n)], residuals = res[seq_len(n)])

      e <- .e_new(height)
      e <- .e_multi_grids(e, 2, titles = c("Residuals vs fitted", "Normal Q-Q"))

      # Panel 1: scatter + loess smooth (ggplot geom_smooth(method = "loess")
      # defaults: span 0.75, degree 2, formula y ~ x).
      pts1 <- lapply(seq_len(nrow(df)), function(i) {
        list(df$fitted[i], df$residuals[i])
      })
      smooth <- tryCatch({
        keep <- is.finite(df$fitted) & is.finite(df$residuals)
        xs <- sort(unique(df$fitted[keep]))
        if (length(xs) >= 5) {
          lo <- stats::loess(residuals ~ fitted, data = df[keep, ],
            span = 0.75, degree = 2
          )
          # predict() returns a named vector; unname it so each point
          # serialises as [x, y] rather than [x, {"<name>": y}].
          pr <- unname(stats::predict(lo, newdata = data.frame(fitted = xs)))
          ok <- is.finite(pr)
          lapply(seq_along(xs)[ok], function(i) list(xs[i], pr[i]))
        } else {
          NULL
        }
      }, error = function(e) NULL)
      series1 <- list(list(
        name = "Residuals", type = "scatter", data = pts1,
        symbolSize = 4, z = 1,
        itemStyle = list(color = .wise_charcoal, opacity = 0.15),
        markLine = .e_zero_line()
      ))
      if (length(smooth) >= 2) {
        series1 <- c(series1, list(list(
          name = "Trend", type = "line", data = smooth,
          symbol = "none", z = 2,
          lineStyle = list(color = .wise_blue, width = 1.5),
          itemStyle = list(color = .wise_blue)
        )))
      }

      # Panel 2: normal QQ precomputed with stat_qq's methods (ppoints/qnorm)
      # and the stat_qq_line quartile reference.
      qq <- stats::qqnorm(df$residuals, plot.it = FALSE)
      pts2 <- lapply(seq_along(qq$x), function(i) list(qq$x[i], qq$y[i]))
      qs <- stats::quantile(df$residuals, c(0.25, 0.75), names = FALSE, na.rm = TRUE)
      xs <- stats::qnorm(c(0.25, 0.75))
      series2 <- list(list(
        name = "Sample quantiles", type = "scatter", data = pts2,
        symbolSize = 4, z = 1,
        itemStyle = list(color = .wise_charcoal, opacity = 0.3),
        markLine = list(
          symbol = "none", silent = TRUE, animation = FALSE,
          label = list(show = FALSE),
          lineStyle = list(color = .wise_blue, width = 1),
          data = list(list(
            list(coord = list(xs[1], qs[1])),
            list(coord = list(xs[2], qs[2]))
          ))
        )
      ))

      e$x$opts$series <- c(series1, series2)
      for (i in seq_along(series1)) {
        e$x$opts$series[[i]]$xAxisIndex <- 0L
        e$x$opts$series[[i]]$yAxisIndex <- 0L
      }
      s2_start <- length(series1) + 1L
      for (i in s2_start:length(e$x$opts$series)) {
        e$x$opts$series[[i]]$xAxisIndex <- 1L
        e$x$opts$series[[i]]$yAxisIndex <- 1L
      }
      e$x$opts$xAxis <- list(
        list(
          gridIndex = 0L,
          type = "value", scale = TRUE, name = "Fitted values",
          nameLocation = "middle", nameGap = 28,
          nameTextStyle = wise_eaxis_name(align = "center"),
          axisLabel = wise_eaxis_label(), splitLine = wise_esplit_line()
        ),
        list(
          gridIndex = 1L,
          type = "value", scale = TRUE, name = "Theoretical quantiles",
          nameLocation = "middle", nameGap = 28,
          nameTextStyle = wise_eaxis_name(align = "center"),
          axisLabel = wise_eaxis_label(), splitLine = wise_esplit_line()
        )
      )
      e$x$opts$grid <- lapply(seq_len(2L), function(i) list(
        left = if (i == 1L) "8%" else "54%",
        right = if (i == 1L) "54%" else "8%",
        top = 44, bottom = 58,
        width = "38%",
        height = "auto", containLabel = TRUE
      ))
      e$x$opts$title <- list(
        list(text = "Residuals vs fitted", left = "8%", top = 4,
          textStyle = list(color = .wise_charcoal, fontSize = 13, fontWeight = "normal")),
        list(text = "Normal Q-Q", left = "54%", top = 8,
          textStyle = list(color = .wise_charcoal, fontSize = 13, fontWeight = "normal"))
      )
      e$x$opts$yAxis <- list(
        list(
          gridIndex = 0L,
          type = "value", scale = TRUE, name = "Residuals",
          nameLocation = "end",
          nameTextStyle = wise_eyaxis_name(),
          axisLabel = wise_eaxis_label(), splitLine = wise_esplit_line()
        ),
        list(
          gridIndex = 1L,
          type = "value", scale = TRUE, name = "Sample quantiles",
          nameLocation = "end",
          nameTextStyle = wise_eyaxis_name(),
          axisLabel = wise_eaxis_label(), splitLine = wise_esplit_line()
        )
      )
      e$x$opts$tooltip <- list(show = FALSE)
      wise_echart_theme(e)
    },
    error = function(e) echart_blank(paste0(
      "Diagnostic plot error: ", conditionMessage(e)
    ), height = height)
  )
}


#' Echarts predicted vs actual distribution
#'
#' Interactive counterpart of [plot_pred_vs_actual()] (guidelines §7).
#' Linear models: dodged histogram of actual vs predicted values over 30
#' shared bins, y = share of households. Logistic models: calibration curve
#' (observed vs predicted rate by decile of predicted risk) with the diagonal
#' reference and +/- 2-SE binomial band.
#'
#' @inheritParams plot_pred_vs_actual
#' @param height Widget height; a CSS length or a number of pixels.
#'
#' @return An `echarts4r` widget.
#'
#' @export
echart_pred_vs_actual <- function(model, is_logistic, outcome_label = "outcome",
                                  height = "400px") {
  # Actual recovery copied verbatim from plot_pred_vs_actual().
  actual <- tryCatch(
    stats::model.frame(model)[[1]],
    error = function(e) {
      f <- tryCatch(stats::fitted(model), error = function(e) NULL)
      r <- tryCatch(stats::residuals(model), error = function(e) NULL)
      if (!is.null(f) && !is.null(r)) f + r else NULL
    }
  )

  if (is.null(actual)) {
    return(echart_blank("Could not recover outcome values from model.",
      height = height
    ))
  }

  if (!is_logistic) {
    predicted <- tryCatch(stats::fitted(model), error = function(e) stats::predict(model))
    n <- min(length(actual), length(predicted))
    actual <- actual[seq_len(n)]
    predicted <- predicted[seq_len(n)]

    all_vals <- c(actual, predicted)
    all_vals <- all_vals[is.finite(all_vals)]
    if (length(all_vals) < 2L || diff(range(all_vals)) <= 0) {
      return(echart_blank("Outcome distribution unavailable.", height = height))
    }
    brks <- seq(min(all_vals, na.rm = TRUE), max(all_vals, na.rm = TRUE),
      length.out = 31
    )
    ha <- .e_hist_shares(actual, brks)
    hp <- .e_hist_shares(predicted, brks)
    labels <- formatC(ha$mid, format = "f", digits = 2)
    observed_max <- max(c(ha$share, hp$share), na.rm = TRUE)
    y_max <- max(1, ceiling(observed_max * 1.12))
    y_interval <- y_max / 5

    e <- .e_new(height)
    bar <- function(d, nm, col) {
      list(
        name = nm, type = "bar",
        data = as.list(round(d$share, 4)),
        itemStyle = list(color = col, opacity = 0.7),
        barMaxWidth = 12
      )
    }
    e$x$opts$series <- list(
      bar(ha, "Actual", .wise_slate),
      bar(hp, "Predicted", .wise_blue)
    )
    e$x$opts$xAxis <- list(list(
      type = "category", data = as.list(labels),
      name = stringr::str_wrap(outcome_label, 40),
      nameLocation = "middle", nameGap = 30,
      nameTextStyle = wise_eaxis_name(align = "center"),
      axisLabel = wise_eaxis_label(fontSize = 10),
      axisTick = list(show = FALSE),
      splitLine = wise_esplit_line()
    ))
    e$x$opts$yAxis <- list(list(
      type = "value", min = 0, max = y_max, interval = y_interval,
      name = "Share of households (%)",
      nameLocation = "end",
      nameTextStyle = wise_eyaxis_name(),
      axisLabel = wise_eaxis_label(
        formatter = htmlwidgets::JS("function(v){ return v + '%'; }")
      ),
      splitLine = wise_esplit_line()
    ))
    e$x$opts$legend <- wise_elegend_style(
      data = list("Actual", "Predicted"),
      right = 36, top = 0, orient = "horizontal"
    )
    e$x$opts$grid <- list(
      containLabel = TRUE, left = 8, right = 20, top = 36, bottom = 46,
      width = "auto", height = "auto"
    )
    e$x$opts$tooltip <- list(
      trigger = "axis", axisPointer = list(type = "shadow"),
      valueFormatter = htmlwidgets::JS("function(v){ return Number(v).toFixed(1) + '%'; }")
    )
    return(wise_echart_theme(e))
  }

  # Logistic: calibration curve; data preparation copied from
  # plot_calibration().
  tryCatch(
    {
      predicted <- tryCatch(
        stats::fitted(model),
        error = function(e) {
          tryCatch(stats::predict(model, type = "response"),
            error = function(e2) NULL
          )
        }
      )
      n_bins <- 10L
      n <- min(length(actual), length(predicted))
      k <- max(3L, min(as.integer(n_bins), floor(n / 20)))
      ord <- order(as.numeric(predicted[seq_len(n)]))
      brks <- unique(floor(seq(0, n, length.out = k + 1)))
      if (length(brks) < 3) {
        return(echart_blank("Too few observations for calibration bins.",
          height = height
        ))
      }
      grp <- cut(seq_len(n), breaks = brks, include.lowest = TRUE)

      bdf <- data.frame(
        pred = as.numeric(predicted[seq_len(n)])[ord],
        obs = as.numeric(actual)[ord],
        grp = grp,
        stringsAsFactors = FALSE
      )
      cal <- stats::aggregate(cbind(pred, obs) ~ grp, data = bdf, FUN = mean)
      names(cal) <- c("grp", "pred", "obs")
      cnt <- as.data.frame(table(bdf$grp))
      cal$n <- cnt$Freq[match(as.character(cal$grp), as.character(cnt$Var1))]
      cal$se <- sqrt(pmax(cal$obs * (1 - cal$obs), 0) / pmax(cal$n, 1))

      e <- .e_new(height)
      tri <- .e_ribbon_series(
        "Calibration", cal$pred, cal$obs,
        pmax(0, cal$obs - 2 * cal$se), pmin(1, cal$obs + 2 * cal$se),
        fill = .wise_blue, line = .wise_blue, line_width = 1.5,
        show_points = TRUE, point_size = 7
      )
      tri[[3]]$markLine <- list(
        symbol = "none", silent = TRUE, animation = FALSE,
        label = list(show = FALSE),
        lineStyle = list(color = .wise_zero, type = "dashed", width = 1),
        data = list(list(
          list(coord = list(0, 0)), list(coord = list(1, 1))
        ))
      )
      e$x$opts$series <- tri
      e$x$opts$xAxis <- list(list(
        type = "value", min = 0, max = 1,
        name = "Predicted risk (bin mean)",
        nameLocation = "middle", nameGap = 30,
            nameTextStyle = wise_eyaxis_name(),
        axisLabel = wise_eaxis_label(), splitLine = wise_esplit_line()
      ))
      e$x$opts$yAxis <- list(list(
        type = "value", min = 0, max = 1,
        name = "Observed rate in bin",
        nameLocation = "end", nameTextStyle = wise_eyaxis_name(),
        axisLabel = wise_eaxis_label(), splitLine = wise_esplit_line()
      ))
      e$x$opts$grid <- list(
        containLabel = TRUE, left = 8, right = 20, top = 34, bottom = 46,
        width = "auto", height = "auto"
      )
      e$x$opts$tooltip <- .e_effect_tooltip(percent = TRUE)
      wise_echart_theme(e)
    },
    error = function(e) echart_blank(paste0(
      "Diagnostic plot error: ", conditionMessage(e)
    ), height = height)
  )
}


#' Echarts welfare histogram with RIF quantile markers
#'
#' The RIF branch of the "Predicted vs actual" figure: a histogram of the
#' original welfare outcome (30 bins, share of households) with dashed
#' markers at the estimated quantiles, matching the ggplot version drawn
#' inline in mod_1_08.
#'
#' @param y       Numeric vector of outcome values.
#' @param taus    Numeric vector of quantile probabilities to mark.
#' @param x_label X-axis label for the outcome variable.
#' @param height  Widget height; a CSS length or a number of pixels.
#'
#' @return An `echarts4r` widget.
#'
#' @export
echart_welfare_quantile_hist <- function(y, taus, x_label, height = "400px") {
  y <- y[is.finite(y)]
  if (length(y) < 2) {
    return(echart_blank("Outcome distribution unavailable.", height = height))
  }
  brks <- seq(min(y), max(y), length.out = 31)
  if (diff(range(brks)) == 0) {
    return(echart_blank("Outcome distribution unavailable.", height = height))
  }
  h <- .e_hist_shares(y, brks)
  labels <- formatC(h$mid, format = "f", digits = 2)
  y_max <- max(1, ceiling(max(h$share, na.rm = TRUE) * 1.12))

  q_vals <- stats::quantile(y, probs = taus, names = FALSE)
  tau_marks <- lapply(seq_along(taus), function(i) {
    idx <- which(h$mid >= q_vals[i])
    if (!length(idx)) idx <- length(h$mid)
    list(
      xAxis = idx[1],
      lineStyle = list(color = .wise_marker_alt, type = "dashed", width = 1),
      label = list(
        show = TRUE, position = "insideEndTop",
        formatter = paste0("\u03c4=", taus[i]),
        color = .wise_marker_alt, fontSize = 10
      )
    )
  })

  e <- .e_new(height)
  e$x$opts$series <- list(list(
    name = "Share", type = "bar",
    data = as.list(round(h$share, 4)),
    itemStyle = list(color = .wise_blue, opacity = 0.7),
    barMaxWidth = 12,
    markLine = list(
      symbol = "none", silent = TRUE, animation = FALSE,
      lineStyle = list(color = .wise_marker_alt, type = "dashed", width = 0.5),
      data = tau_marks
    )
  ))
  e$x$opts$xAxis <- list(list(
    type = "category", data = as.list(labels),
    name = stringr::str_wrap(x_label, 40),
    nameLocation = "middle", nameGap = 30,
    nameTextStyle = wise_eaxis_name(),
    axisLabel = wise_eaxis_label(fontSize = 10),
    axisTick = list(show = FALSE),
    splitLine = wise_esplit_line()
  ))
    e$x$opts$yAxis <- list(list(
      type = "value", min = 0, max = y_max, interval = y_max / 5,
      name = "Share of households (%)",
      nameLocation = "end",
      nameTextStyle = wise_eyaxis_name(),
    axisLabel = wise_eaxis_label(
      formatter = htmlwidgets::JS("function(v){ return v + '%'; }")
    ),
    splitLine = wise_esplit_line()
  ))
  e$x$opts$grid <- list(
    containLabel = TRUE, left = 8, right = 20, top = 14, bottom = 14
  )
    e$x$opts$tooltip <- list(
      trigger = "axis", axisPointer = list(type = "shadow"),
      valueFormatter = htmlwidgets::JS("function(v){ return Number(v).toLocaleString('en-US',{minimumFractionDigits:1,maximumFractionDigits:1}) + '%'; }")
    )
  wise_echart_theme(e)
}
