# ============================================================================ #
# Pure functions used by mod_2_01.
# Stateless and testable without Shiny.
#
# Shared constants (used across multiple modules):
#   RP_LOW            -- low-tail return period map  (name -> quantile prob)
#   RP_HIGH           -- high-tail return period map (name -> quantile prob)
#   SSP_SHORT_LABELS  -- canonical SSP key -> short display label
#
# Shared UI helpers (called inside module server functions):
#   residual_method_ui(ns, input_id)  -- radio buttons + helpText for residuals
#
# Simulation pipeline helper:
#   run_sim_pipeline()  -- weather join -> predict -> back-transform in one call

# ============================================================================ #

# ---- Internal colour / style helpers ---------------------------------------
# Used by enhance_exceedance() and plot_pointrange_climate(). Not exported.

# Canonical SSP keys must match what .normalise_ssp() returns
.ssp_colours <- c(
  "SSP2-4.5" = "#4dac26",   # green  (lower emissions)
  "SSP3-7.0" = "#2166ac",   # blue   (mid emissions)
  "SSP5-8.5" = "#c0392b"    # red    (high emissions)
)



# Normalise any SSP token found in a scenario name to a canonical key
# Strips trailing " / P{pct}" suffix first so keys like "SSP3-7.0 / 2030 / P50"
# are handled identically to the old "SSP3-7.0 / 2030" format.
# e.g. "SSP2" -> "SSP2-4.5", "SSP3-7.0" -> "SSP3-7.0", NA -> NA
.normalise_ssp <- function(nm) {
  nm_clean <- sub(" / P[0-9]+$", "", nm)
  m_full <- regmatches(nm_clean, regexpr("SSP[0-9]-[0-9.]+", nm_clean))
  if (length(m_full) > 0) return(m_full)
  m_short <- regmatches(nm_clean, regexpr("SSP([2-9])", nm_clean))
  if (length(m_short) == 0) return(NA_character_)
  digit  <- sub("SSP", "", m_short)
  lookup <- c("2" = "SSP2-4.5", "3" = "SSP3-7.0", "5" = "SSP5-8.5")
  if (digit %in% names(lookup)) lookup[[digit]] else NA_character_
}

# .parse_year <- function(nm) {
#   if (length(nm) == 1L) {
#     m <- regexpr("\\d{4}-\\d{4}", nm)
#     if (m == -1L) return(NA_character_)
#     return(regmatches(nm, m))
#   }
#   # Vectorised path
#   m <- gregexpr("\\d{4}-\\d{4}", nm)
#   vapply(seq_along(nm), function(i) {
#     matches <- regmatches(nm[i], m[i])[[1]]
#     if (length(matches) == 0) NA_character_ else matches[1]
#   }, character(1))
# }

.parse_year <- function(nm) {
  m <- regexpr("\\d{4}-\\d{4}", nm)
  out <- regmatches(nm, m)
  out[m == -1L] <- NA_character_
  out
}

# ---------------------------------------------------------------------------- #
# Shared constants                                                             #
# ---------------------------------------------------------------------------- #

#' Symmetric Return-Period Probability Maps
#'
#' Named numeric vectors mapping return-period labels to exceedance
#' probabilities. Used consistently across the threshold table and exceedance
#' curve in mod_2_06 and fct_sim_compare.
#'
#' \describe{
#'   \item{RP_LOW}{Rare low-outcome tail: 1:50, 1:20, 1:10, 1:5}
#'   \item{RP_HIGH}{Rare high-outcome tail: 4:5, 9:10, 19:20, 49:50}
#' }
#' @export
RP_LOW  <- c("1:50" = 0.02, "1:20" = 0.05, "1:10" = 0.10, "1:5" = 0.20)

#' @rdname RP_LOW
#' @export
RP_HIGH <- c("4:5" = 0.80, "9:10" = 0.90, "19:20" = 0.95, "49:50" = 0.98)

#' Short Display Labels for SSP Scenarios
#'
#' Maps canonical SSP keys (as returned by `.normalise_ssp()`) to short labels
#' used in UI checkboxes and plot legends.
#'
#' @export
SSP_SHORT_LABELS <- c(
  "SSP2-4.5" = "SSP2",
  "SSP3-7.0" = "SSP3",
  "SSP5-8.5" = "SSP5"
)

# ---------------------------------------------------------------------------- #
# Coefficient-draw constants and helpers                                       #
# ---------------------------------------------------------------------------- #

#' Format elapsed seconds into a human-readable string (e.g. "2m 14s")
#' Used by the simulation progress bar.
#' @noRd
format_elapsed <- function(secs) {
  secs <- round(secs)
  if (secs < 60L) return(sprintf("%ds", secs))
  sprintf("%dm %02ds", secs %/% 60L, secs %% 60L)
}

# SE clustering specification — confirmed default: ~loc_id
# Methodological justification: more conservative than ~loc_id:int_month
# (Moulton minimum). Absorbs within-location serial correlation across
# months and years. Weather data has real within-location temporal
# correlation that ~loc_id:int_month does not correct.
#
# Cluster count decision tree:
#   G >= 50 at ~loc_id : use ~loc_id (default)
#   40 <= G < 50        : use ~loc_id, flag in methodology note
#   G < 40              : warn user, wild cluster bootstrap recommended
#
# Named alternatives — available as constants, not default.
# Use compute_cluster_counts() to check G before switching.
COEF_VCOV_SPEC              <- ~loc_id
COEF_VCOV_SPEC_MOULTON      <- ~loc_id:int_month
COEF_VCOV_SPEC_CONSERVATIVE <- ~code + year + survname + loc_id

#' Compute Cluster Counts for SE Specification Diagnostics
#'
#' Computes the number of clusters at each relevant clustering level for the
#' fitted model data. Used to validate the SE specification and warn when
#' cluster counts are too small for reliable asymptotic inference.
#'
#' @param data Data frame used at model fit time. Must contain \code{loc_id}
#'   and \code{int_month} columns. Optionally \code{code}, \code{year},
#'   \code{survname} for the conservative multi-way spec.
#'
#' @return Named list with integer cluster counts:
#'   \describe{
#'     \item{loc_id}{Number of distinct location clusters (primary spec).}
#'     \item{loc_id_int_month}{Number of distinct location × month clusters
#'       (Moulton minimum).}
#'     \item{conservative}{Number of distinct code × year × survname × loc_id
#'       clusters (conservative multi-way spec). \code{NA} if any column
#'       is absent.}
#'   }
#'
#' @section Cluster count thresholds:
#' \itemize{
#'   \item G >= 50: reliable asymptotic inference, use \code{COEF_VCOV_SPEC}
#'   \item 40 <= G < 50: flag in methodology note
#'   \item G < 40: wild cluster bootstrap recommended (not yet implemented)
#' }
#'
#' @export
compute_cluster_counts <- function(data) {
  has_cols <- function(...) all(c(...) %in% names(data))

  g_loc        <- if (has_cols("loc_id"))
                    dplyr::n_distinct(data[["loc_id"]])
                  else NA_integer_

  g_loc_month  <- if (has_cols("loc_id", "int_month"))
                    dplyr::n_distinct(paste(data[["loc_id"]], data[["int_month"]]))
                  else NA_integer_

  g_conserv    <- if (has_cols("code", "year", "survname", "loc_id"))
                    dplyr::n_distinct(paste(data[["code"]], data[["year"]],
                                           data[["survname"]], data[["loc_id"]]))
                  else NA_integer_

  counts <- list(
    loc_id           = g_loc,
    loc_id_int_month = g_loc_month,
    conservative     = g_conserv
  )

  # Runtime warnings — surface immediately at model fit time
  if (!is.na(g_loc) && g_loc < 40L) {
    warning(sprintf(
      "[SE] Only %d clusters at ~loc_id. VCV SEs may be unreliable. Wild cluster bootstrap recommended (not yet implemented).",
      g_loc
    ))
  } else if (!is.na(g_loc) && g_loc < 50L) {
    message(sprintf(
      "[SE] %d clusters at ~loc_id — borderline. Flag in methodology note.",
      g_loc
    ))
  }

  counts
}

# ---------------------------------------------------------------------------- #
# Cholesky uncertainty propagation                                              #
# ---------------------------------------------------------------------------- #

#' Compute Cholesky Factor of Model VCV Matrix
#'
#' Computes the lower-triangular Cholesky factor of the coefficient covariance
#' matrix for the non-fixed-effect coefficients of a fitted \code{fixest}
#' model. Used once per fitted model — not per weather key.
#'
#' The Cholesky decomposition \eqn{L L' = \Sigma} allows efficient K-dimensional
#' Monte Carlo draws: instead of drawing full N-dimensional prediction vectors,
#' draw \eqn{z_s \sim N(0, I_K)} and compute the perturbation as
#' \eqn{F_i \cdot z_s} where \eqn{F = X_{nonFE} L'} is the factor loading
#' matrix computed once per key in \code{compute_factor_loading()}.
#'
#' @param fit A fitted \code{fixest} model object (from \code{fixest::feols()}).
#' @param vcov_spec A one-sided formula specifying the clustering structure for
#'   the VCV matrix. Defaults to \code{COEF_VCOV_SPEC} (\code{~loc_id}).
#'   See \code{COEF_VCOV_SPEC_MOULTON} and \code{COEF_VCOV_SPEC_CONSERVATIVE}
#'   for alternatives.
#'
#' @return A named list:
#'   \describe{
#'     \item{L}{K \eqn{\times} K lower-triangular Cholesky factor of
#'       \eqn{\Sigma}.}
#'     \item{K}{Integer. Number of non-FE coefficients.}
#'     \item{beta}{Named numeric vector of point-estimate non-FE
#'       coefficients.}
#'     \item{spec}{The VCV formula used.}
#'   }
#'
#' @seealso \code{\link{compute_factor_loading}},
#'   \code{\link{aggregate_with_uncertainty}}
#' @export
compute_chol_vcov <- function(fit, vcov_spec = COEF_VCOV_SPEC) {
  stopifnot(
    "fit must be a fixest model" = inherits(fit, "fixest")
  )

  beta  <- coef(fit)
  Sigma <- vcov(fit, vcov = vcov_spec)

  # Cholesky decomposition — chol() returns upper triangular R such that
  # t(R) %*% R == Sigma. We store L = t(R) (lower triangular) so that
  # L %*% t(L) == Sigma, consistent with the factor loading formula
  # F = X_nonFE %*% t(L).
  L <- tryCatch(
    t(chol(Sigma)),
    error = function(e) stop(sprintf(
      "[compute_chol_vcov] Cholesky decomposition failed. ",
      "Check that VCV matrix is positive definite at spec '%s': %s",
      deparse(vcov_spec), conditionMessage(e)
    ))
  )

  list(
    L    = L,
    K    = nrow(L),
    beta = beta,
    spec = vcov_spec
  )
}


#' Compute Factor Loading Matrix for Coefficient Uncertainty
#'
#' Computes the N \eqn{\times} K factor loading matrix
#' \eqn{F = X_{nonFE} L'} where \eqn{L} is the Cholesky factor from
#' \code{compute_chol_vcov()} and \eqn{X_{nonFE}} is the non-fixed-effect
#' design matrix for the counterfactual data.
#'
#' The factor loading encodes how coefficient uncertainty propagates to
#' prediction uncertainty for each household. Given a K-dimensional standard
#' normal draw \eqn{z_s \sim N(0, I_K)}, the perturbed log-welfare prediction
#' for household \eqn{i} under draw \eqn{s} is:
#' \deqn{y_i^{(s)} = y_i^{point} + F_i \cdot z_s}
#'
#' This is mathematically identical to the previous N \eqn{\times} S matrix
#' approach but requires only K-dimensional draws (K ~ 5-20) instead of
#' N-dimensional draws (N ~ 10,000), giving ~200x speedup.
#'
#' @param X_nonFE Numeric matrix. N \eqn{\times} K non-FE design matrix from
#'   \code{model.matrix(model, data = newdata, type = "rhs")}. Column names
#'   must match \code{names(chol_obj$beta)} exactly.
#' @param chol_obj Named list returned by \code{compute_chol_vcov()}.
#'
#' @return Numeric matrix of dimensions N \eqn{\times} K. Each row \eqn{i}
#'   is the factor loading vector for household \eqn{i}.
#'
#' @seealso \code{\link{compute_chol_vcov}},
#'   \code{\link{aggregate_with_uncertainty}}
#' @export
compute_factor_loading <- function(X_nonFE, chol_obj) {
  stopifnot(
    "X_nonFE must be a numeric matrix"        = is.matrix(X_nonFE) && is.numeric(X_nonFE),
    "chol_obj must contain L and beta"        = all(c("L", "beta") %in% names(chol_obj)),
    "X_nonFE columns must match chol_obj$beta names" =
      identical(colnames(X_nonFE), names(chol_obj$beta))
  )

  # F = X_nonFE %*% t(L)   →   N × K loading matrix
  # Each row i: F_i = x_i' L'  such that  F_i F_i' = x_i' Σ x_i
    X_nonFE %*% chol_obj$L  # N×K — uses L (lower triangular Cholesky factor).
                            # Note: earlier versions incorrectly used t(chol_obj$L)
}



#' Draw Coefficient Vectors from a Fitted Model
#'
#' Generates \code{S} draws of the full coefficient vector for propagating
#' parameter uncertainty into counterfactual simulations. The parametric
#' (VCV) branch draws from the multivariate normal defined by the fitted
#' coefficient vector and its variance-covariance matrix. Bootstrap branches
#' are stubbed so call sites do not need to change when they are implemented.
#'
#' @param fit      A fitted \code{feols} model object.
#' @param S        Integer. Number of draws. Default 500.
#' @param method   Character. One of \code{"vcov"} (default),
#'   \code{"bootstrap"}, \code{"wild_bootstrap"}. The latter two are
#'   not yet implemented.
#' @param vcov_spec Formula or character passed to \code{vcov(fit, vcov = ...)}.
#'   Defaults to \code{COEF_VCOV_SPEC}.
#' @param seed     Optional integer seed for reproducibility.
#'
#' @return An \code{S x K} numeric matrix with column names matching
#'   \code{coef(fit)}.
#'
#' @export
draw_coefs <- function(fit,
                       S         = 500,
                       method    = c("vcov", "bootstrap", "wild_bootstrap"),
                       vcov_spec = COEF_VCOV_SPEC,
                       seed      = NULL) {
  method <- match.arg(method)
  if (!is.null(seed)) set.seed(seed)

  if (method == "vcov") {
    beta_hat <- coef(fit)
    Sigma    <- vcov(fit, vcov = vcov_spec)
    draws    <- MASS::mvrnorm(n = S, mu = beta_hat, Sigma = Sigma)
    # Ensure matrix form even when S = 1
    if (is.null(dim(draws))) {
      draws <- matrix(draws, nrow = 1L, dimnames = list(NULL, names(beta_hat)))
    }
    return(draws)
  }

  stop(sprintf("draw_coefs: method '%s' not yet implemented", method))
}

# ---------------------------------------------------------------------------- #
# Shared UI helper                                                             #
# ---------------------------------------------------------------------------- #

#' Residual Method Radio Buttons UI
#'
#' Produces a consistent `radioButtons` widget + `helpText` block for choosing
#' how prediction residuals are handled. Used in both `mod_2_01_historical` and
#' `mod_2_03_future` to avoid duplicating the same 24-line block.
#'
#' @param ns       The module namespace function (from `session$ns`).
#' @param input_id The input id for the radio buttons (unnamespaced).
#'
#' @return A `tagList` containing `radioButtons` and `helpText`.
#' @export
residual_method_ui <- function(ns, input_id) {
  shiny::tagList(
    shiny::radioButtons(
      inputId  = ns(input_id),
      label    = "Residuals method",
      choices  = residual_choices(),
      selected = "original"
    ),
    shiny::helpText(
      shiny::tags$b("none:"), " return fitted values only.", shiny::tags$br(),
      shiny::tags$b("original:"), " match each observation's own training residual",
      " by ID, preserving individual-level heterogeneity across simulation years.",
      shiny::tags$br(),
      shiny::tags$b("empirical:"), " resample residuals from the training",
      " distribution (non-parametric bootstrap).", shiny::tags$br(),
      shiny::tags$b("normal:"), " draw residuals from N(0, \u03c6) where \u03c6",
      " is the training residual SD.",
      style = "font-size:11px;"
    )
  )
}

# ---------------------------------------------------------------------------- #
# Simulation pipeline helper                                                   #
# ---------------------------------------------------------------------------- #

#' Resolve the ID Column for Residual Matching
#'
#' Returns the first of `c("pid", "hhid", "fid")` that exists in both data
#' frames, or `NULL` if none match.  Used by `run_sim_pipeline()` to enable
#' ID-based residual matching when `residuals = "original"`.
#' @noRd
resolve_id_col <- function(a, b) {
  candidates <- c("pid", "hhid", "fid")
  shared     <- intersect(names(a), names(b))
  match      <- candidates[candidates %in% shared]
  if (length(match) == 0L) NULL else match[[1L]]
}

#' Run Simulation Pipeline for One Weather Key
#'
#' Prepares counterfactual survey data for one weather key, computes point-
#' estimate log-welfare predictions, and returns the factor loading matrix for
#' downstream coefficient uncertainty propagation via
#' \code{aggregate_with_uncertainty()}.
#'
#' This function is called once per weather key (historical + future
#' representatives). It does NOT draw coefficient perturbations — all
#' uncertainty propagation is deferred to display time via
#' \code{aggregate_with_uncertainty()}, making poverty line, weights, and
#' aggregation method fully reactive without re-simulation.
#'
#' @param weather_raw Data frame. One weather key's prepared data from
#'   \code{get_weather()} or \code{summarise_ensemble()}.
#' @param svy Data frame. Survey microdata joined to weather reference data.
#' @param sw One-row data frame of selected weather variable metadata.
#' @param so One-row data frame of selected outcome variable metadata.
#' @param model Fitted \code{fixest} model object.
#' @param residuals Character. Residual treatment passed through to
#'   \code{aggregate_with_uncertainty()}. One of \code{"none"},
#'   \code{"original"}, \code{"normal"}, \code{"resample"}.
#' @param train_data Data frame. Training data used to fit \code{model}.
#'   Used to compute training residuals for \code{train_aug}.
#' @param engine Character. Model engine identifier (e.g. \code{"fixest"}).
#' @param chol_obj Named list from \code{compute_chol_vcov()} or \code{NULL}.
#'   When \code{NULL}, \code{F_loading} in the return value is \code{NULL}
#'   (point estimates only — no coefficient uncertainty).
#'
#' @return Named list or \code{NULL} on prediction failure:
#'   \describe{
#'     \item{y_point}{Numeric vector length N. Log-scale point-estimate welfare
#'       predictions. Back-transformation via \code{exp()} happens inside
#'       \code{aggregate_with_uncertainty()}, not here.}
#'     \item{F_loading}{N \eqn{\times} K numeric matrix from
#'       \code{compute_factor_loading()}, or \code{NULL} when
#'       \code{chol_obj = NULL}.}
#'     \item{sim_year}{Integer vector length N. Simulation year per row.}
#'     \item{weight}{Numeric vector length N or \code{NULL}. Survey weights.}
#'     \item{id_vec}{Vector length N or \code{NULL}. Household IDs for
#'       \code{residuals = "original"} matching.}
#'     \item{id_col}{Character or \code{NULL}. Name of the ID column.}
#'     \item{n_pre_join}{Integer. Number of survey rows before weather join.}
#'     \item{weather_raw}{Data frame. The input weather key data (for
#'       diagnostics).}
#'     \item{train_aug}{Data frame. Training data augmented with \code{.fitted}
#'       and \code{.resid} columns for residual drawing in
#'       \code{aggregate_with_uncertainty()}.}
#'   }
#'
#' @seealso \code{\link{aggregate_with_uncertainty}},
#'   \code{\link{compute_chol_vcov}}, \code{\link{compute_factor_loading}}
#' @export
run_sim_pipeline <- function(weather_raw,
                             svy,
                             sw,
                             so,
                             model,
                             residuals,
                             train_data,
                             engine,
                             chol_obj = NULL) {

  n_pre_join    <- nrow(svy)
  survey_wd_sim <- prepare_hist_weather(weather_raw, svy, sw, so$name)

  # Resolve ID column for "original" residual matching
  id_col <- if (residuals == "original")
               resolve_id_col(train_data, survey_wd_sim)
             else
               NULL

  # ---- Point estimate prediction ------------------------------------------ #
  # predict_outcome() called ONCE — no coefficient draws here.
  # All uncertainty propagation is deferred to aggregate_with_uncertainty().
  pred_out <- tryCatch(
    predict_outcome(
      model      = model,
      newdata    = survey_wd_sim,
      residuals  = "none",     # residuals drawn at display time, not here
      outcome    = so$name,
      id         = id_col,
      train_data = train_data,
      engine     = engine
    ),
    error = function(e) {
      warning("[run_sim_pipeline] predict_outcome() failed: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(pred_out)) {
    rm(survey_wd_sim)
    return(NULL)
  }

  # y_point stays log-scale — back-transformation happens inside
  # aggregate_with_uncertainty() after coefficient perturbation.
  # NOTE: apply_log_backtransform() is NOT called here.
  y_point  <- pred_out$.fitted

  # ---- Simulation year and weights ---------------------------------------- #
  sim_year <- pred_out$sim_year

  wt_col   <- grep("^weight$|^hhweight$|^wgt$|^pw$", names(pred_out),
                   value = TRUE, ignore.case = TRUE)
  weight   <- if (length(wt_col) > 0L) pred_out[[wt_col[[1L]]]] else NULL

  id_vec   <- if (!is.null(id_col) && id_col %in% names(pred_out))
                pred_out[[id_col]]
              else
                NULL

  # ---- Factor loading matrix ---------------------------------------------- #
  # Computed once per key — not per draw.
  # F_loading = X_nonFE %*% L  where L is the Cholesky factor of Sigma.
  # NULL when chol_obj = NULL (point estimates only).
  F_loading <- NULL
  if (!is.null(chol_obj)) {
    X_nonFE <- tryCatch(
      model.matrix(model, data = survey_wd_sim, type = "rhs"),
      error = function(e) {
        warning("[run_sim_pipeline] model.matrix() failed: ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(X_nonFE)) {
      # Column alignment guard — surfaces misalignment bugs immediately
      stopifnot(
        "X_nonFE columns must match chol_obj$beta names" =
          identical(colnames(X_nonFE), names(chol_obj$beta))
      )
      F_loading <- compute_factor_loading(X_nonFE, chol_obj)
    }
  }

  # ---- Training augmentation for residual drawing ------------------------- #
  # train_aug carries .resid for "original" and "resample" residual paths
  # inside aggregate_with_uncertainty(). Computed once per pipeline call.
  train_aug <- tryCatch({
    fitted_train <- as.numeric(stats::predict(model, newdata = train_data))
    train_data |>
      dplyr::mutate(
        .fitted = fitted_train,
        .resid  = !!rlang::sym(so$name) - fitted_train
      )
  }, error = function(e) {
    warning("[run_sim_pipeline] train_aug computation failed: ", conditionMessage(e))
    NULL
  })
  
  # Store prepared weather data for diagnostics plots — has loc_id, int_month
  # joined from svy. Used by plot_weather_density_panel() in mod_2_03.
  weather_prepared <- survey_wd_sim
  rm(survey_wd_sim)

  list(
    y_point     = y_point,
    F_loading   = F_loading,
    sim_year    = sim_year,
    weight      = weight,
    id_vec      = id_vec,
    id_col      = id_col,
    n_pre_join  = n_pre_join,
    weather_raw = weather_raw,
    weather_prepared = weather_prepared,
    train_aug   = train_aug
  )
}

# ---------------------------------------------------------------------------- #
# Simulation date grid                                                         #
# ---------------------------------------------------------------------------- #

#' Build the Date Grid for the Historical Simulation
#'
#' Constructs a vector of first-of-month dates covering every combination of
#' interview month present in `survey_weather` and every year in the requested
#' historical period. These dates are passed directly to `get_weather()` as the
#' `dates` argument.
#'
#' @param survey_weather A data frame containing at least an `int_month` column
#'   (integer, 1-12) representing the interview month of each survey
#'   observation.
#' @param year_range A length-2 integer vector `c(start_year, end_year)`
#'   defining the historical period to simulate over.
#'
#' @return A `Date` vector of first-of-month dates (one per month x year
#'   combination).
#'
#' @examples
#' svy <- data.frame(int_month = c(3L, 6L, 9L))
#' build_hist_sim_dates(svy, c(2000L, 2002L))
#'
#' @export
build_hist_sim_dates <- function(survey_weather, year_range) {
  months <- unique(survey_weather$int_month)
  years  <- seq(year_range[1], year_range[2])

  with(
    expand.grid(int_month = months, int_year = years),
    as.Date(paste(int_year, int_month, "01", sep = "-"))
  )
}

# ---------------------------------------------------------------------------- #
# Residual choice helpers                                                      #
# ---------------------------------------------------------------------------- #

#' Available residual handling choices
#'
#' @return A named character vector suitable for use in `radioButtons()`.
#' @export
residual_choices <- function() {
  c(
    "Original" = "original",
    "Resample"  = "resample",
    "Normal"     = "normal",
    "None"      = "none"
  )
}


# ---------------------------------------------------------------------------- #
# Perturbation method helper                                                   #
# ---------------------------------------------------------------------------- #

#' Build a Perturbation Method Vector for Climate Simulations
#'
#' Derives the named `perturbation_method` vector required by `get_weather()`
#' when an SSP scenario is active. Precipitation and similar accumulation
#' variables (units `"mm"` or `"days"`) use `"multiplicative"` scaling;
#' all other variables (e.g. temperature in `"°C"`) use `"additive"` delta.
#'
#' @param selected_weather A data frame with columns `name` and `units`, as
#'   returned by `build_selected_weather()`.
#'
#' @return A named character vector with one entry per row in
#'   `selected_weather`, values being `"additive"` or `"multiplicative"`.
#'
#' @export
build_perturbation_method <- function(selected_weather) {
  method <- ifelse(
    selected_weather$units %in% c("mm", "days"),
    "multiplicative",
    "additive"
  )
  stats::setNames(method, selected_weather$name)
}


# ---------------------------------------------------------------------------- #
# Weather preparation for simulation                                           #
# ---------------------------------------------------------------------------- #

#' Prepare Historical Weather Data for Simulation
#'
#' Takes raw output from `get_weather()` and joins it back to the survey frame,
#' adding `sim_year` and ensuring `year` is a factor consistent with the
#' training data. Weather and outcome columns from the survey frame are dropped
#' before the join to avoid duplication.
#'
#' @param weather_raw A data frame returned by `get_weather()`, containing at
#'   least columns `code`, `year`, `survname`, `loc_id`, `int_month`, and
#'   `timestamp`.
#' @param survey_weather A data frame of the merged survey-weather training
#'   data. Must contain columns `code`, `year`, `survname`, `loc_id`, and
#'   `int_month`.
#' @param selected_weather A data frame of selected weather variable metadata
#'   with at least a `name` column.
#' @param outcome_name A character string giving the name of the outcome column
#'   in `survey_weather` to exclude before the join.
#'
#' @return A tibble with the weather variables from `weather_raw` joined to the
#'   survey covariate columns from `survey_weather`, with additional columns
#'   `sim_year` (integer) and `year` (factor).
#'
#' @importFrom dplyr mutate select inner_join any_of
#' @export
prepare_hist_weather <- function(weather_raw,
                                 survey_weather,
                                 selected_weather,
                                 outcome_name) {
  drop_cols <- c(selected_weather$name, outcome_name)

    weather_raw |>
    dplyr::mutate(
      year      = as.character(year),
      int_month = as.integer(format(timestamp, "%m")),
      sim_year  = as.integer(format(timestamp, "%Y"))
    ) |>
    dplyr::select(-timestamp) |>
    dplyr::inner_join(
      survey_weather |>
        dplyr::mutate(year = as.character(year)) |>
        dplyr::select(-dplyr::any_of(drop_cols)),
      by = c("code", "year", "survname", "loc_id", "int_month")
    ) |>
    dplyr::mutate(year = as.factor(year))
}


# ---------------------------------------------------------------------------- #
# Back-transformation                                                          #
# ---------------------------------------------------------------------------- #

#' Back-Transform a Log-Transformed Outcome Column
#'
#' Exponentiates the named outcome column in `preds` when `so$transform` is
#' `"log"`. Returns `preds` unchanged for any other transformation or when
#' `so$transform` is `NULL` / `NA`.
#'
#' @param preds A data frame of predictions from `predict_outcome()`.
#' @param so A one-row data frame of outcome metadata as returned by
#'   `build_selected_outcome()`. Must contain columns `name` and `transform`.
#'
#' @return `preds` with the outcome column exponentiated if applicable.
#'
#' @importFrom dplyr mutate
#' @importFrom rlang sym .data
#' @export
apply_log_backtransform <- function(preds, so) {
  if (!isTRUE(so$transform == "log")) return(preds)

  preds |>
    dplyr::mutate(!!rlang::sym(so$name) := exp(.data[[so$name]]))
}