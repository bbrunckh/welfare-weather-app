# ============================================================================ #
# Pure functions for model type and covariate selection logic.                 #
# Used by mod_1_06_model_server(). Stateless and testable without Shiny.      #
# ============================================================================ #


# ---------------------------------------------------------------------------- #
# Policy scenario definitions                                                   #
# ---------------------------------------------------------------------------- #

#' Predefined policy scenario definitions
#'
#' Each entry maps a short key to a human-readable label and a character vector
#' of variable names that the policy requires in the model.
#'
#' @export
POLICY_DEFINITIONS <- list(
  "A" = list(
    label = "Energy / Electricity access",
    vars  = c("electricity")
  ),
  "B" = list(
    label = "Drinking Water access",
    vars  = c("imp_wat_rec")
  ),
  "C" = list(
    label = "Sanitation access",
    vars  = c("imp_san_rec")
  ),
  "D" = list(
    label = "Health access",
    vars  = c("ttime_health")
  ),
  "E" = list(
    label = "Internet access",
    vars  = c("internet")
  ),
  "F" = list(
    label = "Mobile phone access",
    vars  = c("cellphone")
  )
)

#' Named vector of policy choices for selectize input
#' @return Named character vector (display label → key).
#' @export
get_policy_choices <- function() {
  setNames(
    names(POLICY_DEFINITIONS),
    vapply(POLICY_DEFINITIONS, `[[`, character(1), "label")
  )
}

#' Determine which variables are locked by selected policies, split by level
#'
#' Returns a level-based list so that policy availability can be checked against
#' the current analysis level (ind/hh/firm/area). The locked variables are added
#' to the model as interaction terms with the weather hazard.
#'
#' @param selected_policies Character vector of policy keys (e.g. \code{"A"}).
#' @param variable_list     Data frame with \code{name} column and role flags
#'   \code{ind}, \code{hh}, \code{firm}, \code{area}.
#'
#' @return Named list with elements \code{ind}, \code{hh}, \code{firm},
#'   \code{area}, each a character vector of locked variable names.
#' @export
get_policy_locked_vars <- function(selected_policies, variable_list = NULL) {
  empty <- list(ind = character(0), hh = character(0),
                firm = character(0), area = character(0))
  if (is.null(selected_policies) || length(selected_policies) == 0) return(empty)
  if (is.null(variable_list) || nrow(variable_list) == 0) return(empty)

  all_vars <- unique(unlist(lapply(selected_policies, function(k) {
    POLICY_DEFINITIONS[[k]]$vars
  })))

  vl <- variable_list[variable_list$name %in% all_vars, , drop = FALSE]
  if (nrow(vl) == 0) return(empty)

  get_role <- function(role) {
    v <- vl[[role]]
    if (is.null(v)) return(character(0))
    vl$name[!is.na(v) & v == 1L]
  }
  list(ind = get_role("ind"), hh = get_role("hh"),
       firm = get_role("firm"), area = get_role("area"))
}


# ---------------------------------------------------------------------------- #
# Variable list filtering                                                       #
# ---------------------------------------------------------------------------- #

#' Filter a variable list to columns present and sufficiently non-missing in df
#'
#' A variable is considered valid when it exists as a column in `df` and at
#' least `min_complete` proportion of its values are non-missing.
#'
#' @param df            A data frame (e.g. `survey_weather()`).
#' @param variable_list A data frame with at minimum a `name` column.
#' @param min_complete  Numeric in \[0, 1\]. Minimum proportion non-missing.
#'   Default `0.5`.
#'
#' @return The filtered `variable_list`.
#'
#' @export
filter_valid_vars <- function(df, variable_list, min_complete = 0.5) {
  if (is.null(df) || is.null(variable_list)) return(variable_list)
  valid <- names(df)[colMeans(!is.na(df)) >= min_complete]
  variable_list[variable_list$name %in% valid, , drop = FALSE]
}


#' Filter a variable list to rows matching a role flag
#'
#' Selects rows where the column named by `role` equals `1`. Supported roles:
#' `"fe"`, `"hh"`, `"ind"`, `"area"`, `"firm"`, `"interact"`.
#'
#' @param variable_list A data frame with a column named by `role`.
#' @param role          Scalar character naming the role column.
#' @param extra_filter  Optional named list of additional column/value
#'   inequality filters (e.g. `list(type = "numeric")` to exclude numeric rows).
#'
#' @return The filtered `variable_list`.
#'
#' @export
filter_vars_by_role <- function(variable_list, role, extra_filter = NULL) {
  if (is.null(variable_list) || nrow(variable_list) == 0) return(variable_list)
  if (!role %in% names(variable_list)) return(variable_list[0L, , drop = FALSE])

  out <- variable_list[variable_list[[role]] == 1L, , drop = FALSE]

  if (!is.null(extra_filter)) {
    for (col in names(extra_filter)) {
      if (col %in% names(out)) {
        out <- out[out[[col]] != extra_filter[[col]], , drop = FALSE]
      }
    }
  }

  out
}


# ---------------------------------------------------------------------------- #
# Model type helpers                                                            #
# ---------------------------------------------------------------------------- #

#' Model type choices and radio-button label for a given outcome type
#'
#' @param outcome_type Scalar character. `"logical"` for binary outcomes;
#'   `"numeric"` / `"double"` / `"integer"` for continuous outcomes.
#'
#' @return A named list with:
#'   \describe{
#'     \item{`choices`}{Character vector of model type choices.}
#'     \item{`label`}{Scalar character label for the radio button group.}
#'   }
#'
#' @export
model_type_choices <- function(outcome_type) {
  if (identical(outcome_type, "logical")) {
    list(
      choices = c("Logistic regression", "Linear regression"),
      label   = "Classification model:"
    )
  } else {
    list(
      choices = c("Linear regression", "Quantile regression (RIF)"),
      label   = "Regression model:"
    )
  }
}


# ---------------------------------------------------------------------------- #
# Covariate exclusion                                                           #
# ---------------------------------------------------------------------------- #

#' Remove already-selected variables from a candidate variable list
#'
#' Filters out variables already used as the outcome, weather predictors,
#' interaction terms, or fixed effects.
#'
#' @param candidate_vl  Data frame with a `name` column.
#' @param outcome_name  Scalar character. Selected outcome variable name.
#' @param weather_names Character vector. Selected weather variable names.
#' @param interactions  Character vector. Selected interaction variable names.
#' @param fixedeffects  Character vector. Selected fixed effect names.
#'
#' @return `candidate_vl` with excluded rows removed.
#'
#' @export
exclude_selected_vars <- function(candidate_vl,
                                   outcome_name  = character(0),
                                   weather_names = character(0),
                                   interactions  = character(0),
                                   fixedeffects  = character(0)) {
  if (is.null(candidate_vl) || nrow(candidate_vl) == 0) return(candidate_vl)

  exclude <- unique(c(
    outcome_name  %||% character(0),
    weather_names %||% character(0),
    interactions  %||% character(0),
    fixedeffects  %||% character(0)
  ))

  candidate_vl[!candidate_vl$name %in% exclude, , drop = FALSE]
}


# ---------------------------------------------------------------------------- #
# Engine inference                                                               #
# ---------------------------------------------------------------------------- #

#' Infer the fitting engine from the model type
#'
#' Linear and logistic regression default to `"fixest"` (high-dimensional FE
#' via `feols`/`feglm`). Tree-based types map to `"ranger"` or `"xgboost"`
#' respectively.
#'
#' @param model_type Scalar character. E.g. `"Linear regression"`.
#'
#' @return Scalar character engine key matching an `ENGINE_REGISTRY` entry.
#'
#' @export
infer_engine <- function(model_type) {
  switch(
    model_type %||% "",
    "Linear regression"       = "fixest",
    "Logistic regression"     = "fixest",
    "Random forest"           = "ranger",
    "XGBoost"                 = "xgboost",
    "Quantile regression (RIF)" = "rif",
    "fixest"  # safe default
  )
}


# ---------------------------------------------------------------------------- #
# Spec assembly                                                                 #
# ---------------------------------------------------------------------------- #

#' Assemble the full model selection specification
#'
#' Collects all model configuration inputs into a single named list with
#' `NULL`-safe defaults.
#'
#' @param model_type          Scalar character. E.g. `"Linear regression"`.
#' @param engine              Scalar character. Engine key from `ENGINE_REGISTRY`.
#'   When `NULL` (default), inferred automatically from `model_type` via
#'   `infer_engine()`: linear/logistic -> `"fixest"`, random forest ->
#'   `"ranger"`, XGBoost -> `"xgboost"`.
#' @param interactions        Character vector of interaction variable names.
#' @param interaction_mode    Scalar character. `"saturated"` (default) or
#'   `"pairwise"`. See `fit_model()` for details.
#' @param fixedeffects        Character vector of fixed effect variable names.
#' @param covariate_selection Scalar character. `"User-defined"` or `"Lasso"`.
#' @param ind_covariates      Character vector of individual covariate names.
#' @param hh_covariates       Character vector of household covariate names.
#' @param firm_covariates     Character vector of firm covariate names.
#' @param area_covariates     Character vector of area covariate names.
#' @param cluster             Character vector of clustering variable names for
#'   clustered standard errors (supported by compatible engines).
#' @param lasso_alpha         Numeric in \[0, 1\]. Elastic net mixing parameter
#'   (`1` = Lasso, `0` = Ridge). Default `1`.
#' @param lasso_lambda        Scalar character. Lambda selection rule:
#'   `"lambda.1se"` (default, more regularisation) or `"lambda.min"`.
#' @param lasso_nfolds        Integer. Number of cross-validation folds. Default `10`.
#' @param lasso_standardize   Logical. Whether to standardise predictors before
#'   fitting. Default `TRUE`.
#' @param mi_m                Integer. Number of multiple imputations. Default `5`.
#' @param mi_maxit            Integer. Number of imputation iterations. Default `5`.
#' @param stability_threshold Numeric in \[0, 1\]. Minimum selection frequency
#'   across imputations for a variable to be retained. Default `0.5`.
#'
#' @return A named list. All `NULL` inputs are coerced to their defaults.
#'   Lasso/MI fields are always present but only meaningful when
#'   `covariate_selection == "Lasso"`.
#'
#' @export
build_selected_model <- function(model_type          = NULL,
                                  engine              = NULL,
                                  interactions        = NULL,
                                  interaction_mode    = NULL,
                                  fixedeffects        = NULL,
                                  covariate_selection = NULL,
                                  ind_covariates      = NULL,
                                  hh_covariates       = NULL,
                                  firm_covariates     = NULL,
                                  area_covariates     = NULL,
                                  cluster             = NULL,
                                  lasso_alpha         = NULL,
                                  lasso_lambda        = NULL,
                                  lasso_nfolds        = NULL,
                                  lasso_standardize   = NULL,
                                  mi_m                = NULL,
                                  mi_maxit            = NULL,
                                  stability_threshold = NULL) {
  chr0 <- character(0)
  list(
    type                = model_type          %||% chr0,
    engine              = engine              %||% infer_engine(model_type),
    interactions        = interactions        %||% chr0,
    interaction_mode    = interaction_mode    %||% "pairwise",
    fixedeffects        = fixedeffects        %||% chr0,
    covariate_selection = covariate_selection %||% "User-defined",
    ind_covariates      = ind_covariates      %||% chr0,
    hh_covariates       = hh_covariates       %||% chr0,
    firm_covariates     = firm_covariates     %||% chr0,
    area_covariates     = area_covariates     %||% chr0,
    cluster             = cluster             %||% chr0,
    # Lasso / MI options — only meaningful when covariate_selection == "Lasso"
    lasso_alpha         = lasso_alpha         %||% 1,
    lasso_lambda        = lasso_lambda        %||% "lambda.1se",
    lasso_nfolds        = lasso_nfolds        %||% 10L,
    lasso_standardize   = lasso_standardize   %||% TRUE,
    mi_m                = mi_m                %||% 5L,
    mi_maxit            = mi_maxit            %||% 5L,
    stability_threshold = stability_threshold %||% 0.5
  )
}
