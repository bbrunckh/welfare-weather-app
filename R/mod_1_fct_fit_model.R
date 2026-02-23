#' Fit progressive weather-welfare regression models
#'
#' Fits three nested models of increasing complexity to estimate the effect of
#' weather hazards on a welfare outcome. The three models are:
#' \enumerate{
#'   \item Weather terms only (no fixed effects, no controls)
#'   \item Weather terms + fixed effects (as regular covariates)
#'   \item Weather terms + fixed effects + all user-selected controls
#' }
#'
#' The function uses \pkg{parsnip} for a unified model interface, making it
#' straightforward to extend to additional model types in the future by adding
#' cases to the \code{model_type} switch.
#'
#' Fixed effects are included as standard categorical covariates (i.e. dummy-
#' encoded by the underlying engine) rather than via a dedicated FE estimator.
#' For large panels with many FE levels, consider switching to the \pkg{fixest}
#' engine in the future.
#'
#' Weather variables are treated according to two optional properties in
#' \code{selected_weather}:
#' \itemize{
#'   \item If \code{$cont_binned == "Binned"}, the variable is coerced to a
#'     factor before fitting.
#'   \item If \code{$polynomial} is \code{2} or \code{3}, quadratic and/or
#'     cubic terms are added to the formula using \code{I(x^2)} / \code{I(x^3)}.
#'     Polynomial terms are only added for continuous (non-binned) variables.
#' }
#'
#' @param df A \code{data.frame} containing the merged survey-weather dataset.
#'   All variables referenced in \code{selected_outcome}, \code{selected_weather},
#'   and \code{selected_model} must be present as columns.
#'
#' @param selected_outcome A named list describing the outcome variable, with:
#'   \describe{
#'     \item{\code{$name}}{Character. Column name of the outcome in \code{df}.}
#'     \item{\code{$type}}{Character. One of \code{"logical"} (binary outcome)
#'       or \code{"numeric"} / \code{"integer"} (continuous outcome). Used to
#'       validate that logistic regression is only applied to binary outcomes.}
#'   }
#'
#' @param selected_weather A named list or data frame describing weather hazard
#'   variables, with the following fields per weather variable:
#'   \describe{
#'     \item{\code{$name}}{Character vector of length 1 or 2. Column name(s) of
#'       weather hazard variable(s) in \code{df}.}
#'     \item{\code{$cont_binned}}{Character vector (same length as \code{$name}).
#'       \code{"Binned"} indicates the variable should be treated as a factor;
#'       any other value (e.g. \code{"Continuous"}) leaves it numeric.}
#'     \item{\code{$polynomial}}{Integer vector (same length as \code{$name}).
#'       \code{2} adds a quadratic term \code{I(x^2)}; \code{3} adds both
#'       quadratic and cubic terms \code{I(x^2) + I(x^3)}. Only applied to
#'       continuous (non-binned) variables. \code{1} or \code{NA} adds no
#'       polynomial terms.}
#'   }
#'
#' @param selected_model A named list of model specifications, as returned by
#'   \code{mod_1_06_model_server()}, with:
#'   \describe{
#'     \item{\code{$type}}{Character. One of \code{"Linear regression"} or
#'       \code{"Logistic regression"}.}
#'     \item{\code{$fixedeffects}}{Character vector of fixed effect variable
#'       names. These are included as standard covariates and factor-coerced
#'       automatically.}
#'     \item{\code{$interactions}}{Character vector of length 0 or 1. Name of
#'       the moderator variable to interact with each weather hazard term.
#'       Generates \code{haz:moderator} product terms.}
#'     \item{\code{$hh_covariates}}{Character vector of household-level
#'       covariate names (used in model 3 only).}
#'     \item{\code{$area_covariates}}{Character vector of area-level covariate
#'       names (used in model 3 only).}
#'     \item{\code{$ind_covariates}}{Character vector of individual-level
#'       covariate names (used in model 3 only).}
#'     \item{\code{$firm_covariates}}{Character vector of firm-level covariate
#'       names (used in model 3 only).}
#'   }
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{\code{fit1}}{A \pkg{parsnip} model fit object. Weather terms only.}
#'     \item{\code{fit2}}{A \pkg{parsnip} model fit object. Weather terms +
#'       fixed effects.}
#'     \item{\code{fit3}}{A \pkg{parsnip} model fit object. Weather terms +
#'       fixed effects + all controls.}
#'     \item{\code{weather_terms}}{Character vector of base weather variable
#'       names (without polynomial or interaction terms).}
#'     \item{\code{interaction_terms}}{Character vector of interaction term
#'       strings (e.g. \code{"haz_var:urban"}). Empty if no moderator selected.}
#'     \item{\code{fe_terms}}{Character vector of fixed effect variable names.}
#'     \item{\code{y_var}}{Character. Name of the outcome variable.}
#'     \item{\code{model_type}}{Character. Either \code{"linear"} or
#'       \code{"logistic"}.}
#'     \item{\code{formulas}}{Named list of the three \code{formula} objects
#'       used (\code{formula1}, \code{formula2}, \code{formula3}), useful for
#'       inspection and reporting.}
#'   }
#'
#' @section Extending to new model types:
#' To add a new model type (e.g. random forest), add a case to the
#' \code{switch()} block in the "Specify parsnip model" section and update
#' \code{mod_1_06_model_server()} to expose the new option in the UI.
#'
#' @importFrom parsnip linear_reg logistic_reg set_engine fit
#' @importFrom stats as.formula
#'
#' @examples
#' \dontrun{
#' result <- fit_weather_model(
#'   df               = survey_weather_df,
#'   selected_outcome = list(name = "welfare", type = "numeric"),
#'   selected_weather = list(
#'     name        = c("spei", "tmax_anom"),
#'     cont_binned = c("Continuous", "Binned"),
#'     polynomial  = c(2, 1)
#'   ),
#'   selected_model   = list(
#'     type            = "Linear regression",
#'     fixedeffects    = c("int_year", "admin1"),
#'     interactions    = "urban",
#'     hh_covariates   = c("hhsize", "edu_head"),
#'     area_covariates = "nightlights",
#'     ind_covariates  = character(0),
#'     firm_covariates = character(0)
#'   )
#' )
#' result$fit3
#' result$formulas$formula3
#' }
#'
#' @noRd
fit_weather_model <- function(df, selected_outcome, selected_weather, selected_model) {

  # ---------------------------------------------------------------------------
  # 1. Unpack and clean inputs
  # ---------------------------------------------------------------------------

  y_var        <- selected_outcome$name
  outcome_type <- selected_outcome$type

  weather_vars <- selected_weather$name
  weather_vars <- weather_vars[nzchar(weather_vars) & !is.na(weather_vars)]

  # Per-variable weather properties — recycle to length of weather_vars if needed
  n_weather    <- length(weather_vars)
  cont_binned  <- rep_len(selected_weather$cont_binned  %||% "Continuous", n_weather)
  polynomial <- if (is.list(selected_weather$polynomial)) {
    rep_len(selected_weather$polynomial, n_weather)
  } else {
    rep_len(list(selected_weather$polynomial %||% character(0)), n_weather)
  }

  interaction_vars <- selected_model$interactions %||% character(0)
  interaction_vars <- interaction_vars[nzchar(interaction_vars) & !is.na(interaction_vars)]

  fe_vars <- selected_model$fixedeffects %||% character(0)
  fe_vars <- fe_vars[nzchar(fe_vars) & !is.na(fe_vars)]

  covariate_vars <- unique(c(
    selected_model$hh_covariates,
    selected_model$area_covariates,
    selected_model$ind_covariates,
    selected_model$firm_covariates
  ))
  covariate_vars <- covariate_vars[nzchar(covariate_vars) & !is.na(covariate_vars)]

  # ---------------------------------------------------------------------------
  # 2. Validate inputs
  # ---------------------------------------------------------------------------

  if (!y_var %in% names(df)) {
    stop(sprintf("Outcome variable '%s' not found in data.", y_var))
  }
  missing_weather <- setdiff(weather_vars, names(df))
  if (length(missing_weather) > 0) {
    stop(sprintf("Weather variable(s) not found in data: %s", paste(missing_weather, collapse = ", ")))
  }
  if (length(weather_vars) == 0) {
    stop("At least one weather variable must be selected.")
  }

  use_logit <- identical(selected_model$type, "Logistic regression")

  if (use_logit) {
    if (!identical(outcome_type, "logical")) {
      warning("Logistic regression requested but outcome type is not 'logical' — falling back to linear regression.")
      use_logit <- FALSE
    } else {
      y_vals <- df[[y_var]][!is.na(df[[y_var]])]
      if (!all(y_vals %in% c(0, 1, TRUE, FALSE))) {
        warning("Outcome values are not 0/1 — falling back to linear regression.")
        use_logit <- FALSE
      } else {
        df[[y_var]] <- factor(df[[y_var]], levels = c(0, 1))
      }
    }
  }

  # ---------------------------------------------------------------------------
  # 3. Prepare weather variables in df
  #
  # Binned variables: coerce to factor.
  # Continuous variables: leave as-is (polynomial terms handled in formula).
  # ---------------------------------------------------------------------------

  for (i in seq_along(weather_vars)) {
    v <- weather_vars[i]
    if (identical(cont_binned[i], "Binned")) {
      df[[v]] <- as.factor(df[[v]])
    }
  }

  # Coerce fixed effect variables to factors
  for (fe in fe_vars) {
    if (fe %in% names(df)) df[[fe]] <- as.factor(df[[fe]])
  }

  # ---------------------------------------------------------------------------
  # 4. Build formula terms
  #
  # For each weather variable:
  #   - Binned (factor): just the variable name; R expands dummies automatically.
  #   - Continuous, polynomial == 1: just the variable name.
  #   - Continuous, polynomial == 2: var + I(var^2)
  #   - Continuous, polynomial == 3: var + I(var^2) + I(var^3)
  #
  # Interaction terms (haz:moderator) are only added for the base variable name,
  # not for polynomial terms, to keep the model interpretable.
  # ---------------------------------------------------------------------------

  # Build the set of formula terms for each weather variable
  weather_formula_terms <- unlist(lapply(seq_along(weather_vars), function(i) {
    v         <- weather_vars[i]
    is_binned <- identical(cont_binned[i], "Binned")
    poly      <- if (is_binned) character(0) else (polynomial[[i]] %||% character(0))

    # poly is a character vector of selected degrees e.g. character(0), "2", c("2","3")
    terms <- v
    if (!is_binned) {
      if ("2" %in% poly) terms <- c(terms, sprintf("I(%s^2)", v))
      if ("3" %in% poly) terms <- c(terms, sprintf("I(%s^3)", v))
    }
    terms
  }))

  # Interaction terms: all weather formula terms × moderator (including polynomials).
  # e.g. for spei with polynomial=2 and moderator=urban:
  #   spei:urban + I(spei^2):urban
  # I(x^2):moderator is valid R formula syntax and correctly computes the
  # product of the squared term with the moderator.
  if (length(interaction_vars) > 0) {
    # For each weather formula term, create a fully saturated term with each moderator
    interaction_formula_terms <- as.vector(outer(
      weather_formula_terms, interaction_vars,
      FUN = function(h, m) paste0(h, " * ", m)
    ))

    # Colon terms for reporting only (not used in formula)
    interaction_terms <- as.vector(outer(
      weather_formula_terms, interaction_vars,
      FUN = function(h, m) paste0(h, ":", m)
    ))

    # The * expansion includes weather main effects and moderator main effects,
    # so we only need the interaction_formula_terms on the RHS — no need to
    # separately list weather_formula_terms or interaction_vars
    rhs_weather <- interaction_formula_terms

  } else {
    interaction_formula_terms <- character(0)
    interaction_terms         <- character(0)
    rhs_weather               <- weather_formula_terms
  }

  terms1 <- rhs_weather
  terms2 <- c(rhs_weather, fe_vars)
  terms3 <- c(rhs_weather, fe_vars, covariate_vars)

  build_formula <- function(y, rhs_terms) {
    # unique() would drop duplicate I(x^2) strings — safe here since each is distinct
    rhs_terms <- rhs_terms[nzchar(rhs_terms) & !is.na(rhs_terms)]
    rhs_terms <- rhs_terms[!duplicated(rhs_terms)]
    if (length(rhs_terms) == 0) rhs_terms <- "1"
    stats::as.formula(paste(y, "~", paste(rhs_terms, collapse = " + ")))
  }

  formula1 <- build_formula(y_var, terms1)
  formula2 <- build_formula(y_var, terms2)
  formula3 <- build_formula(y_var, terms3)

  # ---------------------------------------------------------------------------
  # 5. Specify parsnip model
  #
  # To add a new model type, add a case here and expose it in mod_1_06_model_ui.
  # ---------------------------------------------------------------------------

  model_spec <- switch(
    selected_model$type,

    "Linear regression" = parsnip::linear_reg() |>
      parsnip::set_engine("lm"),

    "Logistic regression" = parsnip::logistic_reg() |>
      parsnip::set_engine("glm"),

    # Future model types — uncomment and implement as needed:
    # "Random forest"     = parsnip::rand_forest() |> parsnip::set_engine("ranger"),
    # "Gradient boosting" = parsnip::boost_tree()  |> parsnip::set_engine("xgboost"),

    stop(sprintf("Unknown model type: '%s'", selected_model$type))
  )

  # Override to linear if logistic fallback was triggered
  if (identical(selected_model$type, "Logistic regression") && !use_logit) {
    model_spec <- parsnip::linear_reg() |> parsnip::set_engine("lm")
  }

  # ---------------------------------------------------------------------------
  # 6. Fit the three models
  # ---------------------------------------------------------------------------
  fit_one <- function(formula, label) {
    tryCatch(
      parsnip::fit(model_spec, formula = formula, data = df),
      error = function(e) {
        stop(sprintf("Model '%s' failed: %s", label, conditionMessage(e)))
      }
    )
  }

  # Drop rows with NA in any variable used by the most complete model (fit3)
  # to avoid "missing value where TRUE/FALSE needed" from lm/glm internals
  vars_used <- unique(c(y_var, weather_formula_terms, interaction_vars,
                        fe_vars, covariate_vars))
  vars_used <- vars_used[vars_used %in% names(df)]
  df <- df[complete.cases(df[, vars_used, drop = FALSE]), ]

  if (nrow(df) == 0) stop("No complete cases after dropping NA rows.")

  fit1 <- fit_one(formula1, "weather only")
  fit2 <- fit_one(formula2, "weather + FE")
  fit3 <- fit_one(formula3, "weather + FE + controls")

  # ---------------------------------------------------------------------------
  # 7. Return
  # ---------------------------------------------------------------------------

  list(
    fit1              = fit1,
    fit2              = fit2,
    fit3              = fit3,
    weather_terms     = weather_vars,        # base names only, for plots/labels
    interaction_terms = interaction_terms,   # "haz:moderator" strings
    fe_terms          = fe_vars,
    y_var             = y_var,
    model_type        = if (use_logit) "logistic" else "linear",
    formulas          = list(
      formula1 = formula1,
      formula2 = formula2,
      formula3 = formula3
    )
  )
}