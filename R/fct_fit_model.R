# ---------------------------------------------------------------------------- #
# fit_model.R                                                                   #
# ---------------------------------------------------------------------------- #
#
# Architecture: backend dispatch
# --------------------------------
# Each supported engine is registered in `ENGINE_REGISTRY` as a named list
# with four fields:
#
#   $requires       – character vector of package names that must be installed
#   $model_types    – character vector of model types the engine supports
#                     (must match values of selected_model$type)
#   $build_formulas – function(y_var, terms, fe_vars) -> named list of formulae
#                     with elements formula1, formula2, formula3
#   $fit_one        – function(formula, data, model_type, model_spec, opts) ->
#                     a fitted model object
#   $make_spec      – function(model_type, use_logit) -> parsnip model spec
#                     (or NULL for non-parsnip engines)
#   $prepare_outcome – function(df, y_var, use_logit) -> df with outcome
#                     coerced to the type expected by this engine
#
# To add a new engine (e.g. "ranger", "xgboost"):
#   1. Add an entry to ENGINE_REGISTRY below.
#   2. Implement the four fields.
#   3. Update predict_outcome.R to handle the new fitted-object class.
#
# No other changes to this function are needed.
#
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Engine registry                                                                #
# ---------------------------------------------------------------------------- #

ENGINE_REGISTRY <- list(

  # -------------------------------------------------------------------------- #
  # fixest (feols / feglm) — high-dimensional fixed effects                    #
  # -------------------------------------------------------------------------- #
  fixest = list(

    requires    = "fixest",
    model_types = c("Linear regression", "Logistic regression"),

    # Fixed effects absorbed via | syntax: y ~ x1 + x2 | fe1 + fe2
    build_formulas = function(y_var, terms, fe_vars) {
      build <- function(rhs_main, rhs_fe = character(0)) {
        rhs_main <- unique(rhs_main[nzchar(rhs_main) & !is.na(rhs_main)])
        if (length(rhs_main) == 0) rhs_main <- "1"
        rhs_fe <- rhs_fe[nzchar(rhs_fe) & !is.na(rhs_fe)]
        rhs <- if (length(rhs_fe) > 0) {
          paste(paste(rhs_main, collapse = " + "), "|",
                paste(rhs_fe,   collapse = " + "))
        } else {
          paste(rhs_main, collapse = " + ")
        }
        stats::as.formula(paste(y_var, "~", rhs))
      }
      list(
        formula1 = build(terms$weather),
        formula2 = build(c(terms$weather, terms$interactions_main), fe_vars),
        formula3 = build(c(terms$weather, terms$interactions_main,
                           terms$covariates),                        fe_vars)
      )
    },

    fit_one = function(formula, data, model_type, model_spec, opts) {
      args <- c(list(fml = formula, data = data), opts$fixest)
      if (model_type == "logistic") {
        args$family <- stats::binomial("logit")
        do.call(fixest::feglm, args)
      } else {
        do.call(fixest::feols, args)
      }
    },

    make_spec = function(model_type, use_logit) NULL,

    prepare_outcome = function(df, y_var, use_logit) {
      # feglm needs integer 0/1, not a factor
      if (use_logit) df[[y_var]] <- as.integer(as.logical(df[[y_var]]))
      df
    }
  ),

  # -------------------------------------------------------------------------- #
  # Random forest via parsnip + ranger                                          #
  # -------------------------------------------------------------------------- #
  # Notes:
  #   * FE variables are passed as regular features (no absorbing).
  #   * Polynomial / interaction formula terms are included because parsnip
  #     passes the formula to the underlying engine, which evaluates I(x^2) etc.
  #     For tree-based models this is usually unnecessary — the model learns
  #     non-linearities automatically. Keeping them is harmless but redundant.
  #   * Only "Linear regression" (regression mode) is wired here. Add
  #     "Logistic regression" support by switching set_mode("classification").
  # -------------------------------------------------------------------------- #
  ranger = list(

    requires    = c("parsnip", "ranger"),
    model_types = c("Linear regression"),

    build_formulas = function(y_var, terms, fe_vars) {
      build <- function(rhs) {
        rhs <- unique(rhs[nzchar(rhs) & !is.na(rhs)])
        if (length(rhs) == 0) rhs <- "1"
        stats::as.formula(paste(y_var, "~", paste(rhs, collapse = " + ")))
      }
      list(
        formula1 = build(terms$weather),
        formula2 = build(c(terms$weather, fe_vars)),
        formula3 = build(c(terms$weather, fe_vars, terms$covariates))
      )
    },

    fit_one = function(formula, data, model_type, model_spec, opts) {
      parsnip::fit(model_spec, formula = formula, data = data)
    },

    make_spec = function(model_type, use_logit) {
      parsnip::rand_forest(trees = 500, min_n = 5) |>
        parsnip::set_engine("ranger", importance = "impurity") |>
        parsnip::set_mode("regression")
    },

    prepare_outcome = function(df, y_var, use_logit) df
  ),

  # -------------------------------------------------------------------------- #
  # XGBoost via parsnip + xgboost                                               #
  # -------------------------------------------------------------------------- #
  # Notes:
  #   * Same feature considerations as ranger above.
  #   * Hyperparameters below are reasonable defaults; expose additional fields
  #     via selected_model for user-tunable control.
  # -------------------------------------------------------------------------- #
  xgboost = list(

    requires    = c("parsnip", "xgboost"),
    model_types = c("Linear regression", "Logistic regression"),

    build_formulas = function(y_var, terms, fe_vars) {
      build <- function(rhs) {
        rhs <- unique(rhs[nzchar(rhs) & !is.na(rhs)])
        if (length(rhs) == 0) rhs <- "1"
        stats::as.formula(paste(y_var, "~", paste(rhs, collapse = " + ")))
      }
      list(
        formula1 = build(terms$weather),
        formula2 = build(c(terms$weather, fe_vars)),
        formula3 = build(c(terms$weather, fe_vars, terms$covariates))
      )
    },

    fit_one = function(formula, data, model_type, model_spec, opts) {
      parsnip::fit(model_spec, formula = formula, data = data)
    },

    make_spec = function(model_type, use_logit) {
      mode <- if (model_type == "logistic" && use_logit) "classification" else "regression"
      parsnip::boost_tree(
        trees          = 500,
        tree_depth     = 6,
        learn_rate     = 0.05,
        loss_reduction = 0,
        min_n          = 5
      ) |>
        parsnip::set_engine("xgboost") |>
        parsnip::set_mode(mode)
    },

    prepare_outcome = function(df, y_var, use_logit) {
      # xgboost classification needs a factor outcome
      if (use_logit) df[[y_var]] <- factor(df[[y_var]], levels = c(0, 1))
      df
    }
  )

  # -------------------------------------------------------------------------- #
  # Template for a new engine — copy and fill in
  # -------------------------------------------------------------------------- #
  # my_engine = list(
  #   requires    = c("parsnip", "my_pkg"),
  #   model_types = c("Linear regression"),
  #
  #   build_formulas = function(y_var, terms, fe_vars) { ... },
  #   fit_one        = function(formula, data, model_type, model_spec, opts) { ... },
  #   make_spec      = function(model_type, use_logit) { ... },
  #   prepare_outcome = function(df, y_var, use_logit) df
  # )
)

# ---------------------------------------------------------------------------- #
# fit_model()                                                                   #
# ---------------------------------------------------------------------------- #

#' Fit progressive weather-welfare regression models
#'
#' Fits three nested models of increasing complexity:
#' \enumerate{
#'   \item Weather terms only
#'   \item Weather terms + fixed effects
#'   \item Weather terms + fixed effects + all controls
#' }
#'
#' The fitting backend is selected via \code{selected_model$engine} and
#' dispatched through \code{ENGINE_REGISTRY}. Adding support for a new engine
#' only requires a new entry in that registry — no changes to this function.
#'
#' @param df A \code{data.frame} containing all variables.
#' @param selected_outcome Named list with \code{$name} (outcome column) and
#'   \code{$type} (\code{"logical"}, \code{"numeric"}, or \code{"integer"}).
#' @param selected_weather Named list with \code{$name} (weather variable
#'   name(s)), \code{$cont_binned} (\code{"Binned"} or \code{"Continuous"}),
#'   and \code{$polynomial} (\code{"2"}, \code{"3"}, or \code{character(0)}).
#' @param selected_model Named list with:
#'   \describe{
#'     \item{\code{$type}}{Model type string, e.g. \code{"Linear regression"}.}
#'     \item{\code{$engine}}{Engine key matching an \code{ENGINE_REGISTRY}
#'       entry. Inferred from \code{$type} via \code{infer_engine()} when not
#'       set explicitly: linear/logistic -> \code{"fixest"}, random forest ->
#'       \code{"ranger"}, XGBoost -> \code{"xgboost"}.}
#'     \item{\code{$interaction_mode}}{\code{"saturated"} (default) or
#'       \code{"pairwise"}. Saturated crosses all moderators simultaneously
#'       (\code{haz * mod1 * mod2}); pairwise generates independent pairs.}
#'     \item{\code{$fixedeffects}}{Character vector of FE variable names.}
#'     \item{\code{$interactions}}{Character vector of moderator variable names.}
#'     \item{\code{$hh_covariates}, \code{$area_covariates},
#'       \code{$ind_covariates}, \code{$firm_covariates}}{Control variables.}
#'     \item{\code{$cluster}}{Variable name(s) for clustered SEs (fixest only).}
#'   }
#'
#' @return Named list with \code{fit1}, \code{fit2}, \code{fit3},
#'   \code{weather_terms}, \code{interaction_terms}, \code{fe_terms},
#'   \code{y_var}, \code{model_type}, \code{engine}, \code{train_data},
#'   and \code{formulas}.
#'
#' @noRd
fit_model <- function(df, selected_outcome, selected_weather, selected_model) {

  # ---------------------------------------------------------------------------
  # 1. Unpack inputs
  # ---------------------------------------------------------------------------

  y_var        <- selected_outcome$name
  outcome_type <- selected_outcome$type

  weather_vars <- selected_weather$name
  weather_vars <- weather_vars[nzchar(weather_vars) & !is.na(weather_vars)]
  if (length(weather_vars) == 0) stop("At least one weather variable must be selected.")

  n_weather   <- length(weather_vars)
  cont_binned <- rep_len(selected_weather$cont_binned %||% "Continuous", n_weather)
  polynomial  <- if (is.list(selected_weather$polynomial)) {
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

  # Engine lookup
  engine_key <- tolower(selected_model$engine %||% "fixest")
  if (!engine_key %in% names(ENGINE_REGISTRY)) {
    stop(sprintf(
      "Unknown engine '%s'. Available engines: %s",
      engine_key, paste(names(ENGINE_REGISTRY), collapse = ", ")
    ))
  }
  backend <- ENGINE_REGISTRY[[engine_key]]

  # Check required packages are installed
  missing_pkgs <- setdiff(backend$requires, rownames(utils::installed.packages()))
  if (length(missing_pkgs) > 0) {
    stop(sprintf(
      "Engine '%s' requires package(s) not installed: %s",
      engine_key, paste(missing_pkgs, collapse = ", ")
    ))
  }

  # ---------------------------------------------------------------------------
  # 2. Validate
  # ---------------------------------------------------------------------------

  if (!y_var %in% names(df))
    stop(sprintf("Outcome variable '%s' not found in data.", y_var))

  missing_weather <- setdiff(weather_vars, names(df))
  if (length(missing_weather) > 0)
    stop(sprintf("Weather variable(s) not found in data: %s",
                 paste(missing_weather, collapse = ", ")))

  if (!selected_model$type %in% backend$model_types) {
    stop(sprintf(
      "Engine '%s' does not support model type '%s'. Supported types: %s",
      engine_key, selected_model$type,
      paste(backend$model_types, collapse = ", ")
    ))
  }

  use_logit <- identical(selected_model$type, "Logistic regression")

  if (use_logit) {
    if (!identical(outcome_type, "logical")) {
      warning("Logistic regression requested but outcome type is not 'logical' — falling back to linear.")
      use_logit <- FALSE
    } else {
      y_vals <- df[[y_var]][!is.na(df[[y_var]])]
      if (!all(y_vals %in% c(0, 1, TRUE, FALSE))) {
        warning("Outcome values are not 0/1 — falling back to linear.")
        use_logit <- FALSE
      }
    }
  }

  model_type <- if (use_logit) "logistic" else "linear"

  # ---------------------------------------------------------------------------
  # 3. Prepare variables in df
  # ---------------------------------------------------------------------------

  # Outcome coercion delegated to backend (factor, integer, or unchanged)
  df <- backend$prepare_outcome(df, y_var, use_logit)

  # ---------------------------------------------------------------------------
  # 4. Build formula terms
  # ---------------------------------------------------------------------------

  weather_formula_terms <- unlist(lapply(seq_along(weather_vars), function(i) {
    v         <- weather_vars[i]
    is_binned <- identical(cont_binned[i], "Binned")
    poly      <- if (is_binned) character(0) else (polynomial[[i]] %||% character(0))
    terms     <- v
    if (!is_binned) {
      if ("2" %in% poly) terms <- c(terms, sprintf("I(%s^2)", v))
      if ("3" %in% poly) terms <- c(terms, sprintf("I(%s^3)", v))
    }
    terms
  }))

  # ---------------------------------------------------------------------------
  # 4. Build interaction formula terms
  #
  # Two modes, selected via selected_model$interaction_mode:
  #
  #   "pairwise"  (default) — each moderator is crossed with every weather term
  #     independently.  For weather term W and moderators M1, M2 this gives:
  #       W * M1  and  W * M2  as separate terms on the RHS.
  #     This estimates a separate W:M1 slope and a separate W:M2 slope,
  #     and is almost always what you want in practice.
  #
  #   "saturated" — all moderators are crossed simultaneously with each weather
  #     term.  For W and moderators M1, M2 this gives:
  #       W * M1 * M2
  #     which includes the three-way interaction W:M1:M2.  Use this only when
  #     you have a strong theoretical reason to model the joint moderation.
  #
  # In both modes the `*` expansion in R automatically includes all lower-order
  # main effects and two-way interactions, so there is no need to list them
  # separately on the RHS.
  # ---------------------------------------------------------------------------

  interaction_mode <- tolower(selected_model$interaction_mode %||% "saturated")
  if (!interaction_mode %in% c("pairwise", "saturated")) {
    warning(sprintf(
      "Unknown interaction_mode '%s'; falling back to 'saturated'.", interaction_mode
    ))
    interaction_mode <- "saturated"
  }

  if (length(interaction_vars) > 0) {

    if (interaction_mode == "pairwise") {
      # One `W * Mk` term per (weather-term, moderator) pair — kept separate
      interaction_formula_terms <- as.vector(outer(
        weather_formula_terms, interaction_vars,
        FUN = function(h, m) paste0(h, " * ", m)
      ))
      # Reporting strings: W:Mk
      interaction_terms <- as.vector(outer(
        weather_formula_terms, interaction_vars,
        FUN = function(h, m) paste0(h, ":", m)
      ))

    } else {
      # "saturated": W * M1 * M2 * … — one term per weather term
      mod_str <- paste(interaction_vars, collapse = " * ")
      interaction_formula_terms <- paste0(weather_formula_terms, " * ", mod_str)
      # Reporting strings: all combinations of W with each subset of moderators
      interaction_terms <- unlist(lapply(weather_formula_terms, function(h) {
        # Generate colon-joined strings for every non-empty subset of moderators
        subsets <- unlist(lapply(seq_along(interaction_vars), function(k) {
          apply(combn(interaction_vars, k), 2, paste, collapse = ":")
        }))
        paste0(h, ":", subsets)
      }))
    }

    # The `*` expansion already pulls in weather main effects and all moderator
    # main effects, so rhs_weather can be set to the interaction terms only.
    rhs_weather <- interaction_formula_terms

  } else {
    interaction_formula_terms <- character(0)
    interaction_terms         <- character(0)
    rhs_weather               <- weather_formula_terms
  }

  # Bundle term groups for backend$build_formulas()
  # interactions_main: moderator main effects listed explicitly for fixest's
  # benefit (fixest needs them on the main-effects side of |, not absorbed).
  # For lm/parsnip the * expansion in rhs_weather already includes them.
  terms_bundle <- list(
    weather           = rhs_weather,
    interactions_main = interaction_vars,   # all moderator variables, any length
    covariates        = covariate_vars
  )

  # ---------------------------------------------------------------------------
  # 5. Build formulas via backend
  # ---------------------------------------------------------------------------

  formulas <- backend$build_formulas(y_var, terms_bundle, fe_vars)

  # ---------------------------------------------------------------------------
  # 6. Drop incomplete cases on all variables used by the fullest model
  # ---------------------------------------------------------------------------

  vars_used <- unique(c(y_var, weather_formula_terms, interaction_vars,
                        fe_vars, covariate_vars))
  vars_used <- vars_used[vars_used %in% names(df)]
  df        <- df[stats::complete.cases(df[, vars_used, drop = FALSE]), ]

  if (nrow(df) == 0) stop("No complete cases after dropping NA rows.")

  # ---------------------------------------------------------------------------
  # 7. Build model spec + engine-level options
  # ---------------------------------------------------------------------------

  model_spec <- backend$make_spec(model_type, use_logit)

  cluster_vars <- selected_model$cluster %||% character(0)
  cluster_vars <- cluster_vars[nzchar(cluster_vars) & !is.na(cluster_vars)]

  engine_opts <- list(
    fixest = if (length(cluster_vars) > 0) {
      list(cluster = stats::as.formula(
        paste("~", paste(cluster_vars, collapse = " + "))
      ))
    } else {
      list()
    }
  )

  # ---------------------------------------------------------------------------
  # 8. Fit the three models
  # ---------------------------------------------------------------------------

  fit_one <- function(formula, label) {
    tryCatch(
      backend$fit_one(formula, df, model_type, model_spec, engine_opts),
      error = function(e) stop(sprintf("Model '%s' failed: %s", label, conditionMessage(e)))
    )
  }

  fit1 <- fit_one(formulas$formula1, "weather only")
  fit2 <- fit_one(formulas$formula2, "weather + FE")
  fit3 <- fit_one(formulas$formula3, "weather + FE + controls")

  # ---------------------------------------------------------------------------
  # 9. Return
  # ---------------------------------------------------------------------------

  list(
    fit1              = fit1,
    fit2              = fit2,
    fit3              = fit3,
    weather_terms     = weather_vars,
    interaction_terms = interaction_terms,
    fe_terms          = fe_vars,
    y_var             = y_var,
    model_type        = model_type,
    engine            = engine_key,
    train_data        = df,
    formulas          = formulas
  )
}