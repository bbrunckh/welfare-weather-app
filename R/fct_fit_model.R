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
# run_lasso()                                                                  #
# ---------------------------------------------------------------------------- #

#' Run MI + stability LASSO and post-LASSO refits
#'
#' @param df data.frame with analysis variables
#' @param outcome_var character scalar outcome name
#' @param weather_vars character vector weather vars
#' @param fe_vars character vector fixed-effect vars (kept unpenalized)
#' @param int_vars character vector interaction moderators
#' @param valid_vl data.frame variable list with columns name, ind, hh, area, firm
#' @param model_type character scalar ("Linear regression" / "Logistic regression")
#' @param alpha numeric glmnet alpha
#' @param lambda_choice character lambda selector ("lambda.1se" / "lambda.min")
#' @param nfolds integer CV folds
#' @param standardize logical
#' @param mi_m integer number of imputations
#' @param mi_maxit integer mice iterations
#' @param stability_threshold numeric in (0,1)
#'
#' @return list(model, per_imputation_models, selected_covariates, selection_frequency)
#' @export
run_lasso_selection <- function(
  df,
  selected_outcome,
  weather_vars,
  fe_vars = character(0),
  int_vars = character(0),
  valid_vl,
  model_type = "Linear regression",
  alpha = 1,
  lambda_choice = "lambda.1se",
  nfolds = 10,
  standardize = TRUE,
  mi_m = 5,
  mi_maxit = 5,
  stability_threshold = 0.5
) {
  df <- as.data.frame(df)

  # Same outcome handling pattern as fit_model()
  y_var        <- selected_outcome$name
  outcome_type <- selected_outcome$type

  if (!y_var %in% names(df)) {
    stop(sprintf("Outcome variable '%s' not found in data.", y_var))
  }

  is_logit <- identical(model_type, "Logistic regression")

  if (is_logit) {
    if (!identical(outcome_type, "logical")) {
      warning("Logistic regression requested but outcome type is not 'logical' — falling back to linear.")
      is_logit <- FALSE
    } else {
      y_vals <- df[[y_var]][!is.na(df[[y_var]])]
      if (!all(y_vals %in% c(0, 1, TRUE, FALSE))) {
        warning("Outcome values are not 0/1 — falling back to linear.")
        is_logit <- FALSE
      }
    }
  }

  if (is_logit) {
    df[[y_var]] <- as.integer(as.logical(df[[y_var]]))
  }

  df <- df[!is.na(df[[y_var]]), , drop = FALSE]
  if (nrow(df) < 30) stop("Too few observations after removing missing outcome.")

  interaction_terms <- character(0)
  if (length(int_vars) > 0 && length(weather_vars) > 0) {
    interaction_terms <- as.vector(outer(int_vars, weather_vars, paste, sep = ":"))
  }

  core_terms <- unique(c(weather_vars, fe_vars, interaction_terms))
  core_terms <- core_terms[core_terms %in% names(df)]

  # Drop FE terms with <2 observed levels (prevents contrasts errors)
  if (length(fe_vars) > 0) {
    fe_keep <- vapply(fe_vars, function(v) {
      if (!v %in% names(df)) return(FALSE)
      xv <- df[[v]]
      length(unique(stats::na.omit(xv))) >= 2
    }, logical(1))
    fe_vars <- fe_vars[fe_keep]
    core_terms <- unique(c(weather_vars, fe_vars, interaction_terms))
    core_terms <- core_terms[core_terms %in% names(df)]
  }

  if (is.null(valid_vl) || nrow(valid_vl) == 0) stop("Variable list not available or empty.")
  allowed <- valid_vl$name[valid_vl$ind == 1 | valid_vl$hh == 1 | valid_vl$area == 1 | valid_vl$firm == 1]
  exclude <- unique(c(y_var, core_terms))
  candidate_vars <- intersect(setdiff(names(df), exclude), allowed)

  if (length(candidate_vars) > 0) {
    is_num <- vapply(df[, candidate_vars, drop = FALSE], is.numeric, logical(1))
    candidate_vars <- candidate_vars[is_num]
  }
  if (length(candidate_vars) == 0) stop("No valid numeric candidate covariates available for LASSO.")

  non_all_na <- vapply(df[, candidate_vars, drop = FALSE], function(x) any(!is.na(x)), logical(1))
  candidate_vars <- candidate_vars[non_all_na]
  if (length(candidate_vars) == 0) stop("No candidate variables with observed values remain for imputation/LASSO.")

  m <- max(1, as.integer(mi_m))
  imp <- mice::mice(
    df[, candidate_vars, drop = FALSE],
    m = m,
    maxit = max(1, as.integer(mi_maxit)),
    method = "pmm",
    print = FALSE
  )

  selected_list <- vector("list", m)
  final_models  <- vector("list", m)
  family_type <- if (is_logit) "binomial" else "gaussian"

  for (i in seq_len(m)) {
    df_imp <- df
    df_imp[, candidate_vars] <- mice::complete(imp, action = i)

    # Re-drop FE terms that collapse to one level in this imputation
    fe_vars_i <- fe_vars[vapply(fe_vars, function(v) {
      length(unique(stats::na.omit(df_imp[[v]]))) >= 2
    }, logical(1))]
    core_terms_i <- unique(c(weather_vars, fe_vars_i, interaction_terms))
    core_terms_i <- core_terms_i[core_terms_i %in% names(df_imp)]
    core_formula_i <- if (length(core_terms_i) == 0) stats::as.formula("~ 1")
    else stats::as.formula(paste("~", paste(core_terms_i, collapse = " + ")))

    mm_core <- stats::model.matrix(core_formula_i, data = df_imp)
    X_core <- if (ncol(mm_core) > 1) mm_core[, -1, drop = FALSE] else matrix(nrow = nrow(df_imp), ncol = 0)

    X_lasso <- as.matrix(df_imp[, candidate_vars, drop = FALSE])
    keep_cols <- apply(X_lasso, 2, function(x) length(unique(stats::na.omit(x))) > 1)
    X_lasso <- X_lasso[, keep_cols, drop = FALSE]

    if (ncol(X_lasso) == 0) {
      selected_list[[i]] <- character(0)
      next
    }

    X_full <- if (ncol(X_core) > 0) cbind(X_core, X_lasso) else X_lasso
    penalty <- c(rep(0, ncol(X_core)), rep(1, ncol(X_lasso)))

    cvfit <- glmnet::cv.glmnet(
      x = X_full,
      y = df_imp[[y_var]],
      alpha = alpha,
      nfolds = max(2, as.integer(nfolds)),
      family = family_type,
      standardize = isTRUE(standardize),
      penalty.factor = penalty
    )

    coefs <- stats::coef(cvfit, s = lambda_choice)
    sel <- rownames(coefs)[as.numeric(coefs) != 0]
    sel <- setdiff(sel, "(Intercept)")
    sel <- intersect(sel, colnames(X_lasso))
    selected_list[[i]] <- sel
  }

  all_selected <- unique(unlist(selected_list))
  if (length(all_selected) == 0) stop("No covariates selected across imputations.")

  selection_freq <- sapply(all_selected, function(v) mean(vapply(selected_list, function(x) v %in% x, logical(1))))
  final_selected <- names(selection_freq)[selection_freq >= stability_threshold]
  if (length(final_selected) == 0) stop("No covariates stable across imputations.")

  for (i in seq_len(m)) {
    df_imp <- df
    df_imp[, candidate_vars] <- mice::complete(imp, action = i)

    fe_vars_i <- fe_vars[vapply(fe_vars, function(v) {
      length(unique(stats::na.omit(df_imp[[v]]))) >= 2
    }, logical(1))]
    rhs_i <- unique(c(weather_vars, fe_vars_i, interaction_terms, final_selected))
    rhs_i <- rhs_i[rhs_i %in% names(df_imp)]
    final_formula_i <- stats::as.formula(paste(y_var, "~", paste(rhs_i, collapse = " + ")))

    final_models[[i]] <- if (family_type == "gaussian") {
      stats::lm(final_formula_i, data = df_imp)
    } else {
      stats::glm(final_formula_i, data = df_imp, family = stats::binomial())
    }
  }

  final_models <- final_models[!vapply(final_models, is.null, logical(1))]
  if (length(final_models) == 0) stop("All post-LASSO refits failed.")

  pooled_model <- NULL
  mira_obj <- tryCatch(mice::as.mira(final_models), error = function(e) NULL)
  if (!is.null(mira_obj)) {
    pooled_model <- tryCatch(mice::pool(mira_obj), error = function(e) NULL)
  }

  list(
    model = if (!is.null(pooled_model)) pooled_model else final_models,
    per_imputation_models = final_models,
    selected_covariates = final_selected,
    selection_frequency = selection_freq
  )
}

# ---------------------------------------------------------------------------- #
# fit_model()                                                                  #
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