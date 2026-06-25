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
        formula1 = build(terms$hazard),
        formula2 = build(c(terms$hazard, terms$interactions_main), fe_vars),
        formula3 = build(c(terms$hazard, terms$interactions_main,
                           terms$covariates),                        fe_vars)
      )
    },

    fit_one = function(formula, data, model_type, model_spec, opts) {
      args <- c(list(fml = formula, data = data), opts$fixest)
      if (model_type == "logistic") {
        args$family <- stats::binomial("logit")
        do.call(fixest::feglm, args)
      } else {
        if (!is.null(opts$fixest) && length(opts$fixest) > 0) {
          args <- c(args, opts$fixest)
        }
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
        formula1 = build(terms$hazard),
        formula2 = build(c(terms$hazard, fe_vars)),
        formula3 = build(c(terms$hazard, fe_vars, terms$covariates))
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
        formula1 = build(terms$hazard),
        formula2 = build(c(terms$hazard, fe_vars)),
        formula3 = build(c(terms$hazard, fe_vars, terms$covariates))
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
  ),

  # -------------------------------------------------------------------------- #
  # RIF — Unconditional Quantile Regression (Firpo, Fortin & Lemieux 2009)     #
  # -------------------------------------------------------------------------- #
  # Estimates distributional impacts by transforming the outcome into its
  # Recentered Influence Function at each quantile, then fitting standard OLS
  # (via fixest::feols) on the transformed outcome. The stacked multi-LHS
  # syntax fits all quantiles simultaneously. Returns a fixest_multi object.
  # -------------------------------------------------------------------------- #
  rif = list(

    requires    = c("fixest", "broom"),
    model_types = c("Quantile regression (RIF)"),

    # Same FE-absorbing formula structure as fixest; fit_one replaces the LHS
    # with stacked RIF columns.
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
        # Store as character — fit_one will prepend the stacked RIF LHS
        rhs
      }
      list(
        formula1 = build(terms$hazard),
        formula2 = build(c(terms$hazard, terms$interactions_main), fe_vars),
        formula3 = build(c(terms$hazard, terms$interactions_main,
                           terms$covariates),                        fe_vars)
      )
    },

    fit_one = function(formula, data, model_type, model_spec, opts) {
      # formula is actually the RHS string from build_formulas above
      rhs_str  <- formula
      rif_cols <- opts$rif$rif_cols
      lhs      <- paste0("c(", paste(rif_cols, collapse = ", "), ")")
      stacked_fml <- stats::as.formula(paste(lhs, "~", rhs_str))
      args <- list(fml = stacked_fml, data = data, warn = FALSE)
      if (!is.null(opts$fixest) && length(opts$fixest) > 0) {
        args <- c(args, opts$fixest)
      }
      do.call(fixest::feols, args)
    },

    make_spec = function(model_type, use_logit) NULL,

    prepare_outcome = function(df, y_var, use_logit) {
      taus     <- seq(0.1, 0.9, by = 0.1)
      rif_cols <- paste0("rif_", formatC(taus * 100, format = "d"))
      y        <- df[[y_var]]
      for (i in seq_along(taus)) {
        df[[rif_cols[i]]] <- compute_rif(y, tau = taus[i])
      }
      attr(df, "rif_taus") <- taus
      attr(df, "rif_cols") <- rif_cols
      df
    }
  )

)

# ---------------------------------------------------------------------------- #
# run_lasso()                                                                  #
# ---------------------------------------------------------------------------- #

#' Run stability LASSO variable selection
#'
#' @param df data.frame with analysis variables
#' @param selected_outcome named list with `$name` (column) and `$type`
#' @param weather_vars character vector weather vars (unpenalized)
#' @param fe_vars character vector fixed-effect vars (unpenalized)
#' @param int_vars character vector interaction moderators (unpenalized;
#'   weather × int_vars interactions are also forced into the unpenalized core)
#' @param valid_vl data.frame variable list with columns name, ind, hh, area, firm
#' @param model_type character scalar ("Linear regression" / "Logistic regression")
#' @param alpha numeric glmnet alpha
#' @param lambda_choice character lambda selector ("lambda.1se" / "lambda.min")
#' @param nfolds integer CV folds
#' @param standardize logical
#' @param mi_m integer number of imputations
#' @param mi_maxit integer mice iterations
#' @param mi_method character; mice imputation method (default "pmm";
#'   "norm.predict" / "norm" are much faster for numeric-heavy candidate pools)
#' @param stability_threshold numeric in (0,1)
#' @param use_parallel logical; use future.apply / futuremice for parallel runs
#' @param n_workers integer; number of workers for parallel plan (capped at mi_m)
#' @param parallel_min_n integer; auto-disable parallelism when the analytic
#'   sample size (post NA-drop) is below this threshold. Default 20000, which
#'   is the empirical break-even point on a 16-core Mac for `mi_m = 5`; below
#'   it, multisession fork + globals-export overhead dominates the work.
#' @param parallel_seed integer; seed for parallel-safe reproducibility
#' @param globals_max_size numeric; override for future.globals.maxSize (bytes)
#' @param cv_selection character; fold assignment mode for CV ("default" or "random")
#' @param glmnet_tol numeric; convergence tolerance passed to glmnet (thresh)
#'
#' @return list(selected_covariates, selection_frequency)
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
  mi_method = "pmm",
  use_mice = FALSE,
  stability_threshold = 0.5,
  use_parallel = FALSE,
  n_workers = NULL,
  parallel_min_n = 20000L,
  parallel_seed = NULL,
  globals_max_size = NULL,
  cv_selection = c("default", "random"),
  glmnet_tol = NULL
) {
  df <- as.data.frame(df)

  # ---------------------------------------------------------------------------
  # 1. Outcome validation / coercion (same pattern as fit_model())
  # ---------------------------------------------------------------------------
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
  if (is_logit) df[[y_var]] <- as.integer(as.logical(df[[y_var]]))

  df <- df[!is.na(df[[y_var]]), , drop = FALSE]
  if (nrow(df) < 30) stop("Too few observations after removing missing outcome.")

  # ---------------------------------------------------------------------------
  # 2. Core term construction (correctness fix)
  #
  # Real column names that must be unpenalized go in `core_main_terms`.
  # Formula-syntax interaction strings ("int:weather") go in `interaction_terms`
  # and are appended directly to the formula — model.matrix expands them.
  # `int_vars` themselves are unpenalized main effects.
  # ---------------------------------------------------------------------------
  weather_vars <- weather_vars[weather_vars %in% names(df)]
  fe_vars      <- fe_vars[fe_vars %in% names(df)]
  int_vars     <- int_vars[int_vars %in% names(df)]

  # Drop FE terms with <2 observed levels (prevents contrasts errors)
  if (length(fe_vars) > 0) {
    fe_keep <- vapply(fe_vars, function(v) {
      length(unique(stats::na.omit(df[[v]]))) >= 2
    }, logical(1))
    fe_vars <- fe_vars[fe_keep]
  }

  interaction_terms <- character(0)
  if (length(int_vars) > 0 && length(weather_vars) > 0) {
    interaction_terms <- as.vector(outer(int_vars, weather_vars, paste, sep = ":"))
  }

  core_main_terms <- unique(c(weather_vars, int_vars, fe_vars))

  # ---------------------------------------------------------------------------
  # 3. Drop NA rows on outcome + core columns (correctness fix)
  #
  # Without this, mice (which is fed only candidate_vars) leaves NAs in core
  # columns; model.matrix then drops those rows from X_core but not X_lasso,
  # producing a silent row mismatch when cbind()-ing.
  # ---------------------------------------------------------------------------
  if (length(core_main_terms) > 0) {
    df <- df[stats::complete.cases(df[, core_main_terms, drop = FALSE]), , drop = FALSE]
  }
  if (nrow(df) < 30) {
    stop("Too few observations after removing NAs in outcome / core terms.")
  }

  # ---------------------------------------------------------------------------
  # 4. Candidate pool (now correctly excludes int_vars)
  # ---------------------------------------------------------------------------
  if (is.null(valid_vl) || nrow(valid_vl) == 0) stop("Variable list not available or empty.")
  allowed <- valid_vl$name[
    (valid_vl$ind == 1 | valid_vl$hh == 1 | valid_vl$area == 1 | valid_vl$firm == 1) &
      (is.na(valid_vl$outcome) | valid_vl$outcome == 0)
  ]
  exclude <- unique(c(y_var, core_main_terms))
  candidate_vars <- intersect(setdiff(names(df), exclude), allowed)

  if (length(candidate_vars) > 0) {
    is_num <- vapply(df[, candidate_vars, drop = FALSE], is.numeric, logical(1))
    candidate_vars <- candidate_vars[is_num]
  }
  if (length(candidate_vars) == 0) stop("No valid numeric candidate covariates available for LASSO.")

  non_all_na <- vapply(df[, candidate_vars, drop = FALSE],
                       function(x) any(!is.na(x)), logical(1))
  candidate_vars <- candidate_vars[non_all_na]
  if (length(candidate_vars) == 0) {
    stop("No candidate variables with observed values remain for imputation/LASSO.")
  }

  # ---------------------------------------------------------------------------
  # 4b. Complete-case filtering (default; use_mice = TRUE restores MI path)
  #
  # For variable *selection*, complete-case analysis is sufficient when

  # missingness is low (filter_valid_vars enforces >= 90% complete upstream).
  # Dropping NA rows avoids the mice bottleneck at large n. Final model
  # fitting in fit_model() uses the full dataset independently.
  # ---------------------------------------------------------------------------
  if (!isTRUE(use_mice)) {
    cc_mask <- complete.cases(df[, candidate_vars, drop = FALSE])
    n_dropped <- sum(!cc_mask)
    if (n_dropped > 0L) {
      message(sprintf(
        "run_lasso_selection: dropping %d/%d rows with NA in candidates (complete-case mode).",
        n_dropped, nrow(df)
      ))
      df <- df[cc_mask, , drop = FALSE]
      if (nrow(df) < 100L)
        stop("Too few complete cases for LASSO selection (<100 rows remain).")
    }
  }

  # ---------------------------------------------------------------------------
  # 5. Parallel plan (workers capped at mi_m — extra workers idle)
  #
  # Auto-disable parallelism on small samples: below `parallel_min_n` rows the
  # multisession fork + globals-export overhead exceeds the actual work. The
  # 20k default reflects the break-even point on a 16-core Mac with mi_m = 5
  # (see dev/bench_lasso.R).
  # ---------------------------------------------------------------------------
  m <- max(1L, as.integer(mi_m))
  family_type <- if (is_logit) "binomial" else "gaussian"
  cv_selection <- match.arg(cv_selection)

  if (isTRUE(use_parallel) && nrow(df) < as.integer(parallel_min_n)) {
    message(sprintf(
      "run_lasso_selection: n = %d below parallel_min_n = %d; running sequentially.",
      nrow(df), as.integer(parallel_min_n)
    ))
    use_parallel <- FALSE
  }

  # Parallel plan setup is deferred to the MI path (step 8) where map_fun is

  # actually used. The fast path (step 7, no NAs) forces sequential lapply —
  # spawning multisession workers here would waste startup time.
  map_fun <- lapply

  # ---------------------------------------------------------------------------
  # 6. Build design matrices (X_core + X_lasso)
  # ---------------------------------------------------------------------------
  core_formula <- if (length(core_main_terms) == 0 && length(interaction_terms) == 0) {
    stats::as.formula("~ 1")
  } else {
    stats::as.formula(paste(
      "~", paste(c(core_main_terms, interaction_terms), collapse = " + ")
    ))
  }
  mm_core <- stats::model.matrix(core_formula, data = df)
  X_core  <- if (ncol(mm_core) > 1) {
    mm_core[, -1, drop = FALSE]
  } else {
    matrix(0, nrow = nrow(df), ncol = 0)
  }
  rm(mm_core)

  y_vec <- df[[y_var]]

  has_matrixStats <- requireNamespace("matrixStats", quietly = TRUE)
  drop_constant <- function(X) {
    if (ncol(X) == 0) return(X)
    keep <- if (has_matrixStats) {
      matrixStats::colMaxs(X) > matrixStats::colMins(X)
    } else {
      vapply(seq_len(ncol(X)), function(j) {
        v <- X[, j]; length(v) > 0L && (max(v) > min(v))
      }, logical(1))
    }
    X[, keep, drop = FALSE]
  }

  has_na <- anyNA(df[, candidate_vars, drop = FALSE])
  nfolds_i <- max(2L, as.integer(nfolds))

  # ---------------------------------------------------------------------------
  # 7. Fast path: no NAs in candidates (always true after complete-case filter)
  #
  # X_full and penalty are identical across iterations — build once.  The loop
  # only varies the random CV fold assignment for stability selection.
  # Sequential lapply: globals-export overhead of multisession exceeds the
  # cv.glmnet compute time (confirmed via dev/bench_lasso.R).
  # ---------------------------------------------------------------------------
  if (!has_na) {
    X_lasso <- drop_constant(as.matrix(df[, candidate_vars, drop = FALSE]))
    if (ncol(X_lasso) == 0) stop("All candidate variables are constant.")
    lasso_names <- colnames(X_lasso)

    X_full  <- if (ncol(X_core) > 0) cbind(X_core, X_lasso) else X_lasso
    penalty <- c(rep(0, ncol(X_core)), rep(1, ncol(X_lasso)))
    rm(X_core, X_lasso)

    cv_args_base <- list(
      x = X_full,
      y = y_vec,
      alpha = alpha,
      nfolds = nfolds_i,
      family = family_type,
      standardize = isTRUE(standardize),
      penalty.factor = penalty
    )
    if (!is.null(glmnet_tol)) cv_args_base$thresh <- glmnet_tol

    selection_results <- lapply(seq_len(m), function(i) {
      if (!is.null(parallel_seed)) set.seed(parallel_seed + i)
      cv_args <- cv_args_base
      if (identical(cv_selection, "random")) {
        cv_args$foldid <- sample(rep(seq_len(nfolds_i), length.out = nrow(X_full)))
      }
      cvfit <- do.call(glmnet::cv.glmnet, cv_args)
      coefs <- stats::coef(cvfit, s = lambda_choice)
      sel   <- rownames(coefs)[as.numeric(coefs) != 0]
      sel   <- setdiff(sel, "(Intercept)")
      intersect(sel, lasso_names)
    })

  } else {
    # -------------------------------------------------------------------------
    # 8. MI path: impute candidates, rebuild X_lasso per imputation
    # -------------------------------------------------------------------------

    # Set up parallel plan here (not earlier) so the fast path never pays the
    # multisession worker-spawn cost.
    if (isTRUE(use_parallel)) {
      if (!requireNamespace("future", quietly = TRUE) ||
          !requireNamespace("future.apply", quietly = TRUE)) {
        stop("Parallel LASSO requires packages 'future' and 'future.apply'.")
      }
      old_plan <- future::plan()
      on.exit(future::plan(old_plan), add = TRUE)
      old_max_size <- getOption("future.globals.maxSize")
      on.exit(options(future.globals.maxSize = old_max_size), add = TRUE)
      if (is.null(globals_max_size)) {
        globals_max_size <- max(2 * 1024^3, old_max_size %||% 0)
      }
      options(future.globals.maxSize = globals_max_size)
      workers <- if (is.null(n_workers)) future::availableCores() else as.integer(n_workers)
      workers <- max(1L, min(workers, m))
      future::plan(future::multisession, workers = workers)
      map_fun <- function(x, fun) {
        future.apply::future_lapply(
          x, fun,
          future.seed = if (is.null(parallel_seed)) TRUE else parallel_seed
        )
      }
    }

    mi_cols  <- unique(c(y_var, core_main_terms, candidate_vars))
    mi_frame <- df[, mi_cols, drop = FALSE]

    non_num_idx <- vapply(mi_frame, function(x) !is.numeric(x), logical(1))
    if (any(non_num_idx)) {
      mi_frame[non_num_idx] <- lapply(mi_frame[non_num_idx], function(x) {
        if (is.logical(x)) as.integer(x)
        else if (is.factor(x)) as.integer(x)
        else as.integer(as.factor(x))
      })
    }

    use_futuremice <- isTRUE(use_parallel) &&
      utils::packageVersion("mice") >= "3.16.0"
    if (use_futuremice) {
      imp <- mice::futuremice(
        mi_frame,
        m = m,
        maxit = max(1L, as.integer(mi_maxit)),
        method = mi_method,
        parallelseed = parallel_seed,
        print = FALSE
      )
    } else {
      if (!is.null(parallel_seed)) set.seed(parallel_seed)
      imp <- mice::mice(
        mi_frame,
        m = m,
        maxit = max(1L, as.integer(mi_maxit)),
        method = mi_method,
        print = FALSE
      )
    }
    completed_cands_list <- lapply(
      seq_len(m),
      function(i) mice::complete(imp, action = i)[, candidate_vars, drop = FALSE]
    )
    rm(mi_frame, imp)

    selection_results <- map_fun(seq_len(m), function(i) {
      if (!is.null(parallel_seed)) set.seed(parallel_seed + i)

      X_lasso <- drop_constant(as.matrix(completed_cands_list[[i]]))
      if (ncol(X_lasso) == 0) return(character(0))

      X_full  <- if (ncol(X_core) > 0) cbind(X_core, X_lasso) else X_lasso
      penalty <- c(rep(0, ncol(X_core)), rep(1, ncol(X_lasso)))

      foldid <- NULL
      if (identical(cv_selection, "random")) {
        foldid <- sample(rep(seq_len(nfolds_i), length.out = nrow(X_full)))
      }

      cv_args <- list(
        x = X_full,
        y = y_vec,
        alpha = alpha,
        nfolds = nfolds_i,
        family = family_type,
        standardize = isTRUE(standardize),
        penalty.factor = penalty
      )
      if (!is.null(foldid))     cv_args$foldid <- foldid
      if (!is.null(glmnet_tol)) cv_args$thresh <- glmnet_tol

      cvfit <- do.call(glmnet::cv.glmnet, cv_args)

      coefs <- stats::coef(cvfit, s = lambda_choice)
      sel   <- rownames(coefs)[as.numeric(coefs) != 0]
      sel   <- setdiff(sel, "(Intercept)")
      intersect(sel, colnames(X_lasso))
    })
  }

  selected_list <- selection_results
  all_selected  <- unique(unlist(selected_list))
  if (length(all_selected) == 0) stop("No covariates selected across imputations.")

  # ---------------------------------------------------------------------------
  # 9. Selection frequency via tabulate
  # ---------------------------------------------------------------------------
  freq_tbl <- table(unlist(selected_list))
  selection_freq <- setNames(as.numeric(freq_tbl) / m, names(freq_tbl))

  final_selected <- names(selection_freq)[selection_freq >= stability_threshold]
  if (length(final_selected) == 0) stop("No covariates stable across imputations.")

  list(
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

  # Extract RIF metadata (set by the rif engine's prepare_outcome)
  rif_taus <- attr(df, "rif_taus")
  rif_cols <- attr(df, "rif_cols")
  is_rif   <- !is.null(rif_taus)

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
    hazard            = rhs_weather,
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

  # Pass RIF column names and quantile vector to fit_one via engine_opts
  if (is_rif) {
    engine_opts$rif <- list(taus = rif_taus, rif_cols = rif_cols)
  }

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
  # 8b. Slim fit objects to reduce memory
  # ---------------------------------------------------------------------------
  # do.call(feols, args) captures the full evaluated `data` argument inside

  # the call slot — for large surveys this adds hundreds of MB per fit element
  # (×27 for RIF with 9 taus × 3 fits). Strip the embedded data from $call
  # and remove $scores (only needed to re-compute robust VCV, which is already

  # stored in $coeftable). vcov(), r2() and fixef() continue to work after this.
  #
  # NOTE: stats::model.matrix(fit) does NOT survive slimming — fixest
  # re-evaluates the `data` argument from $call to rebuild the design matrix,
  # so once $call[[3]] is gone it errors. The marginal-effect plot
  # (make_weather_effect_plot) needs that matrix for the linear path, so for
  # fit3 (the fit it plots) we cache the design matrix in attr "wise_mm"
  # *before* slimming. RIF plots from a precomputed beta grid and never calls
  # model.matrix(), which is why it was unaffected.

  .slim_fit <- function(fit) {
    if (inherits(fit, "fixest")) {
      if (length(fit$call) >= 3L) fit$call[[3]] <- quote(.data_removed)
      fit$scores <- NULL
    } else if (is.list(fit)) {
      # fixest_multi: list of fixest objects (one per tau)
      for (i in seq_along(fit)) {
        if (inherits(fit[[i]], "fixest")) {
          if (length(fit[[i]]$call) >= 3L) fit[[i]]$call[[3]] <- quote(.data_removed)
          fit[[i]]$scores <- NULL
        }
      }
    }
    fit
  }

  # Cache fit3's design matrix before slimming so the downstream model-fit and
  # effect plots can rebuild predictions / importance without re-evaluating the
  # (now removed) embedded data. fit3 is the only fit plotted. For RIF, all
  # quantile sub-fits share one design matrix, so we cache a single copy on the
  # median sub-fit (the one extract_rif_median() returns) — not all 9 — keeping
  # this to at most one stored matrix either way.
  .cache_mm <- function(fit) {
    if (inherits(fit, "fixest")) {
      mm <- tryCatch(as.data.frame(stats::model.matrix(fit)),
                     error = function(e) NULL)
      if (!is.null(mm)) attr(fit, "wise_mm") <- mm
    } else if (is.list(fit) && length(fit) > 0) {
      # fixest_multi: cache on the median sub-fit (index matches extract_rif_median).
      # Pull the sub-fit out, attach the attr, and write it back — assigning to
      # attr(fit[[idx]], ...) directly would mutate a throwaway copy.
      idx <- min(5L, length(fit))
      sub <- fit[[idx]]
      if (inherits(sub, "fixest")) {
        mm <- tryCatch(as.data.frame(stats::model.matrix(sub)),
                       error = function(e) NULL)
        if (!is.null(mm)) {
          attr(sub, "wise_mm") <- mm
          fit[[idx]] <- sub
        }
      }
    }
    fit
  }

  fit3 <- .cache_mm(fit3)

  fit1 <- .slim_fit(fit1)
  fit2 <- .slim_fit(fit2)
  fit3 <- .slim_fit(fit3)

  # ---------------------------------------------------------------------------
  # 9. Build RIF grid (beta curves) from all three model specifications
  # ---------------------------------------------------------------------------

  rif_grid <- NULL
  if (is_rif) {
    rif_grid <- rbind(
      build_rif_grid(fit1, rif_taus, model_id = 1L),
      build_rif_grid(fit2, rif_taus, model_id = 2L),
      build_rif_grid(fit3, rif_taus, model_id = 3L)
    )
  }

  # ---------------------------------------------------------------------------
  # 10. Return
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
    formulas          = formulas,
    rif_grid          = rif_grid,
    taus              = rif_taus
  )
}