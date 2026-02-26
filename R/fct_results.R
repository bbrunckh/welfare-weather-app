# ============================================================================ #
# Pure functions for model results: outcome preparation, coefficient helpers, #
# and plot/table builders.                                                     #
# Used by mod_1_07_results_server(). Stateless and testable without Shiny.    #
# ============================================================================ #


# ---------------------------------------------------------------------------- #
# Outcome preparation                                                          #
# ---------------------------------------------------------------------------- #

#' Prepare the outcome column in survey_weather before model fitting
#'
#' Applies three sequential transformations in order:
#' 1. **LCU back-conversion** — multiply by `ppp2021` when `units == "LCU"`.
#' 2. **Log transform** — `log()` when `transform == "log"`.
#' 3. **Binary poor indicator** — `welfare < povline` when `name == "poor"`
#'    and `povline` is non-NA.
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
  name    <- as.character(so$name[1])
  units   <- as.character(so$units[1])
  trans   <- as.character(so$transform[1])
  povline <- so$povline[1]

  if (isTRUE(units == "LCU") && "ppp2021" %in% names(df)) {
    df <- df |> dplyr::mutate(!!name := .data[[name]] * .data$ppp2021)
  }
  if (isTRUE(trans == "log")) {
    df <- df |> dplyr::mutate(!!name := log(.data[[name]]))
  }
  if (isTRUE(name == "poor") && !is.na(povline) && "welfare" %in% names(df)) {
    df <- df |>
      dplyr::mutate(!!name := as.numeric(.data[["welfare"]] < povline))
  }

  df
}


# ---------------------------------------------------------------------------- #
# Coefficient helpers                                                          #
# ---------------------------------------------------------------------------- #

#' Extract weather-related coefficient names from a fitted native model
#'
#' Covers base terms, factor-level expansions, polynomial terms (`I(x^n)`),
#' and interaction terms.
#'
#' @param fit               A native `lm`/`glm` object (unwrapped from
#'   parsnip with `$fit`).
#' @param weather_terms     Character vector of base weather variable names.
#' @param interaction_terms Character vector of interaction term strings.
#'   Pass `character(0)` when there are none.
#'
#' @return Character vector of matching coefficient names.
#'
#' @export
weather_coef_names <- function(fit, weather_terms, interaction_terms) {
  all_coefs <- names(stats::coef(fit))

  weather_pattern <- paste(
    paste0("^", weather_terms, "$"),
    paste0("^", weather_terms, "[\\[\\(]"),
    paste0("^I\\(", weather_terms, "\\^"),
    sep = "|"
  )
  weather_pattern <- paste(weather_pattern, collapse = "|")

  interact_pattern <- if (length(interaction_terms) > 0) {
    paste(
      vapply(interaction_terms, function(t) {
        paste0("^", gsub("([\\(\\)\\^])", "\\\\\\1", t))
      }, character(1)),
      collapse = "|"
    )
  } else {
    NULL
  }

  keep <- grepl(weather_pattern, all_coefs)
  if (!is.null(interact_pattern)) keep <- keep | grepl(interact_pattern, all_coefs)
  all_coefs[keep]
}


#' Build a human-readable label for a coefficient name
#'
#' Splits on `":"` and applies `label_fun` to each component, joining
#' with `" × "`.
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
  readable <- vapply(coef_names, coef_label, character(1), label_fun = label_fun)
  stats::setNames(coef_names, readable)
}


# ---------------------------------------------------------------------------- #
# Engine helpers                                                               #
# ---------------------------------------------------------------------------- #

#' Extract the native model object from a fit_model result
#'
#' For the `"lm"` engine, `fit_model()` stores parsnip objects. The underlying
#' `lm`/`glm` lives at `fit$fit$fit`. For `"fixest"`, `"ranger"`, and
#' `"xgboost"` the object is already native. This function unwraps the parsnip
#' layer when needed so downstream plot/table functions always receive a plain
#' R model object.
#'
#' @param fit    A model object as stored in `fit_model()$fit1` etc.
#' @param engine Scalar character engine key (e.g. `"lm"`, `"fixest"`).
#'
#' @return The native model object (e.g. an `lm`, `glm`, `fixest`, or
#'   `ranger` object).
#'
#' @export
extract_native_fit <- function(fit, engine = "fixest") {
  if (identical(engine, "lm")) {
    # parsnip wraps the fit: ._parsnip_model$fit$fit -> native lm/glm
    native <- tryCatch(fit$fit$fit, error = function(e) NULL)
    if (!is.null(native)) return(native)
    # fallback: maybe already unwrapped
    return(fit)
  }
  # fixest, ranger, xgboost — object is already native
  fit
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


#' Plot standard diagnostic panels for a fitted model
#'
#' For `lm` / `glm` objects uses base-R `plot()` (four panels). For fixest
#' objects produces a residual-vs-fitted ggplot since base-R plot dispatch
#' doesn't support fixest. Returns a blank plot on error.
#'
#' @param model  A native model object (output of `extract_native_fit()`).
#' @param engine Scalar character engine key from `fit_model()$engine`.
#'
#' @return A `ggplot` object, or a recorded-plot wrapped in `cowplot::ggdraw`.
#'
#' @export
plot_diagnostics <- function(model, engine = "lm") {
  blank_plot <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = msg,
                        size = 3.5, color = "grey40", hjust = 0.5, vjust = 0.5) +
      ggplot2::theme_void()
  }

  if (identical(engine, "fixest")) {
    p <- tryCatch({
      res    <- stats::residuals(model)
      fitted <- stats::fitted(model)
      ggplot2::ggplot(
        data.frame(fitted = fitted, residuals = res),
        ggplot2::aes(x = .data$fitted, y = .data$residuals)
      ) +
        ggplot2::geom_point(alpha = 0.15) +
        ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
        ggplot2::geom_smooth(method = "loess", se = FALSE, color = "steelblue",
                             linewidth = 0.8, formula = y ~ x) +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = "Residuals vs Fitted",
                      x = "Fitted values", y = "Residuals")
    }, error = function(e) blank_plot(paste("Diagnostic plot error:", conditionMessage(e))))
    return(p)
  }

  # lm / glm branch — base-R plot(), capture as grob
  tryCatch({
    grDevices::dev.new(width = 8, height = 8, noRStudioGD = TRUE)
    on.exit(grDevices::dev.off(), add = TRUE)
    graphics::par(mfrow = c(2, 2))
    plot(model)
    rp <- grDevices::recordPlot()
    cowplot::ggdraw() + cowplot::draw_grob(grid::rasterGrob(rp))
  }, error = function(e) blank_plot(paste("Diagnostic plot error:", conditionMessage(e))))
}


# ---------------------------------------------------------------------------- #
# Plot / table builders                                                        #
# ---------------------------------------------------------------------------- #

#' Build a coefficient plot across three progressive model fits
#'
#' Dispatches on engine type:
#' - `"fixest"` — uses `fixest::iplot()` to show weather coefficients with
#'   clustered SEs, respecting the absorbed fixed effects.
#' - All other engines — uses `jtools::plot_summs()` on three native
#'   `lm`/`glm` objects.
#'
#' @param fit1,fit2,fit3    Native model objects — no FE / FE / FE + controls.
#' @param weather_terms     Character vector of base weather variable names.
#' @param interaction_terms Character vector of interaction term strings.
#' @param outcome_label     Scalar character label for the x-axis.
#' @param label_fun         Function mapping variable names to readable labels.
#' @param engine            Scalar character engine key from `mf$engine`.
#'
#' @return A `ggplot` object.
#'
#' @export
make_coefplot <- function(fit1, fit2, fit3,
                           weather_terms,
                           interaction_terms,
                           outcome_label = "outcome",
                           label_fun     = identity,
                           engine        = "lm") {

  blank_plot <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = msg, color = "grey40") +
      ggplot2::theme_void()
  }

  # --- fixest branch ---------------------------------------------------------
  if (identical(engine, "fixest")) {
    if (!requireNamespace("fixest", quietly = TRUE))
      return(blank_plot("Package 'fixest' is required for engine = 'fixest'."))

    model_list <- list(
      "No FE"         = fit1,
      "FE"            = fit2,
      "FE + controls" = fit3
    )

    p <- tryCatch({
      # Collect HC-robust coefficient tables from all three models.
      coef_data <- purrr::imap_dfr(model_list, function(fit, model_name) {
        ct <- tryCatch(
          as.data.frame(fixest::coeftable(fit, se = "hetero")),
          error = function(e) NULL
        )
        if (is.null(ct)) return(NULL)
        ct$term  <- rownames(ct)
        ct$model <- model_name
        ct
      })

      if (nrow(coef_data) == 0)
        return(blank_plot("No coefficients returned by fixest."))

      # Filter to weather + interaction terms using the same helper as lm/glm.
      keep_terms <- weather_coef_names(fit3, weather_terms, interaction_terms)
      coef_data  <- coef_data[coef_data$term %in% keep_terms, ]

      if (nrow(coef_data) == 0)
        return(blank_plot("No weather coefficients found to plot."))

      # Apply human-readable labels via label_fun, same as make_coef_map().
      coef_map            <- make_coef_map(keep_terms, label_fun)
      coef_data$label     <- coef_map[coef_data$term]
      coef_data$label     <- ifelse(is.na(coef_data$label), coef_data$term, coef_data$label)
      coef_data$conf.low  <- coef_data$Estimate - 1.96 * coef_data$`Std. Error`
      coef_data$conf.high <- coef_data$Estimate + 1.96 * coef_data$`Std. Error`
      coef_data$model     <- factor(coef_data$model,
                                    levels = c("No FE", "FE", "FE + controls"))

      ggplot2::ggplot(
        coef_data,
        ggplot2::aes(
          x      = Estimate,
          y      = stringr::str_wrap(label, 25),
          colour = model,
          shape  = model
        )
      ) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
        ggplot2::geom_pointrange(
          ggplot2::aes(xmin = conf.low, xmax = conf.high),
          position = ggplot2::position_dodge(width = 0.5)
        ) +
        ggplot2::scale_colour_brewer(palette = "Set1", name = NULL) +
        ggplot2::scale_shape_discrete(name = NULL) +
        ggplot2::labs(
          x = stringr::str_wrap(paste0("Effect on ", outcome_label), 50),
          y = NULL
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "bottom")
    },
    error = function(e) blank_plot(paste0("fixest coefplot error: ", conditionMessage(e)))
    )
    return(p)
  }

  # --- lm / glm branch -------------------------------------------------------
  safe_coef <- function(fit) {
    tryCatch(names(stats::coef(fit)), error = function(e) character(0))
  }

  coef_names <- weather_coef_names(fit3, weather_terms, interaction_terms)
  coef_names <- coef_names[
    coef_names %in% safe_coef(fit1) &
    coef_names %in% safe_coef(fit2)
  ]

  if (length(coef_names) == 0)
    return(blank_plot("No weather coefficients found to plot."))

  tryCatch(
    jtools::plot_summs(
      fit1, fit2, fit3,
      robust      = "HC3",
      coefs       = make_coef_map(coef_names, label_fun),
      model.names = c("No FE", "FE", "FE + controls")
    ) +
      ggplot2::scale_y_discrete(labels = function(x) stringr::str_wrap(x, 25)) +
      ggplot2::labs(x = stringr::str_wrap(paste0("Effect on ", outcome_label), 50)),
    error = function(e) blank_plot(paste0("Coefficient plot error: ", conditionMessage(e)))
  )
}

#' Generate Marginal Effect / Interaction Plots with human-readable labels
#'
#' For `"fixest"` engine models, uses `fixest::coefplot()` since
#' `jtools`/`interactions` do not support fixest objects directly.
#' For all other engines, dispatches to `jtools::effect_plot()` or
#' `interactions::interact_plot()` / `interactions::cat_plot()`.
#'
#' @param fit A native model object.
#' @param pred_var Character. The base name of the weather variable.
#' @param interaction_terms Character vector of interaction strings.
#' @param is_binned Logical. Whether `pred_var` is a binned/factor variable.
#' @param label_fun Function mapping variable names to readable labels.
#' @param engine Scalar character engine key from `mf$engine`.
#'
#' @return A `ggplot` object.
#'
#' @export
make_weather_effect_plot <- function(fit, pred_var,
                                     interaction_terms = character(0),
                                     is_binned         = FALSE,
                                     label_fun         = identity,
                                     engine            = "lm") {

  # 1. Resolve Human-Readable Labels
  pred_lab   <- label_fun(pred_var)
  y_var_name <- tryCatch(
    as.character(stats::formula(fit)[[2]]),
    error = function(e) "outcome"
  )
  y_lab <- label_fun(y_var_name)

  # 2. Guard: pred_var must exist in model frame with at least one finite value.
  mf <- tryCatch(
    {
      mm <- stats::model.matrix(fit)
      as.data.frame(mm)
    },
    error = function(e) tryCatch(stats::model.frame(fit), error = function(e2) NULL)
  )
  pred_vals <- if (!is.null(mf) && pred_var %in% names(mf)) mf[[pred_var]] else NULL

  blank_plot <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = msg,
                        size = 3.5, color = "grey40", hjust = 0.5, vjust = 0.5) +
      ggplot2::theme_void()
  }

  if (is.null(pred_vals)) {
    return(blank_plot(paste0("'", pred_var, "' not found in model frame.")))
  }

  if (!is.factor(pred_vals) && !any(is.finite(pred_vals))) {
    return(blank_plot(paste0("No finite values for '", pred_var, "' — cannot build effect plot.")))
  }

  # 3. fixest branch: predict over a grid of pred_var x moderator levels,
  #    replicating the interactions::interact_plot() style from the lm/glm branch.
  if (identical(engine, "fixest")) {
    if (!requireNamespace("fixest", quietly = TRUE))
      return(blank_plot("Package 'fixest' is required for engine = 'fixest'."))

    # Resolve moderator (same logic as lm/glm branch below)
    modx_var <- NULL
    modx_lab <- NULL
    if (length(interaction_terms) > 0) {
      match_term <- grep(paste0("^", pred_var, ":"), interaction_terms, value = TRUE)
      if (length(match_term) > 0) {
        modx_var <- strsplit(match_term[1], ":")[[1]][2]
        modx_lab <- label_fun(modx_var)
      }
    }

    p <- tryCatch({
      mm     <- as.data.frame(stats::model.matrix(fit))
      betas  <- stats::coef(fit)
      vcov_m <- stats::vcov(fit)
      n_grid <- 50L

      # Manual prediction: avoids stats::predict.fixest() which requires FE
      # columns in newdata (they are absorbed; absent from model.matrix()).
      # fit = X_new %*% beta; SE from delta method sqrt(diag(X V X'))
      fixest_predict_manual <- function(X_new) {
        common <- intersect(colnames(X_new), names(betas[!is.na(betas)]))
        X_sub  <- as.matrix(X_new[, common, drop = FALSE])
        b_sub  <- betas[common]
        v_sub  <- vcov_m[common, common, drop = FALSE]
        list(
          fit = as.numeric(X_sub %*% b_sub),
          se  = sqrt(pmax(0, rowSums((X_sub %*% v_sub) * X_sub)))
        )
      }

      x_seq <- if (is_binned || is.factor(mm[[pred_var]])) {
        sort(unique(mm[[pred_var]]))
      } else {
        seq(min(mm[[pred_var]], na.rm = TRUE),
            max(mm[[pred_var]], na.rm = TRUE),
            length.out = n_grid)
      }

      # Hold all other predictors at their mean/mode
      other_means <- lapply(mm, function(col) {
        if (is.numeric(col)) mean(col, na.rm = TRUE)
        else names(sort(table(col), decreasing = TRUE))[1]
      })

      if (!is.null(modx_var) && modx_var %in% names(mm)) {
        modx_col    <- mm[[modx_var]]
        modx_uniq   <- sort(unique(modx_col))
        is_cat_modx <- is.factor(modx_col) || is.character(modx_col) ||
                       length(modx_uniq) <= 5

        modx_vals <- if (is_cat_modx) {
          modx_uniq
        } else {
          modx_mean <- mean(modx_col, na.rm = TRUE)
          modx_sd   <- stats::sd(modx_col, na.rm = TRUE)
          c(modx_mean - modx_sd, modx_mean, modx_mean + modx_sd)
        }

        grid     <- expand.grid(.pred = x_seq, .modx = modx_vals, stringsAsFactors = FALSE)
        new_data <- as.data.frame(lapply(other_means, rep, times = nrow(grid)))
        new_data[[pred_var]] <- grid$.pred
        new_data[[modx_var]] <- grid$.modx

        # Rebuild interaction columns present in the original model matrix
        for (nm in colnames(mm)) {
          if (grepl(":", nm, fixed = TRUE) && !nm %in% names(new_data)) {
            parts <- strsplit(nm, ":")[[1]]
            if (all(parts %in% names(new_data)))
              new_data[[nm]] <- Reduce(`*`, new_data[parts])
          }
        }
        if ("(Intercept)" %in% colnames(mm)) new_data[["(Intercept)"]] <- 1

        preds        <- fixest_predict_manual(new_data)
        new_data$fit <- preds$fit
        new_data$se  <- preds$se

        # Categorical/binary: use raw values as labels; continuous: mean ± SD
        new_data$.modx_label <- factor(
          if (is_cat_modx) as.character(new_data[[modx_var]])
          else paste0(modx_lab, " = ", round(new_data[[modx_var]], 2))
        )

        ggplot2::ggplot(
          new_data,
          ggplot2::aes(
            x      = .data[[pred_var]],
            y      = .data$fit,
            colour = .data$.modx_label,
            fill   = .data$.modx_label
          )
        ) +
          ggplot2::geom_ribbon(
            ggplot2::aes(ymin = fit - 1.96 * se, ymax = fit + 1.96 * se),
            alpha = 0.15, colour = NA
          ) +
          ggplot2::geom_line(linewidth = 0.9) +
          ggplot2::scale_colour_brewer(palette = "Set1", name = modx_lab) +
          ggplot2::scale_fill_brewer(palette = "Set1", name = modx_lab) +
          ggplot2::labs(
            title = paste("Impact of", pred_lab, "by", modx_lab),
            x     = pred_lab,
            y     = paste("Predicted", y_lab)
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title      = ggplot2::element_text(face = "bold", hjust = 0.5, size = 11),
            legend.position = "bottom"
          )

      } else {
        new_data             <- as.data.frame(lapply(other_means, rep, times = length(x_seq)))
        new_data[[pred_var]] <- x_seq

        if ("(Intercept)" %in% colnames(mm)) new_data[["(Intercept)"]] <- 1

        preds        <- fixest_predict_manual(new_data)
        new_data$fit <- preds$fit
        new_data$se  <- preds$se

        ggplot2::ggplot(new_data, ggplot2::aes(x = .data[[pred_var]], y = .data$fit)) +
          ggplot2::geom_ribbon(
            ggplot2::aes(ymin = fit - 1.96 * se, ymax = fit + 1.96 * se),
            alpha = 0.2, fill = "steelblue"
          ) +
          ggplot2::geom_line(colour = "steelblue", linewidth = 0.9) +
          ggplot2::labs(
            title = paste("Predicted", y_lab, "vs", pred_lab),
            x     = pred_lab,
            y     = paste("Predicted", y_lab)
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 11)
          )
      }
    },
    error = function(e) blank_plot(paste0("fixest effect plot error: ", conditionMessage(e)))
    )
    return(p)
  }

  # 4. Look for moderator (lm / glm path)
  modx_var <- NULL
  modx_lab <- NULL
  if (length(interaction_terms) > 0) {
    match_term <- grep(paste0("^", pred_var, ":"), interaction_terms, value = TRUE)
    if (length(match_term) > 0) {
      modx_var <- strsplit(match_term[1], ":")[[1]][2]
      modx_lab <- label_fun(modx_var)
    }
  }

  # 5. Build plot — tryCatch converts internal range/Inf errors to a blank plot
  build <- function() {
    if (is.null(modx_var)) {
      jtools::effect_plot(
        model      = fit,
        pred       = !!rlang::sym(pred_var),
        interval   = TRUE,
        colors     = "blue",
        main.title = paste("Predicted", y_lab, "vs", pred_lab)
      )
    } else if (is_binned) {
      interactions::cat_plot(
        model       = fit,
        pred        = !!rlang::sym(pred_var),
        modx        = !!rlang::sym(modx_var),
        geom        = "line",
        interval    = TRUE,
        modx.labels = NULL,
        main.title  = paste("Impact of", pred_lab, "by", modx_lab)
      )
    } else {
      interactions::interact_plot(
        model      = fit,
        pred       = !!rlang::sym(pred_var),
        modx       = !!rlang::sym(modx_var),
        interval   = TRUE,
        main.title = paste("Impact of", pred_lab, "by", modx_lab)
      )
    }
  }

  p <- withCallingHandlers(
    tryCatch(build(), error = function(e) {
      blank_plot(paste0("Effect plot error:\n", conditionMessage(e)))
    }),
    warning = function(w) {
      if (grepl("Inf|-Inf|non-missing|returning Inf", conditionMessage(w), ignore.case = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  # 6. Final theme and label overrides
  p +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x      = pred_lab,
      y      = paste("Predicted", y_lab),
      colour = modx_lab,
      fill   = modx_lab
    ) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", hjust = 0.5, size = 11),
      legend.position = "bottom"
    )
}


#' Build an HTML regression table across three progressive model fits
#'
#' For `"fixest"` engine models, uses `fixest::etable()` and converts to HTML
#' via `knitr::kable()`. For all other engines, uses `jtools::export_summs()`
#' with `huxtable`.
#'
#' @param fit1,fit2,fit3    Native model objects.
#' @param weather_terms     Character vector of base weather variable names.
#' @param interaction_terms Character vector of interaction term strings.
#' @param label_fun         Function mapping variable names to readable labels.
#' @param engine            Scalar character engine key from `mf$engine`.
#'
#' @return An `htmltools::HTML` string.
#'
#' @export
make_regtable <- function(fit1, fit2, fit3,
                           weather_terms,
                           interaction_terms,
                           label_fun = identity,
                           engine    = "lm") {

  # fixest objects are not supported by jtools::export_summs()
  if (identical(engine, "fixest")) {
    if (!requireNamespace("fixest", quietly = TRUE))
      return(htmltools::HTML("<p>Package 'fixest' required for regression table.</p>"))

    tryCatch({
      et <- fixest::etable(
        fit1, fit2, fit3,
        headers  = c("No FE", "FE", "FE + controls"),
        se.below = TRUE,
        digits   = 3
      )
      htmltools::HTML(
        knitr::kable(et, format = "html",
                     table.attr = "class='table table-sm table-bordered'")
      )
    }, error = function(e) {
      htmltools::HTML(paste0("<p>Regression table error: ", conditionMessage(e), "</p>"))
    })
  } else {
    # lm / glm path
    coef_names <- weather_coef_names(fit3, weather_terms, interaction_terms)
    ht <- jtools::export_summs(
      fit1, fit2, fit3,
      robust      = "HC3",
      model.names = c("No FE", "FE", "FE + controls"),
      coefs       = make_coef_map(coef_names, label_fun),
      digits      = 3
    )
    htmltools::HTML(huxtable::to_html(ht))
  }
}


# ---------------------------------------------------------------------------- #
# Model fit diagnostic plots                                                   #
# ---------------------------------------------------------------------------- #

#' Plot residuals against a single weather predictor
#'
#' Scatters raw residuals against `haz_var` with a horizontal zero-line and
#' binned mean overlay. Returns `NULL` invisibly when `haz_var` is absent from
#' the model frame.
#'
#' @param model   A native `lm`/`glm` object.
#' @param haz_var Scalar character name of the weather predictor.
#' @param x_label Scalar character x-axis label.
#'
#' @return A `ggplot` object, or `NULL` invisibly.
#'
#' @export
plot_resid_weather <- function(model, haz_var, x_label = haz_var) {
  # stats::model.frame() fails on fixest objects; try model.matrix() as fallback.
  df <- tryCatch(stats::model.frame(model), error = function(e) NULL)

  if (is.null(df) || !haz_var %in% names(df)) {
    mm <- tryCatch(as.data.frame(stats::model.matrix(model)), error = function(e) NULL)
    if (!is.null(mm) && haz_var %in% names(mm)) {
      df <- mm
    } else {
      return(invisible(NULL))
    }
  }

  res <- tryCatch(stats::residuals(model), error = function(e) NULL)
  if (is.null(res)) return(invisible(NULL))

  # Lengths may differ after NA removal in fixest
  x_vals <- df[[haz_var]]
  n      <- min(length(x_vals), length(res))
  plot_data <- data.frame(x = x_vals[seq_len(n)], residuals = res[seq_len(n)])

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$residuals)) +
    ggplot2::geom_point(alpha = 0.1) +
    ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
    ggplot2::stat_summary_bin(fun = "mean", bins = 20,
                              color = "orange", size = 2, geom = "point") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = stringr::str_wrap(x_label, 40), y = "Residuals")
}


#' Plot predicted vs actual distribution
#'
#' For linear models: overlaid histogram of actual vs predicted values.
#' For logistic models: confusion-matrix tile plot with percentage labels.
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
      f <- tryCatch(stats::fitted(model),    error = function(e2) NULL)
      r <- tryCatch(stats::residuals(model), error = function(e2) NULL)
      if (!is.null(f) && !is.null(r)) f + r else NULL
    }
  )

  if (is.null(actual)) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "Could not recover outcome values from model.",
                          size = 3.5, color = "grey40", hjust = 0.5) +
        ggplot2::theme_void()
    )
  }

  if (!is_logistic) {
    predicted <- tryCatch(stats::fitted(model), error = function(e) stats::predict(model))
    n         <- min(length(actual), length(predicted))
    plot_data <- data.frame(
      Type   = rep(c("Survey", "Predicted"), each = n),
      Values = c(actual[seq_len(n)], predicted[seq_len(n)])
    )
    ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$Values, fill = .data$Type)) +
      ggplot2::geom_histogram(
        ggplot2::aes(y = 100 * ggplot2::after_stat(count) / sum(ggplot2::after_stat(count))),
        position = "dodge", alpha = 0.7, bins = 30
      ) +
      ggplot2::scale_fill_manual(values = c("Survey" = "steelblue", "Predicted" = "orange")) +
      ggplot2::labs(x = stringr::str_wrap(outcome_label, 40),
                    y = "Share of households (%)") +
      ggplot2::theme_minimal()

  } else {
    predicted  <- tryCatch(
      stats::fitted(model),
      error = function(e) stats::predict(model, type = "response")
    )
    conf_mat   <- table(
      Predicted = factor(ifelse(predicted > 0.5, 1, 0), levels = c(0, 1)),
      Actual    = factor(actual, levels = c(0, 1))
    )
    cm_df <- as.data.frame(conf_mat)
    cm_df$Percent <- cm_df$Freq / sum(conf_mat) * 100
    levels(cm_df$Actual)    <- c("No", "Yes")
    levels(cm_df$Predicted) <- c("No", "Yes")

    ggplot2::ggplot(cm_df, ggplot2::aes(x = .data$Actual, y = .data$Predicted,
                                         fill = .data$Percent)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", .data$Percent)),
                         vjust = 1) +
      ggplot2::scale_fill_gradient(low = "lightblue", high = "steelblue") +
      ggplot2::labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none")
  }
}


#' Compute model fit statistics as a data frame
#'
#' Returns a two-column data frame (`Statistic`, `Value`) appropriate for the
#' model type and engine. For `"fixest"` engine models, pulls fit stats from
#' `fixest::fitstat()` rather than `summary()` which returns a different
#' structure incompatible with `round()`.
#'
#' @param model       A native model object (output of `extract_native_fit()`).
#' @param is_logistic Scalar logical.
#' @param engine      Scalar character engine key from `fit_model()$engine`.
#'
#' @return A data frame with columns `Statistic` and `Value`.
#'
#' @export
calc_fit_stats <- function(model, is_logistic, engine = "lm") {

  fmt_num <- function(x, digits = 3) {
    x <- suppressWarnings(as.numeric(x))
    if (is.na(x)) return(NA_character_)
    as.character(round(x, digits))
  }

  # ---- fixest branch --------------------------------------------------------
  if (identical(engine, "fixest")) {
    nobs_val <- tryCatch(format(stats::nobs(model), big.mark = ","),
                         error = function(e) NA_character_)

    if (!is_logistic) {
      r2_val   <- tryCatch(fmt_num(fixest::r2(model, "r2")),   error = function(e) NA_character_)
      ar2_val  <- tryCatch(fmt_num(fixest::r2(model, "ar2")),  error = function(e) NA_character_)
      wr2_val  <- tryCatch(fmt_num(fixest::r2(model, "wr2")),  error = function(e) NA_character_)
      return(data.frame(
        Statistic = c("Observations", "R-squared", "Adj. R-squared", "Within R-squared"),
        Value     = c(nobs_val, r2_val, ar2_val, wr2_val),
        stringsAsFactors = FALSE
      ))
    } else {
      aic_val <- tryCatch(fmt_num(stats::AIC(model), 0), error = function(e) NA_character_)
      pr2_val <- tryCatch(fmt_num(fixest::r2(model, "pr2")), error = function(e) NA_character_)
      return(data.frame(
        Statistic = c("Observations", "McFadden R\u00b2", "AIC"),
        Value     = c(nobs_val, pr2_val, aic_val),
        stringsAsFactors = FALSE
      ))
    }
  }

  # ---- lm / glm branch ------------------------------------------------------
  if (!is_logistic) {
    s <- summary(model)
    data.frame(
      Statistic = c("Observations", "R-squared", "Adjusted R-squared", "F-statistic"),
      Value     = c(
        format(stats::nobs(model), big.mark = ","),
        fmt_num(s$r.squared),
        fmt_num(s$adj.r.squared),
        fmt_num(s$fstatistic[1], 1)
      ),
      stringsAsFactors = FALSE
    )
  } else {
    ll_model <- as.numeric(stats::logLik(model))
    ll_null  <- as.numeric(-0.5 * model$null.deviance)
    mcfadden <- round(1 - ll_model / ll_null, 3)

    actual     <- stats::model.frame(model)[[1]]
    predicted  <- stats::predict(model, type = "response")
    pred_class <- ifelse(predicted > 0.5, 1, 0)
    cm         <- table(Predicted = pred_class, Actual = actual)

    TP <- cm["1", "1"]; TN <- cm["0", "0"]
    FP <- cm["1", "0"]; FN <- cm["0", "1"]

    data.frame(
      Statistic = c("Observations", "McFadden R\u00b2", "AIC",
                    "Accuracy", "Precision", "Recall"),
      Value     = c(
        format(stats::nobs(model), big.mark = ","),
        fmt_num(mcfadden),
        fmt_num(stats::AIC(model), 0),
        fmt_num((TP + TN) / (TP + TN + FP + FN)),
        fmt_num(TP / (TP + FP)),
        fmt_num(TP / (TP + FN))
      ),
      stringsAsFactors = FALSE
    )
  }
}

#' Plot relative importance of predictors (LMG method, linear models only)
#'
#' Calls `relaimpo::calc.relimp()` and renders a horizontal bar chart. Returns
#' an informative blank plot on failure.
#'
#' @param model     A native `lm` object.
#' @param var_info  Optional data frame with columns `name` and `label` used
#'   to map variable names to readable labels.
#'
#' @return A `ggplot` object.
#'
#' @export
plot_relaimpo <- function(model, var_info = NULL) {
  total_r2 <- tryCatch(summary(model)$r.squared, error = function(e) NA_real_)

  rel_imp <- tryCatch(
    relaimpo::calc.relimp(model, type = "lmg"),
    error = function(e) e
  )

  if (inherits(rel_imp, "error")) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = paste("Relative importance unavailable:",
                                        conditionMessage(rel_imp)),
                          hjust = 0.5) +
        ggplot2::theme_void()
    )
  }

  imp_df <- data.frame(
    Variable     = names(rel_imp$lmg),
    Contribution = as.numeric(rel_imp$lmg),
    stringsAsFactors = FALSE
  )

  if (!is.null(var_info)) {
    imp_df <- dplyr::left_join(imp_df, var_info[, c("name", "label")],
                                by = c("Variable" = "name")) |>
      dplyr::mutate(label = dplyr::if_else(is.na(.data$label), .data$Variable, .data$label))
  } else {
    imp_df$label <- imp_df$Variable
  }

  ggplot2::ggplot(imp_df, ggplot2::aes(x = reorder(.data$label, .data$Contribution),
                                        y = .data$Contribution)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::geom_text(ggplot2::aes(label = round(.data$Contribution, 3)),
                       hjust = -0.2, size = 3.5) +
    ggplot2::geom_hline(yintercept = total_r2, linetype = "dashed", color = "red") +
    ggplot2::annotate("text", x = Inf, y = total_r2,
                      label = paste("Total R\u00b2 =", round(total_r2, 3)),
                      hjust = 1.1, vjust = -0.5, color = "red", size = 3.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, 30)) +
    ggplot2::labs(x = "", y = "R-squared contribution") +
    ggplot2::theme_minimal()
}