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
#' For `"fixest"`, `"ranger"`, and `"xgboost"` engines the object stored by
#' `fit_model()` is already a native R model object — no unwrapping needed.
#'
#' @param fit    A model object as stored in `fit_model()$fit1` etc.
#' @param engine Scalar character engine key (e.g. `"fixest"`). Kept for
#'   backward compatibility but currently unused.
#'
#' @return The native model object.
#'
#' @export
extract_native_fit <- function(fit, engine = "fixest") {
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
plot_diagnostics <- function(model, engine = "fixest") {
  blank_plot <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = msg,
                        size = 3.5, color = "grey40", hjust = 0.5, vjust = 0.5) +
      ggplot2::theme_void()
  }

  tryCatch({
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
}


# ---------------------------------------------------------------------------- #
# Plot / table builders                                                        #
# ---------------------------------------------------------------------------- #

#' Build a coefficient plot across three progressive model fits
#'
#' Uses `fixest` HC-robust SEs and plots all three models side-by-side,
#' replicating the `jtools::plot_summs()` style.
#'
#' @param fit1,fit2,fit3    Native fixest model objects.
#' @param weather_terms     Character vector of base weather variable names.
#' @param interaction_terms Character vector of interaction term strings.
#' @param outcome_label     Scalar character label for the x-axis.
#' @param label_fun         Function mapping variable names to readable labels.
#' @param engine            Scalar character engine key (kept for compat).
#'
#' @return A `ggplot` object.
#'
#' @export
make_coefplot <- function(fit1, fit2, fit3,
                           weather_terms,
                           interaction_terms,
                           outcome_label = "outcome",
                           label_fun     = identity,
                           engine        = "fixest") {

  blank_plot <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = msg, color = "grey40") +
      ggplot2::theme_void()
  }

  if (!requireNamespace("fixest", quietly = TRUE))
    return(blank_plot("Package 'fixest' is required."))

  model_list <- list(
    "No FE"         = fit1,
    "FE"            = fit2,
    "FE + controls" = fit3
  )

  p <- tryCatch({
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

    keep_terms <- weather_coef_names(fit3, weather_terms, interaction_terms)
    coef_data  <- coef_data[coef_data$term %in% keep_terms, ]

    if (nrow(coef_data) == 0)
      return(blank_plot("No weather coefficients found to plot."))

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
      ggplot2::theme_bw(base_size = 14) +
      ggplot2::theme(
        legend.position = "bottom",
        panel.border = ggplot2::element_blank()
      )
  },
  error = function(e) blank_plot(paste0("Coefficient plot error: ", conditionMessage(e)))
  )
  p
}

#' Generate Marginal Effect / Interaction Plots with human-readable labels
#'
#' Predicts over a grid of `pred_var` × moderator levels using manual linear
#' prediction (bypassing `stats::predict()` which requires FE columns in
#' `newdata`). Replicates the `interactions::interact_plot()` style.
#'
#' @param fit A native fixest model object.
#' @param pred_var Character. The base name of the weather variable.
#' @param interaction_terms Character vector of interaction strings.
#' @param is_binned Logical. Whether `pred_var` is a binned/factor variable.
#' @param label_fun Function mapping variable names to readable labels.
#' @param engine Scalar character engine key (kept for compat).
#'
#' @return A `ggplot` object.
#'
#' @export
make_weather_effect_plot <- function(fit, pred_var,
                                     interaction_terms = character(0),
                                     is_binned         = FALSE,
                                     label_fun         = identity,
                                     engine            = "fixest") {

  pred_lab   <- label_fun(pred_var)
  y_var_name <- tryCatch(
    as.character(stats::formula(fit)[[2]]),
    error = function(e) "outcome"
  )
  y_lab <- label_fun(y_var_name)

  mf <- tryCatch(
    as.data.frame(stats::model.matrix(fit)),
    error = function(e) tryCatch(stats::model.frame(fit), error = function(e2) NULL)
  )
  pred_vals <- if (!is.null(mf) && pred_var %in% names(mf)) mf[[pred_var]] else NULL

  blank_plot <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = msg,
                        size = 3.5, color = "grey40", hjust = 0.5, vjust = 0.5) +
      ggplot2::theme_void()
  }

  if (is.null(pred_vals))
    return(blank_plot(paste0("'", pred_var, "' not found in model frame.")))

  if (!is.factor(pred_vals) && !any(is.finite(pred_vals)))
    return(blank_plot(paste0("No finite values for '", pred_var, "' — cannot build effect plot.")))

  if (!requireNamespace("fixest", quietly = TRUE))
    return(blank_plot("Package 'fixest' is required."))

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

  p <- tryCatch({
    mm     <- as.data.frame(stats::model.matrix(fit))
    betas  <- stats::coef(fit)
    vcov_m <- stats::vcov(fit)
    n_grid <- 50L

    # Manual prediction: X_new %*% beta, SE via delta method sqrt(diag(X V X'))
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
        ggplot2::theme_minimal(base_size = 14) +
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
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 11)
        )
    }
  },
  error = function(e) blank_plot(paste0("fixest effect plot error: ", conditionMessage(e)))
  )
  p
}


#' Build an AER-style regression table from up to 3 fixest models
#'
#' @param fit1,fit2,fit3 fixest model objects.
#' @param weather_terms  Character vector of weather variable names.
#' @param interaction_terms Character vector of interaction variable names.
#' @param label_fun      Function mapping variable names to human labels.
#' @param engine         Character, estimation engine (default "fixest").
#'
#' @return A data.frame suitable for renderTable().
#' @noRd
make_regtable <- function(fit1, fit2, fit3,
                          weather_terms     = character(0),
                          interaction_terms = character(0),
                          label_fun         = identity,
                          engine            = "fixest") {

  if (!inherits(fit1, "fixest") || !inherits(fit2, "fixest") || !inherits(fit3, "fixest")) {
    return(htmltools::tags$p("All models must be fixest objects."))
  }

  # --- Extract coefficients and SEs -----------------------------------------
  extract_coefs <- function(fit) {
    ct  <- summary(fit)$coeftable
    nms <- rownames(ct)
    cf  <- ct[, 1]
    se  <- ct[, 2]
    pv  <- ct[, 4]

    stars <- ifelse(pv < 0.001, "***",
             ifelse(pv < 0.01,  "**",
             ifelse(pv < 0.05,  "*",
             ifelse(pv < 0.1,   "\u2020", ""))))

    est <- paste0(formatC(cf, format = "f", digits = 3), stars)
    se_fmt <- paste0("(", formatC(se, format = "f", digits = 3), ")")

    list(names = nms, est = est, se = se_fmt)
  }

  c1 <- extract_coefs(fit1)
  c2 <- extract_coefs(fit2)
  c3 <- extract_coefs(fit3)

  all_vars <- unique(c(c1$names, c2$names, c3$names))

  lookup <- function(coef_list, var) {
    idx <- match(var, coef_list$names)
    if (is.na(idx)) list(est = "", se = "") else list(est = coef_list$est[idx], se = coef_list$se[idx])
  }

  # --- Build HTML table -----------------------------------------------------
  css <- "
    .aer-table { border-collapse:collapse; font-family:'Times New Roman',Times,serif; font-size:14px; margin:20px 0; }
    .aer-table th, .aer-table td { padding:2px 14px; text-align:center; }
    .aer-table th { font-weight:normal; border-bottom:1px solid #000; }
    .aer-table .topline { border-top:2px solid #000; }
    .aer-table .midline td { border-bottom:1px solid #000; }
    .aer-table .botline td { border-top:1px solid #000; }
    .aer-table .var-name { text-align:left; font-style:italic; }
    .aer-table .se-row td { color:#555; }
    .aer-table .stat-label { text-align:left; }
  "

  header <- paste0(
    "<tr class='topline'>",
    "<th style='text-align:left; border-top:2px solid #000; border-bottom:1px solid #000;'></th>",
    "<th style='border-top:2px solid #000; border-bottom:1px solid #000;'>(1)</th>",
    "<th style='border-top:2px solid #000; border-bottom:1px solid #000;'>(2)</th>",
    "<th style='border-top:2px solid #000; border-bottom:1px solid #000;'>(3)</th>",
    "</tr>",
    "<tr>",
    "<th style='text-align:left;'></th>",
    "<th>No FE</th>",
    "<th>FE</th>",
    "<th>FE + Controls</th>",
    "</tr>"
  )

  body_rows <- ""
  for (v in all_vars) {
    lab <- tryCatch(label_fun(v), error = function(e) v)
    v1 <- lookup(c1, v); v2 <- lookup(c2, v); v3 <- lookup(c3, v)

    body_rows <- paste0(body_rows,
      "<tr><td class='var-name'>", htmltools::htmlEscape(lab), "</td>",
      "<td>", v1$est, "</td>",
      "<td>", v2$est, "</td>",
      "<td>", v3$est, "</td></tr>",
      "<tr class='se-row'><td></td>",
      "<td>", v1$se, "</td>",
      "<td>", v2$se, "</td>",
      "<td>", v3$se, "</td></tr>"
    )
  }

  # --- Fit statistics -------------------------------------------------------
  safe_nobs <- function(fit) tryCatch(formatC(fit$nobs, format = "d", big.mark = ","), error = function(e) "")
  safe_r2   <- function(fit) tryCatch(formatC(fixest::r2(fit, "r2"), format = "f", digits = 3), error = function(e) "")
  safe_wr2  <- function(fit) tryCatch(formatC(fixest::r2(fit, "wr2"), format = "f", digits = 3), error = function(e) "")
  safe_fe   <- function(fit) {
    nms <- names(fit$fixef_sizes)
    if (is.null(nms)) "\u2014" else paste(nms, collapse = ", ")
  }

  stat_row <- function(label, fn) {
    paste0("<tr><td class='stat-label'>", label, "</td>",
           "<td>", fn(fit1), "</td>",
           "<td>", fn(fit2), "</td>",
           "<td>", fn(fit3), "</td></tr>")
  }

  stats <- paste0(
    "<tr class='botline'><td style='border-top:1px solid #000;'></td>",
    "<td style='border-top:1px solid #000;'></td>",
    "<td style='border-top:1px solid #000;'></td>",
    "<td style='border-top:1px solid #000;'></td></tr>",
    stat_row("Observations", safe_nobs),
    stat_row("R\u00B2", safe_r2),
    stat_row("Within R\u00B2", safe_wr2),
    stat_row("Fixed effects", safe_fe),
    "<tr><td style='border-bottom:2px solid #000;'></td>",
    "<td style='border-bottom:2px solid #000;'></td>",
    "<td style='border-bottom:2px solid #000;'></td>",
    "<td style='border-bottom:2px solid #000;'></td></tr>"
  )

  # --- Significance note ----------------------------------------------------
  note <- "<tr><td colspan='4' style='text-align:left; font-size:11px; padding-top:6px; color:#555;'>
    \u2020 p&lt;0.1, * p&lt;0.05, ** p&lt;0.01, *** p&lt;0.001
  </td></tr>"

  html <- paste0(
    "<style>", css, "</style>",
    "<table class='aer-table'>",
    "<thead>", header, "</thead>",
    "<tbody>", body_rows, stats, note, "</tbody>",
    "</table>"
  )

  htmltools::HTML(html)
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
      f <- tryCatch(stats::fitted(model),    error = function(e) NULL)
      r <- tryCatch(stats::residuals(model), error = function(e) NULL)
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
#' Returns a two-column data frame (`Statistic`, `Value`) using
#' `fixest::fitstat()` / `fixest::r2()`.
#'
#' @param model       A native fixest model object.
#' @param is_logistic Scalar logical.
#' @param engine      Scalar character engine key (kept for compat).
#'
#' @return A data frame with columns `Statistic` and `Value`.
#'
#' @export
calc_fit_stats <- function(model, is_logistic, engine = "fixest") {

  fmt_num <- function(x, digits = 3) {
    x <- suppressWarnings(as.numeric(x))
    if (is.na(x)) return(NA_character_)
    as.character(round(x, digits))
  }

  nobs_val <- tryCatch(format(stats::nobs(model), big.mark = ","),
                       error = function(e) NA_character_)

  if (!is_logistic) {
    r2_val  <- tryCatch(fmt_num(fixest::r2(model, "r2")),  error = function(e) NA_character_)
    ar2_val <- tryCatch(fmt_num(fixest::r2(model, "ar2")), error = function(e) NA_character_)
    wr2_val <- tryCatch(fmt_num(fixest::r2(model, "wr2")), error = function(e) NA_character_)
    data.frame(
      Statistic = c("Observations", "R-squared", "Adj. R-squared", "Within R-squared"),
      Value     = c(nobs_val, r2_val, ar2_val, wr2_val),
      stringsAsFactors = FALSE
    )
  } else {
    aic_val <- tryCatch(fmt_num(stats::AIC(model), 0), error = function(e) NA_character_)
    pr2_val <- tryCatch(fmt_num(fixest::r2(model, "pr2")), error = function(e) NA_character_)
    data.frame(
      Statistic = c("Observations", "McFadden R\u00b2", "AIC"),
      Value     = c(nobs_val, pr2_val, aic_val),
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