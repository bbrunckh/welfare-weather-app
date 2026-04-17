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

#' Extract coefficient names associated with selected weather predictors
#'
#' Returns coefficient names from `fit` that contain any entry in
#' `weather_terms` (using a word-boundary regex match). This typically captures
#' main weather effects as well as transformed/factor-expanded terms and
#' interactions that include those weather variable names.
#'
#' @param fit A native fitted model object (e.g. `fixest`, `lm`, `glm`).
#' @param weather_terms Character vector of weather variable names to match.
#'
#' @return Character vector of coefficient names matching `weather_terms`.
#'
#' @export
weather_coef_names <- function(fit, weather_terms) {
  all_coefs <- names(stats::coef(fit))
  
  # Build safe word-boundary pattern
  pattern <- paste0("\\b(", paste(weather_terms, collapse = "|"), ")\\b")
  
  # Return matching coefficient names
  all_coefs[grepl(pattern, all_coefs)]
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
  if (is.null(df) || is.na(hv) || !(hv %in% names(df))) return(NA_character_)

  x <- df[[hv]]
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)

  labels <- if (is.factor(x)) levels(x) else sort(unique(as.character(x)))
  if (length(labels) == 0) return(NA_character_)

  labels[[1]]
}

# ---------------------------------------------------------------------------- #
# Plot / table builders                                                        #
# ---------------------------------------------------------------------------- #

#' Build a coefficient plot across three progressive model fits
#'
#' Uses `fixest` HC-robust SEs and plots all three models side-by-side,
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
#'
#' @return A `ggplot` object.
#'
#' @export
make_coefplot <- function(fit1, fit2, fit3,
                           weather_terms,
                           interaction_terms,
                           outcome_label = "outcome",
                           label_fun     = identity,
                           engine        = "fixest",
                           rif_grid      = NULL) {

  blank_plot <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = msg, color = "grey40") +
      ggplot2::theme_void()
  }

  # --- RIF branch: beta curve plot -------------------------------------------
  if (identical(engine, "rif") && !is.null(rif_grid)) {
    return(tryCatch({
      taus <- sort(unique(rif_grid$tau))

      # Filter to weather-related terms
      all_terms <- unique(rif_grid$term)
      weather_pattern <- paste0("\\b(", paste(weather_terms, collapse = "|"), ")\\b")
      keep <- grepl(weather_pattern, all_terms)
      if (!any(keep)) keep <- rep(TRUE, length(all_terms))  # fallback: show all
      plot_terms <- all_terms[keep]

      plot_data <- rif_grid[rif_grid$term %in% plot_terms, ]
      plot_data$model_label <- factor(
        dplyr::case_when(
          plot_data$model == 1L ~ "No FE",
          plot_data$model == 2L ~ "FE",
          TRUE                  ~ "FE + controls"
        ),
        levels = c("No FE", "FE", "FE + controls")
      )
      plot_data$term_label <- vapply(
        plot_data$term, function(t) coef_label(t, label_fun), character(1)
      )

      ggplot2::ggplot(plot_data, ggplot2::aes(x = tau, y = estimate,
                                               colour = model_label,
                                               fill   = model_label)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
        ggplot2::geom_ribbon(
          ggplot2::aes(ymin = conf.low, ymax = conf.high),
          alpha = 0.10, colour = NA
        ) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::geom_point(size = 2) +
        ggplot2::facet_wrap(~ term_label, scales = "free_y") +
        ggplot2::scale_x_continuous(
          breaks = taus,
          labels = scales::percent_format(1)
        ) +
        ggplot2::scale_colour_brewer(palette = "Set1", name = NULL) +
        ggplot2::scale_fill_brewer(palette = "Set1", name = NULL) +
        ggplot2::labs(
          title    = "UQR coefficients across the welfare distribution",
          subtitle = "Ribbon = 95% CI",
          x        = "Welfare quantile",
          y        = stringr::str_wrap(paste0("Effect on ", outcome_label), 50)
        ) +
        ggplot2::theme_bw(base_size = 14) +
        ggplot2::theme(
          legend.position = "bottom",
          panel.border = ggplot2::element_blank()
        )
    }, error = function(e) blank_plot(paste0("RIF coefficient plot error: ", conditionMessage(e)))))
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

    keep_terms <- weather_coef_names(fit3, weather_terms)
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
#'
#' @return A `ggplot` object. Returns an informative blank plot on error.
#'
#' @export
make_weather_effect_plot <- function(fit, pred_var, interaction_terms, is_binned,
                                     label_fun, engine, selected_weather = NULL,
                                     weather_df = NULL, rif_grid = NULL) {

  blank_plot <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = msg,
                        size = 3.5, color = "grey40", hjust = 0.5, vjust = 0.5) +
      ggplot2::theme_void()
  }

  # --- RIF branch: weather beta curve across quantiles -----------------------
  if (identical(engine, "rif") && !is.null(rif_grid)) {
    return(tryCatch({
      pred_lab <- label_fun(pred_var)

      # Filter rif_grid to model 3, terms containing pred_var
      grid3 <- rif_grid[rif_grid$model == 3L, ]
      pred_esc <- gsub("([\\[\\]\\(\\)\\^\\$\\.\\*\\+\\?])", "\\\\\\1", pred_var)
      mask <- grepl(paste0("\\b", pred_esc, "\\b"), grid3$term)
      if (!any(mask)) return(blank_plot(paste0("No RIF terms found for '", pred_var, "'.")))
      plot_data <- grid3[mask, ]

      taus <- sort(unique(plot_data$tau))
      plot_data$term_label <- vapply(
        plot_data$term, function(t) coef_label(t, label_fun), character(1)
      )

      n_terms <- length(unique(plot_data$term))
      if (n_terms == 1) {
        # Single term: simple beta curve
        ggplot2::ggplot(plot_data, ggplot2::aes(x = tau, y = estimate)) +
          ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
          ggplot2::geom_ribbon(
            ggplot2::aes(ymin = conf.low, ymax = conf.high),
            alpha = 0.15, fill = "steelblue"
          ) +
          ggplot2::geom_line(colour = "steelblue", linewidth = 0.9) +
          ggplot2::geom_point(colour = "steelblue", size = 2.5) +
          ggplot2::scale_x_continuous(breaks = taus, labels = scales::percent_format(1)) +
          ggplot2::labs(
            title = paste("Effect of", pred_lab, "across the welfare distribution"),
            x     = "Welfare quantile",
            y     = paste("UQR coefficient")
          ) +
          ggplot2::theme_bw(base_size = 14) +
          ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 11))
      } else {
        # Multiple terms (main + interactions): faceted
        ggplot2::ggplot(plot_data, ggplot2::aes(x = tau, y = estimate)) +
          ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
          ggplot2::geom_ribbon(
            ggplot2::aes(ymin = conf.low, ymax = conf.high),
            alpha = 0.15, fill = "steelblue"
          ) +
          ggplot2::geom_line(colour = "steelblue", linewidth = 0.9) +
          ggplot2::geom_point(colour = "steelblue", size = 2) +
          ggplot2::facet_wrap(~ term_label, scales = "free_y") +
          ggplot2::scale_x_continuous(breaks = taus, labels = scales::percent_format(1)) +
          ggplot2::labs(
            title = paste("Effect of", pred_lab, "across the welfare distribution"),
            x     = "Welfare quantile",
            y     = "UQR coefficient"
          ) +
          ggplot2::theme_bw(base_size = 14) +
          ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 11))
      }
    }, error = function(e) blank_plot(paste0("RIF effect plot error: ", conditionMessage(e)))))
  }

  pred_lab <- label_fun(pred_var)
  pred_x_lab <- paste0(pred_var, " (", pred_lab, ")")
  y_var_name <- tryCatch(
    as.character(stats::formula(fit)[[2]]),
    error = function(e) "outcome"
  )
  y_lab <- label_fun(y_var_name)

  mf <- tryCatch(
    as.data.frame(stats::model.matrix(fit)),
    error = function(e) tryCatch(stats::model.frame(fit), error = function(e2) NULL)
  )

  # --- Resolve pred columns (exact or binned prefix match) ------------------
  pred_esc <- gsub("([\\[\\]\\(\\)\\^\\$\\.\\*\\+\\?])", "\\\\\\1", pred_var)

  if (!is.null(mf) && pred_var %in% names(mf) && !is_binned) {
    # Continuous: exact column exists
    pred_cols     <- pred_var
  } else if (!is.null(mf) && is_binned) {
    # Binned: match columns containing pred_var[
    pred_cols <- grep(paste0("^", pred_esc, "[\\[\\(]"), names(mf), value = TRUE)
  } else {
    pred_cols     <- character(0)
  }

  if (!length(pred_cols))
    return(blank_plot(paste0("'", pred_cols, "' not found in model frame.")))

  # ========================================================================= #
  # BINNED PATH                                                               #
  # ========================================================================= #
  if (is_binned) {
    p <- tryCatch({
      if (!requireNamespace("fixest", quietly = TRUE))
        return(blank_plot("Package 'fixest' is required."))

      mm <- as.data.frame(stats::model.matrix(fit))
      ct <- as.data.frame(fixest::coeftable(fit, se = "hetero"))
      ct$term <- rownames(ct)

      bin_cols <- grep(paste0("^", pred_esc, "[\\[\\(]"), names(mm), value = TRUE)
      bin_cols <- bin_cols[!grepl(":", bin_cols)]
      if (length(bin_cols) == 0) return(blank_plot("No binned columns found in model matrix."))

      ct_main <- ct[
        grepl(paste0("^", pred_esc, "[\\[\\(]"), ct$term) & !grepl(":", ct$term),
        c("term", "Estimate", "Std. Error"),
        drop = FALSE
      ]

      bins_df <- data.frame(term = sort(bin_cols), stringsAsFactors = FALSE)
      bins_df <- dplyr::left_join(bins_df, ct_main, by = "term")
      bins_df$Estimate[is.na(bins_df$Estimate)] <- 0
      bins_df$`Std. Error`[is.na(bins_df$`Std. Error`)] <- 0
      bins_df$bin_index <- seq_len(nrow(bins_df))
      bins_df$bin_label <- bins_df$term

      # Omitted note from first bin label in configured weather data
      omitted_note <- NULL
      if (!is.null(weather_df)) {
        first_bin <- get_first_bin_label(weather_df, pred_var)
        if (!is.na(first_bin) && nzchar(first_bin)) {
          omitted_note <- paste0("Omitted reference bin: ", first_bin, " at y = 0.")
        }
      }

      # Rebuild full table with all bins; missing coefficient => omitted reference (0 effect)
      bins_df <- data.frame(term = sort(bin_cols), stringsAsFactors = FALSE)
      bins_df <- dplyr::left_join(bins_df, ct_main, by = "term")
      bins_df$Estimate[is.na(bins_df$Estimate)] <- 0
      bins_df$`Std. Error`[is.na(bins_df$`Std. Error`)] <- 0
      bins_df$bin_index <- seq_len(nrow(bins_df))
      bins_df$bin_label <- bins_df$term

      # Detect moderator (if any)
      modx_var <- NULL
      modx_lab <- NULL
      if (length(interaction_terms) > 0) {
        mt <- interaction_terms[grepl(pred_var, interaction_terms, fixed = TRUE)]
        if (length(mt) > 0) {
          parts <- strsplit(mt[1], ":")[[1]]
          modx_var <- parts[parts != pred_var][1]
          if (!is.na(modx_var) && nzchar(modx_var)) modx_lab <- label_fun(modx_var)
        }
      }

      # No moderator: single line
      if (is.null(modx_var)) {
        bins_df$conf.low  <- bins_df$Estimate - 1.96 * bins_df$`Std. Error`
        bins_df$conf.high <- bins_df$Estimate + 1.96 * bins_df$`Std. Error`

        return(
          ggplot2::ggplot(
            bins_df,
            ggplot2::aes(x = bin_index, y = Estimate, ymin = conf.low, ymax = conf.high)
          ) +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
            ggplot2::geom_pointrange(colour = "steelblue", size = 0.65) +
            ggplot2::geom_line(ggplot2::aes(group = 1), colour = "steelblue", linewidth = 0.6) +
            ggplot2::scale_x_continuous(breaks = bins_df$bin_index, labels = bins_df$bin_label) +
            ggplot2::labs(
              title = paste("Effect of", pred_lab, "bins on", y_lab),
              x = pred_x_lab,
              y = paste("Effect on", y_lab),
              caption = omitted_note
            ) +
            ggplot2::theme_minimal(base_size = 14) +
            ggplot2::theme(
              plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 11),
              plot.caption = ggplot2::element_text(hjust = 0, size = 9, colour = "grey40"),
              axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
            )
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
      int_se  <- stats::setNames(ct_int$`Std. Error`, ct_int$term)

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
      plot_df$modx <- factor(plot_df$modx)

      ggplot2::ggplot(
        plot_df,
        ggplot2::aes(
          x = bin_index, y = est, ymin = conf.low, ymax = conf.high,
          colour = modx, group = modx
        )
      ) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
        ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 0.2), size = 0.5) +
        ggplot2::geom_line(position = ggplot2::position_dodge(width = 0.2), linewidth = 0.6) +
        ggplot2::scale_colour_brewer(palette = "Set1", name = modx_lab) +
        ggplot2::scale_x_continuous(breaks = bins_df$bin_index, labels = bins_df$bin_label) +
        ggplot2::labs(
          title = paste("Effect of", pred_lab, "bins by", modx_lab),
          x = pred_x_lab,
          y = paste("Effect on", y_lab),
          caption = omitted_note
        ) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 11),
          plot.caption = ggplot2::element_text(hjust = 0, size = 9, colour = "grey40"),
          legend.position = "bottom",
          axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
        )

    }, error = function(e) blank_plot(paste0("Binned effect plot error: ", conditionMessage(e))))

    return(p)
  }

  # ========================================================================= #
  # CONTINUOUS PATH                                                            #
  # ========================================================================= #
  if(!is_binned) {
    pred_vals <- mf[[pred_var]]

    if (!any(is.finite(pred_vals)))
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

      x_seq <- seq(min(mm[[pred_var]], na.rm = TRUE),
                  max(mm[[pred_var]], na.rm = TRUE),
                  length.out = n_grid)

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
            x     = pred_x_lab,
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
            x     = pred_x_lab,
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
    return(p)
  }
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
                          engine            = "fixest",
                          is_logistic       = FALSE,
                          rif_grid          = NULL) {

  # --- RIF branch: quantile coefficient table --------------------------------
  if (identical(engine, "rif") && !is.null(rif_grid)) {
    return(tryCatch({
      grid3 <- rif_grid[rif_grid$model == 3L, ]
      taus  <- sort(unique(grid3$tau))
      terms <- unique(grid3$term)

      # Build pivot: rows = terms, columns = quantiles
      pv_fn <- function(grid_row) {
        est <- formatC(grid_row$estimate, format = "f", digits = 3)
        pv  <- grid_row$p.value
        stars <- ifelse(pv < 0.001, "***",
                 ifelse(pv < 0.01,  "**",
                 ifelse(pv < 0.05,  "*",
                 ifelse(pv < 0.1,   "\u2020", ""))))
        se <- formatC(grid_row$std.error, format = "f", digits = 3)
        list(est = paste0(est, stars), se = paste0("(", se, ")"))
      }

      # CSS
      css <- "
        .rif-table { border-collapse:collapse; font-family:'Times New Roman',Times,serif; font-size:13px; margin:20px 0; }
        .rif-table th, .rif-table td { padding:2px 10px; text-align:center; }
        .rif-table th { font-weight:normal; border-bottom:1px solid #000; }
        .rif-table .topline { border-top:2px solid #000; }
        .rif-table .var-name { text-align:left; font-style:italic; }
        .rif-table .se-row td { color:#555; }
        .rif-table .stat-label { text-align:left; }
      "

      tau_labels <- paste0("\u03C4=", formatC(taus, format = "f", digits = 1))
      header <- paste0(
        "<tr class='topline'>",
        "<th style='text-align:left; border-top:2px solid #000; border-bottom:1px solid #000;'></th>",
        paste(sprintf("<th style='border-top:2px solid #000; border-bottom:1px solid #000;'>%s</th>", tau_labels), collapse = ""),
        "</tr>"
      )

      body_rows <- ""
      for (v in terms) {
        est_cells <- ""
        se_cells  <- ""
        for (tau in taus) {
          row <- grid3[grid3$term == v & grid3$tau == tau, ]
          if (nrow(row) == 1) {
            pv <- pv_fn(row)
            est_cells <- paste0(est_cells, "<td>", pv$est, "</td>")
            se_cells  <- paste0(se_cells,  "<td>", pv$se,  "</td>")
          } else {
            est_cells <- paste0(est_cells, "<td></td>")
            se_cells  <- paste0(se_cells,  "<td></td>")
          }
        }
        body_rows <- paste0(body_rows,
          "<tr><td class='var-name'>", htmltools::htmlEscape(v), "</td>", est_cells, "</tr>",
          "<tr class='se-row'><td></td>", se_cells, "</tr>"
        )
      }

      # Per-quantile fit stats from model 3 (fit3 is fixest_multi)
      stats_rows <- ""
      if (inherits(fit3, "fixest_multi") || is.list(fit3)) {
        nobs_vals <- vapply(seq_along(taus), function(i) {
          tryCatch(formatC(stats::nobs(fit3[[i]]), format = "d", big.mark = ","),
                   error = function(e) "")
        }, character(1))
        r2_vals <- vapply(seq_along(taus), function(i) {
          tryCatch(formatC(fixest::r2(fit3[[i]], "wr2"), format = "f", digits = 3),
                   error = function(e) tryCatch(formatC(fixest::r2(fit3[[i]], "r2"), format = "f", digits = 3),
                                                error = function(e2) ""))
        }, character(1))

        stats_rows <- paste0(
          "<tr><td style='border-top:1px solid #000;'></td>",
          paste(rep("<td style='border-top:1px solid #000;'></td>", length(taus)), collapse = ""),
          "</tr>",
          "<tr><td class='stat-label'>Observations</td>",
          paste(sprintf("<td>%s</td>", nobs_vals), collapse = ""),
          "</tr>",
          "<tr><td class='stat-label'>Within R\u00B2</td>",
          paste(sprintf("<td>%s</td>", r2_vals), collapse = ""),
          "</tr>",
          "<tr><td style='border-bottom:2px solid #000;'></td>",
          paste(rep("<td style='border-bottom:2px solid #000;'></td>", length(taus)), collapse = ""),
          "</tr>"
        )
      }

      note <- paste0("<tr><td colspan='", length(taus) + 1,
                     "' style='text-align:left; font-size:11px; padding-top:6px; color:#555;'>",
                     "Full specification (FE + controls). ",
                     "\u2020 p&lt;0.1, * p&lt;0.05, ** p&lt;0.01, *** p&lt;0.001</td></tr>")

      html <- paste0(
        "<style>", css, "</style>",
        "<table class='rif-table'>",
        "<thead>", header, "</thead>",
        "<tbody>", body_rows, stats_rows, note, "</tbody>",
        "</table>"
      )

      htmltools::HTML(html)
    }, error = function(e) htmltools::tags$p(paste("RIF table error:", conditionMessage(e)))))
  }

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
    lab <- v
    v1 <- lookup(c1, v)
    v2 <- lookup(c2, v)
    v3 <- lookup(c3, v)

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
  safe_nobs <- function(fit) tryCatch(formatC(stats::nobs(fit), format = "d", big.mark = ","), error = function(e) "")
  safe_r2   <- function(fit) tryCatch(formatC(fixest::r2(fit, "r2"),  format = "f", digits = 3), error = function(e) "")
  safe_wr2  <- function(fit) tryCatch(formatC(fixest::r2(fit, "wr2"), format = "f", digits = 3), error = function(e) "")
  safe_pr2  <- function(fit) tryCatch(formatC(fixest::r2(fit, "pr2"), format = "f", digits = 3), error = function(e) "")
  safe_aic  <- function(fit) tryCatch(formatC(stats::AIC(fit), format = "f", digits = 0, big.mark = ","), error = function(e) "")
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

  stats_core <- if (is_logistic) {
    paste0(
      stat_row("Observations", safe_nobs),
      stat_row("McFadden R\u00B2", safe_pr2),
      stat_row("AIC", safe_aic),
      stat_row("Fixed effects", safe_fe)
    )
  } else {
    paste0(
      stat_row("Observations", safe_nobs),
      stat_row("R\u00B2", safe_r2),
      stat_row("Within R\u00B2", safe_wr2),
      stat_row("Fixed effects", safe_fe)
    )
  }

  stats <- paste0(
    "<tr class='botline'><td style='border-top:1px solid #000;'></td>",
    "<td style='border-top:1px solid #000;'></td>",
    "<td style='border-top:1px solid #000;'></td>",
    "<td style='border-top:1px solid #000;'></td></tr>",
    stats_core,
    "<tr><td style='border-bottom:2px solid #000;'></td>",
    "<td style='border-bottom:2px solid #000;'></td>",
    "<td style='border-bottom:2px solid #000;'></td>",
    "<td style='border-bottom:2px solid #000;'></td></tr>"
  )

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
#' For continuous predictors, scatters raw residuals against `haz_var` with a
#' horizontal zero-line and binned mean overlay. For categorical/binned
#' predictors, shows jittered residuals plus category means.
#' Returns `NULL` invisibly when `haz_var` is absent from the model frame.
#'
#' @param model   A native `lm`/`glm`/`fixest` object.
#' @param haz_var Scalar character name of the weather predictor.
#' @param x_label Scalar character x-axis label.
#'
#' @return A `ggplot` object, or `NULL` invisibly.
#'
#' @export
plot_resid_weather <- function(model, haz_var, weather_df, x_label = haz_var) {
  df <- tryCatch(stats::model.frame(model), error = function(e) NULL)

  if (is.null(df) || !haz_var %in% names(df)) {
    mm <- tryCatch(as.data.frame(stats::model.matrix(model)), error = function(e) NULL)
    if (is.null(mm)) return(invisible(NULL))

    if (haz_var %in% names(mm)) {
      df <- mm
    } else {
      # Local binned match: haz_var[...], haz_var(...)
      haz_esc  <- gsub("([\\[\\]\\(\\)\\^\\$\\.\\*\\+\\?])", "\\\\\\1", haz_var)
      bin_cols <- grep(paste0("^", haz_esc, "[\\[\\(]"), names(mm), value = TRUE)
      bin_cols <- bin_cols[!grepl(":", bin_cols)]
      if (length(bin_cols) == 0) return(invisible(NULL))

      Xb <- mm[, bin_cols, drop = FALSE]
      idx <- max.col(as.matrix(Xb), ties.method = "first")
      none_active <- rowSums(Xb != 0, na.rm = TRUE) == 0

      x_from_bins <- bin_cols[idx]
      x_from_bins[none_active] <- get_first_bin_label(weather_df, haz_var)

      df <- data.frame(.haz_x = x_from_bins, stringsAsFactors = FALSE)
      haz_var <- ".haz_x"
    }
  }

  res <- tryCatch(stats::residuals(model), error = function(e) NULL)
  if (is.null(res)) return(invisible(NULL))

  x_vals <- df[[haz_var]]
  n <- min(length(x_vals), length(res))
  x_vals <- x_vals[seq_len(n)]
  res    <- res[seq_len(n)]

  is_binned <- is.factor(x_vals) || is.character(x_vals)

  if (is_binned) {
    plot_data <- data.frame(
      x = as.factor(x_vals),
      residuals = res,
      stringsAsFactors = FALSE
    )

    ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$residuals)) +
      ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
      ggplot2::geom_jitter(width = 0.15, alpha = 0.12) +
      ggplot2::stat_summary(fun = mean, geom = "point", color = "orange", size = 2.5) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      ggplot2::labs(x = stringr::str_wrap(x_label, 40), y = "Residuals")
  } else {
    plot_data <- data.frame(x = as.numeric(x_vals), residuals = res)

    ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$residuals)) +
      ggplot2::geom_point(alpha = 0.1) +
      ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
      ggplot2::stat_summary_bin(fun = mean, bins = 20, color = "orange", size = 2, geom = "point") +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::labs(x = stringr::str_wrap(x_label, 40), y = "Residuals")
  }
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

  # RIF: per-quantile R² table
  if (identical(engine, "rif") && (inherits(model, "fixest_multi") || is.list(model))) {
    taus <- seq(0.1, 0.9, by = 0.1)
    n <- min(length(model), length(taus))
    rows <- lapply(seq_len(n), function(i) {
      m <- model[[i]]
      data.frame(
        Statistic = paste0("\u03C4 = ", formatC(taus[i], format = "f", digits = 1)),
        Nobs = tryCatch(format(stats::nobs(m), big.mark = ","), error = function(e) ""),
        R2 = tryCatch(fmt_num(fixest::r2(m, "r2")), error = function(e) ""),
        `Within R2` = tryCatch(fmt_num(fixest::r2(m, "wr2")), error = function(e) ""),
        stringsAsFactors = FALSE, check.names = FALSE
      )
    })
    return(do.call(rbind, rows))
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


#' Plot standardized coefficient importance (linear models)
#'
#' Computes predictor importance as `abs(beta) * sd(X)` from the fitted model
#' matrix (excluding intercept), then renders a horizontal bar chart of the top
#' predictors.
#'
#' @param model A native fitted model object supporting `model.matrix()` and
#'   `coef()`.
#' @param var_info Optional data frame (unused; kept for interface compatibility).
#'
#' @return A `ggplot` object.
#'
#' @export
plot_relaimpo <- function(model, var_info = NULL) {
  mm    <- stats::model.matrix(model)
  coefs <- stats::coef(model)

  keep <- names(coefs) != "(Intercept)"
  beta <- coefs[keep]

  X <- mm[, names(beta), drop = FALSE]
  sd_x <- apply(X, 2, stats::sd, na.rm = TRUE)
  sd_x[is.na(sd_x)] <- 0

  importance_df <- data.frame(
    Variable   = names(beta),
    Importance = abs(as.numeric(beta)) * as.numeric(sd_x),
    stringsAsFactors = FALSE
  )

  importance_df$label <- importance_df$Variable

  importance_df <- importance_df |>
    dplyr::arrange(dplyr::desc(.data$Importance)) |>
    utils::head(30)

  ggplot2::ggplot(
    importance_df,
    ggplot2::aes(x = reorder(.data$label, .data$Importance), y = .data$Importance)
  ) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "",
      y = "Standardized coefficient importance"
    ) +
    ggplot2::theme_minimal(base_size = 14)
}