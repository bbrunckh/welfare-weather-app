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
