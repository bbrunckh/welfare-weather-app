#' Weighted summary table (long)
#'
#' Lightweight replacement for `sumtable::sumtable()` (sumtable is not on CRAN).
#' Computes weighted mean / weighted sd / min / max / N by group for numeric variables.
#'
#' @param df A data.frame.
#' @param vars Character vector of column names to summarise.
#' @param group Name of grouping column. Default: "countryyear".
#' @param weight Name of weight column. Default: "weight".
#'
#' @return A data.frame in long format with columns:
#'   `countryyear`, `variable`, `Mean`, `Std. Dev.`, `Min`, `Max`, `N`.
#'
#' @noRd
weighted_summary_long <- function(df, vars, group = "countryyear", weight = "weight") {
	if (!length(vars)) {
		return(data.frame())
	}
	if (!all(c(group, weight) %in% names(df))) {
		return(data.frame())
	}

	df <- df[, unique(c(group, weight, vars)), drop = FALSE]

	# keep only numeric vars that exist
	vars <- intersect(vars, names(df))
	vars <- vars[vapply(df[vars], is.numeric, logical(1))]
	if (!length(vars)) {
		return(data.frame())
	}

	by_vals <- split(df, df[[group]], drop = TRUE)

	one_group <- function(d) {
		gval <- d[[group]][1]
		w <- d[[weight]]
		out <- lapply(vars, function(v) {
			x <- d[[v]]
			ok <- is.finite(x) & is.finite(w) & !is.na(x) & !is.na(w) & (w > 0)
			x <- x[ok]
			ww <- w[ok]

			if (!length(x)) {
				return(data.frame(
					countryyear = gval,
					variable = v,
					Mean = NA_real_,
					`Std. Dev.` = NA_real_,
					Min = NA_real_,
					Max = NA_real_,
					N = 0
				))
			}

			mu <- sum(ww * x) / sum(ww)

			# weighted sd with Bessel-like correction (when possible)
			wsum <- sum(ww)
			w2sum <- sum(ww^2)
			denom <- wsum - (w2sum / wsum)
			sdw <- if (denom > 0) sqrt(sum(ww * (x - mu)^2) / denom) else NA_real_

			data.frame(
				countryyear = gval,
				variable = v,
				Mean = mu,
				`Std. Dev.` = sdw,
				Min = min(x),
				Max = max(x),
				N = sum(!is.na(x))
			)
		})
		do.call(rbind, out)
	}

	res <- do.call(rbind, lapply(by_vals, one_group))
	rownames(res) <- NULL
	res
}

#' Ridge distribution plot helper
#'
#' @param df A data.frame.
#' @param x_var Column name for the x-axis.
#' @param group_var Column name for the ridges (y-axis).
#' @param fill_var Column name for the fill aesthetic.
#' @param x_label Optional x-axis label.
#' @param wrap_width Optional integer to wrap x-axis label text.
#' @param log_transform Logical; if TRUE, applies log10 transformation to x-axis. Default FALSE.
#'
#' @return A ggplot object or NULL if inputs are invalid.
#'
#' @noRd
ridge_distribution_plot <- function(
    df,
    x_var,
    group_var = "countryyear",
    fill_var = "code",
    x_label = NULL,
    wrap_width = NULL,
    log_transform = FALSE
) {
    if (is.null(df) || !nrow(df)) return(NULL)
    if (!all(c(x_var, group_var, fill_var) %in% names(df))) return(NULL)

    df_plot <- df[is.finite(df[[x_var]]), , drop = FALSE]
    
    # For log transform, filter out non-positive values
    if (log_transform) {
        df_plot <- df_plot[df_plot[[x_var]] > 0, , drop = FALSE]
    }
    
    if (!nrow(df_plot)) return(NULL)

    label <- x_label
    if (!is.null(label) && !is.null(wrap_width)) {
        label <- stringr::str_wrap(label, wrap_width)
    }
    
    # Add log transform note to label if applicable
    if (log_transform && !is.null(label)) {
        label <- paste0(label, " (log scale)")
    }

    p <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(x = .data[[x_var]], y = .data[[group_var]], fill = .data[[fill_var]])
    ) +
        ggridges::geom_density_ridges(alpha = 0.7, scale = 2) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
            title = "",
            x = label %||% x_var,
            y = "", 
            fill = ""
        ) +
        ggplot2::theme(legend.position = "none")
    
    # Apply log10 scale to x-axis if requested
    if (log_transform) {
        p <- p + ggplot2::scale_x_log10(
            labels = scales::comma_format()
        )
    }
    
    p
}

#' Generate Marginal Effect / Interaction Plots with human-readable labels
#'
#' @param fit A native model object.
#' @param pred_var Character. The base name of the weather variable.
#' @param interaction_terms Character vector of interaction strings.
#' @param is_binned Logical.
#' @param label_fun The get_label function from your server environment.
#' @return A ggplot object.
make_weather_effect_plot <- function(fit, pred_var, interaction_terms = character(0), is_binned = FALSE, label_fun = identity) {
  
  # 1. Resolve Human-Readable Labels
  pred_lab <- label_fun(pred_var)
  
  # Extract Y variable name from the model formula and label it
  y_var_name <- as.character(stats::formula(fit)[[2]])
  y_lab <- label_fun(y_var_name)

  # 2. Look for moderator
  modx_var <- NULL
  modx_lab <- NULL
  if (length(interaction_terms) > 0) {
    match_term <- grep(paste0("^", pred_var, ":"), interaction_terms, value = TRUE)
    if (length(match_term) > 0) {
      modx_var <- strsplit(match_term[1], ":")[[1]][2]
      modx_lab <- label_fun(modx_var)
    }
  }

  # 3. Build Plot
  if (is.null(modx_var)) {
    p <- jtools::effect_plot(
      model = fit, 
      pred = !!rlang::sym(pred_var), 
      interval = TRUE,
      colors = "blue",
      main.title = paste("Predicted", y_lab, "vs", pred_lab)
    )
    
  } else {
    if (is_binned) {
      p <- interactions::cat_plot(
        model = fit, 
        pred = !!rlang::sym(pred_var), 
        modx = !!rlang::sym(modx_var), 
        geom = "line", 
        interval = TRUE,
        modx.labels = NULL, # Let it use levels from data, but label the legend later
        main.title = paste("Impact of", pred_lab, "by", modx_lab)
      )
    } else {
      p <- interactions::interact_plot(
        model = fit, 
        pred = !!rlang::sym(pred_var), 
        modx = !!rlang::sym(modx_var), 
        interval = TRUE,
        main.title = paste("Impact of", pred_lab, "by", modx_lab)
      )
    }
  }

  # 4. Final Theme and Label Overrides
  p <- p + 
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = pred_lab, 
      y = paste("Predicted", y_lab),
      colour = modx_lab, # Updates legend title for interactions
      fill = modx_lab    # Updates legend title for confidence ribbons
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 11),
      legend.position = "bottom"
    )
  
  return(p)
}