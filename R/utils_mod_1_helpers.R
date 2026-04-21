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
					unweighted_mean = NA_real_,
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
				unweighted_mean = mean(x, na.rm = T),
				Mean = mu,
				`Std. Dev.` = sdw,
				Min = if (length(x) && any(is.finite(x))) min(x[is.finite(x)]) else NA_real_,
        Max = if (length(x) && any(is.finite(x))) max(x[is.finite(x)]) else NA_real_,
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

