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
