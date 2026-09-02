#' Weighted summary table (long)
#'
#' Lightweight replacement for `sumtable::sumtable()` (sumtable is not on CRAN).
#' Computes weighted mean / weighted sd / min / max / N by group for numeric variables.
#'
#' The aggregation is a single set of grouped `collapse` matrix passes
#' (PERF-33): the countryyear grouping is built once for the whole frame, the
#' per-variable validity mask (`is.finite(x) & is.finite(w) & w > 0`) is folded
#' into the numeric matrix, and every statistic is one C-level grouped pass
#' over all variables. The weighted SD uses `collapse::fsd(w=)`, whose
#' denominator is $\sum w - 1$ rather than the reliability-weight form
#' $\sum w - \sum w^2 / \sum w$ previously hand-rolled here.
#'
#' @param df A data.frame.
#' @param vars Character vector of column names to summarise.
#' @param group Name of grouping column. Default: "countryyear".
#' @param weight Name of weight column. Default: "weight".
#'
#' @return A data.frame in long format with columns:
#'   `countryyear`, `variable`, `unweighted_mean`, `Mean`, `Std. Dev.`,
#'   `Min`, `Max`, `N`.
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

	# `split()` dropped rows with missing group keys, so they contribute no
	# rows here either.
	df <- df[!is.na(df[[group]]), , drop = FALSE]
	n <- nrow(df)
	if (!n) {
		return(data.frame())
	}

	# One grouping over the survey frame, shared by every variable (PERF-33).
	g <- collapse::GRP(df, by = group)
	n_g <- g$N.groups

	# Numeric columns as one N x V matrix: grouped collapse passes summarise
	# every variable in a single shot (rows are grouped, columns summarised),
	# replacing the old full-frame split() + per-(group, variable) lapply.
	X <- as.matrix(df[vars])
	w <- as.numeric(df[[weight]])

	# Validity mask per the old per-cell rule: finite value and a finite,
	# positive weight. `w` recycles down each matrix column, so the weight
	# terms stay per-row.
	w_ok <- is.finite(w) & (w > 0)
	ok <- is.finite(X) & w_ok
	# Invalid cells are NA-ed out of value and weight so the grouped passes
	# skip them; a (countryyear, variable) whose cells are all masked comes
	# back all-NA with N = 0, like the old empty-group rows.
	X[!ok] <- NA_real_
	w[!w_ok] <- NA_real_

	na_nan <- function(x) { x[is.nan(x)] <- NA_real_; x }

	# All six statistics are G x V matrices (rows = countryyear in group
	# order, columns = vars); as.vector() runs down their columns, giving
	# variable-major order that matches the label repeats below.
	res <- data.frame(
		countryyear     = as.character(rep(g$groups[[1]], times = length(vars))),
		variable        = rep(vars, each = n_g),
		unweighted_mean = na_nan(as.vector(collapse::fmean(X, g = g, na.rm = TRUE))),
		Mean            = na_nan(as.vector(collapse::fmean(X, g = g, w = w, na.rm = TRUE))),
		`Std. Dev.`     = na_nan(as.vector(collapse::fsd(X, g = g, w = w, na.rm = TRUE))),
		Min             = na_nan(as.vector(collapse::fmin(X, g = g, na.rm = TRUE))),
		Max             = na_nan(as.vector(collapse::fmax(X, g = g, na.rm = TRUE))),
		N               = as.integer(as.vector(collapse::fnobs(X, g = g))),
		check.names     = FALSE,
		stringsAsFactors = FALSE
	)

	# `split()` ordered groups by locale-sorted countryyear while GRP() sorts
	# in C locale; the stable order() restores the old presentation order and
	# keeps the variable order within each countryyear.
	res <- res[order(res$countryyear), ]
	rownames(res) <- NULL
	res
}

#' Wave-specific missingness table (long)
#'
#' Computes `100 * mean(is.na(x))` for every variable in `vars` within each
#' `group` level in a single grouped pass (PERF-09), replacing the
#' per-variable `group_by() |> summarise()` loops in the Step 1 stats tables.
#' Rows with missing group keys are kept as their own group, matching the
#' `dplyr::group_by()` behaviour of the loops this replaces.
#'
#' @param df A data.frame.
#' @param vars Character vector of column names (any class; list columns are
#'   skipped).
#' @param group Name of grouping column. Default: "countryyear".
#'
#' @return A data.frame with columns `countryyear`, `variable`, `% Missing`.
#'
#' @noRd
survey_missingness_long <- function(df, vars, group = "countryyear") {
	vars <- intersect(vars, names(df))
	vars <- vars[vapply(df[vars], function(x) !is.list(x), logical(1))]
	if (!length(vars) || !group %in% names(df)) {
		return(data.frame(
			countryyear = character(), variable = character(), `% Missing` = numeric(),
			check.names = FALSE, stringsAsFactors = FALSE
		))
	}

	g <- collapse::GRP(df, by = group)
	miss <- collapse::fmean(is.na(df[vars]), g = g)

	# miss is (group x variable); as.vector() runs down its columns, giving
	# variable-major order that matches the label repeats below.
	out <- data.frame(
		countryyear = as.character(rep(g$groups[[1]], times = length(vars))),
		variable    = rep(vars, each = g$N.groups),
		`% Missing` = 100 * as.vector(miss),
		check.names = FALSE,
		stringsAsFactors = FALSE
	)
	rownames(out) <- NULL
	out
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

    # Pre-compute the bandwidth ggridges would otherwise pick (and announce
    # via `message()`). Passing it explicitly silences the chatty
    # "Picking joint bandwidth of ..." note without changing the visual.
    bw <- tryCatch(stats::bw.nrd0(df_plot[[x_var]]), error = function(e) NULL)
    if (is.null(bw) || !is.finite(bw) || bw <= 0) bw <- NULL

    p <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(x = .data[[x_var]], y = .data[[group_var]], fill = .data[[fill_var]])
    ) +
        ggridges::geom_density_ridges(alpha = 0.7, scale = 2, bandwidth = bw) +
        theme_wise() +
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

#' Extract covariate names from a model-spec entry
#'
#' Model-spec entries are either named (use the names; blank names are
#' dropped) or unnamed (use the values).
#'
#' @param x A model-spec entry (named/unnamed list or character vector), or
#'   NULL.
#'
#' @return Character vector of unique covariate names.
#'
#' @noRd
model_covariate_names <- function(x) {
	if (is.null(x)) return(character(0))
	nms <- names(x)
	if (!is.null(nms) && any(nzchar(nms))) {
		unique(nms[nzchar(nms)])
	} else {
		unique(as.character(unlist(x, use.names = FALSE)))
	}
}

#' Coefficient-name reactives for the selected Step 1 model
#'
#' Shared by the Step 3 lever modules (REACT-08): one definition of how a
#' selected-model list is decomposed into covariate roles.
#'
#' @param selected_model Reactive returning the selected-model list.
#'
#' @return Named list of reactives: `individual`, `hh`, `firm`, `area`,
#'   `interactions` (each a character vector of term names) and `all` (their
#'   union). Each stays silent-empty until `selected_model()` is populated.
#'
#' @noRd
model_coefficient_reactives <- function(selected_model) {
	sm <- reactive({ req(selected_model()); selected_model() })

	individual   <- reactive(model_covariate_names(sm()$individual_covariates))
	hh           <- reactive(model_covariate_names(sm()$hh_covariates))
	firm         <- reactive(model_covariate_names(sm()$firm_covariates))
	area         <- reactive(model_covariate_names(sm()$area_covariates))
	interactions <- reactive(model_covariate_names(sm()$interactions))
	all          <- reactive({
		unique(c(individual(), hh(), firm(), area(), interactions()))
	})

	list(
		individual   = individual,
		hh           = hh,
		firm         = firm,
		area         = area,
		interactions = interactions,
		all          = all
	)
}

#' Collect the variable / term names referenced by a selected model
#'
#' Union of all covariate roles plus interactions, mirroring the `coeffs()`
#' reactive of the policy lever modules. Used to gate which covariate levers
#' may mutate the survey in `apply_policy_to_svy()`. Returns NULL when `sm`
#' is NULL, which `apply_policy_to_svy()` treats as "no gating".
#'
#' @param sm Selected-model list (or NULL).
#' @return Character vector of term names, or NULL when `sm` is NULL.
#'
#' @noRd
model_term_names <- function(sm) {
	if (is.null(sm)) return(NULL)
	unique(c(
		model_covariate_names(sm$individual_covariates),
		model_covariate_names(sm$hh_covariates),
		model_covariate_names(sm$firm_covariates),
		model_covariate_names(sm$area_covariates),
		model_covariate_names(sm$interactions)
	))
}

