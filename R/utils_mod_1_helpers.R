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

  na_nan <- function(x) {
    x[is.nan(x)] <- NA_real_
    x
  }

  # All six statistics are G x V matrices (rows = countryyear in group
  # order, columns = vars); as.vector() runs down their columns, giving
  # variable-major order that matches the label repeats below.
  res <- data.frame(
    countryyear = as.character(rep(g$groups[[1]], times = length(vars))),
    variable = rep(vars, each = n_g),
    unweighted_mean = na_nan(as.vector(collapse::fmean(X, g = g, na.rm = TRUE))),
    Mean = na_nan(as.vector(collapse::fmean(X, g = g, w = w, na.rm = TRUE))),
    `Std. Dev.` = na_nan(as.vector(collapse::fsd(X, g = g, w = w, na.rm = TRUE))),
    Min = na_nan(as.vector(collapse::fmin(X, g = g, na.rm = TRUE))),
    Max = na_nan(as.vector(collapse::fmax(X, g = g, na.rm = TRUE))),
    N = as.integer(as.vector(collapse::fnobs(X, g = g))),
    check.names = FALSE,
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
    variable = rep(vars, each = g$N.groups),
    `% Missing` = 100 * as.vector(miss),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

#' Aggregate values for a ridge distribution plot
#'
#' Reduces raw observations to a common histogram before smoothing. This keeps
#' the expensive density calculation proportional to the number of bins and
#' survey waves rather than the number of households. The result is intended
#' for visualisation, not for numerical density estimation.
#'
#' @param df A data.frame.
#' @param x_var Column name for the x-axis.
#' @param group_var Column identifying independent density curves.
#' @param fill_var Column name for the fill aesthetic. `NULL` uses `group_var`.
#' @param weight_var Optional column of positive observation weights.
#' @param ridge_var Optional column identifying the shared y-axis ridge.
#' @param n_bins Number of histogram bins used before smoothing.
#' @param n_grid Number of points in each smoothed ridge.
#' @param log_transform Logical; if TRUE, aggregate and smooth in log10-space.
#' @param x_range Optional two-value range in transformed display space.
#' @param bandwidth Optional bandwidth in transformed display space.
#' @param bandwidth_scale Multiplier for the automatically estimated bandwidth.
#'
#' @return A list with `data`, `groups`, `bandwidth`, `log_transform`, and
#'   `x_range`, or NULL if inputs are invalid.
#'
#' @noRd
build_ridge_distribution_data <- function(
  df,
  x_var,
  group_var = "countryyear",
  fill_var = "code",
  weight_var = NULL,
  ridge_var = NULL,
  n_bins = 256L,
  n_grid = 256L,
  log_transform = FALSE,
  x_range = NULL,
  bandwidth = NULL,
  bandwidth_scale = 0.85
) {
  if (is.null(df) || !nrow(df)) {
    return(NULL)
  }
  if (!all(c(x_var, group_var) %in% names(df))) {
    return(NULL)
  }
  if (!is.null(fill_var) && !fill_var %in% names(df)) {
    return(NULL)
  }
  if (!is.null(weight_var) && !weight_var %in% names(df)) {
    return(NULL)
  }
  if (is.null(ridge_var)) ridge_var <- group_var
  if (!ridge_var %in% names(df)) {
    return(NULL)
  }

  x <- suppressWarnings(as.numeric(df[[x_var]]))
  group <- as.character(df[[group_var]])
  ridge <- as.character(df[[ridge_var]])
  fill <- if (is.null(fill_var)) group else as.character(df[[fill_var]])
  weight <- if (is.null(weight_var)) {
    rep(1, length(x))
  } else {
    suppressWarnings(as.numeric(df[[weight_var]]))
  }
  keep <- is.finite(x) & !is.na(group) & nzchar(group) &
    !is.na(ridge) & nzchar(ridge) & !is.na(fill) & is.finite(weight) &
    weight > 0
  if (log_transform) keep <- keep & x > 0
  if (!any(keep)) {
    return(NULL)
  }

  x <- x[keep]
  group <- group[keep]
  ridge <- ridge[keep]
  fill <- fill[keep]
  weight <- weight[keep]
  x_work <- if (log_transform) log10(x) else x

  n_bins <- max(32L, min(as.integer(n_bins), 512L))
  n_grid <- max(64L, min(as.integer(n_grid), 512L))
  x_rng <- if (is.null(x_range)) {
    range(x_work, finite = TRUE)
  } else {
    suppressWarnings(as.numeric(x_range[seq_len(min(2L, length(x_range)))]))
  }
  if (length(x_rng) != 2L || any(!is.finite(x_rng))) {
    return(NULL)
  }
  x_rng <- sort(x_rng)
  x_work <- pmin(pmax(x_work, x_rng[1L]), x_rng[2L])
  if (diff(x_rng) == 0) {
    pad <- max(abs(x_rng[1]) * 0.01, 0.5)
    x_rng <- x_rng + c(-pad, pad)
  }

  breaks <- seq(x_rng[1], x_rng[2], length.out = n_bins + 1L)
  bin <- findInterval(
    x_work, breaks,
    rightmost.closed = TRUE, all.inside = TRUE
  )
  centres <- (breaks[-length(breaks)] + breaks[-1L]) / 2

  # Collapse performs the only raw-row grouping pass. Every subsequent
  # operation works on at most n_bins rows per ridge.
  hist <- data.frame(
    group = group,
    ridge = ridge,
    fill = fill,
    bin = bin,
    .weight = weight,
    stringsAsFactors = FALSE
  )
  hist_groups <- collapse::GRP(hist, by = c("group", "ridge", "fill", "bin"))
  hist_counts <- collapse::fsum(
    hist$.weight,
    g = hist_groups, na.rm = TRUE
  )
  hist <- hist_groups$groups
  hist$n <- as.numeric(hist_counts)

  # Scott/Silverman-style bandwidth estimated from the binned moments and
  # approximate quartiles. This avoids bw.nrd0() scanning/sorting millions
  # of household values while remaining visually close to the raw KDE.
  w <- as.numeric(hist$n)
  h_x <- centres[hist$bin]
  n_obs <- sum(w)
  mu <- sum(h_x * w) / n_obs
  variance <- sum((h_x - mu)^2 * w) / max(n_obs - 1, 1)
  sd_x <- sqrt(max(variance, 0))
  # Histogram rows are not guaranteed to be sorted by bin. Compute
  # approximate quantiles from sorted bins while retaining grouped counts.
  ord <- order(h_x)
  c_sorted <- cumsum(w[ord])
  hist_quantile <- function(prob) {
    target <- prob * n_obs
    idx <- match(TRUE, c_sorted >= target)
    if (is.na(idx) || idx == 1L) {
      return(h_x[ord][1L])
    }
    x_sorted <- h_x[ord]
    prev <- c_sorted[idx - 1L]
    span <- max(c_sorted[idx] - prev, 1)
    x_sorted[idx - 1L] + (x_sorted[idx] - x_sorted[idx - 1L]) *
      (target - prev) / span
  }
  iqr_x <- hist_quantile(0.75) - hist_quantile(0.25)
  scale_x <- min(sd_x, iqr_x / 1.34)
  bin_width <- diff(breaks)[1L]
  if (is.null(bandwidth)) {
    # Estimate each series independently using effective sample size. Raw
    # survey-weight totals can be much larger than the information in the
    # weighted sample when weights are unequal. A common median bandwidth
    # keeps the waves visually comparable while still adapting to their
    # different sizes and weight distributions.
    series_bandwidth <- vapply(sort(unique(hist$group)), function(g) {
      take_g <- hist$group == g
      wg <- w[take_g]
      xg <- h_x[take_g]
      n_eff <- sum(wg)^2 / sum(wg^2)
      sd_g <- sqrt(sum((xg - sum(xg * wg) / sum(wg))^2 * wg) /
        max(sum(wg) - 1, 1))
      ord_g <- order(xg)
      cum_g <- cumsum(wg[ord_g])
      quantile_g <- function(prob) {
        target <- prob * sum(wg)
        idx <- match(TRUE, cum_g >= target)
        if (is.na(idx) || idx == 1L) {
          return(xg[ord_g][1L])
        }
        x_sorted <- xg[ord_g]
        prev <- cum_g[idx - 1L]
        span <- max(cum_g[idx] - prev, 1)
        x_sorted[idx - 1L] + (x_sorted[idx] - x_sorted[idx - 1L]) *
          (target - prev) / span
      }
      iqr_g <- quantile_g(0.75) - quantile_g(0.25)
      scale_g <- min(sd_g, iqr_g / 1.34)
      0.9 * scale_g * n_eff^(-0.2)
    }, numeric(1))
    series_bandwidth <- series_bandwidth[is.finite(series_bandwidth) &
      series_bandwidth > 0]
    bandwidth <- if (length(series_bandwidth)) {
      stats::median(series_bandwidth) * bandwidth_scale
    } else {
      0
    }
  } else {
    bandwidth <- suppressWarnings(as.numeric(bandwidth[1L]))
  }
  if (!is.finite(bandwidth) || bandwidth <= 0) {
    bandwidth <- max(bin_width * 1.5, .Machine$double.eps^0.5)
  }

  group_levels <- sort(unique(hist$group))
  ridge_levels <- sort(unique(hist$ridge))
  rows <- lapply(seq_along(group_levels), function(i) {
    g <- group_levels[i]
    take <- hist$group == g
    gx <- centres[hist$bin[take]]
    gw <- w[take]
    gx_ord <- order(gx)
    gx <- gx[gx_ord]
    gw <- gw[gx_ord]
    grid <- seq(x_rng[1], x_rng[2], length.out = n_grid)

    dens <- if (length(gx) >= 2L) {
      tryCatch(
        stats::density(
          gx,
          weights = gw / sum(gw), bw = bandwidth,
          n = n_grid, from = x_rng[1], to = x_rng[2], cut = 0,
          warnWbw = FALSE
        )$y,
        error = function(e) rep(0, n_grid)
      )
    } else {
      stats::dnorm(grid, mean = gx[1L], sd = bandwidth)
    }
    dens[!is.finite(dens)] <- 0
    peak <- max(dens)
    height <- if (peak > 0) dens / peak else rep(0, n_grid)
    fill_i <- hist$fill[which(take)[1L]]
    ridge_i <- hist$ridge[which(take)[1L]]
    data.frame(
      x = if (log_transform) 10^grid else grid,
      y = match(ridge_i, ridge_levels),
      height = height,
      group = g,
      fill = fill_i,
      ridge = ridge_i,
      stringsAsFactors = FALSE
    )
  })

  list(
    data = do.call(rbind, rows),
    groups = group_levels,
    ridges = ridge_levels,
    bandwidth = bandwidth,
    log_transform = isTRUE(log_transform),
    x_range = if (log_transform) 10^x_rng else x_rng,
    quantile_range = if (log_transform) {
      10^c(hist_quantile(0.01), hist_quantile(0.99))
    } else {
      c(hist_quantile(0.01), hist_quantile(0.99))
    }
  )
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
  log_transform = FALSE,
  group_labels = NULL
) {
  agg <- build_ridge_distribution_data(
    df,
    x_var = x_var,
    group_var = group_var,
    fill_var = fill_var,
    log_transform = log_transform
  )
  if (is.null(agg)) {
    return(NULL)
  }

  label <- x_label
  if (!is.null(label) && !is.null(wrap_width)) {
    label <- stringr::str_wrap(label, wrap_width)
  }

  # Add log transform note to label if applicable
  if (log_transform && !is.null(label)) {
    label <- paste0(label, " (log scale)")
  }

  display_groups <- agg$groups
  if (!is.null(group_labels)) {
    mapped <- unname(group_labels[display_groups])
    keep <- !is.na(mapped) & nzchar(mapped)
    display_groups[keep] <- mapped[keep]
  }

  p <- ggplot2::ggplot(
    agg$data,
    ggplot2::aes(
      x = .data$x, y = .data$y,
      group = .data$group, fill = .data$fill
    )
  ) +
    ridge_geometry_layers(scale = 2, alpha = 0.7, linewidth = 0.3) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(agg$groups),
      labels = display_groups,
      expand = ggplot2::expansion(mult = c(0.02, 0.12))
    ) +
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


#' Native ggplot2 layers for precomputed ridgeline data
#'
#' A ribbon plus its upper outline reproduces the visual ridge from
#' `build_ridge_distribution_data()` without a specialised geometry package.
#'
#' @param scale Numeric height multiplier.
#' @param alpha Ribbon transparency.
#' @param linewidth Upper outline width.
#' @param colour Outline colour. Set to `NULL` to inherit a mapped colour.
#'
#' @return A list of ggplot2 layers.
#' @noRd
ridge_geometry_layers <- function(scale = 1, alpha = 0.7, linewidth = 0.3,
                                  colour = "black") {
  ribbon <- ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = .data$y,
      ymax = .data$y + .data$height * scale
    ),
    alpha = alpha,
    colour = NA
  )
  line_mapping <- ggplot2::aes(y = .data$y + .data$height * scale)
  line <- if (is.null(colour)) {
    ggplot2::geom_line(line_mapping, linewidth = linewidth)
  } else {
    ggplot2::geom_line(
      line_mapping,
      linewidth = linewidth, colour = colour
    )
  }
  list(ribbon, line)
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
  if (is.null(x)) {
    return(character(0))
  }
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
  sm <- reactive({
    req(selected_model())
    selected_model()
  })

  individual <- reactive(model_covariate_names(sm()$individual_covariates))
  hh <- reactive(model_covariate_names(sm()$hh_covariates))
  firm <- reactive(model_covariate_names(sm()$firm_covariates))
  area <- reactive(model_covariate_names(sm()$area_covariates))
  interactions <- reactive(model_covariate_names(sm()$interactions))
  all <- reactive({
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
  if (is.null(sm)) {
    return(NULL)
  }
  unique(c(
    model_covariate_names(sm$individual_covariates),
    model_covariate_names(sm$hh_covariates),
    model_covariate_names(sm$firm_covariates),
    model_covariate_names(sm$area_covariates),
    model_covariate_names(sm$interactions)
  ))
}
