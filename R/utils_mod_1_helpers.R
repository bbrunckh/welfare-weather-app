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


# Shared echarts4r plumbing for the Step 1 module charts (guidelines §7) ----

# Base widget with a safe two-column, two-row dummy frame (echarts4r 0.5.x
# rejects single-column and <2-row frames in e_charts()); builders overwrite
# the axis/series/legend opts wholesale, so the dummy is never drawn.
.e_new <- function(height = "300px") {
  e <- echarts4r::e_charts(data.frame(x = 0:1, y = 0:1), x, height = height)
  e$x$opts$xAxis <- NULL
  e$x$opts$yAxis <- NULL
  e$x$opts$series <- NULL
  e
}

# Section-grid injection for n side-by-side panels in one widget (used where
# the ggplot version was a patchwork of separate panels).
.e_multi_grids <- function(e, n, titles = NULL, height = "300px") {
  pad <- 8
  title_h <- if (is.null(titles)) 0 else 24
  gap <- 4
  slot <- (100 - pad * (n + 1) - gap * (n - 1)) / n
  widths <- paste0(slot, "%")
  lefts <- paste0(pad + (pad + slot + gap) * (seq_len(n) - 1), "%")
  grids <- lapply(seq_len(n), function(i) {
    list(
      left = lefts[i], width = widths[i],
      top = if (title_h > 0) title_h + pad else pad,
      bottom = pad, containLabel = TRUE
    )
  })
  e$x$opts$grid <- grids
  e$x$opts$xAxis <- rep(e$x$opts$xAxis %||% list(), n)
  e$x$opts$yAxis <- rep(e$x$opts$yAxis %||% list(), n)
  if (!is.null(titles)) {
    e$x$opts$title <- lapply(seq_len(n), function(i) {
      list(
        text = titles[[i]], left = lefts[i], top = pad - 4,
        textStyle = list(
          color = .wise_charcoal, fontSize = 13, fontWeight = "normal"
        )
      )
    })
  }
  e
}

# Precomputed-ridge renderer shared by the outcome and weather distribution
# charts. `rd_data` is the `$data` frame of `build_ridge_distribution_data()`
# (columns x, y, height, group); `styles` maps each `group` key to its fill
# colour (NA = outline only), line colour and line dash. Native line series are
# used instead of custom renderItem polygons because ECharts' custom renderer
# can silently fall back to black fills in browser and export contexts.
ridge_echart_widget <- function(rd_data, ridge_levels, ridge_labels, styles,
                                height = "300px", log_scale = FALSE,
                                x_name = NULL, y_name = "",
                                tooltip_x_name = NULL,
                                hide_extreme_x = FALSE) {
  e <- .e_new(height)
  if (is.null(rd_data) || !nrow(rd_data) ||
    !all(c("x", "y", "height", "group") %in% names(rd_data))) {
    return(echart_blank("Distribution unavailable", height = height))
  }
  n_r <- length(ridge_levels)
  x_name <- x_name %||% ""

  colour_or_na <- function(x) {
    x <- as.character(x)[1L]
    if (is.na(x) || !nzchar(x)) return(NA_character_)
    if (grepl("^#[0-9A-Fa-f]{6}$", x)) return(x)
    rgb <- tryCatch(grDevices::col2rgb(x), error = function(e) NULL)
    if (is.null(rgb)) {
      return(NA_character_)
    }
    grDevices::rgb(rgb[[1L]], rgb[[2L]], rgb[[3L]], maxColorValue = 255)
  }
  valid_colour <- function(x, fallback) {
    out <- colour_or_na(x)
    if (is.na(out)) fallback else out
  }

  groups <- unique(rd_data$group)
  # Keep the wave-only labels on the y-axis, but distinguish sample and
  # historical curves in the tooltip so both statistics survive deduplication.
  tooltip_labels <- stats::setNames(vapply(groups, function(grp) {
    if (!grepl(" - ", grp, fixed = TRUE)) return(as.character(grp))
    wave <- sub(" - .*", "", grp)
    source <- sub("^.* - ", "", grp)
    ridge_i <- match(wave, as.character(ridge_levels))
    wave_label <- if (!is.na(ridge_i)) as.character(ridge_labels[ridge_i]) else wave
    paste0(wave_label, " - ", source)
  }, character(1)), as.character(groups))
  series <- lapply(groups, function(grp) {
    g <- rd_data[rd_data$group == grp, , drop = FALSE]
    g <- g[order(g$x), , drop = FALSE]
    if (!nrow(g)) {
      return(NULL)
    }
    st <- styles[match(grp, styles$group), , drop = FALSE]
    fill_value <- if (nrow(st)) as.character(st$fill[1L]) else NA_character_
    line_value <- if (nrow(st)) as.character(st$line[1L]) else NA_character_
    fill_col <- valid_colour(fill_value, "#0071BC")
    line_col <- valid_colour(line_value, "#002244")
    is_dashed <- nrow(st) && isTRUE(st$dashed[1L])
    has_fill <- nrow(st) && !is.na(colour_or_na(fill_value))

    y_base <- match(g$ridge[1L], ridge_levels)
    if (is.na(y_base)) y_base <- 1.0
    y_top <- y_base + 0.85 * g$height
    has_tooltip_value <- "tooltip_value" %in% names(g)
    tooltip_value <- if (has_tooltip_value) g$tooltip_value else NULL

    top_data <- lapply(seq_len(nrow(g)), function(i) {
      if (has_tooltip_value) {
        list(g$x[i], y_top[i], tooltip_value[i])
      } else {
        list(g$x[i], y_top[i])
      }
    })
    polygon <- if (has_tooltip_value) {
      rbind(
        cbind(g$x, y_top, tooltip_value),
        cbind(rev(g$x), rep(y_base, nrow(g)), rev(tooltip_value))
      )
    } else {
      rbind(
        cbind(g$x, y_top),
        cbind(rev(g$x), rep(y_base, nrow(g)))
      )
    }
    line_style <- list(
      color = line_col,
      width = 1.5,
      type = if (is_dashed) "dashed" else "solid"
    )

    if (has_fill) {
      list(
        type = "line",
        name = tooltip_labels[[as.character(grp)]],
        data = unname(polygon),
        symbol = "none",
        lineStyle = line_style,
        itemStyle = list(color = fill_col),
        areaStyle = list(color = fill_col, opacity = 0.65),
        emphasis = list(focus = "series"),
        z = 2
      )
    } else {
      list(
        type = "line",
        name = tooltip_labels[[as.character(grp)]],
        data = top_data,
        symbol = "none",
        lineStyle = line_style,
        itemStyle = list(color = line_col),
        areaStyle = list(color = "rgba(0,0,0,0)", opacity = 0),
        emphasis = list(focus = "series"),
        z = 3
      )
    }
  })
  series <- Filter(Negate(is.null), series)

  label_map <- as.list(stats::setNames(
    as.list(as.character(ridge_labels)),
    as.character(seq_along(ridge_labels))
  ))
  x_lo <- min(rd_data$x, na.rm = TRUE)
  x_hi <- max(rd_data$x, na.rm = TRUE)
  x_pad <- if (isTRUE(hide_extreme_x) && !isTRUE(log_scale) && x_hi > x_lo) {
    0.025 * (x_hi - x_lo)
  } else {
    0
  }

  e$x$opts$series <- series
  e$x$opts$xAxis <- list(
    type = if (isTRUE(log_scale)) "log" else "value",
    min = if (isTRUE(log_scale)) max(x_lo, .Machine$double.xmin) else x_lo - x_pad,
    max = x_hi + x_pad,
    scale = TRUE,
    name = x_name,
    nameLocation = "middle", nameGap = 28,
    nameTextStyle = wise_eaxis_name(align = "center"),
    axisLabel = wise_eaxis_label(
      rotate = 0,
      showMinLabel = if (isTRUE(hide_extreme_x)) FALSE else NULL,
      showMaxLabel = if (isTRUE(hide_extreme_x)) FALSE else NULL,
      formatter = if (isTRUE(log_scale)) {
        htmlwidgets::JS(
          "function(v){return v.toLocaleString('en-US');}"
        )
      } else {
        htmlwidgets::JS(
          "function(v){var a=Math.abs(v);if(a>=1000){return (v/1000).toLocaleString('en-US',{maximumFractionDigits:1})+'k';}return String(Math.round(v*100)/100);}"
        )
      }
    ),
    axisLine = list(lineStyle = list(color = .wise_grid)),
    splitLine = wise_esplit_line(),
    splitNumber = if (isTRUE(hide_extreme_x)) 6 else NULL
  )
  e$x$opts$yAxis <- list(
    type = "value", name = y_name,
    nameLocation = "end",
    nameRotate = 0,
    nameGap = 8,
    nameTextStyle = wise_eyaxis_name(),
    min = 0, max = n_r + 1, interval = 1,
    axisLabel = wise_eaxis_label(
      interval = 0L,
      formatter = htmlwidgets::JS(sprintf(
        "function(v){var m=%s;return m[String(Math.round(v))]||'';}",
        jsonlite::toJSON(label_map, auto_unbox = TRUE)
      ))
    ),
    axisLine = list(show = FALSE),
    axisTick = list(show = FALSE),
    splitLine = list(show = FALSE)
  )
  e$x$opts$grid <- list(
    containLabel = TRUE, left = 8, right = 20, top = 40, bottom = 44
  )
  e$x$opts$tooltip <- list(
    trigger = "axis",
    formatter = htmlwidgets::JS(
      sprintf(
        "function(params){\n          var seen = {};\n          var rows = [];\n          var axis = params && params.length ? params[0].axisValue : '';\n          var axisNumber = Number(axis);\n          var axisLabel = isFinite(axisNumber) ? axisNumber.toLocaleString('en-US', {maximumFractionDigits: 2}) : axis;\n          var xName = %s;\n          (params || []).forEach(function(p){\n            var key = p.seriesName || '';\n            if (seen[key]) return;\n            seen[key] = true;\n            var share = Array.isArray(p.value) ? Number(p.value[2]) : NaN;\n            var suffix = isFinite(share) ? (share * 100).toLocaleString('en-US', {maximumFractionDigits: 1}) + '%% < x' : '';\n            rows.push((p.marker || '') + key + (suffix ? ': <b>' + suffix + '</b>' : ''));\n          });\n          return 'x = ' + axisLabel + (xName ? ' ' + xName : '') + (rows.length ? '<br/>' + rows.join('<br/>') : '');\n        }",
        jsonlite::toJSON(tooltip_x_name %||% "", auto_unbox = TRUE)
      )
    )
  )
  wise_echart_theme(e)
}


#' Echarts residuals vs weather plot
#'
#' Interactive counterpart of `plot_resid_weather()` (fct_results.R,
#' guidelines §7): the same data preparation (model frame / binned column
#' fallback, bin ordering and labels) drawn as an `echarts4r` widget. Grey
#' points are individual residuals (jittered within bins for binned
#' predictors), orange marks are bin means, and the dashed line marks zero.
#'
#' @param model      Native fitted model (single model, e.g. median RIF).
#' @param haz_var    Weather variable name.
#' @param weather_df Weather data frame used to recover the first configured
#'   bin label for the omitted reference bin.
#' @param x_label    Unit-complete x-axis label.
#' @param height     Widget height; a CSS length or a number of pixels.
#'
#' @return An `echarts4r` widget, or `NULL` invisibly when there is nothing
#'   to draw (same contract as the ggplot builder).
#'
#' @noRd
echart_resid_weather <- function(model, haz_var, weather_df, x_label = haz_var,
                                 height = "300px") {
  # Data preparation copied verbatim from plot_resid_weather().
  df <- tryCatch(stats::model.frame(model), error = function(e) NULL)

  if (is.null(df) || !haz_var %in% names(df)) {
    mm <- resolve_model_matrix(model)
    if (is.null(mm)) {
      return(invisible(NULL))
    }

    if (haz_var %in% names(mm)) {
      df <- mm
    } else {
      haz_esc <- gsub("([\\[\\]\\(\\)\\^\\$\\.\\*\\+\\?])", "\\\\\\1", haz_var)
      bin_cols <- grep(paste0("^", haz_esc, "[\\[\\(]"), names(mm), value = TRUE)
      bin_cols <- bin_cols[!grepl(":", bin_cols)]
      if (length(bin_cols) == 0) {
        return(invisible(NULL))
      }

      Xb <- mm[, bin_cols, drop = FALSE]
      idx <- max.col(as.matrix(Xb), ties.method = "first")
      none_active <- rowSums(Xb != 0, na.rm = TRUE) == 0

      x_from_bins <- sub(paste0("^", haz_esc), "", bin_cols[idx])
      x_from_bins[none_active] <- get_first_bin_label(weather_df, haz_var)

      df <- data.frame(.haz_x = x_from_bins, stringsAsFactors = FALSE)
      haz_var <- ".haz_x"
    }
  }

  res <- tryCatch(stats::residuals(model), error = function(e) NULL)
  if (is.null(res)) {
    return(invisible(NULL))
  }

  x_vals <- df[[haz_var]]
  n <- min(length(x_vals), length(res))
  x_vals <- x_vals[seq_len(n)]
  res <- res[seq_len(n)]

  is_binned <- is.factor(x_vals) || is.character(x_vals)

  e <- .e_new(height)
  y_axis <- list(
    type = "value", scale = TRUE, name = "Residuals",
    nameLocation = "end",
    nameTextStyle = wise_eaxis_name(),
    axisLabel = wise_eaxis_label(), splitLine = wise_esplit_line()
  )
  zero_mark <- .e_zero_line()

  if (is_binned) {
    lvls <- levels(as.factor(x_vals))
    num_lo <- suppressWarnings(as.numeric(
      regmatches(lvls, regexpr("[0-9]+(\\.[0-9]+)?", lvls))
    ))
    lvls <- lvls[order(ifelse(is.na(num_lo), Inf, num_lo))]
    new_lab <- vapply(lvls, .cut_bin_label, character(1))

    bin_idx <- match(as.character(x_vals), lvls)
    set.seed(1)
    jit <- (bin_idx - 1L) + stats::runif(length(bin_idx), -0.18, 0.18)
    means <- vapply(lvls, function(l) {
      mean(res[as.character(x_vals) == l], na.rm = TRUE)
    }, numeric(1))

    e$x$opts$series <- list(
      list(
        name = "Residuals", type = "scatter",
        data = lapply(seq_along(jit), function(i) list(jit[i], res[i])),
        symbolSize = 4, z = 1,
        itemStyle = list(color = .wise_charcoal, opacity = 0.12),
        silent = TRUE, tooltip = list(show = FALSE),
        markLine = zero_mark
      ),
      list(
        name = "Bin mean", type = "scatter",
        data = lapply(seq_along(lvls), function(i) list(i - 1L, means[i])),
        symbolSize = 9, z = 2,
        itemStyle = list(color = .wise_marker_alt),
        tooltip = list(show = TRUE)
      )
    )
    e$x$opts$xAxis <- list(list(
      type = "category", data = as.character(new_lab),
      name = stringr::str_wrap(x_label, 40),
      nameLocation = "middle", nameGap = 32,
      nameTextStyle = wise_eaxis_name(align = "center"),
      axisLabel = modifyList(
        wise_eaxis_label(rotate = 30),
        list(formatter = htmlwidgets::JS("function(v){return v;}"))
      ),
      axisTick = list(alignWithLabel = TRUE),
      splitLine = wise_esplit_line()
    ))
  } else {
    xv <- as.numeric(x_vals)
    ok <- is.finite(xv) & is.finite(res)
    xv <- xv[ok]
    res <- res[ok]
    brks <- seq(min(xv), max(xv), length.out = 21)
    brks[1] <- brks[1] - 1e-9
    brks[length(brks)] <- brks[length(brks)] + 1e-9
    bins <- cut(xv, breaks = brks, include.lowest = TRUE)
    agg <- stats::aggregate(res ~ bins, FUN = mean)
    mids <- (head(brks, -1) + tail(brks, -1)) / 2
    mean_pts <- lapply(seq_len(nrow(agg)), function(i) {
      list(mids[as.integer(agg$bins[i])], agg$res[i])
    })

    e$x$opts$series <- list(
      list(
        name = "Residuals", type = "scatter",
        data = lapply(seq_along(xv), function(i) list(xv[i], res[i])),
        symbolSize = 4, z = 1,
        itemStyle = list(color = .wise_charcoal, opacity = 0.1),
        silent = TRUE, tooltip = list(show = FALSE),
        markLine = zero_mark
      ),
      list(
        name = "Bin mean", type = "scatter", data = mean_pts,
        symbolSize = 8, z = 2,
        itemStyle = list(color = .wise_marker_alt),
        tooltip = list(show = TRUE)
      )
    )
    e$x$opts$xAxis <- list(list(
      type = "value", scale = TRUE,
      name = stringr::str_wrap(x_label, 40),
      nameLocation = "middle", nameGap = 32,
      nameTextStyle = wise_eaxis_name(align = "center"),
      axisLabel = wise_eaxis_label(), splitLine = wise_esplit_line()
    ))
  }
  e$x$opts$yAxis <- list(y_axis)
  e$x$opts$grid <- list(
    containLabel = TRUE, left = 8, right = 20, top = 36, bottom = 46
  )
  e$x$opts$tooltip <- list(trigger = "item")
  wise_echart_theme(e)
}
