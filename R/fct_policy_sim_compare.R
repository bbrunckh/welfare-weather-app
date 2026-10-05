# Echarts4r builders (guidelines §7) -----------------------------------------
#
# Browser-side counterparts of the ggplot builders above. Statistics stay in
# R with the same parameters as the ggplot versions; echarts only draws the
# precomputed values. Each builder returns a widget, never NULL: inputs that
# the ggplot builder answered with `blank_plot("<message>")` come back as
# `echart_blank("<message>")` so the user-facing message is preserved.

# Deterministic even-stride downsample to <= max_points for very large raw
# series (guidelines §7); aggregated series are never downsampled.
#' @noRd
.wise_stride_downsample <- function(x, max_points = 10000L) {
  n <- length(x)
  if (n <= max_points) {
    return(x)
  }
  idx <- unique(pmin(pmax(floor(seq(1, n, length.out = max_points)), 1L), n))
  x[idx]
}

#' Before/after histogram for one manipulated variable (echarts4r)
#'
#' Binary variables render as grouped proportion bars; continuous variables
#' reuse the shared binned ridge-density renderer with empirical CDF tooltips.
#'
#' @param baseline_vals,policy_vals Raw baseline/policy values.
#' @param var_name Display label (x-axis title).
#' @param height   Widget height (the UI slot's height).
#'
#' @return An `echarts4r` widget.
#' @noRd
echart_before_after_hist <- function(baseline_vals, policy_vals,
                                     var_name, height = "300px") {
  as_finite_numeric <- function(x) {
    if (is.null(x)) return(numeric())
    x <- suppressWarnings(as.numeric(unname(x)))
    x[is.finite(x)]
  }
  baseline_clean <- as_finite_numeric(baseline_vals)
  policy_clean <- as_finite_numeric(policy_vals)
  all_vals <- c(baseline_clean, policy_clean)

  if (length(all_vals) == 0) {
    return(echart_blank("No data available", height = height))
  }
  uniq_vals <- unique(all_vals)
  is_binary <- length(uniq_vals) <= 2L && all(uniq_vals %in% c(0, 1))

  if (is_binary) {
    df <- data.frame(
      Value = factor(c("No", "Yes"), levels = c("No", "Yes")),
      Baseline = c(
        if (length(baseline_clean)) mean(baseline_clean == 0) else NA_real_,
        if (length(baseline_clean)) mean(baseline_clean == 1) else NA_real_
      ),
      Policy = c(
        if (length(policy_clean)) mean(policy_clean == 0) else NA_real_,
        if (length(policy_clean)) mean(policy_clean == 1) else NA_real_
      ),
      stringsAsFactors = FALSE
    )
    e <- df |>
      echarts4r::e_charts(Value, height = height) |>
      echarts4r::e_bar(Baseline) |>
      echarts4r::e_bar(Policy, name = "Policy") |>
      echarts4r::e_color(c(.wise_baseline, .wise_policy)) |>
      echarts4r::e_legend(orient = "horizontal", left = "center", top = 0) |>
      echarts4r::e_x_axis(
        name = var_name,
        nameLocation = "middle",
        nameGap = 26,
        nameMoveOverlap = FALSE,
        nameTextStyle = wise_eaxis_name(fontSize = 13),
        axisLabel = wise_eaxis_label(fontSize = 12),
        axisTick = list(alignWithLabel = TRUE)
      ) |>
      echarts4r::e_y_axis(
        name = NULL,
        max = 1,
        axisLabel = wise_eaxis_label(formatter = htmlwidgets::JS(
          "function(v){return (Number(v)*100).toLocaleString('en-US',{maximumFractionDigits:0})+'%';}"
        )),
        splitLine = wise_esplit_line()
      ) |>
      echarts4r::e_tooltip(
        trigger = "axis",
        valueFormatter = htmlwidgets::JS(
          "function(v){return (Number(v)*100).toLocaleString('en-US',{minimumFractionDigits:2,maximumFractionDigits:2})+'%';}"
        )
      ) |>
      echarts4r::e_grid(containLabel = TRUE, left = 8, right = 14, top = 38, bottom = 54) |>
      wise_echart_theme()
    return(e)
  }

  df <- data.frame(
    Group = factor(
      c(
        rep("Baseline", length(baseline_clean)),
        rep("Policy", length(policy_clean))
      ),
      levels = c("Baseline", "Policy")
    ),
    Value = c(baseline_clean, policy_clean),
    stringsAsFactors = FALSE
  )
  use_log <- all(all_vals > 0)
  rd <- build_ridge_distribution_data(
    df,
    x_var = "Value",
    group_var = "Group",
    fill_var = "Group",
    ridge_var = "Group",
    log_transform = use_log,
    n_bins = 256L,
    n_grid = 256L,
    bandwidth_scale = 1.5
  )
  if (is.null(rd)) {
    return(echart_blank("No data available", height = height))
  }

  rd$data$tooltip_value <- unlist(lapply(rd$groups, function(group) {
    vals <- sort(df$Value[as.character(df$Group) == group])
    x <- rd$data$x[rd$data$group == group]
    findInterval(x, vals) / length(vals)
  }), use.names = FALSE)
  group_colours <- c(Baseline = .wise_baseline, Policy = .wise_policy)
  styles <- data.frame(
    group = rd$groups,
    fill = unname(group_colours[rd$groups]),
    line = unname(c(Baseline = .wise_slate, Policy = .wise_policy_dark)[rd$groups]),
    dashed = FALSE,
    stringsAsFactors = FALSE
  )
  e <- ridge_echart_widget(
    rd$data, rd$ridges, rd$ridges, styles,
    height = height, log_scale = use_log, x_name = var_name,
    tooltip_x_name = var_name
  )
  e$x$opts$legend <- list(
    show = TRUE, orient = "horizontal", left = "center", top = 0,
    data = c("Baseline", "Policy"),
    textStyle = list(color = .wise_charcoal, fontSize = 12)
  )
  e$x$opts$xAxis$axisLabel$formatter <- htmlwidgets::JS(
    "function(v){return Number(v).toLocaleString('en-US',{minimumFractionDigits:2,maximumFractionDigits:2});}"
  )
  e$x$opts$tooltip <- list(
    trigger = "axis", confine = TRUE,
    formatter = htmlwidgets::JS(sprintf(
      "function(params){var p=(params||[]).filter(function(x){return x&&x.value;});if(!p.length)return '';var x=Number(p[0].axisValue);var fmt=function(v){return Number(v).toLocaleString('en-US',{minimumFractionDigits:2,maximumFractionDigits:2});};var rows=[];var seen={};p.forEach(function(s){var d=s.value;var share=Array.isArray(d)?Number(d[2]):NaN;if(seen[s.seriesName]||!isFinite(share))return;seen[s.seriesName]=true;rows.push((s.marker||'')+s.seriesName+': CDF share = <b>'+fmt(share*100)+'%%</b>');});return '<b>'+%s+': '+fmt(x)+'</b><br>'+rows.join('<br>');}",
      jsonlite::toJSON(as.character(var_name)[1L], auto_unbox = TRUE)
    )),
    textStyle = list(color = .wise_charcoal, fontSize = 13)
  )
  e$x$opts$grid <- list(
    containLabel = TRUE, left = 8, right = 20, top = 42, bottom = 54,
    width = "auto", height = "auto"
  )
  wise_echart_theme(e)
}

.wise_result_rate_unit <- function(x_label) {
  if (grepl("(percent)", x_label, fixed = TRUE)) return("%")
  if (grepl("(pp)", x_label, fixed = TRUE)) return(" pp")
  if (grepl("Poverty rate|Poverty gap|Poverty severity", x_label)) return("%")
  ""
}

.wise_result_axis_label <- function(x_label) {
  unit <- .wise_result_rate_unit(x_label)
  if (!nzchar(unit)) return(wise_eaxis_label())
  wise_eaxis_label(formatter = htmlwidgets::JS(
    sprintf("function(v){return (v*100).toLocaleString('en-US',{maximumFractionDigits:1})+%s;}",
      jsonlite::toJSON(unit, auto_unbox = TRUE))
  ))
}

.wise_result_tooltip <- function(x_label) {
  htmlwidgets::JS(sprintf("function(p) {
    var d = p.data || {};
    function esc(s) { return String(s).replace(/[&<>\"']/g, function(c) {
      return {'&':'&amp;','<':'&lt;','>':'&gt;','\"':'&quot;',\"'\":'&#39;'}[c]; }); }
    var percent = %s;
    function num(v, change) { return Number.isFinite(v) ? (percent ? v*100 : v).toLocaleString('en-US', {maximumFractionDigits: percent ? 1 : 3}) + (percent ? (change ? ' pp' : %s) : '') : 'Not available'; }
    var text = '<b>' + esc(d.scenario || p.seriesName) + '</b>';
    if (d.rp_label) text += '<br>' + esc(d.rp_label);
    if (d.source && d.source !== 'All') text += '<br>' + esc(d.source);
    text += '<br>' + esc(d.is_mean ? 'Scenario mean' : %s) + ': <b>' + num(d.outcome) + '</b>';
    if (Number.isFinite(d.baseline)) text += '<br>Baseline: ' + num(d.baseline) + '<br>Policy: ' + num(d.policy) + '<br>Policy change: ' + num(d.policy - d.baseline, true);
    if (Number.isFinite(d.lo) && Number.isFinite(d.hi) && d.hi > d.lo) text += '<br>Climate-model spread: ' + num(d.lo) + ' to ' + num(d.hi);
    return text;
  }", tolower(as.character(nzchar(.wise_result_rate_unit(x_label)))),
    jsonlite::toJSON(.wise_result_rate_unit(x_label), auto_unbox = TRUE),
    jsonlite::toJSON(x_label, auto_unbox = TRUE)))
}

#' Step 3 annual baseline/policy distribution chart (echarts4r)
#'
#' Step-3-specific counterpart of `plot_annual_distribution()`
#' (fct_sim_compare.R) as rendered in the Step 3 results pane: one row per
#' scenario (Historical on top) with violins or boxes over the raw weather-
#' year draws, mean markers, and the historical-baseline reference line.
#' Violin/box statistics are precomputed in R: violin densities use
#' `stats::density()` with default bandwidth (ggplot's nrd0/trim behaviour),
#' scaled to the ggplot's row width; boxes use `boxplot.stats()`. The ggplot
#' builder remains available as the visual reference.
#'
#' @param tbl       A `timeseries_curves`-style data frame (scenario, value,
#'   and optionally source).
#' @param x_label   Outcome-axis title.
#' @param plot_type "violin" or "boxplot".
#' @param height    Widget height (the UI slot's height).
#' @param pending   Scenario labels still computing (Step 2 progressive
#'   results). They get a fixed, empty category row with a muted
#'   "(computing)" axis label so rows do not shift as scenarios land. The
#'   default leaves the output unchanged (Step 3).
#'
#' @return An `echarts4r` widget.
#' @noRd
echart_step3_annual_distribution <- function(tbl, x_label = "Outcome (outcome units)",
                                        plot_type = "violin",
                                       height = "470px",
                                       pending = character(0)) {
  plot_type <- match.arg(plot_type, c("violin", "boxplot"))
  if (is.null(tbl) || !nrow(tbl)) {
    return(echart_blank("No annual simulation results available.", height = height))
  }
  if (!any(is.finite(tbl$value))) {
    return(echart_blank("No finite annual simulation results available.", height = height))
  }
  df <- tbl
  df$scenario <- as.character(df$scenario)
  df$period <- ifelse(df$scenario == "Historical", "Historical",
    vapply(df$scenario, .parse_year, character(1L))
  )
  df$ssp <- ifelse(df$scenario == "Historical", "Historical",
    vapply(df$scenario, .normalise_ssp, character(1L))
  )
  pending <- setdiff(as.character(pending), c(df$scenario, "Historical"))
  scenario_levels <- c("Historical", sort(unique(c(
    df$scenario[df$scenario != "Historical"], pending
  ))))
  scenario_palette <- c(Historical = .wise_history)
  pending_ssps <- vapply(pending, .normalise_ssp, character(1L))
  for (ssp in unique(c(df$ssp[df$ssp != "Historical"], unname(pending_ssps)))) {
    members <- scenario_levels[scenario_levels != "Historical"]
    members <- members[vapply(members, function(s) {
      identical(.normalise_ssp(s), ssp)
    }, logical(1L))]
    members <- members[order(vapply(members, .parse_year, character(1L)))]
    base_col <- if (ssp %in% names(.ssp_colours)) {
      unname(.ssp_colours[[ssp]])
    } else {
      "#0072B2"
    }
    shades <- if (length(members) > 1L) {
      colorspace::lighten(base_col, seq(0.30, 0, length.out = length(members)))
    } else {
      base_col
    }
    scenario_palette[members] <- shades
  }
  df$scenario_key <- factor(df$scenario, levels = scenario_levels)

  has_source <- "source" %in% names(df) && length(unique(df$source)) > 1L
  hist_vals <- if (has_source) {
    h <- df$value[df$scenario == "Historical" & df$source == "Baseline"]
    if (!length(h)) df$value[df$scenario == "Historical"] else h
  } else {
    df$value[df$scenario == "Historical"]
  }
  # The reference and marker use the same displayed historical baseline draws.
  hist_mean <- if (length(hist_vals)) mean(hist_vals, na.rm = TRUE) else NA_real_

  n_rows <- length(scenario_levels)
  df$row_y <- n_rows + 1L - as.integer(df$scenario_key)
  y_breaks <- seq_len(n_rows)
  y_labs <- vapply(y_breaks, function(b) {
    lab <- sub(" / ", "\n", scenario_levels[n_rows + 1L - b], fixed = TRUE)
    if (scenario_levels[n_rows + 1L - b] %in% pending) {
      lines <- strsplit(lab, "\n", fixed = TRUE)[[1L]]
      lines[[length(lines)]] <- paste(lines[[length(lines)]], "(computing)")
      lab <- paste0("{pending|", lines, "}", collapse = "\n")
    }
    lab
  }, character(1L))
  band_ys <- y_breaks[(max(y_breaks) - y_breaks) %% 2 == 1]
  if (has_source) {
    src_off <- c(Baseline = 0.19, Policy = -0.19)
    df$y_off <- unname(src_off[as.character(df$source)])
    alpha_map <- c(Baseline = 0.22, Policy = 0.35)
  }

  e <- echarts4r::e_charts(
    data.frame(x = c(0, 1), y = c(0, 1)),
    x,
    height = height
  )
  series <- list()
  push <- function(s) {
    if (!is.null(s)) {
      series <<- append(series, list(s))
    }
    invisible(NULL)
  }
  area_data <- lapply(band_ys, function(b) {
    list(list(yAxis = b - 0.45), list(yAxis = b + 0.45))
  })
  .mark_area <- function(s) {
    if (!is.null(s) && !is.null(s$type)) {
      s$markArea <- list(
        silent = TRUE,
        itemStyle = list(color = "rgba(247,249,251,0.5)"),
        label = list(show = FALSE),
        data = area_data
      )
    }
    s
  }

  # Render each violin as an explicit polygon so it closes along both density
  # edges instead of filling from its outline to the row baseline.
  violin_series <- function(y, half_w, x_vals, col, opacity) {
    if (length(x_vals) < 2L) {
      return(NULL)
    }
    x_vals <- .wise_stride_downsample(as.numeric(x_vals))
    d <- stats::density(x_vals, n = 512L, bw = "nrd0")
    keep <- d$x >= min(x_vals) & d$x <= max(x_vals)
    if (sum(keep) < 2L) {
      return(NULL)
    }
    d$x <- d$x[keep]
    d$y <- d$y[keep]
    w <- if (max(d$y) > 0) half_w * d$y / max(d$y) else rep(0, length(d$y))
    points <- unname(rbind(cbind(d$x, y + w), cbind(rev(d$x), y - rev(w))))
    list(
      type = "custom",
      renderItem = htmlwidgets::JS(sprintf(
        "function(params, api) { var pts = %s; return {type: 'polygon', shape: {points: pts.map(function(p){return api.coord(p);})}, style: {fill: '%s', opacity: %s, stroke: '%s', lineWidth: 1}}; }",
        jsonlite::toJSON(points, digits = NA), col, format(opacity), col
      )),
      data = list(list(min(d$x), y - half_w, max(d$x), y + half_w)),
      encode = list(x = c(0, 2), y = c(1, 3)),
      itemStyle = list(color = col),
      areaStyle = list(color = col, opacity = opacity),
      lineStyle = list(color = col, width = 1, opacity = 0.9),
      tooltip = list(show = FALSE),
      silent = TRUE,
      z = 2
    )
  }
  # Boxes as whisker line + filled rectangle + median tick, using
  # boxplot.stats() (the same summary geom_boxplot draws).
  box_series <- function(y, half_h, x_vals, col, opacity) {
    if (length(x_vals) < 2L) {
      return(NULL)
    }
    st <- suppressWarnings(grDevices::boxplot.stats(x_vals)$stats)
    if (!length(st) || any(!is.finite(st))) {
      return(NULL)
    }
    q1 <- st[[2L]]; med <- st[[3L]]; q3 <- st[[4L]]
    list(
      whisker = list(
        type = "line",
        data = unname(rbind(c(st[[1L]], y), c(st[[5L]], y))),
        symbol = "none",
        silent = TRUE,
        lineStyle = list(color = .wise_support, width = 1),
        z = 2
      ),
      box = list(
        type = "custom",
        renderItem = htmlwidgets::JS(sprintf(
          "function(params, api) { var a = api.coord([api.value(0), api.value(1)]); var b = api.coord([api.value(2), api.value(3)]); var rect = echarts.graphic.clipRectByRect({x: a[0], y: b[1], width: b[0]-a[0], height: a[1]-b[1]}, params.coordSys); return rect && {type: 'rect', shape: rect, style: {fill: '%s', opacity: %s, stroke: '%s', lineWidth: 1}}; }",
          col, format(opacity), .wise_support
        )),
        data = list(list(q1, y - half_h, q3, y + half_h)),
        encode = list(x = c(0, 2), y = c(1, 3)),
        itemStyle = list(color = col),
        silent = TRUE,
        tooltip = list(show = FALSE),
        z = 3
      ),
      median = list(
        type = "line",
        data = unname(rbind(c(med, y - half_h), c(med, y + half_h))),
        symbol = "none",
        silent = TRUE,
        lineStyle = list(color = .wise_support, width = 2),
        z = 4
      )
    )
  }

  first_shape <- TRUE
  add_shape <- function(s) {
    if (is.null(s)) {
      return(invisible(NULL))
    }
    if (first_shape) {
      # markArea lives on a series; the row banding rides on the first shape.
      s <- .mark_area(s)
      first_shape <<- FALSE
    }
    push(s)
  }

  if (has_source) {
    for (scen in scenario_levels) {
      row_y <- n_rows + 1L - match(scen, scenario_levels)
      for (src in names(src_off)) {
        x_vals <- df$value[df$scenario == scen & df$source == src]
        x_vals <- x_vals[is.finite(x_vals)]
        if (!length(x_vals)) {
          next
        }
        col <- unname(scenario_palette[[scen]])
        y <- row_y + src_off[[src]]
        if (identical(plot_type, "violin")) {
          add_shape(violin_series(y, 0.17, x_vals, col, unname(alpha_map[[src]])))
        } else {
          bs <- box_series(y, 0.08, x_vals, col, unname(alpha_map[[src]]))
          if (!is.null(bs)) {
            add_shape(bs$whisker)
            add_shape(bs$box)
            add_shape(bs$median)
          }
        }
      }
    }
  } else {
    for (scen in scenario_levels) {
      row_y <- n_rows + 1L - match(scen, scenario_levels)
      x_vals <- df$value[df$scenario == scen]
      x_vals <- x_vals[is.finite(x_vals)]
      if (!length(x_vals)) {
        next
      }
      col <- unname(scenario_palette[[scen]])
      if (identical(plot_type, "violin")) {
        add_shape(violin_series(row_y, 0.31, x_vals, col, 0.28))
      } else {
        bs <- box_series(row_y, 0.15, x_vals, col, 0.45)
        if (!is.null(bs)) {
          add_shape(bs$whisker)
          add_shape(bs$box)
          add_shape(bs$median)
        }
      }
    }
  }

  # Raw weather-year draws use deterministic jitter without changing RNG state.
  dot_spread <- if (has_source) 0.20 else 0.36
  jit <- (((seq_len(nrow(df)) * 37L) %% 101L) / 100 - 0.5) * 2 * dot_spread
  # Matrix-derived outcomes retain year names; a named list serializes as an
  # object rather than the array ECharts requires for series.data.
  dot_rows <- unname(which(is.finite(df$value)))
  dot_rows <- dot_rows[.wise_stride_downsample(seq_along(dot_rows), max_points = 10000L)]
  dots <- lapply(dot_rows, function(i) {
    base_y <- df$row_y[[i]] + if (has_source) df$y_off[[i]] else 0
    src <- if (has_source) as.character(df$source[[i]]) else "All"
    list(
      value = c(df$value[[i]], base_y + jit[[i]]),
      outcome = df$value[[i]], scenario = as.character(df$scenario[[i]]),
      source = src,
      itemStyle = list(
        color = unname(scenario_palette[[as.character(df$scenario_key[[i]])]]),
        opacity = 0.2
      )
    )
  })
  # Muted individual years sit behind the summaries and mean markers.
  push(list(
    type = "scatter",
    name = "Draws",
    data = dots,
    symbol = "circle",
    symbolSize = 5,
    z = 1
  ))

  # Series means: open slate-ringed (baseline) and filled policy markers.
  mean_series <- function(src, size, fill_col, stroke_col) {
    if (has_source) {
      m <- stats::aggregate(value ~ scenario_key + source,
        data = df[is.finite(df$value), , drop = FALSE],
        FUN = mean
      )
      m <- m[m$source == src, , drop = FALSE]
      pts <- lapply(seq_len(nrow(m)), function(i) {
        row_y <- n_rows + 1L - as.integer(m$scenario_key[[i]])
        c(m$value[[i]], row_y + src_off[[src]])
      })
    } else {
      m <- stats::aggregate(value ~ scenario_key,
        data = df[is.finite(df$value), , drop = FALSE],
        FUN = mean
      )
      pts <- lapply(seq_len(nrow(m)), function(i) {
        row_y <- n_rows + 1L - as.integer(m$scenario_key[[i]])
        c(m$value[[i]], row_y)
      })
    }
    list(
      type = "scatter",
      name = paste("Mean", src),
      data = lapply(seq_along(pts), function(i) list(
        value = pts[[i]], outcome = m$value[[i]],
        scenario = as.character(m$scenario_key[[i]]),
        source = if (has_source) src else "All", mean = m$value[[i]], is_mean = TRUE
      )),
      symbol = "circle",
      symbolSize = size,
      itemStyle = list(
        color = fill_col,
        borderColor = stroke_col,
        borderWidth = 1
      ),
      z = 6
    )
  }
  if (has_source) {
    push(mean_series("Baseline", 6, "white", .wise_slate))
    push(mean_series("Policy", 6.8, .wise_policy, .wise_policy_dark))
  } else {
    push(mean_series("All", 6, "white", .wise_slate))
  }

  # One-time Baseline/Policy captions right of the top row's last draw.
  if (has_source) {
    top_scen <- scenario_levels[[1L]]
    cap_pts <- lapply(names(src_off), function(src) {
      vals <- df$value[df$scenario == top_scen & df$source == src]
      vals <- vals[is.finite(vals)]
      if (!length(vals)) {
        return(NULL)
      }
      list(
        value = c(
          max(vals) + 0.015 * diff(range(df$value, na.rm = TRUE)),
          n_rows + src_off[[src]]
        ),
        label = list(
          show = TRUE,
          formatter = src,
          position = "right",
          color = if (identical(src, "Baseline")) .wise_slate else .wise_policy_dark,
          fontWeight = "bold",
          fontSize = 13
        ),
        symbolSize = 0
      )
    })
    push(list(
      type = "scatter",
      data = Filter(Negate(is.null), cap_pts),
      silent = TRUE,
      z = 7
    ))
  }

  # Historical mean reference line rides on the last series.
  if (is.finite(hist_mean) && length(series)) {
    series[[length(series)]]$markLine <- list(
      silent = TRUE,
      symbol = "none",
      precision = 12,
      lineStyle = list(type = "dashed", color = .wise_zero, width = 1),
      data = list(list(xAxis = hist_mean)),
      label = list(
        show = TRUE,
        formatter = "Historical mean",
        position = "insideEndTop",
        rotate = 0,
        align = "left",
        offset = c(8, 22),
        color = .wise_zero,
        fontSize = 12
      )
    )
  }
  e$x$opts$series <- series

  e$x$opts$xAxis <- list(
    type = "value",
    scale = TRUE,
    name = x_label,
    nameLocation = "middle",
    nameGap = 28,
    nameTextStyle = wise_eaxis_name(fontSize = 13),
    axisLabel = modifyList(.wise_result_axis_label(x_label), list(fontSize = 12)),
    axisLine = list(lineStyle = list(color = .wise_grid)),
    splitLine = wise_esplit_line()
  )
  y_label_map <- stats::setNames(as.list(y_labs), as.character(y_breaks))
  names(y_label_map) <- as.character(y_breaks)
  e$x$opts$yAxis <- list(
    type = "value",
    min = 0,
    max = n_rows + 1L,
    interval = 1,
    axisLabel = wise_eaxis_label(
      fontSize = 12,
      showMinLabel = FALSE,
      showMaxLabel = FALSE,
      formatter = htmlwidgets::JS(sprintf(
        "function(v){ var m = %s; return m[String(Math.round(v))] || ''; }",
        jsonlite::toJSON(y_label_map, auto_unbox = TRUE)
      ))
    ),
    axisLine = list(show = FALSE),
    splitLine = list(show = FALSE)
  )
  if (length(pending)) {
    e$x$opts$yAxis$axisLabel$rich <- list(
      pending = list(color = "#9aa9b5", fontStyle = "italic", fontSize = 12)
    )
  }
  e$x$opts$tooltip <- list(
    trigger = "item", confine = TRUE,
    formatter = .wise_result_tooltip(x_label),
    textStyle = list(color = .wise_charcoal, fontSize = 13)
  )
  e$x$opts$textStyle <- list(fontFamily = "Helvetica, Arial, sans-serif")
  e$x$opts$grid <- list(containLabel = TRUE, left = 8, right = 90, top = 10, bottom = 58)
  wise_echart_theme(e)
}

#' Adverse return-period dumbbell chart (echarts4r)
#'
#' Browser-side counterpart of `plot_step3_adverse_dot()`: per return-period
#' rows with vertical scenario dodge, alternating row banding, climate-model
#' spread segments, baseline -> policy connector arrows, and one-time
#' Baseline/Policy captions and scenario labels. All positions (rp_y,
#' dodge_offset, label anchors) are precomputed in R with the same code as
#' the ggplot version.
#'
#' @param tbl      A `step3_adverse_dot_data()` data frame.
#' @param x_label  Outcome-axis title.
#' @param height   Widget height (the UI slot's height).
#'
#' @return An `echarts4r` widget.
#' @noRd
echart_step3_adverse_dot <- function(tbl, x_label = "Outcome level",
                                     height = "380px") {
  if (is.null(tbl) || !nrow(tbl)) {
    return(echart_blank("Return-period outcomes are unavailable.", height = height))
  }
  scenario_levels <- c(
    "Historical",
    sort(unique(as.character(tbl$scenario[!tbl$is_historical])))
  )
  scenario_colours <- stats::setNames(vapply(scenario_levels, function(s) {
    if (identical(s, "Historical")) {
      return(.wise_support)
    }
    ssp <- .normalise_ssp(s)
    if (ssp %in% names(.ssp_colours)) unname(.ssp_colours[[ssp]]) else .wise_slate
  }, character(1L)), scenario_levels)
  tbl$scenario_key <- factor(
    ifelse(tbl$is_historical, "Historical", as.character(tbl$scenario)),
    levels = scenario_levels
  )
  dodge_width <- 0.6
  tbl$rp_y <- as.integer(tbl$rp_label)
  tbl$dodge_offset <- stats::ave(
    seq_len(nrow(tbl)),
    tbl$rp_y,
    FUN = function(idx) {
      k <- length(idx)
      if (k <= 1L) {
        return(0)
      }
      seq(-(k - 1L) / 2, (k - 1L) / 2, length.out = k)[
        order(match(as.character(tbl$scenario_key[idx]), scenario_levels))
      ] * (dodge_width / max(k - 1L, 1))
    }
  )
  top_y <- max(tbl$rp_y)
  top_rows <- tbl[tbl$rp_y == top_y, , drop = FALSE]
  top_rows <- top_rows[!duplicated(top_rows$scenario_key), , drop = FALSE]
  x_vals <- c(
    tbl$baseline_val, tbl$policy_val, tbl$policy_lo, tbl$policy_hi,
    tbl$base_lo, tbl$base_hi
  )
  x_span <- diff(range(x_vals, na.rm = TRUE))
  lab_gap <- if (is.finite(x_span) && x_span > 0) 0.012 * x_span else 0
  top_rows$lab_x <- vapply(seq_len(nrow(top_rows)), function(i) {
    r <- top_rows[i, ]
    hi <- suppressWarnings(max(r$policy_hi[[1L]], r$policy_val[[1L]],
      r$base_hi[[1L]], r$baseline_val[[1L]],
      na.rm = TRUE
    ))
    if (!is.finite(hi)) hi <- r$policy_val[[1L]]
    hi + lab_gap
  }, numeric(1L))
  right_mult <- if (is.finite(x_span) && x_span > 0) {
    max(0.12, max(nchar(as.character(top_rows$scenario_key)), 0L) * 0.012)
  } else {
    0.05
  }
  y_breaks <- sort(unique(tbl$rp_y))
  y_labs <- levels(tbl$rp_label)[y_breaks]
  band_ys <- y_breaks[(max(y_breaks) - y_breaks) %% 2 == 1]

  e <- echarts4r::e_charts(
    data.frame(x = c(0, 1), y = c(0, 1)),
    x,
    height = height
  )
  series <- list()
  push <- function(s) {
    if (!is.null(s)) {
      series <<- append(series, list(s))
    }
    invisible(NULL)
  }

  # Alternating return-period banding (markArea rides on the first series).
  first <- TRUE
  .push_marked <- function(s) {
    if (first) {
      s$markArea <- list(
        silent = TRUE,
        itemStyle = list(color = "rgba(247,249,251,0.5)"),
        label = list(show = FALSE),
        data = lapply(band_ys, function(b) {
          list(list(yAxis = b - 0.45), list(yAxis = b + 0.45))
        })
      )
      first <<- FALSE
    }
    push(s)
  }

  # Climate-model spread segments (future rows only), broken into disjoint
  # segments with NA separators inside one series per source.
  spread_series <- function(lo_col, hi_col, col) {
    ok <- !tbl$is_historical & is.finite(tbl[[lo_col]]) & is.finite(tbl[[hi_col]])
    if (!any(ok)) {
      return(NULL)
    }
    d <- tbl[ok, ]
    pts <- unlist(lapply(seq_len(nrow(d)), function(i) {
      y <- d$rp_y[[i]] + d$dodge_offset[[i]]
      list(c(d[[lo_col]][[i]], y), c(d[[hi_col]][[i]], y), c(NA_real_, NA_real_))
    }), recursive = FALSE)
    list(
      type = "line",
      data = pts,
      symbol = "none",
      silent = TRUE,
      lineStyle = list(color = col, width = 2.4, opacity = 0.4, cap = "round"),
      z = 2
    )
  }
  s <- spread_series("base_lo", "base_hi", "#0072B2")
  if (!is.null(s)) .push_marked(s)
  s <- spread_series("policy_lo", "policy_hi", .wise_policy)
  if (!is.null(s)) .push_marked(s)

  # Baseline -> policy connectors with open arrowheads, one 'lines' series
  # coloured per scenario item.
  conn <- lapply(seq_len(nrow(tbl)), function(i) {
    if (!is.finite(tbl$baseline_val[[i]]) || !is.finite(tbl$policy_val[[i]])) {
      return(NULL)
    }
    y <- tbl$rp_y[[i]] + tbl$dodge_offset[[i]]
    list(
      coords = list(
        c(tbl$baseline_val[[i]], y),
        c(tbl$policy_val[[i]], y)
      ),
      lineStyle = list(
        color = unname(scenario_colours[as.character(tbl$scenario_key[[i]])])
      )
    )
  })
  .push_marked(list(
    type = "lines",
    coordinateSystem = "cartesian2d",
    data = Filter(Negate(is.null), conn),
    symbol = c("none", "arrow"),
    symbolSize = 7,
    silent = TRUE,
    tooltip = list(show = FALSE),
      lineStyle = list(width = 1.6),
    z = 3
  ))

  # Baseline (open) and policy (filled) markers.
  y_off_i <- function(i) tbl$rp_y[[i]] + tbl$dodge_offset[[i]]
  .push_marked(list(
    type = "scatter",
    name = "Baseline",
    data = lapply(which(is.finite(tbl$baseline_val)), function(i) {
      list(
        value = c(tbl$baseline_val[[i]], y_off_i(i)),
        outcome = tbl$baseline_val[[i]], source = "Baseline",
        scenario = as.character(tbl$scenario[[i]]), rp_label = as.character(tbl$rp_label[[i]]),
        baseline = tbl$baseline_val[[i]], policy = tbl$policy_val[[i]],
        lo = tbl$base_lo[[i]], hi = tbl$base_hi[[i]],
        itemStyle = list(
          color = "white",
          borderColor = unname(scenario_colours[as.character(tbl$scenario_key[[i]])]),
          borderWidth = 1.2
        )
      )
    }),
    symbolSize = 8,
    z = 5
  ))
  .push_marked(list(
    type = "scatter",
    name = "Policy",
    data = lapply(which(is.finite(tbl$policy_val)), function(i) {
      list(
        value = c(tbl$policy_val[[i]], y_off_i(i)),
        outcome = tbl$policy_val[[i]], source = "Policy",
        scenario = as.character(tbl$scenario[[i]]), rp_label = as.character(tbl$rp_label[[i]]),
        baseline = tbl$baseline_val[[i]], policy = tbl$policy_val[[i]],
        lo = tbl$policy_lo[[i]], hi = tbl$policy_hi[[i]],
        itemStyle = list(
          color = .wise_policy,
          borderColor = .wise_policy_dark,
          borderWidth = 1
        )
      )
    }),
    symbolSize = 8,
    z = 6
  ))

  # One-time per-scenario labels right of the top row (colour per scenario).
  .push_marked(list(
    type = "scatter",
    data = lapply(seq_len(nrow(top_rows)), function(i) {
      list(
        value = c(top_rows$lab_x[[i]], top_rows$rp_y[[i]] + top_rows$dodge_offset[[i]]),
        label = list(
          show = TRUE,
          formatter = as.character(top_rows$scenario_key[[i]]),
          position = "right",
          color = unname(scenario_colours[as.character(top_rows$scenario_key[[i]])]),
          fontWeight = "bold",
          fontSize = 13
        ),
        symbolSize = 0
      )
    }),
    silent = TRUE,
    z = 7
  ))
  e$x$opts$legend <- list(
    top = 0, left = "center", selectedMode = FALSE,
    data = list(
      list(name = "Baseline", icon = "circle", itemStyle = list(color = "white", borderColor = .wise_slate, borderWidth = 1.2)),
      list(name = "Policy", icon = "circle", itemStyle = list(color = .wise_policy, borderColor = .wise_policy_dark))
    ),
    textStyle = list(color = .wise_charcoal, fontSize = 12)
  )

  # Baseline markers after all markArea hosting is done: the markArea rides
  # on the first pushed series only, so the remaining pushes are plain.
  e$x$opts$series <- series
  finite_x <- x_vals[is.finite(x_vals)]
  if (!length(finite_x)) {
    return(echart_blank("No finite adverse-year outcomes available.", height = height))
  }
  x_span <- diff(range(finite_x))
  if (!is.finite(x_span) || x_span <= 0) {
    x_span <- max(abs(finite_x), 1) * 0.1
  }
  e$x$opts$xAxis <- list(
    type = "value",
    min = min(finite_x) - 0.02 * x_span,
    max = max(finite_x) + right_mult * x_span,
    name = x_label,
    nameLocation = "middle",
    nameGap = 28,
    nameTextStyle = wise_eaxis_name(fontSize = 13),
    axisLabel = modifyList(.wise_result_axis_label(x_label), list(fontSize = 12)),
    axisLine = list(lineStyle = list(color = .wise_grid)),
    splitLine = wise_esplit_line()
  )
  e$x$opts$yAxis <- list(
    type = "value",
    min = 0,
    max = max(y_breaks) + 1L,
    interval = 1,
    axisLabel = wise_eaxis_label(
      fontSize = 12,
      showMinLabel = FALSE,
      showMaxLabel = FALSE,
      formatter = htmlwidgets::JS(sprintf(
        "function(v){ var m = %s; return m[String(Math.round(v))] || ''; }",
        jsonlite::toJSON(stats::setNames(as.list(y_labs), as.character(y_breaks)), auto_unbox = TRUE)
      ))
    ),
    axisLine = list(show = FALSE),
    splitLine = list(show = FALSE)
  )
  e$x$opts$tooltip <- list(
    trigger = "item", confine = TRUE, formatter = .wise_result_tooltip(x_label),
    textStyle = list(color = .wise_charcoal, fontSize = 13)
  )
  e$x$opts$textStyle <- list(fontFamily = "Helvetica, Arial, sans-serif")
  e$x$opts$grid <- list(containLabel = TRUE, left = 8, right = 110, top = 35, bottom = 58)
  wise_echart_theme(e)
}

#' Detect columns that differ between the baseline and policy-adjusted frames
#'
#' Returns the names of columns whose values differ between
#' \code{baseline_svy} and \code{policy_svy}. Used by the Step 3 diagnostics
#' table to surface any variable a user manipulation has touched -
#' covariates, interaction variables, or outcomes alike.
#'
#' Comparison rules:
#' \itemize{
#'   \item Numeric columns are compared with tolerance via
#'     \code{isTRUE(all.equal(..., check.attributes = FALSE))}.
#'   \item Other columns are compared with \code{identical()}.
#' }
#'
#' Rows must match across the two frames; if \code{nrow()} differs the
#' function returns the union of column names instead (since values can no
#' longer be compared element-wise).
#'
#' @param baseline_svy Data frame before \code{apply_policy_to_svy()}.
#' @param policy_svy   Data frame after \code{apply_policy_to_svy()}.
#'
#' @param candidates Optional character vector restricting comparison to known
#'   candidate columns. NULL retains the generic all-shared-columns behavior.
#' @return Character vector of column names that changed.
#' @export
detect_manipulated_vars <- function(baseline_svy, policy_svy,
                                    candidates = NULL) {
  if (is.null(baseline_svy) || is.null(policy_svy)) {
    return(character(0))
  }
  shared <- intersect(names(baseline_svy), names(policy_svy))
  if (!is.null(candidates)) shared <- shared[shared %in% candidates]
  if (length(shared) == 0) {
    return(character(0))
  }
  if (nrow(baseline_svy) != nrow(policy_svy)) {
    all_cols <- union(names(baseline_svy), names(policy_svy))
    if (!is.null(candidates)) all_cols <- all_cols[all_cols %in% candidates]
    return(all_cols)
  }
  changed <- vapply(shared, function(v) {
    xb <- baseline_svy[[v]]
    xp <- policy_svy[[v]]
    if (is.numeric(xb) && is.numeric(xp)) {
      !isTRUE(all.equal(xb, xp, check.attributes = FALSE))
    } else {
      !identical(xb, xp)
    }
  }, logical(1))
  shared[changed]
}


#' Build a Diagnostics Summary for Policy-Adjusted Inputs
#'
#' Computes mean / sd / n_nonNA for each covariate in both the baseline and
#' policy-adjusted survey frames, so the Step 3 Results tab can display
#' what changed.
#'
#' @param baseline_svy Data frame before \code{apply_policy_to_svy()}.
#' @param policy_svy   Data frame after \code{apply_policy_to_svy()}.
#' @param vars         Character vector of variable names to summarise. If
#'   \code{NULL}, uses the intersection of the two frames' numeric cols.
#'
#' @return A tibble with columns \code{variable}, \code{mean_baseline},
#'   \code{mean_policy}, \code{delta_mean}, \code{sd_baseline},
#'   \code{sd_policy}, \code{n_nonNA}.
#' @export
policy_input_diagnostics <- function(baseline_svy, policy_svy, vars = NULL) {
  if (is.null(baseline_svy) || is.null(policy_svy)) {
    return(NULL)
  }

  if (is.null(vars)) {
    num_b <- names(baseline_svy)[vapply(baseline_svy, is.numeric, logical(1))]
    num_p <- names(policy_svy)[vapply(policy_svy, is.numeric, logical(1))]
    vars <- intersect(num_b, num_p)
    # Drop obvious non-covariate keys
    vars <- setdiff(vars, c("loc_id", "int_year", "int_month", "sim_year"))
  }

  vars <- vars[vars %in% names(baseline_svy) & vars %in% names(policy_svy)]

  if (length(vars) == 0) {
    return(NULL)
  }

  rows <- lapply(vars, function(v) {
    xb <- suppressWarnings(as.numeric(baseline_svy[[v]]))
    xp <- suppressWarnings(as.numeric(policy_svy[[v]]))
    data.frame(
      variable = v,
      mean_baseline = mean(xb, na.rm = TRUE),
      mean_policy = mean(xp, na.rm = TRUE),
      delta_mean = mean(xp, na.rm = TRUE) - mean(xb, na.rm = TRUE),
      sd_baseline = stats::sd(xb, na.rm = TRUE),
      sd_policy = stats::sd(xp, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}


# Step 3 Results pure helpers ----

# Range-valid probabilities do not validate the analytic policy method.
.policy_endpoint_status <- function(so, context = NULL) {
  type <- tolower(as.character(so$type %||% ""))[1L]
  model_type <- tolower(as.character(if (!is.null(context)) {
    context$model_type %||% ""
  } else ""))[1L]
  unsupported <- type %in% c("binary", "logical", "boolean") ||
    model_type %in% c("logistic", "logistic regression", "binomial")
  list(status = if (unsupported) "unsupported" else "ok",
    reason = if (unsupported) paste(
      "Policy contrast unavailable: the analytic logistic/binary policy correction",
      "is not a validated response-scale method, even when predictions are within [0,1]."
    ) else NULL)
}

#' Build Step 3 Results Headline Cards
#'
#' Pure function returning a list of 5 card specifications for
#' \code{headline_cards_ui()}, focused on policy outcomes:
#' \enumerate{
#'   \item Expected policy effect (signed change, baseline vs policy context, focus scenario)
#'   \item Policy effect at the adverse 1-in-20 threshold (other supported tail effects)
#'   \item Policy channels (level vs resilience breakdown)
#'   \item Program scale & reach (population covered/affected by all implemented policies)
#'   \item Policy robustness (model agreement & simulation scope)
#' }
#' @noRd
step3_headline_cards <- function(paired_summary,
                                 threshold_tbl = NULL,
                                 baseline_agg = NULL,
                                 policy_agg = NULL,
                                 policy_svy = NULL,
                                 sp_scenario = NULL,
                                 timeseries_curves = NULL,
                                 method = "mean",
                                 so = NULL,
                                 baseline_svy = NULL,
                                 analysis_unit = NULL,
                                 metric_context = NULL,
                                 metric_decomposition = NULL,
                                 endpoint_status = list(status = "ok", reason = NULL)) {
  if (is.null(paired_summary) || !nrow(paired_summary) ||
    !"scenario" %in% names(paired_summary)) {
    return(NULL)
  }

  levels <- as.character(paired_summary$scenario)
  fut_effects <- paired_summary[!grepl("^Historical", levels), , drop = FALSE]
  focus <- if (nrow(fut_effects)) fut_effects[1L, , drop = FALSE] else paired_summary[1L, , drop = FALSE]
  focus_scen <- as.character(focus$scenario[[1L]])
  spec <- metric_context %||% metric_metadata(method, so)
  analysis_unit <- analysis_unit %||% spec$analysis_unit %||% NULL
  card_digits <- if (identical(method, "total")) 0L else 2L
  fmt_level <- function(x) if (is.finite(x)) format_metric_value(x, spec, digits = card_digits) else "Unavailable"
  fmt_change <- function(x) if (is.finite(x)) format_metric_value(x, spec, change = TRUE, digits = card_digits) else "Unavailable"

  # Levels and contrast must use the same matched model/year support.
  b_mean <- if ("baseline" %in% names(focus)) focus$baseline[[1L]] else NA_real_
  p_mean <- if ("policy" %in% names(focus)) focus$policy[[1L]] else NA_real_

  # 1. Expected policy effect
  if (is.list(metric_decomposition) && is.data.frame(metric_decomposition$summary) &&
      nrow(metric_decomposition$summary)) {
    focus_summary <- metric_decomposition$summary[
      metric_decomposition$summary$scenario == focus_scen, , drop = FALSE]
    if (nrow(focus_summary)) {
      b_mean <- focus_summary$baseline[[1L]]
      p_mean <- focus_summary$policy[[1L]]
    }
  }
  effect_val <- if (is.finite(b_mean) && is.finite(p_mean)) p_mean - b_mean else
    focus$value[[1L]] %||% focus$effect[[1L]] %||% NA_real_
  val_1 <- fmt_change(effect_val)

  line1_1 <- if (is.finite(b_mean) && is.finite(p_mean)) {
    "Policy vs baseline"
  } else {
    "Paired policy minus baseline"
  }
  line2_1 <- if (nrow(fut_effects) > 1L) {
    paste0(focus_scen, " (focus of ", nrow(fut_effects), ")")
  } else {
    focus_scen
  }

  card1 <- list(
    label = "Expected policy effect",
    value = val_1,
    note = paste(line1_1, line2_1, sep = " \u00b7 "),
    note_html = shiny::tagList(
      shiny::tags$div(line1_1),
      shiny::tags$div(style = "font-weight: 600;", line2_1)
    ),
    info = paste(
      if (is.finite(b_mean) && is.finite(p_mean)) {
        paste0("Policy: ", fmt_level(p_mean), " vs baseline: ", fmt_level(b_mean), ".")
      } else "",
      "Equal-model mean.",
      "Paired difference (policy minus baseline) for the fixed population.",
      "Years averaged within model; climate models weighted equally. A positive value",
      "indicates an increase in the selected metric, not necessarily a benefit.",
      metric_context_note(spec),
      if ("n_dropped_model_years" %in% names(focus) &&
          focus$n_dropped_model_years[[1L]] > 0) {
        paste(focus$n_dropped_model_years[[1L]],
          "nonfinite matched model/year cells excluded from both endpoints and effect.")
      } else ""
    )
  )

  # 2. Adverse weather year protection (1-in-20 headline)
  eff_10 <- NA_real_
  eff_20 <- NA_real_
  eff_50 <- NA_real_
  if (!is.null(threshold_tbl) && nrow(threshold_tbl) && "source" %in% names(threshold_tbl)) {
    rp_map <- metric_decision_return_periods(method %||% "mean", so)
    rp_10 <- unname(rp_map[["Adverse 1-in-10"]])
    rp_20 <- unname(rp_map[["Adverse 1-in-20"]])
    rp_50 <- unname(rp_map[["Adverse 1-in-50"]])

    get_eff <- function(rp_id) {
      if (is.null(rp_id) || !nzchar(rp_id)) {
        return(NA_real_)
      }
      b <- threshold_tbl$value[threshold_tbl$scenario == focus_scen & threshold_tbl$source == "Baseline" &
        threshold_tbl$rp_name == rp_id & threshold_tbl$Estimate %in% c("Equal-model mean", "Single historical estimate", "Central (P50)")]
      p <- threshold_tbl$value[threshold_tbl$scenario == focus_scen & threshold_tbl$source == "Policy" &
        threshold_tbl$rp_name == rp_id & threshold_tbl$Estimate %in% c("Equal-model mean", "Single historical estimate", "Central (P50)")]
      if (length(b) && length(p) && is.finite(b[[1L]]) && is.finite(p[[1L]])) p[[1L]] - b[[1L]] else NA_real_
    }
    eff_10 <- get_eff(rp_10)
    eff_20 <- get_eff(rp_20)
    eff_50 <- get_eff(rp_50)
  }

  metric_focus <- NULL
  metric_tail_focus <- NULL
  metric_focus_reason <- "Metric-aware channel summary is unavailable."
  if (is.list(metric_decomposition)) {
    candidate <- metric_decomposition$scenarios[[focus_scen]]
      if (is.list(candidate) && identical(candidate$status, "ok") &&
          is.data.frame(candidate$summary) && nrow(candidate$summary)) {
      metric_focus <- candidate$summary[1L, , drop = FALSE]
      metric_focus_reason <- NULL
    } else if (!is.null(candidate$reason)) {
      metric_focus_reason <- candidate$reason
    } else if (!is.null(metric_decomposition$reason)) {
      metric_focus_reason <- metric_decomposition$reason
    }
  } else if (!is.null(metric_decomposition$reason)) {
    metric_focus_reason <- metric_decomposition$reason
  }
  if (is.list(metric_decomposition)) {
    tail <- metric_decomposition$return_period
    get_metric_tail <- function(return_period) {
      tail_row <- if (is.data.frame(tail) && nrow(tail)) {
        tail[tail$scenario == focus_scen & tail$scope == "baseline_anchored" &
          abs(tail$return_period - return_period) < 1e-8, , drop = FALSE]
      } else data.frame()
      if (nrow(tail_row) && identical(tail_row$status[[1L]], "ok") &&
          is.finite(tail_row$total[[1L]])) tail_row$total[[1L]] else NA_real_
    }
    tail_20 <- if (is.data.frame(tail) && nrow(tail)) {
      tail[tail$scenario == focus_scen & tail$scope == "baseline_anchored" &
        abs(tail$return_period - 20) < 1e-8 & tail$status == "ok", , drop = FALSE]
    } else data.frame()
    if (nrow(tail_20)) {
      metric_tail_focus <- tail_20[1L, , drop = FALSE]
      if (identical(metric_tail_focus$status[[1L]], "ok")) metric_focus_reason <- NULL
    }
    for (rp in c(10, 20, 50)) {
      value <- get_metric_tail(rp)
      if (!is.finite(value)) next
      if (rp == 10) eff_10 <- value
      if (rp == 20) eff_20 <- value
      if (rp == 50) eff_50 <- value
    }
  }

  # Never substitute a legacy independently-ranked endpoint when the shared
  # Results support is missing or unavailable.
  if (is.list(metric_decomposition)) {
    eff_10 <- get_metric_tail(10)
    eff_20 <- get_metric_tail(20)
    eff_50 <- get_metric_tail(50)
  }

  val_2 <- fmt_change(eff_20)

  line1_2 <- if (is.finite(eff_20)) "Policy vs baseline" else "Baseline-anchored adverse result unavailable"
  line2_2 <- "1-in-20 year"

  card2 <- list(
    label = "Adverse weather years",
    value = val_2,
    note = paste(line1_2, line2_2, focus_scen, sep = " \u00b7 "),
    note_html = shiny::tagList(
      shiny::tags$div(line1_2),
      shiny::tags$div(style = "font-weight: 600;", line2_2),
      shiny::tags$div(focus_scen)
    ),
    info = paste(
      "Baseline-anchored adverse quantile of the selected annual aggregate metric. Adjacent baseline-year ranks are interpolated and the same within-model weights are applied to policy.",
      "SSP values use an equal-model mean. This is not a weather-loss-avoided estimate; component uncertainty is not estimated."
    )
  )

  # 3. Resilience effect in the selected metric. Never fall back to the
  # historical technical decomposition when annual channel attribution fails.
  resilience_modeled <- !is.null(metric_decomposition) &&
    (isTRUE(metric_decomposition$metadata$repositioning_modeled) ||
      isTRUE(metric_decomposition$metadata$interaction_included))
  val_3 <- if (!is.null(metric_tail_focus) && resilience_modeled &&
      length(metric_tail_focus$resilience) && is.finite(metric_tail_focus$resilience[[1L]])) {
    fmt_change(metric_tail_focus$resilience[[1L]])
  } else "Unavailable"
  main_text <- repositioning_text <- interaction_text <- "Unavailable"
  if (!is.null(metric_focus)) {
    repositioning_modeled <- isTRUE(metric_decomposition$metadata$repositioning_modeled)
    interaction_included <- isTRUE(metric_decomposition$metadata$interaction_included)
    main_text <- fmt_change(metric_focus$main[[1L]])
  }
  if (!is.null(metric_tail_focus) && resilience_modeled &&
      length(metric_tail_focus$resilience) && is.finite(metric_tail_focus$resilience[[1L]])) {
    val_3 <- fmt_change(metric_tail_focus$resilience[[1L]])
  }
  if (!is.null(metric_tail_focus)) {
    repositioning_modeled <- isTRUE(metric_decomposition$metadata$repositioning_modeled)
    interaction_included <- isTRUE(metric_decomposition$metadata$interaction_included)
    if (length(metric_tail_focus$main) && is.finite(metric_tail_focus$main[[1L]])) {
      main_text <- fmt_change(metric_tail_focus$main[[1L]])
    }
    repositioning_text <- if (repositioning_modeled && length(metric_tail_focus$repositioning) &&
        is.finite(metric_tail_focus$repositioning[[1L]])) {
      fmt_change(metric_tail_focus$repositioning[[1L]])
    } else if (repositioning_modeled) {
      "Unavailable"
    } else "Not modeled by this engine"
    interaction_text <- if (interaction_included && length(metric_tail_focus$interaction) &&
        is.finite(metric_tail_focus$interaction[[1L]])) {
      fmt_change(metric_tail_focus$interaction[[1L]])
    } else if (interaction_included) {
      "Unavailable"
    } else "Not included in fitted model"
  }
  line1_3 <- if (!is.null(metric_tail_focus)) {
    paste0("Main: ", main_text, " · Repositioning: ",
      if (identical(repositioning_text, "Not modeled by this engine")) "Not modeled" else repositioning_text,
      " · Interaction: ", interaction_text)
  } else if (!is.null(metric_focus)) {
    "1-in-20 year attribution unavailable"
  } else metric_focus_reason
  line2_3 <- paste(focus_scen, "· 1-in-20 year")

  card3 <- list(
    label = "Resilience effect",
    value = val_3,
    note = paste(line1_3, line2_3, sep = " \u00b7 "),
    note_html = shiny::tagList(
      shiny::tags$div(line1_3),
      shiny::tags$div(style = "font-weight: 600;", line2_3)
    ),
    info = paste(
      "Baseline-anchored adverse quantile of the selected annual aggregate metric:",
      "the main effect, then modeled repositioning, then interaction.",
      "The resilience subtotal is repositioning plus interaction, not the main effect.",
      "The same interpolated baseline-year rank weights are applied to every cumulative state.",
      "This is not an avoided-loss estimate. Component uncertainty is not estimated.",
      metric_context_note(spec),
      "Main effect:", main_text,
      "Repositioning:", repositioning_text,
      "Interaction:", interaction_text
    )
  )

  # 4. Program scale & reach
  # Units covered or affected by any implemented policy: social protection
  # recipients plus units whose covariates another policy lever changed -
  # the same union the Diagnostics tab's coverage table reports. Without a
  # baseline frame, fall back to social-protection recipients only.
  touched <- if (!is.null(policy_svy) && is.data.frame(policy_svy)) {
    if (!is.null(baseline_svy) && is.data.frame(baseline_svy) &&
      nrow(baseline_svy) == nrow(policy_svy)) {
      tryCatch(policy_reach_mask(baseline_svy, policy_svy), error = function(e) NULL)
    } else if (SP_TRANSFER_COL %in% names(policy_svy)) {
      v <- suppressWarnings(as.numeric(policy_svy[[SP_TRANSFER_COL]]))
      is.finite(v) & v > 0
    } else {
      NULL
    }
  } else {
    NULL
  }

  scale_val <- "Unavailable"
  reach_households <- NA_real_
  line1_4 <- "Population covered or affected"

  if (!is.null(touched) && any(touched, na.rm = TRUE)) {
    w <- if ("weight" %in% names(policy_svy)) as.numeric(policy_svy$weight) else rep(1, nrow(policy_svy))
    ok <- touched & is.finite(w)
    # The reach card reports people. Diagnostics also breaks represented
    # households out separately for household-mode analysis.
    pop <- sum(w[ok])
    scale_val <- if (is.finite(pop) && pop > 0) paste0(fmt_num(pop / 1e6, 1), "M") else "Unavailable"
    if (identical(analysis_unit, "hh")) {
      hh <- if ("hhsize" %in% names(policy_svy)) {
        suppressWarnings(as.numeric(policy_svy$hhsize))
      } else {
        rep(1, nrow(policy_svy))
      }
      hh[!is.finite(hh) | hh <= 0] <- 1
      reach_households <- sum((w / hh)[ok])
      line1_4 <- "People represented; recipient households reported in Diagnostics"
    }
  }

  card4 <- list(
    label = "Program reach",
    value = scale_val,
    note = line1_4,
    note_html = shiny::tagList(
      shiny::tags$div(line1_4)
    ),
    info = paste(
      "Population covered or affected is the weighted number of people represented",
      "by units touched by any implemented policy: social protection recipients plus",
      "units whose covariates another policy lever changed. It matches the coverage",
      "table on the Diagnostics tab. For household analysis, Diagnostics also reports",
      "represented household equivalents separately."
    )
  )
  if (is.finite(reach_households)) {
    card4$households_represented <- reach_households
    card4$note <- paste0(line1_4, " · ", fmt_num(reach_households), " households")
    card4$note_html <- shiny::tagList(
      shiny::tags$div(line1_4),
      shiny::tags$div(style = "font-weight: 600;", paste0(
        fmt_num(reach_households), " covered-household equivalents"
      ))
    )
  }

  # 5. Policy robustness & consensus
  n_mods <- suppressWarnings(as.integer(focus$n_models %||% 1L))[1L]
  if (!is.finite(n_mods) || n_mods < 1L) n_mods <- 1L

  lo_val <- focus$intermod_lo[[1L]] %||% NA_real_
  hi_val <- focus$intermod_hi[[1L]] %||% NA_real_

  val_5 <- if (is.finite(lo_val) && is.finite(hi_val) && lo_val > 0) {
    "100% positive"
  } else if (is.finite(lo_val) && is.finite(hi_val)) {
    paste(fmt_change(lo_val), "to", fmt_change(hi_val))
  } else if (n_mods > 1L) {
    paste0(n_mods, " models agreed")
  } else {
    "Consistent"
  }

  line1_5 <- if (is.finite(lo_val) && is.finite(hi_val) && n_mods > 1L) {
    range_value <- function(x) {
      formatted <- fmt_change(x)
      suffix <- paste0(" ", spec$change_unit)
      if (endsWith(formatted, suffix)) {
        substr(formatted, 1L, nchar(formatted) - nchar(suffix))
      } else {
        formatted
      }
    }
    paste0("Model range: ", range_value(lo_val), " to ", range_value(hi_val),
      if (identical(spec$format, "percent")) " pp" else
        if (is.null(spec$change_unit) || !nzchar(spec$change_unit)) "" else paste0(" ", spec$change_unit))
  } else if (n_mods > 1L) {
    paste0("Ensemble across ", n_mods, " models")
  } else {
    "Single climate model"
  }

  total_runs <- if (!is.null(timeseries_curves) && nrow(timeseries_curves)) {
    nrow(timeseries_curves[timeseries_curves$source == "Policy", , drop = FALSE])
  } else {
    length(unique(paired_summary$scenario)) * n_mods * 30L
  }

  line2_5 <- paste0("Across ", total_runs, " simulations")
  sample_rows <- if (is.data.frame(baseline_svy)) nrow(baseline_svy) else NA_integer_
  prediction_count <- if (is.finite(sample_rows) && sample_rows > 0L) {
    as.numeric(total_runs) * sample_rows
  } else NA_real_
  prediction_note <- format_prediction_count(prediction_count, analysis_unit)

  card5 <- list(
    label = "Policy robustness",
    value = val_5,
    note = paste(line1_5, line2_5, prediction_note, sep = " \u00b7 "),
    note_html = shiny::tagList(
      shiny::tags$div(line1_5),
      shiny::tags$div(style = "font-weight: 600;", line2_5),
      shiny::tags$div(style = "font-weight: 600;", prediction_note)
    ),
      info = paste(
        "Consistency of the signed policy change across all simulated CMIP6 climate models",
        "and weather years. Disagreement across models indicates climate uncertainty",
        "in policy effectiveness. Model range values use the metric's displayed units."
      )
  )
  card5$prediction_count_native <- prediction_count
  card5$prediction_count_note <- prediction_note
  card5$prediction_sample_rows <- sample_rows

  card1$metric_note <- metric_context_note(spec)
  cards <- list(card1, card2, card3, card4, card5)
  if (!identical(endpoint_status$status, "ok")) {
    for (id in c(1L, 2L, 3L, 5L)) {
      cards[[id]]$value <- "Unavailable"
      cards[[id]]$note <- cards[[id]]$info <- endpoint_status$reason
      cards[[id]]$note_html <- shiny::tags$div(endpoint_status$reason)
    }
  }

  cards
}

step3_headline_df <- function(cards) {
  if (is.null(cards) || !length(cards)) {
    return(tibble::tibble())
  }
  dplyr::bind_rows(lapply(seq_along(cards), function(i) {
    c_info <- cards[[i]]
    tibble::tibble(
      card       = as.integer(i),
      label      = as.character(c_info$label %||% ""),
      value      = as.character(c_info$value %||% ""),
      note       = as.character(c_info$note %||% ""),
      households_represented = suppressWarnings(as.numeric(
        c_info$households_represented %||% NA_real_
      )),
      prediction_count_native = suppressWarnings(as.numeric(c_info$prediction_count_native %||% NA_real_)),
      prediction_sample_rows = suppressWarnings(as.numeric(c_info$prediction_sample_rows %||% NA_real_)),
      prediction_count_display = as.character(c_info$prediction_count_note %||% ""),
      info       = as.character(c_info$info %||% "")
    )
  }))
}

step3_adverse_dot_data <- function(threshold_tbl, method = "mean", so = NULL) {
  if (is.null(threshold_tbl) || !nrow(threshold_tbl)) {
    return(tibble::tibble())
  }
  rp_map <- metric_decision_return_periods(method, so)
  keep_rps <- unname(rp_map)
  tbl <- threshold_tbl[threshold_tbl$rp_name %in% keep_rps, , drop = FALSE]
  if (!nrow(tbl)) {
    return(tibble::tibble())
  }

  has_source <- "source" %in% names(tbl)
  if (!has_source) {
    return(step2_adverse_dot_data(threshold_tbl, method, so))
  }

  central <- tbl[tbl$Estimate %in% c("Equal-model mean", "Single historical estimate", "Central (P50)"), , drop = FALSE]
  if (!nrow(central)) {
    return(tibble::tibble())
  }
  central$rp_label <- names(rp_map)[match(central$rp_name, unname(rp_map))]
  central <- filter_historically_supported_return_periods(central, rp_map,
    source_col = "source")
  central <- central[is.finite(central$value), , drop = FALSE]
  if (!nrow(central)) return(tibble::tibble())

  ens_rows <- tbl[tbl$source == "Policy" & grepl("^Ensemble ", tbl$Estimate), , drop = FALSE]
  if (nrow(ens_rows)) {
    parts <- lapply(split(ens_rows, ens_rows$scenario), function(x) {
      n_each <- nrow(x) %/% 2L
      if (n_each < 1L || nrow(x) != 2L * n_each) {
        return(NULL)
      }
      list(
        lo = x[seq_len(n_each), , drop = FALSE],
        hi = x[seq.int(n_each + 1L, nrow(x)), , drop = FALSE]
      )
    })
    parts <- Filter(Negate(is.null), parts)
    if (length(parts)) {
      ens_lo <- dplyr::bind_rows(lapply(parts, `[[`, "lo"))
      ens_hi <- dplyr::bind_rows(lapply(parts, `[[`, "hi"))
    } else {
      ens_lo <- ens_hi <- tbl[FALSE, , drop = FALSE]
    }
  } else {
    ens_lo <- ens_hi <- tbl[FALSE, , drop = FALSE]
  }

  # Baseline (no-policy) ensemble spread: every future scenario also has a
  # baseline run with its own across-model disagreement, so the dot plot can
  # show a spread band for both series.
  ens_rows_b <- tbl[tbl$source == "Baseline" & grepl("^Ensemble ", tbl$Estimate), , drop = FALSE]
  if (nrow(ens_rows_b)) {
    parts_b <- lapply(split(ens_rows_b, ens_rows_b$scenario), function(x) {
      n_each <- nrow(x) %/% 2L
      if (n_each < 1L || nrow(x) != 2L * n_each) {
        return(NULL)
      }
      list(
        lo = x[seq_len(n_each), , drop = FALSE],
        hi = x[seq.int(n_each + 1L, nrow(x)), , drop = FALSE]
      )
    })
    parts_b <- Filter(Negate(is.null), parts_b)
    if (length(parts_b)) {
      ens_lo_b <- dplyr::bind_rows(lapply(parts_b, `[[`, "lo"))
      ens_hi_b <- dplyr::bind_rows(lapply(parts_b, `[[`, "hi"))
    } else {
      ens_lo_b <- ens_hi_b <- tbl[FALSE, , drop = FALSE]
    }
  } else {
    ens_lo_b <- ens_hi_b <- tbl[FALSE, , drop = FALSE]
  }

  scenarios <- unique(as.character(central$scenario))
  rp_order <- c("Expected", "Adverse 1-in-5", "Adverse 1-in-10", "Adverse 1-in-20", "Adverse 1-in-50")

  rows <- list()
  for (sc in scenarios) {
    for (rp in names(rp_map)) {
      rp_id <- rp_map[[rp]]
      b_row <- central[central$scenario == sc & central$source == "Baseline" & central$rp_name == rp_id, , drop = FALSE]
      p_row <- central[central$scenario == sc & central$source == "Policy" & central$rp_name == rp_id, , drop = FALSE]

      b_val <- if (nrow(b_row)) b_row$value[[1L]] else NA_real_
      p_val <- if (nrow(p_row)) p_row$value[[1L]] else NA_real_

      # A plotted policy contrast is only meaningful when both endpoints use
      # the same Results-owned support. Do not fall back to one endpoint.
      if (!is.finite(b_val) || !is.finite(p_val)) next

      lo_val <- ens_lo$value[ens_lo$scenario == sc & ens_lo$rp_name == rp_id]
      hi_val <- ens_hi$value[ens_hi$scenario == sc & ens_hi$rp_name == rp_id]
      pol_lo <- if (length(lo_val) && is.finite(lo_val[[1L]])) lo_val[[1L]] else p_val
      pol_hi <- if (length(hi_val) && is.finite(hi_val[[1L]])) hi_val[[1L]] else p_val

      # Baseline (no-policy) spread band for this scenario and RP.
      lo_val_b <- ens_lo_b$value[ens_lo_b$scenario == sc & ens_lo_b$rp_name == rp_id]
      hi_val_b <- ens_hi_b$value[ens_hi_b$scenario == sc & ens_hi_b$rp_name == rp_id]
      is_hist <- identical(sc, "Historical")
      base_lo <- if (!is_hist && length(lo_val_b) && is.finite(lo_val_b[[1L]])) {
        lo_val_b[[1L]]
      } else {
        NA_real_
      }
      base_hi <- if (!is_hist && length(hi_val_b) && is.finite(hi_val_b[[1L]])) {
        hi_val_b[[1L]]
      } else {
        NA_real_
      }

      ssp_k <- if (is_hist) "Historical" else .normalise_ssp(sc)
      yr_l <- if (is_hist) "Historical" else .parse_year(sc)

      rows[[length(rows) + 1L]] <- tibble::tibble(
        scenario      = sc,
        rp_name       = rp_id,
        rp_label      = rp,
        baseline_val  = b_val,
        policy_val    = p_val,
        policy_lo     = if (is.finite(p_val)) pol_lo else NA_real_,
        policy_hi     = if (is.finite(p_val)) pol_hi else NA_real_,
        base_lo       = base_lo,
        base_hi       = base_hi,
        effect        = if (is.finite(p_val) && is.finite(b_val)) p_val - b_val else NA_real_,
        ssp_key       = ssp_k,
        yr_lbl        = yr_l,
        is_historical = is_hist
      )
    }
  }
  out <- dplyr::bind_rows(rows)
  if (!nrow(out)) {
    return(tibble::tibble())
  }
  present <- rp_order[rp_order %in% as.character(out$rp_label)]
  out$rp_label <- factor(out$rp_label, levels = rev(present))
  out
}

plot_step3_adverse_dot <- function(tbl, x_label = "Outcome level",
                                   title = NULL, subtitle = NULL) {
  if (is.null(tbl) || !nrow(tbl)) {
    return(blank_plot("Return-period outcomes are unavailable."))
  }
  scenario_levels <- c(
    "Historical",
    sort(unique(as.character(tbl$scenario[!tbl$is_historical])))
  )
  # Scenario colours follow the exceedance plot's scheme: Historical in the
  # dark support navy, futures in their fixed SSP colour.
  scenario_colours <- stats::setNames(vapply(scenario_levels, function(s) {
    if (identical(s, "Historical")) {
      return(.wise_support)
    }
    ssp <- .normalise_ssp(s)
    if (ssp %in% names(.ssp_colours)) unname(.ssp_colours[[ssp]]) else .wise_slate
  }, character(1L)), scenario_levels)
  tbl$scenario_key <- factor(
    ifelse(tbl$is_historical, "Historical", as.character(tbl$scenario)),
    levels = scenario_levels
  )

  # Vertical dodge: multiple scenarios share each return-period row, so
  # offset the dumbbells per scenario to keep them readable. Historical
  # keeps the lower slot; each future scenario takes its own slot above it.
  # Wide multi-SSP runs need generous slot spacing to avoid crowding.
  dodge_width <- 0.6
  tbl$rp_y <- as.integer(tbl$rp_label)
  tbl$dodge_offset <- stats::ave(
    seq_len(nrow(tbl)),
    tbl$rp_y,
    FUN = function(idx) {
      k <- length(idx)
      if (k <= 1L) {
        return(0)
      }
      seq(-(k - 1L) / 2, (k - 1L) / 2, length.out = k)[
        order(match(as.character(tbl$scenario_key[idx]), scenario_levels))
      ] * (dodge_width / max(k - 1L, 1))
    }
  )

  # Direct labeling replaces the legend. Series names anchor just past the
  # right-most end of each top-row (Expected) dumbbell - baseline end
  # included, so the label stays on the right-hand side even when the
  # outcome direction flips and policy sits left of baseline. The x
  # expansion is enlarged so the longest label stays inside the panel.
  top_y <- max(tbl$rp_y)
  top_rows <- tbl[tbl$rp_y == top_y, , drop = FALSE]
  # One label per scenario even if a scenario contributes several top rows.
  top_rows <- top_rows[!duplicated(top_rows$scenario_key), , drop = FALSE]

  x_vals <- c(
    tbl$baseline_val, tbl$policy_val, tbl$policy_lo, tbl$policy_hi,
    tbl$base_lo, tbl$base_hi
  )
  x_span <- diff(range(x_vals, na.rm = TRUE))
  # Uniform label gap, in data units, so every series label clears its
  # marker by the same distance regardless of bands or direction.
  lab_gap <- if (is.finite(x_span) && x_span > 0) 0.012 * x_span else 0

  top_rows$lab_x <- vapply(seq_len(nrow(top_rows)), function(i) {
    r <- top_rows[i, ]
    hi <- suppressWarnings(max(r$policy_hi[[1L]], r$policy_val[[1L]],
      r$base_hi[[1L]], r$baseline_val[[1L]],
      na.rm = TRUE
    ))
    if (!is.finite(hi)) hi <- r$policy_val[[1L]]
    hi + lab_gap
  }, numeric(1L))
  top_rows$lab_col <- unname(scenario_colours[as.character(top_rows$scenario_key)])
  # One-time Baseline/Policy captions on the topmost dumbbell of the top row.
  cap_row <- top_rows[which.max(top_rows$dodge_offset), , drop = FALSE]

  right_mult <- if (is.finite(x_span) && x_span > 0) {
    max(0.12, max(nchar(as.character(top_rows$scenario_key)), 0L) * 0.012)
  } else {
    0.05
  }

  y_breaks <- sort(unique(tbl$rp_y))
  y_labs <- levels(tbl$rp_label)[y_breaks]

  # Alternating light-grey banding behind every second return-period row
  # (counting from the top), so the dumbbells that share a row read as one
  # group. Drawn first, i.e. behind every geom.
  band_ys <- y_breaks[(max(y_breaks) - y_breaks) %% 2 == 1]
  rp_band_data <- data.frame(
    ymin = band_ys - 0.45, ymax = band_ys + 0.45
  )

  p <- ggplot2::ggplot(tbl, ggplot2::aes(y = .data$rp_y + .data$dodge_offset)) +
    ggplot2::geom_rect(
      data = rp_band_data,
      ggplot2::aes(ymin = .data$ymin, ymax = .data$ymax),
      xmin = -Inf, xmax = Inf, fill = ggplot2::alpha("#F7F9FB", 0.5), colour = NA,
      inherit.aes = FALSE, show.legend = FALSE
    ) +
    # Climate-model spread bands, future scenarios only (Historical runs a
    # single climate model): same width for both sources; baseline in
    # transparent blue, policy in transparent policy vermillion.
    ggplot2::geom_segment(
      data = subset(tbl, !is_historical),
      ggplot2::aes(
        x = .data$base_lo, xend = .data$base_hi,
        yend = .data$rp_y + .data$dodge_offset
      ),
      colour = "#0072B2", alpha = 0.40, linewidth = 2.4,
      lineend = "round", na.rm = TRUE
    ) +
    ggplot2::geom_segment(
      data = subset(tbl, !is_historical),
      ggplot2::aes(
        x = .data$policy_lo, xend = .data$policy_hi,
        yend = .data$rp_y + .data$dodge_offset
      ),
      colour = .wise_policy, alpha = 0.40, linewidth = 2.4,
      lineend = "round", na.rm = TRUE
    ) +
    # Baseline -> policy connector: solid, in the scenario colour, with a
    # large open arrowhead so the policy direction reads at a glance.
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$baseline_val, xend = .data$policy_val,
        yend = .data$rp_y + .data$dodge_offset,
        colour = .data$scenario_key
      ),
      linewidth = 1.0, lineend = "round",
      arrow = ggplot2::arrow(length = ggplot2::unit(10, "pt"), type = "open"),
      na.rm = TRUE, show.legend = FALSE
    ) +
    # Baseline point: open marker in the scenario colour.
    ggplot2::geom_point(
      ggplot2::aes(x = .data$baseline_val, colour = .data$scenario_key),
      fill = "white", shape = 21, stroke = 1.1, size = 3.4, na.rm = TRUE
    ) +
    # Policy point: filled policy vermillion.
    ggplot2::geom_point(
      ggplot2::aes(x = .data$policy_val),
      fill = .wise_policy, colour = .wise_policy_dark, shape = 21,
      stroke = 1.0, size = 3.4, na.rm = TRUE
    ) +
    # One-time Baseline/Policy captions above the topmost dumbbell only.
    ggplot2::geom_text(
      data = cap_row,
      ggplot2::aes(x = .data$baseline_val, y = .data$rp_y + .data$dodge_offset),
      label = "Baseline", vjust = -1.4, size = 3.5, fontface = "bold",
      colour = .wise_slate, show.legend = FALSE, inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data = cap_row,
      ggplot2::aes(x = .data$policy_val, y = .data$rp_y + .data$dodge_offset),
      label = "Policy", vjust = -1.4, size = 3.5, fontface = "bold",
      colour = .wise_policy_dark, show.legend = FALSE, inherit.aes = FALSE
    ) +
    # One-time series labels on the top row, coloured per scenario.
    ggplot2::geom_text(
      data = top_rows,
      ggplot2::aes(
        x = .data$lab_x, y = .data$rp_y + .data$dodge_offset,
        label = as.character(.data$scenario_key)
      ),
      colour = top_rows$lab_col, hjust = 0, size = 3.9,
      fontface = "bold", show.legend = FALSE, inherit.aes = FALSE
    ) +
    ggplot2::scale_colour_manual(values = scenario_colours, guide = "none") +
    # Return-period names are the y-axis text, outside the panel like every
    # other chart.
    ggplot2::scale_y_continuous(
      breaks = y_breaks, labels = y_labs,
      expand = ggplot2::expansion(add = c(0.6, 0.6))
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.02, right_mult))
    ) +
    ggplot2::labs(
      x = x_label, y = NULL,
      title = title,
      subtitle = subtitle
    ) +
    theme_wise() +
    ggplot2::theme(
      legend.position = "none"
    )
  # Multiple future periods stay on one shared x-axis: every period variant
  # is its own scenario row with its own top-row label, so no faceting.
  p
}

# Reactable styling for the return-period threshold table (guidelines §6):
# the frame arrives with RP values already rounded to 2 dp by
# build_threshold_table_df(); display keeps that precision.
#' @noRd
.wise_threshold_reactable <- function(df) {
  cols <- lapply(names(df), function(nm) {
    x <- df[[nm]]
    if (is.numeric(x)) {
      reactable::colDef(
        format = reactable::colFormat(digits = 2),
        class = "wise-dt-wrap"
      )
    } else if (is.character(x) || is.factor(x)) {
      reactable::colDef(class = "wise-dt-wrap", minWidth = 170)
    } else {
      reactable::colDef(class = "wise-dt-wrap", minWidth = 70)
    }
  })
  names(cols) <- names(df)
  reactable::reactable(
    df,
    columns = cols,
    compact = TRUE,
    searchable = FALSE,
    defaultPageSize = 10,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 25, 50, 100),
    highlight = TRUE
  )
}

#' Render the UI block for the combined Baseline + Policy results pane.
#'
#' Single-pane layout mirroring Step 2's question-based section card structure.
#' Outputs display baseline and policy series side-by-side with policy highlighted.
#' Inputs and outputs are namespaced via \code{ns()}.
#' @noRd
.results_pane_ui <- function(ns, so, weather_var = NULL) {
  so_name <- if (!is.null(so) && "name" %in% names(so) && !is.null(so[["name"]])) as.character(so[["name"]][1]) else "welfare"
  so_type <- if (!is.null(so) && "type" %in% names(so) && !is.null(so[["type"]])) as.character(so[["type"]][1]) else "numeric"
  so_label <- if (!is.null(so) && "label" %in% names(so) && !is.null(so[["label"]])) as.character(so[["label"]][1]) else so_name
  so_level <- if (!is.null(so) && "level" %in% names(so) && !is.null(so[["level"]])) as.character(so[["level"]][1]) else ""

  outcome_lbl <- tolower(so_label)
  unit_lbl <- switch(tolower(so_level),
    ind  = "individuals",
    firm = "firms",
    "households"
  )
  panel_title <- paste0("How to summarise ", outcome_lbl, " across ", unit_lbl, "?")
  wx_phrase <- format_weather_heading_phrase(weather_var)
  sec1_heading <- if (nzchar(wx_phrase)) {
    paste0("How does the policy shift ", outcome_lbl, " with ", wx_phrase, " across climate scenarios?")
  } else {
    paste0("How does the policy shift ", outcome_lbl, " across climate scenarios and weather years?")
  }

  agg_choices <- hist_aggregate_choices(so_type, so_name)

  pov_units <- if (!is.null(so) && "units" %in% names(so) && !is.null(so[["units"]]) && nzchar(as.character(so[["units"]][1]))) {
    as.character(so[["units"]][1])
  } else {
    "selected outcome units"
  }
  pov_val <- if (!is.null(so) && "povline" %in% names(so) && !is.null(so[["povline"]]) && is.finite(so[["povline"]][1]) && so[["povline"]][1] > 0) {
    so[["povline"]][1]
  } else {
    3.00
  }

  tagList(
    # 0. Stale banner (INT-08), policy summary, & headline cards ----
    shiny::uiOutput(ns("stale_banner_ui")),
    shiny::uiOutput(ns("policy_method_note")),
    shiny::uiOutput(ns("metric_context_note")),
    shiny::uiOutput(ns("policy_summary_ui")),

    # 1. Analysis controls: Aggregation method & poverty line ----
    shiny::div(
      class = "results-aggregation-panel",
      shiny::div(
        class = "results-aggregation-head",
        style = "margin-bottom: 8px;",
        shiny::h5(
          panel_title,
          info_popover(
            title = "Aggregation method",
            shiny::p(
              "Choose how household-level welfare (before and after policy) is",
              "aggregated into an annual population outcome for each simulated",
              "weather year and climate model.",
              "Poverty and prosperity metrics evaluate outcomes relative to the",
              "specified poverty line."
            )
          ),
          style = "font-size: 0.92rem; font-weight: 700; color: #173042; margin: 0;"
        )
      ),
      shiny::div(
        style = "display: flex; align-items: center; gap: 14px; flex-wrap: wrap;",
        pill_toggle(
          inputId  = ns("cmp_agg_method"),
          label    = NULL,
          choices  = agg_choices,
          selected = "mean",
          layout   = "horizontal"
        ),
        pill_toggle(
          inputId = ns("cmp_deviation"),
          label = NULL,
          choices = c(
            "Outcome level"                 = "none",
            "Change from historical mean"   = "mean",
            "Change from historical median" = "median"
          ),
          selected = "none",
          layout = "horizontal"
        ),
        shiny::conditionalPanel(
          condition = paste0(
            "['headcount_ratio','gap','fgt2']",
            ".indexOf(input['", ns("cmp_agg_method"), "']) > -1"
          ),
          shiny::div(
            style = "display: flex; align-items: center; gap: 6px;",
            shiny::tags$label(
              `for` = ns("cmp_pov_line"),
              style = "font-size: 0.8rem; font-weight: 600; color: #526575; margin: 0; white-space: nowrap;",
              paste0("Poverty line (", pov_units, "):")
            ),
            shiny::numericInput(
              ns("cmp_pov_line"),
              label = NULL,
              value = pov_val,
              min   = 0,
              step  = 0.5,
              width = "105px"
            )
          )
        )
      )
    ),

    # 2. Headline cards ----
    shiny::uiOutput(ns("headline_cards_ui")),

    # Section 1: Annual weather variation & policy shift ----
    shiny::h4(
      sec1_heading,
      info_popover(
        title = "Annual weather variation & policy shift",
        shiny::p(
          "Each dot represents the population aggregate outcome under one simulated",
          "weather year. The box and violin illustrate the full range of annual",
          "weather-year variation for the fixed population under baseline versus",
          "policy conditions."
        ),
        shiny::p(
          "Baseline is shown muted; policy is highlighted in the scenario colour.",
          "The dashed horizontal line marks the historical baseline mean."
        ),
        docs = TRUE
      ),
      style = "font-size: 1.05rem; font-weight: 700; color: #173042; margin-top: 24px; margin-bottom: 8px;"
    ),
    shiny::div(
      class = "results-section-card",
      shiny::div(
        style = "display: flex; justify-content: flex-end; align-items: center; margin-bottom: 8px;",
        pill_toggle(
          ns("annual_distribution_type"),
          label    = NULL,
          choices  = c("Violin" = "violin", "Boxplot" = "boxplot"),
          selected = "violin",
          layout   = "horizontal"
        )
      ),
      wise_chart_output(
        ns("annual_distribution_plot"),
        "Distribution of annual aggregates across simulated weather years: baseline and policy",
        height = "470px"
      ),
      shiny::tags$p(
        class = "text-muted small",
        style = "margin-top: 18px; margin-bottom: 0;",
        "Each dot is one simulated weather-year annual aggregate for the fixed population. The selected violin or boxplot summarizes the distribution; dodged pairs contrast baseline (muted, upper) with policy (highlighted, lower), and circle markers mark series means."
      )
    ),

    # Section 2: Adverse weather years (tail protection) ----
    shiny::h4(
      "Does the policy protect against adverse weather years?",
      info_popover(
        title = "Adverse weather-year protection",
        shiny::p(
          "Adverse return-period outcomes represent severe annual weather conditions.",
          "An adverse 1-in-10-year outcome is reached or exceeded in the unfavorable",
          "direction in approximately one out of ten simulated weather years."
        ),
        shiny::p(
          "Dumbbell points connect baseline (open circle) to policy (filled circle).",
          "Horizontal bars show climate-model ensemble spread under the policy."
        ),
        docs = TRUE
      ),
      style = "font-size: 1.05rem; font-weight: 700; color: #173042; margin-top: 24px; margin-bottom: 8px;"
    ),
    shiny::div(
      class = "results-section-card",
      shiny::div(
        style = "display: flex; justify-content: flex-end; align-items: center; margin-bottom: 8px;",
        pill_toggle(
          ns("ensemble_band"),
          label = "Climate model spread",
          choices = c(
            "None"                 = "none",
            "Full ensemble spread" = "minmax",
            "95%"                  = "p025_p975",
            "90%"                  = "p05_p95",
            "80%"                  = "p10_p90"
          ),
          selected = "none",
          layout = "horizontal"
        )
      ),
      wise_chart_output(
        ns("adverse_dot_plot"),
        "Expected and adverse-year outcomes: baseline and policy with model ensemble spread",
        height = "380px"
      ),
      shiny::tags$p(
        class = "text-muted small",
        style = "margin-top: 8px; margin-bottom: 0;",
        "Open circles = baseline (no policy); red filled circles = policy. Connecting lines show the policy buffer. Horizontal intervals show the selected climate-model spread under the policy."
      )
    ),

    # Section 3: Exceedance probability curves ----
    shiny::h4(
      "How does the policy change the probability of severe outcomes?",
      info_popover(
        title = "Exceedance probability",
        shiny::p(
          "Shows the annual probability of reaching or exceeding severe outcome",
          "thresholds across simulated weather years under baseline and policy.",
          "Baseline and policy share each scenario's colour, period linetype, and line width.",
          "Open endpoint circles mark baseline; filled vermillion circles mark policy."
        ),
        docs = TRUE
      ),
      style = "font-size: 1.05rem; font-weight: 700; color: #173042; margin-top: 24px; margin-bottom: 8px;"
    ),
    shiny::div(
      class = "results-section-card",
      shiny::div(
        style = "display: flex; justify-content: flex-end; align-items: center; margin-bottom: 8px;",
        pill_toggle(
          inputId = ns("exceedance_model_spread"),
          label = "Climate model spread",
          choices = c(
            "None"                 = "none",
            "Full ensemble spread" = "minmax",
            "95%"                  = "p025_p975",
            "90%"                  = "p05_p95",
            "80%"                  = "p10_p90"
          ),
          selected = "none",
          layout = "horizontal"
        )
      ),
      wise_chart_output(
        ns("exceedance_plot"),
        "Exceedance probability curves: baseline and policy across climate scenarios",
        height = "400px"
      ),
      shiny::tags$p(
        class = "text-muted small",
        style = "margin-top: 8px; margin-bottom: 0;",
        "Read each curve as the annual probability of reaching an outcome level in the adverse direction. Colour identifies the SSP family and line type identifies the projection period. Baseline and policy share the same scenario line; open endpoint circles mark baseline and filled vermillion circles mark policy. Shaded ribbons show selected climate-model disagreement. Return-period ticks are limited to the available simulated years per climate model; unsupported periods are not extrapolated."
      )
    ),

    # Section 4: Decision & return-period table ----
    shiny::h4(
      "Detailed baseline, policy, and return-period outcomes",
      info_popover(
        title = "Policy decision table",
        shiny::p(
          "Comprehensive summary of central expected and adverse return-period outcomes for",
          "both baseline and policy, with climate model and econometric uncertainty bounds."
        ),
        docs = TRUE
      ),
      style = "font-size: 1.05rem; font-weight: 700; color: #173042; margin-top: 24px; margin-bottom: 8px;"
    ),
    shiny::div(
      class = "results-section-card",
      shiny::div(
        style = "display: flex; justify-content: flex-end; align-items: center; margin-bottom: 8px;",
        wise_reactable_csv_button(ns("summary_threshold_table"), "policy_outcome_thresholds")
      ),
      reactable::reactableOutput(ns("summary_threshold_table")),
      shiny::tags$p(
        class = "text-muted small",
        style = "margin-top: 8px; margin-bottom: 0;",
        "Central estimates summarize each model's weather years at the selected return period, then take the median across climate models for baseline and policy; distinct from the equal-model-mean expected-effect headline. Bounds capture CMIP6 climate model disagreement (Ensemble), econometric sampling precision (Coef), and combined uncertainty (Pooled)."
      )
    ),
  )
}

#' Wire reactives and output bindings for the combined results pane.
#'
#' Takes both baseline and policy reactives and renders one pane that
#' compares them side-by-side. Controls (aggregation method, deviation,
#' weights, scenario filter) drive both sources jointly.
#' @noRd
.wire_results_pane <- function(input, output, session,
                               baseline_hist_sim,
                               baseline_saved_scenarios,
                               policy_hist_sim,
                               policy_saved_scenarios,
                               selected_hist,
                               selected_policies = reactive(NULL),
                               policy_scenarios = reactive(list()),
                               sp_scenario = reactive(NULL),
                               infra_scenario = reactive(NULL),
                               digital_scenario = reactive(NULL),
                               labor_scenario = reactive(NULL),
                               education_scenario = reactive(NULL),
                               residuals = reactive("original"),
                               stale = reactive(FALSE),
                               decomp_scenarios = reactive(list()),
                               decomp_context = reactive(NULL),
                               baseline_svy = reactive(NULL),
                                 policy_svy = reactive(NULL),
                                 aggregation_cache = NULL,
                                 analysis_unit = reactive(NULL),
                                 annual_channels = reactive(NULL)) {
  ns <- session$ns

  aggregation_method <- reactive({
    hs <- baseline_hist_sim()
    choices <- unname(hist_aggregate_choices(hs$so$type, hs$so$name))
    selected <- input$cmp_agg_method %||% "mean"
    if (selected %in% choices) selected else choices[[1L]]
  })
  policy_endpoint_status <- reactive({
    .policy_endpoint_status(baseline_hist_sim()$so, decomp_context())
  })
  output$policy_method_note <- shiny::renderUI({
    status <- policy_endpoint_status()
    if (!identical(status$status, "ok")) {
      shiny::div(class = "alert alert-warning", role = "alert", status$reason)
    }
  })

  # INT-08: stale banner above the results pane. This surface gates its
  # CSV export while stale.
  output$stale_banner_ui <- shiny::renderUI({
    if (isTRUE(stale())) {
      .stale_banner(
        "Step 3 policy results",
        note = NULL
      )
    } else {
      NULL
    }
  })

  output$policy_summary_ui <- shiny::renderUI({
    bh <- baseline_hist_sim()
    req(bh)
    policy_summary_card(
      selected_policies = selected_policies(),
      baseline_hist_sim = bh,
      policy_saved_scenarios = policy_saved_scenarios(),
      selected_weather = bh$sim_summary$weather %||% NULL,
      sp_scenario = sp_scenario(),
      infra_scenario = infra_scenario(),
      digital_scenario = digital_scenario(),
      labor_scenario = labor_scenario(),
      education_scenario = education_scenario(),
      policy_scenarios = policy_scenarios()
    )
  })

  headline_cards_data_rv <- reactive({
    if (isTRUE(stale())) return(NULL)
    summary <- headline_paired_effect_summary_rv()
    if (!nrow(summary) && !identical(policy_endpoint_status()$status, "ok")) {
      summary <- tibble::tibble(scenario = focus_scenario(), value = NA_real_,
        intermod_lo = NA_real_, intermod_hi = NA_real_, n_models = NA_integer_)
    }
    req(summary)
    step3_headline_cards(
      paired_summary    = summary,
      threshold_tbl     = threshold_table_rv(),
      baseline_agg      = baseline_agg_scenarios(),
      policy_agg        = policy_agg_scenarios(),
      metric_decomposition = metric_decomposition(),
      policy_svy        = policy_svy(),
      sp_scenario       = sp_scenario(),
      timeseries_curves = timeseries_curves_rv(),
      method            = aggregation_method(),
      so                = baseline_hist_sim()$so,
      baseline_svy      = baseline_svy(),
      analysis_unit     = analysis_unit(),
      metric_context    = metric_context(),
      endpoint_status   = policy_endpoint_status()
    )
  })

  output$headline_cards_ui <- shiny::renderUI({
    req(headline_cards_data_rv())
    headline_cards_ui(headline_cards_data_rv())
  })

  wise_export_table(
    key = "policy_headline_summary",
    label = "Policy headline summary cards",
    step = 3L,
    fun = function() {
      df <- step3_headline_df(headline_cards_data_rv())
      summary <- headline_paired_effect_summary_rv()
      focus <- if (nrow(summary)) {
        summary[summary$scenario == focus_scenario(), , drop = FALSE]
      } else summary
      df$baseline_native <- df$policy_native <- df$effect_native <- NA_real_
      if (nrow(focus)) {
        df$baseline_native[1L] <- focus$baseline[[1L]]
        df$policy_native[1L] <- focus$policy[[1L]]
        df$effect_native[1L] <- focus$value[[1L]]
      }
      df$scenario <- focus_scenario()
      df$center_method <- ifelse(df$card == 1L, "equal_model_mean",
        ifelse(df$card == 2L, "equal_model_mean", "not_applicable"))
      df$availability <- policy_endpoint_status()$status
      df$reason <- policy_endpoint_status()$reason %||% ""
      annotate_visualization_export(df, aggregation_method(), baseline_hist_sim()$so,
        observation_unit = "headline for fixed survey population",
        aggregation_order = "matched years averaged within model; equal-model mean for expected effect",
        uncertainty = "central headline; component uncertainty not estimated",
        context = metric_context())
    },
    description = paste(
      "Expected effect and endpoint levels: years averaged within model, then equal-model mean on matched support.",
        "Adverse effects use the baseline selected-metric quantile and reuse interpolated year support for policy;",
        "technical resilience channels remain on model scale and are reported separately."
    )
  )

  # Sync aggregation method choices when simulation changes
  observeEvent(baseline_hist_sim(), {
    hs <- baseline_hist_sim()
    if (is.null(hs) || is.null(hs$so)) {
      return()
    }
    agg_choices <- hist_aggregate_choices(hs$so$type, hs$so$name)
    cur_method <- isolate(input$cmp_agg_method) %||% "mean"
    if (!cur_method %in% agg_choices) cur_method <- agg_choices[[1L]]
    shiny::updateRadioButtons(session, "cmp_agg_method",
      choices = agg_choices,
      selected = cur_method, inline = TRUE
    )
  })

  # Resolve the residuals choice captured by the Step 2 run. The live control
  # is only a fallback for older in-memory result objects.
  active_residuals <- function(hs) {
    hs$residuals %||% residuals() %||% "original"
  }

  # INT-05: prefer the historical label captured by the Step 2 run; the live
  # selection is only a fallback for older in-memory result objects.
  hist_label <- reactive({
    hs <- baseline_hist_sim()
    nm <- hs$hist_label %||%
      (if (!is.null(selected_hist)) selected_hist()$scenario_name else NULL)
    if (!is.null(nm) && nzchar(nm)) nm else "Historical"
  })

  # Sync the static poverty-line input to the run's value while the user has
  # not edited it (INT-01: once edited, the user's value survives re-runs).
  # "Edited" means the input differs from the last-synced value, so the
  # sync's own updateNumericInput round-trip never counts as an edit.
  pov_line_touched <- reactiveVal(FALSE)
  .pov_line_last_sync <- reactiveVal(3.00)
  observeEvent(input$cmp_pov_line,
    {
      v <- suppressWarnings(as.numeric(input$cmp_pov_line)[1])
      if (!identical(v, .pov_line_last_sync())) pov_line_touched(TRUE)
    },
    ignoreInit = TRUE
  )
  observeEvent(baseline_hist_sim(), {
    hs <- baseline_hist_sim()
    if (is.null(hs)) {
      return()
    }
    v <- hs$pov_line %||% 3.00
    .pov_line_last_sync(v)
    if (pov_line_touched()) {
      return()
    }
    shiny::updateNumericInput(session, "cmp_pov_line", value = v)
  })

  poverty_methods <- c("headcount_ratio", "gap", "fgt2")
  valid_pov_line <- function(x) {
    x <- suppressWarnings(as.numeric(x)[1L])
    if (length(x) && is.finite(x) && x > 0) x else NULL
  }

  # Debounce only edits to the numeric value, never the aggregation method.
  # Debouncing both together left a 400 ms window where a newly selected
  # poverty method was paired with the previous method's NULL poverty line.
  debounced_pov_line_input <- shiny::debounce(reactive({
    valid_pov_line(input$cmp_pov_line)
  }), 400)

  pov_line_val <- reactive({
    method <- aggregation_method()
    if (!method %in% poverty_methods) {
      return(NULL)
    }

    if (pov_line_touched()) {
      edited <- debounced_pov_line_input()
      if (!is.null(edited)) {
        return(edited)
      }
    }

    valid_pov_line(baseline_hist_sim()$pov_line) %||%
      valid_pov_line(.pov_line_last_sync()) %||% 3.00
  })

  # Scenario selection is intentionally not exposed in Module 3 Results.
  selected_scenario_names <- reactive({
    sc <- baseline_saved_scenarios()
    if (length(sc) == 0) {
      return(character(0))
    }
    names(sc)
  })

  focus_scenario <- reactive({
    sc <- selected_scenario_names()
    if (length(sc)) sc[[1L]] else hist_label()
  })
  metric_context <- reactive({
    hs <- baseline_hist_sim()
    spec <- metric_metadata(aggregation_method(), hs$so,
      pov_line = pov_line_val(), analysis_unit = analysis_unit(),
      weighted = !is.null(hs$pipeline$weight))
    spec$scenario <- focus_scenario()
    spec$run_identity <- if (!is.null(decomp_context())) decomp_context()$run_identity else NULL
    spec$requested_residuals <- active_residuals(hs)
    spec$availability <- if (isTRUE(stale())) "unavailable" else policy_endpoint_status()$status
    spec$reason <- if (isTRUE(stale())) "Policy run is stale." else policy_endpoint_status()$reason
    spec
  })
  output$metric_context_note <- shiny::renderUI({
    shiny::tags$p(class = "diagnostic-note", metric_context_note(metric_context()))
  })

  # PERF-31: per-method aggregation cache ----
  # Aggregating baseline/policy hist + every scenario member is expensive and
  # depends only on (source, aggregation method, poverty line). `cmp_deviation`
  # is applied downstream (hist_ref subtraction in the row builders + axis
  # labels), so it must NOT be part of the key - moving the deviation control
  # used to destroy the entire cache and re-aggregate everything.
  #
  # Invalidation: a fresh cache environment is created whenever any underlying
  # simulation object changes (publishes are atomic - INT-09/REACT-12), so
  # stale entries can never be served. Residual mode is part of the source
  # identity (it is snapshotted per run on the sim objects themselves).
  agg_cache_ws <- reactive({
    baseline_hist_sim()
    policy_hist_sim()
    baseline_saved_scenarios()
    policy_saved_scenarios()
    ws <- new.env(parent = emptyenv())
    attr(ws, "keys") <- character(0)
    attr(ws, "max_entries") <- 32L
    suite_cache <- new.env(parent = emptyenv())
    attr(suite_cache, "keys") <- character(0)
    attr(suite_cache, "max_entries") <- 8L
    attr(ws, "suite_cache") <- suite_cache
    ws
  })
  .agg_cache_key <- function(tag, method, pov_line) {
    paste(tag, method, format(pov_line), sep = "\r")
  }
  .agg_suite_methods <- function() {
    c("mean", "median", "total", "headcount_ratio", "gap", "fgt2",
      "gini", "prosperity_gap", "avg_poverty")
  }
  .agg_cache_get <- function(ws, key) {
    hit <- get0(key, envir = ws)
    if (!is.null(hit)) attr(ws, "keys") <- c(setdiff(attr(ws, "keys"), key), key)
    hit
  }
  .agg_cache_put <- function(ws, key, value) {
    assign(key, value, envir = ws)
    attr(ws, "keys") <- c(setdiff(attr(ws, "keys"), key), key)
    while (length(attr(ws, "keys")) > attr(ws, "max_entries")) {
      evict <- attr(ws, "keys")[[1L]]
      attr(ws, "keys") <- attr(ws, "keys")[-1L]
      if (exists(evict, envir = ws, inherits = FALSE)) rm(list = evict, envir = ws)
    }
    invisible(value)
  }

  # The exceedance chart's outcome-axis label now comes from
  # metric_axis_label() at each call site, matching the other charts.

  # Helper: aggregate hist_sim into Mod 2's rich list-col schema
  # (one row per sim_year, list-cols value_all / value_all_sd / model_id,
  # plus scalar var_within / var_across). This lets us reuse Mod 2's
  # by_model_matrix() + downstream plot helpers verbatim.
  #
  # Both baseline (Mod 2 hist_sim, passed verbatim) and policy (re-simulated
  # by resimulate_with_svy) wrap their single historical run under $pipeline
  # - read it once here so the downstream code paths are identical.
  make_agg_hist <- function(hs, tag) {
    if (is.null(hs)) {
      return(NULL)
    }
    pl <- hs$pipeline
    if (is.null(pl) || is.null(pl$y_point)) {
      return(NULL)
    }
    method <- aggregation_method()
    poverty_line <- pov_line_val()
    if (method %in% poverty_methods && is.null(poverty_line)) poverty_line <- 3.00

    ws <- agg_cache_ws()
    cache_key <- .agg_cache_key(tag, method, poverty_line)
    hit <- .agg_cache_get(ws, cache_key)
    if (!is.null(hit)) {
      return(hit)
    }

    suite_key <- .agg_cache_key(tag, "__suite__", poverty_line)
    suite_cache <- attr(ws, "suite_cache")
    suite <- .agg_cache_get(suite_cache, suite_key)
    if (!is.null(suite)) {
      hit <- list(out = suite[[method]])
      .agg_cache_put(ws, cache_key, hit)
      return(hit)
    }

    baseline_skip_coef <- is.null(pl$F_loading)
    suite_pov <- poverty_line %||% 3
    shared_key <- shared_aggregation_cache_key(
      list(
        arm = tag,
        signature = hs$.step2_sig %||% hs$.sig %||% list(pipeline = "step2")
      ),
      suite_pov, 0.05, TRUE, active_residuals(hs), baseline_skip_coef,
      isTRUE(hs$so$transform == "log"), .agg_suite_methods()
    )
    shared <- shared_aggregation_cache_get(aggregation_cache, shared_key)
    if (!is.null(shared)) {
      .agg_cache_put(suite_cache, suite_key, shared)
      hit <- list(out = shared[[method]])
      .agg_cache_put(ws, cache_key, hit)
      return(hit)
    }

    agg <- aggregate_pipeline_tables_multi(
      pipelines = pl,
      methods = .agg_suite_methods(),
      weighted = TRUE,
       pov_lines = setNames(lapply(.agg_suite_methods(), function(x) suite_pov),
                           .agg_suite_methods()),
      residuals = active_residuals(hs),
      is_log = isTRUE(hs$so$transform == "log"),
      band_q = c(lo = 0.10, hi = 0.90),
      skip_coef = baseline_skip_coef,
      model_ids = "Historical",
      scenario = "Historical",
      shared_context = hs$shared_context
    )
    .agg_cache_put(suite_cache, suite_key, agg)
    shared_aggregation_cache_put(aggregation_cache, shared_key, agg)
    .agg_cache_put(ws, cache_key, list(out = agg[[method]]))
    list(out = agg[[method]])
  }

  # Helper: build agg per saved scenario in Mod 2 schema. Each `s$pipelines`
  # entry is one CMIP6 ensemble member with its own y_point / F_loading.
  # Mod 2's run_full_simulation() and Mod 3's resimulate_with_svy() both
  # populate $pipelines, so this reader works for baseline and policy alike.
  #
  # INT-04: scenario failures are collected (not silently dropped) and
  # surfaced once per distinct failure set via a persistent warning toast.
  .agg_failure_state <- new.env(parent = emptyenv())
  .agg_failure_state$last_key <- NULL

  .notify_agg_failures <- function(failed_names, n_total) {
    if (length(failed_names) == 0L) {
      .agg_failure_state$last_key <- NULL
      return(invisible(NULL))
    }
    key <- paste(sort(failed_names), collapse = "\r")
    if (identical(key, .agg_failure_state$last_key)) {
      return(invisible(NULL))
    }
    .agg_failure_state$last_key <- key
    shiny::showNotification(
      ui = shiny::tagList(
        shiny::strong(sprintf(
          "%d of %d scenario%s could not be aggregated:",
          length(failed_names), n_total, if (length(failed_names) == 1L) "" else "s"
        )),
        shiny::br(),
        paste(failed_names, collapse = ", ")
      ),
      type = "warning", duration = NULL, session = session
    )
  }

  make_agg_scenarios <- function(sc, hs_for_dev, tag) {
    if (length(sc) == 0) {
      return(list())
    }
    method <- aggregation_method()
    poverty_line <- pov_line_val()
    if (method %in% poverty_methods && is.null(poverty_line)) poverty_line <- 3.00
    use_w <- TRUE

    ws <- agg_cache_ws()
    cache_key <- .agg_cache_key(tag, method, poverty_line)
    hit <- .agg_cache_get(ws, cache_key)
    if (!is.null(hit)) {
      return(hit)
    }

    suite_key <- .agg_cache_key(tag, "__suite__", poverty_line)
    suite_cache <- attr(ws, "suite_cache")
    suite <- .agg_cache_get(suite_cache, suite_key)
    if (!is.null(suite)) {
      # Unlike the historical suite, future suites are keyed by scenario first.
      hit <- lapply(suite, function(value) {
        if (is.null(value)) return(NULL)
        list(out = value[[method]])
      })
      .agg_cache_put(ws, cache_key, hit)
      return(hit)
    }

    failed <- character(0)
    # NB: iterate by index (the error handler needs `names(sc)[i]`) but
    # re-attach the scenario names - every consumer below (all_series,
    # pointrange/timeseries/exceedance/threshold row builders) selects
    # scenarios by name, and lapply(seq_along(...)) drops them.
    res <- stats::setNames(lapply(seq_along(sc), function(i) {
      s <- sc[[i]]
      tryCatch(
        {
          pipes <- s$pipelines
          if (is.null(pipes) || length(pipes) == 0L) {
            return(NULL)
          }
           combined <- aggregate_pipeline_tables_multi(
             pipelines = pipes,
             methods = .agg_suite_methods(),
             weighted = use_w,
             pov_lines = setNames(lapply(.agg_suite_methods(), function(x) poverty_line %||% 3),
                                  .agg_suite_methods()),
            residuals = active_residuals(hs_for_dev),
            is_log = isTRUE(s$so$transform == "log"),
            band_q = c(lo = 0.10, hi = 0.90),
            model_ids = names(pipes),
            shared_context = s$shared_context
          )
           if (!length(combined) || !any(vapply(combined, function(x) {
             !is.null(x) && nrow(x) > 0L
           }, logical(1L)))) {
             return(NULL)
           }
           list(out = combined)
        },
        error = function(e) {
          nm <- s$scenario_name %||% names(sc)[i]
          if (is.null(nm) || is.na(nm)) nm <- paste0("scenario_", i)
          failed[[length(failed) + 1L]] <<- nm
          NULL
        }
      )
    }), names(sc))
    .notify_agg_failures(failed, length(sc))
    suite <- lapply(res, function(value) {
      if (is.null(value)) return(NULL)
      value$out
    })
    .agg_cache_put(suite_cache, suite_key, suite)
    selected <- lapply(suite, function(value) {
      if (is.null(value)) return(NULL)
      list(out = value[[method]])
    }) |> stats::setNames(names(suite))
    .agg_cache_put(ws, cache_key, selected)
    selected
  }

  baseline_agg_hist <- reactive({
    req(baseline_hist_sim())
    make_agg_hist(baseline_hist_sim(), "baseline_hist")
  })
  policy_agg_hist <- reactive({
    if (!identical(policy_endpoint_status()$status, "ok")) return(NULL)
    req(policy_hist_sim())
    make_agg_hist(policy_hist_sim(), "policy_hist")
  })

  baseline_agg_scenarios <- reactive({
    req(baseline_hist_sim())
    make_agg_scenarios(
      baseline_saved_scenarios(), baseline_hist_sim(),
      "baseline_scn"
    )
  })
  policy_agg_scenarios <- reactive({
    if (!identical(policy_endpoint_status()$status, "ok")) return(list())
    req(policy_hist_sim())
    make_agg_scenarios(
      policy_saved_scenarios(), policy_hist_sim(),
      "policy_scn"
    )
  })

  historical_matrix_key <- "Historical"

  baseline_all_series <- reactive({
    sc <- baseline_agg_scenarios()
    sel <- selected_scenario_names()
    c(
      setNames(list(baseline_agg_hist()), hist_label()),
      sc[intersect(sel, names(sc))]
    )
  })
  policy_all_series <- reactive({
    sc <- policy_agg_scenarios()
    sel <- selected_scenario_names()
    c(
      setNames(list(policy_agg_hist()), hist_label()),
      sc[intersect(sel, names(sc))]
    )
  })

  metric_decomposition <- reactive({
    baseline_hist <- baseline_hist_sim()
    policy_hist <- policy_hist_sim()
    prepared <- annual_channels()
    context <- decomp_context()
    run_identity <- if (is.environment(prepared)) prepared$run_identity else NULL
    endpoint_baseline <- baseline_all_series()
    endpoint_policy <- policy_all_series()
    method <- aggregation_method()
    pov_line <- pov_line_val()
    requested_residuals <- active_residuals(baseline_hist)
    focus <- focus_scenario()
    unit <- analysis_unit()

    unavailable <- function(result, reason) {
      result$status <- "unavailable"
      result$reason <- reason
      result$annual <- data.frame()
      result$summary <- data.frame()
      result$return_period <- data.frame()
      result$adverse_support <- data.frame()
      result$adverse_by_model <- data.frame()
      result$mechanisms <- list()
      result$scenarios <- list()
      result
    }
    fallback <- list(
      status = "unavailable", reason = "Metric decomposition is unavailable.",
      annual = data.frame(), summary = data.frame(), return_period = data.frame(),
      adverse_support = data.frame(), adverse_by_model = data.frame(),
      mechanisms = list(), endpoint_summary = NULL, scenarios = list(),
      metadata = list()
    )
    calculate <- function(source) {
      .policy_metric_decomposition(
        baseline_hist, policy_hist,
        baseline_saved_scenarios(), policy_saved_scenarios(),
        source, method, pov_line, requested_residuals,
        endpoint_baseline, endpoint_policy, focus, unit
      )
    }
    context_error <- NULL
    if (!is.null(prepared)) {
      context_error <- tryCatch({
        if (!is.environment(prepared) || is.null(run_identity)) {
          stop("Prepared annual channels are missing a run identity.", call. = FALSE)
        }
        .validate_run_decomposition_context(prepared$context, run_identity)
        if (is.null(context) || !identical(context$run_identity, run_identity)) {
          stop("Results and prepared channel run identities differ.", call. = FALSE)
        }
        .validate_run_decomposition_context(context, run_identity)
        NULL
      }, error = function(e) conditionMessage(e))
    }
    source <- if (is.null(context_error) && !isTRUE(stale())) prepared else NULL
    result <- tryCatch(calculate(source), error = function(e) {
      # The helper still owns endpoint summarization when channel inputs are
      # missing or invalid. A second call with no prepared source must not
      # expose partial/stale channel values from the failed calculation.
      tryCatch(calculate(NULL), error = function(endpoint_error) {
        fallback$reason <<- conditionMessage(e)
        fallback
      })
    })
    if (is.null(result) || !is.list(result)) result <- fallback

    if (isTRUE(stale())) {
      return(unavailable(result, "Policy run is stale."))
    }
    endpoint_status <- policy_endpoint_status()
    if (!identical(endpoint_status$status, "ok")) {
      result <- unavailable(result, endpoint_status$reason)
      result$endpoint_summary <- data.frame()
      result$status <- endpoint_status$status
      return(result)
    }
    if (!is.null(context_error)) {
      return(unavailable(result, context_error))
    }
    result
  })

  matrix_transforms_rv <- reactive({
    out <- list()
    add_scenarios <- function(series, source) {
      for (nm in names(series)) {
        out[[paste(source, nm, sep = "\r")]] <<- by_model_matrix(series[[nm]]$out)
      }
    }
    add_hist <- function(aggregate, source) {
      out[[paste(source, historical_matrix_key, sep = "\r")]] <<- by_model_matrix(aggregate$out)
    }
    add_hist(baseline_agg_hist(), "Baseline")
    add_scenarios(baseline_agg_scenarios(), "Baseline")
    add_hist(policy_agg_hist(), "Policy")
    add_scenarios(policy_agg_scenarios(), "Policy")
    out
  })
  matrix_transform <- function(tbl, source, scenario) {
    cached <- matrix_transforms_rv()[[paste(source, scenario, sep = "\r")]]
    if (identical(scenario, historical_matrix_key)) {
      return(cached)
    }
    cached %||% by_model_matrix(tbl)
  }

  # Canonical paired policy-minus-baseline summaries. Arms are aligned at the
  # model/year aggregate level and coefficient gradients are contrasted before
  # uncertainty is calculated, preserving baseline-policy covariance.
  paired_effect_data <- reactive({
    if (!identical(policy_endpoint_status()$status, "ok")) return(list())
    b <- baseline_all_series()
    p <- policy_all_series()
    if (!length(b) || !length(p)) {
      return(list())
    }
    common <- intersect(names(b), names(p))
    stats::setNames(lapply(common, function(nm) {
      paired_model_year_effects(b[[nm]]$out, p[[nm]]$out) |>
        dplyr::mutate(scenario = nm)
    }), common)
  })

  paired_effect_summary_rv <- reactive({
    dat <- paired_effect_data()
    if (!length(dat)) {
      return(tibble::tibble())
    }
    bq <- if (identical(input$ensemble_band %||% "none", "none")) {
      c(lo = 0.5, hi = 0.5)
    } else {
      resolve_band_q(input$ensemble_band %||% "none")
    }
    dplyr::bind_rows(lapply(names(dat), function(nm) {
      paired_effect_summary(dat[[nm]], band_q = bq, scenario = nm,
        center = "equal_model_mean")
    }))
  })

  # Headline robustness is a factual model range, independent of the
  # ensemble-spread setting used to tune individual plots.
  headline_paired_effect_summary_rv <- reactive({
    dat <- paired_effect_data()
    if (!length(dat)) return(tibble::tibble())
    dplyr::bind_rows(lapply(names(dat), function(nm) {
      paired_effect_summary(dat[[nm]], band_q = c(lo = 0, hi = 1), scenario = nm,
        center = "equal_model_mean")
    }))
  })

  headline_adverse_summary_rv <- reactive({
    tails <- metric_decomposition()$return_period
    if (!is.data.frame(tails) || !nrow(tails)) return(tibble::tibble())
    out <- dplyr::bind_rows(lapply(unique(tails$scenario), function(nm) {
      rows <- tails[tails$scenario == nm & tails$scope == "baseline_anchored" &
        tails$return_period == 20, , drop = FALSE]
      if (!nrow(rows)) return(NULL)
      tibble::tibble(scenario = nm, baseline = rows$baseline[[1L]],
        policy = rows$policy[[1L]], value = rows$total[[1L]],
        intermod_lo = NA_real_, intermod_hi = NA_real_, n_models = rows$n_models[[1L]],
        n_years = rows$n_model_years[[1L]], center_method = rows$center_method[[1L]],
        status = rows$status[[1L]], reason = rows$reason[[1L]])
    }))
    if (!nrow(out)) return(out)
    limits <- paired_effect_summary_rv()
    if (is.data.frame(limits) && nrow(limits)) {
      for (i in seq_len(nrow(out))) {
        row <- limits[limits$scenario == out$scenario[[i]], , drop = FALSE]
        if (nrow(row)) {
          out$intermod_lo[[i]] <- row$intermod_lo[[1L]]
          out$intermod_hi[[i]] <- row$intermod_hi[[1L]]
        }
      }
    }
    out
  })

  paired_annual_effects_rv <- reactive({
    dat <- paired_effect_data()
    if (!length(dat)) {
      return(tibble::tibble())
    }
    dplyr::bind_rows(lapply(names(dat), function(nm) {
      x <- dat[[nm]]
      if (is.null(x) || !nrow(x)) {
        return(NULL)
      }
      x[, c("scenario", "sim_year", "model_id", "effect", "effect_sd")]
    })) |>
      dplyr::rename(value = effect)
  })

  paired_adverse_effects_rv <- reactive({
    tbl <- paired_adverse_table_rv()
    if (!"period" %in% names(tbl)) return(tibble::tibble())
    tbl <- tbl[tbl$period != "Expected", , drop = FALSE]
    if (!nrow(tbl)) {
      return(tibble::tibble())
    }
    dplyr::transmute(
      tbl,
      scenario = .data$scenario, tail = .data$period,
      effect = .data$effect, lo = .data$ensemble_lo,
      hi = .data$ensemble_hi
    )
  })

  paired_adverse_table_rv <- reactive({
    tails <- metric_decomposition()$return_period
    if (is.data.frame(tails) && nrow(tails)) {
      return(dplyr::bind_rows(lapply(unique(tails$scenario), function(nm) {
      x <- tails[tails$scenario == nm & tails$scope == "baseline_anchored" &
        tails$return_period %in% c(5, 10, 20, 50), , drop = FALSE]
        if (!nrow(x)) return(NULL)
        dplyr::transmute(x,
          period = paste0("Adverse 1-in-", round(.data$return_period)),
          baseline = .data$baseline, policy = .data$policy, effect = .data$total,
          ensemble_lo = NA_real_, ensemble_hi = NA_real_, scenario = .data$scenario,
          center_method = .data$center_method, scope = .data$scope,
          adverse_basis = .data$adverse_basis, status = .data$status, reason = .data$reason)
      })))
    }
    if (!is.data.frame(tails) || !nrow(tails)) return(tibble::tibble(period = character(),
      baseline = numeric(), policy = numeric(), effect = numeric(), ensemble_lo = numeric(),
      ensemble_hi = numeric(), scenario = character(), center_method = character(),
      scope = character(), adverse_basis = character(), status = character(), reason = character()))
    tibble::tibble(period = character(), baseline = numeric(), policy = numeric(), effect = numeric(),
      ensemble_lo = numeric(), ensemble_hi = numeric(), scenario = character(),
      center_method = character(), scope = character(), adverse_basis = character(),
      status = character(), reason = character())
  })

  # Shared deviation reference (baseline historical) ----
  hist_ref_val <- reactive({
    req(baseline_agg_hist())
    deviation <- input$cmp_deviation %||% "none"
    raw_vals <- baseline_agg_hist()$out$value
    if (identical(deviation, "mean")) {
      mean(raw_vals, na.rm = TRUE)
    } else if (identical(deviation, "median")) {
      stats::median(raw_vals, na.rm = TRUE)
    } else {
      0
    }
  })

  has_draws <- reactive({
    bh <- baseline_hist_sim()
    ph <- policy_hist_sim()
    # Mod 2 schema: F_loading lives on $pipeline; check there first and fall
    # back to top-level for any caller still on the older flat shape.
    isTRUE(
      !is.null(bh$pipeline$F_loading) || !is.null(bh$F_loading) ||
        !is.null(ph$pipeline$F_loading) || !is.null(ph$F_loading)
    )
  })

  # Per-source helpers that mirror Mod 2's reactive trio ----
  # Each takes the per-source aggregate (Mod 2 list-col tibble) and emits
  # the same long-format pointrange / timeseries / exceedance / threshold
  # rows Mod 2's plotters consume, tagged with a `source` column.
  .build_timeseries_rows <- function(agg_hist, agg_scn, hist_ref, source_label) {
    one <- function(tbl, scenario_label, is_hist) {
      if (is.null(tbl) || nrow(tbl) == 0L) {
        return(NULL)
      }
      mm <- matrix_transform(
        tbl, source_label,
        if (is_hist) historical_matrix_key else scenario_label
      )
      if (is.null(mm)) {
        return(NULL)
      }
      vals <- mm$vals
      dplyr::bind_rows(lapply(seq_len(nrow(vals)), function(i) {
        tibble::tibble(
          scenario      = scenario_label,
          source        = source_label,
          model_id      = mm$model_ids[[i]],
          sim_year      = as.integer(mm$sim_years),
          value         = vals[i, ] - hist_ref,
          is_historical = is_hist
        )
      }))
    }
    rows <- list(one(agg_hist$out, "Historical", TRUE))
    if (!is.null(agg_scn)) {
      for (dk in names(agg_scn)) {
        if (!dk %in% selected_scenario_names()) next
        rows[[length(rows) + 1L]] <- one(agg_scn[[dk]]$out, dk, FALSE)
      }
    }
    dplyr::bind_rows(Filter(Negate(is.null), rows))
  }

  .build_exceedance_rows <- function(agg_hist, agg_scn, hist_ref, source_label) {
    method <- aggregation_method()
    so_obj <- tryCatch(if (!is.null(baseline_hist_sim())) baseline_hist_sim()$so else NULL, error = function(e) NULL)
    spec <- metric_metadata(method, so_obj)
    adverse_tail <- spec$adverse_tail

    one <- function(tbl, scenario_label, is_hist) {
      if (is.null(tbl) || nrow(tbl) == 0L) {
        return(NULL)
      }
      mm <- matrix_transform(
        tbl, source_label,
        if (is_hist) historical_matrix_key else scenario_label
      )
      if (is.null(mm)) {
        return(NULL)
      }
      vals <- mm$vals
      sds <- mm$sds
      n_yrs <- ncol(vals)
      if (n_yrs == 0L) {
        return(NULL)
      }

      dplyr::bind_rows(lapply(seq_len(nrow(vals)), function(i) {
        v <- vals[i, ]
        s <- sds[i, ]
        ok <- is.finite(v)
        if (!any(ok)) {
          return(NULL)
        }
        v <- v[ok]
        s <- s[ok]
        n_pts <- length(v)

        # Adverse tail direction:
        ord <- if (identical(adverse_tail, "high")) {
          order(v, decreasing = TRUE)
        } else {
          order(v, decreasing = FALSE)
        }
        v_ord <- v[ord]
        s_ord <- if (length(s) == length(ord)) s[ord] else rep(0, length(ord))
        # Use empirical plotting positions so the rarest point is exactly
        # 1-in-n, rather than implying support beyond the simulated years.
        probs <- seq_along(ord) / n_pts

        # Limit to adverse tail direction only: 0.50 AEP or less
        keep <- probs <= 0.50
        if (!any(keep)) {
          return(NULL)
        }

        tibble::tibble(
          scenario      = scenario_label,
          source        = source_label,
          model_id      = mm$model_ids[[i]],
          rank          = seq_along(ord)[keep],
          welfare_val   = v_ord[keep] - hist_ref,
          coef_sd       = s_ord[keep],
          exceed_prob   = probs[keep],
          is_historical = is_hist
        )
      }))
    }
    rows <- list(one(agg_hist$out, "Historical", TRUE))
    if (!is.null(agg_scn)) {
      for (dk in names(agg_scn)) {
        if (!dk %in% selected_scenario_names()) next
        rows[[length(rows) + 1L]] <- one(agg_scn[[dk]]$out, dk, FALSE)
      }
    }
    dplyr::bind_rows(Filter(Negate(is.null), rows))
  }

  .build_threshold_rows <- function(agg_hist, agg_scn, hist_ref, source_label,
                                    bq_coef, bq_ens) {
    z_lo <- stats::qnorm(bq_coef[["lo"]])
    z_hi <- stats::qnorm(bq_coef[["hi"]])
    method <- aggregation_method()
    so_obj <- tryCatch(
      if (!is.null(baseline_hist_sim())) baseline_hist_sim()$so else NULL,
      error = function(e) NULL
    )
    adverse_tail <- metric_metadata(method, so_obj)$adverse_tail
    RPs <- c(RP_LOW, c("1:1" = 0.5), RP_HIGH)
    one <- function(tbl, scenario_label, is_hist) {
      if (is.null(tbl) || nrow(tbl) == 0L) {
        return(NULL)
      }
      mm <- matrix_transform(
        tbl, source_label,
        if (is_hist) historical_matrix_key else scenario_label
      )
      if (is.null(mm)) {
        return(NULL)
      }
      vals <- mm$vals
      sds <- mm$sds
      n_yrs <- ncol(vals)
      n_pts <- if (is_hist) sum(is.finite(as.numeric(vals))) else n_yrs
      rp_ok <- vapply(RPs, function(p) all(vapply(seq_len(nrow(vals)), function(i) {
        adverse_year_support(vals[i, ], suppressWarnings(as.numeric(mm$sim_years)),
          p, adverse_tail)$status == "ok"
      }, logical(1))), logical(1))
      RPs_keep <- RPs[rp_ok]
      if (length(RPs_keep) == 0L) {
        return(NULL)
      }
      # Per-model rank-interp at each kept RP (matrix: model * RP) - shape
      # guaranteed by the helper (see by_model_rp_matrix()).
      per_model_rp <- matrix(NA_real_, nrow(vals), length(RPs_keep),
        dimnames = list(mm$model_ids, names(RPs_keep)))
      per_model_sd_at_rp <- per_model_rp
      support_records <- list()
      for (i in seq_len(nrow(vals))) for (j in seq_along(RPs_keep)) {
        support <- adverse_year_support(vals[i, ], suppressWarnings(as.numeric(mm$sim_years)),
          RPs_keep[[j]], adverse_tail)
        support_records[[length(support_records) + 1L]] <- data.frame(
          scenario = scenario_label, model_id = mm$model_ids[[i]],
          probability = RPs_keep[[j]], as.list(support), stringsAsFactors = FALSE)
        if (identical(support$status, "ok")) {
          per_model_rp[i, j] <- apply_adverse_year_support(vals[i, ],
            suppressWarnings(as.numeric(mm$sim_years)), support)$value
          per_model_sd_at_rp[i, j] <- apply_adverse_year_support(sds[i, ],
            suppressWarnings(as.numeric(mm$sim_years)), support)$value
        }
      }
      central_vec <- vapply(seq_len(ncol(per_model_rp)), function(j) {
        if (!all(is.finite(per_model_rp[, j]))) NA_real_ else mean(per_model_rp[, j])
      }, numeric(1))
      coef_sd_vec <- if (is_hist) {
        per_model_sd_at_rp[1L, ]
      } else {
        apply(per_model_sd_at_rp, 2L, stats::median, na.rm = TRUE)
      }
      coef_lo_vec <- central_vec + z_lo * coef_sd_vec
      coef_hi_vec <- central_vec + z_hi * coef_sd_vec
      intermod_lo_vec <- if (is_hist) {
        rep(NA_real_, length(RPs_keep))
      } else {
        apply(per_model_rp, 2L, stats::quantile, probs = bq_ens[["lo"]], na.rm = TRUE)
      }
      intermod_hi_vec <- if (is_hist) {
        rep(NA_real_, length(RPs_keep))
      } else {
        apply(per_model_rp, 2L, stats::quantile, probs = bq_ens[["hi"]], na.rm = TRUE)
      }
      var_across_at_rp <- if (is_hist) {
        rep(0, length(RPs_keep))
      } else {
        apply(per_model_rp, 2L, stats::var, na.rm = TRUE)
      }
      var_across_at_rp[is.na(var_across_at_rp)] <- 0
      sd_total_vec <- sqrt(pmax(coef_sd_vec^2 + var_across_at_rp, 0, na.rm = FALSE))
      total_lo_vec <- central_vec + z_lo * sd_total_vec
      total_hi_vec <- central_vec + z_hi * sd_total_vec
      make_row <- function(estimate, vec) {
        tibble::tibble(
          scenario      = scenario_label,
          source        = source_label,
          Estimate      = estimate,
          rp_name       = names(RPs_keep),
          rp_label      = names(RPs_keep),
          value         = vec - hist_ref,
          n_obs         = n_pts,
          is_historical = is_hist
        )
      }
      coef_lo_lbl <- paste0("Coef ", pct_label(bq_coef[["lo"]]))
      coef_hi_lbl <- paste0("Coef ", pct_label(bq_coef[["hi"]]))
      ens_lo_lbl <- paste0("Ensemble ", pct_label(bq_ens[["lo"]], use_minmax = TRUE))
      ens_hi_lbl <- paste0("Ensemble ", pct_label(bq_ens[["hi"]], use_minmax = TRUE))
      pooled_lo_lbl <- paste0("Pooled ", pct_label(bq_coef[["lo"]]))
      pooled_hi_lbl <- paste0("Pooled ", pct_label(bq_coef[["hi"]]))
      rows <- list(
        make_row(if (is_hist) "Single historical estimate" else "Equal-model mean", central_vec),
        make_row(coef_lo_lbl, coef_lo_vec),
        make_row(coef_hi_lbl, coef_hi_vec)
      )
      if (!is_hist) {
        ensemble_rows <- if (identical(ens_lo_lbl, ens_hi_lbl)) {
          list(make_row(ens_lo_lbl, central_vec))
        } else {
          list(
            make_row(ens_lo_lbl, intermod_lo_vec),
            make_row(ens_hi_lbl, intermod_hi_vec)
          )
        }
        rows <- c(rows, ensemble_rows, list(
          make_row(pooled_lo_lbl, total_lo_vec),
          make_row(pooled_hi_lbl, total_hi_vec)
        ))
      }
      out <- dplyr::bind_rows(rows)
      attr(out, "adverse_support") <- dplyr::bind_rows(support_records)
      out
    }
    rows <- list(one(agg_hist$out, "Historical", TRUE))
    if (!is.null(agg_scn)) {
      for (dk in names(agg_scn)) {
        if (!dk %in% selected_scenario_names()) next
        rows[[length(rows) + 1L]] <- one(agg_scn[[dk]]$out, dk, FALSE)
      }
    }
    dplyr::bind_rows(Filter(Negate(is.null), rows))
  }

  timeseries_curves_rv <- reactive({
    if (!identical(policy_endpoint_status()$status, "ok")) return(tibble::tibble())
    req(baseline_agg_hist())
    hr <- hist_ref_val()
    dplyr::bind_rows(
      .build_timeseries_rows(
        baseline_agg_hist(), baseline_agg_scenarios(),
        hr, "Baseline"
      ),
      .build_timeseries_rows(
        policy_agg_hist(), policy_agg_scenarios(),
        hr, "Policy"
      )
    )
  })

  exceedance_curves_rv <- reactive({
    if (!identical(policy_endpoint_status()$status, "ok")) return(tibble::tibble())
    req(baseline_agg_hist())
    hr <- hist_ref_val()
    dplyr::bind_rows(
      .build_exceedance_rows(
        baseline_agg_hist(), baseline_agg_scenarios(),
        hr, "Baseline"
      ),
      .build_exceedance_rows(
        policy_agg_hist(), policy_agg_scenarios(),
        hr, "Policy"
      )
    )
  })

  threshold_table_rv <- reactive({
    if (!identical(policy_endpoint_status()$status, "ok")) return(tibble::tibble())
    req(baseline_agg_hist())
    bq_coef <- resolve_band_q(input$uncertainty_band %||% "p10_p90")
    bq_ens <- if (identical(input$ensemble_band %||% "none", "none")) {
      c(lo = 0.5, hi = 0.5)
    } else {
      resolve_band_q(input$ensemble_band %||% "none")
    }
    hr <- hist_ref_val()
    out <- dplyr::bind_rows(
      .build_threshold_rows(
        baseline_agg_hist(), baseline_agg_scenarios(),
        hr, "Baseline", bq_coef, bq_ens
      ),
      .build_threshold_rows(
        policy_agg_hist(), policy_agg_scenarios(),
        hr, "Policy", bq_coef, bq_ens
      )
    )
    tails <- metric_decomposition()$return_period
    if (nrow(out)) {
      central_label <- function(x) x %in% c("Equal-model mean", "Single historical estimate", "Central (P50)")
      for (i in which(central_label(out$Estimate))) {
        scenario <- out$scenario[[i]]
        probability <- unname(c(RP_LOW, RP_HIGH, "1:1" = 0.5)[out$rp_name[[i]]])
        if (!is.finite(probability) || !probability %in% RP_LOW) next
        tail <- if (is.data.frame(tails) && nrow(tails)) {
          tails[tails$scenario == scenario & abs(tails$probability - probability) < 1e-8, , drop = FALSE]
        } else data.frame()
        state <- if (identical(out$source[[i]], "Baseline")) "baseline" else "policy"
        available <- nrow(tail) && identical(tail$status[[1L]], "ok") && is.finite(tail[[state]][[1L]])
        out$value[[i]] <- if (available) tail[[state]][[1L]] - hr else NA_real_
        if (!"absolute_value" %in% names(out)) out$absolute_value <- rep(NA_real_, nrow(out))
        out$absolute_value[[i]] <- if (available) tail[[state]][[1L]] else NA_real_
        if (!"adverse_basis" %in% names(out)) out$adverse_basis <- rep(NA_character_, nrow(out))
        if (!"scope" %in% names(out)) out$scope <- rep(NA_character_, nrow(out))
        if (!"center_method" %in% names(out)) out$center_method <- rep(NA_character_, nrow(out))
        if (!"status" %in% names(out)) out$status <- rep(NA_character_, nrow(out))
        if (!"reason" %in% names(out)) out$reason <- rep(NA_character_, nrow(out))
        out$adverse_basis[[i]] <- "baseline_selected_metric"
        out$scope[[i]] <- "baseline_anchored"
        out$center_method[[i]] <- if (nrow(tail)) tail$center_method[[1L]] else ""
        out$status[[i]] <- if (nrow(tail)) tail$status[[1L]] else "unavailable"
        out$reason[[i]] <- if (nrow(tail)) tail$reason[[1L]] else "Baseline-anchored adverse support unavailable."
      }
    }
    out
  })

  # Section 1: Annual weather variation (baseline and policy) ----
  # Zero-arg echarts closures shared by the on-screen renders and the export
  # bundle (guidelines §7 pattern); the ggplot builders remain the static
  # export reference.
  annual_distribution_chart <- function() {
    echart_step3_annual_distribution(
      timeseries_curves_rv(),
      x_label = metric_axis_label(
        aggregation_method(),
        baseline_hist_sim()$so,
        input$cmp_deviation %||% "none"
      ),
      plot_type = input$annual_distribution_type %||% "violin",
      height = "470px"
    )
  }
  output$annual_distribution_plot <- echarts4r::renderEcharts4r({
    ch <- annual_distribution_chart()
    req(!is.null(ch))
    ch
  })
  outputOptions(output, "annual_distribution_plot", suspendWhenHidden = TRUE)

  # Section 2: Adverse weather years (tail protection) ----
  adverse_dot_data_rv <- reactive({
    req(threshold_table_rv())
    dot <- step3_adverse_dot_data(
      threshold_table_rv(),
      method = aggregation_method(),
      so     = baseline_hist_sim()$so
    )
    if (identical(input$ensemble_band %||% "none", "none") && nrow(dot)) {
      dot$policy_lo <- NA_real_
      dot$policy_hi <- NA_real_
      dot$base_lo <- NA_real_
      dot$base_hi <- NA_real_
    }
    dot
  })

  adverse_dot_chart <- function() {
    echart_step3_adverse_dot(
      adverse_dot_data_rv(),
      x_label = metric_axis_label(
        aggregation_method(),
        baseline_hist_sim()$so,
        input$cmp_deviation %||% "none"
      ),
      height = "380px"
    )
  }
  output$adverse_dot_plot <- echarts4r::renderEcharts4r({
    ch <- adverse_dot_chart()
    req(!is.null(ch))
    ch
  })
  outputOptions(output, "adverse_dot_plot", suspendWhenHidden = TRUE)

  # Section 4: Decision & return-period table ----
  threshold_table_df <- function() {
    tbl <- threshold_table_rv()
    if (is.null(tbl) || !nrow(tbl) || !"Estimate" %in% names(tbl)) {
      return(NULL)
    }
    so_obj <- tryCatch(if (!is.null(baseline_hist_sim())) baseline_hist_sim()$so else NULL, error = function(e) NULL)
    n_h_yrs <- tryCatch(
      {
        run_info <- if (!is.null(baseline_hist_sim())) baseline_hist_sim()$sim_summary %||% list() else list()
        hy <- run_info$historical_years %||% integer(0)
        if (length(hy) >= 2L) as.integer(hy[2] - hy[1] + 1L) else max(tbl$n_obs, na.rm = TRUE)
      },
      error = function(e) max(tbl$n_obs, na.rm = TRUE)
    )

    build_threshold_table_df(
      threshold_tbl = tbl,
      group_order   = input$cmp_group_order %||% "scenario_x_year",
      show_coef     = TRUE,
      adverse_only  = TRUE,
      method        = aggregation_method(),
      so            = so_obj,
      n_hist_years  = n_h_yrs
    )
  }

  # The threshold table's Download CSV is a client-side
  # wise_reactable_csv_button() (guidelines §6); the R-side download handler
  # it replaced is gone.

  step3_incidence_data <- reactive({
    if (!identical(policy_endpoint_status()$status, "ok")) return(tibble::tibble())
    step3_incidence_by_decile(
      tryCatch(decomp_scenarios(), error = function(e) NULL),
      so = baseline_hist_sim()$so
    )
  })

  wise_export_figure(
    key = "policy_distributional_incidence",
    label = "Policy effect by baseline decile",
    step = 3L,
    fun = function() plot_incidence_by_decile(step3_incidence_data(), "Paired policy minus baseline effect"),
    description = "Paired policy-minus-baseline welfare effect by fixed baseline welfare decile.",
    width = 9, height = 5.5
  )
  wise_export_table(
    key = "policy_distributional_incidence_data",
    label = "Policy effect by baseline decile data",
    step = 3L,
    fun = function() {
      annotate_visualization_export(
        step3_incidence_data(), aggregation_method(), baseline_hist_sim()$so,
        observation_unit = "household-level paired policy minus baseline effect",
        aggregation_order = "fixed weighted baseline decile; weighted mean over households, then years averaged within model and equal-model mean",
        uncertainty = "paired contrast"
      )
    },
    description = "Tidy policy effect data by fixed baseline welfare decile."
  )

  paired_effect_summary_export <- function() {
    annotate_visualization_export(
      paired_effect_summary_rv(), aggregation_method(),
      baseline_hist_sim()$so,
      observation_unit = "scenario-period paired annual aggregate effect",
      aggregation_order = "paired policy minus baseline by model and weather-year; years averaged within model, then equal-model mean",
      uncertainty = "paired coefficient contrast (baseline-X approximation) and inter-model spread",
      context = metric_context()
    )
  }
  paired_annual_effect_export <- function() {
    annotate_visualization_export(
      paired_annual_effects_rv(), aggregation_method(),
      baseline_hist_sim()$so,
      observation_unit = "paired annual aggregate effect for one model-weather-year draw",
      aggregation_order = "policy aggregate minus baseline aggregate on matched household, model, and weather-year draws",
      uncertainty = "paired coefficient contrast", context = metric_context()
    )
  }
  paired_adverse_effect_export <- function() {
    annotate_visualization_export(
      paired_adverse_effects_rv(), aggregation_method(),
      baseline_hist_sim()$so,
      observation_unit = "baseline-anchored adverse outcome quantile contrast",
      aggregation_order = "baseline-selected metric rank interpolation applied to every state, then equal-model mean",
      uncertainty = "central supported contrast; no component uncertainty estimator", context = metric_context()
    )
  }
  wise_export_table(
    key = "policy_paired_effect_summary",
    label = "Paired policy effect summaries",
    step = 3L,
    fun = paired_effect_summary_export,
    description = "Expected policy-minus-baseline effects with paired uncertainty and model counts."
  )
  wise_export_table(
    key = "policy_annual_effect_data",
    label = "Annual paired policy effects",
    step = 3L,
    fun = paired_annual_effect_export,
    description = "Tidy annual policy-minus-baseline effects for matched model-weather-year draws."
  )
  wise_export_table(
    key = "policy_adverse_effects",
    label = "Adverse-year policy effects",
    step = 3L,
    fun = paired_adverse_effect_export,
    description = "Policy effects at adverse ranks selected from baseline annual selected-metric aggregates; interpolated baseline support is reused."
  )
  wise_export_table(
    key = "policy_adverse_effect_table",
    label = "Adverse-year policy effect table",
    step = 3L,
    fun = function() {
      annotate_visualization_export(
        paired_adverse_table_rv(), aggregation_method(),
        baseline_hist_sim()$so,
        observation_unit = "baseline-anchored adverse outcome quantile contrast",
        aggregation_order = "shared baseline support within model, then equal-model mean",
        uncertainty = "central supported contrast; no component uncertainty estimator",
        context = metric_context()
      )
    },
    description = "Expected, 1-in-5, 1-in-10, and 1-in-20 equal-probability paired tail effects where supported."
  )
  wise_export_figure(
    key = "policy_annual_distribution",
    label = "Annual baseline and policy welfare distribution",
    step = 3L,
    fun = annual_distribution_chart,
    description = "Annual baseline and policy welfare distribution shown in the comparison panel.",
    width = 10, height = 6.5
  )
  wise_export_figure(
    key = "policy_adverse_distribution",
    label = "Adverse-year baseline and policy welfare",
    step = 3L,
    fun = adverse_dot_chart,
    description = "Expected and historically supported adverse baseline/policy outcomes using the Step 2 selected-metric baseline quantile convention.",
    width = 10, height = 6.5
  )
  wise_export_table(
    key = "policy_outcome_thresholds",
    label = "Policy outcome threshold details",
    step = 3L,
    fun = function() {
      df <- threshold_table_df()
      if (is.null(df)) {
        return(NULL)
      }
      annotate_visualization_export(
        df,
        aggregation_method(), baseline_hist_sim()$so,
        observation_unit = "scenario-period return-period annual aggregate",
        aggregation_order = "per-model return-period interpolation, then across-model summary",
        uncertainty = "coefficient, ensemble, and pooled bands where supported",
        context = metric_context()
      )
    },
    stale = stale,
    description = "Technical baseline, policy, and threshold detail table behind the advanced risk view."
  )

  output$summary_threshold_table <- reactable::renderReactable({
    df <- threshold_table_df()
    if (is.null(df) || nrow(df) == 0L) {
      return(.wise_threshold_reactable(data.frame(Note = "Insufficient data")))
    }
    .wise_threshold_reactable(as.data.frame(df))
  })
  outputOptions(output, "summary_threshold_table", suspendWhenHidden = TRUE)

  # UI-48: Step 3's baseline-vs-policy comparison figures.
  wise_export_figure(
    key = "policy_outcome_distribution",
    label = "Baseline vs policy welfare by scenario",
    step = 3L,
    fun = function() {
      paired_effect_plot(
        paired_effect_summary_rv(),
        metric_axis_label(
          aggregation_method(),
          baseline_hist_sim()$so,
          input$cmp_deviation %||% "none"
        )
      )
    },
    description = paste(
      "Simulated welfare under the baseline and the policy scenario, by",
      "climate scenario and projection period."
    ),
    width = 10, height = 6.5
  )

  exceedance_chart <- function() {
    curves <- exceedance_curves_rv()
    ah <- baseline_agg_hist()
    if (is.null(curves) || is.null(ah)) {
      return(NULL)
    }
    sel_spread <- input$exceedance_model_spread %||% "none"
    ens_q <- if (identical(sel_spread, "none")) {
      c(lo = 0.5, hi = 0.5)
    } else {
      resolve_band_q(sel_spread)
    }
    echart_exceedance(
      curves_tbl = curves,
      x_label = metric_axis_label(
        aggregation_method(),
        baseline_hist_sim()$so,
        input$cmp_deviation %||% "none"
      ),
      n_sim_years = nrow(ah$out),
      logit_x = TRUE,
      band_q = NULL,
      ensemble_band_q = ens_q,
      height = "400px"
    )
  }

  wise_export_figure(
    key = "policy_exceedance_curve",
    label = "Policy welfare exceedance probability",
    step = 3L,
    fun = exceedance_chart,
    description = paste(
      "Annual probability of reaching an outcome level in the adverse",
      "direction under baseline and policy across climate scenarios."
    ),
    width = 10, height = 6.5
  )

  output$exceedance_plot <- echarts4r::renderEcharts4r({
    ch <- exceedance_chart()
    req(!is.null(ch))
    ch
  })
  outputOptions(output, "exceedance_plot", suspendWhenHidden = TRUE)

  # Invisibly expose the aggregation internals for regression tests
  # (test-policy-sim-compare-agg-cache.R).
  invisible(list(
    baseline_agg_hist = baseline_agg_hist,
    baseline_agg_scenarios = baseline_agg_scenarios,
    policy_agg_hist = policy_agg_hist,
    policy_agg_scenarios = policy_agg_scenarios,
    agg_cache_ws = agg_cache_ws,
    agg_cache_keys = reactive(attr(agg_cache_ws(), "keys")),
    agg_cache_get = .agg_cache_get,
    agg_cache_put = .agg_cache_put,
    matrix_transforms = matrix_transforms_rv,
    matrix_transform = matrix_transform,
    hist_label = hist_label,
    threshold_table = threshold_table_rv,
    paired_effect_summary = paired_effect_summary_rv,
    headline_paired_effect_summary = headline_adverse_summary_rv,
    expected_paired_effect_summary = headline_paired_effect_summary_rv,
    headline_cards = headline_cards_data_rv,
    aggregation_method = aggregation_method,
    poverty_line = pov_line_val,
    focus_scenario = focus_scenario,
    metric_context = metric_context,
    metric_decomposition = metric_decomposition,
    policy_endpoint_status = policy_endpoint_status,
    selected_scenario_names = selected_scenario_names,
    pov_line_val = pov_line_val
  ))
}
