#' 3_08_decomposition UI Function
#'
#' @description A shiny Module. Renders the policy effect decomposition
#'   visualizations: stacked bar chart by decile, beta curve (RIF only),
#'   and summary table.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_3_08_decomposition_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("decomp_header_ui")),
    shiny::wellPanel(
      shiny::h4("Policy Effect Decomposition by Welfare Decile"),
      shiny::plotOutput(ns("decomp_bar_plot"), height = "450px"),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:6px;",
        "Bars show the average effect in each channel by baseline welfare decile.",
        "Decile 1 = poorest. Effects in percentage change.",
        shiny::tags$em(style = "color:#888;",
          " Weather hazard: mean of historical baseline.")
      )
    ),
    shiny::uiOutput(ns("beta_curve_ui")),
    shiny::uiOutput(ns("scenario_range_ui")),
    shiny::wellPanel(
      shiny::h4("Decomposition Summary"),
      DT::DTOutput(ns("decomp_summary_table")),
      shiny::uiOutput(ns("interaction_warning_ui"))
    )
  )
}

#' 3_08_decomposition Server Functions
#'
#' @param id Module id.
#' @param decomp_result Reactive data frame from decompose_policy_effect().
#' @param decomp_scenarios Reactive data frame: per-scenario decompositions.
#' @param model_fit Reactive model fit list (for rif_grid / engine detection).
#' @param so Reactive selected outcome metadata.
#'
#' @noRd
mod_3_08_decomposition_server <- function(id,
                                           decomp_result     = reactive(NULL),
                                           decomp_scenarios  = reactive(list()),
                                           model_fit         = reactive(NULL),
                                           so            = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    is_rif <- reactive({
      mf <- model_fit()
      !is.null(mf) && identical(mf$engine, "rif")
    })

    output$decomp_header_ui <- renderUI({
      req(decomp_result())
      engine_label <- if (is_rif()) "RIF (full: main + repositioning + interaction)"
                      else "OLS (simplified: main + interaction)"
      shiny::div(
        style = paste0(
          "border-left: 4px solid #7b3294; background: #faf5fd; ",
          "padding: 10px 14px; margin-bottom: 12px; border-radius: 3px;"
        ),
        shiny::tags$strong(style = "font-size:15px;",
                           "Policy Effect Decomposition"),
        shiny::tags$br(),
        shiny::tags$span(style = "color:#555; font-size:12px;",
                         paste0("Engine: ", engine_label)),
        shiny::tags$br(),
        shiny::div(
          class = "alert alert-info",
          style = "margin-top:8px; margin-bottom:0; font-size:12px; padding:6px 10px;",
          shiny::icon("info-circle"),
          " The decile bar chart and summary table use the ",
          shiny::tags$strong("mean weather from the historical baseline"),
          " as the weather hazard. Because weather changes across climate ",
          "scenarios and years, the decomposition channels — especially ",
          "repositioning and interaction — will differ. ",
          "See the ", shiny::tags$em("Scenario Range"), " panel below."
        )
      )
    })

    # --- Stacked bar chart by decile ---
    output$decomp_bar_plot <- shiny::renderPlot({
      req(decomp_result())
      .plot_decomp_bars(decomp_result(), is_rif())
    })

    # --- Beta curve (RIF only) ---
    output$beta_curve_ui <- renderUI({
      if (!is_rif()) return(NULL)
      mf <- model_fit()
      if (is.null(mf$rif_grid)) return(NULL)
      shiny::wellPanel(
        shiny::h4("Weather Beta Curve Across Welfare Distribution"),
        shiny::plotOutput(ns("beta_curve_plot"), height = "350px"),
        shiny::tags$p(
          style = "font-size:11px; color:#666; margin-top:6px;",
          "Shows how weather sensitivity varies by quantile.",
          "Repositioning effect arises from households moving along this curve."
        )
      )
    })

    output$beta_curve_plot <- shiny::renderPlot({
      req(is_rif(), model_fit())
      .plot_beta_curves(model_fit())
    })

    # --- Scenario range panel ---
    output$scenario_range_ui <- renderUI({
      sc <- decomp_scenarios()
      if (is.null(sc) || (is.data.frame(sc) && nrow(sc) == 0) ||
          (!is.data.frame(sc) && length(sc) == 0)) return(NULL)
      shiny::wellPanel(
        shiny::h4("Policy Effect Decomposition Across Climate Scenarios"),
        shiny::tags$p(
          style = "font-size:12px; color:#555; margin-bottom:8px;",
          "Each point/line is one SSP scenario \u00d7 period combination.",
          "Variation reflects changing weather conditions rather than uncertainty",
          "in the model coefficients.",
          "The dashed reference line (0) is the historical baseline mean."
        ),
        shiny::plotOutput(ns("scenario_range_plot"), height = "420px")
      )
    })

    output$scenario_range_plot <- shiny::renderPlot({
      sc <- decomp_scenarios()
      req(!is.null(sc), is.data.frame(sc), nrow(sc) > 0)
      .plot_decomp_scenario_range(sc, is_rif())
    })

    # --- Summary table ---
    output$decomp_summary_table <- DT::renderDT({
      req(decomp_result())
      .build_decomp_table(decomp_result(), is_rif())
    })

    # --- Interaction warning ---
    output$interaction_warning_ui <- renderUI({
      res <- decomp_result()
      if (is.null(res)) return(NULL)
      if (all(abs(res$delta_res2) < 1e-10)) {
        shiny::div(
          class = "alert alert-warning",
          style = "margin-top: 10px; font-size: 13px;",
          shiny::icon("exclamation-triangle"),
          " No weather\u00d7policy interaction terms detected in the model. ",
          "The interaction channel is zero. To enable this channel, include ",
          "interaction terms between weather and policy variables in the ",
          "Step 1 model specification."
        )
      }
    })

    invisible(NULL)
  })
}


# ---------------------------------------------------------------------------- #
# Plot helpers
# ---------------------------------------------------------------------------- #

#' @noRd
.plot_decomp_bars <- function(decomp_df, is_rif) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (is.null(decomp_df) || nrow(decomp_df) == 0) return(NULL)

  # Aggregate by decile (weighted mean)
  agg <- do.call(rbind, lapply(sort(unique(decomp_df$decile)), function(d) {
    idx <- decomp_df$decile == d
    w <- decomp_df$weight[idx]
    if (length(w) == 0 || all(is.na(w))) return(NULL)
    data.frame(
      decile   = d,
      pct_main = stats::weighted.mean(decomp_df$pct_main[idx], w, na.rm = TRUE),
      pct_res1 = stats::weighted.mean(decomp_df$pct_res1[idx], w, na.rm = TRUE),
      pct_res2 = stats::weighted.mean(decomp_df$pct_res2[idx], w, na.rm = TRUE)
    )
  }))

  if (is.null(agg) || nrow(agg) == 0) return(NULL)

  # Reshape to long
  if (is_rif) {
    long <- data.frame(
      decile = rep(agg$decile, 3),
      channel = rep(c("Main effect", "Repositioning", "Interaction"), each = nrow(agg)),
      value = c(agg$pct_main, agg$pct_res1, agg$pct_res2)
    )
    long$channel <- factor(long$channel,
                           levels = c("Interaction", "Repositioning", "Main effect"))
  } else {
    long <- data.frame(
      decile = rep(agg$decile, 2),
      channel = rep(c("Main effect", "Interaction"), each = nrow(agg)),
      value = c(agg$pct_main, agg$pct_res2)
    )
    long$channel <- factor(long$channel,
                           levels = c("Interaction", "Main effect"))
  }

  ggplot2::ggplot(long, ggplot2::aes(x = factor(decile), y = value, fill = channel)) +
    ggplot2::geom_col(position = "stack", width = 0.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    ggplot2::scale_fill_manual(
      values = c("Main effect" = "#2166ac",
                 "Repositioning" = "#b2182b",
                 "Interaction" = "#fdae61")
    ) +
    ggplot2::labs(
      x = "Baseline welfare decile (1 = poorest)",
      y = "Effect (% change in welfare)",
      fill = "Channel"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "bottom")
}


#' @noRd
.plot_beta_curves <- function(model_fit) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)

  rg <- model_fit$rif_grid
  weather_vars <- model_fit$weather_terms
  if (is.null(rg) || is.null(weather_vars)) return(NULL)

  # Filter to weather terms only (model 3)
  plot_data <- rg[rg$model == 3L & rg$term %in% weather_vars, ]
  if (nrow(plot_data) == 0) return(NULL)

  ggplot2::ggplot(plot_data, ggplot2::aes(x = tau, y = estimate)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high),
                         alpha = 0.15, fill = "#b2182b") +
    ggplot2::geom_line(linewidth = 0.8, colour = "#b2182b") +
    ggplot2::geom_point(size = 2, colour = "#b2182b") +
    ggplot2::facet_wrap(~term, scales = "free_y") +
    ggplot2::scale_x_continuous(
      breaks = model_fit$taus,
      labels = scales::percent_format(1)
    ) +
    ggplot2::labs(
      x = "Welfare quantile",
      y = "UQR coefficient (weather sensitivity)",
      title = "Weather beta curve across welfare distribution",
      subtitle = "Steeper slope = larger repositioning effect"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}


#' @noRd
.plot_decomp_scenario_range <- function(sc_df, is_rif = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (is.null(sc_df) || nrow(sc_df) == 0) return(NULL)

  channels <- if (is_rif) {
    c("delta_main"  = "Main effect",
      "delta_res1"  = "Repositioning",
      "delta_res2"  = "Interaction",
      "delta_total" = "Total")
  } else {
    c("delta_main"  = "Main effect",
      "delta_res2"  = "Interaction",
      "delta_total" = "Total")
  }

  # Weighted mean per channel per scenario × sim_year
  # (each row in sc_df is a household; aggregate to year-level first)
  has_years <- "sim_year" %in% names(sc_df) && !all(is.na(sc_df$sim_year))

  agg_rows <- lapply(names(channels), function(col) {
    if (!col %in% names(sc_df)) return(NULL)

    by_vars <- if (has_years) {
      list(sc_df$scenario, sc_df$year_start, sc_df$year_end, sc_df$sim_year)
    } else {
      list(sc_df$scenario, sc_df$year_start, sc_df$year_end)
    }
    groups <- split(seq_len(nrow(sc_df)), interaction(by_vars, drop = TRUE))

    rows <- lapply(groups, function(idx) {
      d <- sc_df[idx, ]
      w <- d$weight
      data.frame(
        scenario   = d$scenario[[1]],
        year_start = d$year_start[[1]],
        year_end   = d$year_end[[1]],
        sim_year   = if (has_years) d$sim_year[[1]] else NA_integer_,
        channel    = channels[[col]],
        pct        = stats::weighted.mean((exp(d[[col]]) - 1) * 100, w,
                                          na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  })
  agg_df <- do.call(rbind, Filter(Negate(is.null), agg_rows))
  if (is.null(agg_df) || nrow(agg_df) == 0) return(NULL)

  agg_df$channel    <- factor(agg_df$channel, levels = unname(channels))
  agg_df$period_lbl <- ifelse(
    is.na(agg_df$year_start),
    agg_df$scenario,
    paste0(agg_df$year_start, "\u2013", agg_df$year_end)
  )
  agg_df$ssp <- sub(" / .*$", "", agg_df$scenario)

  # Channels where weather variation drives spread vs. those that are constant
  weather_sensitive <- if (is_rif) {
    c("Repositioning", "Interaction", "Total")
  } else {
    c("Interaction", "Total")
  }
  agg_df$weather_sensitive <- agg_df$channel %in% weather_sensitive

  ggplot2::ggplot(
    agg_df,
    ggplot2::aes(x = period_lbl, y = pct, colour = ssp, fill = ssp)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        colour = "grey50", linewidth = 0.5) +
    # For weather-sensitive channels: boxplot to show within-period year spread
    ggplot2::geom_boxplot(
      data = ~ dplyr::filter(.x, weather_sensitive),
      ggplot2::aes(group = interaction(period_lbl, ssp)),
      alpha = 0.25, outlier.size = 1, linewidth = 0.5,
      position = ggplot2::position_dodge(width = 0.6)
    ) +
    # For main effect (constant across years): point + line across periods
    ggplot2::geom_point(
      data = ~ dplyr::filter(.x, !weather_sensitive),
      size = 3,
      position = ggplot2::position_dodge(width = 0.6)
    ) +
    ggplot2::geom_line(
      data = ~ dplyr::filter(.x, !weather_sensitive),
      ggplot2::aes(group = ssp),
      linewidth = 0.8
    ) +
    ggplot2::facet_wrap(
      ~channel, scales = "free_y",
      ncol = if (is_rif) 2L else 3L
    ) +
    ggplot2::scale_colour_brewer(palette = "Set1", name = "SSP scenario") +
    ggplot2::scale_fill_brewer(palette = "Set1", name = "SSP scenario") +
    ggplot2::labs(
      x        = "Projection period",
      y        = "Effect (% change in welfare)",
      title    = "Policy effect decomposition across climate scenarios",
      subtitle = paste0(
        "Main effect is constant across weather years; ",
        "boxes show within-period year-to-year variation for weather-sensitive channels"
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position  = "bottom",
      axis.text.x      = ggplot2::element_text(angle = 30, hjust = 1),
      strip.text       = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
}


#' @noRd
.build_decomp_table <- function(decomp_df, is_rif) {
  w <- decomp_df$weight

  summary_row <- function(label, vals) {
    data.frame(
      Channel = label,
      `Mean (log-pts)` = round(stats::weighted.mean(vals, w), 4),
      `Mean (%)` = round(stats::weighted.mean((exp(vals) - 1) * 100, w), 2),
      `Median (%)` = round(median((exp(vals) - 1) * 100), 2),
      check.names = FALSE
    )
  }

  rows <- list(
    summary_row("Total effect", decomp_df$delta_total),
    summary_row("  Main effect", decomp_df$delta_main),
    summary_row("    of which: SP transfer", decomp_df$delta_sp)
  )

  if (is_rif) {
    rows <- c(rows, list(
      summary_row("  Resilience: Repositioning", decomp_df$delta_res1),
      summary_row("  Resilience: Interaction", decomp_df$delta_res2)
    ))
  } else {
    rows <- c(rows, list(
      summary_row("  Resilience: Interaction", decomp_df$delta_res2)
    ))
  }

  df <- do.call(rbind, rows)

  DT::datatable(
    df, rownames = FALSE, class = "compact stripe",
    options = list(dom = "t", ordering = FALSE,
                   columnDefs = list(list(className = "dt-right", targets = 1:3)))
  )
}
