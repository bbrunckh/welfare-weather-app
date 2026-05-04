
#' Make a before/after histogram for a single variable
#' @noRd
.make_before_after_hist <- function(baseline_vals, policy_vals,
                                    var_name) {
  baseline_clean <- baseline_vals[!is.na(baseline_vals)]
  policy_clean   <- policy_vals[!is.na(policy_vals)]

  if (length(baseline_clean) == 0 && length(policy_clean) == 0) {
    plot.new()
    graphics::title(main = "No data available")
    return(invisible(NULL))
  }

  all_vals <- c(baseline_clean, policy_clean)
  x_range <- range(all_vals, na.rm = TRUE)
  x_range <- x_range + c(-0.1, 0.1) * diff(x_range)

  dens_base <- if (length(baseline_clean) > 1)
    stats::density(baseline_clean, from = x_range[1], to = x_range[2])
  else NULL

  dens_poli <- if (length(policy_clean) > 1)
    stats::density(policy_clean, from = x_range[1], to = x_range[2])
  else NULL

  y_max <- 0
  if (!is.null(dens_base)) y_max <- max(y_max, max(dens_base$y))
  if (!is.null(dens_poli)) y_max <- max(y_max, max(dens_poli$y))

  plot(
    NULL,
    xlim = x_range,
    ylim = c(0, y_max * 1.1),
    xlab = var_name,
    ylab = "Density",
    main = paste0("Distribution: ", var_name),
    cex.main = 0.95
  )

  if (!is.null(dens_base)) {
    graphics::lines(dens_base, col = "#bdbdbd", lwd = 2.5, lty = 1)
  }
  if (!is.null(dens_poli)) {
    graphics::lines(dens_poli, col = "#d32f2f", lwd = 2.5, lty = 1)
  }

  legend_cols <- c(
    if (!is.null(dens_base)) "#bdbdbd" else NULL,
    if (!is.null(dens_poli)) "#d32f2f" else NULL
  )
  legend_labs <- c(
    if (!is.null(dens_base)) "Baseline" else NULL,
    if (!is.null(dens_poli)) "Policy-adjusted" else NULL
  )

  if (length(legend_cols) > 0) {
    graphics::legend("topright", legend = legend_labs, col = legend_cols,
                     lwd = 2.5, cex = 0.85)
  }

  invisible(NULL)
}


#' Detect columns that differ between the baseline and policy-adjusted frames
#'
#' Returns the names of columns whose values differ between
#' \code{baseline_svy} and \code{policy_svy}. Used by the Step 3 diagnostics
#' table to surface any variable a user manipulation has touched —
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
#' @return Character vector of column names that changed.
#' @export
detect_manipulated_vars <- function(baseline_svy, policy_svy) {
  if (is.null(baseline_svy) || is.null(policy_svy)) return(character(0))
  shared <- intersect(names(baseline_svy), names(policy_svy))
  if (length(shared) == 0) return(character(0))
  if (nrow(baseline_svy) != nrow(policy_svy)) {
    return(setdiff(union(names(baseline_svy), names(policy_svy)), character(0)))
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
  if (is.null(baseline_svy) || is.null(policy_svy)) return(NULL)

  if (is.null(vars)) {
    num_b <- names(baseline_svy)[vapply(baseline_svy, is.numeric, logical(1))]
    num_p <- names(policy_svy)[vapply(policy_svy, is.numeric, logical(1))]
    vars  <- intersect(num_b, num_p)
    # Drop obvious non-covariate keys
    vars  <- setdiff(vars, c("loc_id", "int_year", "int_month", "sim_year"))
  }

  vars <- vars[vars %in% names(baseline_svy) & vars %in% names(policy_svy)]

  if (length(vars) == 0) return(NULL)

  rows <- lapply(vars, function(v) {
    xb <- suppressWarnings(as.numeric(baseline_svy[[v]]))
    xp <- suppressWarnings(as.numeric(policy_svy[[v]]))
    data.frame(
      variable       = v,
      mean_baseline  = mean(xb, na.rm = TRUE),
      mean_policy    = mean(xp, na.rm = TRUE),
      delta_mean     = mean(xp, na.rm = TRUE) - mean(xb, na.rm = TRUE),
      sd_baseline    = stats::sd(xb, na.rm = TRUE),
      sd_policy      = stats::sd(xp, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}


#' Render the UI block for the combined Baseline + Policy results pane.
#'
#' Single-pane layout. The visualisations beneath display baseline and
#' policy series side-by-side. Inputs and outputs are namespaced via
#' \code{ns()}.
#' @noRd
.results_pane_ui <- function(ns, so) {
  tagList(
    shiny::uiOutput(ns("results_header_ui")),
    shiny::wellPanel(
      style = "padding: 10px 14px 6px 14px;",
      shiny::tags$p("Outcome of interest:",
                    style = "font-weight:600; margin-bottom:4px;"),
      shiny::tags$div(
        style = "display:flex; align-items:flex-end; gap:12px; flex-wrap:wrap;",
        shiny::tags$div(style = "flex:1; min-width:160px;",
          shiny::selectInput(
            ns("cmp_agg_method"),
            label    = "Aggregation method",
            choices  = hist_aggregate_choices(so$type, so$name),
            selected = "mean"
          )
        ),
        shiny::tags$div(style = "flex:1; min-width:160px;",
          shiny::selectInput(
            ns("cmp_deviation"),
            label    = "Deviation from historical baseline",
            choices  = c(
              "None (raw value)" = "none",
              "Historical mean"   = "mean",
              "Historical median" = "median"
            ),
            selected = "none"
          )
        ),
        shiny::tags$div(style = "flex:1; min-width:160px;",
          shiny::uiOutput(ns("cmp_pov_line_ui"))
        )
      ),
      shiny::tags$hr(style = "margin: 6px 0;"),
      shiny::tags$p("Scenario Filters",
                    style = "font-weight:600; margin-bottom:4px;"),
      shiny::fluidRow(
        shiny::column(12, shiny::uiOutput(ns("scenario_filter_ui")))
      ),
      shiny::tags$hr(style = "margin: 6px 0;"),
      shiny::checkboxInput(
        ns("use_weights"),
        label = "Use survey weights (if available)",
        value = TRUE
      ),
      shiny::uiOutput(ns("weight_status_ui")),
      shiny::radioButtons(
        ns("cmp_group_order"),
        label    = "Group charts and tables by",
        choices  = c(
          "Scenario × Year" = "scenario_x_year",
          "Year × Scenario" = "year_x_scenario"
        ),
        selected = "scenario_x_year",
        inline   = TRUE
      )
    ),
    shiny::wellPanel(
      shiny::h4("Distribution of outcomes across climate scenarios and timeframes"),
      shiny::plotOutput(ns("summary_box_plot"), height = "600px"),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:6px;",
        "Each scenario shows two dots: baseline (grey) and policy-adjusted (red).",
        "Historical shows only baseline since policy adjustments do not apply.",
        shiny::tags$br(),
        "The dot is the mean annual simulated value;",
        "thick line = 90% CI; thin grey line = 95% CI.",
        shiny::tags$br(),
        "Dashed line = historical mean."
      )
    ),
    shiny::wellPanel(
      shiny::h4("Exceedance probability by climate scenario"),
      shiny::tags$div(
        style = "display:flex; gap:20px; flex-wrap:wrap; margin-bottom:6px;",
        shiny::checkboxInput(
          ns("exceedance_logit_x"),
          "Logit probability axis (emphasise both tails)",
          value = FALSE
        ),
        shiny::checkboxInput(
          ns("show_return_period"),
          "Show return period lines",
          value = TRUE
        )
      ),
      shiny::plotOutput(ns("exceedance_plot"), height = "400px"),
      shiny::uiOutput(ns("exceedance_caption"))
    ),
    shiny::wellPanel(
      shiny::uiOutput(ns("threshold_table_header")),
      DT::DTOutput(ns("summary_threshold_table")),
      shiny::uiOutput(ns("threshold_table_footer"))
    )
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
                               selected_hist) {
  ns <- session$ns

  hist_label <- reactive({
    nm <- if (!is.null(selected_hist)) selected_hist()$scenario_name else NULL
    if (!is.null(nm) && nzchar(nm)) nm else "Historical"
  })

  # SSP / year / model metadata is identical across baseline and policy
  # (same set of saved scenarios), so derive from baseline.
  all_ssps <- reactive({
    sc <- baseline_saved_scenarios()
    if (length(sc) == 0) return(character(0))
    ssps <- unique(.normalise_ssp(names(sc)))
    sort(ssps[!is.na(ssps) & grepl("^SSP", ssps)])
  })

  all_anchor_years <- reactive({
    sc <- baseline_saved_scenarios()
    if (length(sc) == 0) return(character(0))
    ranges <- sort(na.omit(unique(.parse_year(names(sc)))))
    setNames(sub("-", "_", ranges), ranges)
  })

  all_models_info <- reactive({
    sc <- baseline_saved_scenarios()
    if (length(sc) == 0) return(character(0))
    vapply(sc, function(s) s$n_models %||% 1L, integer(1))
  })

  output$weight_status_ui <- shiny::renderUI({
    req(baseline_hist_sim())
    has_w  <- !is.null(baseline_hist_sim()$weight)
    tog_on <- isTRUE(input$use_weights)
    if (has_w && tog_on)
      shiny::tags$p(
        style = "font-size:11px; color:#2e7d32; margin:2px 0 6px 0;",
        "\u2705 Survey weights found and applied (",
        shiny::tags$code("weight"), ")")
    else if (has_w && !tog_on)
      shiny::tags$p(
        style = "font-size:11px; color:#e65100; margin:2px 0 6px 0;",
        "\u26a0 Survey weights available but not applied")
    else
      shiny::tags$p(
        style = "font-size:11px; color:#c62828; margin:2px 0 6px 0;",
        "\U0001F534 No weight column found \u2014 unweighted")
  })

  pov_line_val <- reactive({
    if (isTRUE(input$cmp_agg_method %in%
               c("headcount_ratio", "gap", "fgt2"))) {
      as.numeric(input$cmp_pov_line %||% NULL)
    } else NULL
  })

  selected_scenario_names <- reactive({
    sc <- baseline_saved_scenarios()
    if (length(sc) == 0) return(character(0))
    nms  <- names(sc)
    ssps <- if (length(input$filter_ssp) == 0) all_ssps()
            else input$filter_ssp
    yr_vals <- if (length(input$filter_year) == 0) names(all_anchor_years())
               else sub("_", "-", input$filter_year)
    keep <- vapply(nms, function(nm) {
      is_ssp <- grepl("^SSP", nm)
      if (!is_ssp) return(TRUE)
      ssp_match <- any(vapply(ssps, function(s) startsWith(nm, s), logical(1)))
      yr_match  <- length(yr_vals) == 0 ||
        any(vapply(yr_vals, function(y) grepl(y, nm, fixed = TRUE), logical(1)))
      ssp_match && yr_match
    }, logical(1))
    nms[keep]
  })

  # Helper: aggregate hist_sim using the new compact schema
  make_agg_hist <- function(hs) {
    if (is.null(hs) || is.null(hs$y_point)) return(NULL)
    method    <- input$cmp_agg_method %||% "mean"
    deviation <- input$cmp_deviation  %||% "none"
    use_w     <- isTRUE(input$use_weights)

    agg <- aggregate_with_uncertainty(
      y_point         = hs$y_point,
      F_loading       = hs$F_loading,
      group_vec       = hs$sim_year,
      so              = hs$so,
      agg_method      = method,
      weights         = if (use_w) hs$weight else NULL,
      pov_line        = pov_line_val(),
      train_resid     = if (!is.null(hs$train_data)) hs$train_data$.resid else NULL,
      residual_method = hs$residuals %||% "none",
      id_vec          = hs$id_vec,
      S               = as.integer(hs$S %||% 200L)
    )

    if (!identical(deviation, "none") && nrow(agg) > 0) {
      agg <- deviation_from_centre(df = agg, group = "sim_year",
                                   centre = deviation, loss = FALSE)
      if ("deviation" %in% names(agg)) {
        agg$value <- agg$deviation; agg$deviation <- NULL
      }
    }
    x_label <- if (identical(deviation, "none")) label_agg_method(method)
               else paste0(label_agg_method(method), " \u2014 ",
                           label_deviation(deviation))
    list(out = agg, x_label = x_label)
  }

  # Helper: build agg per saved scenario using new compact schema
  make_agg_scenarios <- function(sc, hs_for_dev) {
    if (length(sc) == 0) return(list())
    method    <- input$cmp_agg_method %||% "mean"
    deviation <- input$cmp_deviation  %||% "none"
    use_w     <- isTRUE(input$use_weights)
    x_label   <- if (identical(deviation, "none")) label_agg_method(method)
                 else paste0(label_agg_method(method), " \u2014 ",
                             label_deviation(deviation))

    lapply(sc, function(s) {
      tryCatch({
        if (is.null(s$models)) return(NULL)
        per_model <- lapply(s$models, function(mod) {
          aggregate_with_uncertainty(
            y_point         = mod$y_point,
            F_loading       = mod$F_loading,
            group_vec       = s$sim_year,
            so              = s$so,
            agg_method      = method,
            weights         = if (use_w) s$weight else NULL,
            pov_line        = pov_line_val(),
            train_resid     = if (!is.null(hs_for_dev$train_data)) hs_for_dev$train_data$.resid else NULL,
            residual_method = hs_for_dev$residuals %||% "none",
            id_vec          = s$id_vec,
            S               = as.integer(hs_for_dev$S %||% 200L)
          )
        })
        combined <- combine_ensemble_results(per_model)

        if (!identical(deviation, "none") && nrow(combined) > 0) {
          hist_agg <- make_agg_hist(hs_for_dev)
          hist_ref <- if (!is.null(hist_agg) && nrow(hist_agg$out) > 0) {
            if (identical(deviation, "mean")) mean(hist_agg$out$value, na.rm = TRUE)
            else if (identical(deviation, "median")) median(hist_agg$out$value, na.rm = TRUE)
            else NA_real_
          } else NA_real_
          if (!is.na(hist_ref)) {
            combined$value <- combined$value - hist_ref
            if ("value_p05" %in% names(combined)) {
              combined$value_p05 <- combined$value_p05 - hist_ref
              combined$value_p95 <- combined$value_p95 - hist_ref
            }
            if ("model_min" %in% names(combined)) {
              combined$model_min <- combined$model_min - hist_ref
              combined$model_max <- combined$model_max - hist_ref
              combined$model_values <- lapply(combined$model_values, function(v) v - hist_ref)
            }
          }
        }
        list(out = combined, x_label = x_label)
      }, error = function(e) NULL)
    })
  }

  baseline_agg_hist <- reactive({
    req(baseline_hist_sim())
    make_agg_hist(baseline_hist_sim())
  })
  policy_agg_hist <- reactive({
    req(policy_hist_sim())
    make_agg_hist(policy_hist_sim())
  })

  baseline_agg_scenarios <- reactive({
    req(baseline_hist_sim())
    make_agg_scenarios(baseline_saved_scenarios(), baseline_hist_sim())
  })
  policy_agg_scenarios <- reactive({
    req(policy_hist_sim())
    make_agg_scenarios(policy_saved_scenarios(), policy_hist_sim())
  })

  baseline_all_series <- reactive({
    sc  <- baseline_agg_scenarios()
    sel <- selected_scenario_names()
    c(setNames(list(baseline_agg_hist()), hist_label()),
      sc[intersect(sel, names(sc))])
  })
  policy_all_series <- reactive({
    sc  <- policy_agg_scenarios()
    sel <- selected_scenario_names()
    c(setNames(list(policy_agg_hist()), hist_label()),
      sc[intersect(sel, names(sc))])
  })

  table_subtitle <- reactive({
    req(baseline_agg_hist(), input$cmp_agg_method, input$cmp_deviation)
    paste0(
      baseline_agg_hist()$x_label, " — ",
      label_agg_method(input$cmp_agg_method), " | ",
      label_deviation(input$cmp_deviation)
    )
  })

  output$results_header_ui <- renderUI({
    req(baseline_hist_sim(), input$cmp_agg_method, input$cmp_deviation)
    so <- baseline_hist_sim()$so
    agg_label <- label_agg_method(input$cmp_agg_method)
    dev_label <- label_deviation(input$cmp_deviation)
    pov_txt   <- if (!is.null(pov_line_val()))
      paste0(" | Poverty line: $", pov_line_val(), "/day") else ""
    notes_txt <- paste0(
      "Showing ", agg_label, " of ", so$label %||% so$name,
      " expressed as ", dev_label, pov_txt,
      ". Baseline (grey) and policy (red) shown side-by-side."
    )
    shiny::div(
      style = paste0(
        "border-left: 4px solid #2166ac; background: #f4f8fd; ",
        "padding: 10px 14px; margin-bottom: 12px; border-radius: 3px;"
      ),
      shiny::tags$strong(style = "font-size:15px;",
                         paste0("Results: ", so$label %||% so$name)),
      shiny::tags$br(),
      shiny::tags$span(style = "color:#555; font-size:12px;", notes_txt)
    )
  })

  output$cmp_pov_line_ui <- renderUI({
    req(input$cmp_agg_method)
    if (input$cmp_agg_method %in% c("headcount_ratio", "gap", "fgt2")) {
      pov_val <- if (!is.null(baseline_hist_sim()$pov_line))
        sprintf("$%.2f", baseline_hist_sim()$pov_line) else "not yet set"
      shiny::helpText(
        style = "font-size:11px; color:#555; margin-bottom:6px;",
        paste0("Poverty line: ", pov_val,
               "/day (set at simulation time).")
      )
    }
  })

  output$scenario_filter_ui <- renderUI({
    sc <- baseline_saved_scenarios()
    if (length(sc) == 0)
      return(shiny::helpText("Run a simulation."))
    ssps <- all_ssps()
    yrs  <- all_anchor_years()
    mi   <- all_models_info()
    tagList(
      shiny::fluidRow(
        shiny::column(4,
          if (length(yrs) > 0)
            shiny::checkboxGroupInput(
              ns("filter_year"), label = "Projection periods",
              choices = yrs, selected = yrs, inline = TRUE
            )
        ),
        shiny::column(4,
          if (length(ssps) > 0)
            shiny::checkboxGroupInput(
              ns("filter_ssp"), label = "SSPs",
              choices = ssps, selected = ssps, inline = TRUE
            )
        ),
        shiny::column(4,
          if (any(mi > 1L))
            shiny::helpText(
              style = "font-size:11px; color:#555; margin-top:24px;",
              paste0("Each SSP aggregates results from ",
                     max(mi), " ensemble model(s).")
            )
        )
      )
    )
  })

  output$summary_box_plot <- renderPlot({
    req(baseline_agg_hist())
    pol_plot_pointrange_climate(
      baseline_scenarios = baseline_all_series(),
      policy_scenarios   = policy_all_series(),
      hist_agg           = baseline_agg_hist(),
      group_order        = input$cmp_group_order %||% "scenario_x_year"
    )
  }, height = 600)
  outputOptions(output, "summary_box_plot", suspendWhenHidden = FALSE)

  output$summary_threshold_table <- DT::renderDT({
    req(baseline_agg_hist())
    df <- pol_build_threshold_table_df(
      baseline_all_series = baseline_all_series(),
      policy_all_series   = policy_all_series(),
      group_order         = input$cmp_group_order %||% "scenario_x_year"
    )
    if (is.null(df))
      return(DT::datatable(data.frame(Message = "Insufficient data"),
                           rownames = FALSE, class = "compact stripe",
                           options  = list(dom = "t")))
    DT::datatable(
      df, rownames = FALSE, class = "compact stripe",
      options = list(
        pageLength = 30, dom = "t", ordering = FALSE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      )
    )
  })
  outputOptions(output, "summary_threshold_table", suspendWhenHidden = FALSE)

  output$threshold_table_header <- renderUI({
    req(baseline_agg_hist())
    tagList(
      shiny::h4("Outcome value at return-period thresholds (both tails)"),
      shiny::tags$small(class = "text-muted", table_subtitle())
    )
  })

  output$threshold_table_footer <- renderUI({
    req(baseline_agg_hist())
    tagList(
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:6px; margin-bottom:2px;",
        "Low odds show the value exceeded in only 1-in-N years."),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:0; margin-bottom:2px;",
        "High odds show the value reached in all but 1-in-N years."),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:0;",
        "1:1 shows the median (50th percentile) simulated value.")
    )
  })

  output$exceedance_plot <- renderPlot({
    req(baseline_agg_hist())
    pol_enhance_exceedance(
      baseline_scenarios = baseline_all_series(),
      policy_scenarios   = policy_all_series(),
      hist_agg           = baseline_agg_hist(),
      x_label            = baseline_agg_hist()$x_label,
      return_period      = isTRUE(input$show_return_period),
      n_sim_years        = nrow(baseline_agg_hist()$out),
      logit_x            = isTRUE(input$exceedance_logit_x)
    )
  })
  outputOptions(output, "exceedance_plot", suspendWhenHidden = FALSE)

  output$exceedance_caption <- renderUI({
    req(baseline_agg_hist())
    axis_txt <- if (isTRUE(input$exceedance_logit_x))
      "Probability axis is logit-scaled, giving equal visual weight to both tails."
    else
      "The curve shows the estimated annual exceedance probability for each outcome value."
    tagList(
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:6px; margin-bottom:2px;",
        axis_txt),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:0; margin-bottom:2px;",
        "Low odds show the value exceeded in only 1-in-N years."),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:0;",
        "High odds show the value reached in all but 1-in-N years.")
    )
  })

  invisible(NULL)
}


# ---------------------------------------------------------------------------- #
# Grouped point-range chart: baseline (grey) vs policy (red), side-by-side     #
# ---------------------------------------------------------------------------- #

#' Grouped Point-Range Chart Comparing Baseline and Policy Scenarios
#'
#' For each future scenario, draws two dots side-by-side:
#'   * Baseline (grey)
#'   * Policy   (red)
#' The Historical entry uses only the baseline dot. The legend at the top
#' of the chart distinguishes baseline vs policy.
#'
#' @param baseline_scenarios Named list; entries from aggregate_sim_preds()
#'   on the baseline survey frame. Should include a Historical entry.
#' @param policy_scenarios   Named list; entries from aggregate_sim_preds()
#'   on the policy-adjusted frame. Historical entry is ignored.
#' @param hist_agg           Historical aggregate (baseline) — used for the
#'   reference line and the y-axis label.
#' @param group_order        Ordering on the x-axis. Default
#'   \code{"scenario_x_year"}.
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_linerange geom_point geom_hline
#'   scale_colour_manual scale_x_discrete labs theme_minimal theme
#'   element_blank element_text position_dodge
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#' @export
pol_plot_pointrange_climate <- function(baseline_scenarios,
                                        policy_scenarios,
                                        hist_agg,
                                        group_order = "scenario_x_year") {

  stopifnot(is.list(hist_agg), all(c("out", "x_label") %in% names(hist_agg)))

  summarise_vals <- .summarise_vals
  ssp_short_map  <- c("SSP2-4.5" = "SSP2",
                      "SSP3-7.0" = "SSP3",
                      "SSP5-8.5" = "SSP5")

  # ---- historical summary (baseline only) ---------------------------------
  hist_s <- summarise_vals(hist_agg$out$value)
  if (is.null(hist_s)) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Run a simulation to see results."))
  }
  hist_mean <- hist_s$mean
  hist_s$pt_key    <- "Historical"
  hist_s$source    <- "Baseline"
  hist_s$ssp_short <- NA_character_
  hist_s$yr        <- NA_character_

  # ---- helper: per-scenario summary rows ----------------------------------
  build_fut <- function(scenarios, source_label) {
    if (length(scenarios) == 0) return(NULL)
    future_nms <- names(scenarios)[vapply(names(scenarios),
      function(nm) !is.na(.normalise_ssp(nm)), logical(1))]
    if (length(future_nms) == 0) return(NULL)
    rows <- lapply(future_nms, function(nm) {
      s <- summarise_vals(scenarios[[nm]]$out$value)
      if (is.null(s)) return(NULL)
      ssp_key   <- .normalise_ssp(nm)
      ssp_short <- ssp_short_map[ssp_key] %||% ssp_key
      yr        <- .parse_year(nm)
      s$pt_key    <- paste0(ssp_short, "\n", yr)
      s$source    <- source_label
      s$ssp_short <- ssp_short
      s$yr        <- yr
      s
    })
    dplyr::bind_rows(Filter(Negate(is.null), rows))
  }

  base_fut <- build_fut(baseline_scenarios, "Baseline")
  pol_fut  <- build_fut(policy_scenarios,   "Policy")
  fut_df   <- dplyr::bind_rows(base_fut, pol_fut)

  # ---- x-axis factor levels with spacers ----------------------------------
  ordered_levels <- "Historical"
  spacer_ids     <- character(0)

  if (!is.null(fut_df) && nrow(fut_df) > 0) {
    if (isTRUE(group_order == "year_x_scenario")) {
      yrs_present  <- sort(unique(fut_df$yr))
      ssps_present <- intersect(c("SSP2", "SSP3", "SSP5"),
                                unique(fut_df$ssp_short))
      spacer_n <- 0L
      for (yr_i in yrs_present) {
        spacer_n       <- spacer_n + 1L
        sid            <- strrep(" ", spacer_n)
        spacer_ids     <- c(spacer_ids, sid)
        ordered_levels <- c(ordered_levels, sid)
        for (ssp in ssps_present) {
          ordered_levels <- c(ordered_levels, paste0(ssp, "\n", yr_i))
        }
      }
    } else {
      ssps_present <- intersect(c("SSP2", "SSP3", "SSP5"),
                                unique(fut_df$ssp_short))
      spacer_n <- 0L
      for (ssp in ssps_present) {
        spacer_n       <- spacer_n + 1L
        sid            <- strrep(" ", spacer_n)
        spacer_ids     <- c(spacer_ids, sid)
        ordered_levels <- c(ordered_levels, sid)
        ssp_yrs <- sort(unique(fut_df$yr[fut_df$ssp_short == ssp]))
        for (yr_i in ssp_yrs) {
          ordered_levels <- c(ordered_levels, paste0(ssp, "\n", yr_i))
        }
      }
    }
  }

  x_label_map <- setNames(
    vapply(ordered_levels, function(lv) {
      if (lv %in% spacer_ids) return("")
      lv
    }, character(1)),
    ordered_levels
  )
  data_levels <- setdiff(ordered_levels, spacer_ids)

  # ---- combine into one data frame ----------------------------------------
  cols     <- c("pt_key", "source", "mean", "lo90", "hi90", "lo95", "hi95")
  hist_row <- hist_s[, cols, drop = FALSE]
  fut_rows <- if (!is.null(fut_df) && nrow(fut_df) > 0)
    fut_df[, cols, drop = FALSE] else NULL

  plot_df <- dplyr::bind_rows(hist_row, fut_rows)
  plot_df$pt_key <- factor(plot_df$pt_key, levels = ordered_levels)
  plot_df$source <- factor(plot_df$source, levels = c("Baseline", "Policy"))
  plot_df <- plot_df[plot_df$pt_key %in% data_levels, ]

  if (nrow(plot_df) == 0)
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Run a future simulation to see comparisons."))

  hist_layer <- plot_df[plot_df$pt_key == "Historical", ]
  fut_layer  <- plot_df[plot_df$pt_key != "Historical", ]
  pos        <- ggplot2::position_dodge(width = 0.55)

  p <- ggplot2::ggplot(plot_df,
         ggplot2::aes(x = .data$pt_key, y = .data$mean,
                      colour = .data$source))

  # Historical (no dodge — single dot, baseline only) ---------------------
  if (nrow(hist_layer) > 0) {
    p <- p +
      ggplot2::geom_linerange(
        data = hist_layer,
        ggplot2::aes(ymin = .data$lo95, ymax = .data$hi95),
        linewidth = 0.5, colour = "grey70"
      ) +
      ggplot2::geom_linerange(
        data = hist_layer,
        ggplot2::aes(ymin = .data$lo90, ymax = .data$hi90),
        linewidth = 2.0
      ) +
      ggplot2::geom_point(
        data = hist_layer,
        shape = 21, fill = "white", size = 3, stroke = 1.4
      )
  }

  # Future scenarios (dodge baseline vs policy) ---------------------------
  if (nrow(fut_layer) > 0) {
    p <- p +
      ggplot2::geom_linerange(
        data = fut_layer,
        ggplot2::aes(ymin = .data$lo95, ymax = .data$hi95,
                     group = .data$source),
        linewidth = 0.5, colour = "grey70",
        position = pos
      ) +
      ggplot2::geom_linerange(
        data = fut_layer,
        ggplot2::aes(ymin = .data$lo90, ymax = .data$hi90),
        linewidth = 2.0, position = pos
      ) +
      ggplot2::geom_point(
        data = fut_layer,
        shape = 21, fill = "white", size = 3, stroke = 1.4,
        position = pos
      )
  }

  p +
    ggplot2::geom_hline(
      yintercept = hist_mean, linetype = "dashed",
      colour = "#808080", linewidth = 0.55
    ) +
    ggplot2::scale_colour_manual(
      values = c("Baseline" = "#808080", "Policy" = "#d32f2f"),
      name   = NULL,
      drop   = FALSE
    ) +
    ggplot2::scale_x_discrete(
      limits = ordered_levels,
      labels = x_label_map,
      drop   = FALSE
    ) +
    ggplot2::labs(title = NULL, x = NULL, y = hist_agg$x_label) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.x        = ggplot2::element_text(size = 10),
      legend.position    = "top"
    )
}

# ---------------------------------------------------------------------------- #
# Threshold table data frame: baseline + policy with Source column             #
# ---------------------------------------------------------------------------- #

#' Build Return-Period Threshold Table Data Frame (Baseline + Policy)
#'
#' Builds a single sorted data frame with one row per (scenario, source)
#' combination. The Historical entry is included only from the baseline
#' series. Future scenarios contribute two rows each: one Baseline, one
#' Policy.
#'
#' @param baseline_all_series Named list of aggregated baseline series
#'   (Historical + SSP scenarios). Each element must have \code{$out$value}.
#' @param policy_all_series   Named list of aggregated policy series. The
#'   Historical entry, if present, is skipped.
#' @param group_order Character. \code{"scenario_x_year"} (default) or
#'   \code{"year_x_scenario"}.
#'
#' @return A data frame with columns: Scenario, Source, Obs, one column
#'   per return-period threshold label. Returns NULL when every series has
#'   insufficient data.
#'
#' @importFrom stats quantile median
#' @export
pol_build_threshold_table_df <- function(baseline_all_series,
                                         policy_all_series,
                                         group_order = "scenario_x_year") {

  build_rows <- function(all_series, source_label, drop_historical = FALSE) {
    if (length(all_series) == 0) return(list())
    lapply(names(all_series), function(nm) {
      is_hist <- !grepl("^SSP", nm)
      if (drop_historical && is_hist) return(NULL)

      vals <- all_series[[nm]]$out$value
      vals <- vals[is.finite(vals)]
      n    <- length(vals)

      keep_low  <- names(RP_LOW)[c(n >= 50, n >= 20, n >= 10, n >= 5)]
      keep_high <- names(RP_HIGH)[c(n >= 5, n >= 10, n >= 20, n >= 50)]
      if (length(keep_low) == 0 && length(keep_high) == 0) return(NULL)

      low_df <- if (length(keep_low) > 0)
        as.data.frame(t(sapply(keep_low,
          function(th) round(stats::quantile(vals, RP_LOW[th]), 3))))
      else data.frame()
      high_df <- if (length(keep_high) > 0)
        as.data.frame(t(sapply(keep_high,
          function(th) round(stats::quantile(vals, RP_HIGH[th]), 3))))
      else data.frame()

      names(low_df)  <- keep_low
      names(high_df) <- keep_high
      median_df <- data.frame("1:1" = round(stats::median(vals), 3),
                              check.names = FALSE)

      cbind(
        data.frame(Scenario = nm, Source = source_label, Obs = n,
                   check.names = FALSE),
        low_df, median_df, high_df
      )
    })
  }

  # Baseline rows include Historical; policy rows skip it.
  rows_b <- Filter(Negate(is.null),
                   build_rows(baseline_all_series, "Baseline",
                              drop_historical = FALSE))
  rows_p <- Filter(Negate(is.null),
                   build_rows(policy_all_series, "Policy",
                              drop_historical = TRUE))
  rows <- c(rows_b, rows_p)
  if (length(rows) == 0) return(NULL)

  all_cols <- unique(unlist(lapply(rows, names)))
  rows <- lapply(rows, function(r) {
    for (m in setdiff(all_cols, names(r))) r[[m]] <- NA_real_
    r[all_cols]
  })
  df <- do.call(rbind, rows)

  # Sort: Historical first, then SSP rows by group_order, then by Source.
  hist_rows <- df[!grepl("^SSP", df$Scenario), , drop = FALSE]
  ssp_rows  <- df[grepl("^SSP", df$Scenario), , drop = FALSE]

  if (nrow(ssp_rows) > 0) {
    ssp_rows$ssp_sort <- sub(" /.*", "", ssp_rows$Scenario)
    yr_m <- regexpr("[0-9]{4}-[0-9]{4}", ssp_rows$Scenario)
    ssp_rows$yr_sort  <- ifelse(
      yr_m > 0,
      regmatches(ssp_rows$Scenario, yr_m),
      regmatches(ssp_rows$Scenario, regexpr("[0-9]{4}", ssp_rows$Scenario))
    )
    pct_m <- regexpr("P([0-9]+)$", ssp_rows$Scenario)
    ssp_rows$pct_sort <- ifelse(
      pct_m > 0,
      as.integer(regmatches(ssp_rows$Scenario,
                            regexpr("[0-9]+$", ssp_rows$Scenario))),
      50L
    )
    ssp_rows <- if (isTRUE(group_order == "year_x_scenario"))
      ssp_rows[order(ssp_rows$yr_sort, ssp_rows$ssp_sort,
                     ssp_rows$pct_sort, ssp_rows$Source), ]
    else
      ssp_rows[order(ssp_rows$ssp_sort, ssp_rows$yr_sort,
                     ssp_rows$pct_sort, ssp_rows$Source), ]
    ssp_rows$ssp_sort <- NULL
    ssp_rows$yr_sort  <- NULL
    ssp_rows$pct_sort <- NULL
  }

  rbind(hist_rows, ssp_rows)
}

# ---------------------------------------------------------------------------- #
# Exceedance curve: baseline (transparent) + policy (opaque) overlays          #
# ---------------------------------------------------------------------------- #

#' Enhanced Exceedance Probability Curve \u2014 Baseline vs Policy
#'
#' Same colour-by-SSP / linetype-by-period scheme as the Step 2 exceedance
#' curve, with an additional alpha mapping for source: baseline is rendered
#' more transparent, policy fully opaque. The Historical entry is drawn
#' once from the baseline data only (policy adjustments do not apply to
#' historical predictions).
#'
#' @param baseline_scenarios Named list of aggregated baseline series
#'   (Historical + SSP scenarios).
#' @param policy_scenarios   Named list of aggregated policy series. The
#'   Historical entry, if present, is skipped.
#' @param hist_agg           Historical aggregate (baseline) \u2014 used only
#'   for the historical-mean reference line.
#' @param x_label            Axis label for the welfare outcome.
#' @param return_period      Logical. Show return period lines. Default TRUE.
#' @param n_sim_years        Integer. Triggers reliability annotation.
#' @param logit_x            Logical. Use logit scale on the probability
#'   axis. Default FALSE.
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_line geom_vline annotate
#'   labs theme_minimal theme scale_color_manual scale_linetype_manual
#'   scale_alpha_manual coord_flip guide_legend element_text
#' @importFrom scales logit_trans
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#' @export
pol_enhance_exceedance <- function(baseline_scenarios,
                                   policy_scenarios,
                                   hist_agg,
                                   x_label,
                                   return_period = TRUE,
                                   n_sim_years   = NULL,
                                   logit_x       = FALSE) {

  stopifnot(is.list(baseline_scenarios), length(baseline_scenarios) > 0)

  build_long <- function(scenarios, source_label, drop_historical = FALSE) {
    if (length(scenarios) == 0) return(NULL)
    labels <- names(scenarios)
    rows <- lapply(seq_along(scenarios), function(i) {
      nm      <- labels[i]
      vals    <- scenarios[[nm]]$out$value
      ssp_key <- .normalise_ssp(nm)
      yr      <- .parse_year(nm)
      is_hist <- is.na(ssp_key)
      if (drop_historical && is_hist) return(NULL)
      data.frame(
        value   = vals,
        group   = nm,
        source  = source_label,
        ssp_key = if (is_hist) "Historical" else ssp_key,
        yr      = if (is.na(yr)) "Historical" else yr,
        stringsAsFactors = FALSE
      )
    })
    dplyr::bind_rows(Filter(Negate(is.null), rows))
  }

  base_long <- build_long(baseline_scenarios, "Baseline",
                          drop_historical = FALSE)
  pol_long  <- build_long(policy_scenarios,   "Policy",
                          drop_historical = TRUE)
  long_df   <- dplyr::bind_rows(base_long, pol_long)
  if (is.null(long_df) || nrow(long_df) == 0)
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Run a simulation to see exceedance probabilities."))

  long_df$source <- factor(long_df$source, levels = c("Baseline", "Policy"))

  # ---- Aesthetic mappings -------------------------------------------------
  all_yr_labels <- sort(unique(long_df$yr))
  yr_styles     <- .resolve_year_styles(all_yr_labels)
  ltype_map_yr  <- yr_styles$linetype_map
  present_yrs   <- names(ltype_map_yr)

  present_ssps <- sort(unique(long_df$ssp_key[long_df$ssp_key != "Historical"]))
  colour_map_ssp <- c(
    "Historical" = "black",
    .ssp_colours[intersect(names(.ssp_colours), present_ssps)]
  )

  hist_mean <- mean(hist_agg$out$value, na.rm = TRUE)
  eps       <- if (isTRUE(logit_x)) 0.005 else 0

  # ---- Build ECDF per (group, source) -------------------------------------
  grp_src_keys <- unique(paste(long_df$group, long_df$source, sep = "::"))
  ecdf_df <- dplyr::bind_rows(lapply(grp_src_keys, function(gs) {
    parts <- strsplit(gs, "::", fixed = TRUE)[[1]]
    grp   <- parts[1]
    src   <- parts[2]
    sub   <- long_df[long_df$group == grp & long_df$source == src &
                     is.finite(long_df$value), ]
    if (nrow(sub) == 0) return(NULL)
    fn     <- stats::ecdf(sub$value)
    xs     <- sort(unique(sub$value))
    exceed <- 1 - fn(xs)
    keep   <- exceed > eps & exceed < (1 - eps)
    if (!any(keep)) return(NULL)
    data.frame(
      value   = xs[keep],
      exceed  = exceed[keep],
      group   = grp,
      source  = src,
      ssp_key = sub$ssp_key[1],
      yr      = sub$yr[1],
      stringsAsFactors = FALSE
    )
  }))

  if (is.null(ecdf_df) || nrow(ecdf_df) == 0)
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Run a simulation to see exceedance probabilities."))

  ecdf_df$source  <- factor(ecdf_df$source, levels = c("Baseline", "Policy"))
  ecdf_df$line_id <- paste(ecdf_df$group, ecdf_df$source, sep = " \u2014 ")

  ann_y <- if (isTRUE(logit_x)) 0.97 else 0.95

  p <- ggplot2::ggplot(
    ecdf_df,
    ggplot2::aes(
      x        = .data$value,
      y        = .data$exceed,
      colour   = .data$ssp_key,
      linetype = .data$yr,
      alpha    = .data$source,
      group    = .data$line_id
    )
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_vline(
      xintercept = hist_mean, linetype = "dotted",
      colour = "black", linewidth = 0.5
    ) +
    ggplot2::annotate(
      "text", x = hist_mean, y = ann_y,
      label = "Hist. mean", hjust = 1.05, vjust = -0.4,
      size = 2.8, colour = "grey30"
    ) +
    ggplot2::scale_color_manual(
      values = colour_map_ssp,
      breaks = c("Historical", present_ssps),
      labels = c("Historical", present_ssps),
      name   = "Climate scenario",
      guide  = ggplot2::guide_legend(order = 1,
                                     override.aes = list(linewidth = 0.9))
    ) +
    ggplot2::scale_linetype_manual(
      values = ltype_map_yr,
      breaks = present_yrs,
      labels = present_yrs,
      name   = "Period",
      guide  = ggplot2::guide_legend(order = 2,
                                     override.aes = list(linewidth = 0.9))
    ) +
    ggplot2::scale_alpha_manual(
      values = c("Baseline" = 0.4, "Policy" = 1.0),
      breaks = c("Baseline", "Policy"),
      labels = c("Baseline", "Policy"),
      name   = "Source",
      guide  = ggplot2::guide_legend(order = 3,
                                     override.aes = list(linewidth = 0.9))
    ) +
    ggplot2::labs(
      title = "Exceedance probability by climate scenario",
      x     = x_label,
      y     = "Annual exceedance probability"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = 12,
                                              hjust = 0.5),
      legend.position = "bottom"
    ) +
    ggplot2::coord_flip()

  # ---- Return period lines (both tails, symmetric) -----------------------
  if (isTRUE(return_period)) {
    rp_all <- c(RP_LOW, RP_HIGH)
    for (nm in names(rp_all)) {
      prob      <- rp_all[nm]
      reliable  <- is.null(n_sim_years) ||
        (!(nm == "1:20" && n_sim_years < 40) &&
         !(nm == "1:50" && n_sim_years < 100))
      rp_label  <- if (reliable) nm else paste0(nm, "*")
      label_col <- if (reliable) "grey40" else "grey65"
      p <- p +
        ggplot2::geom_hline(
          yintercept = prob, linetype = "dashed",
          colour = "grey60", linewidth = 0.35
        ) +
        ggplot2::annotate(
          "text", x = -Inf, y = prob, label = rp_label,
          hjust = -0.1, vjust = -0.3, size = 2.8, colour = label_col
        )
    }
    if (!is.null(n_sim_years) && n_sim_years < 100) {
      p <- p + ggplot2::annotate(
        "text", x = Inf, y = 0.02,
        label  = paste0("\u26a0 unreliable (n = ", n_sim_years, " yrs)"),
        hjust  = 1.05, vjust = 1.5, size = 2.8, colour = "grey50"
      )
    }
  }

  if (isTRUE(logit_x)) {
    logit_breaks <- c(unname(RP_LOW), 0.50, rev(1 - unname(RP_LOW)))
    logit_labels <- c(names(RP_LOW), "Median", rev(names(RP_HIGH)))
    p <- p + ggplot2::scale_y_continuous(
      trans  = scales::logit_trans(),
      breaks = logit_breaks,
      labels = logit_labels,
      limits = c(0.005, 0.995)
    )
  }
  p
}