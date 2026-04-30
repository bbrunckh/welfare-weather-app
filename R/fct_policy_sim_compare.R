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


#' Render the UI block for one results pane (baseline or policy).
#'
#' Inputs and outputs are namespaced via \code{ns(paste0(p, "_..."))}.
#' Mirrors mod_2_02_results' .results_content_ui structure.
#' @noRd
.results_pane_ui <- function(ns, p, so) {
  pid <- function(suffix) ns(paste0(p, "_", suffix))
  tagList(
    shiny::uiOutput(pid("results_header_ui")),
    shiny::wellPanel(
      style = "padding: 10px 14px 6px 14px;",
      shiny::tags$p("Outcome of interest:",
                    style = "font-weight:600; margin-bottom:4px;"),
      shiny::tags$div(
        style = "display:flex; align-items:flex-end; gap:12px; flex-wrap:wrap;",
        shiny::tags$div(style = "flex:1; min-width:160px;",
          shiny::selectInput(
            pid("cmp_agg_method"),
            label    = "Aggregation method",
            choices  = hist_aggregate_choices(so$type, so$name),
            selected = "mean"
          )
        ),
        shiny::tags$div(style = "flex:1; min-width:160px;",
          shiny::selectInput(
            pid("cmp_deviation"),
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
          shiny::uiOutput(pid("cmp_pov_line_ui"))
        )
      ),
      shiny::tags$hr(style = "margin: 6px 0;"),
      shiny::tags$p("Scenario Filters",
                    style = "font-weight:600; margin-bottom:4px;"),
      shiny::fluidRow(
        shiny::column(12, shiny::uiOutput(pid("scenario_filter_ui")))
      ),
      shiny::tags$hr(style = "margin: 6px 0;"),
      shiny::checkboxInput(
        pid("use_weights"),
        label = "Use survey weights (if available)",
        value = TRUE
      ),
      shiny::uiOutput(pid("weight_status_ui")),
      shiny::radioButtons(
        pid("cmp_group_order"),
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
      shiny::plotOutput(pid("summary_box_plot"), height = "600px"),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:6px;",
        "The central dot shows the mean annual simulated value; the thick",
        "spans the 95% interval (P2.5-P97.5).",
        shiny::tags$br(),
        "Future scenarios apply climate-adjusted shifts to the same historical annual base.",
        shiny::tags$br(),
        "Dashed line = historical mean."
      )
    ),
    shiny::wellPanel(
      shiny::h4("Exceedance probability by climate scenario"),
      shiny::tags$div(
        style = "display:flex; gap:20px; flex-wrap:wrap; margin-bottom:6px;",
        shiny::checkboxInput(
          pid("exceedance_logit_x"),
          "Logit probability axis (emphasise both tails)",
          value = FALSE
        ),
        shiny::checkboxInput(
          pid("show_return_period"),
          "Show return period lines",
          value = TRUE
        )
      ),
      shiny::plotOutput(pid("exceedance_plot"), height = "400px"),
      shiny::uiOutput(pid("exceedance_caption"))
    ),
    shiny::wellPanel(
      shiny::uiOutput(pid("threshold_table_header")),
      DT::DTOutput(pid("summary_threshold_table")),
      shiny::uiOutput(pid("threshold_table_footer"))
    )
  )
}


#' Wire reactives and output bindings for one results pane.
#'
#' Mirrors mod_2_02_results' server-side reactives and outputs but uses
#' a per-pane prefix so baseline and policy panes coexist in one module.
#' @noRd
.wire_results_pane <- function(input, output, session, p,
                               hist_sim, saved_scenarios, selected_hist) {
  ns <- session$ns
  iid <- function(suffix) paste0(p, "_", suffix)

  hist_label <- reactive({
    nm <- if (!is.null(selected_hist)) selected_hist()$scenario_name else NULL
    if (!is.null(nm) && nzchar(nm)) nm else "Historical"
  })

  all_ssps <- reactive({
    sc <- saved_scenarios()
    if (length(sc) == 0) return(character(0))
    ssps <- unique(.normalise_ssp(names(sc)))
    sort(ssps[!is.na(ssps) & grepl("^SSP", ssps)])
  })

  all_anchor_years <- reactive({
    sc <- saved_scenarios()
    if (length(sc) == 0) return(character(0))
    ranges <- sort(na.omit(unique(.parse_year(names(sc)))))
    setNames(sub("-", "_", ranges), ranges)
  })

  all_models_info <- reactive({
    sc <- saved_scenarios()
    if (length(sc) == 0) return(character(0))
    vapply(sc, function(s) s$n_models %||% 1L, integer(1))
  })

  weight_col <- reactive({
    req(hist_sim())
    if (!isTRUE(input[[iid("use_weights")]])) return(NULL)
    if ("weight" %in% names(hist_sim()$preds)) "weight" else NULL
  })

  output[[iid("weight_status_ui")]] <- shiny::renderUI({
    req(hist_sim())
    has_w  <- "weight" %in% names(hist_sim()$preds)
    tog_on <- isTRUE(input[[iid("use_weights")]])
    if (has_w && tog_on)
      shiny::tags$p(
        style = "font-size:11px; color:#2e7d32; margin:2px 0 6px 0;",
        "✅ Survey weights found and applied (",
        shiny::tags$code("weight"), ")")
    else if (has_w && !tog_on)
      shiny::tags$p(
        style = "font-size:11px; color:#e65100; margin:2px 0 6px 0;",
        "⚠ Survey weights available but not applied")
    else
      shiny::tags$p(
        style = "font-size:11px; color:#c62828; margin:2px 0 6px 0;",
        "\U0001F534 No weight column found — unweighted")
  })

  pov_line_val <- reactive({
    if (isTRUE(input[[iid("cmp_agg_method")]] %in%
               c("headcount_ratio", "gap", "fgt2"))) {
      if (!is.null(hist_sim())) hist_sim()$pov_line %||% NULL else NULL
    } else NULL
  })

  selected_scenario_names <- reactive({
    sc <- saved_scenarios()
    if (length(sc) == 0) return(character(0))
    nms  <- names(sc)
    ssps <- if (length(input[[iid("filter_ssp")]]) == 0) all_ssps()
            else input[[iid("filter_ssp")]]
    yr_vals <- if (length(input[[iid("filter_year")]]) == 0)
                 names(all_anchor_years())
               else sub("_", "-", input[[iid("filter_year")]])
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

  agg_hist <- reactive({
    req(hist_sim())
    method    <- input[[iid("cmp_agg_method")]] %||% "mean"
    deviation <- input[[iid("cmp_deviation")]]  %||% "none"
    use_w     <- isTRUE(input[[iid("use_weights")]])
    req(!is.null(hist_sim()$agg))

    out <- dplyr::filter(hist_sim()$agg,
                         .data$agg_method == method,
                         .data$weighted   == use_w)
    if (nrow(out) == 0L && isTRUE(use_w)) {
      out <- dplyr::filter(hist_sim()$agg,
                           .data$agg_method == method,
                           .data$weighted   == FALSE)
    }
    if (!identical(deviation, "none") && nrow(out) > 0) {
      out <- deviation_from_centre(
        df     = out,
        group  = "sim_year",
        centre = deviation,
        loss   = FALSE
      )
      if ("deviation" %in% names(out)) {
        out$value <- out$deviation
        out$deviation <- NULL
      }
    }
    x_label <- if (identical(deviation, "none")) {
      label_agg_method(method)
    } else {
      paste0(label_agg_method(method), " — ", label_deviation(deviation))
    }
    list(out = out, x_label = x_label)
  })

  agg_scenarios <- reactive({
    sc <- saved_scenarios()
    if (length(sc) == 0) return(list())
    method    <- input[[iid("cmp_agg_method")]] %||% "mean"
    deviation <- input[[iid("cmp_deviation")]]  %||% "none"
    use_w     <- isTRUE(input[[iid("use_weights")]])
    x_label <- if (identical(deviation, "none")) {
      label_agg_method(method)
    } else {
      paste0(label_agg_method(method), " — ", label_deviation(deviation))
    }

    lapply(sc, function(s) {
      tryCatch({
        if (is.null(s$agg)) return(NULL)
        out <- dplyr::filter(s$agg,
                             .data$agg_method == method,
                             .data$weighted   == use_w)
        if (nrow(out) == 0L && isTRUE(use_w)) {
          out <- dplyr::filter(s$agg,
                               .data$agg_method == method,
                               .data$weighted   == FALSE)
        }
        if (!identical(deviation, "none") && nrow(out) > 0) {
          hist_none <- tryCatch(
            dplyr::filter(hist_sim()$agg,
                          .data$agg_method == method,
                          .data$weighted   == use_w),
            error = function(e) NULL
          )
          if (!is.null(hist_none) && nrow(hist_none) == 0) {
            hist_none <- tryCatch(
              dplyr::filter(hist_sim()$agg,
                            .data$agg_method == method,
                            .data$weighted   == FALSE),
              error = function(e) NULL
            )
          }
          hist_ref <- if (!is.null(hist_none) && nrow(hist_none) > 0) {
            if (identical(deviation, "mean"))
              mean(hist_none$value, na.rm = TRUE)
            else if (identical(deviation, "median"))
              median(hist_none$value, na.rm = TRUE)
            else NA_real_
          } else NA_real_
          if (!is.na(hist_ref)) {
            out <- dplyr::mutate(out, value = .data$value - hist_ref)
          } else {
            out <- deviation_from_centre(
              df = out, group = "sim_year", centre = deviation, loss = FALSE
            )
            if ("deviation" %in% names(out)) {
              out$value <- out$deviation; out$deviation <- NULL
            }
          }
        }
        list(out = out, x_label = x_label)
      }, error = function(e) NULL)
    })
  })

  all_series <- reactive({
    sc  <- agg_scenarios()
    sel <- selected_scenario_names()
    c(setNames(list(agg_hist()), hist_label()),
      sc[intersect(sel, names(sc))])
  })

  table_subtitle <- reactive({
    req(agg_hist(),
        input[[iid("cmp_agg_method")]],
        input[[iid("cmp_deviation")]])
    paste0(
      agg_hist()$x_label, " — ",
      label_agg_method(input[[iid("cmp_agg_method")]]), " | ",
      label_deviation(input[[iid("cmp_deviation")]])
    )
  })

  output[[iid("results_header_ui")]] <- renderUI({
    req(hist_sim(),
        input[[iid("cmp_agg_method")]],
        input[[iid("cmp_deviation")]])
    so <- hist_sim()$so
    agg_label <- label_agg_method(input[[iid("cmp_agg_method")]])
    dev_label <- label_deviation(input[[iid("cmp_deviation")]])
    pov_txt   <- if (!is.null(pov_line_val()))
      paste0(" | Poverty line: $", pov_line_val(), "/day") else ""
    notes_txt <- paste0(
      "Showing ", agg_label, " of ", so$label %||% so$name,
      " expressed as ", dev_label, pov_txt, "."
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

  output[[iid("cmp_pov_line_ui")]] <- renderUI({
    req(input[[iid("cmp_agg_method")]])
    if (input[[iid("cmp_agg_method")]] %in%
        c("headcount_ratio", "gap", "fgt2")) {
      pov_val <- if (!is.null(hist_sim()$pov_line))
        sprintf("$%.2f", hist_sim()$pov_line) else "not yet set"
      shiny::helpText(
        style = "font-size:11px; color:#555; margin-bottom:6px;",
        paste0("Poverty line: ", pov_val,
               "/day (set at simulation time).")
      )
    }
  })

  output[[iid("scenario_filter_ui")]] <- renderUI({
    sc <- saved_scenarios()
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
              ns(iid("filter_year")), label = "Projection periods",
              choices = yrs, selected = yrs, inline = TRUE
            )
        ),
        shiny::column(4,
          if (length(ssps) > 0)
            shiny::checkboxGroupInput(
              ns(iid("filter_ssp")), label = "SSPs",
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

  output[[iid("summary_box_plot")]] <- renderPlot({
    req(agg_hist())
    plot_pointrange_climate(
      scenarios   = all_series(),
      hist_agg    = agg_hist(),
      group_order = input[[iid("cmp_group_order")]] %||% "scenario_x_year"
    )
  }, height = 600)
  outputOptions(output, iid("summary_box_plot"), suspendWhenHidden = FALSE)

  output[[iid("summary_threshold_table")]] <- DT::renderDT({
    req(agg_hist())
    df <- build_threshold_table_df(
      all_series  = all_series(),
      group_order = input[[iid("cmp_group_order")]] %||% "scenario_x_year"
    )
    if (is.null(df))
      return(DT::datatable(data.frame(Message = "Insufficient data"),
                           rownames = FALSE, class = "compact stripe",
                           options  = list(dom = "t")))
    DT::datatable(
      df, rownames = FALSE, class = "compact stripe",
      options = list(
        pageLength = 15, dom = "t", ordering = FALSE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      )
    )
  })
  outputOptions(output, iid("summary_threshold_table"),
                suspendWhenHidden = FALSE)

  output[[iid("threshold_table_header")]] <- renderUI({
    req(agg_hist())
    tagList(
      shiny::h4("Outcome value at return-period thresholds (both tails)"),
      shiny::tags$small(class = "text-muted", table_subtitle())
    )
  })

  output[[iid("threshold_table_footer")]] <- renderUI({
    req(agg_hist())
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

  output[[iid("exceedance_plot")]] <- renderPlot({
    req(agg_hist())
    enhance_exceedance(
      scenarios     = all_series(),
      hist_agg      = agg_hist(),
      x_label       = agg_hist()$x_label,
      return_period = isTRUE(input[[iid("show_return_period")]]),
      n_sim_years   = nrow(agg_hist()$out),
      logit_x       = isTRUE(input[[iid("exceedance_logit_x")]])
    )
  })
  outputOptions(output, iid("exceedance_plot"), suspendWhenHidden = FALSE)

  output[[iid("exceedance_caption")]] <- renderUI({
    req(agg_hist())
    axis_txt <- if (isTRUE(input[[iid("exceedance_logit_x")]]))
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