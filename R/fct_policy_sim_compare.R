
#' Make a before/after histogram for a single variable
#' @noRd
.make_before_after_hist <- function(baseline_vals, policy_vals,
                                    var_name) {
  baseline_clean <- baseline_vals[!is.na(baseline_vals)]
  policy_clean   <- policy_vals[!is.na(policy_vals)]
  all_vals       <- c(baseline_clean, policy_clean)

  blank_plot <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = msg,
                        size = 4, colour = "grey40") +
      ggplot2::theme_void()
  }

  if (length(all_vals) == 0) return(blank_plot("No data available"))

  fill_vals <- c(Baseline = "#bdbdbd", `Policy-adjusted` = "#d32f2f")
  uniq_vals <- unique(all_vals)
  is_binary <- length(uniq_vals) <= 2 && all(uniq_vals %in% c(0, 1))

  # ---- Binary: grouped bar plot of proportions -----------------------------
  if (is_binary) {
    df <- data.frame(
      Group = factor(rep(c("Baseline", "Policy-adjusted"), each = 2),
                     levels = c("Baseline", "Policy-adjusted")),
      Value = factor(rep(c("0", "1"), 2), levels = c("0", "1")),
      Proportion = c(
        if (length(baseline_clean)) mean(baseline_clean == 0) else NA_real_,
        if (length(baseline_clean)) mean(baseline_clean == 1) else NA_real_,
        if (length(policy_clean))   mean(policy_clean   == 0) else NA_real_,
        if (length(policy_clean))   mean(policy_clean   == 1) else NA_real_
      )
    )

    return(
      ggplot2::ggplot(df, ggplot2::aes(x = Value, y = Proportion,
                                       fill = Group)) +
        ggplot2::geom_col(
          position = ggplot2::position_dodge(width = 0.75),
          width    = 0.65,
          colour   = NA
        ) +
        ggplot2::scale_fill_manual(values = fill_vals) +
        ggplot2::scale_y_continuous(limits = c(0, 1),
                                    expand = ggplot2::expansion(c(0, 0.05))) +
        ggplot2::labs(
          x     = var_name,
          y     = "Proportion",
          fill  = NULL
        ) +
        theme_wise(base_size = 12) +
        ggplot2::theme(
          legend.position    = "top",
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank()
        )
    )
  }

  # ---- Continuous: ridge density (Policy on top, Baseline on bottom) -------
  use_log <- all(all_vals > 0)

  df <- data.frame(
    Group = factor(
      c(rep("Baseline", length(baseline_clean)),
        rep("Policy-adjusted", length(policy_clean))),
      # ggridges renders the FIRST level at the bottom, last at the top -
      # so "Baseline" first puts policy-adjusted on top.
      levels = c("Baseline", "Policy-adjusted")
    ),
    Value = c(baseline_clean, policy_clean)
  )

  if (use_log) df <- df[df$Value > 0, , drop = FALSE]
  if (!nrow(df)) return(blank_plot("No data available"))

  # Pre-compute the bandwidth ggridges would otherwise pick (and announce
  # via `message()`). Passing it explicitly silences the chatty
  # "Picking joint bandwidth of ..." note without changing the visual.
  bw <- tryCatch(stats::bw.nrd0(df$Value), error = function(e) NULL)
  if (is.null(bw) || !is.finite(bw) || bw <= 0) bw <- NULL

  p <- ggplot2::ggplot(df, ggplot2::aes(x = Value, y = Group, fill = Group)) +
    ggridges::geom_density_ridges(alpha = 0.7, scale = 1.5, bandwidth = bw) +
    ggplot2::scale_fill_manual(values = fill_vals) +
    ggplot2::labs(
      x     = if (use_log) paste0(var_name, " (log scale)") else var_name,
      y     = "",
      fill  = NULL
    ) +
    theme_wise(base_size = 12) +
    ggplot2::theme(
      legend.position = "none"
    )

  if (use_log) {
    p <- p + ggplot2::scale_x_log10(labels = scales::comma_format())
  }
  p
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
      shiny::uiOutput(ns("stale_banner_ui")),
      shiny::uiOutput(ns("results_header_ui")),
      shiny::wellPanel(
        class = "results-controls",
      style = "padding: 10px 14px 6px 14px;",
      # Single compact row: outcome + uncertainty controls wrap as needed
      shiny::tags$div(
        style = "display:flex; align-items:flex-end; gap:12px; flex-wrap:wrap;",
        shiny::tags$div(style = "flex:0 1 180px;",
          shiny::selectInput(
            ns("cmp_agg_method"),
            label    = "Aggregation method",
            choices  = hist_aggregate_choices(so$type, so$name),
            selected = "mean"
          )
        ),
        shiny::tags$div(style = "flex:0 1 170px;",
          shiny::uiOutput(ns("cmp_pov_line_ui"))
        ),
        shiny::tags$div(style = "flex:0 1 200px;",
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
        shiny::tags$div(style = "flex:0 1 170px;",
          shiny::selectInput(
            ns("uncertainty_band"),
            label   = "Coefficient band",
            choices = c(
              "50% (p25-p75)"   = "p25_p75",
              "60% (p20-p80)"   = "p20_p80",
              "80% (p10-p90)"   = "p10_p90",
              "90% (p05-p95)"   = "p05_p95",
              "95% (p025-p975)" = "p025_p975",
              "99% (p005-p995)" = "p005_p995",
              "Max (min-max)"   = "minmax"
            ),
            selected = "p10_p90"
          )
        ),
        shiny::tags$div(style = "flex:0 1 180px;",
          shiny::selectInput(
            ns("ensemble_band"),
            label    = "Inter-model band",
            choices  = c(
              "50% (p25-p75)"   = "p25_p75",
              "60% (p20-p80)"   = "p20_p80",
              "80% (p10-p90)"   = "p10_p90",
              "90% (p05-p95)"   = "p05_p95",
              "95% (p025-p975)" = "p025_p975",
              "99% (p005-p995)" = "p005_p995",
              "Full range (min-max)" = "minmax"
            ),
            selected = "minmax"
          )
        ),
        shiny::tags$div(
          style = "flex:0 0 auto; padding-bottom:2px;",
          shiny::checkboxInput(
            ns("show_coef_uncertainty"),
            label = "Show coefficient uncertainty",
            value = TRUE
          ),
          shiny::checkboxInput(
            ns("show_model_spread"),
            label = "Show inter-model spread",
            value = TRUE
          )
        )
      ),
      shiny::tags$details(
        shiny::tags$summary(
          style = "cursor:pointer; font-size:11px; color:#555; font-weight:600;",
          "Advanced \u25BC"
        ),
        shiny::radioButtons(
          ns("cmp_group_order"),
          label    = "Group charts and tables by",
          choices  = c(
            "Scenario \u00D7 Year" = "scenario_x_year",
            "Year \u00D7 Scenario" = "year_x_scenario"
          ),
          selected = "scenario_x_year",
          inline   = TRUE
        )
      ),
      shiny::tags$hr(style = "margin: 6px 0;"),
      shiny::tags$p("Scenario filters",
                    style = "font-weight:600; margin: 0 0 4px 0; font-size:12px;"),
      shiny::uiOutput(ns("scenario_filter_ui"))
    ),
    shiny::wellPanel(
      shiny::h4(
        "Distribution of outcome across weather conditions, by climate scenario",
        info_popover(
          title = "Reading this chart",
          shiny::p(
            "Each scenario shows two dots: ", shiny::tags$b("baseline (grey)"),
            " and ", shiny::tags$b("policy-adjusted (red)"),
            ". All bands are drawn relative to the dot (the ensemble-mean",
            " annual aggregate) and answer different questions about",
            " uncertainty. They are not meant to be added together - see",
            " the Diagnostics tab for how the sources combine."
          ),
          shiny::p(shiny::tags$b("Thick coloured band"),
            " (future scenarios only) - how much do climate models disagree?",
            " Inter-model spread: quantile across CMIP6 ensemble members of",
            " each model's time-mean. Can be asymmetric around the dot when",
            " models lean one way."),
          shiny::p(shiny::tags$b("Middle band"),
            " - how much does weather vary year-to-year within a typical",
            " model? Inter-annual variability: per-model quantile across",
            " simulation years, then averaged across models. Reflects the",
            " natural range of outcomes a single climate trajectory produces."),
          shiny::p(shiny::tags$b("Innermost line"),
            " (shown when coefficient uncertainty is enabled) - how precisely",
            " is each (model, year) aggregate estimated? Analytic per-outcome",
            " SE from the regression fit. By default, under 'original'",
            " residuals, restricted to coefficients on weather and the",
            " policy-modified variables, and their interactions",
            " (additive-decomposition SE - see Step 2 settings to widen to",
            " all coefficients). This is precision of a point estimate, not",
            " a spread of outcomes - conceptually distinct from the two",
            " coloured bands."),
          shiny::p(
            "Historical = single 'model', so no inter-model band is shown.",
            "Dashed line = historical mean. A pooled summary SE combining",
            "coefficient and inter-model uncertainty is available in the",
            "return-period table on the Diagnostics tab."
          ),
          docs = TRUE
        )
      ),
      shiny::plotOutput(ns("summary_box_plot"), height = "600px"),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:6px;",
        "Grey dot = baseline; red dot = policy-adjusted; bands = uncertainty ranges (not additive) - click ",
        shiny::icon("circle-info"), " above for details."
      )
    ),
    shiny::wellPanel(
      shiny::h4(
        "Exceedance probability by climate scenario",
        info_popover(
          title = "Exceedance probability",
          shiny::p(
            "Shows the probability that the outcome exceeds a given",
            "threshold, by scenario. The logit axis emphasises both tails;",
            "return period lines mark standard thresholds (e.g. 1-in-20-year",
            "events)."
          ),
          docs = TRUE
        )
      ),
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
    ),
    shiny::wellPanel(
      shiny::h4(
        "Per-model trajectories over simulation years",
        info_popover(
          title = "Reading this chart",
          shiny::p(
            "Thin lines = one CMIP6 member's annual trajectory; bold line =",
            "across-model median per simulation year. Baseline is rendered",
            "faded; policy-adjusted is fully opaque."
          ),
          docs = TRUE
        )
      ),
      shiny::plotOutput(ns("timeseries_plot"), height = "420px"),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:6px;",
        "Faded = baseline; opaque = policy-adjusted; bold = median trajectory."
      )
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
                               selected_hist,
                               residuals = reactive("original"),
                               stale = reactive(FALSE)) {
  ns <- session$ns

  # INT-08: stale banner above the results pane.
  output$stale_banner_ui <- shiny::renderUI({
    if (isTRUE(stale())) .stale_banner("Step 3 policy results") else NULL
  })

  # Resolve the residuals choice captured by the Step 2 run. The live control
  # is only a fallback for older in-memory result objects.
  active_residuals <- function(hs) {
    hs$residuals %||% residuals() %||% "original"
  }

  # INT-05: prefer the historical label captured by the Step 2 run; the live
  # selection is only a fallback for older in-memory result objects.
  hist_label <- reactive({
    hs  <- baseline_hist_sim()
    nm  <- hs$hist_label %||%
      (if (!is.null(selected_hist)) selected_hist()$scenario_name else NULL)
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

  # Survey weights are always applied when available (toggle removed).
  output$weight_status_ui <- shiny::renderUI(NULL)

  # Debounced (400 ms) so rapid spinner/typing edits don't retrigger the
  # aggregation pipeline on every keystroke. Non-poverty methods keep the
  # NULL behaviour so downstream consumers skip the poverty line.
  pov_line_val <- shiny::debounce(reactive({
    if (isTRUE(input$cmp_agg_method %in%
               c("headcount_ratio", "gap", "fgt2"))) {
      as.numeric(input$cmp_pov_line) %||% 3.00
    } else NULL
  }), 400)

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

  # ---- PERF-31: per-method aggregation cache -------------------------------
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
    baseline_hist_sim(); policy_hist_sim()
    baseline_saved_scenarios(); policy_saved_scenarios()
    new.env(parent = emptyenv())
  })
  .agg_cache_key <- function(tag, method, pov_line) {
    paste(tag, method, format(pov_line), sep = "\r")
  }

  agg_axis_label <- reactive({
    method    <- input$cmp_agg_method %||% "mean"
    deviation <- input$cmp_deviation  %||% "none"
    if (identical(deviation, "none")) label_agg_method(method)
    else paste0(label_agg_method(method), " \u2014 ",
                label_deviation(deviation))
  })

  # Helper: aggregate hist_sim into Mod 2's rich list-col schema
  # (one row per sim_year, list-cols value_all / value_all_sd / model_id,
  # plus scalar var_within / var_across). This lets us reuse Mod 2's
  # by_model_matrix() + downstream plot helpers verbatim.
  #
  # Both baseline (Mod 2 hist_sim, passed verbatim) and policy (re-simulated
  # by resimulate_with_svy) wrap their single historical run under $pipeline
  # - read it once here so the downstream code paths are identical.
  make_agg_hist <- function(hs, tag) {
    if (is.null(hs)) return(NULL)
    pl <- hs$pipeline
    if (is.null(pl) || is.null(pl$y_point)) return(NULL)
    method    <- input$cmp_agg_method %||% "mean"

    ws <- agg_cache_ws()
    hit <- get0(.agg_cache_key(tag, method, pov_line_val()), envir = ws)
    if (!is.null(hit)) return(hit)

    is_log <- isTRUE(hs$so$transform == "log")
    bq     <- c(lo = 0.10, hi = 0.90)

    per_yr <- aggregate_pipeline_per_year(
      pipe      = pl,
      method    = method,
      weighted  = TRUE,
      pov_line  = pov_line_val(),
      residuals = active_residuals(hs),
      is_log    = is_log,
      band_q    = bq
    )
    rows <- lapply(per_yr, function(m) {
      sd_yr <- sqrt((m$var_coef %||% 0) + (m$var_resid %||% 0))
      tibble::tibble(
        sim_year     = m$sim_year,
        value        = m$value,
        model_id     = list("Historical"),
        value_all    = list(m$value),
        value_all_sd = list(sd_yr),
        var_within   = sd_yr^2,
        var_across   = 0,
        scenario     = "Historical"
      )
    })
    agg <- dplyr::bind_rows(rows)
    res <- list(out = agg)
    assign(.agg_cache_key(tag, method, pov_line_val()), res, envir = ws)
    res
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
    if (identical(key, .agg_failure_state$last_key)) return(invisible(NULL))
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
    if (length(sc) == 0) return(list())
    method    <- input$cmp_agg_method %||% "mean"
    use_w     <- TRUE

    ws <- agg_cache_ws()
    hit <- get0(.agg_cache_key(tag, method, pov_line_val()), envir = ws)
    if (!is.null(hit)) return(hit)

    failed <- character(0)
    # NB: iterate by index (the error handler needs `names(sc)[i]`) but
    # re-attach the scenario names - every consumer below (all_series,
    # pointrange/timeseries/exceedance/threshold row builders) selects
    # scenarios by name, and lapply(seq_along(...)) drops them.
    res <- stats::setNames(lapply(seq_along(sc), function(i) {
      s <- sc[[i]]
      tryCatch({
        pipes <- s$pipelines
        if (is.null(pipes) || length(pipes) == 0L) return(NULL)
        is_log <- isTRUE(s$so$transform == "log")
        bq     <- c(lo = 0.10, hi = 0.90)
        yrs    <- sort(unique(pipes[[1L]]$sim_year))
        model_ids_all <- names(pipes) %||% paste0("model_", seq_along(pipes))

        # Aggregate each ensemble member across years once via the shared
        # helper, then pivot to a per-year x per-member structure for the
        # ensemble combination step below.
        res_mode <- active_residuals(hs_for_dev)
        per_member_per_yr <- lapply(pipes, function(pipe) {
          aggregate_pipeline_per_year(
            pipe      = pipe,
            method    = method,
            weighted  = use_w,
            pov_line  = pov_line_val(),
            residuals = res_mode,
            is_log    = is_log,
            band_q    = bq
          )
        })

        per_year_rows <- lapply(yrs, function(yr) {
          per_member <- lapply(per_member_per_yr, function(yr_list) {
            for (m in yr_list) if (identical(m$sim_year, yr)) return(m)
            NULL
          })
          keep <- !vapply(per_member, is.null, logical(1L))
          per_member <- per_member[keep]
          ids_yr     <- model_ids_all[keep]
          if (length(per_member) == 0L) return(NULL)
          comb <- combine_ensemble_results(per_member, band_q = bq)
          vals_m <- vapply(per_member, function(x) x$value, numeric(1L))
          sd_m   <- sqrt(pmax(
            vapply(per_member,
                   function(x) (x$var_coef %||% 0) + (x$var_resid %||% 0),
                   numeric(1L)), 0))
          tibble::tibble(
            sim_year     = yr,
            value        = mean(vals_m, na.rm = TRUE),
            model_id     = list(ids_yr),
            value_all    = list(vals_m),
            value_all_sd = list(sd_m),
            var_within   = comb$var_within %||% mean(sd_m^2, na.rm = TRUE),
            var_across   = comb$var_across %||%
                             (if (length(vals_m) > 1L)
                                stats::var(vals_m, na.rm = TRUE) else 0)
          )
        })
        combined <- dplyr::bind_rows(Filter(Negate(is.null), per_year_rows))
        if (nrow(combined) == 0L) return(NULL)
        list(out = combined)
      }, error = function(e) {
        nm <- s$scenario_name %||% names(sc)[i]
        if (is.null(nm) || is.na(nm)) nm <- paste0("scenario_", i)
        failed[[length(failed) + 1L]] <<- nm
        NULL
      })
    }), names(sc))
    .notify_agg_failures(failed, length(sc))
    assign(.agg_cache_key(tag, method, pov_line_val()), res, envir = ws)
    res
  }

  baseline_agg_hist <- reactive({
    req(baseline_hist_sim())
    make_agg_hist(baseline_hist_sim(), "baseline_hist")
  })
  policy_agg_hist <- reactive({
    req(policy_hist_sim())
    make_agg_hist(policy_hist_sim(), "policy_hist")
  })

  baseline_agg_scenarios <- reactive({
    req(baseline_hist_sim())
    make_agg_scenarios(baseline_saved_scenarios(), baseline_hist_sim(),
                       "baseline_scn")
  })
  policy_agg_scenarios <- reactive({
    req(policy_hist_sim())
    make_agg_scenarios(policy_saved_scenarios(), policy_hist_sim(),
                       "policy_scn")
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

  # ---- Shared deviation reference (baseline historical) -------------------
  hist_ref_val <- reactive({
    req(baseline_agg_hist())
    deviation <- input$cmp_deviation %||% "none"
    raw_vals  <- baseline_agg_hist()$out$value
    if (identical(deviation, "mean"))   mean(raw_vals,   na.rm = TRUE)
    else if (identical(deviation, "median")) median(raw_vals, na.rm = TRUE)
    else 0
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

  # ---- Per-source helpers that mirror Mod 2's reactive trio --------------
  # Each takes the per-source aggregate (Mod 2 list-col tibble) and emits
  # the same long-format pointrange / timeseries / exceedance / threshold
  # rows Mod 2's plotters consume, tagged with a `source` column.
  .build_pointrange_rows <- function(agg_hist, agg_scn, hist_ref,
                                     source_label, bq_coef, bq_ens) {
    z_lo <- stats::qnorm(bq_coef[["lo"]])
    z_hi <- stats::qnorm(bq_coef[["hi"]])
    one <- function(tbl, scenario_label, is_hist) {
      if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
      mm <- by_model_matrix(tbl)
      if (is.null(mm)) return(NULL)
      vals <- mm$vals; sds <- mm$sds
      model_means <- rowMeans(vals, na.rm = TRUE)
      intermod <- if (is_hist || length(model_means) <= 1L) {
        mv <- mean(model_means, na.rm = TRUE); c(lo = mv, hi = mv)
      } else c(
        lo = unname(stats::quantile(model_means, bq_ens[["lo"]], na.rm = TRUE)),
        hi = unname(stats::quantile(model_means, bq_ens[["hi"]], na.rm = TRUE))
      )
      if (is_hist) {
        v_flat <- as.numeric(vals)
        interann <- c(
          lo = unname(stats::quantile(v_flat, bq_ens[["lo"]], na.rm = TRUE)),
          hi = unname(stats::quantile(v_flat, bq_ens[["hi"]], na.rm = TRUE))
        )
      } else {
        per_lo <- apply(vals, 1L, stats::quantile, probs = bq_ens[["lo"]], na.rm = TRUE)
        per_hi <- apply(vals, 1L, stats::quantile, probs = bq_ens[["hi"]], na.rm = TRUE)
        interann <- c(lo = mean(per_lo, na.rm = TRUE), hi = mean(per_hi, na.rm = TRUE))
      }
      ens_mean <- mean(as.numeric(vals), na.rm = TRUE)
      sd_mean  <- mean(as.numeric(sds),  na.rm = TRUE)
      coef <- c(lo = ens_mean + z_lo * sd_mean, hi = ens_mean + z_hi * sd_mean)
      # Pooled SE on the central (year- and model-averaged) estimate,
      # mirroring the return-period table's "Pooled" convention. Inter-
      # annual variability is shown separately as its own band rather
      # than pooled in: it describes the spread of the simulated outcome
      # distribution, not uncertainty about the central tendency. When
      # var_across is zero (historical or single-member future), the
      # pooled SE degenerates to the coef SE; we suppress the outer
      # whisker (NA) to avoid drawing a duplicate of the coef band. See
      # mod_2_02_results.R for the parallel implementation.
      var_coef_total <- mean(as.numeric(sds)^2, na.rm = TRUE)
      var_across <- if (!is_hist && nrow(vals) > 1L) {
        v <- stats::var(rowMeans(vals, na.rm = TRUE), na.rm = TRUE)
        if (is.finite(v)) v else 0
      } else 0
      if (var_across > 0) {
        sd_total <- sqrt(max(var_coef_total + var_across, 0, na.rm = TRUE))
        total <- c(lo = ens_mean + z_lo * sd_total,
                   hi = ens_mean + z_hi * sd_total)
      } else {
        total <- c(lo = NA_real_, hi = NA_real_)
      }
      tibble::tibble(
        scenario      = scenario_label,
        source        = source_label,
        value         = ens_mean - hist_ref,
        coef_lo       = unname(coef[["lo"]])     - hist_ref,
        coef_hi       = unname(coef[["hi"]])     - hist_ref,
        interann_lo   = unname(interann[["lo"]]) - hist_ref,
        interann_hi   = unname(interann[["hi"]]) - hist_ref,
        intermod_lo   = unname(intermod[["lo"]]) - hist_ref,
        intermod_hi   = unname(intermod[["hi"]]) - hist_ref,
        total_lo      = unname(total[["lo"]])    - hist_ref,
        total_hi      = unname(total[["hi"]])    - hist_ref,
        is_historical = is_hist,
        n_models      = length(mm$model_ids)
      )
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

  .build_timeseries_rows <- function(agg_hist, agg_scn, hist_ref, source_label) {
    one <- function(tbl, scenario_label, is_hist) {
      if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
      mm <- by_model_matrix(tbl)
      if (is.null(mm)) return(NULL)
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
    one <- function(tbl, scenario_label, is_hist) {
      if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
      mm <- by_model_matrix(tbl)
      if (is.null(mm)) return(NULL)
      vals <- mm$vals; sds <- mm$sds
      dplyr::bind_rows(lapply(seq_len(nrow(vals)), function(i) {
        v <- vals[i, ]; s <- sds[i, ]
        ok <- is.finite(v)
        if (!any(ok)) return(NULL)
        v <- v[ok]; s <- s[ok]
        ord <- order(v)
        tibble::tibble(
          scenario      = scenario_label,
          source        = source_label,
          model_id      = mm$model_ids[[i]],
          rank          = seq_along(ord),
          welfare_val   = v[ord] - hist_ref,
          coef_sd       = if (length(s) == length(ord)) s[ord] else rep(0, length(ord)),
          exceed_prob   = rev((seq_len(length(ord)) - 0.5) / length(ord)),
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
    RPs <- c(RP_LOW, c("1:1" = 0.5), RP_HIGH)
    one <- function(tbl, scenario_label, is_hist) {
      if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
      mm <- by_model_matrix(tbl)
      if (is.null(mm)) return(NULL)
      vals <- mm$vals; sds <- mm$sds
      n_yrs <- ncol(vals)
      n_pts <- if (is_hist) sum(is.finite(as.numeric(vals))) else n_yrs
      rp_ok    <- RPs >= (1 / n_yrs) & RPs <= (1 - 1 / n_yrs)
      RPs_keep <- RPs[rp_ok]
      if (length(RPs_keep) == 0L) return(NULL)
      per_model_rp <- t(apply(vals, 1L, function(v) {
        v <- v[is.finite(v)]
        if (length(v) < 2L) return(rep(NA_real_, length(RPs_keep)))
        sv <- sort(v)
        vapply(RPs_keep, function(p) rank_interp(sv, p), numeric(1L))
      }))
      per_model_sd_at_rp <- t(vapply(seq_len(nrow(vals)), function(i) {
        v <- vals[i, ]; s <- sds[i, ]
        ok <- is.finite(v)
        if (sum(ok) < 2L) return(rep(NA_real_, length(RPs_keep)))
        ord <- order(v[ok]); s_sorted <- s[ok][ord]
        vapply(RPs_keep, function(p) rank_interp(s_sorted, p), numeric(1L))
      }, numeric(length(RPs_keep))))
      central_vec <- if (is_hist) per_model_rp[1L, ] else
        apply(per_model_rp, 2L, stats::median, na.rm = TRUE)
      coef_sd_vec <- if (is_hist) per_model_sd_at_rp[1L, ] else
        apply(per_model_sd_at_rp, 2L, stats::median, na.rm = TRUE)
      coef_lo_vec <- central_vec + z_lo * coef_sd_vec
      coef_hi_vec <- central_vec + z_hi * coef_sd_vec
      intermod_lo_vec <- if (is_hist) rep(NA_real_, length(RPs_keep)) else
        apply(per_model_rp, 2L, stats::quantile, probs = bq_ens[["lo"]], na.rm = TRUE)
      intermod_hi_vec <- if (is_hist) rep(NA_real_, length(RPs_keep)) else
        apply(per_model_rp, 2L, stats::quantile, probs = bq_ens[["hi"]], na.rm = TRUE)
      var_across_at_rp <- if (is_hist) rep(0, length(RPs_keep)) else
        apply(per_model_rp, 2L, stats::var, na.rm = TRUE)
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
      coef_lo_lbl   <- paste0("Coef ",     pct_label(bq_coef[["lo"]]))
      coef_hi_lbl   <- paste0("Coef ",     pct_label(bq_coef[["hi"]]))
      ens_lo_lbl    <- paste0("Ensemble ", pct_label(bq_ens[["lo"]], use_minmax = TRUE))
      ens_hi_lbl    <- paste0("Ensemble ", pct_label(bq_ens[["hi"]], use_minmax = TRUE))
      pooled_lo_lbl <- paste0("Pooled ",   pct_label(bq_coef[["lo"]]))
      pooled_hi_lbl <- paste0("Pooled ",   pct_label(bq_coef[["hi"]]))
      rows <- list(
        make_row("Central (P50)", central_vec),
        make_row(coef_lo_lbl,     coef_lo_vec),
        make_row(coef_hi_lbl,     coef_hi_vec)
      )
      if (!is_hist) {
        rows <- c(rows, list(
          make_row(ens_lo_lbl,    intermod_lo_vec),
          make_row(ens_hi_lbl,    intermod_hi_vec),
          make_row(pooled_lo_lbl, total_lo_vec),
          make_row(pooled_hi_lbl, total_hi_vec)
        ))
      }
      dplyr::bind_rows(rows)
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

  pointrange_bands_rv <- reactive({
    req(baseline_agg_hist())
    bq_coef <- resolve_band_q(input$uncertainty_band %||% "p10_p90")
    bq_ens  <- resolve_band_q(input$ensemble_band    %||% "minmax")
    hr      <- hist_ref_val()
    dplyr::bind_rows(
      .build_pointrange_rows(baseline_agg_hist(), baseline_agg_scenarios(),
                             hr, "Baseline", bq_coef, bq_ens),
      .build_pointrange_rows(policy_agg_hist(),   policy_agg_scenarios(),
                             hr, "Policy",   bq_coef, bq_ens)
    )
  })

  timeseries_curves_rv <- reactive({
    req(baseline_agg_hist())
    hr <- hist_ref_val()
    dplyr::bind_rows(
      .build_timeseries_rows(baseline_agg_hist(), baseline_agg_scenarios(),
                             hr, "Baseline"),
      .build_timeseries_rows(policy_agg_hist(),   policy_agg_scenarios(),
                             hr, "Policy")
    )
  })

  exceedance_curves_rv <- reactive({
    req(baseline_agg_hist())
    hr <- hist_ref_val()
    dplyr::bind_rows(
      .build_exceedance_rows(baseline_agg_hist(), baseline_agg_scenarios(),
                             hr, "Baseline"),
      .build_exceedance_rows(policy_agg_hist(),   policy_agg_scenarios(),
                             hr, "Policy")
    )
  })

  threshold_table_rv <- reactive({
    req(baseline_agg_hist())
    bq_coef <- resolve_band_q(input$uncertainty_band %||% "p10_p90")
    bq_ens  <- resolve_band_q(input$ensemble_band    %||% "minmax")
    hr      <- hist_ref_val()
    dplyr::bind_rows(
      .build_threshold_rows(baseline_agg_hist(), baseline_agg_scenarios(),
                            hr, "Baseline", bq_coef, bq_ens),
      .build_threshold_rows(policy_agg_hist(),   policy_agg_scenarios(),
                            hr, "Policy",   bq_coef, bq_ens)
    )
  })

  table_subtitle <- reactive({
    req(baseline_agg_hist(), input$cmp_agg_method, input$cmp_deviation)
    paste0(
      agg_axis_label(), " - ",
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
      default_val <- baseline_hist_sim()$pov_line %||% 3.00
      # INT-01: keep the user's poverty line when this input is rebuilt
      # (aggregation-method toggle); the run's value is only the fallback.
      prev_pl <- shiny::isolate(input$cmp_pov_line)
      shiny::numericInput(
        inputId = ns("cmp_pov_line"),
        label   = "Poverty line ($/day)",
        value   = .restore_numeric(prev_pl, 0.01, Inf, fallback = default_val),
        min     = 0.01,
        step    = 0.5
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
    # INT-01: keep the user's filter selection across re-runs that change the
    # scenario set; only filters that no longer exist are dropped, and an
    # empty result falls back to "all selected".
    prev_fy  <- shiny::isolate(input$filter_year)
    prev_fs  <- shiny::isolate(input$filter_ssp)
    tagList(
      shiny::fluidRow(
        shiny::column(4,
          if (length(yrs) > 0)
            shiny::checkboxGroupInput(
              ns("filter_year"), label = "Projection periods",
              choices = yrs,
              selected = .restore_selection(prev_fy, yrs, fallback = yrs),
              inline = TRUE
            )
        ),
        shiny::column(4,
          if (length(ssps) > 0)
            shiny::checkboxGroupInput(
              ns("filter_ssp"), label = "SSPs",
              choices = ssps,
              selected = .restore_selection(prev_fs, ssps, fallback = ssps),
              inline = TRUE
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
    req(pointrange_bands_rv())
    bands <- pointrange_bands_rv()
    if (!isTRUE(input$show_model_spread)) {
      bands$intermod_lo <- NA_real_
      bands$intermod_hi <- NA_real_
    }
    plot_pointrange_climate(
      bands_tbl   = bands,
      x_label     = agg_axis_label(),
      group_order = input$cmp_group_order %||% "scenario_x_year",
      show_coef   = isTRUE(input$show_coef_uncertainty) && has_draws()
    )
  }, height = 600)
  outputOptions(output, "summary_box_plot", suspendWhenHidden = FALSE)

  output$summary_threshold_table <- DT::renderDT({
    req(threshold_table_rv())
    tbl <- threshold_table_rv()
    if (!isTRUE(input$show_model_spread))
      tbl <- tbl[!grepl("^Ensemble |^Pooled ", tbl$Estimate), , drop = FALSE]
    df <- build_threshold_table_df(
      threshold_tbl = tbl,
      group_order   = input$cmp_group_order %||% "scenario_x_year",
      show_coef     = isTRUE(input$show_coef_uncertainty) && has_draws()
    )
    if (is.null(df) || nrow(df) == 0L)
      return(DT::datatable(data.frame(Message = "Insufficient data"),
                           rownames = FALSE, class = "compact stripe",
                           options  = list(dom = "t")))
    DT::datatable(
      df, rownames = FALSE, class = "compact stripe",
      options = list(
        pageLength = 30, dom = "Bt", ordering = list(list(2, "desc")),
        columnDefs = list(list(className = "dt-center", targets = "_all")),
        # INT-08: export is disabled while the results are stale.
        buttons = if (isTRUE(stale())) NULL else
          list(list(extend = "csv", filename = "outcome_thresholds"))
      ),
      extensions = "Buttons"
    )
  })
  outputOptions(output, "summary_threshold_table", suspendWhenHidden = FALSE)

  output$threshold_table_header <- renderUI({
    req(baseline_agg_hist())
    tagList(
      shiny::h4(
        "Outcome value at return-period thresholds (both tails)",
        info_popover(
          title = "Return-period thresholds",
          shiny::p("Low odds show the value exceeded in only 1-in-N years."),
          shiny::p("High odds show the value reached in all but 1-in-N years."),
          shiny::p("1:1 shows the median (50th percentile) simulated value."),
          docs = TRUE
        )
      ),
      shiny::tags$small(class = "text-muted", table_subtitle())
    )
  })

  output$threshold_table_footer <- renderUI({
    req(baseline_agg_hist())
    shiny::tags$p(
      style = "font-size:11px; color:#666; margin-top:6px;",
      "Odds relative to a 1-in-N-year event - click ",
      shiny::icon("circle-info"), " above for definitions."
    )
  })

  output$exceedance_plot <- renderPlot({
    req(exceedance_curves_rv())
    ens_q <- if (isTRUE(input$show_model_spread))
      resolve_band_q(input$ensemble_band %||% "minmax")
    else c(lo = 0.5, hi = 0.5)
    enhance_exceedance(
      curves_tbl      = exceedance_curves_rv(),
      x_label         = agg_axis_label(),
      return_period   = isTRUE(input$show_return_period),
      n_sim_years     = nrow(baseline_agg_hist()$out),
      logit_x         = isTRUE(input$exceedance_logit_x),
      band_q          = if (isTRUE(input$show_coef_uncertainty) && has_draws())
                          resolve_band_q(input$uncertainty_band %||% "p10_p90")
                        else NULL,
      ensemble_band_q = ens_q
    )
  })
  outputOptions(output, "exceedance_plot", suspendWhenHidden = FALSE)

  output$timeseries_plot <- renderPlot({
    req(timeseries_curves_rv())
    ens_q <- if (isTRUE(input$show_model_spread))
      resolve_band_q(input$ensemble_band %||% "minmax")
    else c(lo = 0.5, hi = 0.5)
    plot_timeseries_spaghetti(
      ts_tbl          = timeseries_curves_rv(),
      x_label         = agg_axis_label(),
      ensemble_band_q = ens_q
    )
  })
  outputOptions(output, "timeseries_plot", suspendWhenHidden = FALSE)

  output$exceedance_caption <- renderUI({
    req(baseline_agg_hist())
    axis_txt <- if (isTRUE(input$exceedance_logit_x))
      "Probability axis is logit-scaled, giving equal visual weight to both tails."
    else
      "The curve shows the estimated annual exceedance probability for each outcome value."
    shiny::tags$p(
      style = "font-size:11px; color:#666; margin-top:6px;",
      axis_txt
    )
  })

  # Invisibly expose the aggregation internals for regression tests
  # (test-policy-sim-compare-agg-cache.R).
  invisible(list(
    baseline_agg_hist      = baseline_agg_hist,
    baseline_agg_scenarios = baseline_agg_scenarios,
    policy_agg_hist        = policy_agg_hist,
    policy_agg_scenarios   = policy_agg_scenarios,
    agg_cache_ws           = agg_cache_ws,
    hist_label             = hist_label
  ))
}
