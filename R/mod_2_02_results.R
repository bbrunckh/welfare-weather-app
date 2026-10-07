#' Results tab content UI (inserted into the Results tabPanel once).
#' @noRd
.results_content_ui <- function(ns, so, weather_var = NULL) {
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
    paste0("How is ", outcome_lbl, " predicted to vary with ", wx_phrase, " across climate scenarios?")
  } else {
    paste0("How is ", outcome_lbl, " predicted to vary across climate scenarios and weather years?")
  }

  agg_choices <- hist_aggregate_choices(so_type, so_name)

  pov_units <- if (!is.null(so) && "units" %in% names(so) && !is.null(so[["units"]]) && nzchar(as.character(so[["units"]][1]))) {
    as.character(so[["units"]][1])
  } else {
    "$/day, 2021 PPP"
  }
  pov_val <- if (!is.null(so) && "povline" %in% names(so) && !is.null(so[["povline"]]) && is.finite(so[["povline"]][1]) && so[["povline"]][1] > 0) {
    so[["povline"]][1]
  } else {
    3.00
  }

  tagList(
    # 0. Stale banner (INT-08) ----
    shiny::tagList(
      shiny::uiOutput(ns("stale_banner")),
      shiny::uiOutput(ns("provisional_banner"))
    ),
    shiny::uiOutput(ns("simulation_summary_ui")),

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
              "Choose how household-level welfare is aggregated into an annual",
              "population outcome for each simulated weather year and climate model.",
              "Poverty and prosperity metrics evaluate outcomes relative to the",
              "specified poverty line; prosperity gap uses a fixed threshold of 28."
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
          aria_label = "Aggregation method",
          choices  = agg_choices,
          selected = "mean",
          layout   = "horizontal"
        ),
        pill_toggle(
          inputId = ns("cmp_deviation"),
          label = NULL,
          aria_label = "Outcome or difference from historical",
          choices = c(
            "Outcome level"                 = "none",
            "Difference from historical mean"   = "mean",
            "Difference from historical median" = "median"
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
              `for` = ns("pov_line"),
              style = "font-size: 0.8rem; font-weight: 600; color: #526575; margin: 0; white-space: nowrap;",
              paste0("Poverty line (", pov_units, "):")
            ),
            shiny::numericInput(
              ns("pov_line"),
              label = NULL,
              value = pov_val,
              min   = 0,
              step  = 0.5,
              width = "105px"
            )
          )
        ),
        shiny::conditionalPanel(
          condition = paste0(
            "input['", ns("cmp_agg_method"), "'] === 'prosperity_gap'"
          ),
          shiny::div(
            class = "text-muted small",
            style = "font-size: 0.8rem;",
            "Prosperity gap uses a fixed threshold of 28; currency and time applicability are described in the headline context."
          )
        )
      )
    ),

    # 2. Headline cards ----
    shiny::uiOutput(ns("headline_cards_ui")),
    # Section 1: Central outcomes & weather-year variation ----
    shiny::h4(
      sec1_heading,
      info_popover(
        title = "Annual weather variation",
        shiny::p(
          "Each dot represents the population aggregate outcome under one simulated",
          "weather year. The box and violin illustrate the full range of annual",
          "weather-year variation for the fixed baseline population under each",
          "climate regime."
        ),
        shiny::p(
          "The dashed vertical line marks the historical baseline mean. The white circle",
          "shows the central expected outcome."
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
          aria_label = "Distribution chart type",
          choices  = c("Violin" = "violin", "Boxplot" = "boxplot"),
          selected = "violin",
          layout   = "horizontal"
        )
      ),
      wise_chart_output(
        ns("annual_distribution_plot"),
        "Distribution of annual aggregates across simulated weather years by climate scenario",
        height = "470px"
      ),
      shiny::tags$p(
        class = "text-muted small",
        style = "margin-top: 18px; margin-bottom: 0;",
        "Each dot is one simulated weather-year annual aggregate for the fixed baseline population. The selected violin or boxplot summarizes the distribution; open circles mark scenario means. This captures weather-year variability, not household inequality."
      )
    ),

    # Section 2: Adverse weather years (tail risk) ----
    shiny::h4(
      "What outcomes are predicted in adverse weather years?",
      info_popover(
        title = "Adverse weather-year risk",
        shiny::p(
          "Adverse return-period outcomes represent severe annual weather conditions.",
          "An adverse 1-in-10-year outcome is reached or exceeded in the unfavorable",
          "direction in approximately one out of ten simulated weather years."
        ),
        shiny::p(
          "Points show expected outcomes and adverse-year thresholds; intervals",
          "show disagreement across CMIP6 climate models (ensemble spread)."
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
            "None" = "none",
            "Full ensemble spread" = "minmax",
            "95%" = "p025_p975",
            "90%" = "p05_p95",
            "80%" = "p10_p90"
          ),
          selected = "none",
          layout = "horizontal"
        )
      ),
      wise_chart_output(
        ns("adverse_dot_plot"),
        "Expected and adverse-year outcomes with climate-model ensemble spread",
        height = "380px"
      ),
      shiny::tags$p(
        class = "text-muted small",
        style = "margin-top: 8px; margin-bottom: 0;",
        "Median across climate models; distinct from the equal-model-mean expected headline. Adverse tail direction is mapped automatically according to the selected outcome metric. Horizontal bars show inter-model ensemble spread across climate projections."
      )
    ),

    # Section 3: Exceedance probability curves ----
    shiny::h4(
      "What is the probability of severe outcomes occurring?",
      info_popover(
        title = "Exceedance probability",
        shiny::p(
          "Shows the annual probability of reaching or exceeding severe outcome",
          "thresholds across simulated weather years under each climate regime.",
          "The probability axis labels standard return periods (e.g. 1 in 10 or 1 in 20 year events)."
        ),
        shiny::p(
          "The navy curve is the historical baseline. Coloured curves show the",
          "ensemble median across climate models, with shaded ribbons indicating inter-model spread."
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
        "Exceedance probability curves across simulated climate scenarios",
        height = "400px"
      ),
      shiny::tags$p(
        class = "text-muted small",
        style = "margin-top: 8px; margin-bottom: 0;",
        "Read each curve as the annual probability of reaching an outcome level in the adverse direction: lower outcomes for higher-is-better measures and higher outcomes for lower-is-better measures. Coloured lines show the across-model median; shaded ribbons show climate-model disagreement for each future baseline and policy series. Return-period guides and ticks are limited to the available simulated years per climate model; unsupported periods are not extrapolated."
      )
    ),

    # Section 4: Decision & return-period table ----
    shiny::h4(
      "Detailed return-period outcomes and uncertainty",
      info_popover(
        title = "Return-period decision table",
        shiny::p(
          "Summary of central expected outcomes and severe weather-year thresholds",
          "for each scenario across return periods, with climate model and econometric uncertainty bounds."
        ),
        docs = TRUE
      ),
      style = "font-size: 1.05rem; font-weight: 700; color: #173042; margin-top: 24px; margin-bottom: 8px;"
    ),
    shiny::div(
      class = "results-section-card",
      shiny::div(
        style = "display: flex; justify-content: flex-end; align-items: center; margin-bottom: 8px;",
        wise_reactable_csv_button(
          ns("summary_threshold_table"),
          "climate_outcome_thresholds"
        )
      ),
      reactable::reactableOutput(ns("summary_threshold_table")),
      shiny::tags$p(
        class = "text-muted small",
        style = "margin-top: 8px; margin-bottom: 0;",
          "Adverse point estimates use the selected metric's baseline annual quantile within each model and an equal-model mean across SSP models; this differs from distribution-curve medians. Bounds are existing diagnostics, not uncertainty estimates for the equal-model mean."
      )
    ),

    # Section 5: Uncertainty decomposition ----
    shiny::h4(
      "What drives the uncertainty in these predictions?",
      info_popover(
        title = "Uncertainty sources",
        shiny::p(
          "Compares the absolute standard deviation contributed by each distinct source:",
          "year-to-year weather variability, climate-model disagreement, and model-estimation uncertainty."
        ),
        shiny::p(
          "Coefficient uncertainty is the analytic delta-method standard error from the fitted model's coefficient covariance matrix",
          "(plus residual-draw variance when stochastic residuals are enabled). It is shown as one standard deviation, not a 95% confidence interval;",
          "an approximate normal 95% interval would be estimate +/- 1.96 times this value."
        ),
        shiny::p(
          "Because standard deviations are not additive, bars are displayed side-by-side on a common scale."
        ),
        docs = TRUE
      ),
      style = "font-size: 1.05rem; font-weight: 700; color: #173042; margin-top: 24px; margin-bottom: 8px;"
    ),
    shiny::div(
      class = "results-section-card",
      wise_chart_output(
        ns("uncertainty_sources_plot"),
        "Standard deviation of outcome by uncertainty source",
        height = "300px"
      ),
      shiny::tags$p(
        class = "text-muted small",
        style = "margin-top: 8px; margin-bottom: 0;",
        "Bars are standard deviations in outcome units, not confidence intervals. Inter-annual variability is the within-model standard deviation across simulated weather years; inter-model spread is the standard deviation of model means across climate models; coefficient uncertainty is the analytic delta-method SE from the fitted model covariance (plus any enabled stochastic residual variance)."
      )
    )
  )
}


#' Placeholder rows for scenarios still computing (provisional mode only)
#'
#' Appends one row per pending scenario to the threshold table: scenario label,
#' Estimate "computing...", every other column blank.
#' @noRd
.append_pending_threshold_rows <- function(df, pending) {
  pending <- as.character(pending %||% character(0))
  scn_col <- intersect(c("Scenario / Period", "Scenario"), names(df))[1L]
  if (is.null(df) || !nrow(df) || !length(pending) || is.na(scn_col) ||
      !"Estimate" %in% names(df)) {
    return(df)
  }
  pending <- setdiff(pending, df[[scn_col]])
  if (!length(pending)) return(df)
  new <- df[rep(NA_integer_, length(pending)), , drop = FALSE]
  new[[scn_col]] <- pending
  new$Estimate <- "computing..."
  out <- rbind(df, new)
  # rbind() leaves character row names, which reactable would display.
  rownames(out) <- NULL
  out
}

# Lazy per-method aggregation list ----
# Built inside mod_2_02_results_server(); the S3 methods force the builder on
# first access. Defined at top level and registered (not in the module
# closure) so dispatch does not depend on the caller's environment.

.force_lazy_aggregation_method <- function(x) {
  if (!inherits(x, "wise_lazy_aggregation_method_list")) {
    return(x)
  }
  state <- attr(x, "state", exact = TRUE)
  if (!isTRUE(state$built)) {
    state$value <- state$builder()
    state$built <- TRUE
  }
  x
}

#' @export
#' @noRd
`[[.wise_lazy_aggregation_method_list` <- function(x, i, ...) {
  x <- .force_lazy_aggregation_method(x)
  attr(x, "state", exact = TRUE)$value[[i]]
}

#' @export
#' @noRd
`$.wise_lazy_aggregation_method_list` <- function(x, name) {
  x <- .force_lazy_aggregation_method(x)
  attr(x, "state", exact = TRUE)$value[[name]]
}

#' @export
#' @noRd
names.wise_lazy_aggregation_method_list <- function(x) {
  names(unclass(x))
}

#' @export
#' @noRd
length.wise_lazy_aggregation_method_list <- function(x) 1L

#' 2_02_results Server Functions
#'
#' Appends a Results tab to the main tabset once the historical simulation
#' has run. All comparison outputs update reactively as saved_scenarios change.
#'
#' @param id              Module id.
#' @param hist_sim        ReactiveVal. Named list with slots:
#'   \code{$preds} (full prediction data frame), \code{$agg} (pre-aggregated
#'   summary by method x weighted x deviation x sim_year), \code{$so}
#'   (selected outcome metadata), \code{$pov_line} (simulation-time poverty
#'   line), \code{$has_weights} (logical weight flag), \code{$weather_raw},
#'   \code{$train_data}, \code{$n_pre_join}.
#' @param saved_scenarios ReactiveVal holding named scenario entries.
#' @param selected_hist   Reactive one-row data frame from weathersim.
#' @param selected_weather Reactive data frame of selected weather variables.
#' @param tabset_id       Character id of the parent tabset panel.
#' @param tabset_session  Shiny session for the tabset.
#' @param live_run        Reactive; NULL or the streaming-run list from
#'   mod_2_01 (see review/step2_live_run_contract.md). Drives the provisional
#'   display source.
#' @param adopted_partials Reactive; accepted for the cache-seeding phase,
#'   not read yet.
#' @param display_settings_out Optional reactiveVal written with
#'   \code{list(method, pov_line, bandwidth_p0)} in committed mode only.
#'
#' @noRd
mod_2_02_results_server <- function(id,
                                    hist_sim,
                                    saved_scenarios,
                                    selected_hist,
                                    selected_weather = NULL,
                                    tabset_id,
                                    tabset_session = NULL,
                                     residuals = reactive("original"),
                                     skip_coef_draws = reactive(FALSE),
                                     shared_aggregation_cache = NULL,
                                     stale = reactive(FALSE),
                                     live_run = reactive(NULL),
                                     adopted_partials = reactive(NULL),
                                     display_settings_out = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) tabset_session <- session$parent %||% session

    # INT-08: stale banner above the results pane. This surface gates its
    # CSV export while stale.
    output$stale_banner <- renderUI({
      if (isTRUE(stale())) {
        .stale_banner(
          "Step 2 simulation results",
          note = NULL
        )
      } else {
        NULL
      }
    })

    # Exports are committed-only. While a run streams (provisional mode) the
    # shared display closures are fed by partials, so each registered `fun`
    # is wrapped to signal "not ready" (req), which the bundle skips silently.
    # Once the run is adopted, or cancelled with the committed run showing,
    # exports work again from the committed data.
    # The condition is a wise_export_skip, which the bundle reports as a
    # skipped artefact with this note (README and notification) rather than
    # dropping it silently.
    .committed_only <- function(fun) {
      force(fun)
      function() {
        if (.is_provisional()) {
          stop(structure(
            class = c("wise_export_skip", "error", "condition"),
            list(
              message = paste(
                "A new Step 2 run is still in progress; export again once",
                "it has finished."
              ),
              call = NULL
            )
          ))
        }
        fun()
      }
    }

    # results_source(): single display source ----
    # Every display consumer reads this instead of hist_sim()/saved_scenarios().
    # Returns NULL when there is nothing to show, otherwise a plain list:
    #   mode              "committed" | "provisional"
    #   so                outcome row
    #   has_weights, has_draws, residuals, weight_key
    #   analysis_unit, sim_summary, hist_label  (NULL when provisional)
    #   scenario_names    landed scenario labels (NULL only when committed
    #                     saved_scenarios() is NULL)
    #   scenario_n_models named integer, same order as scenario_names
    #   pending_names     labels still computing (provisional only)
    #   methods_available aggregation methods the outcome supports
    #   locked_method / locked_pov_line / locked_bandwidth
    #                     NULL when committed; the partial's display settings
    #                     when provisional
    #   progress          NULL when committed; list(groups_done, groups_total)
    #   hist_agg(method), scn_agg(method)
    #                     nested list(<weight slot> = list(<method> = table));
    #                     committed: today's lazy workspace path; provisional:
    #                     streamed partial tables (every method in
    #                     methods_available), NULL for other methods.
    # Provisional wins when live_run() has a historical partial. C-class
    # consumers (incidence, aggregation workspace/cache, exports, the
    # Diagnostics return API) keep reading hist_sim()/saved_scenarios().
    # live_run() is rewritten on every 0.5 s progress tick (elapsed, member
    # counters). Display consumers depend only on the run identity and its
    # partials, so mirror those into a reactiveVal: identical writes do not
    # invalidate, and charts re-render once per landed partial, not per tick.
    live_data <- reactiveVal(NULL)
    observe({
      lr <- live_run()
      live_data(if (is.null(lr)) NULL else list(
        generation = lr$generation,
        scenario_labels = lr$scenario_labels,
        groups_total = lr$groups_total,
        partials = lr$partials
      ))
    })

    results_source <- reactive({
      live <- live_data()
      hp <- live$partials$historical
      if (!is.null(live) && !is.null(hp)) {
        disp <- hp$display %||% list()
        method <- disp$method %||% "mean"
        wk <- disp$weight_key %||%
          if (isTRUE(hp$has_weights)) "weighted" else "unweighted"
        landed <- live$partials$scenarios %||% list()
        labels <- as.character(live$scenario_labels %||% character(0))
        # names(list()) is NULL and intersect(x, NULL) is NULL in R >= 4.2;
        # a NULL scenario_names would read as "no scenarios" and req() out
        # every chart while only the historical partial has landed.
        landed_names <- as.character(names(landed) %||% character(0))
        scn_names <- c(
          intersect(labels, landed_names),
          setdiff(landed_names, labels)
        )
        # Every streamed method is served from the partial's `tables`; a
        # partial without `tables` carries only the captured method (`table`).
        tbl_of <- function(p, m) {
          p$tables[[m]] %||% if (identical(m, method)) p$table else NULL
        }
        streamed <- unique(c(
          method,
          as.character(hp$display$methods %||% names(hp$tables))
        ))
        streamed <- intersect(
          c(unname(hist_aggregate_choices(hp$so$type, hp$so$name)), method),
          streamed
        )
        wrap <- function(tbl, m) {
          if (is.null(tbl)) return(NULL)
          list(
            unweighted = setNames(list(tbl), m),
            weighted = setNames(list(tbl), m)
          )
        }
        return(list(
          mode = "provisional",
          so = hp$so,
          has_weights = isTRUE(hp$has_weights),
          has_draws = isTRUE(hp$has_draws),
          residuals = disp$residuals %||% "original",
          weight_key = wk,
          analysis_unit = NULL,
          sim_summary = NULL,
          hist_label = NULL,
          scenario_names = scn_names,
          scenario_n_models = vapply(
            scn_names, function(nm) as.integer(landed[[nm]]$n_models %||% NA_integer_),
            integer(1)
          ),
          pending_names = setdiff(labels, scn_names),
          methods_available = streamed,
          locked_method = method,
          locked_pov_line = disp$pov_line,
          locked_bandwidth = disp$bandwidth_p0,
          progress = list(
            groups_done = length(landed),
            groups_total = live$groups_total %||% length(labels)
          ),
          hist_agg = function(m) {
            if (m %in% streamed) wrap(tbl_of(hp, m), m) else NULL
          },
          scn_agg = function(m) {
            if (!m %in% streamed || !length(scn_names)) return(NULL)
            setNames(
              lapply(scn_names, function(nm) wrap(tbl_of(landed[[nm]], m), m)),
              scn_names
            )
          }
        ))
      }
      hs <- hist_sim()
      if (is.null(hs)) {
        return(NULL)
      }
      sc <- if (!is.null(saved_scenarios)) saved_scenarios() else NULL
      so <- hs$so
      list(
        mode = "committed",
        so = so,
        has_weights = isTRUE(hs$has_weights),
        has_draws = !is.null(hs$chol_obj),
        residuals = hs$residuals %||% residuals() %||% "original",
        weight_key = if (isTRUE(hs$has_weights)) "weighted" else "unweighted",
        analysis_unit = hs$analysis_unit,
        sim_summary = hs$sim_summary,
        hist_label = hs$hist_label,
        scenario_names = if (is.null(sc)) NULL else if (length(sc)) names(sc) else character(0),
        scenario_n_models = NULL,
        pending_names = character(0),
        methods_available = unname(hist_aggregate_choices(so$type, so$name)),
        locked_method = NULL,
        locked_pov_line = NULL,
        locked_bandwidth = NULL,
        progress = NULL,
        hist_agg = function(m) .get_hist_agg(m),
        scn_agg = function(m) .get_scn_agg(m)
      )
    })
    .is_provisional <- reactive({
      !is.null(live_data()$partials$historical)
    })

    output$simulation_summary_ui <- renderUI({
      src <- results_source()
      # Provisional: reduced card from what the partials carry (no
      # sim_summary / baseline survey); n_models comes from each partial.
      summary_hist <- if (is.null(src)) {
        NULL
      } else if (identical(src$mode, "provisional")) {
        list(so = src$so)
      } else {
        hist_sim()
      }
      summary_scn <- if (is.null(src)) {
        NULL
      } else if (identical(src$mode, "provisional")) {
        setNames(
          lapply(src$scenario_names, function(nm) {
            list(n_models = unname(src$scenario_n_models[[nm]]))
          }),
          src$scenario_names
        )
      } else {
        saved_scenarios()
      }
      simulation_summary_card(
        hist_sim = summary_hist,
        saved_scenarios = summary_scn,
        selected_hist = if (!is.null(selected_hist)) selected_hist() else NULL,
        selected_weather = if (is.function(selected_weather)) selected_weather() else selected_weather
      )
    })

    # Metadata views for the headline cards. Committed: the objects
    # themselves. Provisional: only what the partials carry (so, landed
    # scenario names), which makes the cards degrade to "Unavailable" for
    # the prediction-count note.
    .display_hist_sim <- function() {
      src <- results_source()
      if (!is.null(src) && identical(src$mode, "provisional")) {
        return(list(so = src$so))
      }
      hist_sim()
    }
    .display_saved_scenarios <- function() {
      src <- results_source()
      if (!is.null(src) && identical(src$mode, "provisional")) {
        return(setNames(
          vector("list", length(src$scenario_names)), src$scenario_names
        ))
      }
      if (!is.null(saved_scenarios)) saved_scenarios() else list()
    }

    # Observed-survey calibration of the simulated historical baseline
    # (committed results only; the placeholder frame carries no survey).
    baseline_check_rv <- reactive({
      req(hist_agg_rv())
      if (.is_provisional()) return(NULL)
      hs <- hist_sim()
      method <- .selected_method()
      wk <- weight_key()
      sim_vals <- hist_agg_rv()[[wk]][[method]]$value
      step2_baseline_check(
        hs$svy, hs$so, method, pov_line_val(),
        use_weights = identical(wk, "weighted"),
        simulated = mean(sim_vals, na.rm = TRUE),
        metadata = metric_metadata(method, hs$so %||% NULL)
      )
    })

    # Coefficient (estimation) uncertainty of the climate shift for the first
    # future scenario shown on the cards: needs the aggregation gradients, so
    # it is NULL when coefficient uncertainty was skipped.
    delta_ci_rv <- reactive({
      req(hist_agg_rv())
      if (.is_provisional()) return(NULL)
      bands <- headline_bands_rv()
      fut <- bands[!bands$is_historical, , drop = FALSE]
      if (!nrow(fut)) return(NULL)
      wk <- weight_key()
      method <- .selected_method()
      entry <- scenario_agg_rv()[[fut$scenario[[1L]]]]
      step2_delta_ci(hist_agg_rv()[[wk]][[method]], entry[[wk]][[method]])
    })

    # Weather-support summary of the first future scenario on the cards
    # (computed during the run; absent for older runs or when unavailable).
    weather_support_rv <- reactive({
      req(hist_agg_rv())
      if (.is_provisional()) return(NULL)
      bands <- headline_bands_rv()
      fut <- bands[!bands$is_historical, , drop = FALSE]
      if (!nrow(fut)) return(NULL)
      ws <- tryCatch(saved_scenarios()[[fut$scenario[[1L]]]]$weather_support,
        error = function(e) NULL)
      if (!is.data.frame(ws) || !nrow(ws)) return(NULL)
      sw <- if (!is.null(selected_weather)) selected_weather() else NULL
      if (!is.null(sw) && all(c("name", "label") %in% names(sw))) {
        lab <- stats::setNames(as.character(sw$label), as.character(sw$name))
        ws$weather_label <- unname(lab[ws$weather_variable])
      }
      ws
    })

    headline_cards_data_rv <- reactive({
      req(headline_bands_rv())
      bands <- headline_bands_rv()
      if (!nrow(bands) ||
        !all(c("is_historical", "scenario") %in% names(bands))) {
        return(NULL)
      }
      step2_headline_cards(
        bands = bands,
        threshold_tbl = tryCatch(threshold_table_rv(), error = function(e) NULL),
        hist_sim = tryCatch(.display_hist_sim(), error = function(e) NULL),
        saved_scenarios = tryCatch(.display_saved_scenarios(),
          error = function(e) list()
        ),
        method = .selected_method(),
        timeseries_curves = tryCatch(timeseries_curves_rv(), error = function(e) NULL),
        deviation = input$cmp_deviation %||% "none",
        baseline_check = tryCatch(baseline_check_rv(), error = function(e) NULL),
        delta_ci = tryCatch(delta_ci_rv(), error = function(e) NULL),
        weather_support = tryCatch(weather_support_rv(), error = function(e) NULL),
        metadata = {
          hs <- tryCatch(.display_hist_sim(), error = function(e) NULL)
          selected <- .selected_method()
          metric_metadata(
            selected,
            hs$so %||% NULL,
            pov_line = if (identical(selected, "prosperity_gap")) NULL else pov_line_val(),
            analysis_unit = hs$analysis_unit %||%
              .metric_context_value(hs$so, "level"),
            weighted = identical(weight_key(), "weighted")
          )
        }
      )
    })

    output$headline_cards_ui <- renderUI({
      cards <- headline_cards_data_rv()
      if (is.null(cards)) {
        return(NULL)
      }
      src <- results_source()
      note <- if (identical(src$mode, "provisional")) {
        shiny::div(
          class = "headline-card-note provisional-note",
          style = "margin: -4px 0 10px;",
          paste0(
            "Based on ", length(src$scenario_names), " of ",
            length(src$scenario_names) + length(src$pending_names), " scenarios"
          )
        )
      }
      shiny::tagList(headline_cards_ui(cards), note)
    })

    # Lazy delta-method aggregation ----
    # Replaces the eager compute_hist_agg / compute_scenario_agg path. Returns
    # the same nested list shape (weighted/unweighted -> method -> tibble) so
    # downstream consumers in fct_sim_compare.R see a compatible schema.
    agg_methods <- reactive({
      req(hist_sim())
      so <- results_source()$so
      unname(hist_aggregate_choices(so$type, so$name))
    })

    # Input updates can lag outcome changes by a browser round-trip. Keep every
    # aggregation consumer on a method supported by the current outcome.
    .selected_method <- reactive({
      src <- results_source()
      req(src)
      # Provisional: follow the control among the streamed methods, else the
      # method captured at submit.
      if (identical(src$mode, "provisional")) {
        selected <- input$cmp_agg_method
        return(
          if (!is.null(selected) && selected %in% src$methods_available) {
            selected
          } else {
            src$locked_method
          }
        )
      }
      choices <- agg_methods()
      selected <- input$cmp_agg_method %||% "mean"
      if (length(choices) && selected %in% choices) selected else "mean"
    })

    # pov_line is always supplied (the aggregation pre-computes every method
    # per year, not just the currently selected one). Non-poverty methods
    # ignore it; poverty methods need it. Default 3.00 USD/day if the input
    # hasn't been initialised yet.
    .pov_line_input <- debounce(reactive({
      as.numeric(input$pov_line %||% 3.00)
    }), 400)
    pov_line_val <- reactive({
      if (.is_provisional()) {
        locked <- results_source()$locked_pov_line
        if (!is.null(locked)) return(as.numeric(locked))
      }
      .pov_line_input()
    })

    bandwidth_p0 <- reactive({
      if (.is_provisional()) {
        locked <- results_source()$locked_bandwidth
        if (!is.null(locked)) return(as.numeric(locked))
      }
      # No UI control sets the kernel bandwidth; use the fixed default.
      0.05
    })

    # Value-affecting aggregation inputs ----
    # The aggregation cache is keyed by the inputs each method actually
    # consumes (PERF-30), so moving the coefficient-band or poverty-line
    # control only invalidates the methods that read it. band_q is display-
    # only: aggregate_with_uncertainty_delta() applies it to value_lo/hi,
    # which no builder below consumes - the band is re-derived from the
    # cached SDs at render time. Display uses a fixed neutral pair.
    AGG_BAND_Q <- c(lo = 0.10, hi = 0.90)
    .POV_LINE_METHODS <- c("headcount_ratio", "gap", "fgt2")
    .BANDWIDTH_METHODS <- "headcount_ratio"

    # Aggregation workspace + per-method cache ----
    # Captures the heavy dependencies that invalidate every cached method
    # (hist_sim, saved scenarios, residuals, coef-draw skipping) into a
    # workspace that's recreated whenever any of them changes. The workspace
    # carries a mutable cache so we only compute each aggregation method once
    # per workspace version. The Results tab reads only the currently selected
    # method, and an eager observer pre-computes the default ("mean") as soon
    # as hist_sim() arrives - so the first render is fast even before the
    # user clicks anything.
    #
    # Display-only controls (coefficient band, poverty line, headcount
    # bandwidth) are deliberately NOT workspace dependencies: changing them
    # used to destroy the whole cache. Instead they are read at cache-lookup
    # time and folded into the per-method cache key for exactly the methods
    # that consume them (see .pl_bw_key).
    cache_workspace_ref <- new.env(parent = emptyenv())
    cache_workspace_ref$current <- NULL

    .clear_aggregation_cache <- function(ws = cache_workspace_ref$current) {
      if (is.null(ws)) {
        return(invisible(NULL))
      }
      if (is.environment(ws$cache)) {
        cached <- ls(ws$cache, all.names = TRUE)
        if (length(cached)) rm(list = cached, envir = ws$cache)
      }
      if (is.environment(ws$cache_order)) {
        ws$cache_order$keys <- character(0)
        ws$cache_order$hits <- 0L
        ws$cache_order$misses <- 0L
        ws$cache_order$evictions <- character(0)
      }
      if (is.environment(ws$prep_cache)) {
        ws$prep_cache$entries <- list()
        ws$prep_cache$keys <- character(0)
      }
      if (is.environment(ws$weighted_suite_cache)) {
        cached_suites <- ls(ws$weighted_suite_cache, all.names = TRUE)
        if (length(cached_suites)) {
          rm(list = cached_suites, envir = ws$weighted_suite_cache)
        }
      }
      if (identical(ws, cache_workspace_ref$current)) {
        cache_workspace_ref$current <- NULL
      }
      invisible(NULL)
    }

    .cache_touch <- function(ws, key) {
      order <- ws$cache_order
      order$keys <- c(setdiff(order$keys, key), key)
      invisible(NULL)
    }

    .cache_get <- function(ws, key) {
      if (!exists(key, envir = ws$cache, inherits = FALSE)) {
        ws$cache_order$misses <- ws$cache_order$misses + 1L
        return(list(found = FALSE, value = NULL))
      }
      ws$cache_order$hits <- ws$cache_order$hits + 1L
      .cache_touch(ws, key)
      list(found = TRUE, value = get(key, envir = ws$cache, inherits = FALSE))
    }

    .cache_put <- function(ws, key, value) {
      assign(key, value, envir = ws$cache)
      .cache_touch(ws, key)
      while (length(ws$cache_order$keys) > ws$cache_order$max_entries) {
        old <- ws$cache_order$keys[[1L]]
        ws$cache_order$keys <- ws$cache_order$keys[-1L]
        if (exists(old, envir = ws$cache, inherits = FALSE)) {
          rm(list = old, envir = ws$cache)
          ws$cache_order$evictions <- c(ws$cache_order$evictions, old)
        }
      }
      invisible(value)
    }

    .cache_snapshot <- function(ws = cache_workspace_ref$current) {
      if (is.null(ws)) {
        return(list(
          keys = character(0), n_entries = 0L, max_entries = 0L,
          hits = 0L, misses = 0L, evictions = character(0),
          object_bytes = 0, serialized_bytes = 0,
          entry_object_bytes = numeric(0)
        ))
      }
      keys <- ws$cache_order$keys
      values <- if (length(keys)) {
        lapply(keys, get, envir = ws$cache, inherits = FALSE)
      } else {
        list()
      }
      object_bytes <- if (length(values)) {
        sum(vapply(values, function(x) as.numeric(utils::object.size(x)), numeric(1)))
      } else {
        0
      }
      serialized_bytes <- tryCatch(
        length(serialize(values, NULL, version = 3L)),
        error = function(e) NA_real_
      )
      list(
        keys = keys,
        n_entries = length(keys),
        max_entries = ws$cache_order$max_entries,
        hits = ws$cache_order$hits,
        misses = ws$cache_order$misses,
        evictions = ws$cache_order$evictions,
        object_bytes = object_bytes,
        serialized_bytes = serialized_bytes,
        entry_object_bytes = if (length(values)) {
          stats::setNames(
            vapply(values, function(x) as.numeric(utils::object.size(x)), numeric(1)),
            keys
          )
        } else {
          numeric(0)
        }
      )
    }
    aggregation_cache <- function() {
      published <- tryCatch(shiny::isolate(hist_sim()), error = function(e) NULL)
      if (is.null(published)) {
        return(.cache_snapshot(NULL))
      }
      .cache_snapshot()
    }

    agg_workspace <- reactive({
      req(hist_sim())
      .clear_aggregation_cache()
      cache_order <- new.env(parent = emptyenv())
      cache_order$keys <- character(0)
      cache_order$max_entries <- 8L
      cache_order$hits <- 0L
      cache_order$misses <- 0L
      cache_order$evictions <- character(0)
       ws <- list(
        hs = hist_sim(),
        sc = saved_scenarios(),
        res = hist_sim()$residuals %||% residuals() %||% "original",
        skip = isTRUE(skip_coef_draws()),
        cache = new.env(parent = emptyenv()),
         cache_order = cache_order,
         prep_cache = .new_aggregation_preparation_cache(max_entries = 32L),
         weighted_suite_cache = new.env(parent = emptyenv())
       )
      cache_workspace_ref$current <- ws
      .seed_aggregation_cache(ws)
      ws
    })

    session$onSessionEnded(function() .clear_aggregation_cache())
    observeEvent(hist_sim(),
      {
        if (is.null(hist_sim())) .clear_aggregation_cache()
      },
      ignoreInit = FALSE
    )

    # Cache-key suffix for the poverty line / bandwidth values a method reads.
    # Methods that ignore them get a constant key so moving the poverty-line
    # slider does not force their recomputation.
    .pl_bw_key <- function(method, pl_v, bw) {
      parts <- character(0)
      if (method %in% .POV_LINE_METHODS) parts <- c(parts, format(pl_v))
      if (method %in% .BANDWIDTH_METHODS) parts <- c(parts, format(bw))
      if (length(parts) == 0L) "" else paste0("_", paste(parts, collapse = "_"))
    }

    .new_lazy_aggregation_method_list <- function(builder, method) {
      state <- new.env(parent = emptyenv())
      state$builder <- builder
      state$value <- NULL
      state$built <- FALSE
      structure(
        setNames(list(NULL), method),
        class = c("wise_lazy_aggregation_method_list", "list"),
        state = state
      )
    }

    .build_lazy_aggregation_method <- function(builder, method) {
      .new_lazy_aggregation_method_list(builder, method)
    }

    .lazy_aggregation_value <- function(x) {
      if (inherits(x, "wise_lazy_aggregation_method_list")) {
        state <- attr(x, "state", exact = TRUE)
        if (!isTRUE(state$built)) .force_lazy_aggregation_method(x)
        return(state$value)
      }
      x
    }

    .lazy_aggregation_table <- function(x, method) {
      value <- .lazy_aggregation_value(x)
      if (is.list(value) && !is.null(value[[method]])) value[[method]] else value
    }

     .build_hist_for_method <- function(ws, method, pl_v) {
      pl <- ws$hs$pipeline
      bq <- AGG_BAND_Q
      is_log <- isTRUE(ws$hs$so$transform == "log")
       build_for <- function(weighted) {
             if ((isTRUE(weighted) || !isTRUE(has_w)) && !identical(method, "prosperity_gap")) {
              suite_pov <- pl_v %||% 3
              suite_key <- paste0("hist_suite_", weighted, "_", format(suite_pov), "_", format(bandwidth_p0()))
            shared_key <- shared_aggregation_cache_key(
              ws$hs$.sig %||% list(pipeline = "step2"), suite_pov,
              bandwidth_p0(), weighted, ws$res, ws$skip, is_log, agg_methods()
            )
            suite <- shared_aggregation_cache_get(shared_aggregation_cache, shared_key)
            if (!is.null(suite)) return(setNames(list(suite[[method]]), method))
            suite <- get0(suite_key, envir = ws$weighted_suite_cache)
           if (is.null(suite)) {
             suite <- aggregate_pipeline_tables_multi(
               pipelines = pl,
               methods = agg_methods(),
               weighted = weighted,
                pov_lines = setNames(lapply(agg_methods(), function(x) suite_pov), agg_methods()),
               residuals = ws$res,
               is_log = is_log,
               band_q = bq,
               skip_coef = ws$skip,
               bandwidth_p0 = bandwidth_p0(),
               model_ids = "Historical",
               scenario = "Historical",
               shared_context = ws$hs$shared_context,
               preparation_cache = ws$prep_cache
              )
              assign(suite_key, suite, envir = ws$weighted_suite_cache)
              shared_aggregation_cache_put(shared_aggregation_cache, shared_key, suite)
           }
           out <- suite[[method]]
           return(setNames(list(out), method))
         }
         out <- aggregate_pipeline_table(
          pipelines = pl,
          method = method,
          weighted = weighted,
           pov_line = pl_v,
          residuals = ws$res,
          is_log = is_log,
          band_q = bq,
          skip_coef = ws$skip,
          bandwidth_p0 = bandwidth_p0(),
          model_ids = "Historical",
          scenario = "Historical",
          shared_context = ws$hs$shared_context,
          preparation_cache = ws$prep_cache
         )
        setNames(list(out), method)
       }
       has_w <- !is.null(pl$weight)
      list(
        unweighted = .build_lazy_aggregation_method(
          function() build_for(FALSE), method
        ),
        weighted = .build_lazy_aggregation_method(
          if (has_w) function() build_for(TRUE) else function() build_for(FALSE),
          method
        )
      )
    }

     .build_scn_for_method <- function(ws, method, pl_v) {
      sc <- ws$sc
      if (length(sc) == 0L) {
        return(NULL)
      }
      bq <- AGG_BAND_Q
      setNames(lapply(seq_along(sc), function(s_idx) {
        s <- sc[[s_idx]]
        pipes <- s$pipelines
        is_log <- isTRUE(s$so$transform == "log")
        has_w <- !is.null(pipes[[1L]]$weight)
         build_for <- function(weighted) {
             if ((isTRUE(weighted) || !isTRUE(has_w)) && !identical(method, "prosperity_gap")) {
              suite_pov <- pl_v %||% 3
              suite_key <- paste0("scenario_suite_", names(sc)[[s_idx]], "_", format(suite_pov), "_", format(bandwidth_p0()))
             suite <- get0(suite_key, envir = ws$weighted_suite_cache)
             if (is.null(suite)) {
               suite <- aggregate_pipeline_tables_multi(
                 pipelines = pipes,
                 methods = agg_methods(),
                 weighted = weighted,
                  pov_lines = setNames(lapply(agg_methods(), function(x) pl_v %||% 3), agg_methods()),
                 residuals = ws$res,
                 is_log = is_log,
                 band_q = bq,
                 skip_coef = ws$skip,
                 bandwidth_p0 = bandwidth_p0(),
                 model_ids = names(pipes) %||% paste0("m", seq_along(pipes)),
                 shared_context = s$shared_context,
                 preparation_cache = ws$prep_cache
               )
               assign(suite_key, suite, envir = ws$weighted_suite_cache)
             }
             out <- suite[[method]]
             return(setNames(list(out), method))
           }
           out <- aggregate_pipeline_table(
            pipelines = pipes,
            method = method,
            weighted = weighted,
              pov_line = pl_v,
            residuals = ws$res,
            is_log = is_log,
            band_q = bq,
            skip_coef = ws$skip,
            bandwidth_p0 = bandwidth_p0(),
            model_ids = names(pipes) %||% paste0("m", seq_along(pipes)),
            shared_context = s$shared_context,
            preparation_cache = ws$prep_cache
          )
          setNames(list(out), method)
        }
        list(
          unweighted = .build_lazy_aggregation_method(
            function() build_for(FALSE), method
          ),
          weighted = .build_lazy_aggregation_method(
            if (has_w) function() build_for(TRUE) else function() build_for(FALSE),
            method
          )
        )
      }), names(sc))
    }

    # Cache seeding from adopted partials (plan 5.5) ----
    # When the workspace is built for the run whose partials were just adopted
    # (hist_sim()$.sig identical to the adoption signature), the displayed
    # method's tables are pre-inserted under the keys .get_hist_agg() /
    # .get_scn_agg() would use, so the first committed render does not
    # re-aggregate it. Pure cache: any mismatch (signature, method, residuals,
    # skip_coef, is_log, weight slot, model count) leaves the cache untouched.
    # The pov_line/bandwidth are folded into the key exactly as for a normal
    # lookup, so a different poverty line in the committed controls simply
    # misses the seeded entry. adopted_partials() is read with isolate():
    # mod_2_01 sets it in the same commit as hist_sim()/saved_scenarios().
    # Scenarios are seeded per slot: a scenario without a matching partial
    # keeps its normal lazy builder inside the same cached value.
    .seeded_lazy <- function(tbl, method) {
      x <- .new_lazy_aggregation_method_list(function() NULL, method)
      state <- attr(x, "state", exact = TRUE)
      state$value <- setNames(list(tbl), method)
      state$built <- TRUE
      x
    }

    .seed_aggregation_cache <- function(ws) {
      ap <- shiny::isolate(adopted_partials())
      hp <- ap$partials$historical
      sig <- ws$hs$.sig
      if (is.null(hp) || is.null(sig) || !identical(sig, ap$dependency_signature)) {
        return(invisible(FALSE))
      }
      expected_wk <- if (isTRUE(ws$hs$has_weights)) "weighted" else "unweighted"
      # Table of method m carried by partial p (`tables`, else the captured
      # method's `table`).
      tbl_of <- function(p, m) {
        tbl <- p$tables[[m]] %||%
          if (identical(m, p$display$method)) p$table else NULL
        if (is.data.frame(tbl)) tbl else NULL
      }
      matches <- function(p, so, expect_models = NULL) {
        d <- p$display
        if (is.null(d)) return(FALSE)
        if (is.null(d$method) || !is.character(d$method) || length(d$method) != 1L) {
          return(FALSE)
        }
        if (is.null(tbl_of(p, d$method))) return(FALSE)
        if (!identical(d$method, hp$display$method)) return(FALSE)
        if (!identical(d$weight_key, expected_wk)) return(FALSE)
        if (!identical(d$residuals, ws$res)) return(FALSE)
        if (!identical(isTRUE(d$skip_coef), isTRUE(ws$skip))) return(FALSE)
        if (!identical(isTRUE(d$is_log), isTRUE(so$transform == "log"))) return(FALSE)
        if (!identical(d$pov_line, hp$display$pov_line) ||
            !identical(d$bandwidth_p0, hp$display$bandwidth_p0)) {
          return(FALSE)
        }
        if (!is.null(expect_models) && !is.null(p$n_models) &&
            !identical(as.integer(p$n_models), as.integer(expect_models))) {
          return(FALSE)
        }
        TRUE
      }
      if (!matches(hp, ws$hs$so)) return(invisible(FALSE))
      pl_v <- hp$display$pov_line
      bw <- hp$display$bandwidth_p0
      wk <- hp$display$weight_key
      supported <- unname(hist_aggregate_choices(ws$hs$so$type, ws$hs$so$name))
      methods <- intersect(
        supported,
        unique(c(hp$display$method, hp$display$methods, names(hp$tables)))
      )
      methods <- methods[vapply(methods, function(m) !is.null(tbl_of(hp, m)), logical(1))]
      if (!length(methods)) return(invisible(FALSE))

      # One historical entry and one scenario entry per method. The workspace
      # cache is capped at 8 entries, which would evict seeded methods as
      # soon as the whole suite (up to 2 entries per method) is inserted;
      # size it to the seeded method count, still bounded.
      ws$cache_order$max_entries <- max(
        ws$cache_order$max_entries, 2L * length(methods) + 2L
      )

      sc <- ws$sc
      landed <- ap$partials$scenarios %||% list()
      ok_scn <- if (length(sc) > 0L) {
        Filter(function(nm) {
          p <- landed[[nm]]
          !is.null(p) &&
            matches(p, sc[[nm]]$so %||% ws$hs$so, length(sc[[nm]]$pipelines))
        }, names(sc))
      } else {
        character(0)
      }
      for (method in methods) {
        key_sfx <- .pl_bw_key(method, pl_v, bw)
        hist_val <- .build_hist_for_method(ws, method, pl_v)
        hist_val[[wk]] <- .seeded_lazy(tbl_of(hp, method), method)
        .cache_put(ws, paste0("h_", method, key_sfx), hist_val)

        seeded <- FALSE
        if (length(sc) > 0L) {
          scn_val <- .build_scn_for_method(ws, method, pl_v)
          for (nm in ok_scn) {
            tbl <- tbl_of(landed[[nm]], method)
            if (!is.null(tbl)) {
              scn_val[[nm]][[wk]] <- .seeded_lazy(tbl, method)
              seeded <- TRUE
            }
          }
          if (seeded) .cache_put(ws, paste0("s_", method, key_sfx), scn_val)
        }
      }
      invisible(TRUE)
    }

    .get_hist_agg <- function(method) {
      ws <- agg_workspace()
      pl_v <- pov_line_val()
      bw <- bandwidth_p0()
      key <- paste0("h_", method, .pl_bw_key(method, pl_v, bw))
      cached <- .cache_get(ws, key)
      if (!isTRUE(cached$found)) {
        value <- .build_hist_for_method(ws, method, pl_v)
        .cache_put(ws, key, value)
      } else {
        value <- cached$value
      }
      value
    }

    .get_scn_agg <- function(method) {
      ws <- agg_workspace()
      pl_v <- pov_line_val()
      bw <- bandwidth_p0()
      key <- paste0("s_", method, .pl_bw_key(method, pl_v, bw))
      cached <- .cache_get(ws, key)
      if (!isTRUE(cached$found)) {
        value <- .build_scn_for_method(ws, method, pl_v)
        .cache_put(ws, key, value)
      } else {
        value <- cached$value
      }
      value
    }

    hist_agg_rv <- reactive({
      method <- .selected_method()
      results_source()$hist_agg(method)
    })

    scenario_agg_rv <- reactive({
      src <- results_source()
      req(src, !is.null(src$scenario_names))
      if (length(src$scenario_names) == 0L) {
        return(NULL)
      }
      method <- .selected_method()
      src$scn_agg(method)
    })

    # Reactive computations (carried over from mod_2_06) ----


    # Always use survey weights when available (UI toggle removed - weighting
    # is the correct default for survey-based welfare estimates).
    weight_key <- reactive({
      src <- results_source()
      if (!is.null(src)) src$weight_key else "unweighted"
    })

    # Shared deviation reference - used by all_series_tbl and exceedance_ribbon
    hist_ref_val <- reactive({
      req(hist_agg_rv())
      method <- .selected_method()
      wk <- weight_key()
      deviation <- input$cmp_deviation %||% "none"
      if (identical(deviation, "none")) {
        return(0)
      }
      raw_vals <- hist_agg_rv()[[wk]][[method]]$value
      if (identical(deviation, "mean")) {
        mean(raw_vals, na.rm = TRUE)
      } else {
        stats::median(raw_vals, na.rm = TRUE)
      }
    })

    # Per-coefficient gradient of the historical reference being subtracted.
    # When deviation = mean: average of per-year F_agg across historical years.
    # When deviation = median: F_agg at the historical year closest to the median.
    # Used by .apply_contrast_sd() below to switch coefficient SE from
    # level-CI (||F_s||) to contrast-CI (||F_s - F_ref||), the correct SE for
    # paired counterfactual analysis on the same population.
    hist_F_agg_ref <- reactive({
      req(hist_agg_rv())
      method <- .selected_method()
      wk <- weight_key()
      deviation <- input$cmp_deviation %||% "none"
      if (identical(deviation, "none")) {
        return(NULL)
      }
      ht <- hist_agg_rv()[[wk]][[method]]
      if (is.null(ht) || nrow(ht) == 0L || !"F_agg_all" %in% names(ht)) {
        return(NULL)
      }
      # Historical has one "model" so each F_agg_all row is a 1 x K matrix.
      F_list <- lapply(ht$F_agg_all, function(m) {
        if (is.null(m) || !is.matrix(m) || nrow(m) == 0L) {
          NULL
        } else {
          as.numeric(m[1L, ])
        }
      })
      F_list <- Filter(Negate(is.null), F_list)
      if (length(F_list) == 0L) {
        return(NULL)
      }
      if (identical(deviation, "mean")) {
        Reduce(`+`, F_list) / length(F_list)
      } else {
        vals <- ht$value
        med_v <- stats::median(vals, na.rm = TRUE)
        med_idx <- which.min(abs(vals - med_v))
        if (length(med_idx) == 0L) {
          Reduce(`+`, F_list) / length(F_list)
        } else {
          F_list[[med_idx]]
        }
      }
    })

    # Replace per-(model, year) coefficient SDs with paired-contrast SDs
    # when a deviation reference is active. By overwriting `value_all_sd`
    # here, every downstream consumer (pointrange_bands_rv,
    # threshold_table_rv, exceedance_curves_rv) automatically uses the
    # tightened contrast variance.
    .apply_contrast_sd <- function(tbl, F_ref) {
      if (is.null(F_ref) || is.null(tbl) || nrow(tbl) == 0L) {
        return(tbl)
      }
      if (!"F_agg_all" %in% names(tbl) || !"value_all_sd" %in% names(tbl)) {
        return(tbl)
      }
      for (k in seq_len(nrow(tbl))) {
        F_mat <- tbl$F_agg_all[[k]]
        if (is.null(F_mat) || !is.matrix(F_mat) || ncol(F_mat) != length(F_ref)) {
          next
        }
        F_diff <- sweep(F_mat, 2L, F_ref, "-")
        tbl$value_all_sd[[k]] <- sqrt(rowSums(F_diff * F_diff))
      }
      tbl
    }

    # One immutable derived frame per published run/control state. Every
    # Results consumer below resolves its model/year matrix from this frame,
    # avoiding repeated reshaping and preserving identical canonical ordering.
    derived_results_frame_rv <- reactive({
      req(hist_agg_rv())
      wk <- weight_key()
      method <- .selected_method()
      deviation <- input$cmp_deviation %||% "none"
      F_ref <- hist_F_agg_ref()
      entries <- list()
      add_entry <- function(tbl, label, historical) {
        tbl <- .lazy_aggregation_table(tbl, method)
        if (is.null(tbl) || !nrow(tbl)) {
          return(invisible(NULL))
        }
        tbl <- .apply_contrast_sd(tbl, F_ref)
        key <- digest::digest(tbl, serialize = TRUE)
        entry <- new.env(parent = emptyenv())
        entry$table <- tbl
        entry$matrix <- by_model_matrix(tbl)
        entry$scenario <- label
        entry$is_historical <- historical
        entry$key <- key
        lockEnvironment(entry, bindings = TRUE)
        entries[[label]] <<- entry
        invisible(NULL)
      }
      add_entry(hist_agg_rv()[[wk]][[method]], "Historical", TRUE)
      sa <- scenario_agg_rv()
      if (!is.null(sa)) {
        for (label in names(sa)) {
          add_entry(sa[[label]][[wk]][[method]], label, FALSE)
        }
      }
      frame <- new.env(parent = emptyenv())
      frame$method <- method
      frame$deviation <- deviation
      frame$weight <- wk
      frame$.entries <- entries
      frame$.matrices <- stats::setNames(
        lapply(entries, `[[`, "matrix"),
        vapply(entries, `[[`, character(1L), "key")
      )
      lockEnvironment(frame, bindings = TRUE)
      frame
    })


    # Coefficient draws availability ----
    has_draws <- reactive({
      src <- results_source()
      req(src)
      isTRUE(src$has_draws)
    })


    # Landed scenario names only; pending ones are handled by placeholders.
    selected_scenario_names <- reactive({
      results_source()$scenario_names %||% character(0)
    })

    agg_hist <- reactive({
      req(hist_agg_rv())
      method <- .selected_method()
      deviation <- input$cmp_deviation %||% "none"
      out <- hist_agg_rv()[[weight_key()]][[method]]
      req(!is.null(out))
      hist_ref <- hist_ref_val()
      if (!identical(deviation, "none") && nrow(out) > 0) {
        out <- dplyr::mutate(out, value = value - hist_ref)
      }
      x_label <- if (identical(deviation, "none")) {
        label_agg_method(method)
      } else {
        paste0(label_agg_method(method), " \u2014 ", label_deviation(deviation))
      }
      list(out = out, x_label = x_label)
    })

    # `exceedance_ribbon` removed - the ribbon is now built inside
    # the static exceedance renderer directly from each series' (value_all, value_all_sd)
    # using analytic delta-method bands, so there is nothing to precompute here.

    # Three-source uncertainty decomposition ----
    # All three downstream displays (hero, exceedance, table) source their
    # bands from the helpers below. Each helper produces a per-scenario view
    # that decomposes uncertainty into:
    #   - coefficient (per-outcome SE from value_all_sd)
    #   - inter-annual (within-model spread of value_all across years)
    #   - inter-model  (across-model spread of model means; future only)

    # Helpers are now defined in R/fct_uncertainty_helpers.R as package-internal
    # functions so Module 3 can call the same code path. Aliases keep the
    # existing inline call sites below readable.
    .by_model_matrix <- function(tbl) {
      frame <- derived_results_frame_rv()
      key <- digest::digest(tbl, serialize = TRUE)
      hit <- .results_frame_matrix(frame, key)
      if (!is.null(hit)) {
        return(hit)
      }
      by_model_matrix(tbl)
    }
    .pct_label <- pct_label

    # pointrange_bands_rv: one row per scenario, three nested bands ----
    pointrange_bands_rv <- reactive({
      req(hist_agg_rv())
      bq_ens <- if (identical(input$ensemble_band %||% "none", "none")) {
        c(lo = 0.5, hi = 0.5)
      } else {
        resolve_band_q(input$ensemble_band %||% "none")
      }
      hist_ref <- hist_ref_val()
      wk <- weight_key()
      method <- .selected_method()

      one_scenario <- function(tbl, scenario_label, is_hist) {
        if (is.null(tbl) || nrow(tbl) == 0L) {
          return(NULL)
        }
        mm <- .by_model_matrix(tbl)
        if (is.null(mm)) {
          return(NULL)
        }
        vals <- mm$vals

        # Inter-model spread: per-model mean across years, then quantile across models.
        model_means <- rowMeans(vals, na.rm = TRUE)
        intermod <- if (is_hist || length(model_means) <= 1L) {
          mean_v <- mean(model_means, na.rm = TRUE)
          c(lo = mean_v, hi = mean_v)
        } else {
          c(
            lo = unname(stats::quantile(model_means, bq_ens[["lo"]], na.rm = TRUE)),
            hi = unname(stats::quantile(model_means, bq_ens[["hi"]], na.rm = TRUE))
          )
        }

        # Inter-annual variability: for each model take the band_q quantile
        # across years, then average across models.
        if (is_hist) {
          v_flat <- as.numeric(vals)
          interann <- c(
            lo = unname(stats::quantile(v_flat, bq_ens[["lo"]], na.rm = TRUE)),
            hi = unname(stats::quantile(v_flat, bq_ens[["hi"]], na.rm = TRUE))
          )
        } else {
          per_mod_lo <- apply(vals, 1L, stats::quantile,
            probs = bq_ens[["lo"]], na.rm = TRUE
          )
          per_mod_hi <- apply(vals, 1L, stats::quantile,
            probs = bq_ens[["hi"]], na.rm = TRUE
          )
          interann <- c(
            lo = mean(per_mod_lo, na.rm = TRUE),
            hi = mean(per_mod_hi, na.rm = TRUE)
          )
        }

        # Retained chart center: summarise each model across its weather years,
        # then take the median across models (not the expected headline center).
        ens_mean <- if (is_hist) {
          mean(as.numeric(vals), na.rm = TRUE)
        } else {
          stats::median(model_means, na.rm = TRUE)
        }

        tibble::tibble(
          scenario = scenario_label,
          value = ens_mean - hist_ref,
          interann_lo = unname(interann[["lo"]]) - hist_ref,
          interann_hi = unname(interann[["hi"]]) - hist_ref,
          intermod_lo = unname(intermod[["lo"]]) - hist_ref,
          intermod_hi = unname(intermod[["hi"]]) - hist_ref,
          is_historical = is_hist,
          n_models = length(mm$model_ids)
        )
      }

      rows <- list(one_scenario(
        .apply_contrast_sd(hist_agg_rv()[[wk]][[method]], hist_F_agg_ref()),
        "Historical", TRUE
      ))
      sa <- scenario_agg_rv()
      if (!is.null(sa) && length(sa) > 0L) {
        for (dk in names(sa)) {
          if (!dk %in% selected_scenario_names()) next
          rows[[length(rows) + 1L]] <- one_scenario(
            .apply_contrast_sd(sa[[dk]][[wk]][[method]], hist_F_agg_ref()),
            dk, FALSE
          )
        }
      }
      support_tables <- lapply(Filter(Negate(is.null), rows), attr, which = "adverse_support")
      out <- dplyr::bind_rows(Filter(Negate(is.null), rows))
      attr(out, "adverse_support") <- dplyr::bind_rows(Filter(Negate(is.null), support_tables))
      out
    })

    wise_export_table(
      key = "climate_adverse_support",
      label = "Climate baseline-anchored adverse support",
      step = 2L,
      fun = .committed_only(function() {
        support <- attr(threshold_table_rv(), "adverse_support")
        if (is.data.frame(support)) support else data.frame()
      }),
      description = "Per-model annual selected-metric baseline quantile and interpolation year-rank support. SSP point centers are equal-model means."
    )

    # Only the expected headline changes center; all existing chart/threshold
    # reactives retain their median ensemble convention and deviation behavior.
    headline_bands_rv <- reactive({
      bands <- pointrange_bands_rv()
      frame <- derived_results_frame_rv()
      for (i in seq_len(nrow(bands))) {
        entry <- .results_frame_entry(frame, bands$scenario[[i]])
        vals <- entry$matrix$vals
        vals[!is.finite(vals)] <- NA_real_
        bands$value[[i]] <- mean(rowMeans(vals, na.rm = TRUE), na.rm = TRUE) - hist_ref_val()
      }
      bands$center_method <- "equal_model_mean"
      bands
    })

    # timeseries_curves_rv: per (scenario, model, sim_year) values ----
    build_timeseries_curves <- function(selected_only = TRUE) {
      req(hist_agg_rv())
      hist_ref <- hist_ref_val()
      wk <- weight_key()
      method <- .selected_method()

      one_scenario <- function(tbl, scenario_label, is_hist) {
        if (is.null(tbl) || nrow(tbl) == 0L) {
          return(NULL)
        }
        mm <- .by_model_matrix(tbl)
        if (is.null(mm)) {
          return(NULL)
        }
        vals <- mm$vals
        rows <- lapply(seq_len(nrow(vals)), function(i) {
          tibble::tibble(
            scenario      = scenario_label,
            model_id      = mm$model_ids[[i]],
            sim_year      = as.integer(mm$sim_years),
            value         = vals[i, ] - hist_ref,
            is_historical = is_hist
          )
        })
        dplyr::bind_rows(rows)
      }

      rows <- list(one_scenario(
        .apply_contrast_sd(hist_agg_rv()[[wk]][[method]], hist_F_agg_ref()),
        "Historical", TRUE
      ))
      sa <- scenario_agg_rv()
      if (!is.null(sa) && length(sa) > 0L) {
        for (dk in names(sa)) {
          if (isTRUE(selected_only) && !dk %in% selected_scenario_names()) next
          rows[[length(rows) + 1L]] <- one_scenario(
            .apply_contrast_sd(sa[[dk]][[wk]][[method]], hist_F_agg_ref()),
            dk, FALSE
          )
        }
      }
      dplyr::bind_rows(Filter(Negate(is.null), rows))
    }

    timeseries_curves_rv <- reactive(build_timeseries_curves(TRUE))
    annual_distribution_curves_rv <- reactive(build_timeseries_curves(FALSE))

    # variance_breakdown_rv: one row per scenario, three components ----
    # Aggregates the per-(sim_year) var_within / var_across columns to scalars
    # and re-computes var_coef from the per-(model, year) SD list-column.
    variance_breakdown_rv <- reactive({
      req(derived_results_frame_rv())
      frame <- derived_results_frame_rv()

      one_scenario <- function(entry) {
        if (is.null(entry)) {
          return(NULL)
        }
        tbl <- entry$table
        scenario_label <- entry$scenario
        is_hist <- entry$is_historical
        sds_flat <- as.numeric(unlist(tbl$value_all_sd))
        var_coef <- if (length(sds_flat)) {
          mean(sds_flat^2, na.rm = TRUE)
        } else {
          0
        }
        # Use value-matrix-derived var_within / var_across so the metric
        # matches what the inter-annual / inter-model bands visualise and
        # avoids double-counting var_coef. (Unlike the pointrange total
        # band, this decomposition panel intentionally includes
        # var_within - its purpose is to show the share of every source,
        # including year-to-year spread.)
        mm <- .results_frame_matrix(frame, entry$key)
        vals <- if (is.null(mm)) NULL else mm$vals
        var_within <- if (!is.null(vals) && ncol(vals) > 1L) {
          v <- mean(apply(vals, 1L, stats::var, na.rm = TRUE), na.rm = TRUE)
          if (is.finite(v)) v else 0
        } else {
          0
        }
        var_across <- if (!is_hist && !is.null(vals) && nrow(vals) > 1L) {
          v <- stats::var(rowMeans(vals, na.rm = TRUE), na.rm = TRUE)
          if (is.finite(v)) v else 0
        } else {
          0
        }
        tibble::tibble(
          scenario      = scenario_label,
          var_coef      = var_coef,
          var_within    = var_within,
          var_across    = var_across,
          is_historical = is_hist
        )
      }

      labels <- names(frame$.entries)
      rows <- list(one_scenario(.results_frame_entry(frame, "Historical")))
      for (dk in setdiff(labels, "Historical")) {
        if (!dk %in% selected_scenario_names()) next
        rows[[length(rows) + 1L]] <- one_scenario(
          .results_frame_entry(frame, dk)
        )
      }
      dplyr::bind_rows(Filter(Negate(is.null), rows))
    })

    # exceedance_curves_rv: per (scenario, model) ECDF rows ----
    # One row per (scenario, model, rank). welfare_val is sorted in the adverse
    # tail direction; exceed_prob is (rank - 0.5)/n_pts limited to <= 0.50 AEP.
    exceedance_curves_rv <- reactive({
      req(hist_agg_rv())
      hist_ref <- hist_ref_val()
      wk <- weight_key()
      method <- .selected_method()
      so_obj <- tryCatch(results_source()$so, error = function(e) NULL)
      spec <- metric_metadata(method, so_obj)
      adverse_tail <- spec$adverse_tail

      one_scenario <- function(tbl, scenario_label, is_hist) {
        if (is.null(tbl) || nrow(tbl) == 0L) {
          return(NULL)
        }
        mm <- .by_model_matrix(tbl)
        if (is.null(mm)) {
          return(NULL)
        }
        vals <- mm$vals
        sds <- mm$sds
        n_yrs <- ncol(vals)
        if (n_yrs == 0L) {
          return(NULL)
        }

        do.call(dplyr::bind_rows, lapply(seq_len(nrow(vals)), function(i) {
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
          # If adverse_tail == "low" (welfare/consumption): smaller is worse -> ascending.
          # If adverse_tail == "high" (poverty): larger is worse -> descending.
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

          # Limit strictly to adverse tail: 0.50 AEP or less
          keep <- probs <= 0.50
          if (!any(keep)) {
            return(NULL)
          }

          tibble::tibble(
            scenario      = scenario_label,
            model_id      = mm$model_ids[[i]],
            rank          = seq_along(ord)[keep],
            welfare_val   = v_ord[keep] - hist_ref,
            coef_sd       = s_ord[keep],
            exceed_prob   = probs[keep],
            is_historical = is_hist
          )
        }))
      }

      rows <- list(one_scenario(
        .apply_contrast_sd(hist_agg_rv()[[wk]][[method]], hist_F_agg_ref()),
        "Historical", TRUE
      ))
      sa <- scenario_agg_rv()
      if (!is.null(sa) && length(sa) > 0L) {
        for (dk in names(sa)) {
          if (!dk %in% selected_scenario_names()) next
          rows[[length(rows) + 1L]] <- one_scenario(
            .apply_contrast_sd(sa[[dk]][[wk]][[method]], hist_F_agg_ref()),
            dk, FALSE
          )
        }
      }
      dplyr::bind_rows(Filter(Negate(is.null), rows))
    })

    # threshold_table_rv: long-format rows ready to pivot wide ----
    # One row per (scenario, Estimate, RP). Estimate names are derived from
    # the user's band quantile selection: e.g., with coef=p10_p90 and
    # ensemble=minmax the rows are "Central (P50)", "Coef P10", "Coef P90",
    # "Ensemble min", "Ensemble max", "Pooled P10", "Pooled P90". Pooled
    # rows combine the coefficient and inter-model components assuming
    # independence: SE_pooled = sqrt(coef_sd^2 + var_across_at_rp).
    # Historical (no inter-model component) does not emit Pooled rows -
    # they would duplicate the Coef rows.
    threshold_table_rv <- reactive({
      req(hist_agg_rv())
      bq_coef <- resolve_band_q("p10_p90")
      bq_ens <- if (identical(input$ensemble_band %||% "none", "none")) {
        c(lo = 0.5, hi = 0.5)
      } else {
        resolve_band_q(input$ensemble_band %||% "none")
      }
      z_coef_lo <- stats::qnorm(bq_coef[["lo"]])
      z_coef_hi <- stats::qnorm(bq_coef[["hi"]])
      hist_ref <- hist_ref_val()
      wk <- weight_key()
      method <- .selected_method()
      so_obj <- tryCatch(results_source()$so, error = function(e) NULL)
      adverse_tail <- metric_metadata(method, so_obj)$adverse_tail

      RPs <- c(RP_LOW, c("1:1" = 0.5), RP_HIGH)

      one_scenario <- function(tbl, scenario_label, is_hist) {
        if (is.null(tbl) || nrow(tbl) == 0L) {
          return(NULL)
        }
        mm <- .by_model_matrix(tbl)
        if (is.null(mm)) {
          return(NULL)
        }
        vals <- mm$vals
        sds <- mm$sds
        n_yrs <- ncol(vals)
        n_pts <- if (is_hist) sum(is.finite(as.numeric(vals))) else n_yrs

        # Each model must independently support each rank; a longer member
        # cannot manufacture support for a shorter one.
        rp_ok <- vapply(RPs, function(p) all(vapply(seq_len(nrow(vals)), function(i) {
          support <- adverse_year_support(vals[i, ], suppressWarnings(as.numeric(mm$sim_years)),
            p, adverse_tail)
          identical(support$status, "ok")
        }, logical(1))), logical(1))
        RPs_keep <- RPs[rp_ok]
        if (length(RPs_keep) == 0L) {
          return(NULL)
        }

        # Per-model rank-interp at each kept RP (matrix: model * RP) - shape
        # guaranteed by the helper (see by_model_rp_matrix()).
        per_model_rp <- matrix(NA_real_, nrow = nrow(vals), ncol = length(RPs_keep),
          dimnames = list(mm$model_ids, names(RPs_keep)))
        per_model_sd_at_rp <- matrix(NA_real_, nrow = nrow(vals), ncol = length(RPs_keep),
          dimnames = list(mm$model_ids, names(RPs_keep)))
        sd_years <- suppressWarnings(as.numeric(colnames(sds)))
        if (length(sd_years) != ncol(sds) || any(!is.finite(sd_years))) {
          sd_years <- suppressWarnings(as.numeric(mm$sim_years))
        }
        support_records <- vector("list", nrow(vals) * length(RPs_keep))
        for (i in seq_len(nrow(vals))) for (j in seq_along(RPs_keep)) {
          support <- adverse_year_support(vals[i, ], suppressWarnings(as.numeric(mm$sim_years)),
            RPs_keep[[j]], adverse_tail)
          support_records[[(i - 1L) * length(RPs_keep) + j]] <- data.frame(
            scenario = scenario_label, model_id = mm$model_ids[[i]],
            probability = RPs_keep[[j]], as.list(support), stringsAsFactors = FALSE)
          if (identical(support$status, "ok")) {
            applied <- apply_adverse_year_support(vals[i, ], suppressWarnings(as.numeric(mm$sim_years)), support)
            per_model_rp[i, j] <- applied$value
            sd_value <- if (identical(support$year_lo, support$year_hi) && support$weight_hi == 0) {
              if (is.finite(sds[i, match(support$year_lo, sd_years)])) sds[i, match(support$year_lo, sd_years)] else NA_real_
            } else {
              lo <- match(support$year_lo, sd_years); hi <- match(support$year_hi, sd_years)
              if (anyNA(c(lo, hi)) || any(!is.finite(sds[i, c(lo, hi)]))) NA_real_ else
                support$weight_lo * sds[i, lo] + support$weight_hi * sds[i, hi]
            }
            per_model_sd_at_rp[i, j] <- sd_value
          }
        }

        # Aggregate across models for each RP
        central_vec <- vapply(seq_len(ncol(per_model_rp)), function(j) {
          column <- per_model_rp[, j]
          if (!all(is.finite(column))) NA_real_ else mean(column)
        }, numeric(1))
        coef_sd_vec <- if (is_hist) {
          per_model_sd_at_rp[1L, ]
        } else {
          apply(per_model_sd_at_rp, 2L, stats::median, na.rm = TRUE)
        }
        coef_lo_vec <- central_vec + z_coef_lo * coef_sd_vec
        coef_hi_vec <- central_vec + z_coef_hi * coef_sd_vec

        intermod_lo_vec <- if (is_hist) {
          rep(NA_real_, length(RPs_keep))
        } else {
          apply(per_model_rp, 2L, stats::quantile,
            probs = bq_ens[["lo"]], na.rm = TRUE
          )
        }
        intermod_hi_vec <- if (is_hist) {
          rep(NA_real_, length(RPs_keep))
        } else {
          apply(per_model_rp, 2L, stats::quantile,
            probs = bq_ens[["hi"]], na.rm = TRUE
          )
        }

        # Total band combines coefficient and inter-model variance at each RP,
        # assuming independence. Inter-annual variability is already baked
        # into the per-rank value so it isn't added a second time here.
        var_across_at_rp <- if (is_hist) {
          rep(0, length(RPs_keep))
        } else {
          apply(per_model_rp, 2L, stats::var, na.rm = TRUE)
        }
        var_across_at_rp[is.na(var_across_at_rp)] <- 0
        sd_total_vec <- sqrt(pmax(coef_sd_vec^2 + var_across_at_rp, 0,
          na.rm = FALSE
        ))
        total_lo_vec <- central_vec + z_coef_lo * sd_total_vec
        total_hi_vec <- central_vec + z_coef_hi * sd_total_vec

        make_row <- function(estimate, vec) {
          tibble::tibble(
            scenario = scenario_label,
            Estimate = estimate,
            rp_name = names(RPs_keep),
            rp_label = names(RPs_keep),
            value = vec - hist_ref,
            absolute_value = vec,
            n_obs = n_pts,
            is_historical = is_hist
          )
        }
        coef_lo_lbl <- paste0("Coef ", .pct_label(bq_coef[["lo"]]))
        coef_hi_lbl <- paste0("Coef ", .pct_label(bq_coef[["hi"]]))
        ens_lo_lbl <- paste0("Ensemble ", .pct_label(bq_ens[["lo"]],
          use_minmax = TRUE
        ))
        ens_hi_lbl <- paste0("Ensemble ", .pct_label(bq_ens[["hi"]],
          use_minmax = TRUE
        ))
        pooled_lo_lbl <- paste0("Pooled ", .pct_label(bq_coef[["lo"]]))
        pooled_hi_lbl <- paste0("Pooled ", .pct_label(bq_coef[["hi"]]))

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
        supports <- dplyr::bind_rows(support_records)
        attr(out, "adverse_support") <- supports
        out
      }

      rows <- list(one_scenario(
        .apply_contrast_sd(hist_agg_rv()[[wk]][[method]], hist_F_agg_ref()),
        "Historical", TRUE
      ))
      sa <- scenario_agg_rv()
      if (!is.null(sa) && length(sa) > 0L) {
        for (dk in names(sa)) {
          if (!dk %in% selected_scenario_names()) next
          rows[[length(rows) + 1L]] <- one_scenario(
            .apply_contrast_sd(sa[[dk]][[wk]][[method]], hist_F_agg_ref()),
            dk, FALSE
          )
        }
      }
      dplyr::bind_rows(Filter(Negate(is.null), rows))
    })

    # UI-48: register Step 2's result figures for the export bundle.
    wise_export_table(
      key = "climate_headline_summary",
      label = "Climate headline summary cards",
      step = 2L,
      fun = .committed_only(function() step2_headline_df(headline_cards_data_rv())),
      description = paste(
        "Headline values are display-formatted; native numeric endpoints, units, selected deviation, metric context, and summary operators are included.",
        "Expected, model-agreement and range summaries average years within model and weight climate models equally; adverse annual selected-metric quantiles use an equal-model mean."
      )
    )

    # Zero-arg echarts closures shared by the renders and the export bundle
    # (guidelines §7): one builder call per chart, used for both the live
    # output and wise_export_figure(). Heights match the UI slots.
    pointrange_chart <- function() {
      bands <- pointrange_bands_rv()
      req(bands)
      if (identical(input$ensemble_band %||% "none", "none")) {
        bands$intermod_lo <- NA_real_
        bands$intermod_hi <- NA_real_
      }
      echart_pointrange_climate(
        bands_tbl    = bands,
        x_label      = agg_hist()$x_label,
        group_order  = "scenario_x_year",
        show_coef    = FALSE,
        height       = "600px"
      )
    }
    wise_export_figure(
      key = "climate_outcome_distribution",
      label = "Simulated welfare by scenario and period",
      step = 2L,
      fun = .committed_only(pointrange_chart),
      description = "Simulated welfare by climate scenario and projection period; median across climate-model means, distinct from the equal-model-mean expected headline.",
      width = 10, height = 6.5
    )

    annual_distribution_chart <- function() {
      req(annual_distribution_curves_rv())
      echart_annual_distribution(
        annual_distribution_curves_rv(),
        x_label = metric_axis_label(
          .selected_method(),
          results_source()$so,
          input$cmp_deviation %||% "none"
        ),
        plot_type = input$annual_distribution_type %||% "violin",
        height = "470px",
        pending = results_source()$pending_names
      )
    }
    # Streaming renders: while provisional, keep the existing ECharts instance
    # (dispose = FALSE) when only new scenario data arrived, so ECharts animates
    # the added series instead of redrawing the whole chart from zero. A change
    # of run, mode or display control, or the last pending scenario landing,
    # forces a full redraw because setOption() merging cannot remove series.
    .SMOOTH_CONTROLS <- c(
      "cmp_agg_method", "cmp_deviation", "annual_distribution_type",
      "ensemble_band", "exceedance_model_spread"
    )
    smooth_state <- new.env(parent = emptyenv())
    .smooth_echart <- function(ch, id) {
      key <- shiny::isolate({
        src <- results_source()
        list(
          provisional = identical(src$mode, "provisional"),
          generation = live_data()$generation,
          pending = length(src$pending_names) > 0L,
          controls = lapply(.SMOOTH_CONTROLS, function(nm) input[[nm]])
        )
      })
      prev <- smooth_state[[id]]
      smooth_state[[id]] <- key
      if (isTRUE(key$provisional) && identical(prev, key)) ch$x$dispose <- FALSE
      ch
    }

    output$annual_distribution_plot <- echarts4r::renderEcharts4r({
      ch <- annual_distribution_chart()
      req(!is.null(ch))
      .smooth_echart(ch, "annual_distribution_plot")
    })

    incidence_data_rv <- reactive({
      # Committed only: needs full pipelines and the survey.
      req(!.is_provisional(), hist_sim(), saved_scenarios(), shiny::isolate(input$cmp_agg_method))
      sc <- selected_scenario_names()
      if (!length(sc)) {
        return(tibble::tibble())
      }
      is_log <- identical(hist_sim()$so$transform, "log")
      svy <- hist_sim()$svy %||% hist_sim()$survey
      if (is.null(svy)) {
        return(tibble::tibble())
      }
      dplyr::bind_rows(lapply(sc, function(nm) {
        entry <- saved_scenarios()[[nm]]
        pipes <- entry$pipelines %||% list(entry$pipeline %||% entry)
        step2_incidence_by_decile(
          svy, hist_sim()$so$name, hist_sim()$pipeline, pipes, is_log, nm
        )
      }))
    })

    # Distributional incidence table: raw values; the CSV button for it
    # lives in the export bundle's reactable flow (there is no mounted UI
    # slot - the data table is exported as the bundle artefact below).
    wise_export_table(
      key = "climate_distributional_incidence_data",
      label = "Distributional incidence data",
      step = 2L,
      fun = .committed_only(function() {
        annotate_visualization_export(
          incidence_data_rv(), .selected_method(), hist_sim()$so,
          observation_unit = "household-level simulated welfare effect",
          aggregation_order = "fixed weighted observed baseline decile; weighted mean over households and model summaries",
          uncertainty = "scenario/model variation summarized by selected model set"
        )
      }),
      description = "Tidy weighted incidence data by fixed baseline welfare decile."
    )

    # Export the same tidy annual aggregates used by the distribution plot.
    annual_distribution_export <- function() {
      curves <- annual_distribution_curves_rv()
      req(curves)
      annotate_visualization_export(
        curves,
        .selected_method(),
        results_source()$so,
        observation_unit = "annual aggregate for fixed survey population under one weather-year draw",
        aggregation_order = "weighted household aggregate by model and weather-year; model means retained",
        uncertainty = "inter-annual weather variation"
      )
    }
    wise_export_figure(
      key = "climate_annual_distribution",
      label = "Annual outcome distribution across weather years",
      step = 2L,
      fun = .committed_only(annual_distribution_chart),
      description = "Annual aggregate distribution for the fixed population; one observation is one model-weather-year draw.",
      width = 10, height = 6.5
    )
    wise_export_table(
      key = "climate_annual_distribution_data",
      label = "Annual outcome distribution data",
      step = 2L,
      fun = .committed_only(annual_distribution_export),
      description = "Tidy data behind the annual aggregate distribution, including metric and aggregation metadata."
    )
    # UI-48: one builder behind the on-screen table, its CSV button and the
    # export bundle.
    threshold_table_df <- function() {
      tbl <- threshold_table_rv()
      if (is.null(tbl) || !nrow(tbl) || !"Estimate" %in% names(tbl)) {
        return(NULL)
      }
      so_obj <- tryCatch(results_source()$so, error = function(e) NULL)
      n_h_yrs <- tryCatch(
        {
          run_info <- results_source()$sim_summary %||% list()
          hy <- run_info$historical_years %||% integer(0)
          if (length(hy) >= 2L) as.integer(hy[2] - hy[1] + 1L) else max(tbl$n_obs, na.rm = TRUE)
        },
        error = function(e) max(tbl$n_obs, na.rm = TRUE)
      )

      build_threshold_table_df(
        threshold_tbl = tbl,
        group_order   = "scenario_x_year",
        show_coef     = TRUE,
        adverse_only  = TRUE,
        method        = .selected_method(),
        so            = so_obj,
        n_hist_years  = n_h_yrs
      )
    }

    uncertainty_chart <- function() {
      req(variance_breakdown_rv())
      echart_variance_contribution(
        variance_breakdown_rv(), height = "300px",
        percent = identical(metric_metadata(
          .selected_method(), results_source()$so
        )$format, "percent")
      )
    }
    output$uncertainty_sources_plot <- echarts4r::renderEcharts4r({
      ch <- uncertainty_chart()
      req(!is.null(ch))
      .smooth_echart(ch, "uncertainty_sources_plot")
    })
    outputOptions(output, "uncertainty_sources_plot", suspendWhenHidden = TRUE)

    wise_export_figure(
      key = "climate_uncertainty_sources",
      label = "Climate simulation uncertainty sources",
      step = 2L,
      fun = .committed_only(uncertainty_chart),
      description = "Standard deviation contribution from weather-year, coefficient, residual, and climate-model uncertainty sources.",
      width = 9, height = 5
    )

    adverse_dot_data_rv <- reactive({
      req(threshold_table_rv())
      table <- threshold_table_rv()
      historical_rows <- table$scenario == "Historical" &
        table$Estimate %in% c("Single historical estimate", "Equal-model mean", "Central (P50)")
      historical_year_count <- if ("n_obs" %in% names(table) && any(historical_rows)) {
        max(as.numeric(table$n_obs[historical_rows]), na.rm = TRUE)
      } else NA_real_
      rp_map <- metric_decision_return_periods(.selected_method(), results_source()$so)
      if (is.finite(historical_year_count)) {
        rp_map <- rp_map[names(rp_map) == "Expected" |
          vapply(names(rp_map), function(label) {
            if (identical(label, "Expected")) return(TRUE)
            historical_year_count >= as.integer(sub("^Adverse 1-in-", "", label))
          }, logical(1))]
      }
      dot <- step2_adverse_dot_data(
        table[table$rp_name %in% unname(rp_map), , drop = FALSE],
        method = .selected_method(),
        so = results_source()$so
      )
      if (identical(input$ensemble_band %||% "none", "none") && nrow(dot)) {
        dot$intermod_lo <- NA_real_
        dot$intermod_hi <- NA_real_
      }
      dot
    })
    adverse_dot_chart <- function() {
      req(adverse_dot_data_rv())
      echart_step2_adverse_dot(
        adverse_dot_data_rv(),
        x_label = metric_axis_label(
          .selected_method(),
          results_source()$so,
          input$cmp_deviation %||% "none"
        ),
        height = "380px",
        pending = results_source()$pending_names
      )
    }
    output$adverse_dot_plot <- echarts4r::renderEcharts4r({
      ch <- adverse_dot_chart()
      req(!is.null(ch))
      .smooth_echart(ch, "adverse_dot_plot")
    })
    outputOptions(output, "adverse_dot_plot", suspendWhenHidden = TRUE)

    wise_export_figure(
      key = "climate_adverse_return_periods",
      label = "Outcome in adverse weather years",
      step = 2L,
      fun = .committed_only(adverse_dot_chart),
      description = "Expected and adverse return-period outcomes with inter-model ensemble spread.",
      width = 9, height = 5
    )

    wise_export_table(
      key = "climate_outcome_thresholds",
      label = "Outcome threshold exceedance",
      step = 2L,
      fun = .committed_only(threshold_table_df),
      description = paste(
        "Simulated welfare outcomes against each threshold, by climate",
        "scenario and projection period, with uncertainty bounds."
      )
    )

    # Return-period decision table as a reactable (guidelines §6): raw values
    # in the data, rounded only for display (.threshold_col_defs()), rendered
    # client-side; the CSV download moved to the shared client-side
    # wise_reactable_csv_button() next to the widget. INT-08: while the
    # results are stale the table stays visible.
    threshold_reactable <- function() {
      df <- threshold_table_df()
      if (is.null(df) || nrow(df) == 0L) {
        return(.step2_reactable_note("Insufficient data"))
      }
      src <- results_source()
      if (identical(src$mode, "provisional")) {
        df <- .append_pending_threshold_rows(df, src$pending_names)
      }
      .step2_reactable(df, col_defs = .threshold_col_defs(df))
    }
    output$summary_threshold_table <- reactable::renderReactable({
      threshold_reactable()
    })

    # UI-48: the exceedance curve, for the export bundle.
    exceedance_chart <- function() {
      curves <- exceedance_curves_rv()
      ah <- agg_hist()
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
          .selected_method(),
          results_source()$so,
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
      key = "climate_exceedance_curve",
      label = "Welfare exceedance probability",
      step = 2L,
      fun = .committed_only(exceedance_chart),
      description = paste(
        "Probability of welfare falling below each level in adverse weather years, by climate scenario",
        "and projection period."
      ),
      width = 10, height = 6.5
    )

    output$exceedance_plot <- echarts4r::renderEcharts4r({
      ch <- exceedance_chart()
      req(!is.null(ch))
      .smooth_echart(ch, "exceedance_plot")
    })


    # observeEvent handlers ----

    # Insert the Results tab once the display source first has data
    # (provisional or committed); remove it again when the source is empty
    # (INT-07) so the empty state returns and a later run re-inserts a fresh
    # tab instead of writing into a stale one.
    #
    # The pane is built from the outcome (`so`) only; values flow through
    # reactives. It is therefore rebuilt (clear + insert) only when the
    # outcome changes or a committed run replaces a committed run (today's
    # behaviour) - never on each partial, and not on provisional -> committed
    # adoption or committed <-> provisional switches with an unchanged `so`.
    results_tab_added <- reactiveVal(FALSE)
    # Bumped each time the pane is (re)built, so control state can be re-sent.
    pane_rev <- reactiveVal(0L)

    # Run identity of the provisional source. A reactiveVal drops identical
    # writes, so the lifecycle observers below do not re-fire per partial.
    live_pane_key <- reactiveVal(NULL)
    observe({
      lr <- live_run()
      hp <- lr$partials$historical
      live_pane_key(
        if (is.null(hp)) NULL else list(generation = lr$generation, so = hp$so)
      )
    })
    .pane_trigger <- function() list(hist_sim(), live_pane_key())
    .pane_so <- function() {
      lk <- live_pane_key()
      if (!is.null(lk)) lk$so else hist_sim()$so
    }
    pane_state <- new.env(parent = emptyenv())
    pane_state$mode <- NULL
    pane_state$so <- NULL

    observeEvent(.pane_trigger(),
      {
        provisional <- !is.null(live_pane_key())
        if (is.null(.pane_so())) {
          if (results_tab_added()) {
            shiny::removeTab(
              inputId = tabset_id,
              target  = "sim_tab",
              session = tabset_session
            )
            results_tab_added(FALSE)
          }
          pane_state$mode <- NULL
          pane_state$so <- NULL
          return()
        }
        so <- .pane_so()
        mode <- if (provisional) "provisional" else "committed"
        keep_pane <- results_tab_added() &&
          identical(so, pane_state$so) &&
          !(identical(mode, "committed") && identical(pane_state$mode, "committed"))
        pane_state$mode <- mode
        pane_state$so <- so
        if (keep_pane) {
          return()
        }

        # UI-50: only ever one Results tab. This used to append unconditionally,
        # so every re-run of Step 2 added another copy - and because the new tab
        # carried a second `#results_section`, the `insertUI()` below targeted
        # the *first* match, filling the original tab and leaving the new one
        # empty. Steps 1 and 3 already guarded their appends; this brings Step 2
        # into line.
        if (!results_tab_added()) {
          shiny::appendTab(
            inputId = tabset_id,
            shiny::tabPanel(
              title = "Results",
              value = "sim_tab",
              shiny::div(id = ns("results_section"))
            ),
            select = TRUE,
            session = tabset_session
          )
          results_tab_added(TRUE)
        } else {
          # The tab is already there. Clear its contents so the re-run's results
          # replace the previous run's rather than stacking beneath them - the
          # pane is built from the outcome (`so`), which a new run may have
          # changed.
          # Both this and the insert below are deferred to the end of the flush
          # and run in call order, so the clear always precedes the rewrite.
          # Deferring also keeps the first-run path byte-for-byte as it was:
          # appendTab's DOM insertion lands before anything targets
          # #results_section.
          shiny::removeUI(selector = paste0("#", ns("results_section"), " > *"), multiple = TRUE)
          try(shiny::updateTabsetPanel(tabset_session,
            inputId = tabset_id,
            selected = "sim_tab"
          ), silent = TRUE)
        }

        sw_obj <- tryCatch(if (is.function(selected_weather)) selected_weather() else selected_weather, error = function(e) NULL)
        wx_lbl <- if (!is.null(sw_obj) && "label" %in% names(sw_obj) && length(sw_obj$label)) {
          paste(tolower(sw_obj$label), collapse = ", ")
        } else if (!is.null(sw_obj) && "name" %in% names(sw_obj) && length(sw_obj$name)) {
          paste(tolower(sw_obj$name), collapse = ", ")
        } else {
          ""
        }

        shiny::insertUI(
          selector = paste0("#", ns("results_section")),
          where    = "afterBegin",
          ui       = .results_content_ui(ns, so, weather_var = wx_lbl)
        )
        pane_rev(isolate(pane_rev()) + 1L)
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE
    )

    # On subsequent runs, just re-select the tab.
    observeEvent(.pane_trigger(),
      {
        if (!is.null(.pane_so())) {
          shiny::updateTabsetPanel(
            session  = tabset_session,
            inputId  = tabset_id,
            selected = "sim_tab"
          )
        }
      },
      ignoreInit = TRUE
    )

    # Keep agg method choices in sync with outcome.
    observeEvent(.pane_trigger(),
      {
        req(.pane_so())
        so <- .pane_so()
        choices <- hist_aggregate_choices(so$type, so$name)
        current <- isolate(input$cmp_agg_method)
        new_sel <- if (!is.null(current) && current %in% choices) current else "mean"
        src <- isolate(results_source())
        if (identical(src$mode, "provisional") &&
            !new_sel %in% src$methods_available) {
          new_sel <- src$locked_method
        }
        shiny::updateRadioButtons(session, "cmp_agg_method",
          choices  = choices,
          selected = new_sel,
          inline   = TRUE
        )
      },
      ignoreInit = TRUE
    )

    # Provisional banner ----
    # Shown only while a streaming run drives the display. Progress uses the
    # same fraction as the sidebar run panel (.step2_live_pct()).
    output$provisional_banner <- renderUI({
      src <- results_source()
      if (is.null(src) || !identical(src$mode, "provisional")) {
        return(NULL)
      }
      lr <- live_run()
      n_done <- length(src$scenario_names)
      n_all <- n_done + length(src$pending_names)
      pct <- .step2_live_pct(list(
        has_hist = TRUE,
        done_labels = names(lr$partials$scenarios) %||% character(0),
        groups_total = lr$groups_total, groups_done = lr$groups_done,
        current_label = lr$current_label,
        member_index = lr$member_index, period_members = lr$period_members
      ))
      shiny::div(
        class = "alert alert-info provisional-banner",
        style = "margin-bottom: 12px; padding: 8px 12px; font-size: 0.85rem;",
        # While streaming, keep landed charts fully visible during the
        # per-partial recalculation instead of Shiny's dimmed state. Removed
        # with the banner, so committed-mode loading cues are unchanged.
        shiny::tags$style(
          paste0("#", ns("results_section"), " .recalculating { opacity: 1 !important; transition: none; }")
        ),
        shiny::div(
          shiny::strong("Simulation in progress: "),
          paste0(n_done, " of ", n_all, " scenarios ready. Results are provisional.")
        ),
        if (!is.null(pct)) {
          shiny::div(
            class = "progress",
            shiny::div(
              class = "progress-bar", role = "progressbar",
              `aria-valuenow` = round(100 * pct), `aria-valuemin` = 0,
              `aria-valuemax` = 100,
              style = sprintf("width: %d%%;", round(100 * pct))
            )
          )
        },
        shiny::div(
          class = "text-muted",
          paste(
            "The poverty line unlocks when the run finishes.",
            if (length(setdiff(
              unname(hist_aggregate_choices(src$so$type, src$so$name)),
              src$methods_available
            ))) "Prosperity gap is also unavailable until then.",
            "Step 3 uses the last completed run, and Step 2 exports",
            "wait for the run to finish."
          )
        )
      )
    })

    # Control gating ----
    # One observer keyed on the display mode: in provisional mode only the
    # method pills that were not streamed (typically prosperity_gap) and the
    # poverty line are disabled; any exit (adoption, cancel, stale, failure)
    # flips the mode back and re-enables them. pane_rev() re-sends the state
    # after the pane is (re)built, because inserted controls start enabled.
    # The pill selection is only forced to the captured method when the
    # current one is unavailable; it is not written to display_settings_out
    # (committed-only).
    .gate_key <- reactive({
      src <- results_source()
      if (is.null(src) || !identical(src$mode, "provisional")) {
        list(provisional = FALSE)
      } else {
        list(
          provisional = TRUE, locked = src$locked_method, so = src$so,
          available = src$methods_available
        )
      }
    })
    observe({
      key <- .gate_key()
      pane_rev()
      req(results_tab_added())
      if (isTRUE(key$provisional)) {
        choices <- unname(hist_aggregate_choices(key$so$type, key$so$name))
        update_pill_toggle_disabled(
          session, "cmp_agg_method",
          disabled = setdiff(choices, key$available),
          tooltip = "Available when the simulation completes"
        )
        update_input_disabled(session, "pov_line", TRUE,
          tooltip = "Available when the simulation completes"
        )
        current <- shiny::isolate(input$cmp_agg_method)
        if (is.null(current) || !current %in% key$available) {
          shiny::updateRadioButtons(session, "cmp_agg_method",
            selected = key$locked
          )
        }
      } else {
        update_pill_toggle_disabled(session, "cmp_agg_method", FALSE)
        update_input_disabled(session, "pov_line", FALSE)
      }
    })

    # Display settings writer ----
    # Publishes the committed-mode controls so the next run is submitted with
    # them (mod_2_01 reads this at submit). Not written while provisional,
    # and not before the controls exist.
    if (!is.null(display_settings_out)) {
      observe({
        src <- results_source()
        req(src, identical(src$mode, "committed"), input$cmp_agg_method)
        display_settings_out(list(
          method = .selected_method(),
          pov_line = pov_line_val(),
          bandwidth_p0 = bandwidth_p0()
        ))
      })
    }

    # Suspend outputs when Results tab is hidden ----
    outputOptions(output, "annual_distribution_plot", suspendWhenHidden = TRUE)
    outputOptions(output, "summary_threshold_table", suspendWhenHidden = TRUE)
    outputOptions(output, "exceedance_plot", suspendWhenHidden = TRUE)

    # Return API ----
    ts_committed <- new.env(parent = emptyenv())
    ts_committed$value <- NULL
    # timeseries_curves bundles everything the Diagnostics tab needs to render
    # the per-model trajectories plot (the plot lives there now): the
    # per-(scenario, model, sim_year) table, the x-axis label, and the
    # inter-model band quantiles resolved from the Results-tab controls.
    list(
      variance_breakdown = variance_breakdown_rv,
      results_tab_added = results_tab_added,
      aggregation_cache = aggregation_cache,
      derived_results_frame = derived_results_frame_rv,
      timeseries_curves = reactive({
        # Diagnostics is never driven by provisional data: while a run
        # streams, return the last committed value (no dependency on the
        # partials, so it does not recompute per partial). Kept lazily rather
        # than by an eager observer so the table is only built when the
        # Diagnostics tab asks for it. NULL if nothing was committed yet.
        if (.is_provisional()) {
          return(ts_committed$value)
        }
        if (is.null(hist_sim())) ts_committed$value <- NULL
        req(timeseries_curves_rv())
        ens_q <- if (!identical(input$ensemble_band %||% "none", "none")) {
          resolve_band_q(input$ensemble_band %||% "none")
        } else {
          c(lo = 0.5, hi = 0.5)
        }
        out <- list(
          tbl     = timeseries_curves_rv(),
          x_label = metric_axis_label(.selected_method(), results_source()$so),
          ens_q   = ens_q
        )
        ts_committed$value <- out
        out
      })
    )
  })
}
