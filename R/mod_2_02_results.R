#' 2_02_results UI Function
#'
#' @description A shiny Module. Renders the Results tab content: point-range
#'   chart, threshold table/bar, and exceedance curve. Consolidates logic from
#'   the former mod_2_02_historical_sim (tab insertion) and
#'   mod_2_06_sim_compare (all visualisations).
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_2_02_results_ui <- function(id) {
  # Placeholder — the real content is injected via insertUI in the server.
  tagList()
}


#' Results tab content UI (inserted into the Results tabPanel once).
#' @noRd
.results_content_ui <- function(ns, so) {
  tagList(
    # ---- 1. Results header -------------------------------------------------
    shiny::uiOutput(ns("results_header_ui")),

    # ---- 2. Analysis controls ----------------------------------------------
    shiny::wellPanel(
      style = "padding: 8px 12px 4px 12px;",
      # Row 1: Outcome
      shiny::tags$p("Outcome",
                    style = "font-weight:600; margin: 0 0 4px 0; font-size:12px;"),
      shiny::tags$div(
        style = "display:flex; align-items:flex-end; gap:10px; flex-wrap:wrap; margin-bottom:4px;",
        shiny::tags$div(style = "flex:1; min-width:140px;",
          shiny::selectInput(
            ns("cmp_agg_method"),
            label    = "Aggregation method",
            choices  = hist_aggregate_choices(so$type, so$name),
            selected = "mean"
          )
        ),
        shiny::tags$div(style = "flex:1; min-width:140px;",
          shiny::conditionalPanel(
            condition = paste0("['headcount_ratio','gap','fgt2',",
                               "'prosperity_gap','avg_poverty']",
                               ".indexOf(input['", ns("cmp_agg_method"), "']) > -1"),
            shiny::numericInput(
              ns("pov_line"),
              label = "Poverty line (daily, 2021 PPP USD)",
              value = 3.00, min = 0, step = 0.5
            )
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
        )
      ),

      # Row 2: Uncertainty
      shiny::tags$p("Uncertainty",
                    style = "font-weight:600; margin: 6px 0 4px 0; font-size:12px;"),
      shiny::tags$div(
        style = "display:flex; align-items:flex-end; gap:10px; flex-wrap:wrap; margin-bottom:4px;",
        shiny::tags$div(style = "flex:1; min-width:160px;",
          shiny::selectInput(
            ns("uncertainty_band"),
            label   = "Coefficient band",
            choices = c(
              "50% (p25–p75)"   = "p25_p75",
              "60% (p20–p80)"   = "p20_p80",
              "80% (p10–p90)"   = "p10_p90",
              "90% (p05–p95)"   = "p05_p95",
              "95% (p025–p975)" = "p025_p975",
              "99% (p005–p995)" = "p005_p995",
              "Max (min–max)"   = "minmax"
            ),
            selected = "p10_p90"
          )
        ),
        shiny::tags$div(style = "flex:1; min-width:160px;",
          shiny::selectInput(
            ns("ensemble_band"),
            label    = "Inter-model band",
            choices  = c(
              "50% (p25–p75)"   = "p25_p75",
              "60% (p20–p80)"   = "p20_p80",
              "80% (p10–p90)"   = "p10_p90",
              "90% (p05–p95)"   = "p05_p95",
              "95% (p025–p975)" = "p025_p975",
              "99% (p005–p995)" = "p005_p995",
              "Full range (min–max)" = "minmax"
            ),
            selected = "minmax"
          )
        ),
        shiny::tags$div(
          style = "flex:1; min-width:160px; padding-bottom:6px;",
          shiny::checkboxInput(
            ns("show_coef_uncertainty"),
            label = "Show coefficient uncertainty",
            value = TRUE
          ),
          shiny::checkboxInput(
            ns("show_model_spread"),
            label = "Show inter-model spread",
            value = TRUE
          ),
          shiny::uiOutput(ns("coef_uncertainty_status_ui"))
        )
      ),

      # Advanced
      shiny::tags$details(
        shiny::tags$summary(
          style = "cursor:pointer; font-size:11px; color:#555; font-weight:600;",
          "Advanced ▼"
        ),
        shiny::tags$div(
          style = "display:flex; gap:10px; flex-wrap:wrap; margin-top:4px;",
          shiny::tags$div(style = "flex:1; min-width:160px;",
            shiny::numericInput(
              ns("bandwidth_p0"),
              label = "Headcount smoothing bandwidth (log scale)",
              value = 0.05, min = 0.005, max = 0.5, step = 0.01
            )
          ),
          shiny::tags$div(style = "flex:1; min-width:160px;",
            shiny::radioButtons(
              ns("cmp_group_order"),
              label    = "Group by",
              choices  = c(
                "Scenario × Year" = "scenario_x_year",
                "Year × Scenario" = "year_x_scenario"
              ),
              selected = "scenario_x_year",
              inline   = TRUE
            )
          )
        ),
        shiny::helpText(
          "Smoothing bandwidth for the headcount-ratio kernel approximation. ",
          "Smaller = sharper indicator, wider bands at the threshold. 0.05 (default) is a good starting point.",
          style = "font-size:11px; color:#555;"
        )
      ),

      # Scenario filters
      shiny::tags$hr(style = "margin: 6px 0;"),
      shiny::tags$p("Scenario filters",
                    style = "font-weight:600; margin: 0 0 4px 0; font-size:12px;"),
      shiny::uiOutput(ns("scenario_filter_ui"))
    ),

    # ---- 3. Hero point-range chart -----------------------------------------
    shiny::wellPanel(
      shiny::h4("Distribution of outcomes across climate scenarios and timeframes"),
      shiny::plotOutput(ns("summary_box_plot"), height = "600px"),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:6px;",
        "All bands are drawn ",
        shiny::tags$b("relative to the central dot"),
        " (the ensemble mean) and answer different questions about uncertainty. They are not meant to be added together — see the variance-contribution bar on the Diagnostics tab for how the sources combine.",
        shiny::tags$br(), shiny::tags$br(),
        shiny::tags$b("Central dot"),
        " = mean of the annual aggregate across all simulated (model, year) outcomes.",
        shiny::tags$br(),
        shiny::tags$b("Outer grey whisker with caps — “How wide is the full predictive band?”"),
        shiny::tags$br(),
        " Combined “total” uncertainty assuming the three sources are independent: SE = sqrt(var_coef + var_within + var_across). Use this for an at-a-glance overall spread.",
        shiny::tags$br(),
        shiny::tags$b("Thick coloured band — “How much do climate models disagree?”"),
        " (future scenarios only)",
        shiny::tags$br(),
        " Inter-model spread: quantile across CMIP6 ensemble members of each model’s time-mean. This band can be asymmetric around the dot when models lean one way.",
        shiny::tags$br(),
        shiny::tags$b("Middle band — “How much does weather vary year-to-year within a typical model?”"),
        shiny::tags$br(),
        " Inter-annual variability: per-model quantile across simulation years, then averaged across models. Reflects the natural range of outcomes a single climate trajectory produces.",
        shiny::tags$br(),
        shiny::tags$b("Innermost line — “How precisely is each (model, year) aggregate estimated?”"),
        " (shown when coefficient uncertainty is enabled)",
        shiny::tags$br(),
        " Analytic per-outcome SE from the regression fit. This is precision of a point estimate, not a spread of outcomes — conceptually distinct from the two coloured bands.",
        shiny::tags$br(), shiny::tags$br(),
        "Historical = single “model” so no inter-model band is shown."
      )
    ),

    # ---- 4. Exceedance curve -----------------------------------------------
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
        ),
      ),
      shiny::plotOutput(ns("exceedance_plot"), height = "400px"),
      shiny::uiOutput(ns("exceedance_caption"))
    ),

    # ---- 5. Threshold table ------------------------------------------------
    shiny::wellPanel(
      shiny::uiOutput(ns("threshold_table_header")),
      DT::DTOutput(ns("summary_threshold_table")),
      shiny::uiOutput(ns("threshold_table_footer"))
    ),

    # ---- 6. Time-series spaghetti ------------------------------------------
    shiny::wellPanel(
      shiny::h4("Per-model trajectories across simulation years"),
      shiny::plotOutput(ns("timeseries_plot"), height = "380px"),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:6px;",
        shiny::tags$b("Thin coloured lines"),
        " = one CMIP6 ensemble member each (a “spaghetti” trace of model trajectories).",
        shiny::tags$br(),
        shiny::tags$b("Bold line"),
        " = across-model median curve for each scenario.",
        shiny::tags$br(),
        shiny::tags$b("Translucent ribbon"),
        " (future scenarios only) = inter-model spread at the selected band quantiles.",
        shiny::tags$br(),
        "Each scenario × projection period gets its own colour (SSP family) and ",
        "linetype (period), shown as one entry in the legend."
      )
    )
  )
}


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
#' @param tabset_id       Character id of the parent tabset panel.
#' @param tabset_session  Shiny session for the tabset.
#'
#' @noRd
mod_2_02_results_server <- function(id,
                                     hist_sim,
                                     saved_scenarios,
                                     selected_hist,
                                     tabset_id,
                                     tabset_session  = NULL,
                                     sim_n           = reactive(150L),
                                     residuals       = reactive("none"),
                                     skip_coef_draws = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) tabset_session <- session$parent %||% session

    # ---- Lazy delta-method aggregation -------------------------------------
    # Replaces the eager compute_hist_agg / compute_scenario_agg path. Returns
    # the same nested list shape (weighted/unweighted -> method -> tibble) so
    # downstream consumers in fct_sim_compare.R see a compatible schema.
    agg_methods <- reactive({
      req(hist_sim())
      so <- hist_sim()$so
      unname(hist_aggregate_choices(so$type, so$name))
    })

    # pov_line is always supplied (the aggregation pre-computes every method
    # per year, not just the currently selected one). Non-poverty methods
    # ignore it; poverty methods need it. Default 3.00 USD/day if the input
    # hasn't been initialised yet.
    pov_line_val <- debounce(reactive({
      as.numeric(input$pov_line %||% 3.00)
    }), 400)

    bandwidth_p0 <- reactive({
      as.numeric(input$bandwidth_p0 %||% 0.05)
    })

    band_q_active <- reactive({
      resolve_band_q(input$uncertainty_band %||% "p10_p90")
    })

    .one_member_delta <- function(pipe, idx, method, weighted, pov_line,
                                  band_q, is_log) {
      F_idx <- if (!is.null(pipe$F_loading) && !isTRUE(skip_coef_draws()))
                 pipe$F_loading[idx, , drop = FALSE] else NULL
      w_idx <- if (weighted && !is.null(pipe$weight)) pipe$weight[idx] else NULL
      id_idx <- if (!is.null(pipe$id_vec)) pipe$id_vec[idx] else NULL
      # RIF pipelines set train_aug = NULL by construction (fct_simulations.R).
      # If the residuals selector still says "original" or "resample", honour
      # the pipeline by falling back to "none" so draw_residuals_vec doesn't
      # blow up on a missing .resid column.
      res_mode <- residuals() %||% "none"
      if (is.null(pipe$train_aug) && !identical(res_mode, "none"))
        res_mode <- "none"
      aggregate_with_uncertainty_delta(
        y_point      = pipe$y_point[idx],
        F_loading    = F_idx,
        method       = method,
        weights      = w_idx,
        pov_line     = pov_line,
        residuals    = res_mode,
        train_aug    = pipe$train_aug,
        id_vec       = id_idx,
        id_col       = pipe$id_col,
        is_log       = is_log,
        band_q       = band_q,
        bandwidth_p0 = bandwidth_p0()
      )
    }

    # ---- Aggregation workspace + per-method cache --------------------------
    # Captures heavy dependencies (band, poverty line, bandwidth, residuals,
    # skip_coef_draws) into a workspace that's recreated whenever any of them
    # changes. The workspace carries a mutable cache so we only compute each
    # aggregation method once per workspace version. The Results tab reads
    # only the currently selected method, and an eager observer pre-computes
    # the default ("mean") as soon as hist_sim() arrives — so the first render
    # is fast even before the user clicks anything.
    agg_workspace <- reactive({
      req(hist_sim())
      list(
        hs       = hist_sim(),
        sc       = saved_scenarios(),
        bq       = band_q_active(),
        pl_v     = pov_line_val(),
        bw       = bandwidth_p0(),
        res      = residuals() %||% "none",
        skip     = isTRUE(skip_coef_draws()),
        cache    = new.env(parent = emptyenv())
      )
    })

    .build_hist_for_method <- function(ws, method) {
      pl   <- ws$hs$pipeline
      yrs  <- sort(unique(pl$sim_year))
      bq   <- ws$bq
      pl_v <- ws$pl_v
      is_log <- isTRUE(ws$hs$so$transform == "log")
      build_for <- function(weighted) {
        rows <- lapply(yrs, function(yr) {
          idx <- pl$sim_year == yr
          m   <- .one_member_delta(pl, idx, method, weighted, pl_v, bq, is_log)
          sd_yr <- sqrt((m$var_coef %||% 0) + (m$var_resid %||% 0))
          tibble::tibble(
            sim_year     = yr,
            value        = m$value,
            model_id     = list("Historical"),
            value_all    = list(m$value),
            value_all_sd = list(sd_yr),
            var_within   = sd_yr^2,
            var_across   = 0,
            agg_method   = method,
            weighted     = weighted,
            scenario     = "Historical"
          )
        })
        out <- dplyr::bind_rows(rows)
        setNames(list(out), method)
      }
      has_w <- !is.null(pl$weight)
      list(
        unweighted = build_for(FALSE),
        weighted   = if (has_w) build_for(TRUE) else build_for(FALSE)
      )
    }

    .build_scn_for_method <- function(ws, method) {
      sc <- ws$sc
      if (length(sc) == 0L) return(NULL)
      bq   <- ws$bq
      pl_v <- ws$pl_v
      setNames(lapply(sc, function(s) {
        pipes  <- s$pipelines
        is_log <- isTRUE(s$so$transform == "log")
        yrs    <- sort(unique(pipes[[1L]]$sim_year))
        has_w  <- !is.null(pipes[[1L]]$weight)
        build_for <- function(weighted) {
          rows <- lapply(yrs, function(yr) {
            mod_ids <- names(pipes) %||% paste0("m", seq_along(pipes))
            per_member_named <- lapply(seq_along(pipes), function(i) {
              idx <- pipes[[i]]$sim_year == yr
              m   <- .one_member_delta(pipes[[i]], idx, method, weighted,
                                       pl_v, bq, is_log)
              if (is.null(m)) return(NULL)
              list(id = mod_ids[[i]], m = m)
            })
            per_member_named <- Filter(Negate(is.null), per_member_named)
            if (length(per_member_named) == 0L) return(NULL)
            comb <- combine_ensemble_results(
              lapply(per_member_named, `[[`, "m"), band_q = bq)
            if (is.null(comb)) return(NULL)
            vals_m <- vapply(per_member_named,
                             function(x) x$m$value, numeric(1L))
            sd_m   <- sqrt(pmax(vapply(per_member_named,
                                       function(x) (x$m$var_coef  %||% 0)
                                                 + (x$m$var_resid %||% 0),
                                       numeric(1L)), 0))
            ids_m  <- vapply(per_member_named,
                             function(x) x$id, character(1L))
            tibble::tibble(
              sim_year     = yr,
              value        = mean(vals_m, na.rm = TRUE),
              model_id     = list(ids_m),
              value_all    = list(vals_m),
              value_all_sd = list(sd_m),
              var_within   = comb$var_within %||% mean(sd_m^2, na.rm = TRUE),
              var_across   = comb$var_across %||%
                               (if (length(vals_m) > 1L)
                                  stats::var(vals_m, na.rm = TRUE) else 0),
              agg_method   = method,
              weighted     = weighted
            )
          })
          out <- dplyr::bind_rows(Filter(Negate(is.null), rows))
          setNames(list(out), method)
        }
        list(
          unweighted = build_for(FALSE),
          weighted   = if (has_w) build_for(TRUE) else build_for(FALSE)
        )
      }), names(sc))
    }

    .get_hist_agg <- function(method) {
      ws <- agg_workspace()
      key <- paste0("h_", method)
      if (!exists(key, envir = ws$cache, inherits = FALSE)) {
        assign(key, .build_hist_for_method(ws, method), envir = ws$cache)
      }
      get(key, envir = ws$cache, inherits = FALSE)
    }

    .get_scn_agg <- function(method) {
      ws <- agg_workspace()
      key <- paste0("s_", method)
      if (!exists(key, envir = ws$cache, inherits = FALSE)) {
        assign(key, .build_scn_for_method(ws, method), envir = ws$cache)
      }
      get(key, envir = ws$cache, inherits = FALSE)
    }

    # Eagerly pre-compute the default ("mean") aggregation as soon as the
    # simulation finishes, so the Results tab renders immediately when the
    # user opens it. Subsequent method changes are computed on-demand and
    # cached within the current workspace.
    observeEvent(agg_workspace(), {
      req(agg_workspace())
      isolate({
        .get_hist_agg("mean")
        if (length(agg_workspace()$sc) > 0L) .get_scn_agg("mean")
      })
    }, priority = 100, ignoreInit = FALSE)

    hist_agg_rv <- reactive({
      method <- input$cmp_agg_method %||% "mean"
      .get_hist_agg(method)
    })

    scenario_agg_rv <- reactive({
      req(saved_scenarios())
      if (length(saved_scenarios()) == 0L) return(NULL)
      method <- input$cmp_agg_method %||% "mean"
      .get_scn_agg(method)
    })

    # ---- Reactive computations (carried over from mod_2_06) ----------------

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
      # Return model counts per scenario for display
      vapply(sc, function(s) s$n_models %||% 1L, integer(1))
    })



  output$coef_uncertainty_status_ui <- shiny::renderUI({
      req(hist_sim())
      if (!has_draws()) {
        shiny::tags$p(
          style = "font-size:11px; color:#c62828; margin:2px 0 6px 0;",
          "\U0001f534 Coefficient draws skipped at simulation time"
        )
      } else if (!isTRUE(input$show_coef_uncertainty)) {
        shiny::tags$p(
          style = "font-size:11px; color:#e65100; margin:2px 0 6px 0;",
          "\u26a0 Coefficient uncertainty available but not shown"
        )
      } else {
        shiny::tags$p(
          style = "font-size:11px; color:#2e7d32; margin:2px 0 6px 0;",
          "\u2705 Coefficient uncertainty shown"
        )
      }
    })
    outputOptions(output, "coef_uncertainty_status_ui",
                  suspendWhenHidden = TRUE)

    # Always use survey weights when available (UI toggle removed — weighting
    # is the correct default for survey-based welfare estimates).
    weight_key <- reactive({
      if (!is.null(hist_sim()) && isTRUE(hist_sim()$has_weights))
        "weighted" else "unweighted"
    })

    # Shared deviation reference — used by all_series_tbl and exceedance_ribbon
        hist_ref_val <- reactive({
          req(hist_agg_rv())
          method    <- input$cmp_agg_method %||% "mean"
          wk        <- weight_key()
          deviation <- input$cmp_deviation %||% "none"
          if (identical(deviation, "none")) return(0)
          raw_vals <- hist_agg_rv()[[wk]][[method]]$value
          if (identical(deviation, "mean"))
            mean(raw_vals, na.rm = TRUE)
          else
            stats::median(raw_vals, na.rm = TRUE)
        })


        # ---- Coefficient draws availability -----------------------------------
    has_draws <- reactive({
      req(hist_sim())
      !is.null(hist_sim()$chol_obj)
    })

    # Auto-uncheck coefficient uncertainty when no draws available
    observeEvent(hist_sim(), {
      req(hist_sim())
      if (!has_draws()) {
        shiny::updateCheckboxInput(
          session,
          "show_coef_uncertainty",
          value = FALSE
        )
      }
    }, ignoreInit = TRUE)





    selected_scenario_names <- reactive({
      sc   <- saved_scenarios()
      if (length(sc) == 0L) return(character(0))
      keys <- names(sc)

      # Read each grid checkbox
      selected <- Filter(Negate(is.null), lapply(keys, function(key) {
        cb_id <- paste0("sc_", gsub("[^a-zA-Z0-9]", "_", key))
        val   <- input[[cb_id]]
        if (isTRUE(val)) key else NULL
      }))

      # Enforce minimum 1 selected
      if (length(selected) == 0L) keys[1L] else unlist(selected)
    })

    agg_hist <- reactive({
      req(hist_agg_rv())
      method    <- input$cmp_agg_method %||% "mean"
      deviation <- input$cmp_deviation  %||% "none"
      out       <- hist_agg_rv()[[weight_key()]][[method]]
      req(!is.null(out))
      hist_ref  <- hist_ref_val()
      if (!identical(deviation, "none") && nrow(out) > 0)
        out <- dplyr::mutate(out, value = value - hist_ref)
      x_label <- if (identical(deviation, "none")) label_agg_method(method) else
        paste0(label_agg_method(method), " \u2014 ", label_deviation(deviation))
      list(out = out, x_label = x_label)
    })

    agg_scenarios <- reactive({
      req(scenario_agg_rv())
      sc <- saved_scenarios()
      if (length(sc) == 0) return(list())
      method    <- input$cmp_agg_method %||% "mean"
      deviation <- input$cmp_deviation  %||% "none"
      hist_ref <- hist_ref_val() 
      x_label <- if (identical(deviation, "none")) label_agg_method(method) else
        paste0(label_agg_method(method), " \u2014 ", label_deviation(deviation))
        selected <- selected_scenario_names()
        result <- setNames(lapply(names(sc), function(display_key) {
          if (!display_key %in% selected) return(NULL)
          out <- scenario_agg_rv()[[display_key]][[weight_key()]][[method]]
          if (is.null(out) || nrow(out) == 0L) return(NULL)
          if (!identical(deviation, "none"))
            out <- dplyr::mutate(out, value = value - hist_ref)
          list(out = out, x_label = x_label)
        }), names(sc))
      Filter(function(x) !is.null(x) && !is.null(x$out) && nrow(x$out) > 0, result)
    })

    # `exceedance_ribbon` removed — the ribbon is now built inside
    # enhance_exceedance() directly from each series' (value_all, value_all_sd)
    # using analytic delta-method bands, so there is nothing to precompute here.
    

    # `all_series` is now a thin passthrough: it gathers the deviation-shifted
    # tibbles from agg_hist()/agg_scenarios() and tags each with its scenario
    # name. No analytic band augmentation — each plot/table reactive below
    # constructs its own bands from value_all + value_all_sd directly.
    all_series <- reactive({
      req(agg_hist())
      hist_list <- list(Historical = list(
        out      = dplyr::mutate(agg_hist()$out, scenario = "Historical"),
        x_label  = agg_hist()$x_label
      ))
      sc <- agg_scenarios()
      if (length(sc) == 0L) return(hist_list)
      sc_list <- setNames(lapply(names(sc), function(dk) {
        out <- sc[[dk]]$out
        if (is.null(out) || nrow(out) == 0L) return(NULL)
        list(out = dplyr::mutate(out, scenario = dk),
             x_label = sc[[dk]]$x_label)
      }), names(sc))
      c(hist_list, Filter(Negate(is.null), sc_list))
    })




    table_subtitle <- reactive({
      req(agg_hist(), input$cmp_agg_method, input$cmp_deviation)
      paste0(
        agg_hist()$x_label, " \u2014 ",
        label_agg_method(input$cmp_agg_method), " | ",
        label_deviation(input$cmp_deviation)
      )
    })

    # ---- renderUI / render* outputs ----------------------------------------

    output$results_header_ui <- renderUI({
      req(hist_sim(), input$cmp_agg_method, input$cmp_deviation)
      so <- hist_sim()$so

      agg_label    <- label_agg_method(input$cmp_agg_method)
      dev_label    <- label_deviation(input$cmp_deviation)
      pov_txt      <- if (!is.null(pov_line_val()))
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

    output$scenario_filter_ui <- renderUI({
      sc <- saved_scenarios()
      if (length(sc) == 0L)
        return(shiny::helpText("Run a simulation."))

      # Parse scenario keys into SSP × period grid
      keys  <- names(sc)
      ssps  <- sort(unique(sub(" / .*$", "", keys)))
      yrs   <- sort(unique(sub("^.* / ", "", keys)))

      # Build header row
      header <- shiny::tags$tr(
        shiny::tags$th(""),
        lapply(ssps, function(s)
          shiny::tags$th(s,
            style = "text-align:center; font-size:11px;
                    font-weight:600; padding:2px 8px;"))
      )

      # Build one row per period
      period_rows <- lapply(yrs, function(yr) {
        shiny::tags$tr(
          shiny::tags$td(yr,
            style = "font-size:11px; font-weight:600;
                    padding:2px 8px; white-space:nowrap;"),
          lapply(ssps, function(s) {
            key     <- paste0(s, " / ", yr)
            exists  <- key %in% keys
            cb_id   <- ns(paste0("sc_", gsub("[^a-zA-Z0-9]", "_", key)))
            shiny::tags$td(
              style = "text-align:center; padding:2px 4px;",
              if (exists)
                shiny::checkboxInput(
                  cb_id,
                  label = NULL,
                  value = TRUE
                )
              else
                shiny::tags$span(
                  style = "color:#ccc; font-size:11px;",
                  "—"
                )
            )
          })
        )
      })

      shiny::tags$table(
        id    = "scenario-filter-grid",
        style = "border-collapse:collapse; margin-top:4px;",
        shiny::tags$style(shiny::HTML("
          #scenario-filter-grid .checkbox { margin: 0; padding: 0; }
          #scenario-filter-grid .checkbox label { 
            padding-left: 0; 
            min-height: 0;
          }
          #scenario-filter-grid .checkbox label span { display: none; }
          #scenario-filter-grid input[type='checkbox'] { 
            width: 16px; height: 16px; 
            margin: 0 auto; 
            display: block;
            position: static;
          }
          #scenario-filter-grid td { padding: 4px 12px; }
          #scenario-filter-grid th { padding: 4px 12px; font-size: 11px; }
        ")),
        shiny::tags$thead(header),
        shiny::tags$tbody(period_rows)
      )
    })

    # ---- Three-source uncertainty decomposition ----------------------------
    # All three downstream displays (hero, exceedance, table) source their
    # bands from the helpers below. Each helper produces a per-scenario view
    # that decomposes uncertainty into:
    #   - coefficient (per-outcome SE from value_all_sd)
    #   - inter-annual (within-model spread of value_all across years)
    #   - inter-model  (across-model spread of model means; future only)

    # Helper: take a long tibble (one row per sim_year) with list-cols
    # value_all/value_all_sd/model_id and return a nested matrix-like
    # structure: model_ids × n_years matrix of values, parallel SDs.
    .by_model_matrix <- function(tbl) {
      if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
      all_ids <- unique(unlist(tbl$model_id, use.names = FALSE))
      n_yrs   <- nrow(tbl)
      vals_mat <- matrix(NA_real_, nrow = length(all_ids), ncol = n_yrs,
                         dimnames = list(all_ids, tbl$sim_year))
      sds_mat  <- matrix(NA_real_, nrow = length(all_ids), ncol = n_yrs,
                         dimnames = list(all_ids, tbl$sim_year))
      for (k in seq_len(n_yrs)) {
        ids  <- as.character(tbl$model_id[[k]])
        vals <- tbl$value_all[[k]]
        sds  <- tbl$value_all_sd[[k]]
        # Recycle a scalar SD across all members (1-member historical)
        if (length(sds) == 1L && length(vals) > 1L) sds <- rep(sds, length(vals))
        vals_mat[ids, k] <- vals
        sds_mat[ids,  k] <- sds
      }
      list(vals = vals_mat, sds = sds_mat, model_ids = all_ids,
           sim_years = tbl$sim_year)
    }

    # Format a quantile probability as "P05", "P50", "P95", "min", "max" for
    # use in threshold-table row labels.
    .pct_label <- function(q, use_minmax = FALSE) {
      if (isTRUE(use_minmax) && q <= 0.001) return("min")
      if (isTRUE(use_minmax) && q >= 0.999) return("max")
      paste0("P", formatC(round(q * 100), width = 2, flag = "0"))
    }

    # Rank-position interpolation used by both the exceedance plot and the
    # threshold table so the table cells always match the curve.
    .rank_interp <- function(sorted_vals, p) {
      n  <- length(sorted_vals)
      if (n == 0L) return(NA_real_)
      k  <- n * (1 - p) + 0.5
      # No clamping: if the requested RP falls outside the observed empirical
      # range (k < 1 or k > n), return NA rather than extrapolate.
      if (k < 1 || k > n) return(NA_real_)
      lo <- floor(k); hi <- ceiling(k)
      if (lo == hi) sorted_vals[lo]
      else sorted_vals[lo] + (k - lo) * (sorted_vals[hi] - sorted_vals[lo])
    }

    # ---- pointrange_bands_rv: one row per scenario, three nested bands -----
    pointrange_bands_rv <- reactive({
      req(hist_agg_rv())
      bq_coef <- resolve_band_q(input$uncertainty_band %||% "p10_p90")
      bq_ens  <- resolve_band_q(input$ensemble_band    %||% "minmax")
      z_coef_lo <- stats::qnorm(bq_coef[["lo"]])
      z_coef_hi <- stats::qnorm(bq_coef[["hi"]])
      hist_ref  <- hist_ref_val()
      wk        <- weight_key()
      method    <- input$cmp_agg_method %||% "mean"

      one_scenario <- function(tbl, scenario_label, is_hist) {
        if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
        mm <- .by_model_matrix(tbl)
        if (is.null(mm)) return(NULL)
        vals <- mm$vals; sds <- mm$sds

        # Inter-model spread: per-model mean across years, then quantile across models.
        model_means <- rowMeans(vals, na.rm = TRUE)
        intermod <- if (is_hist || length(model_means) <= 1L) {
          mean_v <- mean(model_means, na.rm = TRUE)
          c(lo = mean_v, hi = mean_v)
        } else {
          c(lo = unname(stats::quantile(model_means, bq_ens[["lo"]], na.rm = TRUE)),
            hi = unname(stats::quantile(model_means, bq_ens[["hi"]], na.rm = TRUE)))
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
                              probs = bq_ens[["lo"]], na.rm = TRUE)
          per_mod_hi <- apply(vals, 1L, stats::quantile,
                              probs = bq_ens[["hi"]], na.rm = TRUE)
          interann <- c(lo = mean(per_mod_lo, na.rm = TRUE),
                        hi = mean(per_mod_hi, na.rm = TRUE))
        }

        # Coefficient uncertainty: per-outcome SE, centred on ensemble mean.
        ens_mean <- mean(as.numeric(vals), na.rm = TRUE)
        sd_mean  <- mean(as.numeric(sds),  na.rm = TRUE)
        coef     <- c(lo = ens_mean + z_coef_lo * sd_mean,
                      hi = ens_mean + z_coef_hi * sd_mean)

        # "Total" band: combine all three sources assuming independence.
        # var_coef = mean per-outcome variance from regression fit.
        # var_within = mean within-model annual variance (from tbl$var_within).
        # var_across = mean across-model snapshot variance (from tbl$var_across).
        var_coef_total <- mean(as.numeric(sds)^2, na.rm = TRUE)
        var_within     <- mean(tbl$var_within, na.rm = TRUE)
        var_across     <- if (is_hist) 0 else mean(tbl$var_across, na.rm = TRUE)
        sd_total <- sqrt(max(var_coef_total + var_within + var_across, 0,
                             na.rm = TRUE))
        total <- c(lo = ens_mean + z_coef_lo * sd_total,
                   hi = ens_mean + z_coef_hi * sd_total)

        tibble::tibble(
          scenario     = scenario_label,
          value        = ens_mean - hist_ref,
          coef_lo      = unname(coef[["lo"]])       - hist_ref,
          coef_hi      = unname(coef[["hi"]])       - hist_ref,
          interann_lo  = unname(interann[["lo"]])   - hist_ref,
          interann_hi  = unname(interann[["hi"]])   - hist_ref,
          intermod_lo  = unname(intermod[["lo"]])   - hist_ref,
          intermod_hi  = unname(intermod[["hi"]])   - hist_ref,
          total_lo     = unname(total[["lo"]])      - hist_ref,
          total_hi     = unname(total[["hi"]])      - hist_ref,
          is_historical = is_hist,
          n_models     = length(mm$model_ids)
        )
      }

      rows <- list(one_scenario(hist_agg_rv()[[wk]][[method]],
                                "Historical", TRUE))
      sa <- scenario_agg_rv()
      if (!is.null(sa) && length(sa) > 0L) {
        for (dk in names(sa)) {
          rows[[length(rows) + 1L]] <- one_scenario(sa[[dk]][[wk]][[method]],
                                                    dk, FALSE)
        }
      }
      dplyr::bind_rows(Filter(Negate(is.null), rows))
    })

    # ---- timeseries_curves_rv: per (scenario, model, sim_year) values ------
    timeseries_curves_rv <- reactive({
      req(hist_agg_rv())
      hist_ref <- hist_ref_val()
      wk       <- weight_key()
      method   <- input$cmp_agg_method %||% "mean"

      one_scenario <- function(tbl, scenario_label, is_hist) {
        if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
        mm <- .by_model_matrix(tbl)
        if (is.null(mm)) return(NULL)
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

      rows <- list(one_scenario(hist_agg_rv()[[wk]][[method]],
                                "Historical", TRUE))
      sa <- scenario_agg_rv()
      if (!is.null(sa) && length(sa) > 0L) {
        for (dk in names(sa)) {
          if (!dk %in% selected_scenario_names()) next
          rows[[length(rows) + 1L]] <- one_scenario(sa[[dk]][[wk]][[method]],
                                                    dk, FALSE)
        }
      }
      dplyr::bind_rows(Filter(Negate(is.null), rows))
    })

    # ---- variance_breakdown_rv: one row per scenario, three components -----
    # Aggregates the per-(sim_year) var_within / var_across columns to scalars
    # and re-computes var_coef from the per-(model, year) SD list-column.
    variance_breakdown_rv <- reactive({
      req(hist_agg_rv())
      wk     <- weight_key()
      method <- input$cmp_agg_method %||% "mean"

      one_scenario <- function(tbl, scenario_label, is_hist) {
        if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
        sds_flat <- as.numeric(unlist(tbl$value_all_sd))
        var_coef <- if (length(sds_flat))
          mean(sds_flat^2, na.rm = TRUE) else 0
        tibble::tibble(
          scenario      = scenario_label,
          var_coef      = var_coef,
          var_within    = mean(tbl$var_within, na.rm = TRUE),
          var_across    = if (is_hist) 0 else mean(tbl$var_across, na.rm = TRUE),
          is_historical = is_hist
        )
      }

      rows <- list(one_scenario(hist_agg_rv()[[wk]][[method]],
                                "Historical", TRUE))
      sa <- scenario_agg_rv()
      if (!is.null(sa) && length(sa) > 0L) {
        for (dk in names(sa)) {
          if (!dk %in% selected_scenario_names()) next
          rows[[length(rows) + 1L]] <- one_scenario(sa[[dk]][[wk]][[method]],
                                                    dk, FALSE)
        }
      }
      dplyr::bind_rows(Filter(Negate(is.null), rows))
    })

    # ---- exceedance_curves_rv: per (scenario, model) ECDF rows -------------
    # One row per (scenario, model, rank). welfare_val is sorted ascending per
    # model; exceed_prob is rev((seq - 0.5)/n_years). coef_sd is the per-
    # (model, year) SD reordered to match the welfare sort.
    exceedance_curves_rv <- reactive({
      req(hist_agg_rv())
      hist_ref <- hist_ref_val()
      wk       <- weight_key()
      method   <- input$cmp_agg_method %||% "mean"

      one_scenario <- function(tbl, scenario_label, is_hist) {
        if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
        mm <- .by_model_matrix(tbl)
        if (is.null(mm)) return(NULL)
        vals <- mm$vals; sds <- mm$sds
        n_yrs <- ncol(vals)
        if (n_yrs == 0L) return(NULL)
        probs <- rev((seq_len(n_yrs) - 0.5) / n_yrs)

        do.call(dplyr::bind_rows, lapply(seq_len(nrow(vals)), function(i) {
          v <- vals[i, ]; s <- sds[i, ]
          ok <- is.finite(v)
          if (!any(ok)) return(NULL)
          v <- v[ok]; s <- s[ok]
          ord <- order(v)
          tibble::tibble(
            scenario    = scenario_label,
            model_id    = mm$model_ids[[i]],
            rank        = seq_along(ord),
            welfare_val = v[ord] - hist_ref,
            coef_sd     = if (length(s) == length(ord)) s[ord] else rep(0, length(ord)),
            exceed_prob = rev((seq_len(length(ord)) - 0.5) / length(ord)),
            is_historical = is_hist
          )
        }))
      }

      rows <- list(one_scenario(hist_agg_rv()[[wk]][[method]],
                                "Historical", TRUE))
      sa <- scenario_agg_rv()
      if (!is.null(sa) && length(sa) > 0L) {
        for (dk in names(sa)) {
          if (!dk %in% selected_scenario_names()) next
          rows[[length(rows) + 1L]] <- one_scenario(sa[[dk]][[wk]][[method]],
                                                    dk, FALSE)
        }
      }
      dplyr::bind_rows(Filter(Negate(is.null), rows))
    })

    # ---- threshold_table_rv: long-format rows ready to pivot wide ---------
    # One row per (scenario, Estimate, RP). Estimate names are derived from
    # the user's band quantile selection: e.g., with coef=p10_p90 and
    # ensemble=minmax the rows are "Central (P50)", "Coef P10", "Coef P90",
    # "Ensemble min", "Ensemble max", "Total P10", "Total P90". Total rows
    # combine the coefficient and inter-model components assuming
    # independence: SE_total = sqrt(coef_sd² + var_across_at_rp).
    threshold_table_rv <- reactive({
      req(hist_agg_rv())
      bq_coef <- resolve_band_q(input$uncertainty_band %||% "p10_p90")
      bq_ens  <- resolve_band_q(input$ensemble_band    %||% "minmax")
      z_coef_lo <- stats::qnorm(bq_coef[["lo"]])
      z_coef_hi <- stats::qnorm(bq_coef[["hi"]])
      hist_ref  <- hist_ref_val()
      wk        <- weight_key()
      method    <- input$cmp_agg_method %||% "mean"

      RPs <- c(RP_LOW, c("1:1" = 0.5), RP_HIGH)

      one_scenario <- function(tbl, scenario_label, is_hist) {
        if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
        mm <- .by_model_matrix(tbl)
        if (is.null(mm)) return(NULL)
        vals <- mm$vals; sds <- mm$sds
        n_yrs <- ncol(vals)
        n_pts <- if (is_hist) sum(is.finite(as.numeric(vals))) else n_yrs

        # Drop RPs that aren't comfortably supported by n_yrs of data. A 1-in-N
        # return period needs at least N observations (p in [1/n, 1-1/n]); we
        # don't report tighter probabilities — they'd rest on the single most
        # extreme observed year and are not meaningful as a "1-in-N" estimate.
        rp_ok    <- RPs >= (1 / n_yrs) & RPs <= (1 - 1 / n_yrs)
        RPs_keep <- RPs[rp_ok]
        if (length(RPs_keep) == 0L) return(NULL)

        # Per-model rank-interp at each kept RP (matrix: model × RP)
        per_model_rp <- t(apply(vals, 1L, function(v) {
          v <- v[is.finite(v)]
          if (length(v) < 2L) return(rep(NA_real_, length(RPs_keep)))
          sv <- sort(v)
          vapply(RPs_keep, function(p) .rank_interp(sv, p), numeric(1L))
        }))
        # Per-model SD at each kept RP via the same rank-position math, so the
        # coefficient band on the table matches the exceedance plot.
        per_model_sd_at_rp <- t(vapply(seq_len(nrow(vals)), function(i) {
          v <- vals[i, ]; s <- sds[i, ]
          ok <- is.finite(v)
          if (sum(ok) < 2L) return(rep(NA_real_, length(RPs_keep)))
          ord <- order(v[ok])
          s_sorted <- s[ok][ord]
          vapply(RPs_keep, function(p) .rank_interp(s_sorted, p), numeric(1L))
        }, numeric(length(RPs_keep))))

        # Aggregate across models for each RP
        central_vec <- if (is_hist) per_model_rp[1L, ] else
          apply(per_model_rp, 2L, stats::median, na.rm = TRUE)
        coef_sd_vec <- if (is_hist) per_model_sd_at_rp[1L, ] else
          apply(per_model_sd_at_rp, 2L, stats::median, na.rm = TRUE)
        coef_lo_vec <- central_vec + z_coef_lo * coef_sd_vec
        coef_hi_vec <- central_vec + z_coef_hi * coef_sd_vec

        intermod_lo_vec <- if (is_hist) rep(NA_real_, length(RPs_keep)) else
          apply(per_model_rp, 2L, stats::quantile,
                probs = bq_ens[["lo"]], na.rm = TRUE)
        intermod_hi_vec <- if (is_hist) rep(NA_real_, length(RPs_keep)) else
          apply(per_model_rp, 2L, stats::quantile,
                probs = bq_ens[["hi"]], na.rm = TRUE)

        # Total band combines coefficient and inter-model variance at each RP,
        # assuming independence. Inter-annual variability is already baked
        # into the per-rank value so it isn't added a second time here.
        var_across_at_rp <- if (is_hist) rep(0, length(RPs_keep)) else
          apply(per_model_rp, 2L, stats::var, na.rm = TRUE)
        var_across_at_rp[is.na(var_across_at_rp)] <- 0
        sd_total_vec <- sqrt(pmax(coef_sd_vec^2 + var_across_at_rp, 0,
                                  na.rm = FALSE))
        total_lo_vec <- central_vec + z_coef_lo * sd_total_vec
        total_hi_vec <- central_vec + z_coef_hi * sd_total_vec

        make_row <- function(estimate, vec) {
          tibble::tibble(
            scenario   = scenario_label,
            Estimate   = estimate,
            rp_name    = names(RPs_keep),
            rp_label   = names(RPs_keep),
            value      = vec - hist_ref,
            n_obs      = n_pts,
            is_historical = is_hist
          )
        }
        coef_lo_lbl <- paste0("Coef ",  .pct_label(bq_coef[["lo"]]))
        coef_hi_lbl <- paste0("Coef ",  .pct_label(bq_coef[["hi"]]))
        ens_lo_lbl  <- paste0("Ensemble ", .pct_label(bq_ens[["lo"]],
                                                       use_minmax = TRUE))
        ens_hi_lbl  <- paste0("Ensemble ", .pct_label(bq_ens[["hi"]],
                                                       use_minmax = TRUE))
        total_lo_lbl <- paste0("Total ", .pct_label(bq_coef[["lo"]]))
        total_hi_lbl <- paste0("Total ", .pct_label(bq_coef[["hi"]]))

        rows <- list(
          make_row("Central (P50)", central_vec),
          make_row(coef_lo_lbl,     coef_lo_vec),
          make_row(coef_hi_lbl,     coef_hi_vec)
        )
        if (!is_hist) {
          rows <- c(rows, list(
            make_row(ens_lo_lbl,   intermod_lo_vec),
            make_row(ens_hi_lbl,   intermod_hi_vec),
            make_row(total_lo_lbl, total_lo_vec),
            make_row(total_hi_lbl, total_hi_vec)
          ))
        } else {
          # Historical: Total == Coef (no inter-model component)
          rows <- c(rows, list(
            make_row(total_lo_lbl, coef_lo_vec),
            make_row(total_hi_lbl, coef_hi_vec)
          ))
        }
        dplyr::bind_rows(rows)
      }

      rows <- list(one_scenario(hist_agg_rv()[[wk]][[method]],
                                "Historical", TRUE))
      sa <- scenario_agg_rv()
      if (!is.null(sa) && length(sa) > 0L) {
        for (dk in names(sa)) {
          if (!dk %in% selected_scenario_names()) next
          rows[[length(rows) + 1L]] <- one_scenario(sa[[dk]][[wk]][[method]],
                                                    dk, FALSE)
        }
      }
      dplyr::bind_rows(Filter(Negate(is.null), rows))
    })

    output$summary_box_plot <- renderPlot({
      req(pointrange_bands_rv())
      bands <- pointrange_bands_rv()
      if (!isTRUE(input$show_model_spread)) {
        bands$intermod_lo <- NA_real_
        bands$intermod_hi <- NA_real_
      }
      plot_pointrange_climate(
        bands_tbl    = bands,
        x_label      = agg_hist()$x_label,
        group_order  = input$cmp_group_order %||% "scenario_x_year",
        show_coef    = isTRUE(input$show_coef_uncertainty) && has_draws()
      )
    }, height = 600)

    output$timeseries_plot <- renderPlot({
      req(timeseries_curves_rv())
      # When inter-model spread is toggled off, collapse the ribbon by passing
      # a degenerate band (lo = hi = 0.5).
      ens_q <- if (isTRUE(input$show_model_spread))
        resolve_band_q(input$ensemble_band %||% "minmax")
      else c(lo = 0.5, hi = 0.5)
      plot_timeseries_spaghetti(
        ts_tbl          = timeseries_curves_rv(),
        x_label         = agg_hist()$x_label,
        ensemble_band_q = ens_q
      )
    })

    output$summary_threshold_table <- DT::renderDT({
      req(threshold_table_rv())
      tbl <- threshold_table_rv()
      if (!isTRUE(input$show_model_spread)) {
        tbl <- tbl[!grepl("^Ensemble |^Total ", tbl$Estimate), , drop = FALSE]
      }
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
          pageLength = 15, dom = "tip", ordering = list(list(2, "desc")),
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        extensions = "Buttons"
      )
    })

    output$threshold_table_header <- renderUI({
      req(agg_hist())
      tagList(
        shiny::h4("Outcome value at return-period thresholds (both tails)"),
        shiny::tags$small(class = "text-muted", table_subtitle())
      )
    })

    output$threshold_table_footer <- renderUI({
      req(agg_hist())
      tagList(
        shiny::tags$p(style = "font-size:11px; color:#666; margin-top:6px; margin-bottom:2px;",
                      shiny::tags$b("Central (P50)"),
                      " = across-model median of each model's return-period value (or the single historical curve)."),
        shiny::tags$p(style = "font-size:11px; color:#666; margin-top:0; margin-bottom:2px;",
                      shiny::tags$b("Coef Pxx"),
                      " = analytic per-outcome SE band around the central value (coefficient + residual uncertainty). Percentiles follow the “Coefficient uncertainty band” selector."),
        shiny::tags$p(style = "font-size:11px; color:#666; margin-top:0; margin-bottom:2px;",
                      shiny::tags$b("Ensemble Pxx / min / max"),
                      " (future only) = quantile of per-model return-period values across CMIP6 ensemble members. Percentiles follow the “Weather + model spread band” selector."),
        shiny::tags$p(style = "font-size:11px; color:#666; margin-top:0; margin-bottom:2px;",
                      shiny::tags$b("Total Pxx"),
                      " = combined band assuming independence: SE_total = sqrt(coef_SE² + var_across_models). Historical: Total ≡ Coef (no inter-model component)."),
        shiny::tags$p(style = "font-size:11px; color:#666; margin-top:0; margin-bottom:2px;",
                      "Low odds show the value exceeded in only 1-in-N years; high odds = value reached in all but 1-in-N years; 1:1 is the median year."),
        shiny::tags$p(style = "font-size:11px; color:#666; margin-top:0;",
                      "Obs = number of simulated years feeding each per-model exceedance curve. Return periods that fall outside the empirical range supported by Obs (probability < 0.5/Obs or > 1 − 0.5/Obs) are not reported rather than extrapolated.")
      )
    })

    output$exceedance_plot <- renderPlot({
      req(exceedance_curves_rv())
      ens_q <- if (isTRUE(input$show_model_spread))
        resolve_band_q(input$ensemble_band %||% "minmax")
      else c(lo = 0.5, hi = 0.5)
      enhance_exceedance(
        curves_tbl      = exceedance_curves_rv(),
        x_label         = agg_hist()$x_label,
        return_period   = isTRUE(input$show_return_period),
        n_sim_years     = nrow(agg_hist()$out),
        logit_x         = isTRUE(input$exceedance_logit_x),
        band_q          = if (isTRUE(input$show_coef_uncertainty) && has_draws())
                            resolve_band_q(input$uncertainty_band %||% "p10_p90")
                          else NULL,
        ensemble_band_q = ens_q
      )
    })

    output$exceedance_caption <- renderUI({
      req(agg_hist())
      axis_txt <- if (isTRUE(input$exceedance_logit_x))
        "Probability axis is logit-scaled, giving equal visual weight to both tails."
      else
        "Annual exceedance probability — each curve is computed over the simulation years."
      tagList(
        shiny::tags$p(style = "font-size:11px; color:#666; margin-top:6px; margin-bottom:2px;",
                      axis_txt),
        shiny::tags$p(style = "font-size:11px; color:#666; margin-top:0; margin-bottom:2px;",
                      shiny::tags$b("Central curve"),
                      " = across-model median of each model's empirical exceedance curve."),
        shiny::tags$p(style = "font-size:11px; color:#666; margin-top:0; margin-bottom:2px;",
                      shiny::tags$b("Filled ribbon"),
                      " (future scenarios only) = inter-model spread; quantile across ensemble members at each return period."),
        shiny::tags$p(style = "font-size:11px; color:#666; margin-top:0; margin-bottom:2px;",
                      shiny::tags$b("Dashed outlines"),
                      " (when coefficient uncertainty enabled) = analytic per-outcome SE band around the central curve. The dashed outlines may fall inside or outside the inter-model ribbon depending on which source dominates."),
        shiny::tags$p(style = "font-size:11px; color:#666; margin-top:0;",
                      "Low odds = value exceeded in only 1-in-N years; high odds = value reached in all but 1-in-N years.")
      )
    })

    # ---- observeEvent handlers ---------------------------------------------

    # Insert Results tab + content once (on first hist_sim).
    observeEvent(hist_sim(), {
      req(hist_sim())

      shiny::appendTab(
        inputId = tabset_id,
        shiny::tabPanel(
          title = "Results",
          value = "sim_tab",
          shiny::div(id = "results_section")
        ),
        select  = TRUE,
        session = tabset_session
      )

      shiny::insertUI(
        selector = "#results_section",
        where    = "afterBegin",
        ui       = .results_content_ui(ns, hist_sim()$so)
      )
    }, ignoreInit = TRUE, once = TRUE)

    # On subsequent runs, just re-select the tab.
    observeEvent(hist_sim(), {
      shiny::updateTabsetPanel(
        session  = tabset_session,
        inputId  = tabset_id,
        selected = "sim_tab"
      )
    }, ignoreInit = TRUE)

    # Keep agg method choices in sync with outcome.
    observeEvent(hist_sim(), {
      req(hist_sim()$so)
      so      <- hist_sim()$so
      choices <- hist_aggregate_choices(so$type, so$name)
      current <- isolate(input$cmp_agg_method)
      new_sel <- if (!is.null(current) && current %in% choices) current else "mean"
      shiny::updateSelectInput(session, "cmp_agg_method",
                               choices  = choices,
                               selected = new_sel)
    }, ignoreInit = TRUE)

    # ---- Suspend outputs when Results tab is hidden ----------------------
    outputOptions(output, "summary_box_plot",        suspendWhenHidden = TRUE)
    outputOptions(output, "timeseries_plot",         suspendWhenHidden = TRUE)
    outputOptions(output, "summary_threshold_table", suspendWhenHidden = TRUE)
    outputOptions(output, "exceedance_plot",         suspendWhenHidden = TRUE)
    outputOptions(output, "results_header_ui",       suspendWhenHidden = TRUE)
    outputOptions(output, "scenario_filter_ui",      suspendWhenHidden = TRUE)
    outputOptions(output, "threshold_table_header",  suspendWhenHidden = TRUE)
    outputOptions(output, "threshold_table_footer",  suspendWhenHidden = TRUE)
    outputOptions(output, "exceedance_caption",      suspendWhenHidden = TRUE)
    
    # ---- Return API --------------------------------------------------------
    list(
      variance_breakdown = variance_breakdown_rv
    )
  })
}
