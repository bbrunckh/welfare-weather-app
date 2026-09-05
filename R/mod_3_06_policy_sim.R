#' 3_06_policy_sim UI Function
#'
#' @description A shiny Module. Renders status banner for policy adjustments.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_3_06_policy_sim_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("sim_status_ui"))
  )
}

#' 3_06_policy_sim Server Functions
#'
#' Applies user-defined policy adjustments to survey covariates from the
#' policy scenario modules (mod_3_01 through mod_3_05), then re-runs the
#' Step 2 simulation pipeline against both the baseline and policy-adjusted
#' survey frames using the cached Step 2 weather, model fit and draws.
#'
#' @param id                Module id.
#' @param survey_weather    Reactive survey-weather df to be adjusted.
#' @param sp_scenario       Reactive named list from mod_3_01_sp_server().
#' @param infra_scenario    Reactive named list from mod_3_02_infra_server().
#' @param digital_scenario  Reactive named list from mod_3_03_digital_server().
#' @param labor_scenario    Reactive named list from mod_3_04_labor_server().
#' @param education_scenario Reactive list from mod_3_05_education_server().
#' @param selected_model    Reactive list of the selected Step 1 model's
#'   parameters. Used to restrict covariate levers to variables still in the
#'   model, so dropped variables do not appear as manipulated in diagnostics.
#' @param model_fit         Reactive list from mod_1 model fit.
#' @param selected_weather  Reactive selected-weather metadata.
#' @param hist_sim          Reactive Step 2 hist_sim list.
#' @param saved_scenarios   Reactive Step 2 named scenario list.
#' @param run_trigger       Reactive the parent fires to request a policy run
#'   (REACT-09): the run lifecycle stays inside this module instead of the
#'   parent calling the exported `run()` closure.
#'
#' @return Named list with baseline_svy, policy_svy, sim_run_id, plus
#'   re-simulated baseline_hist_sim/baseline_saved_scenarios and
#'   policy_hist_sim/policy_saved_scenarios.
#'
#' @noRd
mod_3_06_policy_sim_server <- function(id,
                                        survey_weather,
                                        sp_scenario        = reactive(NULL),
                                        infra_scenario     = reactive(NULL),
                                        digital_scenario   = reactive(NULL),
                                        labor_scenario     = reactive(NULL),
                                        education_scenario = reactive(NULL),
                                        selected_model     = reactive(NULL),
                                        model_fit          = reactive(NULL),
                                        selected_weather   = reactive(NULL),
                                        hist_sim           = reactive(NULL),
                                        saved_scenarios    = reactive(list()),
                                        analysis_unit      = reactive("hh"),
                                        skip_coef_draws    = reactive(FALSE),
                                        residuals          = reactive("original"),
                                        propagate_all_covariate_uncertainty =
                                          reactive(FALSE),
                                        survey_version     = reactive(0L),
                                        sim_stale          = reactive(FALSE),
                                        run_trigger        = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    baseline_svy_rv     <- reactiveVal(NULL)
    policy_svy_rv       <- reactiveVal(NULL)
    sim_error           <- reactiveVal(NULL)
    sim_run_id          <- reactiveVal(0L)
    # REACT-02: TRUE while a policy simulation is executing.
    sim_running         <- reactiveVal(FALSE)
    decomp_rv           <- reactiveVal(NULL)
    decomp_scenarios_rv <- reactiveVal(list())
    # INT-08: TRUE while the stored policy results' run signature no longer
    # matches the current Step 2 output / scenario inputs.
    policy_stale        <- reactiveVal(FALSE)

    baseline_hist_sim_rv        <- reactiveVal(NULL)
    baseline_saved_scenarios_rv <- reactiveVal(list())
    policy_hist_sim_rv          <- reactiveVal(NULL)
    policy_saved_scenarios_rv   <- reactiveVal(list())

    output$sim_status_ui <- shiny::renderUI({
      err <- sim_error()
      if (is.null(err)) return(NULL)
      shiny::div(
        class = "alert alert-danger",
        role  = "alert",
        style = "margin-bottom: 10px;",
        shiny::tags$b("Policy simulation failed:"),
        shiny::span(conditionMessage(err))
      )
    })

    # ---- Run signature (INT-08) ----------------------------------------------
    # The policy run inherits Step 2's signature and adds the scenario
    # configuration; a mismatch (or a stale Step 2) marks the results stale.

    .policy_sig_from_live <- function(hs = hist_sim()) {
      list(
        step           = "policy",
        sim_sig        = if (!is.null(hs)) hs$.sig %||% NULL else NULL,
        survey_version = survey_version(),
        scenarios      = .sig_plain(list(
          sp        = sp_scenario(),
          infra     = infra_scenario(),
          digital   = digital_scenario(),
          labor     = labor_scenario(),
          education = education_scenario()
        ))
      )
    }

    # REACT-18: take the *reactive*, not its value. Passing `sp_scenario()`
    # here handed the helper a promise; `observeEvent()` quotes the symbol
    # `observe_what`, so the promise was forced on the observer's first run -
    # registering the dependency once - and every later evaluation returned
    # the cached value without re-registering it. The observer therefore fired
    # exactly once per session and then went deaf, which is why changing a
    # Step 3 lever never marked the policy results stale. Calling `react()`
    # inside the quoted expression re-establishes the dependency on every
    # invalidation.
    .mark_stale_on_change <- function(react) {
      shiny::observeEvent(react(), {
        bh <- baseline_hist_sim_rv()
        if (!is.null(bh) && !identical(.policy_sig_from_live(), bh$.sig))
          policy_stale(TRUE)
      }, ignoreInit = TRUE)
    }
    .mark_stale_on_change(hist_sim)
    .mark_stale_on_change(sp_scenario)
    .mark_stale_on_change(infra_scenario)
    .mark_stale_on_change(digital_scenario)
    .mark_stale_on_change(labor_scenario)
    .mark_stale_on_change(education_scenario)
    .mark_stale_on_change(survey_version)
    # Cascade: when Step 2 is stale (inputs changed, not yet re-run) the
    # policy results built on it are stale too.
    shiny::observeEvent(sim_stale(), {
      if (isTRUE(sim_stale()) && !is.null(baseline_hist_sim_rv()))
        policy_stale(TRUE)
    }, ignoreInit = TRUE)
    shiny::observeEvent(baseline_hist_sim_rv(), policy_stale(FALSE))

    run <- function() {
      # REACT-02: one policy simulation at a time. The guard is owned by the
      # module doing the work; the triggering button (mod_3_scenario) is
      # disabled via the exposed reactive.
      if (isTRUE(sim_running())) return(invisible(NULL))
      sim_running(TRUE)
      on.exit(sim_running(FALSE), add = TRUE)

      sim_error(NULL)

      # REACT-17: these are upstream reactives that req() internally (e.g.
      # selected_weather() on the Step 1 weather selector). An unmet req()
      # used to propagate out of this function as a silent error, so the
      # click produced no progress bar, no notification and no banner - a
      # dead button with nothing to explain it. Read them defensively and
      # turn every missing prerequisite into a message the user can act on.
      .safe <- function(expr) tryCatch(expr, error = function(e) NULL)
      mf  <- .safe(model_fit())
      sw  <- .safe(selected_weather())
      hs  <- .safe(hist_sim())
      ss  <- .safe(saved_scenarios())
      # Use the exact survey that Step 2 used as the baseline. Step 2 may have
      # filtered survey_weather() to a single survey round (baseline_svy). The
      # Step 2 weather_raw was fetched against that filtered survey, so it
      # contains rows for all survey years in selected_surveys - joining the
      # FULL survey_weather() would pull in extra households from non-baseline
      # rounds and produce a systematically different aggregate.
      svy <- hs$svy %||% .safe(survey_weather())

      .fail <- function(msg) {
        sim_error(simpleError(msg))
        shiny::showNotification(msg, type = "error", duration = 8)
        invisible(NULL)
      }

      if (is.null(mf)) {
        return(.fail(paste(
          "No fitted model. Run the Step 1 model before simulating policy",
          "scenarios."
        )))
      }
      if (is.null(hs)) {
        return(.fail(
          "Step 2 simulation must be run before policy simulation."
        ))
      }
      if (is.null(sw)) {
        return(.fail(paste(
          "No weather variables selected. Configure them in Step 1 before",
          "simulating policy scenarios."
        )))
      }
      if (is.null(svy)) {
        return(.fail("Survey data not available."))
      }

      tryCatch(
        {
          # INT-08: the signature is captured up front from the exact inputs
          # this run consumes (scenario reactives are read again below); a
          # signature built at publish time could record mid-run edits.
          policy_sig <- .policy_sig_from_live(hs)

          svy_mod <- apply_policy_to_svy(
            svy,
            infra         = infra_scenario(),
            sp            = sp_scenario(),
            digital       = digital_scenario(),
            labor         = labor_scenario(),
            education     = education_scenario(),
            model_vars    = model_term_names(.safe(selected_model())),
            analysis_unit = analysis_unit(),
            seed          = WISEAPP_DEFAULT_SEED
          )

          # A social-protection-only scenario changes nothing but the cash
          # transfer column, which is deliberately excluded from the covariate
          # deltas - so it is a perfectly valid run and must not be treated as
          # "nothing configured". What is worth flagging is a scenario that is
          # a literal no-op (every lever at its zero default): the run still
          # goes ahead, but the policy arm will equal the baseline.
          if (!.scenario_has_effect(svy, svy_mod)) {
            shiny::showNotification(
              paste(
                "No policy change is configured - every lever is at zero, so",
                "the policy results will match the baseline. Set a transfer",
                "amount or budget under Social protection, or adjust another",
                "lever."
              ),
              type = "warning", duration = 10
            )
          }

          shiny::withProgress(
            message = "Re-running simulations for baseline and policy...",
            value   = 0.1,
            {
              shiny::setProgress(value = 0.2, detail = "Baseline (reusing Step 2)...")
              # Baseline = Step 2 output verbatim. The survey is unchanged in
              # the baseline arm, so re-simulating would just reproduce the
              # Step 2 results. Pass Step 2's hist_sim and saved_scenarios
              # straight through so the Results pane reads exactly the same
              # values Mod 2 shows. The Results pane and policy resimulation
              # both consume the Mod 2 schema ($pipeline for hist_sim,
              # $pipelines for each saved scenario), so no translation is
              # required here. (Held in locals - INT-09 publishes all state
              # atomically at the end of a fully successful run.)
              res_choice <- hs$residuals %||% residuals() %||% "original"
              # Preserve the residual treatment captured by the Step 2 run.
              hs_for_baseline <- hs
              hs_for_baseline$residuals <- res_choice
              baseline_out         <- hs_for_baseline
              baseline_scenarios_out <- ss %||% list()

              shiny::setProgress(value = 0.6, detail = "Policy...")
              # Derive the policy arm from the baseline pipelines by adding
              # the analytic per-household delta_total (the same number the
              # Decomposition pane reports). This (a) eliminates the
              # baseline/policy disagreement when residual draws have any
              # stochastic component - both arms now share identical
              # train_aug / id_vec / svy_row_id, so residuals line up
              # household-for-household and a no-op policy yields a no-op
              # visual effect; and (b) removes the per-CMIP6-member re-
              # simulation, which was the dominant cost of every policy
              # adjustment.
              skip_coef_val <- isTRUE(skip_coef_draws())

              # PERF-22: covariate deltas and the training-outcome ecdf are
              # weather- and scenario-independent - every decompose_policy_
              # effect() call below (historical + per scenario-year) would
              # otherwise rebuild both. Compute once, pass through.
              deltas_pre <- .compute_policy_deltas(
                svy, svy_mod, hs$so$name, mf$weather_terms
              )
              F_hat_pre <- if (identical(mf$engine, "rif") &&
                               !is.null(mf$train_data) &&
                               hs$so$name %in% names(mf$train_data)) {
                stats::ecdf(mf$train_data[[hs$so$name]])
              } else NULL

              pol_out <- apply_policy_delta_to_baseline(
                svy_baseline             = svy,
                svy_policy               = svy_mod,
                model_fit                = mf,
                so                       = hs$so,
                hist_sim_baseline        = baseline_out,
                saved_scenarios_baseline = baseline_scenarios_out,
                skip_coef                = skip_coef_val,
                deltas                   = deltas_pre,
                F_hat                    = F_hat_pre
              )
              if (is.null(pol_out)) {
                stop("Policy simulation produced no results.", call. = FALSE)
              }

              # Decompose policy effects using HISTORICAL mean weather.
              # REACT-05: a decomposition failure now fails the whole run
              # instead of silently presenting the previous run as new.
              shiny::setProgress(value = 0.85, detail = "Decomposing effects...")
              decomp <- decompose_policy_effect(
                svy_baseline = svy,
                svy_policy   = svy_mod,
                model_fit    = mf,
                so           = hs$so,
                weather_raw  = hs$weather_raw,
                skip_coef    = skip_coef_val,
                deltas       = deltas_pre,
                F_hat        = F_hat_pre
              )
              if (is.null(decomp)) {
                stop("Effect decomposition produced no results.", call. = FALSE)
              }

              # Decompose per saved scenario * sim_year for year-to-year
              # variation. Per-year failures are collected: if every attempt
              # fails the run fails; otherwise partial results are published
              # with a warning naming the count of dropped pieces (INT-04).
              shiny::setProgress(value = 0.90, detail = "Decomposing scenario effects...")
              sc_list <- pol_out$saved_scenarios %||% list()
              decomp_sc_errors <- character(0)
              decomp_sc <- lapply(seq_along(sc_list), function(i) {
                sc       <- sc_list[[i]]
                w_raw    <- sc$weather_raw
                if (is.null(w_raw)) return(NULL)
                sc_label <- names(sc_list)[i] %||% paste0("Scenario ", i)

                # Identify years present in this scenario's weather panel
                # (computed once and reused for subsetting below, rather than
                # re-parsing timestamps for every year in the loop)
                if ("timestamp" %in% names(w_raw)) {
                  w_years   <- as.integer(format(w_raw$timestamp, "%Y"))
                  sim_years <- sort(unique(w_years))
                } else {
                  # No year column - fall back to single decomposition (mean weather)
                  w_years   <- NULL
                  sim_years <- NA_integer_
                }

                year_results <- lapply(sim_years, function(yr) {
                  # Subset to this year's weather rows (or use full panel if no year info)
                  w_yr <- if (!is.na(yr)) {
                    w_raw[w_years == yr, ]
                  } else {
                    w_raw
                  }
                  tryCatch(
                    decompose_policy_effect(
                      svy_baseline = svy,
                      svy_policy   = svy_mod,
                      model_fit    = mf,
                      so           = hs$so,
                      weather_raw  = w_yr,
                      skip_coef    = skip_coef_val,
                      deltas       = deltas_pre,
                      F_hat        = F_hat_pre
                    ) |> dplyr::mutate(
                      scenario   = sc_label,
                      sim_year   = yr,
                      year_start = sc$year_range[[1]] %||% NA_integer_,
                      year_end   = sc$year_range[[2]] %||% NA_integer_
                    ),
                    error = function(e) {
                      decomp_sc_errors <<- c(decomp_sc_errors, paste0(
                        sc_label, if (!is.na(yr)) paste0(" (", yr, ")"), ": ",
                        conditionMessage(e)
                      ))
                      NULL
                    }
                  )
                })
                dplyr::bind_rows(Filter(Negate(is.null), year_results))
              })
              decomp_sc <- dplyr::bind_rows(Filter(Negate(is.null), decomp_sc))
              if (length(decomp_sc_errors) > 0L && nrow(decomp_sc) == 0L) {
                stop(
                  "All scenario decompositions failed. First error: ",
                  decomp_sc_errors[[1]],
                  call. = FALSE
                )
              }

              shiny::setProgress(value = 1, detail = "Complete")
            }
          )

          # -- Atomic publish (INT-09) -----------------------------------------
          # Every reactive value is written only now that the complete run
          # (simulation + decomposition) succeeded, so a failure anywhere
          # above leaves the previous results, diagnostics, and run ID intact.
          # INT-08: the policy run signature is stored with both result arms.
          baseline_out$.sig <- policy_sig
          if (!is.null(pol_out$hist_sim)) pol_out$hist_sim$.sig <- policy_sig
          baseline_svy_rv(svy)
          policy_svy_rv(svy_mod)
          baseline_hist_sim_rv(baseline_out)
          baseline_saved_scenarios_rv(baseline_scenarios_out)
          policy_hist_sim_rv(pol_out$hist_sim)
          policy_saved_scenarios_rv(pol_out$saved_scenarios)
          decomp_rv(decomp)
          decomp_scenarios_rv(decomp_sc)
          policy_stale(FALSE)

          sim_run_id(isolate(sim_run_id()) + 1L)
          if (length(decomp_sc_errors) > 0L) {
            shiny::showNotification(
              paste0(
                "Policy simulation succeeded, but ", length(decomp_sc_errors),
                " scenario decomposition(s) failed and are omitted (first: ",
                decomp_sc_errors[[1]], ")."
              ),
              type = "warning", duration = 10
            )
          } else {
            shiny::showNotification(
              "Policy adjustments applied and simulation re-run.",
              type = "message", duration = 3
            )
          }
        },
        error = function(e) {
          sim_error(e)
          shiny::showNotification(
            paste0("Policy simulation failed: ", conditionMessage(e)),
            type = "error", duration = 8
          )
        }
      )
      invisible(NULL)
    }

    # REACT-09: the parent requests a run by firing this trigger instead of
    # calling the exported run() closure.
    #
    # REACT-19: no `ignoreInit` here. The parent's `req()` already blocks both
    # states that precede a real click - NULL while the button has not been
    # rendered, and 0 once it has (an action button's 0 is not truthy). That
    # made `ignoreInit` actively harmful: it skips the handler on the
    # observer's first *successful* evaluation of the event expression, and
    # because every earlier evaluation aborted on the unmet `req()`, the first
    # successful one was the user's first click. Step 3 therefore ignored the
    # first "Run simulation" and worked on every click after that.
    shiny::observeEvent(run_trigger(), {
      run()
    })

    list(
      running                  = sim_running,
      baseline_svy             = baseline_svy_rv,
      policy_svy               = policy_svy_rv,
      sim_run_id               = sim_run_id,
      decomp_result            = decomp_rv,
      decomp_scenarios         = decomp_scenarios_rv,
      baseline_hist_sim        = baseline_hist_sim_rv,
      baseline_saved_scenarios = baseline_saved_scenarios_rv,
      policy_hist_sim          = policy_hist_sim_rv,
      policy_saved_scenarios   = policy_saved_scenarios_rv,
      stale                    = policy_stale
    )
  })
}
