#' 1_07_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_1_07_results_ui <- function(id) {
  tagList()
}

#' 1_07_results Server Functions
#'
#' @param id              Module id.
#' @param variable_list   Reactive data frame of variable metadata.
#' @param selected_surveys Reactive data frame of selected surveys.
#' @param selected_outcome Reactive one-row data frame from mod_1_03_outcome.
#' @param selected_weather Reactive data frame from mod_1_04_weather.
#' @param survey_weather  Reactive data frame from mod_1_05_weatherstats.
#' @param selected_model  Reactive list from mod_1_06_model.
#' @param fit_guard       Busy guard shared with mod_1_06 (REACT-02); optional.
#' @param tabset_id       Character id of the parent tabset panel.
#' @param tabset_session  Shiny session for the tabset (defaults to parent).
#'
#' @noRd
mod_1_07_results_server <- function(id,
                                    variable_list,
                                    selected_surveys,
                                    selected_outcome,
                                    selected_weather,
                                    survey_weather,
                                    selected_model,
                                    run_model,
                                    fit_guard = NULL,
                                    stored_breaks = NULL,
                                    survey_version = reactive(0L),
                                    tabset_id,
                                    tabset_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) tabset_session <- session$parent %||% session

    # Internal state ----

    model_fit_val <- reactiveVal(NULL)
    results_tab_added <- reactiveVal(FALSE)
    # INT-08: TRUE while the stored fit's run signature no longer matches the
    # current upstream inputs.
    stale <- reactiveVal(FALSE)
    fit_generation <- reactiveVal(0L)
    fit_status <- reactiveVal("idle")

    # Run signature (INT-08) ----
    # Immutable snapshot of everything the fit depends on; recomputed from
    # live inputs and compared with the stored fit's signature.

    .fit_sig_from_live <- function() {
      sw <- survey_weather()
      list(
        step           = "fit",
        survey_version = survey_version(),
        survey_shape   = if (is.null(sw)) NULL else c(nrow(sw), ncol(sw)),
        outcome        = .sig_plain(selected_outcome()),
        weather        = .sig_plain(selected_weather()),
        model          = .sig_plain(selected_model())
      )
    }

    live_fit_sig <- shiny::reactive(.fit_sig_from_live())

    observeEvent(live_fit_sig(),
      {
        mf <- model_fit_val()
        if (!is.null(mf) && !identical(live_fit_sig(), mf$.sig)) stale(TRUE)
      },
      ignoreInit = TRUE
    )

    output$stale_banner <- renderUI({
      if (isTRUE(stale())) .stale_banner("Step 1 model results") else NULL
    })

    # REACT-14: persistent banner disclosing specification fallbacks
    # (logistic -> linear, clustered -> unclustered VCV) recorded by
    # fit_model(). The results below come from the fitted specification, so
    # the deviation from the requested one must stay visible. This is a
    # correctness disclosure and stays on screen; the descriptive run
    # provenance it used to sit beside now travels with the export bundle
    # instead (see fct_provenance.R).
    output$fallback_banner <- renderUI({
      fb <- model_fit_val()$fallbacks %||% list()
      if (!length(fb)) {
        return(NULL)
      }
      items <- lapply(fb, function(x) {
        shiny::tags$li(sprintf(
          "%s: requested %s, fitted %s (%s).",
          switch(x$kind,
            model_family = "Model family",
            vcv          = "Standard errors",
            x$kind
          ),
          x$requested, x$used, x$reason
        ))
      })
      shiny::div(
        class = "alert alert-warning",
        role = "alert",
        style = "margin-bottom: 10px;",
        shiny::tags$b(
          "\u26a0 The fitted model differs from the requested specification."
        ),
        "The fit fell back as follows; all results below come from the",
        "fitted specification:",
        shiny::tags$ul(items)
      )
    })

    native_fit <- function(fit) extract_native_fit(fit, model_fit_val()$engine)

    # Run model ----
    # The "Run model" button lives in mod_1_06_model; the reactive `run_model`
    # parameter wraps that button's input counter and fires here on click.

    observeEvent(run_model(),
      {
        fit_generation(fit_generation() + 1L)
        fit_status("running")
        completed <- FALSE
        on.exit(
          {
            if (!completed) fit_status("failure")
          },
          add = TRUE
        )
        req(selected_outcome(), selected_weather(), selected_model(), survey_weather())
        # REACT-02: honour the shared mod_1_06 guard; one fit at a time.
        if (!is.null(fit_guard)) {
          if (!fit_guard$begin()) {
            return(invisible(NULL))
          }
          on.exit(fit_guard$end(), add = TRUE)
        }

        nid <- shiny::showNotification("Fitting the welfare model...",
          type = "message", duration = NULL,
          closeButton = FALSE
        )
        on.exit(shiny::removeNotification(nid), add = TRUE)

        df_raw <- as.data.frame(survey_weather())
        # Bin-edge labels: cut() levels carry the +/-Inf sentinel edges used
        # for bucketing; substitute the observed outer breaks stored at weather
        # load so every downstream label reads "(36.3, 40.1]", not
        # "(36.3, Inf]". Applied to the fitted frame AND the snapshot, so
        # cards, figures and tables all show observed ranges.
        svw <- relabel_bin_levels(
          df_raw,
          tryCatch(if (is.function(stored_breaks)) stored_breaks() else stored_breaks,
            error = function(e) NULL
          )
        )
        df <- prepare_outcome_df(svw, selected_outcome())

        fit_args <- list(
          df               = df,
          selected_outcome = selected_outcome(),
          selected_weather = selected_weather(),
          selected_model   = selected_model()
        )
        if ("seed" %in% names(formals(fit_model))) {
          fit_args$seed <- wise_current_seed()
        }
        fit_list <- tryCatch(
          do.call(fit_model, fit_args),
          error = function(e) {
            shiny::showNotification(paste("Model failed:", conditionMessage(e)),
              type = "error", duration = 10
            )
            NULL
          }
        )

        if (!is.null(fit_list)) {
          # INT-05: snapshot every label/setting the renderers need at fit time.
          # Result renderers must describe the fitted run, not whatever is
          # selected when they re-render.
          fit_list$.snap <- list(
            outcome        = selected_outcome(),
            weather        = selected_weather(),
            survey_weather = svw,
            variable_list  = if (is.function(variable_list)) variable_list() else variable_list,
            model          = selected_model()
          )
          # INT-08: the run signature is stored with the result and compared
          # against live inputs; a mismatch marks the results stale.
          fit_list$.sig <- .fit_sig_from_live()
          stale(FALSE)
          model_fit_val(fit_list)
          fit_status("success")
          completed <- TRUE
          # REACT-14: disclose any specification fallback the fitter applied.
          # A model-family change (logistic -> linear) alters the estimand, so
          # it additionally requires explicit acknowledgement.
          fb <- fit_list$fallbacks %||% list()
          if (length(fb)) {
            shiny::showNotification(
              paste0(
                "Models fitted with specification fallbacks (see the banner ",
                "on the Results tab)."
              ),
              type = "warning", duration = 10
            )
            family_fb <- Filter(function(x) identical(x$kind, "model_family"), fb)
            if (length(family_fb)) {
              shiny::showModal(shiny::modalDialog(
                title = "Model family fallback",
                shiny::tags$p(
                  "The requested logistic regression could not be fitted and",
                  " the model fell back to linear. All Step 1-3 results use the",
                  " fitted specification unless you re-fit:"
                ),
                shiny::tags$ul(
                  lapply(family_fb, function(x) {
                    shiny::tags$li(sprintf("%s (%s).", x$reason, x$used))
                  })
                ),
                easyClose = FALSE,
                footer = shiny::modalButton("I understand")
              ))
            }
          }
        }
      },
      ignoreInit = TRUE
    )

    # Render outputs ----

    observeEvent(model_fit_val(),
      {
        req(model_fit_val(), selected_weather())

        mf <- model_fit_val()
        snap <- mf$.snap
        # INT-05: every renderer below binds to the fit-time snapshot; changing
        # the outcome/weather selections afterwards cannot relabel old results.
        label_fun <- .label_lookup(snap$variable_list)
        sw_snap <- snap$weather
        outcome_snap <- snap$outcome

        # Selected model card (snapshot, INT-05 pattern) ----
        # Describes the specification the button captured, like every other
        # output on this tab.

        output$selected_model_card <- renderUI({
          req(snap$model)
          selection_summary_card(
            title = "Selected model",
            badge = model_covariate_badge(snap$model),
            rows = model_card_rows(
              snap$model,
              label_fun      = label_fun,
              outcome_label  = as.character(outcome_snap$label[1]),
              weather_labels = as.character(sw_snap$label)
            ),
            info = paste(
              "The fitted specification is shown as selected outcome, weather,",
              "interaction, and fixed-effect variables. Results",
              "reflect this run until you press Run model again."
            )
          )
        })

        # UI-48: one builder per figure, used by both renderPlot and the export
        # bundle - a downloaded PNG is the plot on screen, not a re-derivation
        # of it. Registered inside this observer so each re-fit refreshes the
        # closure (and with it the fit-time snapshot the figure is drawn from);
        # `wise_export_register()` replaces by key, so re-fits cannot accumulate
        # duplicate entries.
        # Shared figure language (engine- and scale-aware) ----
        # One set of labels/flags derived from the fit snapshot so section 3
        # (relationship), section 4 (who is most affected), section 5
        # (stability) and the focused table cannot disagree.
        is_rif <- identical(mf$engine, "rif")
        is_logit <- is_logistic_fit(mf)
        is_lpm <- !is_logit && identical(tolower(as.character(outcome_snap$type[1])), "logical")
        is_log_out <- identical(as.character(outcome_snap$transform[1]), "log")
        y_lab_lower <- tolower(as.character(outcome_snap$label[1]))
        has_int <- length(mf$interaction_terms) > 0

        # P5: derive translations once for this completed fit. Everything below
        # consumes these fit-time values, so stale live selections cannot trigger
        # recomputation or change the displayed/exported results.
        scenarios_by_var <- stats::setNames(
          lapply(mf$weather_terms, function(v) {
            tryCatch(step1_scenarios(mf, snap, v), error = function(e) NULL)
          }),
          mf$weather_terms
        )
        rif_scenarios <- if (is_rif) {
          stats::setNames(
            lapply(mf$weather_terms, function(v) {
              tryCatch(.s1_rif_scenarios(mf, snap, v, taus = c(0.1, 0.9)),
                error = function(e) NULL
              )
            }),
            mf$weather_terms
          )
        } else {
          NULL
        }
        rif_heterogeneity <- if (is_rif) {
          stats::setNames(
            lapply(mf$weather_terms, function(v) {
              tryCatch(step1_rif_heterogeneity_p(mf, snap, v),
                error = function(e) NULL
              )
            }),
            mf$weather_terms
          )
        } else {
          NULL
        }
        headline_res <- tryCatch(
          step1_headline_cards(
            mf, snap,
            label_fun = label_fun,
            scenarios_list = scenarios_by_var,
            rif_scenarios = rif_scenarios,
            rif_heterogeneity = rif_heterogeneity
          ),
          error = function(e) NULL
        )
        headline_tbl <- tryCatch(
          step1_headline_table(result = headline_res),
          error = function(e) NULL
        )

        # Coefficient plots show model-scale coefficients, not translated
        # effects, so their axis carries the coefficient unit.
        coef_unit_lab <- if (is_logit) {
          "Coefficient (log-odds)"
        } else if (is_rif || is_log_out) {
          "Coefficient (log points)"
        } else if (is_lpm) {
          "Coefficient (probability)"
        } else {
          "Coefficient"
        }

        # Spec (3) is only "FE + controls" when controls were actually chosen;
        # otherwise it is numerically identical to spec (2) and the labels must
        # say so (coefplot legend + spec comparison table).
        n_covs <- length(unique(c(
          snap$model$hh_covariates, snap$model$area_covariates,
          snap$model$ind_covariates, snap$model$firm_covariates
        )))
        has_controls <- n_covs > 0 ||
          !identical(snap$model$covariate_selection, "User-defined")

        # Short outcome phrase for probability wording (strips the poverty-line
        # parenthetical from labels like "Poor (welfare < poverty line)").
        y_short <- sub("\\s*\\(.*$", "", y_lab_lower)

        # Reference-profile linear predictor for binary outcomes: the same map
        # the headline cards use, so binned-plot pp effects match the cards.
        profile_eta0 <- if (is_logit) {
          pe <- scenarios_by_var[[mf$weather_terms[1]]]$profile_eta
          if (length(pe) == 1 && is.finite(pe)) pe else NA_real_
        } else {
          NA_real_
        }

        # Effect-plot y-axis: continuous shapes draw predicted levels, binned
        # shapes draw bin-vs-reference contrasts - these need different labels.
        is_bin_vec <- vapply(seq_along(mf$weather_terms), function(i) {
          identical(as.character(sw_snap$cont_binned[sw_snap$name == mf$weather_terms[i]][1]), "Binned")
        }, logical(1))
        effect_y_lab <- function(i) {
          binned <- is_bin_vec[i]
          un <- as.character(sw_snap$units[sw_snap$name == mf$weather_terms[i]][1])
          if (is.na(un) || !nzchar(un)) un <- "unit"
          if (identical(as.character(outcome_snap$name[1]), "poor")) {
            line <- suppressWarnings(as.numeric(outcome_snap$povline[1]))
            if (is.finite(line)) {
              poor_label <- paste0("Poor ($", formatC(line, format = "f", digits = 2), "/day)")
              if (binned) {
                return(paste0("Change in ", poor_label, " probability vs reference bin (pp)"))
              }
              return(paste0("pp change in ", poor_label, " probability per +1 ", un))
            }
          }
          if (binned) {
            if (is_logit) {
              if (is.finite(profile_eta0)) {
                paste0(
                  "Change in ", y_short,
                  " probability vs reference bin (pp)"
                )
              } else {
                "Effect vs reference bin (log-odds)"
              }
            } else if (is_lpm) {
              paste0("Change in ", y_short, " probability vs reference bin")
            } else if (is_log_out) {
              paste0(
                "Effect on log ", y_lab_lower,
                " vs reference bin (log points)"
              )
            } else {
              paste0("Effect on ", y_lab_lower, " vs reference bin")
            }
          } else {
            # Continuous shapes draw the marginal effect (slope vs weather).
            if (is_logit) {
              if (is.finite(profile_eta0)) {
                paste0("pp change in ", y_short, " probability per +1 ", un)
              } else {
                paste0("log-odds change per +1 ", un)
              }
            } else if (is_lpm) {
              paste0("pp change in ", y_short, " probability per +1 ", un)
            } else if (is_log_out) {
              paste0("% change in ", y_lab_lower, " per +1 ", un)
            } else {
              paste0("Change in ", y_lab_lower, " per +1 ", un)
            }
          }
        }
        # Effect transform per shape: binned logit contrasts map through
        # plogis at the reference profile ("pp"); binned LPM contrasts are
        # x100 ("pp100"); continuous log-outcome slopes map through exp
        # ("pct"); continuous logit slopes scale by p(1-p) ("pp"); LPM slopes
        # x100 ("pp100"); everything else stays on the model scale.
        effect_scale_arg <- function(i) {
          binned <- is_bin_vec[i]
          if (is_logit) {
            if (is.finite(profile_eta0)) "pp" else "model"
          } else if (is_lpm) {
            "pp100"
          } else if (is_log_out) {
            if (binned) "model" else "pct"
          } else {
            "model"
          }
        }

        # Unit-complete x-axis label from the weather snapshot: never the raw
        # column name.
        axis_lab <- function(i) {
          var <- mf$weather_terms[i]
          lab <- wise_label_short(label_fun(var))
          un <- as.character(sw_snap$units[sw_snap$name == var][1])
          binned <- identical(as.character(sw_snap$cont_binned[sw_snap$name == var][1]), "Binned")
          if (is.na(un) || !nzchar(un)) {
            return(if (binned) paste0(lab, " bins") else lab)
          }
          if (binned) paste0(lab, " bins (", un, ")") else paste0(lab, " (", un, ")")
        }

        sd_named <- stats::setNames(vapply(mf$weather_terms, function(v) {
          x <- suppressWarnings(as.numeric(mf$train_data[[v]]))
          x <- x[is.finite(x)]
          if (length(x) > 10) stats::sd(x) else NA_real_
        }, numeric(1)), mf$weather_terms)
        sd_named <- sd_named[is.finite(sd_named)]

        coef_fig <- function(i) {
          function() {
            mf <- tryCatch(model_fit_val(), error = function(e) NULL)
            if (is.null(mf) || length(mf$weather_terms) < i) {
              return(NULL)
            }
            echart_make_coefplot(
              fit1              = extract_native_fit(mf$fit1, mf$engine),
              fit2              = extract_native_fit(mf$fit2, mf$engine),
              fit3              = extract_native_fit(mf$fit3, mf$engine),
              weather_terms     = mf$weather_terms,
              interaction_terms = mf$interaction_terms,
              outcome_label     = outcome_snap$label,
              label_fun         = label_fun,
              engine            = mf$engine,
              rif_grid          = mf$rif_grid,
              pred_var          = mf$weather_terms[i],
              x_label           = coef_unit_lab,
              has_controls      = has_controls,
              height            = "600px"
            )
          }
        }

        # Section 3: the weather-outcome relationship (main profile, no
        # moderator overlay).
        effect_fig <- function(i) {
          function() {
            mf <- tryCatch(model_fit_val(), error = function(e) NULL)
            if (is.null(mf) || length(mf$weather_terms) < i) {
              return(NULL)
            }
            is_logit_i <- is_logistic_fit(mf)
            echart_weather_effect_plot(
              fit = native_fit(mf$fit3),
              pred_var = mf$weather_terms[i],
              interaction_terms = mf$interaction_terms,
              is_binned = identical(sw_snap$cont_binned[i], "Binned"),
              label_fun = label_fun,
              engine = mf$engine,
              selected_weather = sw_snap,
              weather_df = snap$survey_weather,
              rif_grid = mf$rif_grid,
              mode = "main",
              is_logistic = is_logit_i,
              x_label = axis_lab(i),
              y_label = effect_y_lab(i),
              caption = if (is_logit_i && is_bin_vec[i] && is.finite(profile_eta0)) {
                "pp effects evaluated at the median-risk household profile."
              } else {
                NULL
              },
              effect_scale = effect_scale_arg(i),
              profile_eta = profile_eta0,
              height = "500px"
            )
          }
        }

        # Section 4: who is most affected - RIF quantile curve with quantile
        # marks, or the moderated effect plot when interactions are specified.
        who_fig <- function(i) {
          function() {
            mf <- tryCatch(model_fit_val(), error = function(e) NULL)
            if (is.null(mf) || length(mf$weather_terms) < i) {
              return(NULL)
            }
            if (is_rif) {
              echart_weather_effect_plot(
                fit               = native_fit(mf$fit3),
                pred_var          = mf$weather_terms[i],
                interaction_terms = mf$interaction_terms,
                is_binned         = identical(sw_snap$cont_binned[i], "Binned"),
                label_fun         = label_fun,
                engine            = mf$engine,
                selected_weather  = sw_snap,
                weather_df        = snap$survey_weather,
                rif_grid          = mf$rif_grid,
                mark_taus         = c(0.1, 0.5, 0.9),
                height            = "420px"
              )
            } else {
              has_modx <- any(grepl(
                paste0("\\b", mf$weather_terms[i], "\\b"),
                mf$interaction_terms %||% character(0)
              ))
              if (!has_modx) {
                # No moderation: the section shows the "Uniform by design" note
                # instead of a plot, so no (blank) figure is registered.
                return(NULL)
              }
              echart_weather_effect_plot(
                fit               = native_fit(mf$fit3),
                pred_var          = mf$weather_terms[i],
                interaction_terms = mf$interaction_terms,
                is_binned         = identical(sw_snap$cont_binned[i], "Binned"),
                label_fun         = label_fun,
                engine            = mf$engine,
                selected_weather  = sw_snap,
                weather_df        = snap$survey_weather,
                rif_grid          = mf$rif_grid,
                mode              = "moderated",
                is_logistic       = is_logistic_fit(mf),
                x_label           = axis_lab(i),
                y_label           = effect_y_lab(i),
                effect_scale      = effect_scale_arg(i),
                profile_eta       = profile_eta0,
                height            = "420px"
              )
            }
          }
        }

        for (i in seq_along(mf$weather_terms)) {
          local({
            idx <- i
            term <- label_fun(mf$weather_terms[idx])
            if (!is_rif) {
              wise_export_figure(
                key = paste0("coefficient_plot_", idx),
                label = paste0("Coefficient stability - ", term),
                step = 1L,
                fun = coef_fig(idx),
                description = paste0(
                  "Weather coefficients with confidence intervals across the three ",
                  "nested specifications (specification 3 emphasised), for ", term,
                  ". Outcome: ", outcome_snap$label, "."
                ),
                width = 9, height = 6
              )
            }
            wise_export_figure(
              key = paste0("marginal_effect_plot_", idx),
              label = paste0("Weather-outcome relationship - ", term),
              step = 1L,
              fun = effect_fig(idx),
              description = paste0(
                "Weather-outcome relationship from the full specification ",
                "(fixed effects and controls): bin effects vs the omitted ",
                "reference bin, or the continuous marginal effect with ",
                "observed-weather rug, for ", term, ". Outcome: ",
                outcome_snap$label, "."
              ),
              width = 9, height = 6
            )
            if (is_rif || any(grepl(
              paste0("\\b", mf$weather_terms[idx], "\\b"),
              mf$interaction_terms %||% character(0)
            ))) {
              wise_export_figure(
                key = paste0("who_affected_plot_", idx),
                label = paste0("Who is most affected - ", term),
                step = 1L,
                fun = who_fig(idx),
                description = if (is_rif) {
                  paste0(
                    "Effect of ", term, " across the welfare distribution ",
                    "(tau = 0.1 poorest 10% to tau = 0.9 richest 10%), with median ",
                    "and decile marks."
                  )
                } else {
                  paste0(
                    "Effect of ", term, " by moderator level (moderated effect plot) ",
                    "from the full specification."
                  )
                },
                width = 9, height = 6
              )
            }
          })
        }

        # Stability plots: one per weather variable (hidden for RIF - the
        # quantile curve in "Who is most affected?" already carries that
        # content, per the plan's duplicate-suppression rule).
        output$coefplot1 <- echarts4r::renderEcharts4r({
          req(model_fit_val(), length(model_fit_val()$weather_terms) >= 1)
          ch <- coef_fig(1)()
          req(!is.null(ch))
          ch
        })

        output$coefplot2 <- echarts4r::renderEcharts4r({
          req(model_fit_val(), length(model_fit_val()$weather_terms) >= 2)
          ch <- coef_fig(2)()
          req(!is.null(ch))
          ch
        })

        # Regression table (guidelines §6: reactable + client-side CSV)
        output$regtable <- reactable::renderReactable({
          req(model_fit_val())
          make_regtable_df_reactable(regtable_df())
        })

        # UI-45: the coefficient table renders from the same tidy data frame
        # that the export bundle registers, so screen, CSV button and bundle
        # cannot diverge.
        regtable_df <- function() {
          mf <- model_fit_val()
          if (is.null(mf)) {
            return(NULL)
          }
          make_regtable_df(
            fit1 = extract_native_fit(mf$fit1, mf$engine),
            fit2 = extract_native_fit(mf$fit2, mf$engine),
            fit3 = extract_native_fit(mf$fit3, mf$engine),
            engine = mf$engine,
            rif_grid = mf$rif_grid,
            label_fun = label_fun
          )
        }

        # UI-48: the same estimates go into the export bundle.
        wise_export_table(
          key = "model_coefficients",
          label = "Model coefficients",
          step = 1L,
          fun = regtable_df,
          description = paste(
            "Coefficients, standard errors and p-values for all three nested",
            "specifications (weather only; + fixed effects; + fixed effects and",
            "controls). One row per specification and term."
          )
        )

        # At a glance: outcome definition + headline cards ----
        # One row of four cards per weather variable (effect, who is most
        # affected, spec robustness, sample/fit). All values derive from the fit
        # snapshot (INT-05) through the single translation path in
        # fct_step1_headline.R, so cards, figures and the table cannot diverge.
        output$headline_cards_ui <- renderUI({
          if (is.null(headline_res) || !length(headline_res$rows)) {
            return(NULL)
          }
          shiny::tagList(
            lapply(headline_res$rows, function(r) {
              shiny::tagList(
                shiny::tags$div(class = "step1-headline-var", r$var_label),
                headline_cards_ui(r$cards)
              )
            })
          )
        })

        wise_export_table(
          key = "step1_headline_summary",
          label = "Step 1 headline summary",
          step = 1L,
          fun = function() headline_tbl,
          description = paste(
            "At-a-glance summary per weather variable: translated effect, who",
            "is most affected, specification robustness, and sample/fit."
          )
        )

        # Section 6: focused estimates table (T2) ----
        # Results-first: weather + interaction coefficients of the full
        # specification with CI, p, and the translated per-+1-SD column that
        # matches the At a glance cards. The full AER table stays reachable as
        # an expandable panel.
        scale_note <- if (identical(tolower(as.character(outcome_snap$type[1])), "logical")) {
          "probability scale (0/1)"
        } else {
          units_txt <- if (!is.null(outcome_snap$units) && length(outcome_snap$units)) {
            as.character(outcome_snap$units[1])
          } else {
            ""
          }
          if (is.na(units_txt)) units_txt <- ""
          paste0(
            if (nzchar(units_txt)) paste0(units_txt, " basis, ") else "",
            if (is_log_out) "log scale" else "level scale"
          )
        }
        cluster_txt <- as.character(snap$model$cluster %||% character(0))
        vcv_note <- if (length(cluster_txt)) {
          paste0("SEs clustered by ", paste(cluster_txt, collapse = ", "))
        } else {
          "HC1 robust SEs"
        }
        focused_subheader <- paste0(
          "Weather effects, full specification (3) \u2014 Dependent variable: ",
          outcome_snap$label[1], " (", scale_note, ") \u00b7 ", vcv_note
        )
        focused_footnotes <- c(
          "\u2020 p<0.1 \u00b7 * p<0.05 \u00b7 ** p<0.01 \u00b7 *** p<0.001",
          "95% CI = estimate \u00b1 1.96 \u00d7 SE",
          if (is_logit) {
            "pp effect evaluated at the median-risk household profile; log-odds shown in the Effect column."
          } else if (any(is_bin_vec)) {
            "Translations: per +1 SD for continuous weather terms; hottest bin vs reference bin for binned terms."
          } else {
            "Per +1 SD uses the sample SD of each weather variable; see At a glance for the full contrast including interactions."
          },
          if (is_lpm) "Linear-probability model: predictions can fall outside 0\u20131." else NULL,
          if (is_rif) "RIF coefficients are effects on unconditional quantiles in log points; % translation is approximate." else NULL
        )
        has_poly_terms <- any(grepl(
          "^I\\(",
          names(stats::coef(native_fit(mf$fit3)))
        ))
        focused_df <- function() {
          mf <- model_fit_val()
          if (is.null(mf)) {
            return(NULL)
          }
          tryCatch(
            make_regtable_focused_df(
              fit3 = native_fit(mf$fit3),
              weather_terms = mf$weather_terms,
              interaction_terms = mf$interaction_terms,
              label_fun = label_fun,
              engine = mf$engine,
              is_logistic = is_logistic_fit(mf),
              is_lpm = is_lpm,
              is_log_outcome = is_log_out,
              rif_grid = mf$rif_grid,
              mf = mf,
              scenarios_list = scenarios_by_var,
              sd_x = sd_named
            ),
            error = function(e) NULL
          )
        }
        output$focused_header_ui <- renderUI({
          req(model_fit_val())
          p(class = "wise-subheader", focused_subheader)
        })
        output$focused_table <- reactable::renderReactable({
          req(model_fit_val())
          make_regtable_focused_reactable(
            focused_df(),
            engine = model_fit_val()$engine,
            is_logistic = is_logistic_fit(model_fit_val()),
            is_lpm = is_lpm
          )
        })
        output$focused_notes_ui <- renderUI({
          req(model_fit_val())
          notes <- c(
            focused_footnotes,
            if (has_poly_terms) {
              "Polynomial terms are part of the +1 SD contrast of their base variable; their interaction slope differences vary with the weather level (see the moderated effect plot)."
            } else {
              NULL
            }
          )
          if (!length(notes)) {
            return(NULL)
          }
          htmltools::tagList(lapply(notes, function(f) p(class = "t2-note", f)))
        })
        wise_export_table(
          key = "step1_focused_estimates",
          label = "Focused weather estimates",
          step = 1L,
          fun = focused_df,
          description = paste(
            "Weather and interaction coefficients from the full specification",
            "with 95% CI, p-values and the translated per-+1-SD column."
          )
        )
        wise_export_table(
          key = "step1_spec_comparison",
          label = "Specification comparison",
          step = 1L,
          fun = function() {
            mf <- model_fit_val()
            if (is.null(mf) || identical(mf$engine, "rif")) {
              return(NULL)
            }
            make_regtable_specs_df(
              fit1              = extract_native_fit(mf$fit1, mf$engine),
              fit2              = extract_native_fit(mf$fit2, mf$engine),
              fit3              = extract_native_fit(mf$fit3, mf$engine),
              weather_terms     = mf$weather_terms,
              interaction_terms = mf$interaction_terms,
              label_fun         = label_fun,
              has_controls      = has_controls
            )
          },
          description = paste(
            "Weather and interaction coefficients across the three progressive",
            "specifications (no FE, FE, FE + controls)."
          )
        )
        output$specs_table <- reactable::renderReactable({
          req(model_fit_val())
          mf <- model_fit_val()
          if (identical(mf$engine, "rif")) {
            return(NULL)
          }
          make_regtable_specs_reactable(
            fit1              = extract_native_fit(mf$fit1, mf$engine),
            fit2              = extract_native_fit(mf$fit2, mf$engine),
            fit3              = extract_native_fit(mf$fit3, mf$engine),
            weather_terms     = mf$weather_terms,
            interaction_terms = mf$interaction_terms,
            label_fun         = label_fun,
            has_controls      = has_controls
          )
        })


        # Relationship plots (one per weather variable) - reuse the section-3
        # builder so screen and export stay identical.
        output$effectplot1 <- echarts4r::renderEcharts4r({
          req(model_fit_val(), length(model_fit_val()$weather_terms) >= 1)
          ch <- effect_fig(1)()
          req(!is.null(ch))
          ch
        })

        output$effectplot2 <- echarts4r::renderEcharts4r({
          req(model_fit_val(), length(model_fit_val()$weather_terms) >= 2)
          ch <- effect_fig(2)()
          req(!is.null(ch))
          ch
        })

        # "Who is most affected?" plots (one per weather variable): annotated
        # RIF quantile curves or moderated effect plots.
        output$who_plot1 <- echarts4r::renderEcharts4r({
          req(model_fit_val(), length(model_fit_val()$weather_terms) >= 1)
          ch <- who_fig(1)()
          req(!is.null(ch))
          ch
        })

        output$who_plot2 <- echarts4r::renderEcharts4r({
          req(model_fit_val(), length(model_fit_val()$weather_terms) >= 2)
          ch <- who_fig(2)()
          req(!is.null(ch))
          ch
        })

        # Add / switch Results tab ----

        # INT-05: engine-conditional headings are reactive outputs bound to the
        # current fit, so a re-fit with a different engine updates them instead
        # of describing the first engine forever.
        output$heading_effect <- renderUI({
          req(model_fit_val())
          shiny::h4(
            if (identical(model_fit_val()$engine, "rif")) {
              paste0(
                "How does weather relate to ", y_lab_lower,
                " across the welfare distribution?"
              )
            } else {
              paste0("How does weather relate to ", y_lab_lower, "?")
            },
            info_popover(shiny::tagList(
              shiny::p(paste(
                "Each figure shows how the fitted model translates weather into",
                "the outcome, with all other variables held fixed. Numbers are",
                "associations, not causal effects."
              )),
              shiny::p(if (is_rif) {
                paste(
                  "One curve per welfare quantile: \u03c4 = 0.1 is the poorest 10%,",
                  "\u03c4 = 0.9 the richest 10% of households; the dashed grey marks",
                  "highlight the deciles."
                )
              } else {
                paste(
                  "Binned weather: one point per bin, showing that bin's effect",
                  "relative to the omitted reference bin (the dashed line at y = 0)",
                  "with a 95% confidence interval. Continuous weather: the line is",
                  "the marginal effect per +1 unit with its 95% CI (ribbon); it is",
                  "curved when polynomial terms are specified, flat otherwise. The",
                  "dashed vertical line marks the sample mean and the rug shows",
                  "the observed weather values."
                )
              })
            ))
          )
        })
        output$heading_who <- renderUI({
          req(model_fit_val())
          shiny::h4(
            "Who is most affected?",
            info_popover(shiny::p(if (identical(model_fit_val()$engine, "rif")) {
              paste(
                "The quantile (RIF) model estimates the weather effect at each",
                "point of the welfare distribution: \u03c4 = 0.1 is the poorest",
                "10%, \u03c4 = 0.9 the richest 10%. Heterogeneous effects reflect",
                "estimated distributional gradients under the rank-stability",
                "assumption."
              )
            } else if (has_int) {
              paste(
                "With interactions, the weather effect differs across moderator",
                "levels. Each line is the effect at one level, with other",
                "covariates held at their sample averages."
              )
            } else {
              paste(
                "This specification applies one weather effect to all households;",
                "distributional differences emerge in Steps 2-3 through the",
                "welfare distribution."
              )
            }))
          )
        })
        output$heading_coef <- renderUI({
          req(model_fit_val())
          if (identical(model_fit_val()$engine, "rif")) {
            return(NULL)
          }
          shiny::h4(
            "Is the estimate stable across specifications?",
            info_popover(shiny::p(paste(
              "Weather coefficients from the three nested specifications:",
              "(1) no fixed effects, (2) + fixed effects, (3) + controls",
              "(emphasised). Specification (3) is what Steps 2-3 apply."
            )))
          )
        })
        output$heading_table <- renderUI({
          req(model_fit_val())
          shiny::h4("Full model estimates")
        })

        # Section 4 content: figures when the model carries distributional or
        # moderator heterogeneity, otherwise the homogeneity note card.
        output$who_layout <- shiny::renderUI({
          req(model_fit_val())
          mf <- model_fit_val()
          if (!identical(mf$engine, "rif") && !has_int) {
            return(NULL)
          }
          wt <- mf$weather_terms %||% character(0)
          weather_plot_layout(
            ns, length(wt),
            ids = c("who_plot1", "who_plot2"),
            height = "420px",
            echarts = TRUE,
            alts = vapply(seq_len(max(length(wt), 1L)), function(i) {
              if (identical(mf$engine, "rif")) {
                paste(
                  "Unconditional quantile regression effect of",
                  label_fun(wt[i]), "across the welfare distribution,",
                  "with median and decile marks"
                )
              } else {
                paste(
                  "Moderated effect plot: predicted", outcome_snap$label,
                  "versus", label_fun(wt[i]), "by moderator level"
                )
              }
            }, character(1))
          )
        })
        output$who_note_ui <- renderUI({
          req(model_fit_val())
          mf <- model_fit_val()
          if (identical(mf$engine, "rif")) {
            shiny::p(class = "step1-headline-note", paste(
              "The curve shows how the weather effect changes across the",
              "welfare distribution: \u03c4 = 0.1 is the poorest 10%, \u03c4 =",
              "0.9 the richest 10%, \u03c4 = 0.5 the median. Ribbon = 95% CI.",
              "Heterogeneous effects reflect estimated distributional gradients",
              "under the rank-stability assumption; they are not causal",
              "subgroup effects."
            ))
          } else if (has_int) {
            shiny::p(class = "step1-headline-note", paste(
              "Each line is the weather effect at one moderator level, with",
              "other covariates at their sample averages. Ribbon = 95% CI",
              "(coefficient uncertainty)."
            ))
          } else {
            card <- if (!is.null(headline_res) && length(headline_res$rows)) {
              headline_res$rows[[1]]$cards[[2]]
            } else {
              NULL
            }
            shiny::div(
              class = "alert alert-info", role = "status",
              style = "margin-bottom: 10px;",
              shiny::tags$b(card$value %||% "Uniform by design"), ". ",
              card$note %||% paste(
                "This specification applies one weather effect to all",
                "households."
              )
            )
          }
        })

        # Reactive layouts so panels switch between 1 and 2 columns when the
        # model is re-fit with a different number of weather variables.
        # UI-36: alt text follows the fitted engine and current weather labels.
        output$effectplot_layout <- shiny::renderUI({
          req(model_fit_val())
          mf <- model_fit_val()
          wt <- mf$weather_terms %||% character(0)
          is_rif <- identical(mf$engine, "rif")
          weather_plot_layout(
            ns, length(wt),
            ids = c("effectplot1", "effectplot2"),
            height = "500px",
            echarts = TRUE,
            alts = vapply(seq_len(max(length(wt), 1L)), function(i) {
              if (is_rif) {
                paste(
                  "Unconditional quantile regression effect of",
                  label_fun(wt[i]), "across the welfare distribution"
                )
              } else {
                paste(
                  "Line plot of predicted", outcome_snap$label, "versus",
                  label_fun(wt[i]), "with 95% CI ribbon, observed-weather",
                  "rug and translated-effect annotation"
                )
              }
            }, character(1))
          )
        })
        output$coefplot_layout <- shiny::renderUI({
          req(model_fit_val())
          mf <- model_fit_val()
          if (identical(mf$engine, "rif")) {
            return(NULL)
          }
          wt <- mf$weather_terms %||% character(0)
          weather_plot_layout(
            ns, length(wt),
            ids = c("coefplot1", "coefplot2"),
            height = "600px",
            echarts = TRUE,
            alts = vapply(seq_len(max(length(wt), 1L)), function(i) {
              paste(
                "Coefficient stability plot with confidence intervals for",
                label_fun(wt[i]), "across the three model specifications,",
                "specification 3 emphasised"
              )
            }, character(1))
          )
        })

        if (!results_tab_added()) {
          shiny::appendTab(
            inputId = tabset_id,
            shiny::tabPanel(
              title = "Results",
              value = "results",
              shiny::uiOutput(ns("fallback_banner")),
              shiny::uiOutput(ns("stale_banner")),
              uiOutput(ns("selected_model_card")),
              shiny::uiOutput(ns("headline_cards_ui")),
              shiny::br(),
              shiny::uiOutput(ns("heading_effect")),
              shiny::uiOutput(ns("effectplot_layout")),
              shiny::br(),
              shiny::uiOutput(ns("heading_who")),
              shiny::uiOutput(ns("who_layout")),
              shiny::uiOutput(ns("who_note_ui")),
              shiny::br(),
              shiny::uiOutput(ns("heading_coef")),
              shiny::uiOutput(ns("coefplot_layout")),
              shiny::br(),
              shiny::uiOutput(ns("heading_table")),
              shiny::uiOutput(ns("focused_header_ui")),
              shiny::tags$div(
                class = "wise-reactable-controls",
                wise_reactable_csv_button(ns("focused_table"), "step1_focused_estimates")
              ),
              reactable::reactableOutput(ns("focused_table")),
              shiny::uiOutput(ns("focused_notes_ui")),
              shiny::tags$details(
                shiny::tags$summary("Compare specifications (1) \u2013 (3)"),
                shiny::tags$div(
                  class = "wise-reactable-controls",
                  wise_reactable_csv_button(ns("specs_table"), "step1_spec_comparison")
                ),
                reactable::reactableOutput(ns("specs_table"))
              ),
              shiny::tags$details(
                shiny::tags$summary("All coefficients (controls, fixed effects)"),
                shiny::tags$div(
                  class = "wise-reactable-controls",
                  wise_reactable_csv_button(ns("regtable"), "model_coefficients")
                ),
                reactable::reactableOutput(ns("regtable"))
              ),
              shiny::p(class = "step1-headline-note", paste(
                "Residuals, predicted vs actual, and the raw model summary",
                "are on the Model fit tab."
              ))
            ),
            select = TRUE,
            session = tabset_session
          )
          results_tab_added(TRUE)
        } else {
          try(shiny::updateTabsetPanel(tabset_session,
            inputId = tabset_id,
            selected = "results"
          ), silent = TRUE)
        }

        shiny::showNotification("Results ready.", type = "message", duration = 3)
      },
      ignoreInit = TRUE
    )

    # Return ----

    list(
      model_fit = model_fit_val,
      stale = stale,
      fit_generation = fit_generation,
      fit_status = fit_status
    )
  })
}
