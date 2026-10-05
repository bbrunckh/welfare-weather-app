# Step 2 live-run contract (Phases 4-7)

Shared interface between `mod_2_01_weathersim.R` (producer), `mod_2_simulation.R`
(wiring) and `mod_2_02_results.R` (consumer). Temporary working doc for the
progressive-results implementation; fold into the plan when done.

## Partial payload (produced by the worker, Phase 3; schema 1)

list(schema = 1L, kind = "historical"|"scenario", label = chr(1),
     ordinal = int (0 = historical, then 1..groups_total in completion order),
     groups_total = int, n_models = int, n_models_requested = int,
     display = list(method, pov_line, bandwidth_p0, residuals, skip_coef,
                    is_log, weight_key = "weighted"|"unweighted",
                    methods = chr  # every streamed method),
     so = <outcome row>, has_weights = lgl, has_draws = lgl,
     table = <the display method's table = tables[[display$method]]; kept for
              backward compatibility>,
     tables = named list method -> aggregated tibble, each identical to the
              module's hist_agg_rv()/scenario_agg_rv() [[weight_key]][[method]]
              for that method. The worker computes the full suite once per
              group (every hist_aggregate_choices() method except
              prosperity_gap, same arguments as the module's suite branch);
              prosperity_gap is included only when it is display$method.
              Optional on the wire: a partial without `tables` serves only
              `table`. Validation: named list of non-empty data frames that
              includes display$method.)

## mod_2_01 exports (Phase 4)

- `live_run`: reactive, NULL when no run is streaming. Otherwise:
  list(generation = int, dependency_signature = list, status = chr,
       started_at = POSIXct, elapsed = num,
       groups_total = int, groups_done = int,
       scenario_labels = chr  # ALL expected scenario labels, in run order
                              # (SSP outer, period inner), built with
                              # .step2_scenario_display_key()
       current_label = chr|NULL, member_index = int|NULL,
       period_members = int|NULL, eta_seconds = num|NULL,
       display = list(method, pov_line, bandwidth_p0),  # captured at submit
       partials = list(historical = <partial>|NULL,
                       scenarios = named list label -> <partial>,
                                   in arrival order))
  Set to a fresh value at submit (partials empty). Cleared (NULL) on cancel,
  stale, failure, session end and successful adoption.
- `adopted_partials`: reactive, NULL or
  list(dependency_signature = list, partials = <same as live_run$partials>).
  Set inside the same commit as hist_sim()/saved_scenarios() on successful
  adoption (hist_sim()$.sig is identical to dependency_signature). Set to NULL
  when a new run is submitted or hist_sim is cleared.

## Display settings (Phase 4 reads, Phase 5 writes)

- `mod_2_simulation.R` creates `display_settings <- reactiveVal(NULL)`.
- `mod_2_02_results_server(..., display_settings_out = NULL)` writes
  list(method, pov_line, bandwidth_p0) whenever its controls change (committed
  mode only).
- `mod_2_01_weathersim_server(..., display_settings = reactive(NULL))` reads it
  with isolate() at submit and puts it in snapshot$input$display (NULL -> the
  worker defaults: mean / so$povline / 0.05).

## Wiring (Phase 5 owns mod_2_simulation.R)

mod_2_02_results_server gets `live_run = s1$live_run`,
`adopted_partials = s1$adopted_partials`, `display_settings_out = display_settings`.
mod_2_01_weathersim_server gets `display_settings = display_settings`.
