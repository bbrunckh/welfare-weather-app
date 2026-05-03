# Known Issues — Welfare Weather App

## 1. SSP5-8.5 / 2080-2090 near-zero variance in exceedance table
   Flagged: 2026-04-24 21:21:50
   Symptom: All return-period thresholds nearly identical (~4.554-4.556)
   Hypothesis: Very few CMIP6 models have data for SSP5-8.5 / 2080-2090.
     summarise_ensemble() averaging across fewer models produces degenerate
     distribution. Check n_members in combine_ensemble_results() output.
   Impact: Exceedance curve will appear as a near-vertical line for this
     scenario. Not a pipeline error — data limitation.
   Investigation needed:
     1. Check how many CMIP6 models have data for SSP5-8.5 post-2070
     2. Add n_members to scenario metadata and surface in UI tooltip
     3. Consider warning when n_members < 3 for a scenario group
   Priority: Low — edge case, not blocking
   Related: summarise_ensemble() in fct_get_weather.R


## 2. Flat headcount ratio at pov_line = $5/day (Guinea-Bissau 2021)
   Flagged: 2026-04-24 21:57:26
   Symptom: All scenarios and return periods show exactly 0.726 at pov_line=5
   Hypothesis: $5/day sits at a natural breakpoint in the welfare distribution.
     With F_loading=NULL (skip_coef_draws=ON), draw_values has zero variance
     so all return-period quantiles are identical point estimates.
     Verify by enabling coefficient draws — bands should appear if data-driven.
   Impact: Cosmetic — exceedance curve appears as vertical line at this threshold
   Investigation: Plot welfare distribution histogram, check density at $5/day
   Priority: Low — data artifact, not a pipeline error

## 4. Code review — weather_raw vs weather_prepared naming + pipeline audit
   Flagged: 2026-04-25 18:46:48
   Both weather_raw and weather_prepared passed through pipeline.
   Review: sensible renaming, remove redundancy, logical flow audit.
   Priority: Medium — post-Phase 3, during cleanup Step 8

## 5. Positive temperature-welfare relationship (counter-intuitive)
   Flagged: 2026-04-25 19:04:41
   Symptom: LASSO selection with Guinea-Bissau 2018+2021, temp only,
     electricity policy scenario produces positive temp-welfare coefficient.
   Hypothesis: LASSO regularization selecting spurious correlation,
     or welfare is consumption-based and higher temps correlate with
     agricultural productivity in this context.
   Action: Flag for methodology review. Consider Module 1 output banner
     showing key coefficient signs + magnitudes for sanity check.
   Priority: Medium — methodological, not a pipeline error

## 6. Some models excluded due to missing variables
   Flagged: 2026-04-25 19:04:41
   Symptom: Console shows models excluded during simulation due to
     missing weather variables for certain SSP/period combinations.
   Hypothesis: CMIP6 models have incomplete coverage for some variables
     across all SSP scenarios and time periods.
   Action: Build a model-variable coverage matrix — flag which CMIP6
     models are missing key variables for which periods/SSPs.
     Important for report writing — need to know if key models are
     systematically absent for certain projections.
   Priority: Medium — data quality, important for reporting

## 7. Module 1 output banner for coefficient sanity checks
   Flagged: 2026-04-25 19:04:41
   Symptom: No easy way to save/refer to model output for follow-up.
   Action: Add Module 1 results banner showing key coefficient signs,
     magnitudes, SE, cluster counts. Exportable to PDF/CSV.
     Useful for methodology notes and sanity checking.
   Priority: Low — UX improvement, post-Phase 3

## 8. Dev mode label misleading — rename to 'Single model (fast)' vs 'Full ensemble'
   Flagged: 2026-04-25 19:42:01
   Current label: 'Dev mode: 1 ensemble model only'
   Better: 'Fast mode: single model per SSP/period' vs 'Full ensemble (slow)'
   Priority: Low — UX, before merge back into golem

## 9. Gini performance vs exactness tradeoff
   Flagged: 2026-04-27
   Symptom: Gini requires per-column sorted ranks — no exact full matrix
     vectorization is possible. Current apply() approach is the practical
     optimum for exact computation.
   At S=150, 5 keys: ~90s additional simulation time from gini alone.
   Options considered:
     1. Current apply() — exact, ~90s at S=150 (implemented)
     2. matrixStats::colRanks — exact for unweighted only; weighted F_i
        still requires per-column sort, ~60s at S=150
     3. Single sort by row means — approximate (~99.5% accuracy), ~8s at S=150
     4. Accept cost + notify user
   Proposed resolution:
     Add "approximate gini" toggle in Module 2 advanced settings.
     All other 8 methods (mean, total, headcount_ratio, gap, fgt2,
     prosperity_gap, avg_poverty, median) are provably exact —
     fully vectorized with no approximation.
   Priority: Medium — discuss with leadership before implementing

## 10. Results tab 20s delay after simulation complete
   Flagged: 2026-04-27
   Symptom: After "Simulation complete" notification, results tab takes
     ~20s to display plots. profvis shows renderFunc at 28s.
   Root cause: Shiny reactive graph invalidation fires after
     hist_agg_rv/scenario_agg_rv are set — all renderFunc calls
     evaluate simultaneously.
   Options:
     1. Progress notification (partially implemented)
     2. Progressive rendering as each key completes
     3. Lower priority observer for initial plot render
   Priority: High — visible freeze after every simulation run

## 11. Parallelization of scenario aggregation — deferred
   Flagged: 2026-04-27
   Context: compute_scenario_agg() lapply() over independent keys —
     trivially parallelizable with furrr::future_map().
   Deferred: leadership preference not default, needed before golem merge.
   Estimated gain: ~4x speedup on 4-core machine.
   Priority: High — implement before golem merge

## 12. Poverty line and survey weights UI decisions pending
   Flagged: 2026-04-27
   Current state:
     - Poverty line: baked in at simulation time, not reactive post-sim
     - Weights: pre-computed both variants, toggle is instant
     - UI still shows poverty line as editable — misleading
   Decisions needed:
     1. Disable/grey out poverty line after simulation?
     2. Weights toggle label clarification
     3. Reactive poverty line for FGT methods only?
   Priority: Medium — UX confusion, not correctness issue

## 13. DuckDB rolling window stall ~11s/key after materialisation fixes
   Flagged: 2026-04-27
   Current state: Two materialisation fixes applied — per-key cost
     reduced from ~29s to ~11s (~62% improvement).
     DuckDB no longer in top 30 profvis at 5 keys.
   Remaining: roll_exprs_climate window function still causes 0%->16%
     stall. At 10 keys: ~110s DuckDB wait.
   Options not tried:
     1. Materialise rolling windows before delta join (approximation)
     2. Pre-cache weather data per SSP at startup
     3. Accept current performance
   Priority: Low at 5 keys, High at 10+ keys
   SOLUTION: NONE - Exactness and running with full ensembles prefered default.

   ## 14. Potential contradiction in multi-hazard results
   Flagged: 2026-04-27
   Symptom: With multiple weather hazards + multiple SSPs, mean 
     consumption and poverty rate can move in same direction — 
     contradicting economic theory.
   Observed: CIV, 2 hazards, multiple SSPs (Temp, and CMIP6, LASSO selection, hh-level)
   Not observed: Guinea-Bissau single hazard, CIV single hazard/SSP
   Possible causes:
     1. Ensemble member dropout producing non-representative sample
     2. Multi-hazard coefficient interactions producing unexpected signs
     3. combine_ensemble_results() averaging across conflicting directions
   Needs: Economic/climate science review of multi-hazard model results
     before production use with multiple hazards
   Priority: High — affects result validity for multi-hazard runs

   ## 15. DuckDB rolling window approximation — methodology review needed
   Flagged: 2026-04-28
   
   ## Current behaviour
   For each climate scenario key (SSP × period × model), the batch query
   applies the rolling window AFTER adding climate deltas to the base weather:
     roll(base + delta)  — exact, per model, per SSP key
   This requires a separate DuckDB rolling window computation per SSP key,
   partitioned by (model, code, year, survname, loc_id, timestamp).
   At 10 keys × 2 hazards this causes the dominant DuckDB stall (~192s).

   ## Proposed approximation
   Apply rolling window to the BASE series only (already materialised in
   loc_weather_base), then add the delta after:
     roll(base) + delta  — approximate
   loc_weather_base is already computed and materialised before the SSP loop.
   This would eliminate roll_exprs_climate entirely from the batch query.
   Estimated saving: ~100-130s at 10 keys.

   ## Mathematical accuracy
   roll(base + delta) = roll(base) + roll(delta) only when delta is constant
   over the rolling window. In practice:
   
   - Additive perturbations (e.g. temperature):
     delta varies monthly across the window → approximation introduces error
     Error = roll(delta) - mean(delta) = deviation of delta from its window mean
     For smooth climate signals this is small (~0.1-0.5% relative error)
   
   - Multiplicative perturbations (e.g. precipitation):
     roll(base * factor) ≠ roll(base) * factor in general
     Error depends on covariance of base and factor within the window
     Likely larger than additive case
   
   - Window length matters:
     3-month window (current): moderate approximation error
     12-month window: delta smooths out → approximation improves
     1-month window: exact (no averaging needed)
   
   - Variable growing season window (future plans):
     If window length varies by location/season, the approximation error
     becomes location-specific and harder to bound analytically

   ## What needs methodology review
   1. Is roll(base) + delta acceptable for additive variables (temperature)?
      Need to quantify error on real data — suggest comparing exact vs
      approximate output for 2-3 survey countries.
   2. Is roll(base) * factor acceptable for multiplicative (precipitation)?
      Likely needs separate treatment — may need to keep exact computation
      for multiplicative variables only.
   3. How does error change with 3m vs 6m vs growing-season windows?
   4. Could a hybrid approach work?
      Exact for multiplicative, approximate for additive — halves the cost.

   ## Implementation complexity
   Low once methodology approved — replace roll_exprs_climate in batch query
   with a simple join to loc_weather_base + arithmetic delta application.
   No structural changes to get_weather() needed.

   ## Priority
   High for performance (biggest remaining DuckDB bottleneck), but requires
   methodology sign-off before implementation. Do not implement without
   quantitative accuracy assessment on real survey data.

   SOLUTION: NONE - priority to use full ensembles and correct weather data for input.


   ## 16. Residual method change does not update exceedance/hero plots
   Flagged: 2026-04-28
   Symptom: Changing residual method (e.g. original → none) triggers
   chart reload notification but plots appear unchanged.
   Possible causes:
   1. res_sim baked in at simulation time — changing post-simulation
      has no effect since draw_values already computed
   2. Reactive dependency on residual input not wired through to
      exceedance_ribbon or all_series reactives
   3. Display-only formatting issue — plots actually unchanged
      because residuals don't affect point estimates, only draws
   Investigation needed: confirm whether res_sim is simulation-time
   or display-time parameter, and whether UI should disable after run.
   Priority: Low — investigate during UX cleanup pass


 ## Issue #17 — Out-of-Memory Crash on Large Ensemble Runs

**Status:** Open  
**Severity:** High — crashes R session  
**Reproducible:** Yes — 99 keys × 30 yrs × S=30 on survey with ~4,889 households  

### Symptom
Warning: [run_sim_pipeline] predict_outcome() failed:
fixest predict failed: cannot allocate vector of size 84.1 Mb
R 4.5.2 exited unexpectedly: exit code 1

### Root Cause
`predict_outcome()` allocates a large intermediate matrix 
(N households × model matrix columns) during `fixest::predict()`. 
With 99 keys × 30 years × S draws running sequentially, peak memory 
accumulates across keys until R exhausts available RAM and crashes.

### Conditions That Trigger It
- Large survey (N > 3,000 households)
- Many ensemble keys (> ~60 keys = 2+ SSPs × ~30 models)
- High S (draws ≥ 30)
- Complex model (K_s > 20 factor loadings)

### Workarounds (short-term)
- Reduce S to 10–15 for multi-SSP runs
- Run 1 SSP × 1 period at a time
- Use dev_mode = TRUE for testing

### Planned Fix
1. Add `gc()` + `rm()` calls between key iterations in 
   `fct_run_simulation.R` key loop to release memory between keys
2. Chunk `predict_outcome()` predictions into row-blocks 
   (~500–1,000 rows) to cap peak allocation per call
3. Longer term: parallelise key loop with `furrr` using 
   `future::plan(multisession, workers = 2)` with memory-aware 
   worker count — only safe once chunking is implemented

### Related Issues
- Phase 4 speed improvements (parallelisation) deferred until this is resolved

##  DEV ISSUE #18 — Outcome direction inconsistency across transformations
 The exceedance curve, outcome table, and hero plot all assume that
 "rare event" (low exceedance probability) = "bad/extreme outcome".
 This is true for welfare (mean consumption) but REVERSED for poverty rate
 and other negatively-scaled outcomes (higher poverty = worse).

# Affected code:
   - build_row() in fct_sim_compare.R: quantile(vals_in, 1 - RP_LOW[th])
   - all_cols ordering in build_threshold_table_df(): fixed RP_LOW → RP_HIGH
   - enhance_exceedance(): no scale_x_reverse() for poverty outcomes

# Fix (deferred): 
 add an `outcome_direction` parameter ("higher_is_better" /
 "lower_is_better") to build_threshold_table_df() and enhance_exceedance().
 When "lower_is_better": reverse column order in table and add
 scale_x_reverse() to exceedance plot.

# Source: outcome metadata in fct_outcome.R outcome registry.

## DEV ISSUE #19— Diagnostics scenario filter does not mirror Results tab

**Current behaviour:**
The scenario filter panel in the Diagnostics tab (mod_2_03_diagnostics.R)
shows a simplified selector — does not allow selection of individual
climate scenario × year combinations the way the Results tab does.

**Desired behaviour:**
The Diagnostics tab scenario filter should use the same
scenario × year selection table as the Results tab
(mod_2_02_results.R) — allowing users to select specific
SSP × year-range combinations for diagnostic comparison.

**Affected files:**
- mod_2_03_diagnostics.R — scenario_filter_panel() call
- mod_2_02_results.R — reference implementation of scenario × year selector

**Priority:** Medium — affects usability of diagnostics tab
**Risk:** Low — UI change only, no simulation logic affected
**Fix:** Pass the same scenario × year reactive used in Results tab
into the Diagnostics module, or refactor scenario_filter_panel()
to accept a `show_year_range` parameter.