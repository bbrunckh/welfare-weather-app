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
   Priority: Medium — discuss with boss before implementing

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
   Deferred: boss preference not default, needed before golem merge.
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