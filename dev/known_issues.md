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
