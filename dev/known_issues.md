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
