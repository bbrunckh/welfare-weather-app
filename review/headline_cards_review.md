# Headline ("At a glance") cards: review and implementation plan

Scope: the summary cards at the top of the Step 1, Step 2 and Step 3 Results tabs.

- Step 1: `step1_headline_cards()` in `R/fct_step1_headline.R`
- Step 2: `step2_headline_cards()` in `R/fct_sim_compare.R`
- Step 3: `step3_headline_cards()` in `R/fct_policy_sim_compare.R`
- Rendering: `headline_cards_ui()` in `R/utils_ui.R`; CSS `.headline-card*` in `inst/app/www/`
- Metric direction/units: `R/fct_metric_registry.R`

Status: proposal only. No code has been changed.

## 1. Purpose and design principles

The cards give an at-a-glance answer to the question each step exists to answer, without crowding the page:

- Step 1: what does weather do to welfare, for whom, and how much should we trust it?
- Step 2: how does climate change shift welfare risk, in average and bad years, and how sure are we?
- Step 3: does the policy help, in bad years as well as average ones, who does it reach, and does it hold across climate models?

Principles agreed so far:

1. **Minimal card text.** A card has a label, one headline number with its unit, one short context line, and at most one status flag. Anything explanatory goes in the (i) popover.
2. **Intervals, not verdicts.** Print the 95% CI where one exists and keep any significance language in the (i) popover. Step 1 shows no significant / not significant flag, because a prominent verdict can lead users to discard a result prematurely. Stars were considered and dropped for the same reason. Where there is no CI (for example heterogeneity), the test statistic stays in the card as a plain line (the RIF p-value) or in the popover.
3. **Direction-aware.** Whether a change is favourable or adverse comes from the metric registry (`direction`), never from the sign alone.
4. **One clear question per card, not a fixed template.** Each card answers one question that readers of that step actually ask. The original idea of the same four slots in every step (how big, bad years, who, how sure) was a consistency device, not a user need, and is withdrawn (section 11.8). A conditional fifth card is allowed only when a configuration gives it something distinct to say.
5. **Provenance is not a result.** Simulation counts, observation counts and similar go in a one-line basis strip or the popover, not in a card of their own.

## 2. Findings from the current code and screenshots

| # | Finding | Where |
|---|---|---|
| F1 | Metric direction (`lower_is_better` / `higher_is_better`) exists in the registry but the cards only use a binary "neutral" style. Nothing tells the reader whether a change is good or bad. | `fct_metric_registry.R`, `utils_ui.R` |
| F2 | Step 2 has a coefficient-uncertainty band (`coef_lo`, `coef_hi`) that no card shows. Cards cover weather variability and model spread only, and model spread is evaluated at central coefficients. | `fct_sim_compare.R` |
| F3 | Step 3 card 5 shows "100% positive" when the model range is above zero. For a poverty rate, positive means more poverty, so the label reads as good news when it is bad. | `fct_policy_sim_compare.R` |
| F4 | Step 3 "Resilience effect" headline is the 1-in-20-year resilience subtotal (repositioning + interaction), but the headline does not say so, and the sign is not interpreted. In the sample screenshot, Main -4.48 + Resilience +0.23 = -4.25 on a poverty rate, so +0.23 means the policy makes outcomes more sensitive to bad weather, which the bare "+0.23 pp" does not convey. The card answers a core question (how much does the policy change sensitivity to weather?) and should be kept, with clearer framing. | `fct_policy_sim_compare.R` |
| F5 | Step 1 "Spec robustness" compares specification 1 (no fixed effects) with specification 3 only. Specification 1 mixes in cross-sectional variation, so "Stable" says little about whether the effect survives fixed effects. | `fct_step1_headline.R` |
| F6 | Step 1 RIF card says "poorest 10% vs richest 10%", but the RIF estimate is the effect on the 10th and 90th percentiles of the welfare distribution, not on the households currently at those ranks. | `fct_step1_headline.R` |
| F7 | Step 1 effect card contrast is "+1 SD of X" or "highest vs lowest bin". Neither is in physical units, and bin cut-points are only in the popover. | `fct_step1_headline.R` |
| F8 | Step 2 headline "41.23% vs 42.33%" wraps on two lines and makes the reader subtract. The difference is already computed but sits on the third line. Two decimals overstate precision. | `fct_sim_compare.R` |
| F9 | Provenance occupies card space: Step 2 card 5, Step 3 card 5 ("Across 690 simulations", "4.9M household-years"), and "Sample & fit". | all three |
| F10 | Step 3 reach shows "1,284,449.2 covered-household equivalents": a fractional household count and a long caveat. | `fct_policy_sim_compare.R` |
| F11 | Step 1 fit card reports Within R-squared. Simulations in Steps 2 and 3 predict welfare levels using the fixed effects, so within R-squared describes something the simulations do not rely on, and a low value reads as a bad model when it is normal for weather panels. | `fct_step1_headline.R` |
| F12 | Card heights and wrapping are uneven across all three steps (label rows wrap, info icons drop to a second line). | CSS, `utils_ui.R` |

## 3. Common structure across steps

Four cards per step plus a one-line basis strip. The slot table below is the original proposal; the fixed same-slots-in-every-step requirement was withdrawn after review (section 11.8), and the cards as built are listed in section 11.8.

| Slot | Step 1: weather effect | Step 2: climate risk | Step 3: policy |
|---|---|---|---|
| 1. How big? | Effect of the weather variable, in physical units, with significance flag | Expected change vs historical | Expected policy effect |
| 2. Bad years / tail | Non-linearity: hot-end effect or turning point (where applicable) | 1-in-20-year level and how often it now occurs | Resilience effect (repositioning + interaction) in a 1-in-20 year: how much the policy changes sensitivity to weather |
| 3. Who? | Effect on poor vs rich (RIF) or by moderator | People affected (extra people in poverty, where meaningful) | Reach and targeting |
| 4. How sure? | Survives fixed effects? Model fit | Signal vs noise: model agreement, shift vs variability, estimation | Models agreeing on direction; share of climate-driven change offset |
| Basis strip | N, units, years, fixed effects | scenarios x models x years | simulations, sample rows |

Card anatomy:

- Label: short, plain language.
- Headline: one number with its unit.
- Context line: one short line, for example "41.2% -> 42.3% (historical -> SSP)".
- Status flag (optional): favourable / adverse / uncertain, direction-aware, always worded as well as coloured. For effect cards the flag is "Significant" or "Not significant".
- (i) popover: definition, CI, method, caveats (current popovers largely keep their text).

Existing hooks: the card spec already has a `class` field and the CSS has `.headline-card.neutral`, so status variants are a small extension.

## 4. Recommendations by step

### Step 1: what weather does to welfare

1. **Physical units for the contrast (F7).** Replace "+1 SD of Temperature" with "per +1 SD (= X deg C)" or "per +1 deg C" where the specification allows. Binned: keep "highest vs lowest bin" on the card and the cut-points in the popover.
2. **Significance flag on the effect card, CI kept.** "Significant" / "Not significant" at 95%, computed from the same CI the card shows. The CI stays in the context line. Where the sign is clear, direction colours the card.
3. **Non-linearity card for polynomial and binned weather.** Report where harm begins or the turning point (cf. Burke, Hsiang and Miguel 2015). Fall back to the single-contrast card for linear terms.
4. **Robustness redefined around fixed effects (F5).** Compare specification 2 with specification 3, or report "survives fixed effects". Card text: "Robust" / "Sensitive" / "Not robust" with "n of 3 specifications agree" as the context line. Definitions in the popover.
5. **Plain-language "who" card (F6).** Label it "Effect on 10th vs 90th percentile" and show labelled values. Originally proposed: move "RIF distribution p" into the popover and show a significance flag. Superseded by principle 2: the p-value line stays on the card and no flag is shown.
6. **Fit statistic.** See section 5.

### Step 2: climate risk

1. **Lead with the delta (F8).** Headline "+1.10 pp"; context line "41.2% -> 42.3% (historical -> SSP)". Use one decimal on cards. Colour by direction (F1).
2. **Return-period shift on the adverse-year card.** Headline the 1-in-20-year change and add "now a 1-in-12 year" as the context line, if it can be computed from the simulated years.
3. **One "signal vs noise" card instead of two uncertainty cards (F2).** Candidate content: share of climate models whose change has the same sign as the average ("20 of 22 models"), and whether the shift exceeds historical year-to-year variability. Estimation uncertainty (coefficient band) goes in the popover with a flag if it dominates. Replaces "Range across years" and "Climate-model spread", whose ranges move to the popover and to the existing figures.
4. **"People affected" card for poverty-type metrics only.** Translate pp into additional people using survey weights. Suppress for metrics where it is not meaningful.
5. **Scope tag.** One persistent short line: "Short-run weather response; adaptation not included." Full explanation in the popover.
6. **Extrapolation check (to verify).** Share of simulated household-years whose weather lies outside the estimation sample's observed range. Relevant because future climate can leave the support of the data.
7. Move "Simulation years" and prediction counts to the basis strip (F9).

### Step 3: policy

1. **Keep the Resilience effect card, reframed (F4).** The policy-changes-weather-sensitivity question is core to the app, so this card stays as slot 2.
   - Headline: the resilience subtotal (repositioning + interaction) in a 1-in-20 year, as today, with "1-in-20 year" stated on the card rather than only in the sub-line.
   - Direction-aware flag from the metric registry: "Reduces weather sensitivity" (favourable) or "Increases weather sensitivity" (adverse). In the sample, +0.23 pp on a lower-is-better poverty rate would read as adverse.
   - Context line: the total adverse-year effect and the main effect, for example "total -4.25 pp (main -4.48 pp)". This absorbs the separate "Adverse weather years" card, so the glance row stays at four cards. The full main / repositioning / interaction breakdown stays in the popover and the Decomposition tab.
   - Resilience is currently only available at the tail (1-in-20), not for the average year. If an average-year resilience value is cheap to compute from the same decomposition, add it to the popover for contrast; otherwise leave it out.
   - Where repositioning is not modelled (engine) or interaction is not in the fitted model, the card says which component is missing instead of showing a partial subtotal as if complete.
2. **Direction-aware robustness verdict (F3).** "All 22 models agree: lowers poverty rate", "20 of 22 agree", or "Models disagree". Uses `spec$direction`.
3. **"Offsets X% of climate-driven change" (to verify).** Policy effect divided by the Step 2 climate delta. Needs a check that Step 3's baseline and focus scenario match the Step 2 historical/SSP basis before it is trusted.
4. **Targeting instead of raw reach (F10).** Headline "9.3M people" with the household count rounded and in the context line. For poverty metrics add the share of the poor reached, if the baseline poverty line is available in the frame. Cost per person or per pp only if the transfer budget is available in the scenario (to verify).
5. Move simulation and prediction counts to the basis strip (F9).

## 5. Model fit: what to show

Question raised: since the fitted model drives the Step 2 and 3 simulations, should the cards include a fit statistic, and which?

What the code shows today:

- All reported fit statistics are in-sample. `calc_fit_stats()` gives R-squared, adjusted and within R-squared (linear), McFadden R-squared and AIC (binary), and per-quantile R-squared (RIF). There is no holdout or cross-validated statistic.
- Step 2 and 3 predict levels (fixed effects, controls and weather together) and then aggregate to poverty rates, gaps, Gini and so on. Level metrics depend on how well the model reproduces the welfare distribution, not only the weather coefficient.

Recommendation, in order of cost:

1. **Cheapest: swap Within R-squared for overall R-squared on the card** (linear and RIF-median), and keep Within R-squared in the popover. Overall R-squared includes the fixed effects, which the simulations use, so it matches what the simulations rely on. Show it with N: "R-squared 0.62" with "13,779 observations" as the context line. Binary models: McFadden R-squared as now. No fit "quality" flag, because any threshold would be arbitrary.
2. **Better, new computation: baseline reproduction check.** Compare the model's predicted baseline with the observed survey for the headline quantity, for example mean (or poverty rate) predicted vs observed. This speaks directly to whether Steps 2 and 3 start from the right level. Caveat: the aggregation metric is chosen in Step 2, so on Step 1 this would be limited to the mean (or event rate for binary outcomes), with the full set of metrics offered in Step 2 as a one-line check.
3. **Best, most expensive: out-of-sample fit** (cross-validated R-squared, or leave-one-year-out for panels). It is the most honest fit measure for a model used to extrapolate weather, but needs a new compute path and cost control. Treat as a later, optional item.
4. **Binary outcomes:** add AUC or Brier score only if calibration (already plotted) is not enough. Probably popover-only.
5. **Engine caveats to verify:** RIF R-squared is for recentered influence functions and is not comparable to level R-squared, so the card should say which quantile it refers to or omit it; tree-based engines currently show "tree-based model" with no number and need an alternative (for example out-of-bag or test-set R-squared, if the engine exposes it).

Suggested plan: do (1) now as part of the Step 1 card rework, scope (2) as a small separate piece, and defer (3).

## 6. Cross-cutting items

- **Direction-aware status styling (F1).** One helper that maps (value, metric direction, significance) to favourable / adverse / uncertain. Used by every step.
- **Fixed card count and heights (F12).** Four cards per step; shorter labels; info icon kept on the label line.
- **Unified number formatting.** Pick one rule per metric type (percent, pp, level) and one precision policy (one decimal on cards).
- **Basis strip.** A single compact line component reused by all three steps.
- **Export contracts.** `step1_headline_table()` and `step3_headline_df()` feed export bundles, so changes to card labels or counts change exported tables. Existing tests that read card labels or values need updating together with the code.
- **Shared card constructor.** A single builder used by the three step files so label / headline / context / flag / popover fields are filled consistently. Not a refactor of the engines; it replaces the repeated `list(label=, value=, note=, note_html=, info=)` boilerplate where cards are touched.

## 7. Suggested implementation order

Each phase is independently shippable.

**Phase 1: shared foundation (UI only, low risk)**
- Status-flag helper (direction and significance aware) and CSS variants.
- Fixed card heights, shorter labels, basis-strip component.
- Move provenance (counts, N) out of cards into the strip or popovers.
- Tests: card rendering and any export tests that read card text.

**Phase 2: Step 3 corrections (largest misleading-wording risk)**
- Keep "Resilience effect" (repositioning + interaction) and reframe it: state "1-in-20 year" on the card, add a direction-aware flag, show total and main effect in the context line, and drop the separate "Adverse weather years" card (F4).
- Direction-aware robustness verdict replacing "100% positive" (F3).
- Reach card: round counts, drop the long caveat, shorten (F10).
- Tests: `test-policy-*` and visualization-contract tests that match on card text.

**Phase 3: Step 2 delta headline and uncertainty**
- Delta headline, one decimal, direction colour (F8).
- Single signal-vs-noise card; coefficient band surfaced (F2).
- Scope tag.

**Phase 4: Step 1 translation**
- Physical units, significance flag, CI to popover.
- Robustness around fixed effects (F5), plain-language RIF card (F6).
- Fit card: overall R-squared with N (section 5, option 1).

**Phase 5: new computed statistics (each needs verification against the pipeline)**
- Return-period shift (Step 2).
- People affected (Step 2) and extrapolation check (Step 2).
- Non-linearity / turning-point card (Step 1).
- Offset share and targeting (Step 3).
- Baseline reproduction check (section 5, option 2).

**Deferred**
- Cross-validated or out-of-sample fit (section 5, option 3).
- Cost-effectiveness, if transfer cost data is exposed to the results layer.

## 8. Open questions

1. Significance level: fixed at 95% on cards, or follow any level exposed elsewhere in the app?
2. Should the Step 1 non-linearity card replace "Spec robustness" for non-linear specifications, or is a fifth card acceptable there?
3. Is a status flag on fit wanted at all, or only the number?
4. For tree-based engines, which fit statistic can be offered without a new compute path?
5. Is the transfer budget or cost reliably available to Step 3 results, so that cost per person or per pp is feasible?
6. Step 3: is folding the adverse-year total effect into the Resilience card's context line acceptable, or should Step 3 keep five cards (expected effect, adverse-year effect, resilience, reach, robustness)?

## 9. References for context

- Dell, Jones and Olken (2014), "What Do We Learn from the Weather?", Journal of Economic Literature.
- Hsiang (2016), "Climate Econometrics", Annual Review of Resource Economics.
- Burke, Hsiang and Miguel (2015), "Global non-linear effect of temperature on economic production", Nature.
- Hallegatte et al. (2016), "Shock Waves: Managing the Impacts of Climate Change on Poverty", World Bank.
- Hawkins and Sutton (2009), "The Potential to Narrow Uncertainty in Regional Climate Predictions", BAMS.
- Kolstad and Moore (2020), "Estimating the Economic Impacts of Climate Change Using Weather Observations", Review of Environmental Economics and Policy.
- Firpo, Fortin and Lemieux (2009), "Unconditional Quantile Regressions", Econometrica.
- Bowen et al. (2020), "Adaptive Social Protection: Building Resilience to Shocks", World Bank.

## 10. Implementation status

| Phase | Status | Notes |
|---|---|---|
| 1. Shared foundation | Done | `headline_status()`, status CSS, basis strip, fixed label height, provenance moved out of cards. |
| 2. Step 3 corrections | Done | Four cards; Resilience effect kept and reframed; direction-aware robustness verdict; compact reach. |
| 3. Step 2 | Done, except extrapolation card | Delta headline, return-period shift, signal vs noise, people in poverty, scope tag. |
| 4. Step 1 | Done | Physical-unit contrast, significance flag with CI, fixed-effects robustness, percentile wording, overall R-squared. |
| 5. New computed stats | Done, with two items deliberately not built | See below. |
| 6. Review round 2 | In progress | Step 1 shape card done; see section 11 for decisions, plans and the to-do list. |

Phase 5 outcomes:

- **Offset share (Step 3): done.** `step3_offset_share()`; shown on the Expected policy effect card only when climate worsens the metric and the policy improves it. Uses the Historical row of the paired summary as the no-policy historical baseline.
- **Return-period shift and people affected (Step 2): done in Phase 3.**
- **Binned non-linearity (Step 1): done as popover text** (largest bin, steady change across bins; linear and log outcomes only). Quadratic turning point done in Phase 4. No separate card, to keep four cards.
- **Extrapolation check (Step 2): not duplicated.** It already exists as the weather-support check in the Diagnostics tab (`weather_support_summary()` in `R/fct_sim_diag.R`). It needs the resolved scenario weather, which is read lazily from file references and only when the Diagnostics outputs are visible. Recomputing it for a card would add file reads to every Results render, and putting it in the run would change the async worker payload. The Expected change popover now points to the Diagnostics check. Revisit if the support summary is added to the run output.
- **Baseline reproduction check (Step 2): done.** `step2_baseline_check()` aggregates the observed survey outcome (from `hist_sim$svy`, prepared with `prepare_outcome_df()` and exponentiated for log outcomes) with the same metric function, weights and poverty line as the simulation, and compares it with the mean simulated historical value. It is always stated in the Expected change popover, and flagged in the basis strip when the gap exceeds 2 pp (rates) or 10% (levels). Skipped when no observed outcome is available, for the binary `poor` outcome, and for LCU log outcomes without the PPP factor. The simulated value averages many weather years while the survey is one period, so small gaps are expected.
- **Targeting / share of the poor reached (Step 3): decided against for the card.** Reasons as above; share of population reached stays on the Reach card and the Step 3 Diagnostics tab keeps the eligibility-vs-treatment detail.

Deferred (unchanged): cross-validated fit; cost-effectiveness.

## 11. Review round 2: decisions, audits and plans

### 11.1 Why the baseline check exists

Steps 2 and 3 report levels (a poverty rate, a gap, a Gini) built from model predictions, so any level error in the simulated baseline carries into every figure and into every scenario difference. Weather coefficients can be well estimated while the predicted welfare distribution sits too high or low (for example because of log retransformation, fixed effects absorbing level information, or the tails of the distribution that decide who is below the poverty line). Overall R-squared does not reveal this because it is a statistic about variance explained, not about the level of a threshold metric.

The check puts the simulated historical average next to the same metric computed directly from the survey (same metric function, weights and poverty line). A gap is flagged in the basis strip when it exceeds 2 pp for rates or 10% for levels. The thresholds are working defaults: they are tight enough to catch a miscalibrated baseline that shifts a poverty rate by more than sampling error, and loose enough not to flag the normal difference between a weather-year average and a single survey period. They are meant to be tuned once real runs have been looked at. The flag is advisory; it never blocks results.

### 11.2 Consistency audit (superseded)

The audit compared the built cards with the four-slot template (how big, bad years, who, how sure) and found that only Step 3 matched it fully: Step 1 has a tail card only for non-linear specifications, and Step 2 has no "who" card. The proposed fix, a Step 2 distributional card from the decile incidence, is withdrawn. See section 11.8 for the decision and the reasons.

### 11.3 Decisions recorded

1. Card count: four core cards plus a conditional fifth, not a fixed five. The fixed same-slots-in-every-step template is withdrawn (section 11.8).
2. Step 1 shape card: implemented for polynomial and binned weather (fixest fits; linear weather has no card; RIF is not covered). Polynomial: effect of moving from the median to the 95th percentile with a 95% CI and the turning point. Binned: where the response starts (first bin whose interval excludes zero) with the largest bin and its CI. Computed in the same pass as the existing scenarios (two extra design rows per variable), so the cached card path stays cheap.
3. The Step 2 count card is labelled "Number of poor".
4. Tree-based engines are not implemented in the app yet; their card stubs stay as they are and are out of scope.
5. Downstream consumers checked (section 11.5).
6. No distributional ("who") card in Step 2, and none to be added to Step 3 (it already has Program reach). Step 2's distributional view stays in the incidence-by-decile chart and its export.
7. Step 1 prints no significance verdict; intervals are shown instead (section 11.4A).

### 11.4 Plans

**A. Significance display.** Revised after the visual review: Step 1 prints the 95% CI and no significant / not significant verdict, because a prominent verdict can lead users to discard a result prematurely. Stars were considered and dropped for the same reason. The same principle applies to Steps 2 and 3: print an interval where one exists, and keep any significance language in the (i) popover. Direction colouring stays on Steps 2 and 3 cards that report a change; it is not applied to Step 1.

Status: built for the headline effect cards (section 11.9). In Steps 2 and 3 the interval covers coefficient (estimation) uncertainty only, so label it "95% CI (estimation)" and keep climate-model spread and weather variability as separate statements.
- Step 3: `paired_effect_summary()` already derives a coefficient band from the paired gradient contrast, but the headline summary is built with a 0-1 quantile band, which makes the coefficient limits infinite. Add a `coef_sd` column (non-breaking) and compute center +/- 1.96 coef_sd for the card.
- Step 2: the climate shift needs the coefficient SD of (scenario - historical), not a year-paired contrast (historical and future years are not paired). The deviation-from-historical-mean path (`.apply_contrast_sd()` with `hist_F_agg_ref()`) already carries that contrast SD; reuse it for the headline instead of building a new one.

**B. Extrapolation check (share data from the Diagnostics tab).** The weather-support summary (`weather_support_summary()`) is computed in `mod_2_03_diagnostics` from the resolved scenario weather, and only when that tab's outputs are visible. The Results module is built first and the Diagnostics module takes Results' time series as an input, so the two cannot simply call each other. Plan:
1. Add a shared `weather_support` reactiveVal created in `mod_2_simulation` and passed to both modules.
2. In the Diagnostics module, compute the summary for all selected weather variables (not only the ones ticked in the tab) and write it to the shared value, reusing its existing cache key so the file reads happen once per run.
3. Time the file read for a realistic run. If it is too slow to do eagerly, compute it lazily the first time the Results tab renders and show nothing until it is ready.
4. Use it in Step 2 as a status on the Signal vs noise card: "x% of scenario weather outside the historical range" with a warning flag above 5% (the threshold the Diagnostics tab already uses), plus the variable list in the popover.

**C. Cost-effectiveness (Step 3, detail in Diagnostics). Superseded: see `review/step3_cost_effectiveness_plan.md`; the card version is deferred.** What exists: `.sp_transfer_totals()` returns the annual population cost of the social-protection transfer actually applied (it reads the transfer column the run wrote), and the Diagnostics coverage table already has a `realized_cost` column, populated for social protection only. The other policy levers (infrastructure, digital, labour, education) have no cost input, so cost-effectiveness applies to the social-protection component only and must say so.
- Metrics (poverty-line metrics only, from existing quantities): cost per person lifted out of poverty = annual cost / (-change in headcount x weighted population); cost per percentage point of headcount reduction; for the poverty gap, the share of transferred money that closes the gap (vertical expenditure efficiency). For mean or median welfare: welfare gain per $1 transferred.
- Placement: a conditional fifth Step 3 card, "Cost per person lifted", with the cost-effectiveness table, assumptions and per-scenario values in the Diagnostics tab.
- Caveats to state: the effect is the modelled equal-model-mean change, not observed; cost excludes administration; cost is annual and the effect is an annual average over weather years.
- Verification: confirm the weighted-population and unit (household vs person) arithmetic against `.sp_transfer_totals()`, and that cost does not change across climate scenarios (it should not, since the transfer is fixed).

**D. Households vs people.** Cost per person lifted needs the weighted population from the baseline frame; reuse the same weight convention as the Reach card (weights represent people).

### 11.5 Downstream audit

Checked by search of `R/`, `inst/`, `dev/`, `tests/`, `AGENTS.md`, `README.md` and `NEWS.md`.

- The only programmatic consumers of the card structures are the three export tables: `step1_headline_summary` (`step1_headline_table()`), `climate_headline_summary` (`step2_headline_df()`) and `policy_headline_summary` (`step3_headline_df()`). Their row counts, labels, notes and some values changed as described above. The Step 3 export keeps expected effect in row 1, and the Step 2 export keeps the count/basis card, so the code that reads those rows by position still holds. The `climate_headline_summary` description was updated to match.
- No JavaScript or other module reads card labels or positions. CSS selectors for the new classes live in `custom.css` only.
- "Resilience effect" in `AGENTS.md` and `fct_policy_decompose.R` refers to the decomposition concept and remains valid.
- Tests that read cards were updated; the characterisation snapshot was re-accepted with only the headline-cards value changing.
- The `spelling` package is not installed here, so `tests/spelling.R` has not been run.
- Unrelated failures from another agent's removal of ggplot functions (`plot_step2_adverse_dot`, `enhance_exceedance`, `incidence_chart`, `plot_step3_adverse_dot`) are not part of this work.

### 11.6 To-do list, in order

1. Visual review of all three tabs (in progress); adjust CSS and text from findings.
2. Done: Step 2 and 3 intervals on the headline effect cards (section 11.9).
3. Done: extrapolation check (section 11.10).
4. Deferred: cost-effectiveness. The summary-card version is deferred; the fuller Step 3 results are planned in `review/step3_cost_effectiveness_plan.md`.
5. Tune the working thresholds (80% model agreement for "Robust", baseline check 2 pp / 10%) after reviewing real runs.
6. Deferred: cross-validated fit; share of the poor reached; weighted analyses of non-social-protection cost.

### 11.7 Changes after the visual review

Applied:
- Step 1: removed the "Significant / Not significant" and "Significant difference" flags from the effect, shape and who cards. The CI stays on the effect and shape cards, and the RIF p-value line stays on the who card. Step 1 cards no longer carry a direction colour.
- Step 2: the adverse-year line now reads "Historical 1-in-20 year -> about 1-in-N in <scenario>" and the focus scenario is named on the Expected change, Adverse weather years, Signal vs noise and Year-to-year range cards, as in Step 3.
- Step 2: "Shift = 1.3x year-to-year variability" is now "Shift is 1.3x the usual year-to-year swing" (explained in the popover).
- Step 2: the simulation-years note was using the number of saved scenarios and the focus scenario's model count. It now counts the scenarios, models and years actually present in the simulated table, and states ranges when they differ across scenarios.
- Steps 2 and 3: the number of poor is printed on the first two cards for headcount metrics with survey weights (for example "about 120K more poor"). The Step 2 "Number of poor" card is removed, so the fourth card is always Year-to-year range.
- Step 3: the Resilience effect card no longer repeats "Total ... (main ...)"; both are in the popover. The card still names a missing component (for example "Interaction: not included in fitted model").

Still open from the to-do list: cost-effectiveness, now deferred and planned separately in `review/step3_cost_effectiveness_plan.md`. The extrapolation check is done (11.10).

### 11.8 Decision: no fixed card template, no Step 2 "who" card

After the visual review the current card topics were judged informative as they stand. The fixed same-slots-in-every-step template is withdrawn, and the proposed Step 2 distributional card is not built.

Reasons:
- The template was a design device for consistency, not something readers asked for.
- The distributional question already has a home: Step 1's who card answers it for the weather effect, Step 3's Program reach answers it for the policy, and Step 2 has the incidence-by-decile chart and export.
- A Step 2 card would need per-household values for every scenario and metric, and a bottom-40%-versus-rest split is not a natural summary for several metrics (for example the Gini or the prosperity gap).
- Year-to-year range, the fourth Step 2 card, gives the context needed to judge whether a climate shift matters.

Rule going forward: each card answers one clear question for readers of that step; detail goes in the (i) popover and provenance in the basis strip; a conditional fifth card appears only when a configuration gives it something distinct to say (the Step 1 shape card now, and cost-effectiveness in Step 3 if built).

Cards as built:

| Step | Cards |
|---|---|
| 1 | Effect (with 95% CI); Shape of response (non-linear specifications only); Who is most affected; Spec robustness; Model fit |
| 2 | Expected change; Adverse weather years; Signal vs noise; Year-to-year range; basis strip (scope tag, simulation counts, baseline check flag) |
| 3 | Expected policy effect; Resilience effect; Program reach; Policy robustness; basis strip (simulation counts) |

### 11.9 Intervals on Step 2 and Step 3 headline effect cards

Built:
- Step 2, Expected change: prints "(95% CI: a to b)" for the climate shift (scenario minus historical). The interval is the coefficient (estimation) uncertainty only. It comes from `step2_delta_ci()`, which takes the norm of the difference between the scenario's and the historical mean aggregate gradients (`F_agg_all`), the same construction as the deviation modes' contrast SD. Because the same coefficient draws move both aggregates together, this is the SD of the difference, not the sum of two marginal SDs. It needs the aggregation gradients, so there is no interval when coefficient uncertainty was skipped. Computed for the first future scenario shown on the cards, by `delta_ci_rv()` in the Results module.
- Step 3, Expected policy effect: prints the interval in place of the generic "Policy vs baseline" line (which stays in the popover), from a new `coef_sd` column on the paired effect summary. `coef_sd` averages the per-model coefficient SDs, which is slightly conservative (it is an upper bound on the SD of the equal-model mean).
- No interval on the adverse-year, resilience or robustness cards: a coefficient interval for a quantile or a decomposition component is not available from the current aggregates. Their popovers say component uncertainty is not estimated.
- No stars and no verdict flags (section 11.4A). The popover states what the interval covers and points to the cards that show climate-model spread and weather variability.
- Export: the card objects carry `change_ci_native` (Step 2) and `effect_ci_native` (Step 3); the export tables do not yet include them. The Step 3 paired effect summary gained a `coef_sd` column.

### 11.10 Extrapolation check (weather support)

Built, with a different route than the one sketched in 11.4B. The Diagnostics tab computes weather support lazily from the resolved scenario weather (file reads, only when that tab is visible), and the Results and Diagnostics modules cannot call each other. Instead of sharing a reactive and reading files again, the same support computation now runs inside the simulation while each key's weather is already in memory:

- `weather_support_summary()` was split into reusable pieces without changing its output: `weather_support_reference()` (the historical 1%-99% interval, or supported bins), `weather_support_scenario()` (the share outside) and `weather_support_pool()` (adds member counts across climate-model members and recomputes the share). The Diagnostics tab keeps using `weather_support_summary()` unchanged.
- `fct_run_simulation()` records the historical reference when the historical key arrives, checks every climate-model member against it, and attaches a pooled `weather_support` table to each scenario entry. Checking every member is more complete than the Diagnostics tab, which checks only the first member. The check is best effort: any failure (for example survey cells that cannot be matched) leaves the scenario without a summary and does not affect the run.
- The Expected change card shows the per-variable shares in its popover, and when any variable exceeds the existing 5% rule the basis strip adds "Weather outside historical range: Temperature 12.3% (see Diagnostics)".
- Runs made before this change carry no summary, so the card simply omits it.
- Not changed: the Diagnostics tab still recomputes its own table from files. It could read the stored summary instead to avoid the file reads, which is a possible follow-up.
- Cost: vectorised comparisons on data already in memory, plus one merge to match the historical weather to the survey locations and months, which the Diagnostics tab already does on demand.
