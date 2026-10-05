# Adverse-Year Definition and Output Alignment Plan

## Status

**Implementation handoff, reviewed against current code on 2026-10-02. No application behavior is changed by this document.** Implement the baseline-anchored definition below. This is an intentional estimand change, not a numerical optimization of the existing state-specific quantile contrast. The user will hand this document to Luna (High) in a separate session; this review does not start that session.

The goal is to make Step 2, Step 3 metric-aware Results, Step 3 metric-aware decomposition, and the retained technical decomposition use one comprehensible adverse-year definition. Technical channel values remain on the fitted model scale; alignment concerns which model-year ranks and interpolation weights are evaluated, not a conversion of model-scale values into the selected metric.

## User Question and Estimand

The adverse-weather question is:

> Under the selected Results metric, how does the policy change outcomes at weather conditions that are adverse in the no-policy baseline?

For each SSP climate model independently:

1. Calculate the selected metric for every valid **baseline scenario-model-year** using the same survey population, survey weights, outcome transform, residual realization/mode, threshold and metric eligibility rules used by Results.
2. Order those annual baseline metric values in the metric's registered adverse direction. Examples: low mean welfare is adverse; high poverty rate is adverse. Never infer direction from a label or a generic weather sign.
3. Use the canonical rank-interpolation convention to locate the requested 1-in-N baseline quantile. This gives one or two adjacent baseline-year ranks and interpolation weights. It is not necessarily a single observed year.
4. Apply those exact baseline-derived rank weights to the corresponding annual values for **baseline, after-main, after-repositioning, policy, and technical model-scale channels**. Do not re-rank years for later cumulative states or by policy effect.
5. Difference adjacent weighted state values to obtain main, repositioning, and interaction. Average the resulting per-model state values and contributions equally over the baseline run's required SSP models. If any required model fails support, the scenario/return-period contrast is unavailable; do not reduce the ensemble to successful models.

Historical is not an SSP ensemble: report its one historical baseline-anchored quantile without a model-ensemble reduction. For SSPs, climate models receive equal weight regardless of their retained year counts. Survey weights remain inside each annual metric; they are not model weights.

This deliberately replaces the current equal-probability/state-specific quantile contrast: that method calculates a separate adverse quantile for every cumulative state, so its states may use different year ranks, and it centers the ensemble with a median. Under this proposal, the selected adverse rank is determined once by the baseline selected metric. The policy contrast is therefore a baseline-anchored adverse-weather effect, not the difference between independently ranked baseline and policy outcome distributions.

## Terminology Contract

Use these terms consistently in cards, charts, tables, popovers, CSVs, and bundle exports:

- **Adverse 1-in-20 year (baseline-anchored)**: shorthand UI label for a rank-interpolated tail of the selected metric's annual no-policy aggregate within each scenario model.
- **Baseline-anchored adverse quantile**: full method name for technical notes and export metadata.
- **Baseline annual selected-metric quantile**: the value that defines the ranks and interpolation weights.
- **Interpolated baseline-year support**: the adjacent year ranks and weights used to evaluate each state. Do not call it a single selected observed year when interpolation uses two ranks.
- **Equal-model mean**: SSP ensemble center for the adverse states and their contributions. It is distinct from Step 2's retained median-based diagnostics unless those diagnostics are explicitly brought into alignment.
- **Selected metric scale**: native outcome/metric levels and contributions (for example currency/day or percentage points).
- **Technical model scale**: original fitted outcome scale (including log-outcome units where applicable); these are not contributions to the selected metric even when they use the same year support.

Avoid the unqualified phrases “adverse weather years,” “same adverse years,” and “1-in-20 event” when the displayed result is an interpolated quantile. The compact card label may remain “1-in-20 year” as requested, but its info popover must state the baseline anchor, interpolation, metric direction, and that the result is not a weather-loss-avoided estimate.

## Output Contract

### Step 2

- Continue to aggregate annual no-policy predictions by scenario, climate model, and simulation year before calculating adverse outcomes. Do not rank households or raw daily weather observations.
- Use the selected metric and its registered adverse direction to identify the within-model quantile. Preserve canonical survey weights, transform, threshold, residual, and annual-metric eligibility behavior.
- For an SSP adverse headline, compute a baseline annual quantile per model, then use the **equal-model mean** across model quantiles. Show the center in the card/popover/export. Historical remains the single historical estimate.
- Preserve the minimum of N finite baseline annual values per required model for a 1-in-N request. Fix the current shared-matrix-column-count gate and silent `na.rm` model exclusion. Never clamp an unsupported request to the most extreme observed year.
- Align every Step 2 adverse point estimate/marker that claims a 1-in-N annual outcome with this baseline annual selected-metric quantile and equal-model center. Distribution curves and climate-model spread intervals may retain their existing distribution/median operators, but must be described as ensemble-distribution summaries rather than a different adverse-year selection rule.
- Step 2 threshold tables may currently store deviations from a historical reference rather than absolute levels. Keep that display contract if needed, but retain/use the absolute per-model annual metric quantile as the support/parity source; never compare a Step 3 absolute baseline level to a Step 2 historical deviation.

### Step 3 Results Cards and Metric-Aware Tables

- The adverse card's baseline level is the baseline-anchored metric quantile. The policy level applies the same baseline rank weights to policy annual metric values. The displayed policy-minus-baseline total is their difference.
- The resilience card reads the same 1-in-20 baseline-anchored row as the adverse card. Show the main package, repositioning, interaction, and resilience subtotal; preserve engine/term-unavailable states rather than substituting zero.
- Compute each model's weighted state and adjacent contributions using the baseline quantile's year-rank weights; then average those values equally across all required SSP models, only if all are available. This makes all states and components additive by construction, including with unequal retained years across models.
- The baseline level must match Step 2's 1-in-20 baseline level when scenario, selected metric, population, threshold, residual settings, model/year support, and baseline snapshot match. State any scope difference rather than forcing parity.
- Do not calculate policy's own 1-in-20 quantile for this baseline-anchored contrast. It is a different estimand and may be retained only as a separately named distributional statistic if deliberately requested later.
- Align the Step 3 adverse-dot endpoints, return-period decision table, paired adverse-effect table, and their exports, not only the cards/decomposition table. Baseline endpoints come from shared baseline support; policy endpoints and effects apply that support. Unavailable channel attribution must not revive the legacy independent-quantile card/chart fallback.

### Step 3 Technical Decomposition

- Retain its model-scale interpretation, formulas, central/SE caveats, and technical channel names.
- Replace observed-year pickers and policy-effect-ranked compact future rows with the shared baseline metric's per-model adverse rank/interpolation weights.
- For every available model-scale cumulative state/channel, apply the same within-model baseline rank weights and then take the equal-model mean across SSPs. Existing channel-only sources need not manufacture absolute cumulative levels. The technical values are rank-aligned diagnostics; they remain in fitted model units and must not be presented as selected-metric dollar/pp contributions.
- Remove any technical statement that implies a single observed adverse year if two annual ranks contribute through interpolation. If the UI displays member/year support, show both year keys and weights.
- Historical technical diagnostics use the historical selected-metric baseline quantile and its interpolation weights; do not select years by raw `y_point` mean unless that is exactly the selected metric under the production transform/residual semantics.
- Use production prediction-row annual technical channels for adverse historical and future summaries. Retain the existing raw-weather mean technical diagnostic and its scope/SE caveats as a distinct Mean view. Do not blend raw weather panels before evaluating nonlinear technical kernels.

### Charts and Exports

- All adverse plots and exports identify `adverse_basis = baseline_selected_metric`, metric ID/direction, probability/return period, quantile algorithm, scenario/model identity, model/year support, and interpolation year-rank keys/weights.
- Include `ensemble_center = equal_model_mean` for SSP headlines and distinguish the single historical estimate.
- Metric-aware contribution data carry native metric values/units and `scale = metric_aware`; technical channel data carry model-scale units and `scale = model_scale`.
- Export metric-aware baseline, after-main, after-repositioning, and policy tail values plus adjacent main/repositioning/interaction/resilience/total contrasts on identical support. Export native technical channels on that support; include technical cumulative levels only where available. Keep displayed values separate from native numeric columns.
- Disclose that the outcome-tail quantile does not identify one universally shared weather field/date across geographically varying household exposures. The rank is over annual **population aggregate outcomes** within a scenario model.

## Current-Code Alignment Map

Current paths that the implementation handoff must reconcile:

- `R/mod_2_02_results.R`: constructs model-by-year annual metric matrices, uses `by_model_rp_matrix()`/`rank_interp()` within each model, then takes a median across SSP model quantiles for adverse thresholds. This is already metric-aware at the annual aggregate level, but its current ensemble center differs from the proposed equal-model mean.
- `R/fct_policy_metric_decompose.R::.policy_metric_tails()`: annual metric aggregation is correct in scope (scenario-model-year), but it currently re-ranks each cumulative state separately and takes median model quantiles. Replace this with baseline-derived interpolation indices/weights reused across all states, plus equal-model aggregation.
- `R/fct_policy_sim_compare.R::step3_headline_cards()`: adverse and resilience cards consume the Results-owned tail row. Keep them sourced from one row/operator; do not independently reconstruct tail values in the UI.
- `R/mod_3_09_decomposition.R`: technical historical adverse selection currently chooses a discrete observed year from weighted mean baseline `y_point`; compact future selection currently orders model/year rows using `delta_total`. Both diverge from this contract and need replacement with shared baseline metric rank support.
- `R/fct_uncertainty_helpers.R::by_model_rp_matrix()`: current Step 2 interpolation returns values but not reusable rank/index weights. A shared helper should expose interpolation support so all states can reuse the exact baseline ranks without duplicating quantile formulas.
- Step 2 currently takes median model quantiles for future adverse central values. All adverse point estimates/markers are in scope for alignment; only uncertainty/spread summaries may retain separate median/distribution conventions, with explicit labels.
- `R/fct_policy_sim_compare.R::.wire_results_pane()`: independently constructs Step 3 threshold rows, adverse-dot endpoints, `paired_adverse_table_rv()`/`paired_adverse_effects_rv()`, and export descriptions. These are additional legacy-quantile paths to replace. `R/fct_sim_compare.R::paired_adverse_effect_table()` is the current paired table operator.
- Existing Results-to-technical wiring is `.wire_results_pane()$metric_decomposition` -> `mod_3_07_results_server()` -> `mod_3_scenario.R` -> `mod_3_09_decomposition_server()`. Extend this API with reusable support; do not create another settings authority.
- `R/fct_policy_sim.R::apply_policy_delta_to_baseline()` invokes `.apply_policy_annual_pipeline()` with scenario/member keys for future members, but currently without them for historical, so historical produces no compact annual summary. `R/mod_3_06_policy_sim.R` currently prepares/caches raw-weather adverse bases at run time. Both paths need deliberate updates for live metric-based historical support.

## Calculation Details and Guardrails

### Interpolation Support

For each scenario/model, calculate a canonical support record from the sorted finite baseline annual metric values:

`model_id`, requested adverse probability `p = 1/N`, adverse-tail direction, lower/upper sorted ranks, lower/upper simulation-year keys, interpolation fraction, retained annual-year count, and status/reason.

Apply the support record as the same convex combination to each annual state vector in that model. For a state vector `v`, its tail value is `w_lo * v[year_lo] + w_hi * v[year_hi]`; when the interpolation lands exactly on one rank, only that year has nonzero weight. This preserves linear additivity of the adjacent contributions. It does not assert that the policy-state annual outcome at those ranks is itself at its own 1-in-N quantile.

The canonical formula in `rank_interp()` is **not** R's default `quantile()` convention. Sort finite baseline values ascending; define `q_exceed = p` for the high adverse tail and `q_exceed = 1 - p` for the low adverse tail. Then `k = n * (1 - q_exceed) + 0.5`, `lo = floor(k)`, `hi = ceiling(k)`, `w_hi = k - lo`, and `w_lo = 1 - w_hi`. For an exact rank, store the same year/rank in both slots with weights 1 and 0. Reject ranks outside `[1, n]`; do not clamp. With `1:20` at 1-in-20, the low value is 1.5 and the high value is 19.5.

**Tie decision:** sort by baseline value ascending, then numeric `sim_year` ascending. Reject duplicate or missing scenario/model/year keys. Do not rely on input row order, lexical year ordering, or random ties. This preserves one/two-year support and baseline quantile values, but policy effects at tied baseline values can depend on the documented year tie-break. Export `tie_method = value_then_sim_year_ascending`; do not average tied blocks in this implementation.

For complementary probabilities in the existing Step 2 threshold table, enforce `n >= ceiling(1 / min(p, 1 - p))`, with at least two finite years. For the adverse probabilities `RP_LOW`, this is `n >= ceiling(1 / p)`. Keep the probability-0.5 central-year statistic separate from the expected annual mean. Never accidentally interpret the table's `1:1` label as requiring only one year.

### Missingness and Support

- Baseline metric validity determines the selected rank support. Every cumulative state and technical channel must have valid values at every year with nonzero baseline interpolation weight; otherwise mark that model's decomposition unavailable with a reason.
- Do not independently drop years per state, renormalize weights differently across channels, replace unavailable values with zero, or reduce the model ensemble silently.
- If a model lacks the retained baseline support needed for the requested quantile, report unavailable under the documented minimum-year rule. The 1-in-20 threshold must not be manufactured by clamping.
- Record model and year counts, per-model support, dropped models/years, parity error, requested/effective residual modes, and the exact metric/threshold snapshot.
- The required model universe comes from the baseline run's pipeline/member identities, not from whichever models survive an annual-table filter. Retain support/status records even for zero-finite-year models. Show failed model IDs and reasons; successful-model counts alone are insufficient.
- Baseline nonfinite annual metric values are excluded before ranking and counted explicitly. A finite baseline year must not be removed merely because a policy/intermediate state is unavailable. Missing selected state/channel values invalidate that contrast, not the independently valid Step 2 baseline estimate.
- Invalid/unmatched year keys, duplicate annual keys, no required models, and unknown run identity are explicit unavailable/error states. An exact-rank application ignores its zero-weight slot, avoiding `0 * NA` contamination.
- Preserve `metric_metadata()` as the direction authority, including its shipped unknown-direction low-tail fallback. When `direction_known = FALSE`, carry `adverse_note` and label the result as a low-tail statistic with direction unknown, not as a confirmed adverse outcome. Do not introduce a new direction heuristic or silently change registry behavior.

### Why Equal-Model Mean

The plan adopts equal model weights for the SSP headline: first produce one baseline-anchored adverse value/contribution per climate model, then average those model results equally. A model with more valid years must not receive extra ensemble weight. The same equal weights must be applied to all cumulative levels and adjacent contributions so the ensemble rows remain additive.

Step 2 currently uses the median of model quantiles for its adverse headline, unlike its expected headline's equal-model mean. Aligning Step 2's adverse headline to the proposed equal-model mean is required for exact Step 2/3 baseline parity. If the median is retained for any other chart, label it explicitly as a different secondary ensemble center.

## Proposed Implementation Phases

1. **Characterize current operators.** Add small fixtures with unequal per-model year counts, low/high adverse directions, nonlinear metrics, interpolated ranks, and deliberately different state-specific rankings. Prove that baseline-rank reuse and independent state quantiles differ as expected.
2. **Centralize adverse support.** Add a pure helper near `rank_interp()` that returns quantile value and interpolation year indices/weights under one canonical convention. Preserve existing helper contracts or update all callers deliberately. Test exact-rank, interpolated-rank, support edge, tie ordering, nonfinite values, and metric adverse direction.
3. **Align Step 2.** Reuse the helper for annual metric thresholds. Change the SSP adverse headline ensemble center to equal-model mean for parity with Step 3. Keep unrelated median charts unchanged and labeled. Verify Step 2's no-policy adverse annual values and baseline snapshot inputs.
4. **Align metric-aware Step 3 tails/cards.** Derive support only from baseline annual selected-metric aggregates for each scenario model. Apply the same support to all cumulative states; summarize models equally; wire the adverse and resilience cards to that shared result. Verify component telescoping and Step 2 baseline parity.
5. **Align technical diagnostics.** Carry the same support records into historical and compact future technical summaries. Apply support to technical model-scale annual channels without relabeling them as metric-aware. Remove selection by mean `y_point` and by `delta_total`. Retain Mean/1-in-10/1-in-20 technical controls; each adverse probability uses the same Results-owned support in all technical views, and the summary table's retained 1-in-5 row uses that operator too. Mean remains the existing diagnostic, not a rank-supported adverse view. Results cards remain fixed at 1-in-20.
6. **Align outputs and exports.** Update table/card/chart labels, tooltips, CSV and bundle metadata; include year-rank support/weights and distinct scale fields. Remove obsolete terminology and update handoffs.
7. **Verify.** Run focused unit/module/export suites, then full tests. Compare Step 2 and Step 3 baseline values at identical run scope; check zero-policy zero contributions; compare reference and optimized rank application; verify historical plus multi-SSP, log and identity outcomes, mean/median/poverty metrics, high/low adverse direction, missing years, and threshold edits. Bench real data and inspect UI interactively before release.

## Acceptance Criteria

- One canonical annual metric adverse-support helper defines ranks, interpolation, tie behavior, support, and direction for every aligned output.
- For each SSP model, the 1-in-20 support is selected from baseline annual aggregate values under the live Results metric snapshot and reused unchanged for every state/channel.
- SSP values use an equal-model mean, not pooled model-years or a median, for the aligned headline outputs.
- Step 2 and Step 3 baseline adverse levels agree at identical scope/settings/support.
- For each model and for the equal-model ensemble, `main + repositioning + interaction = total` within the named tolerance; `resilience = repositioning + interaction`.
- Resilience and adverse cards read the same 1-in-20 result; no separate UI quantile calculation exists.
- Technical outputs are clearly model-scale while using exactly the same baseline metric year-rank support.
- No technical path selects an adverse year using only raw outcome mean or `delta_total`.
- Exports reveal metric/direction, baseline anchor, quantile/center methods, per-model year-rank weights, scale/units, support counts, unavailable reasons, and run identity.
- Unsupported thresholds or mismatched state/channel support are visible as unavailable; there is no silent clamping, filtering, reweighting, or stale result fallback.

## Decisions for Implementation Handoff

This proposal assumes equal-model mean for all aligned SSP adverse point estimates and requires Step 2 adverse headline/marker alignment. Climate-model spread intervals and full distribution charts are not themselves the adverse-year center and may retain their existing uncertainty/distribution operators if distinctly labeled.

This proposal does not change model fitting, policy targeting, annual prediction construction, residual method, policy-channel formulas, or the metric definitions themselves. It changes adverse support/ensemble reduction and the technical views' use of that support.

### Shared Helper and Result Contract

Use small package-internal pure helpers adjacent to `rank_interp()` in `R/fct_uncertainty_helpers.R`. Suggested names below are implementation guidance, not a requirement to create a large new abstraction:

- `adverse_year_support(values, sim_years, probability, adverse_tail)` returns one scalar record: `status`, `reason`, `baseline_value`, `probability`, `return_period`, `adverse_tail`, `n_years_total`, `n_years_finite`, `n_years_excluded`, `min_years`, `rank_lo`, `rank_hi`, `year_lo`, `year_hi`, `weight_lo`, `weight_hi`, `quantile_method`, and `tie_method`. Validate lengths/keys/probability/direction before sorting. Keep unavailable records with typed NA numeric fields.
- `apply_adverse_year_support(values, sim_years, support)` matches by year key, not positional row index. Require a unique match and finite value at every nonzero-weight year; return value plus status/reason. This same operation serves metric states, technical channels, and decile annual channels.
- Scenario-level callers add `scenario`, `model_id`, the metric/settings snapshot and run identity. Build one support row per required model/probability, including failed models. Build per-model supported state/contribution rows before the scenario equal-model reduction.
- Preserve `rank_interp()`'s value-only contract for unrelated distribution callers. If extending `by_model_rp_matrix()`, preserve its `rp`/`sd` matrices and models-by-RP shape, including one RP and one model cases; add support/status deliberately. The annual support rule belongs to aligned annual callers and must not unexpectedly change unrelated distribution diagnostics.
- Expose `adverse_support` and `adverse_by_model` alongside existing `annual`, `summary`, `return_period`, `scenarios`, and `metadata` fields in the Results-owned decomposition object. Per-scenario objects expose the corresponding tables too. Keep failed scenario/probability records in the top-level adverse tables, rather than losing them through the existing `good <- Filter(...)` aggregation.
- Set adverse rows to `scope = baseline_anchored`, `adverse_basis = baseline_selected_metric`, `center_method = equal_model_mean` for SSPs or `single_historical` for historical. Retain `quantile_method = rank_interp_n_p_plus_half` if keeping the current machine identifier, but document the exact formula above. Replace every filter on `scope == equal_probability`; do not preserve that misleading alias.

### Baseline Authority and Parity

The current `.policy_metric_decomposition()` filters annual rows to paired finite baseline/policy endpoint support and then checks both baseline and policy against their own marginal thresholds. That sequence cannot implement the new estimand.

1. Obtain the baseline annual absolute selected-metric series and required members from the baseline Results endpoint aggregation/run, before paired endpoint filtering. Use `by_model_matrix(endpoint_series_baseline[[scenario]]$out)` where supplied; pure-test callers without endpoints can use the full unfiltered baseline annual calculation. In either case record provenance.
2. Validate each available reconstructed annual baseline metric against the canonical baseline endpoint value under the same metric/settings snapshot. Derive support from baseline only. Preserve the support even if a downstream policy/channel calculation fails.
3. Keep the existing paired model/year population for **expected** summaries where appropriate. Do not let its mask select adverse ranks. Keep a full annual channel/state lookup for support application; do not discard non-paired baseline years first.
4. Apply selected support to cumulative states. A policy/intermediate value absent at a selected year makes the adverse contrast unavailable. A missing policy value at an unselected year must not change the baseline quantile; preserve expected-summary missingness behavior separately.
5. Replace the current two-marginal-threshold parity assertion. Compare only the baseline supported level to the absolute baseline quantile and compare supported policy/state endpoints to their corresponding annual values. The new policy level is deliberately **not** policy's marginal quantile.
6. Use the existing `.policy_metric_tolerance = 1e-8` relative tolerance (`abs(error) <= tolerance * max(1, abs(reference))`) for baseline parity and telescoping. Record per-model and ensemble errors/status.
7. Step 2/3 parity is conditional: compare identical scenario/model/year keys, population/weight mode, transform, metric/threshold/analysis unit, residual realization and baseline snapshot. Separate tabs need not have synchronized controls. Export these snapshots and a parity status/reason; do not force matching values when their scopes differ.

Explicitly remove the current scenario-wide all-state finite gate before `.policy_metric_tails()`. Where `.policy_metric_pipeline()` fails before it can emit annual rows, retain independently calculated canonical baseline support and a scenario/channel failure reason. A missing intermediate metric at an unselected year must not invalidate an otherwise computable adverse contrast. Fundamental run/exposure/production reconstruction errors may still invalidate channel calculations, but must not erase the baseline support record or alter its ranks. Keep expected-summary availability separate from adverse-tail availability: cards/technical consumers must look up the tail/support status directly, not require a valid expected `summary` row first. Pure callers without endpoint series still need baseline aggregation independent of successful downstream state/channel evaluation.

### Results Consumers and Uncertainty

- Step 2 consumers include `threshold_table_rv()`, `step2_headline_cards()`/`step2_headline_df()`, `step2_adverse_dot_data()`, the threshold table/CSV, and `climate_headline_summary`/`climate_adverse_return_periods`. Retain absolute native point values and support independently of `value = absolute - hist_ref` display deviations.
- Replace the misleading displayed `Central (P50)` label for aligned adverse points with an equal-model-mean label. Update string-based filters in both compare helpers/modules, table pivots, tests, and exports together. Do not change unrelated median-based distribution plots. If an internal lookup key is retained to minimize edits, it must not leak as a P50 claim in the UI/CSV.
- Step 3 consumers additionally include `.wire_results_pane()`'s threshold builder, `step3_adverse_dot_data()`, paired adverse table/effect export builders, headline export metadata, and `step3_headline_cards()`. Replace their adverse rows with Results-owned supported baseline/policy/effect values. Preserve expected rows and unrelated distribution plots. Remove independent quantile fallbacks for an aligned adverse card, including the prefilled `eff_10`/`eff_20`/`eff_50` values when a metric tail is unavailable.
- Aligned Step 3 contribution rows remain **central only**. Do not attach old independent-policy/baseline quantile coefficient intervals to the new supported contrast or invent component SEs. Per-model supported point values may supply explicitly labeled climate-model spread, using the same complete required ensemble. Do not describe that spread as uncertainty of the equal-model mean.
- Step 2 coefficient-SD-at-rank and pooled interval formulas are existing diagnostics, not newly derived uncertainty estimators for an equal-model mean. Preserve them only with explicit diagnostic labels/caveats, or hide unsupported intervals; do not divide by model count or assume climate-model independence. Point support does not fail merely because coefficient SD is unavailable. With ensemble bands off, suppress their row/marker rather than calling the equal-model mean `Ensemble P50`.
- Missing or unsupported results render/export as unavailable with reasons and NA native values. Do not display stale numeric values, zero contributions, or an interval without a valid point estimate.

### Technical Data and Reactive Lifecycle

- Reuse `.apply_policy_annual_pipeline()`'s compact per-year channel/decile reductions for historical as well as future. In `apply_policy_delta_to_baseline()`, pass explicit historical scenario/member identities and publish its small annual compact tables with the run-owned bundle. Keep historical separate from the future scenario selector. Ensure year/model keys agree with the Results-owned historical support (the existing historical member key is `Historical`).
- For each annual channel, calculate `sum_delta / weight_delta` within that model/year first. Apply the baseline support to these annual means, then average model results equally. Do **not** interpolate sums and denominators then divide, pool selected-year weights, or pool model rows via `.compact_future_combine()`; these yield different operators when weights vary.
- Apply that same population-level support to every baseline-decile channel table. Do not select adverse years separately within deciles. Preserve baseline-decile membership; make unavailable selected decile/model support explicit. Define household/population count columns as support diagnostics rather than summing two year panels and claiming unique households.
- Retain channel identities `delta_main`, `delta_sp`, `delta_main_covar`, `delta_res1`, `delta_res2`, `delta_res`, and `delta_total`. Add cumulative technical levels only if actually available from the same annual source; do not fabricate them from metric-aware levels. For technical exports, annual supported channel effects and their additivity are sufficient where the legacy source has no absolute technical state levels.
- Perform interpolation/equal-model reduction on **native model-scale values**. Any existing `log_effect_to_percent()` presentation occurs only afterward and remains explicitly a transformed technical diagnostic, not a selected-metric contribution. Nonlinear percentage transformations are not additive; assert telescoping on native channels, not transformed percentages. Preserve existing log/identity applicability restrictions and mean-view SEs; adverse channels are central only unless a separately validated covariance-aware method exists.
- Remove raw-outcome adverse preparation from `.prepare_decomp_adverse_bases()` and the run-time adverse-panel cache as an adverse-output authority; retain raw mean context/preparation needed by the unchanged mean diagnostic. Audit `.build_decomposition_context()`/`.finalize_decomposition_context()` consumers before removing fields. Legacy data-frame adverse selectors must take shared support or return unavailable, never select on `delta_total` or fall back to all years.
- Current Results metric, threshold and residual settings own support, computed for every available scenario (including Historical), model and required adverse probability. The Results focus scenario selects its cards, not the entire support universe. Independent technical scenario controls and multi-scenario headlines retrieve their matching scenario/probability records. Run identity/correction version own channel values. Metric/threshold edits must recompute support and all adverse technical views without rerunning the policy simulation or constructing household-by-year caches. Reject mismatched run/context keys. Pending/failed recomputations do not reuse the previous support as if current.
- Keep the technical Mean/1-in-10/1-in-20 controls. The headline and decile selectors may remain independent controls, but both call the same support/application operator at their selected adverse probability. The summary table's adverse 1-in-5/10/20 rows use that operator for each probability. Mean retains the existing historical raw-weather mean and future mean-summary operators with explicit scopes; it does not use adverse rank support and is outside this estimand change. Metric changes update every adverse view; Results adverse/resilience cards remain at 1-in-20.
- Only small annual/support tables cross reactive boundaries. Reuse current bounded preparation/context caches, atomic publication, and weather-store leases. Follow `review/optimization_guidelines.md`; do not introduce a new worker pool, unbounded cache, full household/year retention, or synchronous reruns on every control edit.

### Export Contract

Preserve existing export registry keys, particularly `policy_decomposition_headline_data` and `policy_decomposition_summary`, while adding native values/provenance and correcting descriptions. Add separately registered normalized support/per-model adverse tables for Steps 2 and 3, with descriptive keys such as `climate_adverse_support`, `policy_adverse_support`, and `policy_adverse_by_model`. Keep changes in `wise_export_table()` builders so browser CSV and bundle CSV share the same data contract.

Required scalar CSV fields, either directly on a row or joined by explicit keys to its accompanying support table:

- Identity: `scenario`, `model_id` for model rows, run identity/baseline snapshot, policy correction version, metric ID, threshold (NA when unused), analysis unit, transform, weighting/population scope, requested/effective residual modes, direction/direction-known and adverse tail.
- Operator: `adverse_basis`, `scope`, probability/return period, `quantile_method`, `tie_method`, `center_method` and `ensemble_center` (consistent values), `scale`, native units, technical presentation transform if any.
- Support: lower/upper year keys/ranks/weights, required/supported model counts, total/finite/excluded year counts, minimum years, failed model/year diagnostics, baseline quantile and baseline parity status/error.
- Values: native supported cumulative metric states and main/repositioning/interaction/resilience/total; native technical channels where available; formatted display columns separate. Unavailable rows retain identity/provenance with `status`, `reason`, and NA values.

Do not repeat opaque list columns in CSV or imply one shared year across models. Scenario-level rows reference the support table by scenario/probability/run/settings identity; model rows contain the scalar support fields. Update bundle manifest/descriptions and popovers to explain baseline annual population aggregates, equal-model centering, tie-breaking, scope distinctions, and model-scale versus metric-aware values.

### Verification Handoff

Update tests that deliberately characterize the old estimand rather than making new code satisfy both definitions. Relevant suites include:

- `test-fct-policy-metric-decompose.R`: currently asserts separate-state quantiles/median center and marginal policy-threshold parity. Replace those expectations with baseline reuse, equal-model centering, and baseline-only parity.
- `test-visualization-contracts.R`, `test-mod_3_07_results.R`: direction/rank fixtures, one-RP matrix shape, card sourcing, scope filters, adverse-dot/table/export native values and updated labels.
- `test-policy-decomposition-uncertainty.R`, `test-w3-b-future-decomposition-characterization.R`: currently expect observed-year/effect-ranked technical selection. Replace adverse expectations while preserving mean technical channel/SE and registry-key contracts.
- `test-export-bundle.R`: support/per-model CSV round trip, typed NA unavailable rows, metadata/units and stable existing registry keys.

Minimum additional fixtures: exact rank and interpolated rank; low/high metric direction; tied baseline values with shuffled input and deterministic year tie-break; duplicate keys; N versus N-1 finite years; a baseline model with no finite years; nonfinite selected state versus unselected state; unequal model/year/household weights; policy-state rank reversals; nonlinear poverty/median metrics; log/identity outcomes; zero-policy effects; unknown metric direction; historical plus multiple SSPs; selected deciles; metric/threshold/residual edits without rerun; stale run/context; Step 2 absolute/deviation separation; unavailable cards without legacy fallback. Compare a simple reference support application to the implementation and assert native telescoping at the named tolerance.

Suggested commands from the repository root after implementation:

```sh
Rscript -e 'devtools::test(filter = "fct-policy-metric-decompose|visualization-contracts|mod_3_07_results|policy-decomposition-uncertainty|w3-b-future-decomposition-characterization|export-bundle")'
Rscript -e 'devtools::test()'
```

Run any additional suites affected by helper/caller changes. Inspect the app's Step 2 and Step 3 cards, adverse dots, technical controls, CSVs and bundle at matching scope, then after a poverty-line edit. Check BFA and IRN real-data timing/memory using existing harnesses, especially the new historical compact reduction and multi-SSP path; report unavailable data/UI infrastructure or existing test failures rather than claiming validation. No deployment, commit, or new Agent Manager session is part of this document review.
