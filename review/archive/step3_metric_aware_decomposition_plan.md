# Step 3 Metric-Aware Policy Decomposition Plan

## Status and Objective

Finalized implementation handoff, reviewed against the current code on 2026-10-01. This document changes no application behavior. Implement only the bounded phases below, not the deferred research items.

Implementation status (2026-10-02): **Phases 1-7 implemented; release verification remains incomplete.** See completion handoffs below. Production uses exact row-aligned annual corrections; Results owns the shared native metric-aware cumulative-state calculation, expected summaries, mechanisms, and equal-probability quantile tail attribution. A former baseline-selected adverse-year mean path has been removed from production results and exports. Earlier BFA/IRN single-scenario metric-switch checks passed correctness/status but remained materially synchronous. The Phase 7 real-data multi-SSP run hit R's 16 GB vector-memory limit before Step 3, so multi-scenario scaling, real-data RIF performance, responsiveness, and interactive browser verification remain release gates.

### Equal-Probability Tail Scope Update (2026-10-02)

- The Step 3 resilience Results card now uses the equal-probability 1-in-20 quantile-attribution row for resilience, repositioning, and interaction. This shares its tail scope and ensemble operator with the adverse policy-effect card; when all channels are available, main + resilience equals the total adverse policy effect.
- Expected metric-aware contributions in the primary Decomposition table remain production scenario-year summaries (years averaged within model, models equally weighted). The separate `Resilience in Adverse Weather` table shows equal-probability cumulative-state quantile contrasts across supported return periods. Its 1-in-20 row is therefore consistent with the card, but its other return periods are additional tail views.
- Removed the baseline-selected same-year tail-mean calculation and its table/export presentation. The info popovers and table scope labels state that each cumulative state uses its own outcome quantile and the underlying weather years need not match.
- Validation: focused metric-tail, Step 3 card, decomposition-export, and Results aggregation tests pass. Real-data interactive verification remains part of the previously documented release gates.

Make Step 3 answer four questions clearly: what outcome is being summarized, how much the policy changes the selected metric, whether it changes modeled weather sensitivity, and whether that sensitivity change comes from repositioning or interaction. Resilience is a primary policy finding, not a hidden technical detail. Results and the primary Decomposition view must share the metric, population, scenario, prediction settings, and summary operator. Preserve existing technical outputs separately.

Conceptual reference: `.claude/policy_effects.md`. Its main/resilience distinction motivates the presentation; production channel conventions, not an idealized formula, determine the numbers.

## Decisions and Scope

| Decision | Implementation contract |
| --- | --- |
| Metric authority | Results controls only; no second aggregation or poverty-line selector. |
| Headline center | **User selected equal-model mean.** Average matched years within each climate model, then average models equally. Apply identical weights to all levels and contributions. This intentionally replaces the current Step 3 median-of-model-means expected-effect headline; align Step 2's expected-outcome headline center only, as specified below. |
| Initial uncertainty | Central channel estimates only. No metric-aware component SEs or intervals. Existing Results uncertainty and technical channel SEs remain separate, with their limitations stated. |
 | Default scope | Primary Decomposition opens on the Results focus scenario: first non-historical scenario in Results order, otherwise historical. Expected summaries use scenario years; adverse card/resilience attribution uses equal-probability 1-in-N outcome quantiles. |
| Attribution order | Main package, then RIF repositioning, then weather-policy interaction. No transfer-versus-covariate sub-attribution in this release. |
| Annual weather correction | **User requires year-specific resilience in production predictions.** Apply channels to each prediction row using the weather exposure used by Step 2 for that row, not a scenario-wide mean or an independently reconstructed annual exposure. |
| Supported channels | Continuous identity/log outcomes using fixest linear models or RIF, where channel states reproduce the actual production policy pipeline. |
| Binary/logistic outcomes | Correct percent/pp formatting for valid probability aggregates, but metric-aware channels are explicitly unavailable initially. Existing analytic logistic policy endpoints are also methodologically unsupported, even if values lie in [0,1]. Do not add money to probabilities or apply linear coefficient deltas to logistic response predictions. |
| Existing technical views | Retain model-scale outputs and controls, but align future production-channel summaries to the new row-aligned annual states. Preserve genuinely separate illustrative historical/adverse diagnostics with explicit scope. Numerical changes caused by annual corrections are intentional and must be characterized, not forbidden. |
| Resilience prominence | Keep a dedicated `Resilience effect` Results card. Show repositioning and interaction separately by default when modeled. Add a primary weather-sensitivity mechanism panel; do not bury it in the technical section. |
| Adverse headline | Preserve the existing probabilities/return-period outcome comparison and expose its main/repositioning/interaction attribution. Updated policy inputs include annual resilience, so tail values can change. Use equal-probability contrasts consistently; do not publish a separate baseline-selected adverse-year scope. |

The user selected the equal-model-mean center, requires weather-sensitivity resilience and its drivers to remain prominent, and explicitly requires computationally efficient year-specific corrections because severe-weather protection is central. This now includes a bounded production policy-method change. The limited Step 2 alignment does not authorize refitting models or redesigning Step 2 predictions.

Non-goals: changing policy targeting, fitting new models, tree-engine decomposition, logistic decomposition, Shapley allocation, inventing a no-shock/reference-weather state, metric-aware decile charts, new residual methods, or wholesale Step 2/UI/performance refactors.

## Review Findings and Code Map

Read `AGENTS.md` and `review/optimization_guidelines.md` before implementation. Use these function names as entry points; line numbers can drift.

| Area | Files/functions and current behavior |
| --- | --- |
| Metric definitions | `R/fct_aggregation.R`: `aggregate_welfare()`, `hist_aggregate_choices()`, `aggregate_pipeline_tables_multi()`. Canonical weights, thresholds, and native values live here. |
| Residual/annual preparation | `R/fct_aggregation_delta.R`: `aggregate_pipeline_per_year()` and its preparation helpers. Log outcomes aggregate `exp(y_point + residual)`; RIF without training residual data falls back to `none`. |
| Display metadata | `R/fct_metric_registry.R`: `metric_metadata()`, `metric_axis_label()`, `annotate_visualization_export()`. Current metadata lacks explicit absolute-change units and complete currency/threshold context. |
| Outcome metadata | `R/fct_outcome.R`: `build_selected_outcome()`; `R/mod_1_03_outcome.R` currency choices. Monetary `so$units` stores PPP/LCU; do not assume it contains a complete currency/time/welfare denominator. |
| Pairing/summary | `R/fct_sim_compare.R`: `paired_model_year_effects()` pairs model/year aggregates; `paired_effect_summary()` averages years then takes a median across models. Medians of channels do not add. |
| Results ownership | `R/fct_policy_sim_compare.R`: `.wire_results_pane()` owns `cmp_agg_method`, validated/debounced `pov_line_val()`, aggregates, and cards. `step3_headline_cards()` currently uses separate marginal means for level context and historical model-scale values for resilience. |
| Module wiring | `R/mod_3_07_results.R` currently returns only `show_coef_uncertainty`; `R/mod_3_scenario.R` wires Results and Decomposition and has `analysis_unit`. |
| Production policy arm | `R/fct_policy_sim.R`: `apply_policy_delta_to_baseline()` adds `.policy_central_delta()` to baseline `y_point` using `svy_row_id`; preserves residual context, weights, years, and `F_loading`. |
| Canonical channels | `R/fct_policy_decompose.R`: `.compute_rif_channels()`, `.decompose_ols()`, `.policy_central_delta()`, `decompose_policy_effect()`, run-owned decomposition context. |
| Run storage | `R/mod_3_06_policy_sim.R` stores compact future model-scale summaries. They are not household-level future channel states and cannot be inverted for nonlinear metrics. |
| Technical presentation | `R/mod_3_09_decomposition.R`, `R/fct_decomposition_summary.R`. Existing channel summaries transform mean log deltas with `100 * (exp(x) - 1)`; these are not mean welfare changes or poverty pp contributions. |

### Two Existing Paths and Required Replacement

`apply_policy_delta_to_baseline()` currently computes one household correction per pipeline using its full weather panel (period-mean hazard), then broadcasts that correction across simulated years. Existing future technical summaries instead recompute channels by weather year. These are different calculations.

**Production policy predictions** are the household-by-simulated-year `y_point` values consumed by Results aggregation, outcome curves, and return-period tables. They currently receive the full-panel correction. **Technical future decomposition summaries** are separately computed model-scale channel summaries in `mod_3_06_policy_sim.R`; they slice scenario weather by year and reduce household channels to compact weighted summaries for Decomposition. They do not update those production predictions, and cannot be inverted into household outcomes for poverty/median/tail attribution.

Replace this duplication with one canonical row-aligned annual channel source. It must update production `y_point` and generate model-scale/metric-aware summaries from those same channel states. Reuse Step 2 baseline predictions, row identifiers, weights, weather exposures, model fits, and residual context. Most inputs exist, but the retained pipeline must prove the exact exposure-to-row mapping; add minimal retained exposure metadata at Step 2 construction if it currently drops that mapping.

After replacement, the new annual policy prediction is `baseline_y_point[row] + main[svy_row_id] + repositioning[row] + interaction[row]`. The main-derived RIF ranks and policy deltas remain fixed under the existing convention; annual hazard values/category exposure make resilience vary. Do not recompute ranks from each year's predicted welfare without a separately justified method change. Current period-mean output is a characterization baseline only, not a required equality target.

## Interpretation Contract

### Main and Resilience

- **Main effect:** direct cash transfer plus model-based effects of changed non-weather policy covariates. Group these into one package; do not call it cash alone.
- **Repositioning:** RIF change in weather sensitivity after the main package moves the household along the fitted distribution. This is a model-derived rank/sensitivity mechanism, not proof of causal protection.
- **Interaction:** fitted weather-policy interaction contribution. In current RIF code it is evaluated at the post-main quantile; the conceptual note's baseline-quantile formula is simplified. Do not silently change the production convention to match that formula.
- **Resilience contribution:** sum of the ordered repositioning and interaction contributions to the selected metric. It can be positive, negative, or zero; do not automatically label it beneficial.
- For fixest linear models, repositioning is not modeled. Show `Not modeled by this engine`, not a measured zero protection effect. A modeled interaction that evaluates to zero is distinct from a missing interaction term.
- Missing/unavailable channels must not become numeric zero. Group channels only when the canonical state supports the group; otherwise mark decomposition unavailable with a reason.

The primary view is **ordered attribution of a modeled policy effect at the simulated weather conditions**. It is not a unique causal partition and not an estimate of weather losses avoided relative to a no-shock world. A main package can affect a poverty metric nonlinearly without any modeled resilience channel. Log transformation can also make the same model-scale main delta produce different currency effects at different weather states.

An avoided-loss diagnostic would need a separate aligned adverse-versus-reference-weather contrast. Do not invent a reference weather state or implement this diagnostic in this release.

### Weather Sensitivity Must Remain Visible

Two complementary primary outputs are required, not substitutes for one another:

- **Resilience contribution to the selected metric:** the contribution of modeled repositioning and interaction at the scenario's production weather basis, in currency/index units or pp. This reconciles to the total effect.
- **Why weather sensitivity changes:** hazard-specific model-scale sensitivity changes implied by the canonical channel formulas. This directly explains the mechanism even when its aggregate effect is small or cancels across households/hazards. It does not switch units when the user selects poverty rate instead of mean welfare.

For continuous weather variable `H`, expose the coefficient changes already implied by the kernels:

`repositioning_sensitivity_i = beta_H(tau_post_i) - beta_H(tau_pre_i)` for RIF;

`interaction_sensitivity_i = sum_x beta_H:x(tau_post_i) * delta_x_i` for RIF, and `sum_x beta_H:x * delta_x_i` for linear fixest.

These are channel-implied changes in modeled sensitivity, holding the main-derived ranks and policy changes fixed. They are not an independently refitted policy model or a complete derivative of all nonlinear model terms. Multiply by the same resolved hazard values to recover the corresponding canonical channel deltas where the formula applies. For binned weather, expose category-versus-reference coefficient contrasts, not a continuous derivative or coefficient divided by the hazard. Multiple weather variables need separate rows/panels with their own units; never add slopes in unlike weather units.

Use native model-outcome units per weather unit (log-outcome units per weather unit for log models), with explicit labels. Do not call these slopes currency contributions or pp effects on the selected poverty metric. Avoid universally describing the sign as `less sensitive`: whether a change buffers damage depends on the hazard contrast, model scale, and outcome direction. Show signed coefficients/changes and explain supported beneficial/adverse interpretations only for a defined hazard direction.

Reuse the canonical beta interpolation/interaction-term maps and pre/post ranks. Add optional central diagnostic fields to the kernel output if needed; no second formula engine. Aggregate diagnostics using the same survey weight semantics and matched scope, separately from selected-metric aggregation. If showing baseline/post sensitivity, label it `Channel-implied weather sensitivity` and disclose included/excluded terms; a change-only panel is safer where a full baseline slope is not supported. Report opposing household changes/cancellation where present; do not infer no mechanism from a zero mean contribution.

Make the RIF fitted sensitivity curve readily visible in this primary panel, with an accompanying pre/post rank summary illustrating movement along the unchanged fitted curve. The curve itself is from Step 1 and does not change with the policy; household positions and channel-implied sensitivity can change. Do not relabel the existing unchanged curve as a policy-updated curve. Fixest displays interaction diagnostics and `Repositioning not modeled`; a model without interaction terms states `Interaction not included in fitted model`, not `Policy cannot build resilience`.

No mandatory resilience-as-share-of-total percentage: shares become misleading when total is near zero or channels offset. Show absolute main, repositioning, interaction, and resilience contributions first.

## Step 2 Alignment

Step 2 asks how climate changes outcomes without the policy. Step 3 asks how a policy changes outcomes under those climate conditions. Both must use the same annual household/population metric, unit conventions, and underlying Step 2 prediction snapshot; they need not make every chart use the same ensemble statistic.

### Bounded Required Changes

- In `R/mod_2_02_results.R`, use the equal-model mean of weather-year model means for the **expected-outcome headline** and its displayed level context. In `R/fct_sim_compare.R`, update `step2_headline_cards()` text to say `Years averaged within model; climate models weighted equally`. Do not globally replace medians in exceedance curves, return-period tables, trajectories, or robustness diagnostics. If the headline shares a reactive with another output, create a small dedicated headline summary rather than unintentionally changing those other outputs.
- Reuse the centralized display metadata for Step 2 expected/adverse headline cards and related exports: percent levels, pp for absolute deviations of rates, truthful monetary/weighted-sum units, and numeric threshold context. Do not change underlying annual metric values. Fix the prosperity-gap editable-line mismatch in both steps, keeping its canonical threshold 28.
- Make Step 2's deviation setting explicit: `Outcome level`, `Difference from historical mean`, or `Difference from historical median`. The Step 3 baseline/policy comparison uses absolute prediction levels, not already-subtracted Step 2 display deviations. Do not change Step 2 deviation behavior as a side effect of sharing formatters.
- Add summary-method/scope metadata to headline exports. Retained median-based Step 2 charts say `Median across climate models` where applicable. Step 3 mean headlines must not appear to reproduce a median chart point.

### Numerical Alignment Rule

At the same completed Step 2 run, scenario, model/year support, annual metric, effective threshold, and residual settings, Step 3's baseline expected level must equal the aligned Step 2 no-policy expected level. With a zero policy, Step 3 policy equals that baseline and all modeled channel contributions are zero. If Step 3 uses a different fixed population or drops model/years, explicitly show the scope difference; never force equality by relabeling.

Step 2 and Step 3 metric controls remain independently authoritative within their steps; do not add a hidden bidirectional control coupling. Their selected values must be shown clearly, and only matching selections warrant a numerical equality claim. Test comparisons with Step 2 deviation off, and separately prove that display deviation does not leak into Step 3 predictions.

Return-period quantities remain distinct: Step 2 shows no-policy adverse outcome levels; Step 3 shows policy-minus-baseline levels at equal return-period probabilities. They are not the same weather-year contrast or an avoided-loss diagnostic. No policy-resilience decomposition belongs in Step 2 because it has no policy counterfactual.

### Outcome Versus Metric

A modeled binary `poor` outcome summarized by mean is not the same estimand as a poverty rate derived from continuous welfare and a poverty line. Name both the modeled outcome and the aggregation in UI and exports. Do not infer the direction of a binary indicator from its name; use explicit outcome semantics, or display signed changes without a benefit claim when direction is unknown.

For validated probability outputs, use response-scale predictions. Do not apply `exp()` or `plogis()` twice, clip invalid predictions to conceal a mismatch, or treat money as probability changes. Current analytic logistic policy corrections use linear coefficient deltas on response predictions; passing a [0,1] range check is not sufficient validation. Until a separately approved response-scale policy method exists, show the Step 3 logistic policy contrast as methodologically unavailable, not just its channels. Binary formatting can be tested with explicitly valid synthetic endpoints and baseline aggregates; it is not evidence that current production policy endpoints are valid. These probability-scale policy limitations are a separate bug/method task, not permission to redesign them here.

## Metric and Unit Contract

Extend the centralized metadata helpers rather than scattering `if (method == ...)` formatting across modules. Keep aggregation values native internally; formatting scales them once.

| Metric/outcome | Native internal value | Display levels | Display absolute change/contribution |
| --- | --- | --- | --- |
| Continuous `mean`, `median` | Selected outcome units | Confirmed currency/time/welfare basis, otherwise truthful outcome-unit fallback | Same units, signed |
| `total` | `sum(y * weight)`, or `sum(y)` without weights | Weighted sum, or unweighted sum | Same sum units, not a per-capita change |
| `headcount_ratio` | Fraction; strict `y < poverty_line` | Percent | Percentage points (`pp`), multiply native difference by 100 |
| `gap`, `fgt2` | Native FGT index fraction | Percent index | Percentage points in the index |
| Binary `mean` | Probability/share | Percent | Percentage points; formatting support does not imply channel support |
| `gini` | Canonical Gini index | Index on documented native scale | Absolute index-point change |
| `prosperity_gap` | Canonical ratio with fixed threshold 28 | Ratio; show actual fixed threshold and unit applicability | Absolute ratio change |
| `avg_poverty` | Canonical mean inverse positive welfare | Registered days-per-dollar unit only for confirmed applicable dollar/day welfare; otherwise inverse outcome units | Same inverse unit |

Display metadata must resolve: outcome name/label, method/label, native unit, level/change units, display multiplier, currency basis, time basis, welfare denominator, survey analysis unit, weight interpretation, threshold kind/value/unit, direction, and missing-context note. Extend `metric_metadata()` or add one focused helper in `R/fct_metric_registry.R`; follow existing calling conventions and preserve current callers.

### Metadata Rules

- Survey analysis unit (`hh`, `ind`, `firm`) identifies rows. It does **not** establish the welfare denominator. Household-row welfare may be per person. Keep these two metadata fields distinct.
- Carry confirmed unit metadata from Step 1 into the run snapshot if it is currently dropped. Use the existing PPP/LCU 2021 control contract only where the run's selection establishes it. Do not manufacture a currency symbol, denominator, or year from unrelated labels.
- Dollar examples are illustrative, not required fallbacks. For confirmed dollar/person/day welfare: `Mean welfare: $3.40 to $3.72 per person per day; change +$0.32`. For incomplete metadata: `Mean welfare: 3.40 to 3.72; change +0.32 (selected PPP units; welfare/time basis unavailable)`.
- `total` must say `Weighted sum` when weights are present. Default weight interpretation is unknown. Show `selected outcome units x survey-weight units` and identify the input welfare basis separately; never claim population-wide dollars/day without confirmed compatible expansion-weight semantics. Do not divide by population.
- `headcount_ratio`, `gap`, and `fgt2` consume the validated selected poverty line. Show its numeric value and compatible unit. Example: `Poverty rate: 32% to 28%; change -4 pp; poverty line: $3.00/person/day` when confirmed.
- Prosperity gap ignores the editable poverty line and uses 28 in canonical code. Correct its Results control/metadata applicability: no editable poverty-line control for this metric, show the fixed threshold, and exclude the editable line from its cache key. Do not change the metric formula or introduce an LCU conversion. Warn about currency applicability when the welfare basis cannot support the documented threshold.
- Average poverty uses positive welfare rows under canonical semantics. Preserve that behavior, document it, and do not claim it averages all rows if nonpositive values were excluded.
- Unknown direction means no better/worse claim. Poverty/inequality metrics use their registered direction; binary means need outcome-specific semantics.

## Calculation Contract

### Shared Summary Operator

For each scenario, use one common valid set of matched model/year observations for baseline, policy, and all cumulative states. Within model `m`, average equally over its retained years. Then average those model means equally across `M` models:

`S(v) = (1 / M) * sum_m ((1 / n_m) * sum_y v[m, y])`.

Use `S` for baseline, policy, main, repositioning, interaction, resilience, and total. This is not a pooled mean over all model/year rows when models have unequal year counts. Survey weights operate inside each annual metric, not as climate-model weights. Record model counts and retained/dropped year counts.

`S(policy) - S(baseline) = S(main) + S(repositioning) + S(interaction)` within tolerance. Do not take separate medians of components. Retain existing median/model-range summaries only as clearly labeled secondary statistics. Add an explicit center option or a focused Step 3 summary helper without changing other callers' defaults. Step 2 has independent summary code in `mod_2_02_results.R`; align its expected headline explicitly, not through an assumed shared paired helper. Existing Results paired charts/tables must say whether they use mean or median; do not leave an unlabeled center disagreement.

The expected-effect center can be computed from paired production endpoints even when channels are unsupported. A channel failure must not silently filter endpoint rows and change the Results headline. If the same support cannot be decomposed fully, publish the endpoint summary and mark its channel summary unavailable.

### Aligned Inputs

Implement a pure helper, suggested home `R/fct_policy_metric_decompose.R`. Separate run/pipeline channel preparation from metric aggregation so changing metric never refits or re-predicts.

Input per scenario/member:

- Actual baseline and policy pipelines from the same completed Step 3 run, with `y_point`, `svy_row_id`, `sim_year`, weight, residual context (`id_vec`, `train_aug`, etc.), transform, model/member identity, and weather context.
- Run-invariant main/rank/sensitivity data in survey-row order plus row-aligned annual hazard exposures/category codes, or compact owned handles that reproduce `delta_main`, `delta_res1`, and `delta_res2` without refitting. Include availability flags and weather-exposure provenance.
- Run-owned model/survey/outcome context, selected aggregation method, validated effective threshold, and requested residual mode captured by the run (not a changed live control), plus effective mode resolved by canonical preparation for each pipeline.

Pipeline row alignment is authoritative. Retain an expanded row index as well as `svy_row_id`; repeated household/year keys may exist. Validate row counts, ordered identifiers, years, weights, and residual context. If a unique full key permits explicit reordering, test it; otherwise reject mismatched ordering. Never zip unmatched rows, silently replace missing deltas by zero, or reconstruct policy levels from technical summary averages. Older objects without sufficient identifiers receive an explicit unavailable reason; do not add speculative compatibility paths.

### Step 2 Exposure Locator

`get_weather()` weather rows are keyed by `code`, survey-round `year`, `survname`, `loc_id`, and `timestamp`. `run_sim_pipeline()` derives `int_month` and `sim_year` from timestamp, then joins survey/weather on `code`, survey `year`, `survname`, `loc_id`, and `int_month`. Survey `year` is not simulation calendar year. The expanded frame contains exact selected weather values, but the returned pipeline currently retains `svy_row_id`/`sim_year` rather than the full exposure locator.

At that authoritative join, retain a compact exposure-table/index or narrow equivalent with prediction-row ordinal, `svy_row_id`, simulation year, location, interview month, survey identity, and exact weather anchor identity. Survey identity fields may be recovered through `svy_row_id` only if validated losslessly. If year/month does not uniquely identify an exposure, retain timestamp/exposure ID; do not collapse duplicate exposures to a mean. Exposure indices must survive row filtering/reordering in prediction and compact Step 2 payload paths.

Weather columns already incorporate spatial aggregation, rolling windows, transforms, and bins. Use those exact model-input exposures/category levels; do not re-roll or re-bin. The anchor date may represent a window crossing a calendar boundary, so grouping raw daily weather by calendar year is incorrect. The new method is both year-specific and interview-season/survey-wave-aware, unlike the existing location annual mean/modal-bin technical convention.

The current `.compute_hazard_values()` reduces a panel by location and has mean/modal/fallback behavior. Do not call that reducer to obtain the new row-aligned exposure. Supply exact hazard vectors through the canonical low-level kernels or a minimal explicit-exposure adapter. The optimized slope/category lookup must match a reference using those same explicit exposures, not the old location reducer. Missing/ambiguous exposures fail validation; no grand-mean, baseline-weather, or zero fallback for annual production.

Resolve actual member weather references through `step2_resolve_weather(member_weather, scenario_owner)` with the owner/shared weather payload, not the scenario representative member. Where Step 2 compact payload moves residual training state into `shared_context`, resolve it through canonical aggregation before deciding effective residual mode; an absent pipeline-local `train_aug` alone does not prove residuals are unavailable.

### Ordered States and Parity

1. Prepare channels at policy run time using the retained Step 2 exposure corresponding to each prediction row. Share this source between the revised `apply_policy_delta_to_baseline()` and decomposition; do not compute an independent full-panel or calendar-year-mean correction. Use run/context ownership validators.
2. Reuse `.compute_rif_channels()`/`.decompose_ols()` through a minimal central-channel adapter. Current `central_only = TRUE` already returns main, repositioning, interaction, total, and interaction availability; reuse these fields without coefficient-SE work. `.policy_central_delta()` returns only total and must preserve that existing return contract. Add optional central sensitivity/rank diagnostics only where needed; do not duplicate the formulas.
3. Gather the run-invariant main package by `svy_row_id` and calculate row/year-specific resilience from aligned exposures. Form `C0 = baseline`, `C1 = C0 + main`, `C2 = C1 + repositioning`, `C3 = C2 + interaction` on the pipeline prediction scale. For fixest, omit the non-modeled repositioning display step.
4. Assert `C3$y_point` matches the actual policy pipeline before aggregation. Use a named numerical tolerance, e.g. `1e-8 * max(1, max(abs(policy$y_point)))` for finite rows. Check missingness separately. Mismatch means unavailable/error with diagnostics, not an extra unexplained component to force equality.
5. Aggregate states with canonical Results semantics, per member/year, using the same survey weights, effective threshold, residual realization, and transform. Log outcomes receive residuals on the log scale before `exp()`; identity outcomes follow their production level-scale residual treatment. Any pipeline lacking the required `train_aug`/`.resid` falls back to effective residual mode `none` under canonical preparation, including but not limited to RIF. Record both requested and effective modes; disclose mixed effective modes across members if present.
6. Calculate contributions as adjacent aggregate differences: `main = A(C1)-A(C0)`, `repositioning = A(C2)-A(C1)`, `interaction = A(C3)-A(C2)`, `resilience = repositioning+interaction`, `total = A(C3)-A(C0)`.
7. Compare `A(C0)` and `A(C3)` against Results endpoint aggregates on identical model/year support. Apply shared `S` to every level/contribution; return parity diagnostics and scope metadata.

Reuse the canonical residual preparation or prove its deterministic stream is identical for all states. Never draw residuals independently per state or add channels after separately exponentiating them. Do not retain four full prediction frames per model in reactive state: reuse one temporary cumulative `y_point` vector and retain small aggregate outputs.

Use one common population/validity mask for state contrasts under existing Results rules. Do not invent metric-specific clipping or missing-value imputation. If canonical metric eligibility differs by state (for example inverse-welfare positivity), report excluded counts and the canonical eligibility caveat; do not imply a newly fixed eligible subpopulation. Nonfinite aggregate cells cannot be averaged separately per component; drop only complete matched cells with explicit counts, or mark the summary unavailable if endpoint support would change.

### Returned Data

Suggested result contract, finalized before wiring UI:

- `status`: `ok`, `unsupported`, or `unavailable`; `reason` and engine/channel availability.
- `annual`: scenario/member/model/year, baseline, after-main, after-repositioning, policy, main, repositioning, interaction, resilience, total, retained/excluded counts, and parity error. Values are native numeric units.
- `summary`: same numeric fields under `S`, model/year counts, center method `equal_model_mean`, and the exact focus scenario identifier.
- `return_period`: cumulative-state outcome levels and ordered contributions at each supported adverse probability, with the exact ensemble/quantile convention and `equal_probability` scope identifier.
- `mechanisms`: separate hazard-specific channel-implied sensitivity/rank summaries with model/weather units, hazard category/reference where relevant, support/weight/scope information, and availability. These do not enter metric contribution sums.
- `metadata`: run identity, outcome/metric/unit contract, threshold/probability, exposure source/mapping identity, correction version `row_aligned_annual_v1`, requested/effective residual modes (per pipeline where needed), fixed survey population, order `main -> repositioning -> interaction`, scale `metric_aware`, and uncertainty `central_only`.

Keep model-scale results in their existing structures. Do not retrofit their `sd_*` fields into this result.

## Efficiency, Tails, and Uncertainty

### Efficient Annual Correction Design

- Reuse Step 2's fitted model and baseline `y_point`, not `predict()`/`run_sim_pipeline()` for every counterfactual. Annual correction is arithmetic/interpolation over existing rows, not a new fit or full design-matrix build.
- Prepare covariate deltas, transfer-derived main effects, training ECDF, pre/post ranks, term maps, and coefficient interpolations once per completed policy run. For continuous hazards, gather invariant sensitivity-change vectors by `svy_row_id` and multiply by row-aligned hazards. For binned hazards, prepare coefficient-change lookup tables by survey row and category; gather the category for each prediction row. Handle only model-required terms/categories.
- Reuse existing join/index information. Build a narrow exposure lookup once per baseline pipeline, not repeated location joins inside household/year/channel loops. Unique exposure tables plus an integer row-to-exposure index are preferable to repeating weather columns in every cumulative prediction frame. Do not extend the old weather-panel-digest `hazard_products` cache into one survey-length hazard vector per member/year/month; bounded exposure indices and invariant slope/category products avoid that growth.
- Process one member/year or bounded row chunk at a time; update one policy prediction vector and reduce channel/counterfactual outputs. Retain run-owned compact invariants/exposure handles plus small annual summaries. Do not store four household-by-year prediction copies or retain a full household-by-year-by-channel table in reactive state.
- Reuse metric-independent invariants after a metric/poverty-line change. Retain or cheaply reconstruct central channel blocks from run-owned exposure handles; do not rerun baseline prediction, policy targeting, model fitting, or joins that are already indexed. Reuse canonical residual preparation once per aligned block and aggregate cumulative states centrally without component gradients/SEs.
- Remove the duplicate future technical decomposition pass where it recomputes the same central channels. Technical coefficient SE calculations remain optional and separate; do not repeat them for every metric or probability.
- Return-period calculations use the small annual aggregate series for each cumulative state, not household-level sorting for every probability. Reuse sorted annual series/quantile preparation for all supported return periods under the canonical algorithm.
- Extend `dev/bench_step3_helpers.R` and `dev/bench_step2.R` with `WISEAPP_STEP2_INCLUDE_STEP3=1`, using `dev/run_step2_benchmark.sh` for process-tree RSS where applicable. Compare old production, a straightforward correctness-reference annual implementation, and the optimized annual implementation. Old-versus-new numerical equality is not required because the weather method changes; reference-versus-optimized equality is required. Record BFA and IRN timings, peak memory, multi-period/multi-SSP scaling, policy preparation time, metric-switch time, and main-process blocking. Use real guideline data, not the smoke fixture, as performance evidence. No ad-hoc new benchmark framework or dependency upgrade.

Benchmarks are a release gate, not a reason to revert to period means silently. If the annual path is too slow, profile and move bounded work into the shared coordinator or optimize the exposed hot loop while retaining annual semantics.

### Adverse Return-Period Attribution

Keep existing supported probabilities and minimum-year rules. Adversity comes from the selected metric's registered tail (low welfare, high poverty), not a universal sign or highest temperature alone.

**Equal-probability outcome contrast (existing Results question):** calculate the adverse outcome quantile for every cumulative state over its annual series, then difference adjacent state quantiles. Use the same quantile interpolation and ensemble reduction as the actual Results threshold endpoints. For an endpoint operator `T_p`, report `T_p(C1)-T_p(C0)`, `T_p(C2)-T_p(C1)`, and `T_p(C3)-T_p(C2)`. These telescope to `T_p(policy)-T_p(baseline)` even though the years contributing to each state quantile may differ. Do not take quantiles of annual channel differences and expect them to add. If the endpoint operator takes medians across model quantiles, difference cumulative medians; do not take medians of per-model channel contributions. This tail center may remain median-based, unlike the expected equal-model mean, and must be labeled.

All primary adverse-weather attribution uses the equal-probability outcome quantile contrast above, including the resilience card and adverse Results card. Do not separately select baseline adverse years or present a same-year tail mean. This quantile attribution identifies whether repositioning or interaction drives the adverse outcome contrast, but does not remove the main welfare uplift to define weather losses avoided relative to a chosen no-shock world. That separate reference-weather diagnostic remains deferred.

### Uncertainty and Ownership

- New metric-aware component charts/tables show central estimates only and state `Component uncertainty not estimated`. Weather/model variation in the scenario is averaged for the center, not an interval for each component.
- Existing Results uncertainty uses paired prediction gradients where available. Production policy `F_loading` currently remains baseline-X; do not claim full policy-coefficient uncertainty. Do not center intervals on an old median while showing a new mean. If the Step 3 paired-summary center option is changed, center its existing coefficient bands consistently and retain named weather/model ranges without relabeling them as confidence intervals.
- Existing technical channel SEs use documented approximations, including diagonal covariance assumptions. Keep them in the technical section with those limitations.
- Own channel preparation at the completed-run/context level; own metric summaries in one shared Results reactive. Use existing bounded cache infrastructure or a single-entry per-run cache where sufficient. Cache keys include run/model/member identity, metric, consumed threshold, captured residual/transform settings, and scope. Display-only labels do not invalidate prediction/channel preparation.
- A stale run, run/context mismatch, missing weather, unavailable identifiers, unsupported engine/link, or final-state mismatch must produce a visible reason. Never show previous-run values under a new metric or new scope.
- No new worker pools, large reactive copies, or unbounded caches. Step 3 orchestration is currently synchronous; start with bounded vectorized preparation and measure blocking. If it is material, use the existing process-wide mirai coordinator/ExtendedTask pattern with an explicit immutable snapshot and parent run-identity validation. The locked context/environment is process-local and cannot simply be shipped as a worker-owned live context. Never start a pool per model/year/session; record any required worker snapshot/atomic publication contract in the phase handoff.

## Module and UI Contract

### Single Shared Calculation

`.wire_results_pane()` should return a small API captured by `mod_3_07_results_server()` and passed through `mod_3_scenario_server()`:

- validated `aggregation_method` reactive;
- effective `poverty_line` reactive from existing `pov_line_val()` semantics, not raw numeric input;
- shared `metric_context` and focus-scenario reactive;
- shared `metric_decomposition` reactive for all available scenarios, plus endpoint summary when channels are unavailable;
- existing `show_coef_uncertainty` reactive unchanged.

Suggested names are descriptive targets, not permission for multiple parallel abstractions. Results already has both baseline/policy historical and future arms; make it own the shared metric-aware calculation, using the run-owned decomposition context supplied by the parent. Decomposition consumes the returned small result instead of independently reassembling predictions. Pass `analysis_unit` and confirmed unit context from the parent to both surfaces. No circular dependency on Decomposition UI inputs.

The validated method must fall back when the selected outcome changes before a browser update round-trip. Preserve poverty-line edit/default/debounce behavior. A method switch is not debounced together with its threshold. Metadata and numbers must use the same effective threshold snapshot; do not advertise an edited value before its calculation updates.

### Decomposition Layout

1. Prominent tag: modeled outcome, aggregation, level/change units, applicable threshold, currency/welfare basis where known, and selected scenario. Keep notes short, with detailed method text available separately.
2. Primary section titled `Policy Effect in the Selected Metric`: baseline, policy, signed total, then a contribution chart/table with main, repositioning, and interaction visible by default where modeled, and a clearly highlighted resilience subtotal. Do not double-count resilience alongside its subcomponents in a stacked total.
3. Compact scope/method note: fixed survey population; survey-weighted annual metric; years averaged within model then equal-model mean; year-specific row-aligned weather correction; main-first order; central estimates only.
4. Scenario selector, if multiple scenario results exist, defaults to the shared Results focus. Other selections carry `Different scenario from Results headline`. Primary weather scope remains the production scenario-years scope; technical historical/adverse controls do not alter it.
5. Primary companion section titled `How the Policy Changes Weather Sensitivity`: explicit repositioning versus interaction diagnostics, hazard-specific coefficient changes and RIF rank movement/unchanged fitted curves as specified above. This section is not hidden inside the technical appendix. Keep its hazard/scope/unit metadata visible alongside the selected-metric contribution view.
6. Add a primary `Resilience in Adverse Weather` table/chart across supported return periods with main, repositioning, interaction, resilience subtotal, and total. Use equal-probability cumulative-state quantile contrasts consistently with the adverse Results card; disclose that the cumulative state quantiles need not use the same weather years.
7. Secondary section titled `Technical Decomposition on the Model Scale`. Retain historical/adverse illustrative controls and outputs with explicit scope. Future production-channel summaries derive from the new annual states, not a second weather-year loop; explain that transformed mean log channels are not currency changes or selected-metric contributions.

Use existing visual language. Follow repository requirements for new tables/figures: reactable with client-side CSV and echarts4r. Avoid a new plotting-library migration of the retained technical views. Chart accessible text and tooltips must include signed contributions, units, scope, and unavailable reasons; color alone must not encode benefit.

### Results Cards

- Expected effect: selected metric label and absolute-change unit; baseline and policy levels from the same paired support and `S`; focus scenario and `Equal-model mean` note. Change the center even when channel attribution is unsupported, using valid endpoint summaries.
- Keep a prominent dedicated `Resilience effect` card: headline value is the selected-metric resilience contribution; always show `Repositioning: ...` and `Interaction: ...` beneath it when modeled, with explicit missing-engine/term status otherwise. Include the main contribution as context and a direct link to `How the Policy Changes Weather Sensitivity`. Use the shared focus summary, not historical `decomp_result` or `decomposition_summary_data()`. Explain that resilience arises from modeled weather-sensitivity channels, and can amplify or buffer damage depending on the hazard/outcome; do not replace this card with a generic channels label.
- Retain the prominent adverse card, labeled `Policy effect at the adverse 1-in-20 threshold`; format native differences correctly and say `Policy minus baseline at the same return-period probability; not necessarily the same weather years`. Preserve supported return-period probabilities and quantile conventions but recompute from new annual policy predictions. Include a link to tail-channel attribution. Its total reconciles to its own cumulative-quantile decomposition, not to the expected-effect total.
- Program scale/reach and robustness calculations stay unchanged. Correct a note only where the changed center/scope would otherwise make it inaccurate.
- Unsupported channels: show a precise unavailable reason, not a blank percentage, zero, or historical fallback. Binary mean formatting uses percent levels/pp changes only for independently validated probability endpoints. Current analytic logistic policy endpoints are unavailable even when in range; do not advertise them as valid policy effects. Baseline probability levels may still be shown separately if valid.

## Exports

Add distinct metric-aware summary, annual contribution, and weather-sensitivity mechanism exports. Mechanism exports carry hazard units/category contrasts, rank convention, channel-implied coefficient changes, and explicitly separate model-scale units from selected-metric contributions. Keep technical export keys and numeric data, relabel descriptions to identify model scale. Use existing `wise_export_table()`, `wise_export_figure()`, and metric annotation patterns. Every new displayed table needs browser CSV; also register the corresponding bundle export.

Both browser CSV data and bundle data must include native numeric levels/contributions and interpretation fields: outcome name/label/type/transform, metric ID/label, native and displayed units/multiplier, numeric threshold/unit/kind when used, currency/welfare basis and unknown-context note, survey analysis unit/weight interpretation, scenario/member/year where relevant, model/year counts, run/scope identity, exposure mapping/correction version, probability/return period when relevant, requested/effective residual modes, component order/method, center method, `metric_aware` versus `model_scale`, availability/reason, and uncertainty status. Keep native versus display-scaled columns unambiguous.

Update headline export descriptions too: remove `paired extreme-year protection` claims. Preserve legacy `poverty_line` boolean fields for existing consumers if present; add numeric threshold fields rather than changing that field's type.

## Implementation Phases for Clean Sessions

Execute sequentially. Each implementer reads this plan and the named files/tests, inspects current git changes, preserves unrelated work, and stops at its phase boundary. Do not assume a previous chat transcript. Each phase handoff records files changed, public/internal API contracts, tests run, remaining limitations, and the next phase. Do not commit unless requested. The Step 2 changes are limited to the explicit alignment contract above.

### Phase 1: Characterization and Summary Center

Files: `R/fct_sim_compare.R`, Step 3 paired/headline flow in `R/fct_policy_sim_compare.R`, expected-headline flow only in `R/mod_2_02_results.R`, relevant tests.

Add failing/characterization fixtures first for unequal model year counts, median nonadditivity, current level-context mismatch, and period-mean-versus-year-specific channel differences. Implement the explicit equal-model-mean Step 3 center and paired baseline/policy levels; preserve other callers' defaults. Align Step 2's expected headline only to the same center, using a dedicated reactive if needed. Relabel retained median summaries and re-center affected headline coefficient bands consistently, keeping existing uncertainty limitations explicit. Gate: levels subtract to expected effect; Step 2 expected and Step 3 baseline levels match for identical scope/settings; Step 2 return-period/trajectory/diagnostic centers and annual predictions unchanged.

Tests: `test-mod_3_07_results.R`, pairing/summary tests found by searching `paired_effect_summary`, `test-w3-a-aggregation-characterization.R`.

#### Phase 1 Completion Handoff (2026-10-01)

Completed scope:

- Step 3 expected-effect headline and paired expected-effect summaries now explicitly use equal-model means, after averaging retained matched years within each model. Baseline/policy level context comes from the same paired summary, not separate marginal aggregate means. Historical-only focus also receives paired levels.
- Step 2 has a dedicated `headline_bands_rv()` that changes only expected-headline values and level context. `pointrange_bands_rv()`, return-period thresholds, annual series, uncertainty diagnostics, baseline pipelines, and existing historical mean/median display-deviation behavior remain unchanged. Tests demonstrate same-scope Step 2 expected level equals Step 3 paired baseline, with zero-policy parity and no deviation leakage into underlying annual aggregates/predictions.
- Step 3 paired coefficient bands are centered on the new mean; existing SD calculation and baseline-X policy-gradient approximation are unchanged. Weather/model ranges retain their existing definitions. Retained median-based threshold summaries and Step 2 chart exports are labeled distinctly from expected mean headlines.
- Added fixtures for unequal model year counts (not a pooled row mean), median component nonadditivity, paired versus marginal level-context mismatch, finite matched support, zero policy, and full-panel broadcast versus explicit per-year central corrections. The latter characterizes the old production method only; no annual method was shipped.

Files changed:

- `R/fct_sim_compare.R`: summary center option, paired levels/support metadata, expected-headline/secondary-summary labels.
- `R/fct_policy_sim_compare.R`: explicit mean callers, paired headline levels, summary/export labels, small regression-test reactive accessors.
- `R/mod_2_02_results.R`: headline-only mean reactive and retained-median scope labels/export descriptions.
- `tests/testthat/test-mod_2_02_results.R`, `test-mod_3_07_results.R`, `test-policy-sim-compare-agg-cache.R`, `test-w3-a-aggregation-characterization.R`, `test-policy-central-kernel.R`.
- This plan. The plan was already untracked at session start; no unrelated edits were reverted and nothing was committed.

API contracts for the next session:

- Internal `paired_effect_summary(effect_tbl, band_q, scenario, center = c("median", "equal_model_mean"))` preserves the median default for other callers. Step 3 explicitly passes `center = "equal_model_mean"` in both paired and headline reactives. Mean mode requires finite baseline, policy, and effect cells on common support.
- Its result adds native `baseline`, `policy`, `center_method` (`equal_model_mean` or `median_model_mean`), `n_model_years`, and `n_dropped_model_years`. Existing `n_years` remains the minimum retained year count per model, not total years. Endpoint levels reconcile to `value` in mean mode; median endpoint levels need not reconcile.
- `step3_headline_cards()` reads `baseline`/`policy` from its focus summary. Existing aggregate arguments remain in the internal signature, but are not used to reconstruct headline levels. No marginal/historical fallback is used when paired levels are absent.
- Step 2's `headline_bands_rv()` reuses the immutable results-frame matrices, applies `mean(rowMeans(...))` over finite values, and subtracts the existing historical display reference only at display time. Other band columns are retained for existing range cards, not new uncertainty estimates for the mean.
- `.wire_results_pane()` still returns its existing test/internal API and additionally exposes `paired_effect_summary`, `headline_paired_effect_summary`, and `headline_cards` reactives. This is not yet the Phase 2 validated selection/context API. The existing module uncertainty return has not changed.
- The paired adverse table reactive adds `center_method = "median_model_quantile"`; its numbers and probability conventions are unchanged.

Validation:

```sh
Rscript -e 'devtools::test(filter = "policy-central-kernel|policy-decomposition-uncertainty|policy-sim-compare-agg-cache|fct_aggregation_delta|fct-aggregation-kernel|visualization-contracts|mod_3_07_results|mod_2_02_results|w3-|step3-wave2-e", reporter = "summary", stop_on_failure = TRUE)'
git diff --check
```

Both passed with no focused-suite warnings or skips. The full `Rscript -e 'devtools::test(reporter = "summary", stop_on_failure = TRUE)'` completed, but reported three **preexisting** failures in `test-policy-diagnostic-snapshot.R:202,204,210`. The stale fixture mocks `decompose_policy_effect()` while the unchanged module calls `.decompose_policy_effect_run()`; the fixture also lacks `fit3`, so its first run fails before publishing the snapshot/run ID. Replacing all three changed production files' definitions in-memory with their `HEAD` versions reproduced the same failures; untouched module/test files also match `HEAD`. This unrelated fixture was not modified. Full-suite warnings: deprecated glmnet `thresh` argument and the incomplete weather-cache manifest test's missing-file warning. No interactive browser check or real-data performance benchmark was performed; this phase changes only small summaries, not prediction/performance kernels.

Remaining limitations and Phase 2 start (historical Phase 1 handoff; superseded by Phase 2 below):

- Implement **Phase 2 only** next, starting with the display metadata and validated selection API below. Read the plan, `AGENTS.md`, optimization guidelines, and current git diff rather than assuming a transcript or clean worktree.
- Units/rate formatting, prosperity-gap controls/cache applicability, unsupported logistic policy-endpoint guards, effective method/threshold/focus/context API, and Decomposition wiring are not implemented yet. Headline benefit wording and adverse-protection wording still need the subsequent metric/method interpretation work.
- The resilience card still uses existing technical/historical decomposition; annual corrections, selected-metric channels, sensitivity mechanisms, tail attribution, and native metric-aware exports await Phases 3-7. Production still broadcasts full-panel corrections. Do not treat the new characterization test as evidence of row-aligned annual correctness.
- Existing SD approximation, tail-center conventions, and annual metric formulas were deliberately preserved. No component uncertainty, model refit, prediction rerun, new worker pool, dependency change, or large retained prediction state was added.

### Phase 2: Display Metadata and Selection API

Files: `R/fct_metric_registry.R`, `R/fct_policy_sim_compare.R`, `R/mod_3_07_results.R`, `R/mod_3_scenario.R`, Step 2 headline formatters/threshold controls in `R/fct_sim_compare.R` and `R/mod_2_02_results.R`; minimal outcome snapshot changes only if confirmed metadata is dropped.

Centralize level/change/threshold formatting, unknown-unit fallbacks, binary pp, and weighted-sum semantics; apply shared formatting to both steps' headline outputs/exports and label Step 2 deviations explicitly. Correct prosperity-gap editable-threshold applicability in both steps without changing its formula. Add one method-validity guard for unsupported logistic policy endpoints across Step 3 policy-effect cards, comparison outputs, and exports, not just the new channel view; valid Step 2 baseline outputs need not be suppressed. Return validated method/effective line/focus/context reactives, preserving current uncertainty return. Pass these to Decomposition without implementing its new chart yet. Gate: all registry categories format honestly; unsupported policy contrasts are not published as valid estimates; method/threshold changes do not invoke policy run/prediction functions.

Tests: `test-visualization-contracts.R`, `test-policy-sim-compare-agg-cache.R`, `test-mod_3_07_results.R`, outcome metadata tests if touched.

#### Phase 2 Completion Handoff (2026-10-01)

Completed scope:

- Centralized metadata/formatters distinguish percent levels from pp changes, native index/ratio changes, selected PPP/LCU 2021 basis, unknown welfare/time denominator, and weighted versus unweighted sums. Analysis unit is separate from welfare denominator. Legacy boolean `poverty_line` is preserved; `uses_poverty_line` identifies editable-threshold applicability.
- Prosperity gap shows fixed threshold 28, hides the editable line in both Results UIs, and excludes unrelated editable-line values from its calculation keys. No metric formula or currency conversion changed. Average-poverty metadata states positive-welfare eligibility and uses inverse outcome-unit fallback rather than assuming dollar/day welfare.
- Both steps' expected/adverse headlines use centralized formatting and effective threshold context. Step 2 deviation labels are explicit and rate deviations use pp. Step 3 adverse wording distinguishes equal-probability contrasts from same-weather-year effects. Unknown binary direction is not inferred from an indicator name.
- Results owns validated aggregation, effective/debounced poverty line, focus scenario, and metric context reactives, passed through the parent to Decomposition. No second selector, channel aggregation, primary Decomposition redesign, or Step 2/3 control coupling was added.
- Logistic/binary analytic policy endpoints are unavailable in Step 3 Results even when predictions lie in [0,1]. Policy comparisons, derived Results outputs and exports are withheld; cards/notice show the reason. Baseline aggregates remain independently available; program reach remains unchanged. This is a guard, not a replacement response-scale policy method.

Phase 2 files changed, preserving Phase 1 work:

- `R/fct_metric_registry.R`, `R/fct_policy_sim_compare.R`, Step 2 headline functions in `R/fct_sim_compare.R`, `R/mod_2_02_results.R`.
- `R/mod_3_07_results.R`, `R/mod_3_scenario.R`, `R/mod_3_09_decomposition.R` (selection API plumbing and unsupported logistic diagnostic suppression; no new metric-aware chart).
- `R/fct_outcome.R` preserves explicit binary direction if supplied, otherwise stores `unknown`, rather than deriving direction from the indicator name.
- `R/fct_policy_decompose.R` retains run-owned model/link type, a small snapshot addition needed to identify numeric-coded logistic fits without consulting changed live controls.
- `tests/testthat/test-visualization-contracts.R`, `test-mod_2_02_results.R`, `test-mod_3_07_results.R`, `test-policy-sim-compare-agg-cache.R`, run-owned logistic type/family checks in `test-policy-central-kernel.R`, and unit expectations in `test-w3-a-aggregation-characterization.R`.
- This plan. No files were committed.

API contracts:

- `metric_metadata(method = "mean", so = NULL, pov_line = NULL, analysis_unit = NULL, weighted = NULL)` preserves positional callers. `weighted = NULL` means unknown. New fields include native/level/change units, multiplier, currency/time/welfare basis, outcome labels/type, analysis unit, weight interpretation, numeric threshold/kind/unit, direction certainty, missing context, and `uses_poverty_line`. PPP/LCU selection establishes the selected 2021 currency contract, not time/denominator or expansion semantics. Explicit binary outcome direction is honored; otherwise direction is unknown.
- `format_metric_value(value, metadata, change = FALSE, digits = 2)` formats one native scalar, scaling once. `metric_context_note(metadata)` provides interpretation text. Export annotation accepts trailing `pov_line`, `analysis_unit`, `weighted`, and `context`; a metadata-list context is authoritative for the effective snapshot, a string context appends a note. Zero-row exports stay zero-row.
- `.wire_results_pane(..., aggregation_cache = NULL, analysis_unit = reactive(NULL))` returns `aggregation_method`, `poverty_line`, `focus_scenario`, `metric_context`, and internal `policy_endpoint_status`, in addition to its earlier API. `metric_context` carries run identity when available, requested residual mode, scenario, availability/reason, and the same effective threshold consumed by calculation.
- `mod_3_07_results_server()` returns the first four selection/context reactives plus unchanged `show_coef_uncertainty`. The parent supplies `analysis_unit` and passes them to new trailing Decomposition server arguments. The technical module does not yet consume them for metric-aware outputs, but uses the same method-validity helper to suppress unsupported logistic channel effects and export the unavailable reason. Do not add placeholder numeric `metric_decomposition`; calculation belongs to Phase 5. Supported technical diagnostics remain unchanged.
- `.policy_endpoint_status(so, context)` rejects binary/logical/boolean outcome types and run-owned logistic/binomial model types. Immutable context now stores `model_type`, falling back to fitted-family identification where necessary. No refit, clipping, money-to-probability correction, or new policy prediction method was introduced.
- Headline exports retain formatted text and add native numeric/context fields and summary-method/scope metadata. Full contribution/mechanism/tail export contracts await subsequent phases; existing keys are preserved.

Validation and limitations:

- Focused tests cover metadata/formatting, logistic withholding, Results wiring, method fallback, effective thresholds, prosperity-key isolation, Step 2 headline/deviation behavior, exports, technical context and Phase 1 regressions. The following command passed without warnings/skips; `git diff --check` also passed:

```sh
Rscript -e 'devtools::test(filter = "mod_2_02_results|mod_3_07_results|policy-sim-compare-agg-cache|visualization-contracts|policy-central-kernel|policy-decomposition-uncertainty|w3-|step3-wave2-e|ui-migration-step3|export-wiring-contract|mod_1_03_outcome|fct-outcome|outcome-summary", reporter = "summary", stop_on_failure = TRUE)'
```

- Full suite was not rerun for Phase 2, as the user indicated it may be unnecessary. Phase 1's three preexisting diagnostic-snapshot fixture failures remain documented above; this phase does not fix that unrelated fixture.
- Canonical aggregation parity also passed with `Rscript -e 'devtools::test(filter = "fct_aggregation_delta|fct-aggregation-kernel", reporter = "summary", stop_on_failure = TRUE)'`.
- Existing outcome metadata callers passed with `Rscript -e 'devtools::test(filter = "fct-outcome|outcome-summary|mod_1_03_outcome", reporter = "summary", stop_on_failure = TRUE)'`.
- `devtools::load_all(quiet = TRUE)` passed with the updated module signatures.
- Read-only review identified and fixed a missing effective context on the adverse-table export, upstream name-derived binary direction, and unsupported logistic technical effects. A pending line edit retains the old disclosed effective/debounced threshold until debounce resolves while the method changes immediately; a new transition test checks context and calculation agree throughout. This is intentional preserved control behavior, not a stale metadata mismatch.
- Snapshot/export compatibility passed with `Rscript -e 'devtools::test(filter = "active-mask|uncertainty-decomposition|export-bundle|step2-payload", reporter = "summary", stop_on_failure = TRUE)'`; this additional suite emitted the existing incomplete weather-cache manifest missing-file warning, with no failures/skips.
- No interactive browser check or real-data annual-correction benchmark was performed. Unit metadata remains unknown where the outcome snapshot supplies no confirmation. Metric-aware resilience cards, mechanisms, annual provenance and full component exports remain unimplemented.

Historical Phase 2 next-session instruction (superseded by the Phase 3 handoff below): implement Phase 3 only, preserving Phase 1/2 work and leaving production annual integration to Phase 4.

### Phase 3: Annual Exposure Contract and Central Kernel

Files: `R/fct_policy_decompose.R`, pipeline construction in `R/fct_simulations.R` only for minimal retained exposure mapping, new focused helper file `R/fct_policy_metric_decompose.R`, tests. Build the annual kernel before publishing new policy outputs.

Characterize exact Step 2 weather exposure/row mapping first. Retain minimal lossless metadata if needed without changing baseline predictions. Reuse central kernel formulas and prepare run-invariant slopes/ranks once; implement continuous/binned row-aligned annual channels with no SE work. Validate scope/context ownership. Build a slow, transparent correctness reference used only in tests/benchmarks. Gate: optimized annual channels equal the reference for each expanded row; weather exposure matches baseline construction including location/month/category; fixed weather reproduces old corrections when exposure conventions match; logistic/tree engines explicitly unsupported; compact future summaries never used as household states.

Tests: `test-policy-central-kernel.R`, `test-policy-decomposition-uncertainty.R`; new `test-fct-policy-metric-decompose.R` for adapter/alignment cases.

#### Phase 3 Completion Handoff (2026-10-01)

Completed scope:

- Step 2 now retains exact prepared weather anchors and selected model-input weather columns, including factors, in a narrow table with integer prediction-row exposure indices. Cached and inline joins assign exposure IDs before expansion; prediction output carries both the exposure ID and expanded prediction-row ordinal. Duplicate timestamps/exposures are not collapsed. Filtering/reordering follows retained IDs, never positional assumptions. Compact Step 2 payloads preserve the nested mapping unchanged.
- Added a separate annual central-channel source. Preparation evaluates existing `.compute_rif_channels()`/`.decompose_ols()` kernels on unit continuous hazards and individual factor categories, storing survey-row slopes/category contrasts once. It does not introduce another coefficient/interaction formula engine. RIF main-derived pre/post ranks stay fixed in the run context; production weather values are gathered exactly through the retained indices. Category diagnostics are fitted-reference contrasts, not slopes or ratios to hazard values.
- Added a deliberately slow row-by-row explicit-exposure reference for tests/benchmarks. Optimized channels match it for RIF/linear, identity/log, continuous/multiple hazards and binned weather. Fixed comparable hazards reproduce old central corrections; zero policy yields zero modeled channels. Flat RIF curves can retain rank movement with zero repositioning, and zero hazards can have zero contributions despite nonzero sensitivity changes.
- Alignment checks reject stale runs, missing survey join identity, missing/nonfinite exposures, unknown categories, invalid/fractional IDs, mismatched ordered survey IDs/years/weights/residual IDs, and mismatched timestamp year/month or survey wave/location/month. No missing-exposure, annual-mode-bin, mean-hazard, or technical-summary fallback was added. Unsupported logistic/binary methods and tree engines remain explicit.
- **Production boundary preserved:** `apply_policy_delta_to_baseline()`, future technical year-loop calculations, Results aggregation/cards, and Decomposition UI were not switched to the annual adapter. Existing characterization tests still prove period-mean production broadcasting. Baseline `y_point` values are unchanged; retained metadata is the only Step 2 behavior addition.

Files changed:

- `R/fct_simulations.R`: exposure table/mapping retention at the authoritative join and prediction output boundary.
- `R/fct_policy_decompose.R`: retain survey join identity and outcome type in immutable contexts; `skip_coef = TRUE` bypasses OLS covariance extraction; correct central-only return documentation. `.policy_central_delta()` retains its numeric-vector return contract.
- New `R/fct_policy_metric_decompose.R`: internal preparation, validation, chunk-capable annual evaluation, and correctness reference. No exports or new dependency.
- New `tests/testthat/test-fct-policy-metric-decompose.R`, new `test-policy-exposure-mapping.R`, and updated additive pipeline field contract in `test-step2-contract.R`.
- This plan. Worktree was clean at session start; prior phases were already present. No commits were made.

Internal API contracts for Phase 4:

- `pipeline$weather_exposure` has `status = "ok"` or `"unavailable"`, `reason`, `available`, `table`, `row_index`, `prediction_row_id`, `svy_row_id`, `sim_year`, `weight`, `id_vec`, and `id_col`. The table retains `.policy_exposure_id`, `code`, survey `year`, `survname`, `loc_id`, interview `int_month`, `sim_year`, exact `timestamp`, and selected weather columns. Each original weather row has its own ID even with duplicate timestamp keys. Row-vector fields follow actual prediction order. Lost prediction tags yield explicit unavailable metadata, not a guessed mapping.
- `.prepare_policy_annual_channels(context, run_identity)` returns a locked environment with `status = "ok"`, immutable context/run identity, `delta_sp`, `delta_main_covar`, `delta_main`, per-hazard `products` (survey-row `repositioning`/`interaction` matrices and `categories`, NULL for continuous), `tau_i_pre`/`tau_i_post`, `repositioning_modeled`, `interaction_included`, and `correction_version = "row_aligned_annual_v1"`. Unsupported/unavailable methods return a small status/reason list instead. Call once per policy run, then reuse it across members/scenarios and metric edits. Preparation has no coefficient-SE work when supplied a central context built with `skip_coef = TRUE`.
- `.policy_annual_channels(pipeline, prepared, run_identity, rows = seq_along(pipeline$y_point))` validates the run and whole mapping, then returns native model-scale central `delta_*` vectors for selected rows, prediction ordinals and engine/interaction flags. `rows` may be a bounded chunk and may be reordered, but must be integral, unique and in range. It never mutates the baseline pipeline. Do not retain its full expanded channel lists for all members; consume/reduce a bounded block and discard it. Whole-mapping validation currently repeats on calls; Phase 4 should measure this before choosing its chunk orchestration rather than assume negligible overhead.
- `.policy_annual_channels_reference(pipeline, context, run_identity)` is intentionally slow and for tests/benchmarks only. It invokes the unchanged canonical kernels at each row's exact exposures, then selects that survey row. Do not call it from Shiny production.
- `.policy_annual_channel_status(context)` centralizes engine/link/transform support for this adapter. `.validate_policy_annual_exposure()` requires complete survey join keys in the run context and matching ordered pipeline/mapping metadata. Older Step 2 objects without exact retained mapping must fail the new policy run or require a fresh Step 2 run; no speculative reconstruction is implemented.
- Exposure data are self-contained per pipeline, so annual evaluation needs no independent weather-panel reducer or representative-member lookup. When Phase 4 handles other technical weather references, continue using `step2_resolve_weather(member_weather, scenario_owner)` and shared residual context as specified above. Do not substitute those references for the exact retained exposure table.

Validation:

```sh
Rscript -e 'devtools::test(filter = "fct-policy-metric-decompose|policy-exposure-mapping|policy-central-kernel|policy-decomposition-uncertainty|step2-contract|step2-payload|fct_run_simulation|w3-b-future-decomposition", reporter = "summary", stop_on_failure = TRUE)'
Rscript -e 'devtools::test(filter = "mod_2_02_results|mod_3_07_results|policy-sim-compare-agg-cache|visualization-contracts|active-mask|uncertainty-decomposition|policy-context|policy-run|fct_aggregation_delta|fct-aggregation-kernel", reporter = "summary", stop_on_failure = TRUE)'
git diff --check
```

- All completed commands passed. The first suite emitted only the existing incomplete weather-cache manifest missing-file warning; the second had no warnings/skips. `policy-context`/`policy-run` filter tokens match no dedicated files; do not treat them as separate ownership-suite coverage. Ownership rejection is tested in the new helper tests and existing central-kernel tests.
- Independent read-only review identified fractional ID truncation and skipped missing survey identity checks; both were fixed with regressions before final validation.
- A full `devtools::test(reporter = "summary", stop_on_failure = TRUE)` attempt stopped during `fct_get_weather` before completion through the tracked process tool; no final suite result was obtained. It is **not** a full-suite pass. Phase 1's documented preexisting diagnostic-snapshot failures were not repaired in this phase.
- No browser check, real-data BFA/IRN timing/RSS benchmark, or production annual integration was performed. Reference parity here is synthetic correctness evidence, not release performance evidence. No new worker/coordinator/cache was created. Run-owned environments remain process-local; no worker snapshot/publication contract was needed in this phase.

Next session: implement **Phase 4 only** below. Read this plan, `AGENTS.md`, optimization guidelines, current git status/diff and helper/tests. Prepare the annual source once per run, apply bounded row-aligned corrections to reused baseline predictions, derive compact future production-channel summaries from that same source, remove the duplicate central year loop, version/invalidate policy caches and publish atomically. Invalid/missing mappings must fail the new run without partial output or period-mean fallback. Extend the existing benchmark harness for reference/optimized annual parity and initial BFA/IRN measurements. Metric counterfactual aggregation/tails/shared reactive remain Phase 5; primary UI changes remain Phase 6.

### Phase 4: Production Annual Policy Integration

Files: `R/fct_policy_sim.R`, `R/mod_3_06_policy_sim.R`, run-owned decomposition context/storage helpers, benchmark harness.

Replace full-panel broadcast in production with annual row-aligned correction using Phase 3's source. Keep baseline pipelines/residual context unchanged; version correction metadata as `row_aligned_annual_v1` and invalidate old policy caches. Generate compact future production-channel summaries from the same annual states and remove duplicate central year-loop calculations. Publish new run state atomically; failed exposure mapping must fail the new policy run, never fall back to period means or publish a partial mixture. Run reference parity and initial BFA/IRN timing/memory checks. Gate: mild/severe years get appropriate distinct modeled resilience; zero policy changes nothing; source channels reconstruct new production policy predictions exactly; no full baseline re-prediction or repeated SE computation.

Tests: `test-policy-central-kernel.R`, `test-w3-b-future-decomposition-characterization.R`, run/cache ownership tests; extend `dev/bench_step3_helpers.R` through the existing harness.

#### Phase 4 Completion Handoff (2026-10-01)

Completed scope:

- Replaced production period-mean broadcasting, ID fallback and missing-delta-to-zero behavior with exact annual exposure corrections. Historical and every future member reuse baseline predictions, weights, residual context and baseline-X gradients. Missing/invalid mappings or unsupported engines/links fail the run; no partial arm or period-mean fallback is published.
- Annual preparation is reused once per run. Production validates each whole mapping once, then evaluates bounded 100,000-row central blocks. The channel adapter's standalone entry point remains strictly validated. Future model-scale channel/decile statistics are reduced from those same blocks, retaining only annual/member statistics. A profiled matrix/`rowsum()` reducer replaced expensive per-year data-frame assembly; intermediate storage is bounded by a block plus years/deciles, not number of chunks.
- Removed the duplicate future central weather-year pass and its partial-success publication. Historical mean/adverse illustrative technical diagnostics and optional coefficient SE work remain separate. Run signatures include `row_aligned_annual_v1`; owner/pipeline correction metadata identify version, run, exposure basis and central/gradient limitations. Context finalization occurs before any result publication; failed runs retain the last successful bundle and run ID.
- Technical adverse decile filtering now uses the selected member/year key, avoiding pooled deciles from other members in that year. This remains the legacy technical diagnostic, not the Phase 5 selected-metric adverse attribution.
- Extended the existing benchmark harness with preparation, old full-panel characterization, bounded exact-reference parity, full annual blocking, compact future sizes and optional R profiling. `WISEAPP_STEP3_INTERACTIONS` enables a real fitted weather-policy interaction fixture; defaults remain unchanged. Error cases produce scalar report fields and a nonzero harness exit rather than malformed CSV construction.

Files changed in Phase 4, preserving the preexisting uncommitted Phase 3 files:

- `R/fct_policy_sim.R`, `R/mod_3_06_policy_sim.R`, `R/fct_policy_metric_decompose.R` (previously untracked), and the small technical member/year filter in `R/mod_3_09_decomposition.R`.
- `dev/bench_step3_helpers.R`, `dev/bench_step2.R`.
- `tests/testthat/test-fct-policy-metric-decompose.R`, `test-policy-central-kernel.R`, `test-determinism.R`, `test-policy-diagnostic-snapshot.R`. The latter's stale mock was updated to the actual context-owned call path because atomic failure coverage is required here; the previously documented three fixture failures now pass.
- This plan. `devtools::document()` regenerated the changed exported helper and Phase 3 pipeline/kernel help locally; `man/` is git-ignored (confirmed with `git check-ignore`), so generated help does not appear in status. Roxygen reported existing unresolved plot/theme links. No commits were made; benchmark artifacts are under ignored `dev/outputs/`.

API contracts for Phase 5:

- `apply_policy_delta_to_baseline(..., decomp_context = NULL, run_identity = NULL, annual_channels = NULL, chunk_size = 100000L)` returns `hist_sim`, `saved_scenarios`, `annual_channels` (locked prepared environment), `decomp_scenarios` (existing compact class), and `correction_version`. The prepared source must belong to the exact supplied context; stale/mismatched sources error. Standalone calls build a central-only context. Required top-level NULL prerequisites still return NULL; invalid channel/mapping data error.
- Pipelines retain every old field unchanged except `y_point`, plus `policy_correction` with version/run/exposure source/counts/scope and `baseline_X_gradient` uncertainty limitation. Owner lists gain `policy_correction_version`. Baseline objects are not mutated. No Step 2 prediction or metric formula changed.
- `.policy_annual_channel_block()` is an internal hot loop, only for callers that have already validated the owned source and full exposure mapping. `.policy_annual_channels()` remains the strict public/internal adapter for arbitrary row selections. Phase 5 must not bypass mapping validation or recompute ranks.
- Compact future tables now carry `member`, `correction_version`, `scope = production_prediction_rows`, and `uncertainty = central_only`; channel statistics are additive sums/weight sums. They are technical model-scale reductions, not invertible household states and not selected-metric inputs. Every member has its own exact mapping, including duplicate expanded rows.
- `mod_3_06_policy_sim_server()` returns a new `annual_channels` reactive from the successful decomposition bundle. Its source retains the original locked preparation context; the published technical `decomp_context` is a finalized clone carrying adverse diagnostic results with the same run identity. Do not require pointer equality between these two contexts in Phase 5: use the prepared source's context for channel evaluation and run validators for ownership. Neither environment is a worker snapshot. Parent-to-Results wiring for this new reactive is intentionally left to Phase 5.
- Old policy cache signatures cannot equal the new versioned run signature. Existing Results aggregation caches are already owned by published arm reactives and invalidate on a successful new arm. Metric-aware cache ownership, parity, residual resolution and threshold keys remain Phase 5 work.

Validation:

```sh
Rscript -e 'devtools::test(filter = "policy|w3-|step3-wave2|determinism|step2-contract|visualization-contracts|mod_3_07_results|mod_2_02_results", reporter = "summary", stop_on_failure = TRUE)'
Rscript -e 'devtools::test(filter = "mod_2_02_results|mod_3_07_results|policy-sim-compare-agg-cache|visualization-contracts|policy-exposure-mapping|step2-payload|fct_run_simulation|fct_aggregation_delta|fct-aggregation-kernel|active-mask|uncertainty-decomposition", reporter = "summary", stop_on_failure = TRUE)'
Rscript -e 'devtools::test(filter = "fct-policy-metric-decompose|policy-central-kernel|policy-decomposition-uncertainty|policy-diagnostic-snapshot|determinism|w3-b-future-decomposition", reporter = "summary", stop_on_failure = TRUE)'
git diff --check
```

All passed. Warnings were the existing glmnet `thresh` deprecation and the deliberate incomplete weather-cache manifest fixture's missing-file warning. New tests cover RIF/fixest identity/log and bins, chunk-independent reconstruction and weighted summaries, member-specific exposures, zero policy, single whole-map validation, stale/missing inputs, no predictor/period reducer/duplicate channel pass, atomic failure preservation, and technical adverse member/year alignment. Read-only review found chunk-summary accumulation and member/year decile mismatch; both were fixed and tested. No full suite or interactive browser run was performed.

Initial real-data performance (single repetition, cold, OLS, latest baseline wave; all selected waves used for fitting, uncertainty disabled, mean aggregation):

| Payload | Annual Rows Across Members | Prep (s) | Old Full-Panel Corrections (s) | Initial Annual Pass (s) | Optimized Annual Pass (s) | Sampled Step 3 Tree RSS (MiB) |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| BFA, 7,128 baseline rows | 5,132,160 | 0.045 | 1.116 | 26.487 | 3.449 | 4,448 |
| IRN, 37,468 baseline rows | 26,976,960 | 0.033 | 4.265 | 107.491 | 18.160 | 6,920 |

Both use temperature plus a fitted electricity interaction, combined covariate/SP policy, 23 future members and one SSP/period. Exact row-reference parity was zero error on 16 rows per pipeline (384 per country); this is **bounded sample parity**, not a full real-data row-reference run. Synthetic tests verify all fixture rows. The old timing is central correction only, while annual timing includes validation, correction and compact technical reduction; old/new equality is neither expected nor required. Initial-versus-optimized annual timings are comparable full passes. The combined launcher maximum RSS fell from 8,796,258,304 to 8,120,156,160 bytes (whole benchmark process, not isolated Step 3 incremental memory). Sampled Step 3 RSS is a diagnostic, not a deployment memory gate.

Artifacts: `dev/outputs/step3-annual-phase4-interaction/` (before matrix reducer), `step3-annual-phase4-profile/`, and `step3-annual-phase4-optimized/`. Reproduce the optimized run:

```sh
WISEAPP_DATA_PATH="$HOME/Library/CloudStorage/OneDrive-WBG/wiseapp - Documents" WISEAPP_STEP2_COUNTRIES=BFA,IRN WISEAPP_STEP2_MODELS=ols WISEAPP_STEP2_WEATHER=t WISEAPP_STEP3_INTERACTIONS=electricity WISEAPP_STEP2_WORKLOADS=one_ssp_one_period WISEAPP_STEP2_INCLUDE_STEP3=1 WISEAPP_STEP3_POLICIES=combined WISEAPP_STEP2_REPETITIONS=1 WISEAPP_STEP2_CACHE_STATES=cold WISEAPP_STEP2_UNCERTAINTY=disabled WISEAPP_STEP2_AGG_METHODS=mean WISEAPP_STEP3_REFERENCE_ROWS=16 WISEAPP_STEP2_OUTPUT_DIR=dev/outputs/step3-annual-phase4-optimized dev/run_step2_benchmark.sh
```

Remaining limitations and next session:

- Implement **Phase 5 only** next. Read the plan and current diff; aggregate cumulative states using the retained source and canonical shared residual context, match Results endpoint support, implement tails and the single shared Results reactive. No primary UI redesign until Phase 6.
- The default two-hazard (`t,spei6`) IRN future run failed strict exposure validation (`Missing exact weather exposure or anchor identity`); baseline Step 2 itself succeeded. No missing-weather imputation or period-mean fallback was applied. Temperature-only explicit-interaction BFA/IRN runs succeeded. The default-hazard missing-data provenance still needs characterization; do not claim the default IRN combination passed.
- Annual processing remains synchronous and IRN blocking of 18.16 seconds is material despite the measured optimization. No new pool or worker was added. Release requires further profiling and/or immutable snapshots through the existing coordinator before claiming acceptable responsiveness; the process-local locked contexts cannot simply be shipped as worker environments. Record snapshot/parent identity validation if moved async. Do not remove annual semantics to regain speed.
- Initial checks do not complete the release gate: multi-period/multi-SSP, real-data RIF/bin performance, repeated timing/RSS, metric-switch cost, full suite and browser verification remain open for later phases. Metric-switch/tail timing cannot be measured before Phase 5 exists. Existing historical illustrative resilience cards remain technical until Phase 6; new future production predictions and technical summaries intentionally changed.

### Phase 5: Metric Counterfactuals, Tail Attribution, Shared Reactive

Files: helper from Phase 3, canonical aggregation integration only as necessary, Results shared calculation/wiring.

Aggregate annual ordered states with canonical residual/weight/threshold semantics; compute annual contributions and shared `S`; reuse endpoint aggregates for parity checks. Implement cumulative-state quantile attribution matching actual threshold endpoint operators and the separately labeled fixed-baseline-adverse-year view. Return the small status/annual/summary/return-period/mechanism contract and cache boundedly. Gate: annual, expected, and tail decompositions reconcile within their own scopes; endpoint support preserved; no prediction rerun on metric/threshold/probability edits; no stale cache leakage.

Tests: new helper tests, `test-fct_aggregation_delta.R`, `test-policy-sim-compare-agg-cache.R`, `test-policy-central-kernel.R`.

#### Phase 5 Completion Handoff (2026-10-01)

Completed scope:

- Added pure native-valued cumulative metric aggregation, using the retained annual source and actual production endpoints. One temporary member/year cumulative vector and one canonical year-seeded residual realization are reused for all states. Survey weights, strict poverty thresholds, log back-transforms and canonical state-specific eligibility are preserved. No coefficient gradients/SEs, model refit, prediction rerun or rank recomputation is performed.
- Annual main/repositioning/interaction/resilience/total contributions and equal-model-mean summaries reconcile. Results endpoint aggregate tables are reused to validate parity and select precisely the finite matched headline support. Channel failures preserve independent endpoint summaries; they never silently filter the headline. Prediction alignment, correction version/run ownership, residual context, missingness and final-state parity are checked before publishing a scenario result.
- Added adverse 1-in-5/10/20/50 attribution using cumulative rank-interpolated state endpoints and median model quantiles, matching the Results threshold operator. Sparse model support is unavailable, not silently reduced. Equal-probability attribution is withheld if matched channel support cannot reproduce Results marginal threshold endpoints; the valid expected summary remains distinct.
- Added hazard/category-specific annual and equal-model-mean sensitivity-change/rank summaries, signed positive/negative household shares, unchanged fitted RIF grid and explicit missing engine/interaction statuses. These use native model-scale units, not selected-metric units; confirmed weather units are resolved from the Step 2 run snapshot, otherwise a truthful fitted-input-unit fallback is retained.
- Results owns and returns one `metric_decomposition` reactive, passed through the parent into Decomposition together with existing metric controls. Reactive memoization retains only its latest compact result. Source/context/metric/effective threshold invalidation is automatic; deviation/uncertainty plot controls do not trigger this calculation. Stale/mismatched runs withhold all channel tables. No primary UI/card redesign or new exports was performed.
- Extended the existing benchmark harness with mean/headcount switches, timings and result sizes. Updated the stale benchmark expectation from household rows to Phase 4's compact member/year rows and added metric benchmark assertions.

Files changed in Phase 5, preserving all preexisting Phase 3/4 changes:

- `R/fct_policy_metric_decompose.R`, `tests/testthat/test-fct-policy-metric-decompose.R` (both already untracked at session start).
- `R/fct_policy_sim_compare.R`, `R/mod_3_07_results.R`, `R/mod_3_scenario.R`, `R/mod_3_09_decomposition.R` (wiring only; existing technical member/year edits preserved).
- `tests/testthat/test-policy-sim-compare-agg-cache.R`, `test-mod_3_07_results.R`, `test-bench-step3.R`.
- `dev/bench_step3_helpers.R`, `dev/bench_step2.R` and this plan. No commits or dependency changes.

API contracts for Phase 6:

- `.policy_metric_decomposition(baseline_hist, policy_hist, baseline_scenarios, policy_scenarios, prepared, method, pov_line = NULL, requested_residuals = "original", endpoint_series_baseline = NULL, endpoint_series_policy = NULL, focus_scenario = NULL, analysis_unit = NULL)` returns `status`, `reason`, `annual`, `summary`, `return_period`, `mechanisms`, `metadata`, `endpoint_summary`, and named `scenarios`. Production Results always supplies its existing endpoint series. Optional absent endpoint tables support the benchmark/reference path only; that path does not prove independent Results parity.
- `status` describes focus-scenario channel availability; individual `scenarios[[name]]` have their own status/reason. Successful scenario tables may coexist with an unavailable focus. `endpoint_summary` is independently computed with existing `paired_effect_summary(..., center = "equal_model_mean")`, including on channel failure. Logistic policy methods are unsupported and publish no policy endpoint summary. Stale Results wrapper sets `unavailable`; retained endpoint summaries then refer only to the last completed run and must not be presented as current results.
- Numeric state columns are `baseline`, `after_main`, `after_repositioning`, `policy`; contribution columns are `main`, `repositioning`, `interaction`, `resilience`, `total`. Summary adds model/year/dropped counts and `center_method`. Numeric repositioning is an internal identity step for fixest; **display availability from metadata/mechanism status**, not a measured zero mechanism. Missing interaction terms likewise have an explicit status even though the canonical identity step is numeric zero.
- `return_period$scope` is `equal_probability` (median cumulative model quantiles, canonical `rank_interp`). Rows have status/reason, probability/return period, center/quantile method and support. Sparse/mismatched quantile rows contain no publishable contribution values. The existing separate paired-adverse table still uses `stats::quantile` and median model effects; do not confuse it with this threshold-card operator or change it implicitly.
- `mechanisms` contains `annual`, `summary`, `fitted_curve`, `curve_scope`, `repositioning_status`, `interaction_status` and diagnostic `metadata`. It is not part of the metric contribution sums. Its fitted RIF curve is the unchanged Step 1 grid; positions are fixed main-derived pre/post ranks. Category rows are fitted-reference contrasts, never derivatives divided by hazard exposure. Annual and summary rows carry model/weather unit labels.
- Metadata includes centralized metric/unit/threshold context, run/focus, order, correction/exposure source, named relative parity tolerance `1e-8`, requested/effective residual modes, mixed-mode flag, canonical population/eligibility caveat and central-only uncertainty. `avg_poverty` excluded counts can change by state, as the canonical metric defines eligibility; no invented fixed positive-welfare population is claimed.
- `.wire_results_pane()` and `mod_3_07_results_server()` accept trailing `annual_channels = reactive(NULL)` and return `metric_decomposition`. Parent passes `s6$annual_channels`, and Decomposition accepts trailing `metric_decomposition = reactive(NULL)`. Prepared and finalized technical contexts need matching validated run identities, not pointer equality. Do not independently reconstruct household channels in Phase 6.

Validation:

```sh
Rscript -e 'devtools::test(filter = "fct-policy-metric-decompose|fct_aggregation_delta|fct-aggregation-kernel|policy-central-kernel|policy-decomposition-uncertainty|policy-exposure-mapping|step2-payload|w3-", reporter = "summary", stop_on_failure = TRUE)'
Rscript -e 'devtools::test(filter = "fct-policy-metric-decompose|policy-sim-compare-agg-cache|mod_3_07_results|mod_2_02_results|visualization-contracts|policy-decomposition-uncertainty|ui-migration-step3", reporter = "summary", stop_on_failure = TRUE)'
Rscript -e 'devtools::load_all(quiet = TRUE)'
git diff --check
```

- Focused suites and package load passed. Coverage includes all registered metrics, identity/log fixest/RIF, all four residual modes, compact shared residual context, fallback to none, zero policy, strict poverty crossing, changing inverse-welfare eligibility, unequal model-year counts, category/rank mechanisms, member-specific exposures, endpoint support/parity failures, sparse/tied tails, marginal-versus-matched threshold mismatch and actual shared Shiny reactive/headline equality through metric/line edits with preparation/prediction calls forbidden. The payload suite emitted only the known deliberately incomplete weather-cache warning. Independent read-only helper review found no additional concrete correctness issue.
- Full `devtools::test(reporter = "summary", stop_on_failure = TRUE)` completed but was **not a pass**: five failures, comprising the stale benchmark expectation (120 household rows versus 2 compact member/year rows) and four preexisting rate-formatting assertions in `test-result-distribution-charts.R:96-97` and `test-result-exceedance-charts.R:38-39`. The benchmark fixture was corrected and `bench-step3|fct-policy-metric-decompose|policy-sim-compare-agg-cache` then passed. The four chart failures were reproduced after replacing all definitions from `R/fct_policy_sim_compare.R` in-memory with their `HEAD` versions; untouched chart tests also remain unchanged. These unrelated chart bugs were not fixed in Phase 5. Full-suite warnings were existing glmnet `thresh` deprecation and missing-file weather-cache fixture; browser-backed tests also logged a Chromote `Browser.close` timeout. This automated full-suite run is not an interactive application browser check.

Real-data performance (single cold repetition each, OLS, temperature plus electricity interaction, combined policy, one SSP/period, 23 future members; same Phase 4 workload):

| Payload | Annual Rows | Initial Mean Switch (s) | Optimized Mean Switch (s) | Initial Poverty Switch (s) | Optimized Poverty Switch (s) | Retained Metric Result |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| BFA | 5,132,160 | 8.241 | 4.916 | 8.431 | 4.969 | ~534 KB object / ~798 KB serialized |
| IRN | 26,976,960 | 53.347 | 27.727 | 52.767 | 29.199 | ~534 KB object / ~798 KB serialized |

- Optimization removes duplicate whole-policy exposure validation (exact baseline/policy mapping equality is checked) and repeated per-year residual-context hashing/lookup preparation; the canonical draw helper, row mask, year seed and effective mode are unchanged and parity tested. Each result has 720 annual rows, 2 scenario summaries, 16 tail rows and 720 mechanism rows. No invariant preparation or prediction rerun occurs during a switch. Timings include all scenario calculations/mechanisms/tails but exclude preexisting Results endpoint aggregation; the real benchmark path does not supply independent cached endpoint tables.
- Artifacts: ignored `dev/outputs/step3-metric-switch/` and `step3-metric-switch-optimized/`. Sampled Step 3 tree RSS was approximately 4,037/6,427 MiB before and 4,785/7,384 MiB after for BFA/IRN; whole-launcher maximum RSS was 8,280,702,976 / 8,527,773,696 bytes. These are noisy whole-workload single-run measurements, **not evidence of a memory improvement or isolated metric incremental peak**. Annual reference sample errors remained zero (384 sampled rows/country), not a full real-data reference run.
- Reproduce the optimized benchmark using Phase 4's command above with output directory `dev/outputs/step3-metric-switch-optimized`; harness automatically records metric fields when Step 3 is enabled. Smoke benchmark is correctness only, not performance evidence.

Remaining limitations and next session:

- Implement **Phase 6 only** next. Read this plan, repository guidance and current dirty diff. Use `s7$metric_decomposition` for primary contribution/sensitivity/adverse views and resilience card; keep Results authoritative and existing separate technical math intact. Native formatting must use the effective metadata snapshot, and scope/availability must be respected per scenario and tail row.
- Optimized 28-29 second IRN metric switching and 17.5 second annual production application still materially block the main R process. This phase does not claim acceptable responsiveness and adds no pool or worker. Release still requires further profiling and/or immutable snapshots through the existing coordinator with explicit parent run-identity validation; process-local prepared environments cannot be shipped as live worker contexts. Do not silently revert annual semantics.
- Multi-period/multi-SSP, default IRN two-hazard missing-exposure provenance, real-data RIF/bin performance, repeated/RSS-isolated measurements and browser checks remain open. No primary UI change or browser check in Phase 5; full native browser/bundle contribution exports remain Phase 7. Component uncertainty is not estimated.

### Phase 6: Primary View and Headline Alignment

Files: `R/mod_3_09_decomposition.R`, `R/fct_policy_sim_compare.R`, module wiring as needed. Do not edit technical channel math.

Add metric tag, paired levels, main/repositioning/interaction chart/table, highlighted resilience subtotal, weather-sensitivity panel, and adverse return-period/paired-year views. Keep dedicated resilience card and important adverse-threshold outputs. Gate: expected and tail totals match their corresponding Results outputs; resilience/drivers visible by default; quantile versus same-year scopes distinguishable; fitted RIF curve not presented as policy-updated. Retained standalone illustrative diagnostic values stay unchanged; production future technical values intentionally align with new annual channels and carry updated method metadata.

Tests: `test-mod_3_07_results.R`, `test-policy-decomposition-uncertainty.R`, `test-w3-b-future-decomposition-characterization.R`, Shiny module/reactive tests.

#### Phase 6 Completion Handoff (2026-10-01)

Completed scope:

- Replaced the Results resilience card's historical model-scale fallback with the shared metric-aware focus-scenario summary. It displays main, repositioning and interaction contributions separately, marks missing engine/term status explicitly, links to the primary sensitivity panel, and never converts unavailable channels to zero. The adverse threshold card takes its headline total from the matching `equal_probability` cumulative-state quantile row when available, retains the Results endpoint value otherwise, labels the same-probability/not-necessarily-same-years scope and links to the tail panel.
- Added the primary `Policy Effect in the Selected Metric` view with scenario selection defaulting to the Results focus, metric/unit/threshold/population/method/correction context, matched cumulative levels and native numeric contributions, highlighted resilience subtotal, and per-result unavailable reason. Added `Resilience in Adverse Weather` rows for equal-probability quantile contrasts.
- Added `How the Policy Changes Weather Sensitivity` with hazard/category-specific channel-implied model-scale changes, positive/negative household shares, rank movement, weather/model units, scope and rank-convention notes. RIF results reuse the unchanged Step 1 fitted curve and label it unchanged; fixest/missing-interaction statuses remain distinct. Retained historical/adverse technical diagnostics under `Technical Decomposition on the Model Scale` without changing technical calculation paths.
- Added module coverage for native expected contributions, adverse rows and mechanism rendering; expanded UI structure assertions; added card tests verifying metric-aware resilience and tail values/links. Existing technical OLS/RIF render tests remain active.

Files changed in Phase 6:

- `R/fct_policy_sim_compare.R`, `R/mod_3_09_decomposition.R`.
- `tests/testthat/test-mod_3_07_results.R`, `tests/testthat/test-policy-decomposition-uncertainty.R`.
- This plan. No technical channel formulas, prediction code or dependencies were changed. No commits were made.

UI/API contracts for Phase 7:

- The UI consumes only the Results-owned `metric_decomposition` reactive passed from `mod_3_07_results_server()` through `mod_3_scenario_server()`. It does not compute or refit channels. Scenario-specific data comes from `result$scenarios[[scenario]]`; summary, tail and mechanism statuses/reasons must remain scoped to that scenario. Metric metadata is the completed calculation snapshot (`result$metadata`), falling back only when absent to the existing Results metric context.
- The contribution table includes displayed metric values and native numeric cumulative states/contributions. The tail table preserves native baseline/after-main/after-repositioning/policy and contribution columns, probability, quantile/center method, model support and unavailable reason in the rendered-data helper; Phase 7 must preserve these fields in browser CSV and bundle exports.
- The mechanism table consumes `result$mechanisms$summary`, filters its rows to the chosen scenario, and retains hazard/category/contrast, signed channel-implied values, positive/negative shares, pre/post ranks, model/weather units and model support. The fitted grid is unchanged Step 1 state; never label it as policy-updated.
- The current Step 3 browser CSV buttons use the same underlying result tables as the Phase 7 `wise_export_table()` records (`policy_metric_contributions`, `policy_metric_adverse_attribution`, `policy_weather_sensitivity`); all three are registered with numeric/context metadata. Do not replace the technical export keys or alter their scale.
- Results cards use the shared focus metric result for resilience and the shared equal-probability tail total when that row is available. Expected endpoint summary stays independent and remains available when channels fail. Preserve explicit unavailable behavior and do not infer component zero from absent values.

Validation performed:

```sh
Rscript -e 'devtools::test(filter = "mod_3_07_results|policy-decomposition-uncertainty|w3-b-future-decomposition-characterization|policy-sim-compare-agg-cache|fct-policy-metric-decompose", reporter = "summary", stop_on_failure = TRUE)'
Rscript -e 'devtools::load_all(quiet = TRUE)'
git diff --check
```

All listed focused suites passed after implementation. The suite covers metric-aware Results card behavior, primary contribution/tail/mechanism module rendering, expected module structural labels, and preservation of existing technical OLS/RIF rendering. A real interactive browser check has not been performed. There were no data-backed UI checks for all metric formats, sparse-return periods, multiple scenarios or RIF curves in this phase.

Remaining limitations and next session:

- Implement **Phase 7 only** next. Register rich native-valued exports for expected summary, annual contributions, equal-probability quantile tails and weather mechanisms. Verify both browser CSV and export bundle tables and preserve legacy technical export descriptions/keys.
- Re-run focused and full suites, resolve Phase 5's documented existing chart failures only if they are still current and necessary to establish the acceptance run, perform interactive checks using the required data/backend when available, and record any skipped checks honestly.
- Extend/use only the existing benchmark harness to record BFA/IRN multi-period/multi-SSP correctness-reference parity, timings, peak memory and main-process blocking. Include real RIF/bin runs where supported, metric-switch time, and no-rebuild behavior for display-only controls. Current single-scenario OLS timings show materially synchronous work and do not pass the responsiveness release gate.
- Default IRN two-hazard exposure provenance, isolated/repeated RSS, deployable browser behavior and end-to-end Step 2/Step 3 metric-control parity remain release checks. No async worker refactor is authorized by this bounded phase; if responsiveness cannot meet the gate, document a separately scoped immutable-snapshot/coordinator follow-up rather than moving process-local live contexts to workers.

### Phase 7: Exports, Performance, End-to-End Verification

Files: existing export registrations and metadata helpers, new export tests as necessary.

Add native-valued expected/annual contribution, equal-probability tail and mechanism exports with numeric threshold/probability/scope/order/annual-correction/uncertainty metadata. Verify browser CSV and bundle data. Run focused/full tests, interactive checks, and final BFA/IRN multi-scenario benchmarks with timings/peak memory/blocking evidence. Gate: correctness-reference parity, bounded memory and acceptable measured runtime, no policy correction rebuild on display-only edits, all acceptance criteria verified; report environment-dependent skips honestly.

#### Phase 7 Completion Handoff (2026-10-01)

Scope update (2026-10-02): the separate baseline-selected adverse-year mean was removed. Production results and exports now retain only equal-probability cumulative-state quantile contrasts. The Step 3 resilience card uses the same 1-in-20 tail row as the adverse card, so its main/repositioning/interaction components reconcile to the tail total when channels are available. The info popover notes that state quantiles need not represent the same weather years.

Completed scope:

- Registered the three primary Results-owned metric-aware tables in the export bundle under `policy_metric_contributions`, `policy_metric_adverse_attribution`, and `policy_weather_sensitivity`. Contribution exports include the equal-model expected summary and annual model/year states. Tail exports include equal-probability cumulative-quantile records. Mechanism exports include equal-model summary and annual diagnostics, signed repositioning/interaction values, positive/negative shares, pre/post ranks, fitted category contrast, model/weather units and rank/term conventions.
- Added shared export annotation for native metric/unit fields, outcome identity/type/transform, numeric threshold and units, currency/time/welfare basis and missing-context note, survey unit/weight interpretation, scenario/run/scope, exposure/correction identity, requested/effective residual modes, component order/method, center, scale, uncertainty, channel availability/reason and canonical population/eligibility notes. Annual and mechanism detail rows are explicitly tagged so their center is not confused with the expected headline.
- The three visible Reactable data frames use the same native values and provenance as the corresponding bundle artifacts. Unavailable/stale results export an explicit status and reason instead of stale values or fabricated zeros; endpoints remain separately exportable where only channel attribution is unavailable. Technical export keys/data scales were preserved.
- Added module/export tests for all three browser CSV controls, bundle registration and CSV round-trip contents, native contribution/tail/mechanism columns, threshold/correction metadata, tail scopes, year keys, and model-scale mechanism units. Focused tests passed:

```sh
Rscript -e 'devtools::test(filter = "policy-decomposition-uncertainty|export-wiring-contract|csv-export-wiring-contract|export-bundle|mod_3_07_results|policy-sim-compare-agg-cache|fct-policy-metric-decompose|fct_aggregation_delta|fct-aggregation-kernel", reporter = "summary", stop_on_failure = TRUE)'
Rscript -e 'devtools::load_all(quiet = TRUE)'
git diff --check
```

- Full `devtools::test(reporter = "summary", stop_on_failure = FALSE)` completed with exactly four failures, all previously characterized unrelated rate-formatting assertions in `test-result-distribution-charts.R:96-97` and `test-result-exceedance-charts.R:38-39`; no Phase 7 export failures occurred. Existing glmnet deprecation and intentionally incomplete prepared-weather-cache warnings appeared. Browser-backed tests passed but logged the known Chromote `Browser.close` timeout. Interactive human/browser verification was not performed.
- Attempted the existing real-data harness with local LKA input, OLS/RIF, three SSPs by three periods, warm cache, one repetition, mean/headcount aggregation, uncertainty disabled, and Step 3 enabled. The OLS Step 2 stage ran 8m44s, then failed with `vector memory limit of 16.0 Gb reached`; the command's 10-minute cap terminated the launcher during RIF Step 2. No Step 3 summary/reference-parity, Step 3 blocking, RIF result, BFA/IRN multi-scenario timing or isolated peak-memory evidence was produced. This is a failed release-gate attempt, not evidence that the annual implementation has bounded multi-scenario memory or acceptable runtime. The workload also reported excluded CMIP6 members with missing selected weather variables. Earlier single-scenario BFA/IRN metric-switch numbers in Phase 6 remain the only real-data performance measurements.

Files changed in Phase 7:

- `R/mod_3_09_decomposition.R`.
- `tests/testthat/test-policy-decomposition-uncertainty.R`.
- This plan. No dependencies or technical export keys/scales were changed. No commits were made. Preexisting Phase 6 changes in `R/fct_policy_sim_compare.R` and `tests/testthat/test-mod_3_07_results.R` remain in the shared dirty worktree and are not part of the Phase 7 file list.

Remaining release gates:

- Reduce/measure memory for the existing real multi-SSP Step 2 workload or use a documented bounded real workload that still exercises Step 3; then run BFA and IRN multi-period/multi-SSP benchmarks through `dev/run_step2_benchmark.sh` with Step 3 enabled. Record full annual reference parity, metric-switch timings, main-process blocking, and process-tree peak RSS. Include supported real-data RIF/binned-weather cases. Do not treat smoke fixtures or sampled rows as production parity evidence.
- Perform interactive browser checks for multiple scenarios, all registry metric formats, sparse/available return periods, RIF curve/mechanisms, stale handling and live CSV downloads. The headless module/bundle tests are not a substitute.
- The display-only no-channel-rebuild/reactive behavior and single-scenario switch tests from earlier phases pass, but a full Step 2/Step 3 live control-parity/browser check remains outstanding. Responsiveness is not accepted while large annual policy runs materially block the main process; any async change requires a separately scoped immutable snapshot/coordinator design.
- The full suite retains the four unrelated chart formatting failures listed above. They were not changed in Phase 7.

Independent review follow-up (2026-10-01):

- Gated primary metric tables and Results headline cards while the Step 3 run is stale; stale table/CSV data now contains only unavailable status and reason, not prior-run values.
- Kept resilience unavailable when neither repositioning nor interaction is modeled, instead of presenting the internal identity sum as a measured zero.
- Labeled binned-weather mechanism rows as category contrasts without per-unit slope units.
- Made intended Reactable columns explicitly visible despite hidden-by-default provenance columns; added regression assertions for display and stale status.
- Focused Step 3, aggregation and export suites passed after these fixes. Full browser interaction and the multi-scenario performance/memory release gates above remain incomplete.

Independent phases 1-7 review (2026-10-02):

- Reviewed summary/headline alignment, annual channel ownership/exposure/residual/tail correctness, and primary UI/export contracts in independent passes. Fixed percent level metadata and chart scaling (including pp deviations and policy changes), unit-aware robustness ranges, incomplete training-residual fallback parity, scenario-scoped export availability/reasons, and visible reasons for unavailable tail rows.
- Bounded the Results aggregation-suite cache to eight LRU entries, reusing the existing eviction helper; repeated threshold edits no longer retain every previous suite. The benchmark now fails its overall status when a required metric-switch verification is unavailable or errors, rather than reporting a successful run.
- Added regressions for these cases. The final non-overlapping `Rscript -e 'devtools::test(reporter = "summary", stop_on_failure = TRUE)'` passed, including the previously failing chart assertions and browser-backed bundle tests. Existing glmnet deprecation and deliberate incomplete weather-cache warnings remain. Focused regression suites, `devtools::load_all(quiet = TRUE)` and `git diff --check` also passed. Earlier runs during editing loaded older code; one concurrent browser-backed run had intermittent Chromote PNG failures, not reproduced in the final full run.
- No new real-data benchmarks or interactive application checks were performed. The multi-scenario memory/runtime, full-row real-data reference parity, real RIF/bin performance, and interactive release gates above remain open; this review does not declare production release readiness or introduce an async redesign.

Example commands from the repository root:

```sh
Rscript -e 'devtools::test(filter = "policy|aggregation|visualization-contracts|mod_3_07_results|w3-")'
Rscript -e 'devtools::test()'
```

Use `devtools::load_all()`/the existing dev launcher for interactive checks; do not silently treat a missing data/backend dependency as a passed check. Regenerate package documentation only if changed exported signatures require it.

## Required Test Matrix

| Case | Required evidence |
| --- | --- |
| Linear fixest, identity/log welfare | Main + modeled interaction parity; no repositioning claim; final state equals production pipeline. |
| RIF | Main -> repositioning -> post-main interaction; zero/missing interaction distinguished; effective residual mode tested. |
| Resilience mechanisms | Repositioning-only, interaction-only, both, neither included, and offsetting changes; driver values visible by default. Zero hazard can yield zero scenario contribution but nonzero channel-implied sensitivity change. Flat RIF weather curve yields zero repositioning despite rank movement. Continuous slopes and binned contrasts never mixed or divided by hazards. |
| Step 2 alignment | Same-scope expected Step 2 level equals Step 3 baseline; zero policy yields zero effects. Different metric/line/scope is labeled; Step 2 display deviations do not enter Step 3 predictions. Step 2 expected headline changes to mean while return-period/trajectory/diagnostic values remain characterized unchanged. |
| Strict poverty crossing | Values `[2, 3, 4]`, equal weights, line 3; main moves first value to 3. Rate changes from 1/3 to 0 because comparison is strict `<`; display approximately -33.33 pp, not -100%. |
| All registry metrics | Canonical weighted/native aggregation parity, including median, total, Gini, prosperity gap, average poverty, gap, fgt2. |
| Ensemble nonadditivity | At least three models where channel medians fail to sum; equal-model means reconcile. Unequal year counts prove this is not a pooled row mean. |
| Residuals | `none`, `original`, `normal`, `resample` for supported continuous models; same realization across states; zero policy delta gives zero contributions. Missing training residuals fall back to `none` for any engine; requested/effective modes are exported accurately. |
| Row/context integrity | Expanded/repeated survey rows; rejected mismatched ordering/weights/missing IDs; stale run rejected; nonfinite/eligibility counts disclosed. |
| Weather semantics | Mild/severe years receive distinct row-aligned resilience; multiple interview months/survey waves and duplicate keys handled losslessly; rolling windows crossing calendar boundaries preserved; bins use row exposure, not annual mode. Annual reference parity; fixed hazard parity with old convention when comparable. Scenario/member exposure provenance never mixed. |
| Adverse probabilities | Preserve supported return periods and direction-aware tails. Cumulative-state quantile contributions telescope to actual threshold endpoint difference, including median ensemble endpoints and rank-switching years. Fixed-baseline-adverse-year contributions use identical year keys; empirical fraction/ties/minimum support are disclosed. |
| Binary/logistic/tree | Synthetic validated binary endpoint percent/pp formatting; unsupported channel status; current analytic logistic policy endpoints unavailable even if in range; monetary/probability mismatch not hidden by clipping or fallback. |
| Units and controls | Confirmed PPP/LCU, missing welfare/time basis, household rows with per-capita welfare, unknown/normalized weights; no hard-coded `$` or false population-total label. Prosperity gap ignores editable poverty line in UI/key. |
| Reactivity | Method/consumed line changes update tag, cards, contribution values, exports; unrelated line/label edits do not rebuild channels; policy run counter unchanged. |
| Export/technical preservation | Native/display units explicit; numeric threshold/probability/context/order/correction version present. Standalone illustrative technical diagnostics unchanged; future production summaries intentionally changed to the new canonical annual source. |
| Performance | BFA/IRN and multi-period/multi-SSP reference-versus-optimized parity; timings/peak memory/blocking recorded; invariant preparation once per run; no N-household refit/prediction/join loop per year; bounded owned exposure/channel storage. |

Interactive minimum: confirmed-unit continuous welfare under mean/median/total; poverty rate with a line crossing; gap/fgt2; RIF resilience detail; mild/severe year-specific interaction and repositioning; multiple supported adverse return periods with reconciled channel attribution; fixed-baseline-year contrast clearly distinguished; binary mean unavailable-channel messaging; multi-scenario default/alternate selection; stale run and metric-change behavior.

## Acceptance Criteria

- One authoritative Results metric/threshold updates both surfaces without refitting or rerunning policy predictions.
- Default expected-effect and primary Decomposition share the same paired endpoint support, focus scenario, run residual settings, and equal-model-mean operator.
- Baseline-to-policy difference, total, and component sum agree within documented numerical tolerance at annual and headline levels for supported channels.
- Unsupported or mismatched channels do not become zeros, stale values, fabricated components, or silently changed headline populations.
- Rate levels are percent; rate contributions are pp. Other metrics remain in honest native units; weighted totals are not labeled per-person/per-household changes.
- Modeled outcome, aggregation, threshold/probability, known unit basis, scenario, row-aligned annual exposure basis, correction version, ordering, and uncertainty status are visible or readily accessible.
- Main/resilience mechanism attribution is distinguished from avoided weather losses; adverse return-period contrasts are not described as paired same-year protection.
- Resilience remains a dedicated headline, with repositioning/interaction contributions visible by default and a primary hazard-specific weather-sensitivity explanation. A small/zero aggregate resilience contribution is not equated with absent sensitivity change.
- Step 2 and Step 3 expected baseline levels agree for identical scope/settings using the equal-model mean; unit/threshold conventions agree, while median return-period/diagnostic summaries retain explicit distinct labels.
- Production policy predictions and future decomposition share one canonical annual channel source; separate technical diagnostics remain available with explicit scope and uncertainty assumptions. No silent period-mean fallback or legacy/new cache mixing.
- Adverse return-period outputs remain prominent and use new annual policy predictions. Main/repositioning/interaction attribution reconciles to each displayed quantile contrast; same-year severe-weather contributions are separately identifiable.
- Measured efficiency and memory results satisfy the benchmark gate without sacrificing annual exposure semantics or re-running Step 2 predictions.
- Browser CSV and bundle exports carry sufficient numeric and context metadata to interpret results outside the app.
- Focused/full tests and interactive checks have recorded outcomes, including explicit skipped/unavailable checks.

## Deferred Follow-Ups

These are not implementation prerequisites and must not be silently added to this task: valid logistic/binary policy response and transfer mapping; full policy-X/coefficient covariance; paired metric-aware component uncertainty; adverse-versus-reference avoided-loss diagnostic; Shapley/order sensitivity; richer survey weight/currency metadata; distributional metric-aware decile attribution. Annual row-aligned policy corrections and adverse attribution are now required scope, not deferred work.
