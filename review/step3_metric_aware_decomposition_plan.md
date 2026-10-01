# Step 3 Metric-Aware Policy Decomposition Plan

## Status and Objective

Finalized implementation handoff, reviewed against the current code on 2026-10-01. This document changes no application behavior. Implement only the bounded phases below, not the deferred research items.

Implementation status (2026-10-01): **Phases 1 and 2 complete; Phase 3 is next in a new agent session.** See the completion handoffs below. Summary centers, centralized display metadata, validated selection wiring, and logistic policy-output guards have been implemented; annual production corrections and metric-aware channels remain future phases.

Make Step 3 answer four questions clearly: what outcome is being summarized, how much the policy changes the selected metric, whether it changes modeled weather sensitivity, and whether that sensitivity change comes from repositioning or interaction. Resilience is a primary policy finding, not a hidden technical detail. Results and the primary Decomposition view must share the metric, population, scenario, prediction settings, and summary operator. Preserve existing technical outputs separately.

Conceptual reference: `.claude/policy_effects.md`. Its main/resilience distinction motivates the presentation; production channel conventions, not an idealized formula, determine the numbers.

## Decisions and Scope

| Decision | Implementation contract |
| --- | --- |
| Metric authority | Results controls only; no second aggregation or poverty-line selector. |
| Headline center | **User selected equal-model mean.** Average matched years within each climate model, then average models equally. Apply identical weights to all levels and contributions. This intentionally replaces the current Step 3 median-of-model-means expected-effect headline; align Step 2's expected-outcome headline center only, as specified below. |
| Initial uncertainty | Central channel estimates only. No metric-aware component SEs or intervals. Existing Results uncertainty and technical channel SEs remain separate, with their limitations stated. |
| Default scope | Primary Decomposition opens on the Results focus scenario: first non-historical scenario in Results order, otherwise historical. It summarizes that scenario's simulated years, not the technical adverse-weather selection. |
| Attribution order | Main package, then RIF repositioning, then weather-policy interaction. No transfer-versus-covariate sub-attribution in this release. |
| Annual weather correction | **User requires year-specific resilience in production predictions.** Apply channels to each prediction row using the weather exposure used by Step 2 for that row, not a scenario-wide mean or an independently reconstructed annual exposure. |
| Supported channels | Continuous identity/log outcomes using fixest linear models or RIF, where channel states reproduce the actual production policy pipeline. |
| Binary/logistic outcomes | Correct percent/pp formatting for valid probability aggregates, but metric-aware channels are explicitly unavailable initially. Existing analytic logistic policy endpoints are also methodologically unsupported, even if values lie in [0,1]. Do not add money to probabilities or apply linear coefficient deltas to logistic response predictions. |
| Existing technical views | Retain model-scale outputs and controls, but align future production-channel summaries to the new row-aligned annual states. Preserve genuinely separate illustrative historical/adverse diagnostics with explicit scope. Numerical changes caused by annual corrections are intentional and must be characterized, not forbidden. |
| Resilience prominence | Keep a dedicated `Resilience effect` Results card. Show repositioning and interaction separately by default when modeled. Add a primary weather-sensitivity mechanism panel; do not bury it in the technical section. |
| Adverse headline | Preserve the existing probabilities/return-period outcome comparison and expose its main/repositioning/interaction attribution. Updated policy inputs include annual resilience, so tail values can change. Distinguish equal-probability contrasts from effects evaluated on the same baseline-selected adverse years. |

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
- `return_period`: cumulative-state outcome levels and ordered contributions at each supported adverse probability, with the exact ensemble/quantile convention and separate `equal_probability` versus `baseline_adverse_years` scope identifiers.
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

**Same-weather adverse-year mechanism view (additional resilience question):** select adverse years by baseline annual aggregate within each model using a documented empirical tail rule, then hold that selection fixed for all cumulative states. Show mean policy effect and main/repositioning/interaction contributions for those same years. Do not select policy years independently or imply this mean equals a return-period quantile contrast. Report model/year support and empirical probability; use existing minimum-year support rules and mark sparse tails unavailable instead of manufacturing exact 1-in-50 evidence.

For a reproducible initial rule, with adverse probability `p=1/R` and `n` valid baseline years per model, require the canonical minimum support, choose the `ceiling(n*p)` most adverse baseline years, break threshold ties deterministically by simulation-year key, and disclose achieved fraction `k/n`. Use these same keys for all channels/states and summarize within model then equally across models. This is a selected-tail mean, not the interpolated quantile at `p`, and has no new confidence interval in this release.

Both views identify whether repositioning or interaction drives severe-weather policy effects. Neither by itself removes the main welfare uplift to define weather losses avoided relative to a chosen no-shock world. That separate reference-weather diagnostic remains deferred.

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
6. Add a primary `Resilience in Adverse Weather` table/chart across supported return periods with main, repositioning, interaction, resilience subtotal, and total. Clearly distinguish outcome-distribution quantile contrasts from contributions on the same baseline-selected adverse years, as specified below.
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

Both browser CSV data and bundle data must include native numeric levels/contributions and interpretation fields: outcome name/label/type/transform, metric ID/label, native and displayed units/multiplier, numeric threshold/unit/kind when used, currency/welfare basis and unknown-context note, survey analysis unit/weight interpretation, scenario/member/year where relevant, model/year counts, run/scope identity, exposure mapping/correction version, probability/return-period/achieved tail fraction when relevant, equal-probability versus baseline-selected-year scope, requested/effective residual modes, component order/method, center method, `metric_aware` versus `model_scale`, availability/reason, and uncertainty status. Keep native versus display-scaled columns unambiguous.

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

Next session: implement **Phase 3 only** below. Read this plan, `AGENTS.md`, optimization guidelines, and current git diff. Preserve all uncommitted Phase 1/2 work. Prepare exact exposure/index retention and canonical annual-channel adapter/reference parity without switching production policy predictions until Phase 4. Keep logistic guards and display/selection APIs intact.

### Phase 3: Annual Exposure Contract and Central Kernel

Files: `R/fct_policy_decompose.R`, pipeline construction in `R/fct_simulations.R` only for minimal retained exposure mapping, new focused helper file `R/fct_policy_metric_decompose.R`, tests. Build the annual kernel before publishing new policy outputs.

Characterize exact Step 2 weather exposure/row mapping first. Retain minimal lossless metadata if needed without changing baseline predictions. Reuse central kernel formulas and prepare run-invariant slopes/ranks once; implement continuous/binned row-aligned annual channels with no SE work. Validate scope/context ownership. Build a slow, transparent correctness reference used only in tests/benchmarks. Gate: optimized annual channels equal the reference for each expanded row; weather exposure matches baseline construction including location/month/category; fixed weather reproduces old corrections when exposure conventions match; logistic/tree engines explicitly unsupported; compact future summaries never used as household states.

Tests: `test-policy-central-kernel.R`, `test-policy-decomposition-uncertainty.R`; new `test-fct-policy-metric-decompose.R` for adapter/alignment cases.

### Phase 4: Production Annual Policy Integration

Files: `R/fct_policy_sim.R`, `R/mod_3_06_policy_sim.R`, run-owned decomposition context/storage helpers, benchmark harness.

Replace full-panel broadcast in production with annual row-aligned correction using Phase 3's source. Keep baseline pipelines/residual context unchanged; version correction metadata as `row_aligned_annual_v1` and invalidate old policy caches. Generate compact future production-channel summaries from the same annual states and remove duplicate central year-loop calculations. Publish new run state atomically; failed exposure mapping must fail the new policy run, never fall back to period means or publish a partial mixture. Run reference parity and initial BFA/IRN timing/memory checks. Gate: mild/severe years get appropriate distinct modeled resilience; zero policy changes nothing; source channels reconstruct new production policy predictions exactly; no full baseline re-prediction or repeated SE computation.

Tests: `test-policy-central-kernel.R`, `test-w3-b-future-decomposition-characterization.R`, run/cache ownership tests; extend `dev/bench_step3_helpers.R` through the existing harness.

### Phase 5: Metric Counterfactuals, Tail Attribution, Shared Reactive

Files: helper from Phase 3, canonical aggregation integration only as necessary, Results shared calculation/wiring.

Aggregate annual ordered states with canonical residual/weight/threshold semantics; compute annual contributions and shared `S`; reuse endpoint aggregates for parity checks. Implement cumulative-state quantile attribution matching actual threshold endpoint operators and the separately labeled fixed-baseline-adverse-year view. Return the small status/annual/summary/return-period/mechanism contract and cache boundedly. Gate: annual, expected, and tail decompositions reconcile within their own scopes; endpoint support preserved; no prediction rerun on metric/threshold/probability edits; no stale cache leakage.

Tests: new helper tests, `test-fct_aggregation_delta.R`, `test-policy-sim-compare-agg-cache.R`, `test-policy-central-kernel.R`.

### Phase 6: Primary View and Headline Alignment

Files: `R/mod_3_09_decomposition.R`, `R/fct_policy_sim_compare.R`, module wiring as needed. Do not edit technical channel math.

Add metric tag, paired levels, main/repositioning/interaction chart/table, highlighted resilience subtotal, weather-sensitivity panel, and adverse return-period/paired-year views. Keep dedicated resilience card and important adverse-threshold outputs. Gate: expected and tail totals match their corresponding Results outputs; resilience/drivers visible by default; quantile versus same-year scopes distinguishable; fitted RIF curve not presented as policy-updated. Retained standalone illustrative diagnostic values stay unchanged; production future technical values intentionally align with new annual channels and carry updated method metadata.

Tests: `test-mod_3_07_results.R`, `test-policy-decomposition-uncertainty.R`, `test-w3-b-future-decomposition-characterization.R`, Shiny module/reactive tests.

### Phase 7: Exports, Performance, End-to-End Verification

Files: existing export registrations and metadata helpers, new export tests as necessary.

Add native-valued expected/tail/paired-adverse-year/mechanism exports with numeric threshold/probability/scope/order/annual-correction/uncertainty metadata. Verify browser CSV and bundle data. Run focused/full tests, interactive checks, and final BFA/IRN multi-scenario benchmarks with timings/peak memory/blocking evidence. Gate: correctness-reference parity, bounded memory and acceptable measured runtime, no policy correction rebuild on display-only edits, all acceptance criteria verified; report environment-dependent skips honestly.

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
