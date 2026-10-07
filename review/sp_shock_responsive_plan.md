# Social protection module: shock-responsive scenarios and new design options

Scope: brainstorm and plan for extending the Step 3 social protection (SP) module (`mod_3_01_sp`) with shock-responsive cash transfers and richer program design options.

Status: proposal only. No code has been changed. Written for the WISE-APP maintainers and the policy team who will decide what to build.

Evidence note: statements about the code come from reading the files cited. Literature claims are limited to what a quick search confirmed (section 9). Anything I could not verify is marked "to verify" rather than filled in from memory.

## 1. Summary and recommendation

1. **Today SP is a static, weather-blind welfare top-up.** One daily amount per household is computed once, from the baseline survey, and added to every household-year in every weather draw and climate member. It cannot represent "pay only when a shock happens" because the transfer never sees the weather. The "Shock-responsive" toggle in the UI is display-only and is coerced to "regular" (`mod_3_01_sp.R:500-505`).
2. **The one structural change that unlocks almost everything** is making the transfer a function of (household, simulation year, climate member). The cleanest hook is the annual correction loop that already visits every prediction row with its weather exposure (`.apply_policy_annual_pipeline()`, `fct_policy_metric_decompose.R:214`), fed by a small pre-computed "trigger state" table per (member, year, location). Details in section 4.
3. **Recommended build order**
   - **Phase 0, independent quick wins** (also improve the regular program): admin cost markup, cost-effectiveness metrics, a realistic targeting-error model, per-capita vs per-household amounts. First fix the SP currency bug R2-BUG-04, because every dollar-denominated trigger or amount inherits it.
   - **Phase 1, shock-responsive MVP (decided scope):** two hazard-based triggers only, a weather variable threshold and an exceedance probability (return period) threshold, evaluated per survey location (`loc_id`); a fixed payout per activation; targeting by the existing static rules (universal, ex-ante poor, proxy, with errors) applied within triggered locations; and the new outputs policymakers care about most: activation frequency, annual cost distribution (expected and 1-in-20-year cost), and basis risk. The dynamic SP effect is folded into the Main channel, clearly labelled.
   - **Phase 2, impact-based and anticipatory:** triggers on modelled loss, poverty rate or number of poor; budgets linked to loss; forecast skill (hit rate, false-alarm rate) and an anticipatory effectiveness multiplier; geographic and predicted-welfare targeting; the separate decomposition channel for dynamic SP.
   - **Phase 3, design-space tools:** named presets (protective, preventive, promotive), a design sweep with a cost vs poverty-reduction frontier, reserve dynamics.
   - **Not planned for now:** a beneficiary registry and the vertical vs horizontal expansion distinction (section 7, item 1).
4. **Biggest gaps in your list** (section 7): basis risk; the financing view (cost distribution and climate-adjusted cost); delivery delay; existing programs already inside the survey welfare; realistic error structure; and the fact that the model is a static annual accounting model, which limits what "anticipatory" can mean. Vertical vs horizontal expansion is also a gap, but it is parked because the survey data cannot support it.

Decisions already taken (7 October 2026): see section 10.

## 2. How SP works today

### 2.1 Data flow

| Stage | Where | What happens |
|---|---|---|
| UI and spec | `mod_3_01_sp.R:496-550` | `sp_scenario_spec()` returns a flat list: `sp_type`, `budget_mode`, `budget_fixed`, `targeting`, `targeting_threshold`, `pmt_variable`, `pmt_cutoff`, `inclusion_error_pct`, `exclusion_error_pct`, `transfer_amount_usd`, `transfer_frequency`, `transfer_n_payments`, `transfer_timing`, `timeliness_weeks`. `is_regular` is hard-coded `TRUE`, so the last three timing fields are dead. |
| Eligibility | `.determine_sp_eligibility()`, `fct_policy_sim.R:863` | Universal, bottom x% of survey welfare, or a proxy variable cutoff. Inclusion and exclusion errors flip uniformly random units, counted by rows, not weights. Drawn once, under the stream `wise_seed(seed, "policy", "sp")`. |
| Amount | `.sp_transfer_values()`, `fct_policy_sim.R:441` | `transfer_first`: `amount * n_payments / 365` per day. `budget_first`: budget divided by weighted eligible units, over 365. Both divided by `hhsize` because welfare is per capita. |
| Write | `apply_policy_to_svy()`, `fct_policy_sim.R:1327` | Result stored in one survey column, `.wiseapp_sp_transfer` (`SP_TRANSFER_COL`). |
| Welfare effect, RIF | `.compute_rif_channels()`, `fct_policy_decompose.R:186` | `delta_sp` is `log(exp(y_baseline) + sp) - y_baseline`, folded into `delta_main`, which also moves the household's quantile rank (`tau_i_post`) and hence the repositioning channel. |
| Welfare effect, OLS | `.decompose_ols()` and `run_sim_pipeline()`, `fct_simulations.R:634-655` | Same level-scale add, re-logged for log outcomes. |
| Annual broadcast | `.prepare_policy_annual_channels()`, `fct_policy_metric_decompose.R:35`; `.policy_annual_channel_block()`, `:188` | `delta_sp[ids]` is looked up by baseline household id and broadcast across all (year, member) rows. The prepared object is locked and invariant to weather by design. |
| Reach and cost preview | `.sp_scenario_reach()`, `fct_policy_sim.R:523` | UI-32. Reuses the run's eligibility and arithmetic exactly (tested in `test-sp-reach.R`). |
| Diagnostics | `.sp_transfer_totals()`, `fct_policy_sim.R:364` | Annual cost as the weighted sum of the column times 365. |

### 2.2 What the current design can and cannot express

Can: a flat annual top-up to a fixed set of households, with imperfect targeting, sized by amount or by budget.

Cannot:

- Any dependence on weather, year or climate member. SP therefore contributes only to the "main effect"; it has no weather-dependent (resilience) channel of its own.
- A cost that varies across years. The cost is one number, so there is no financing-need distribution.
- Eligibility that changes over time (ex-post targeting, geographic targeting at trigger).
- Admin cost. The comment at `mod_3_01_sp.R:325-333` describes an admin percentage, but there is no input and no use of it in `.sp_transfer_values()`.

### 2.3 Details that matter for the redesign

- **Dead fields are an advantage.** `transfer_frequency`, `transfer_timing` and `timeliness_weeks` already exist in the spec list, and `sp_type` is already part of the run signature. New fields can follow the same pattern.
- **Frequency is accounting, not timing.** `amount * n_payments / 365` is the right convention for an annual-mean daily welfare outcome. It also means within-year timing is invisible to the model, which constrains the anticipatory design (section 5.3).
- **Welfare is per capita per day.** A per-household flat transfer is divided by household size, so large households get less per person. That is a real design choice (flat per household vs per capita) and is currently not exposed.
- **Currency.** R2-BUG-04 (`review/REVIEW-2026-10-06.md`): the amount is entered in the outcome's currency label but added to PPP welfare. Any trigger or budget expressed in dollars inherits the same problem until it is fixed.
- **The survey welfare already contains existing transfers.** The simulated program is incremental. Vertical expansion (section 7) needs to know who already gets a transfer, and the harmonized microdata may not carry that flag. To verify per data source.

## 3. Design space and where each of your ideas lands

### 3.1 Assessment table

Relevance: how often a policymaker asks for it. Effort: S under a week, M one to three weeks, L more. "Needs hook" means it depends on the dynamic transfer hook in section 4.

| # | Feature | Policy relevance | Feasibility and effort | Needs hook | Notes |
|---|---|---|---|---|---|
| **Budget** | | | | | |
| B1 | Fixed budget, fixed transfer | Done | Done | No | Keep. |
| B2 | Admin cost markup | High | S | No | Useful for regular and shock programs. Section 5.1. |
| B3 | Payout per activation, with annual cost distribution | Very high | M | Yes | The core shock-responsive output. |
| B4 | Share of modelled welfare loss at trigger | High | M | Yes | Needs loss definition (section 4.4). |
| B5 | Proportional to increase in poverty rate or number poor | Medium to high | M | Yes | Same machinery as B4 with a different loss metric. |
| B6 | Sized to annual expected loss | High for finance ministries | M | Yes | Use as a sizing and reporting rule first, not reserve dynamics. Section 5.1. |
| **Frequency and timing** | | | | | |
| F1 | Regular, repeated transfers | Done | Done | No | |
| F2 | One-off payment per activation, or N payments after activation | High | S | Yes | |
| F3 | Ex-post vs anticipatory | High | M | Yes | Cannot be modelled by timing alone. Section 5.3. |
| F4 | Custom anticipatory multiplier | Medium | S | Yes | Agree with your instinct, with a breakeven display. |
| F5 | Delivery delay for ex-post | Medium | S | Yes | Revives the dead `timeliness_weeks` field. |
| **Trigger** | | | | | |
| T1 | Weather variable above or below x | Very high | M | Yes | The parametric-insurance analogue. |
| T2 | Return period of event of at least x years | Very high | M | Yes | Fits the existing Step 2 return-period machinery. |
| T3 | Modelled welfare loss of at least $x | Medium | M | Yes | Only meaningful at aggregate level. Section 4.5. |
| T4 | Modelled rise in poverty gap, rate or number | Medium to high | M | Yes | Same. |
| T5 | Trigger level: household, region, national | Essential | M | Yes | Region is the natural default. Section 4.5. |
| **Targeting** | | | | | |
| G1 | Universal, ex-ante poor, proxy, errors | Done | Done | No | Make errors more realistic (section 5.4). |
| G2 | Predicted welfare below threshold at trigger | Medium | M | Yes | Optimistic if it uses the model's own prediction. |
| G3 | Geographic: worst-affected locations | High | M | Yes | Often combined with a household rule. |
| G4 | Layered rule: geography AND household | High | M | Yes | Replaces the single dropdown. |
| **Amount** | | | | | |
| A1 | Equal amount | Done | Done | No | |
| A2 | Per capita instead of per household | High | S | No | Cheap. Changes who gains. |
| A3 | Tiered or gap-filling amount | Medium | S to M | No | Gap-filling ("top up to the poverty line") is a useful benchmark. |
| A4 | Amount proportional to modelled household loss | Low | M | Yes | Not observable in practice; offer only as an oracle benchmark. |
| **Cross-cutting** | | | | | |
| X1 | Vertical vs horizontal expansion, registry coverage | Very high | M | Yes | Parked: survey data has no beneficiary flag. Section 7. |
| X2 | Basis-risk metrics | High | S once T1 to T3 exist | Yes | Section 7. |
| X3 | Cost-effectiveness metrics | Very high | S to M | No | Section 5.5. |
| X4 | Named presets (protective, preventive, promotive) | Medium | S | Partly | Section 6. |
| X5 | Design sweep and frontier | High | M | Yes | Section 6. |

### 3.2 Your policy design space

| Design | How it maps onto parameters | Modelled? |
|---|---|---|
| Protective: regular transfers to the vulnerable | Today's regular program | Yes |
| Preventive: anticipatory, forecast-based | Hazard trigger on a forecast, payment before the shock, forecast skill, effectiveness multiplier | Yes, with the caveats in 5.3 |
| Promotive: larger amounts to enable recovery | Larger amount or longer payment duration after activation | Yes |
| Transformative: address underlying vulnerabilities | Changes weather sensitivity itself (assets, livelihoods, services) | Not by cash alone. The infrastructure, labour, digital and education levers (`mod_3_02` to `mod_3_05`) already move covariates and, through interaction terms, the resilience channels. Treat "cash plus" as a combined scenario, not a new SP engine feature. |

## 4. Core architecture: making the transfer weather-dependent

### 4.1 Why the current structure cannot be stretched

The transfer is a vector with one entry per baseline survey row. The prepared annual channels are explicitly weather-invariant (`prepared$delta_sp[ids]`), and the quantile-repositioning logic uses the transfer to compute `tau_i_post` once per household. A trigger depends on (year, member), so the transfer becomes a quantity indexed by prediction row, not survey row.

### 4.2 Recommended design: a trigger state table plus a dynamic transfer layer

1. **New pure file `fct_sp_shock.R`** (no Shiny, as with the other `fct_` files) with:
   - `sp_trigger_state(pipeline, exposure, reference, spec)` returns a table keyed by (member, sim_year, location) with `active` (logical) and optionally `intensity` (for example exceedance size or modelled loss).
   - `sp_dynamic_transfer(pipeline, state, static_eligibility, spec, seed)` returns a daily-equivalent transfer per prediction row.
2. **Hook** inside `.apply_policy_annual_pipeline()`: after the channel block for each chunk, add the dynamic transfer on the level scale, re-logged for log outcomes (the same arithmetic as `run_sim_pipeline()`, `fct_simulations.R:645-655`). The weather exposure per prediction row (`exposure$table[[v]][idx]`) and household id are already available in that loop.
3. **Pre-pass.** Aggregate triggers (regional or national) depend on all rows of a (member, year). Compute the state table in one cheap pass before the chunk loop (a `rowsum` by year and location), then look it up per chunk. This avoids breaking the 100k-row chunking.
4. **Determinism.** Draw per-activation errors from a stream keyed by (household id, year, member), never by row position. This follows R2-BUG-12 and the existing `lever_seed()` pattern, and keeps preview/run parity testable.

### 4.3 Interaction with the RIF repositioning channel

For a static transfer, RIF folds `delta_sp` into `delta_main` and shifts the household's quantile rank. A dynamic transfer cannot do that without making `tau_i_post` year-specific, which breaks the "prepared once, weather-invariant" design.

Recommendation for v1: add the dynamic transfer as an additive post-prediction boost outside the repositioning channel, exactly as the OLS path already does, and state the approximation. The static program keeps its current behaviour. A side benefit is that the dynamic layer is cheap to recompute for many designs without redoing the RIF channels (section 6).

Decision (taken): in v1, the dynamic SP effect is folded into the Main channel and labelled as such. A separate channel is the better long-term home because the effect is weather-dependent, and it makes the "does the transfer offset climate losses" story visible. It touches `.compact_decomp_channels` (`mod_3_09_decomposition.R:109`), the level channels, the decomposition summaries and exports, so it is deferred to Phase 2.

Consequences of this choice to keep in mind:

- "Main" then means the static policy effect plus the dynamic SP effect. Label it in the UI and in exports so it is not read as a pure covariate effect.
- Snapshots and exports will change twice, once now and once when the channel is split. Keep the dynamic SP effect as its own vector (`delta_sp_shock`) inside the correction block from the start, and add it into `delta_main` only at the final sum. That makes the Phase 2 split a presentation change, not a numerical one.
- Do not fold it into Interaction. That channel is a model-derived coefficient-times-weather term and should keep that meaning.

### 4.4 Defining "modelled loss"

For household i in year t, define the modelled weather loss relative to that household's own reference weather:

`loss_it = level(yhat_it) - mean over historical years of level(yhat_it')`

where `level()` back-transforms log outcomes. Use the central prediction without residual draws, so triggers respond to weather, not noise. The historical reference comes from the Step 2 historical pipeline, mapped by baseline row id, so future-member pipelines need a reference from the historical arm. That is a new cross-pipeline dependency but both exist before Step 3 runs.

### 4.5 Trigger level: what can be observed in practice

Real triggers (parametric insurance, anticipatory action protocols) fire on observable hazard indices at a geographic unit, not on household-level welfare. Modelled household welfare loss is not observable by a program. Recommendation:

| Trigger type | Level | Rationale |
|---|---|---|
| Weather variable threshold, return period | Location (`loc_id`) | The realistic default. Weather is already aggregated to `loc_id`. |
| Modelled loss, poverty rate or number poor | Location or national aggregate | Mimics impact-based forecasting. |
| Any household-level modelled quantity | Household | Offer only as an "oracle" benchmark, labelled as such. |

Open question: confirm what `loc_id` represents (admin unit or hex cell) before offering regional wording in the UI. I did not verify this.

Other trigger inputs:

- **Only the model's weather terms.** Triggers can use only the weather variables in the Step 1 model, which is sensible since those drive welfare.
- **Adverse direction per weather variable.** Outcome direction exists in the metric registry (`fct_metric_registry.R`), but weather variables have no direction. Heat and drought are high or low tails, and standardized indices such as SPEI are negative. This needs a small per-variable "adverse tail" setting.
- **Fixed historical threshold under climate change.** Define the threshold on the historical distribution and apply it unchanged to SSP members. This matches how Step 2 already defines return periods (`step2_adverse_return_period()`, `fct_sim_compare.R:496`) and mirrors real contracts. The policy-relevant consequence is that a fixed 1-in-10 trigger fires more often under warming, so cost rises. That is the headline result for finance planners.
- **Limited historical record.** A 1-in-20 trigger from 30 historical years is estimated from one or two events. Decision: apply the Step 2 rule (offer 1-in-N only when the historical record has at least N finite years) and show the number of years behind each trigger.
- **Two triggers, two spatial behaviours.** A common weather value fires more often where the variable is typically more extreme, so cost concentrates in those locations. A return-period trigger uses each location's own 1-in-N level, so every location has the same activation probability. Label both clearly and show activation frequency by location.
- **Location-level basis risk.** Basis risk is scored at the trigger's own level: a location has a loss event when its mean modelled household weather loss (4.4) passes a user-set share of welfare. Report false positives (trigger without a loss event) and false negatives (loss event without a trigger), with a household-level view as secondary.

### 4.6 Activation rule vs payout scope

These are two separate questions, and the spec needs one control for them (decision 10).

- **Activation rule:** where is "fired" decided? Per location, or as one national event.
- **Payout scope:** who is paid once it fires? Households in the triggered locations only, or every targeted household nationwide.

| Choice | Activation | Payout | Represents | Cost shape |
|---|---|---|---|---|
| (a) default | Per location | Triggered locations only | Local drought or flood response | Smooth; cost scales with locations hit; low leakage |
| (b) | National gate: fires when at least k% of the population lives in locations that exceed the threshold | Triggered locations only | National decision with local delivery | Fewer activations; still targeted |
| (c) | National gate | All targeted households nationwide | National index cover with government allocation (from memory; to verify before citing) | Lumpy; rare, large bills; highest basis risk |

Implementation notes:

- **National gate definition.** The exposed share per (member, year) is the survey-weighted share of households in locations whose weather exceeds the trigger. This is one number per (member, year) in the pre-pass of the trigger state table (4.2), so the national gate costs almost nothing extra. The same `k` handles the case of the return-period trigger, where each location exceeds its own 1-in-N level.
- **Eligibility composition.** `payout = static_household_eligible AND payout_scope(location, year, member)`. For (a) and (b) the scope is "location is triggered" in that year and member. For (c) it is "national gate fired".
- **Ex-ante poor and national line.** The bottom x% is defined on the whole survey frame, as today (decision 11), then restricted by scope. The reach preview must use the same definition so preview and run agree.
- **Leakage metric.** Under (c), a large share of spending reaches locations with no modelled loss event (4.5). The leakage output in section 5.5 should be reported by scope, so that the cost of national payouts is visible, not hidden in an average.
- **Neighbour spillover** (paying locations adjacent to a triggered one) is not in the plan. It could be added later as a scope option.

### 4.7 Reproducibility requirement

Same inputs must give the same outputs. For SP this means: for the same survey, model, Step 2 run, SP scenario and analysis seed (`wise_current_seed()`), the transfer vector, trigger state, costs, preview figures and every output derived from them are identical, and the preview equals the run (UI-32). The repository already enforces this for the static program through seeded per-lever streams (`wise_seed(seed, "policy", "sp")`, R2-BUG-12), `test-determinism.R` and the preview-parity tests in `test-sp-reach.R`. Everything added by this plan must meet the same bar:

- **Seeded streams only.** Every random draw (targeting errors, forecast skill later) comes from a `wise_seed()` stream, never from the global RNG state, and never from a clock or session value.
- **Keys, not positions.** Draws are keyed by (household id, year, member), so results do not change with row order, chunk size (`chunk_size` in the correction loop), worker count or thread count.
- **New fields count as inputs.** Every new spec field enters the run signature, so changing it marks results stale, and is recorded in the export and provenance bundle, so a past run can be reproduced from its record.
- **No hidden state.** Thresholds are computed from the historical record of the run (not cached across runs), and the number of historical years used is stored with the result.
- **Tested.** Each Phase 0 and Phase 1 task that changes numbers or draws carries a determinism test (same seed gives identical output; reordered rows give identical output after realignment); P1-10 collects them.

## 5. Feature details

### 5.1 Budget

- **Admin cost markup (Phase 0).** Implement as a share of total cost (net transfer is `(1 - a)` of spend), which is how the existing comment frames it. Define it once and label it; a cost-transfer ratio (markup on transfers) is the other common convention and the two differ by `a / (1 - a)`.
  - **Defaults.** Literature on cost-transfer ratios exists (Caldés et al. 2006; Coady, Grosh and Hoddinott 2004; Bastagli et al. 2016; an OPM report on cost-efficiency of humanitarian cash). I did not extract numeric ranges. Do a source pass before choosing presets, and prefer evidence-backed presets by delivery type (existing registry scale-up vs new targeting exercise; mobile money vs manual) over a single hard-coded number. One search result states that stricter targeting limits transfer costs but raises the admin share, which supports making admin cost depend on the targeting method.
  - **Fixed vs variable.** A one-off setup cost vs a per-payment cost matters for shock-responsive programs that fire rarely. Start with one variable percentage and an optional fixed annual standby cost.
- **Payout per activation (Phase 1).** Amount times recipients, per activation. The program has no fixed annual cost; the output is a distribution across years and members.
- **Share of modelled loss (Phase 2).** Budget for a triggered location equals `s` times aggregate modelled loss of its (eligible or all) households, then split by the amount rule (5.4). Cap at a user-set ceiling so a catastrophic draw does not produce an absurd bill.
- **Proportional to poverty increase (Phase 2).** Budget equals a multiple of the modelled rise in the number of poor, or of the poverty gap. Same machinery as above with a different loss metric.
- **Annual expected loss (AAL).** Compute the historical mean annual loss (per location or in aggregate) and use it two ways: (a) size the payout so that expected annual cost equals a chosen fraction of AAL; (b) report the "premium-equivalent" cost for comparison with insurance. Reserve dynamics (carry-over across years, depletion) need ordered years and an explicit reserve rule; the simulation years are treated as draws, so leave this to Phase 3.
- **Trade-offs table.** Your trade-offs (fixed is predictable but may be insufficient; welfare-linked is responsive but variable; expected loss smooths over time) map to measurable outputs: coefficient of variation of annual cost, share of modelled loss offset, and share of shock-affected poor not covered. Show those next to each budget rule so the trade-off is quantified, not just described.

### 5.2 Frequency

- **One-off payment per activation** or **N payments after activation** (for example monthly for D months). Parameters: payments per activation and payment amount. Annual accounting stays `amount * payments / 365` per activated year.
- Regular transfers remain as today. In a combined design, a regular base transfer plus a shock top-up share the same eligibility machinery.

### 5.3 Timing: ex-post vs anticipatory

**The constraint.** The model predicts annual-mean welfare. A payment a few days before a flood and the same payment a few days after land in the same year and have identical annual value. Timing therefore only matters if we add something, and I see three ingredients:

1. **Forecast skill.** An anticipatory trigger fires on a forecast, not on the realised hazard. Model it as a noisy copy of the realised hazard, or directly as probability of detection (POD) and false-alarm rate (FAR), with seeded draws keyed by (location, year, member), like the existing inclusion and exclusion errors. This produces what anticipatory programs actually trade off: cost of false alarms vs cost of missed events. Skill typically falls with lead time, so presets could pair a short lead (high skill) with a long lead (lower skill, more time to act).
2. **Effectiveness multiplier.** An anticipatory payment can be worth more than the same payment afterwards (protects assets, funds evacuation, avoids distress sales); a delayed ex-post payment can be worth less. Implement one multiplier `m` on the transferred amount, default 1, applied to the welfare effect but not to cost. This is your "custom multiplier" idea.
   - **Show the breakeven `m`.** Report the multiplier at which an anticipatory design matches an ex-post design on the chosen outcome. That is more defensible than asking users to pick a number the model cannot calibrate.
   - **Evidence.** A WFP impact evaluation in Bangladesh reports better outcomes for households that received cash before a flood than for comparison households (section 9). It supports the direction of the effect, not a transferable numeric `m`. Keep `m = 1` as default and show sensitivity.
3. **Delivery delay.** For ex-post designs, a delay in weeks reduces effective value (use a multiplier below 1). This revives the dead `timeliness_weeks` field.

**Limitation to state in the app.** Because the model is static and annual, it cannot capture the mechanism behind most anticipatory benefits (avoided asset sales and long-run scarring). The multiplier is a stand-in, and it should be labelled as such.

### 5.4 Targeting and amount

- **Layered eligibility (Phase 2).** `eligible = static_household_rule AND dynamic_rule(year, member)`. The static rule is the existing dropdown (universal, ex-ante poor, proxy). The dynamic rule is optional and adds geography (triggered locations, or the worst-affected share of locations) and optionally predicted welfare at trigger.
- **Predicted welfare at trigger (G2).** This uses the model's own prediction as the targeting signal, which is an oracle: real programs do not observe predicted welfare. Pair it with larger default error rates and label it as an upper bound on targeting quality.
- **More realistic errors (Phase 0).** Today's errors flip uniformly random units, regardless of how close they are to the cutoff. Evidence from proxy-means testing in Africa (section 9) says errors concentrate near the cutoff and exclusion of the poor is often substantial. Replace uniform flips with distance-weighted flips: keep the exact inclusion and exclusion counts, but choose which units flip with probability decreasing in distance from the cutoff (one concentration parameter; its "uniform" setting reproduces today's behaviour exactly). This replaced an earlier noisy-score proposal, which cannot hit two target error rates with one noise parameter and would need a calibration step. Keep the sliders, change which units flip. Weighted counts instead of row counts are a separate later change (decision 14). Whether 10% / 10% remains a sensible default given the literature is not settled; the default stays unchanged in Phase 0, and revisiting it needs a source pass on proxy-means-test error rates (decision 13 follow-up).
- **Amount rules.**
  - **Per household vs per capita (A2).** Offer both. Per capita shifts benefits to larger households.
  - **Tiered or gap-filling (A3).** Gap-filling tops each recipient up toward a welfare line. It is a common humanitarian benchmark and a useful "ideal adequacy" comparator.
  - **Loss-proportional (A4).** Not observable in practice; offer only as an oracle benchmark, or skip.
  - Your note, "not clear if variable amounts are common or needed", fits this: ship A2 and gap-filling first; hold loss-proportional.

### 5.5 Outputs that make designs comparable

This is the "simulation considerations" list made concrete. None of it exists in Step 3 today beyond a single cost figure.

| Output | Definition | Needs hook |
|---|---|---|
| Activation frequency | Share of (member, year) pairs with the trigger active, by scenario and period | Yes |
| Annual cost distribution | Mean, median and 1-in-20 cost across years and members, with admin | Yes |
| Climate-adjusted cost | Cost under SSP members vs historical, same fixed trigger | Yes |
| Coverage of the shock-affected poor | Share of poor households with a modelled loss that received a transfer | Yes |
| Leakage | Share of spending reaching non-poor or unaffected households | Partly |
| Adequacy | Transfer as a share of modelled loss, or of the poverty gap | Partly |
| Poverty effect per cost | Poverty-rate reduction, or people lifted out of poverty, per unit cost, in average and bad years | No for static, yes for shock |
| Basis risk | False-positive and false-negative rates of the trigger against modelled loss events | Yes |
| Budget exhaustion | Share of years where a cap binds, if a cap is set | Yes |

Cost-effectiveness metrics can be built first for the regular program, which delivers value before the shock engine exists.

## 6. Presets and the design sweep

- **Presets.** Four or five named presets (protective, preventive, promotive, plus a "custom") that set the parameter groups at once. They reduce sidebar clutter (the SP panel is already a compact accordion) and give policy users a starting point. They are data, not engine logic, so they are cheap.
- **Design sweep (Phase 3).** The request mentions simulating different parameter combinations. Today the Step 3 policy scenarios (`policy_scenarios` in `mod_3_06`) hold lever sets, not SP designs, and each run recomputes the full policy arm. If the dynamic SP layer is additive and sits outside the RIF channels (section 4.3), then evaluating a grid of designs reuses the baseline pipelines and prepared channels and only recomputes the cheap layer. That makes a "cost vs poverty reduction" frontier feasible. The static SP is not additive in the RIF path, so the sweep would apply to the shock-responsive arm only, at least at first.
- **Bounded grids.** Limit grid size and report progress, in line with the app's resource limits (`review/optimization_guidelines.md`).

## 7. Things missing from your list

1. **Vertical vs horizontal expansion and registry coverage (parked).** The standard shock-responsive typology (OPM, from memory; to verify the exact citation) distinguishes raising transfers for existing beneficiaries (vertical) from adding new households (horizontal), plus piggybacking, shadow alignment and refocusing. It matters because most real programs scale up an existing registry whose coverage and quality bound what is possible. The survey data cannot tell us who already receives assistance, so the app cannot model a real registry. Decision: do not build a registry. Use the specified targeting rules (universal, ex-ante poor, proxy, with errors) within triggered locations, and do not claim vertical or horizontal expansion. Document the limitation: the simulated program is an idealised new program with its own targeting, not an expansion of a named existing one. Revisit if a data source gains a beneficiary flag, or if a synthetic registry (poorest x% with a coverage parameter) is later wanted as a labelled assumption.
2. **Basis risk.** Triggers that fire without losses, or losses without triggers, are the central quality measure of parametric designs. The model can compute both against modelled loss.
3. **The financing view.** Treasuries need the cost distribution, the 1-in-20 bill, how it changes under climate, and the gap between a fixed budget and modelled need. That is a different framing from "what does one run cost".
4. **Existing programs inside the survey welfare.** The baseline already includes current transfers. The incremental effect is what the app simulates. Say so, and check whether the survey flags current beneficiaries.
5. **Realistic targeting errors** (5.4) and **spatially correlated errors**: exclusion in a poorly connected region is correlated across households.
6. **Duration of support** after an activation (promotive designs), and the adequacy of the payment relative to need.
7. **Delivery delay** (5.3).
8. **Limits of the static annual model.** No poverty traps, asset scarring, price effects, local multipliers or crowd-out of private transfers. These are the usual arguments for early support. List them in the app's documentation, and treat any multiplier as a labelled assumption.
9. **Currency consistency** (R2-BUG-04) before dollar-denominated triggers.
10. **Determinism, run signature, export and provenance.** New fields must enter `sp_scenario`, the policy signature (`.sig_plain`), `fct_export.R` and `fct_provenance.R`, and per-(household, year, member) draws must be keyed by ids (section 4.2).
11. **Preview parity.** The UI-32 reach card promises the preview equals the run. With triggers, the preview should show expected activation frequency and expected annual cost from the historical pipeline, which exists before Step 3 runs.
12. **Choice of loss metric and welfare line.** Poverty-based triggers and budgets need the poverty line already tracked in Step 2 (`hs$pov_line`); make the dependency explicit.

## 8. Phased plan

### Phase 0: independent improvements (about 1 to 2 weeks in total)

- Fix R2-BUG-04 (currency), per the existing tracker (`review/REVIEW-2026-10-06-tracking.md`, batch B4).
- Admin cost markup, with evidence-backed presets after a source pass.
- Per-capita vs per-household amount option.
- Noisy-score targeting error model; revisit defaults.
- Cost-effectiveness metrics for the static program (cost per person lifted out of poverty, leakage, adequacy).

### Phase 1: shock-responsive MVP (about 3 to 5 weeks)

- `fct_sp_shock.R`: trigger state table and dynamic transfer; hook in `.apply_policy_annual_pipeline()`.
- Triggers: weather variable threshold and exceedance probability (return period) threshold, per survey location, fixed historical threshold; adverse-tail setting per weather variable. Other trigger types stay in the plan for Phase 2.
- Response: payout per activation ("$ per household per activation" only; no envelope budget in shock mode, decision 12), one-off or N payments, admin markup.
- Payout scope: one control (4.6): per-location and triggered only (default), national gate with triggered only, national gate with all targeted households. The national gate adds one parameter, the exposed population share `k`.
- Eligibility: existing static rules applied inside triggered locations. No beneficiary registry (section 7, item 1).
- Outputs: activation frequency, annual cost distribution including 1-in-20, basis risk. Basis risk in Phase 1 is measured against the model's own loss events (a hazard trigger vs modelled household loss), so it needs the loss definition in 4.4 even though loss triggers are deferred.
- UI: enable the "Shock-responsive" toggle with a Trigger group and a Response group; remove the display-only notice; extend the reach card to expected activation and cost. Label the trigger unit as "survey location".
- Decomposition: dynamic SP effect folded into Main, kept as its own vector internally (4.3).

### Phase 2: impact-based, anticipatory, richer targeting (about 4 to 6 weeks)

- Modelled loss, poverty rate and number-of-poor triggers at location and national level.
- Budget as share of modelled loss, poverty-linked budgets, AAL-based sizing.
- Forecast skill (POD, FAR) with seeded draws; anticipatory and delay multipliers with breakeven display.
- Separate decomposition channel for dynamic SP (split out of Main).
- Layered geographic and predicted-welfare targeting.

### Phase 3: design-space tools (about 3 to 4 weeks)

- Presets, design sweep with cost vs poverty-reduction frontier, reserve dynamics, optional gap-filling and tiered amounts.

Effort figures are rough, from reading the code, not from prototyping. The largest uncertainty is the decomposition channel change.

## 9. Testing plan for the shock engine

Invariants that give high confidence cheaply:

- **Never triggers:** a threshold no draw reaches reproduces the baseline exactly (zero effect, zero cost).
- **Always triggers:** with an always-on trigger and `n` payments, the dynamic transfer reproduces the static program's cost and, for OLS, its welfare effect. For RIF the difference is the repositioning approximation, which should be bounded and documented.
- **Monotonicity:** a lower threshold never reduces activation frequency; a larger amount never reduces welfare.
- **Preview parity:** preview and run give the same activation table and cost, as `test-sp-reach.R` does for the static case.
- **Determinism:** same seed gives identical activation and error draws regardless of row order or chunk size.
- **Budget identities:** total cost equals net transfers plus admin; budget-first never exceeds the envelope.
- **Climate direction:** a fixed historical trigger activates at least as often under a warming member for a heat variable (a sanity check on tail direction handling).

New test files would follow the repository's naming (`test-fct_sp_shock.R`, extensions of `test-sp-reach.R`).

## 10. Decisions

Decided (7 October 2026):

| # | Decision | Outcome |
|---|---|---|
| 1 | Decomposition channel | Fold dynamic SP into Main for v1, labelled; separate channel in Phase 2. Keep it as its own vector internally so the split is presentational. |
| 2 | Triggers in v1 | Weather variable threshold and exceedance probability (return period) threshold only. Modelled-loss and poverty-based triggers stay in the plan for later phases. |
| 3 | Beneficiary registry | Not built. The survey data has no beneficiary flag. Use the specified targeting rules within triggered locations; no vertical vs horizontal claim. |
| 4 | Anticipatory design | Deferred to Phase 2. Phase 1 is ex-post only. |
| 5 | Trigger unit | `loc_id` (survey location; weather is population-weighted from H3 cells to `loc_id`, and `fct_loc_panel.R` links locations across rounds). It is not an admin boundary, so label it "survey location". Verified in the code on 7 October 2026. |
| 6 | Basis-risk loss event | A location has a loss event when its mean modelled household weather loss passes a user-set share of welfare. This matches the level at which the trigger fires. Household-level scoring is a secondary view. Loss is defined as in 4.4 (central prediction, no residual draws, against the household's own historical mean). |
| 7 | Return-period input | Entered as years (1-in-x), consistent with Step 2; the annual probability is shown beside it. |
| 8 | Historical record limit | Same rule as Step 2 (`filter_historically_supported_return_periods()`, `fct_metric_registry.R:328`): offer 1-in-N only when the historical record has at least N finite years. Show the number of historical years behind each trigger. A pooled climate-member record is not in v1. |
| 9 | Weather vs return-period threshold | The weather-variable trigger takes one common value everywhere (a national-style rule that fires more often in places where the variable is typically more extreme). The return-period trigger is location-specific by construction (each location's own 1-in-N level, equal activation probability everywhere). Do not offer location-specific weather thresholds; they would overlap with the return-period trigger. |

| 10 | Payout scope vs trigger | One control with three choices: (a) per-location activation, pay triggered locations only (default); (b) national gate, pay triggered locations only; (c) national gate, pay all targeted households nationwide. "Any location fires, pay everyone" is not offered. Details in 4.6. |
| 11 | Ex-ante poor rule within triggered locations | National line (bottom x% of the whole survey frame, as today), then restricted to triggered locations. A rich triggered location may have few recipients. Keeps the preview arithmetic and the national eligibility meaning. |
| 12 | Budget modes for shock-responsive | Phase 1 supports "$ per household per activation" only. Total cost varies with activations, and the annual cost distribution is the output. Envelope budgets (cap with pro-rata scale-down, or envelope per activation) are deferred. Regular programs keep both budget modes. |
| 13 | Admin cost in v1 | A user-entered percentage of total cost, default 0, no presets. Evidence-backed presets by delivery method are a separate, non-blocking follow-up that needs a literature pass (cost-transfer ratio sources in section 9). Owner: the project owner unless someone else is named; it does not gate any Phase 0 or Phase 1 task. |
| 14 | Row-count vs weighted-count targeting errors | Phase 0 keeps today's row-count behaviour and documents it. The weighted-share fix is a separate optional task (P0-8 in `review/sp_shock_responsive_tasks.md`), run after the distance-weighted error change so that each numerical change is attributable, with its own Decision log entry. Not a v1 gate. |

## Sources

Verified by search:

- Pople et al., "The Importance of Being Early: Anticipatory Cash Transfers for Flood-Affected Households" (University of Oxford): https://www.csae.ox.ac.uk/node/2132846
- WFP, "Bangladesh anticipatory action impact evaluation": https://www.wfp.org/publications/bangladesh-anticipatory-action-impact-evaluation
- Brown, Ravallion and van de Walle, proxy-means testing in Africa: https://www.nber.org/papers/w22919.pdf and https://www.imf.org/external/pubs/ft/fandd/2017/12/brown.htm
- Caldés, Coady and Maluccio (2006) on cost-transfer ratios: https://www.peiglobal.org/sites/pei/themes/pei/kc_files/Caldés et al. 2006.pdf
- Bastagli et al., cash transfer evidence review: https://www.peiglobal.org/cash-transfers-what-does-evidence-say-rigorous-review-programme-impact-and-role-design-and
- OPM, cost-efficiency of cash in humanitarian programmes: https://opml.co.uk/projects/cash-transfers-humanitarian-programmes-assessing-cost-efficiency
- Shock-responsive social protection building blocks (trigger, financing, delivery): https://openknowledge.worldbank.org/server/api/core/bitstreams/6f3a1a7d-d5d6-4f4f-9d6d-4417b0ff6f69/content

To verify before relying on it: the OPM vertical/horizontal expansion typology citation, numeric cost-transfer ratio ranges, and typical PMT error rates for use as default slider values.
