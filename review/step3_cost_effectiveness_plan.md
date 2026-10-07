# Step 3 cost-effectiveness results: initial plan

Status: first draft for discussion. No code has been changed. The summary-card version of cost-effectiveness is deferred (see `review/headline_cards_review.md`, section 11.4C); this note plans the fuller results that the cards would later point to.

Written for: the WISE-APP maintainers and the policy team deciding what Step 3 should say about value for money.

Evidence note: statements about the code come from reading the files named. Literature references are given from general knowledge and are marked "to verify" until a source pass is done.

## 1. Why this matters

Step 3 answers "how could policy mitigate the welfare impacts of weather?" with effects: how much a policy changes the headline metric, in average and bad years. Policy users also need the other half of the comparison, what that effect costs. Without it, a large effect from an expensive design looks better than a modest effect from a cheap one, and the app cannot support questions such as:

- How much does it cost to lift one person out of poverty under this design?
- Does the policy still deliver in a 1-in-20 weather year, and at what cost per person lifted then?
- Is money reaching the poor (coverage and leakage), and is the transfer large enough to matter (adequacy)?
- How does a different design (targeting rule, amount, budget) compare on cost and effect?

Cost-effectiveness here is not a cost-benefit analysis. It reports a modelled effect per unit of money spent. It does not value benefits in money terms, and it does not cover costs the app does not know about.

## 2. What exists today

| Item | Where | Notes |
|---|---|---|
| Annual cost of the applied transfer | `.sp_transfer_totals()` in `R/fct_policy_sim.R` | Reads the transfer column the run wrote (`.wiseapp_sp_transfer`, a daily per-capita amount) and returns annual population cost, cost per recipient, recipients in sample and weighted, households. Handles weights and household size. |
| Cost and reach before a run | `.sp_scenario_reach()` | Same eligibility draw and arithmetic as the run, tested to match. Used for the sidebar preview. |
| Realized cost in Diagnostics | `policy_component_matrix()` in `R/fct_policy_diagnostics.R` | A `realized_cost` column, filled for social protection only (NA for other levers). |
| Baseline welfare per household | `svy$welfare` | Used for ex-ante poor targeting, so baseline poverty status is available for a given poverty line. |
| Policy effect and its uncertainty | Step 3 paired effect summary, metric-aware decomposition, `coef_sd` | Equal-model mean effect for the average year and the 1-in-20 year, with a coefficient-uncertainty SD and the climate-model range. |
| Policy arms | `baseline_svy`, `policy_svy` | Before and after the policy columns are applied. |

What does not exist:

- No cost input for the infrastructure, digital, labour or education levers. Those levers change covariates (for example electricity access or travel time to health); the app has no unit cost for them.
- No administration cost. A parallel plan (`review/sp_shock_responsive_plan.md`, task P0-2 in `review/sp_shock_responsive_tasks.md`) adds an admin cost markup as a share of total cost.
- No cost-effectiveness metrics. The same parallel plan lists them as task P0-5 (coverage of the poor, leakage, adequacy, poverty effect per cost) in a new `R/fct_sp_effectiveness.R`, shown in the Diagnostics tab. This note builds on P0-5 rather than duplicating it.
- The model is a static annual accounting model: the transfer is added to every household-year in every weather draw, so cost is the same every year in a regular program. That changes if the shock-responsive transfer is built.

## 3. Scope and non-goals

In scope: social protection cost-effectiveness for regular programs now, designed so the shock-responsive cost distribution can plug in later.

Out of scope for now:

- Cost-benefit analysis (valuing welfare gains in money) and discounting. The effect is an annual average and the cost is annual, so there is nothing to discount until a multi-year design exists.
- Costs of the non-social-protection levers. Adding them needs unit-cost inputs, listed as an optional later phase (section 9).
- Financing and general equilibrium effects (who pays, tax distortions, prices). Cost is stated as spending, not as a welfare cost to taxpayers.
- Benefits the model does not represent (for example avoided asset sales, health and education effects).

## 4. Proposed metrics

Definitions use the weights and household-size conventions of `.sp_transfer_totals()`: weights represent people; in household analysis the transfer is per capita, so household counts divide by household size. All are computed for a chosen scenario, a chosen weather basis (average year or adverse 1-in-N year) and the selected poverty line.

### 4.1 Cost block

| Metric | Definition | Source |
|---|---|---|
| Gross annual cost | Weighted sum of the transfer column x 365, plus administration | `.sp_transfer_totals()`, P0-2 |
| Net transfer | Gross cost x (1 - admin share) | P0-2 |
| Cost per recipient | Annual cost / recipients (people and households) | existing |
| Cost per person in the population | Annual cost / total weighted population | new, trivial |

### 4.2 Targeting block (static, from the baseline and policy frames)

| Metric | Definition |
|---|---|
| Coverage of the poor | Weighted share of poor people (baseline welfare below the line) who receive a transfer |
| Leakage | Share of spending reaching people above the line |
| Inclusion and exclusion error | Realised against the targeting rule's ideal eligibility (the diagnostics snapshot already keeps pre-error eligibility) |
| Adequacy | Average transfer to recipients below the line as a share of their average poverty gap |

These need no simulation results, only the two survey frames, so they are cheap and exact. Coady, Grosh and Hoddinott (2004) is the standard reference for targeting accuracy and leakage measures (to verify).

### 4.3 Effect-per-cost block (needs the Step 3 effect)

| Metric | Definition | Applies to |
|---|---|---|
| Cost per person lifted out of poverty | Annual cost / (reduction in headcount x weighted population) | Poverty rate |
| Cost per percentage point | Annual cost / pp reduction in the headline metric | Poverty rate, gap, severity |
| Poverty gap closed per $1 | Reduction in the aggregate poverty gap (money terms) / cost. Close to "vertical expenditure efficiency" (Beckerman 1979; to verify) | Poverty gap |
| Welfare gain per $1 | Change in total welfare (money terms, annual) / cost. Above 1 means the model finds more than a dollar of welfare per dollar transferred, which can occur through behavioural channels the model includes | Mean, median, total |
| Same metrics in an adverse year | Use the baseline-anchored 1-in-N effect and the same cost | All of the above |

Not defined (shown as "not applicable", never as zero): Gini and prosperity gap, which have no natural per-dollar reading; any ratio where the effect is zero or has the wrong sign.

### 4.4 Which weather basis

Cost is fixed in a regular program; effect varies with the weather. Reporting cost per person lifted in the average year and in the 1-in-20 year makes the resilience question concrete: does the same spending deliver more in a bad year? The existing metric-aware decomposition already supplies the adverse-year effect, so no new simulation is needed. Section 8 explains how this changes with shock-responsive transfers, where cost also varies.

## 5. Uncertainty and what to print

- Effect: use the paired contrast's coefficient SD (`coef_sd`, added for the Expected policy effect interval) and the climate-model range. A ratio of an uncertain effect to a fixed cost is monotone in the effect, so the interval for a ratio can be obtained by applying the formula to the effect's interval endpoints. That is exact for cost per person lifted when cost is fixed, and it fails when the interval includes zero (the ratio is unbounded): print "not bounded" rather than a number.
- Cost: deterministic given the scenario and the targeting-error seed. Targeting errors are a single seeded draw, so coverage and leakage have draw-to-draw variation that is not shown. State that, and consider a small repeat-draw range later.
- Print intervals, not verdicts, in line with the headline-card decision (no significant / not significant flags).

## 6. Where results appear

User direction: detail in the Diagnostics tab; fuller results in Step 3, not only summary cards.

Options:

| Option | Description | For | Against |
|---|---|---|---|
| A. Diagnostics section only | A "Cost-effectiveness" section in the Diagnostics tab (the plan in P0-5) | Smallest; matches the existing parallel task | Hidden from users who look only at Results; no figure |
| B. Results section plus Diagnostics detail (recommended) | A "Cost and value for money" section in the Results tab with a compact table (cost, people lifted, cost per person lifted, coverage, leakage) and one figure; assumptions and the full metric table in Diagnostics | Answers the question where users already look; keeps detail separate | More UI to build and test |
| C. New tab | A dedicated "Cost-effectiveness" tab after Results | Room for comparison views and design sweeps | Another tab to maintain; duplicates Results context |

Recommendation: B for the first release, with the Results section built as a self-contained module so it can move to a tab (C) if the design-comparison view grows. Summary cards stay deferred; when they return, they should show one figure from this section.

Figures worth building (echarts, consistent with the rest of Step 3):

1. Cost against effect: a point per scenario or design (x: annual cost, y: change in the headline metric or people lifted), with the effect interval. Useful the moment more than one policy scenario or design exists.
2. Average vs adverse year: cost per person lifted in the average year and the 1-in-20 year, side by side, with intervals.
3. Cost split: net transfer to the poor, to the non-poor (leakage), and administration, as one stacked bar.

## 7. Engine design

- New pure functions in `R/fct_sp_effectiveness.R` (already named in P0-5): inputs are the baseline and policy frames, the weights and household size, the analysis unit, the poverty line, the cost vector and the effect (with its interval). No Shiny dependency, one tested implementation, reused by the Diagnostics table, the Results section, the cards later and the exports.
- Compute once per policy run, inside the diagnostics snapshot (`.policy_diagnostics_snapshot()`), because the static metrics depend only on the two frames; the effect-dependent metrics join in the Results layer, where the effect reactives live.
- Currency: cost is entered in the outcome's currency (`sp$currency`), while the stored welfare is 2021 PPP; the transfer column is already on the welfare scale. Compute ratios on the PPP scale and show cost in the entry currency beside it. The LCU handling (R2-BUG-04) must be correct first, as the parallel plan notes.
- Accept the cost as a per-(climate member, year) vector, not a scalar, even though a regular program produces a constant vector. This lets the shock-responsive engine plug in without changing the metric functions.
- Handle the analysis unit explicitly (household, individual, firm) and say which unit the denominator counts.
- Exports: register the tables with `wise_export_table()` (metric table, assumptions, per-scenario values) so they enter the export bundle.

## 8. Fit with the shock-responsive plan

`review/sp_shock_responsive_plan.md` plans a transfer that depends on weather. Cost-effectiveness changes in three ways when it lands:

1. Cost becomes a distribution across years and climate members. The right headline is expected cost and a bad-year cost (for example the 1-in-20 cost), not one number.
2. Cost per person lifted becomes a ratio of two random quantities. Report it for the expected year and for adverse years using paired (member, year) values, not a ratio of means.
3. New comparable outputs (activation frequency, basis risk, climate-adjusted cost) belong in the same section, following the table in section 5.5 of that plan.

Build order: P0-2 (admin cost) and P0-5 (static metrics) first, then this plan's Results section, then extend for shock-responsive cost when that engine exists.

## 9. Phases and tasks

| Phase | Work | Depends on | Size |
|---|---|---|---|
| 0 | Confirm the cost arithmetic against `.sp_transfer_totals()` with a hand calculation; confirm R2-BUG-04 status; decide net vs gross people lifted (section 10) | none | S |
| 1 | `R/fct_sp_effectiveness.R`: cost block and targeting block; tests against hand calculations on a small survey (weights, household size, missing poverty line) | P0-2 (admin cost) | M |
| 2 | Diagnostics section: metric table and assumptions, export registration | Phase 1 | S |
| 3 | Effect-per-cost metrics with intervals (average and adverse year), "not applicable" and "not bounded" handling | Phase 1; `coef_sd`; decomposition tail effect | M |
| 4 | Results section: compact table and the three figures; stale-state handling like the other Step 3 outputs | Phases 2 and 3 | M |
| 5 | Summary card (one figure, popover points to the section) | Phase 4 | S |
| 6 (optional) | Unit-cost inputs for the other levers, so their cost-effectiveness can be reported; needs a source for unit costs | policy team input | L |

## 10. Decisions needed

1. Tab or section: the plan recommends a Results section plus Diagnostics detail (option B). Is that right, or do you want a dedicated tab now?
2. People lifted: gross or net? The model gives the net change in the poverty rate for the whole population. Gross movements (some households leaving poverty while others enter) need household-level baseline and policy status in each year, which the annual aggregates do not keep. The plan assumes net and labels it "net change in the number of poor", which is standard but understates people helped if some move the other way.
3. Which metric leads: cost per person lifted (poverty rate only) or a metric available for every outcome such as welfare gain per $1? Proposal: lead with cost per person lifted when the metric is a poverty rate and with welfare gain per $1 otherwise.
4. Administration cost: depends on the parallel P0-2 default (0). A cost-effectiveness figure with a zero admin share is optimistic; the plan shows the admin share used beside every figure.
5. Do you want the other levers in scope at all (Phase 6), given the app has no unit costs for them? If the policy team can supply unit costs or accept a user-entered cost per unit, it is feasible.
6. Targeting-error randomness: one seeded draw (current) or a small repeated-draw range for coverage and leakage?
7. Poverty line: use the Results poverty line input (which users can change) so the section follows the rest of Step 3, or fix it to the scenario's line?

## 11. Risks and cautions to state in the app

- Effects are modelled associations applied to simulated weather, not observed programme impacts.
- Cost covers the transfer (and admin if set), not delivery of other levers, targeting surveys or financing.
- A cost per person lifted is conditional on the poverty line and the targeting draw; it is not a property of the programme alone.
- Welfare gain per $1 above 1 should be read with care: it depends on the model's behavioural channels and is not a measure of fiscal efficiency.
- Ratios with a near-zero or wrong-signed effect are unstable; the app should print "not applicable" or "not bounded", never an extreme number.

## 12. Next steps

1. Review this plan and settle the decisions in section 10, starting with items 1 to 3.
2. Coordinate with the owner of `review/sp_shock_responsive_plan.md` so the cost-effectiveness work (P0-5) and this plan share one implementation and one set of tests.
3. When Phase 1 is agreed, begin with the pure functions and tests before any UI.

## References (to verify)

- Coady, Grosh and Hoddinott (2004), Targeting of Transfers in Developing Countries, World Bank.
- Beckerman (1979), The impact of income maintenance payments on poverty in Britain, 1975, Economic Journal (vertical expenditure efficiency).
- Caldés, Coady and Maluccio (2006); Bastagli et al. (2016): cost-transfer ratios and cash transfer evidence (also cited in `review/sp_shock_responsive_plan.md`).
