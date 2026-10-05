# Step 2 result size: de-duplicating `weather_exposure`

Status: option A implemented (2026-10-05, uncommitted); option C not done (small). Date: 2026-10-05.

## Outcome (BFA OLS, 2 SSP x 1 period, real data)

| | Before | After |
|---|---:|---:|
| Result in memory | 1,481 MB | 625 MB (-58%) |
| qs2 result file | 196.5 MB | 113.8 MB (-42%) |
| Resolve all 16 members of one scenario (with per-scenario cache) | - | 0.07 s |

- Producer: `run_sim_pipeline()` stores `.policy_exposure_compact()` (schema 2). Resolver: `step2_exposure_resolve(pipeline, owner, cache)` in `R/fct_simulations.R`; schema-1 mappings pass through unchanged.
- Consumers: `.validate_policy_annual_exposure()` resolves; `owner`/cache threaded through `.policy_annual_channels()`, `.policy_annual_channels_reference()`, `.apply_policy_annual_pipeline()` (callers in `R/fct_policy_sim.R`) and `.policy_metric_pipeline()` / `.policy_metric_decomposition()`. The baseline/policy alignment check now also requires identical `weather_raw`.
- Parity: the resolved mapping is `identical()` to the schema-1 mapping for every pipeline of a real run (31 members + historical), and in unit tests (plain, shared-key weather, cached), including the end-to-end annual policy correction.
- Finding (separate issue, pre-existing): weather inputs are not bit-reproducible across runs. One CMIP6 member's `spei6` differed by about 4e-5 relative between two identical BFA runs, which suggests non-deterministic floating-point aggregation in DuckDB. Predictions were identical in that case, but it is worth a determinism follow-up.
Scope: the per-pipeline `weather_exposure` mapping produced in `run_sim_pipeline()` (`R/fct_simulations.R`) and consumed by the Step 3 metric-aware decomposition (`R/fct_policy_metric_decompose.R`).

## 1. Problem

A completed Step 2 result is very large *in memory*, even though it is compact on disk:

| BFA OLS, 2 SSP x 1 period, 31 climate models (2026-10-05) | In memory | qs2 on disk |
|---|---:|---:|
| Whole result | 1,481 MB | 196 MB |
| `new_scenarios[[*]]$pipelines[[*]]$weather_exposure` (sum) | **882 MB (60%)** | most of the compressed size |
| Member `weather_raw` (already value-only via `step2_weather_share_members()`) | 154 MB | |
| `F_loading`, `y_point`, `weight`, `id_vec`, `sim_year`, `svy_row_id` (sum) | ~330 MB | |
| `hist_sim_result` (historical, including its own 28 MB exposure) | 78 MB | |

Extrapolated to 3 SSP x 3 periods (about 145 pipelines), the main Shiny process holds several GB per user after adoption. The same bytes also drive the end-of-run `qs2` write (worker) and read (main thread), which is the remaining wait once streaming has shown the charts.

## 2. What `weather_exposure` contains (per climate model, about 28.5 MB)

Built by `.policy_exposure_mapping()` (`R/fct_simulations.R:891`) from `.policy_exposure_table()` (`R/fct_simulations.R:869`):

| Field | Size | Redundancy |
|---|---:|---|
| `table` (310,500 x 10: `.policy_exposure_id`, `code`, `year`, `survname`, `loc_id`, `int_month`, `sim_year`, `timestamp`, weather columns) | 21.2 MB | Fully derivable: `.policy_exposure_table(weather_raw, weather_columns)` on the member's own `weather_raw`, with `.policy_exposure_id = seq_len(nrow)`. The 7 key columns are identical across members of a scenario; only the weather columns differ. |
| `svy_row_id`, `sim_year`, `weight`, `id_vec` | 5.6 MB | Identical to the same-named fields of the pipeline itself (verified `identical()`). |
| `row_index`, `prediction_row_id` | 1.7 MB | Identical across all members of a scenario (verified). |
| `status`, `available`, `reason`, `id_col` | ~0 | |

Removing only the derivable and duplicated parts removes about 27 of the 28.5 MB per member. For the BFA 2x1 case that is about 0.85 GB of 1.48 GB (around 55-60%).

## 3. Consumers (all in Step 3)

- `.validate_policy_annual_exposure()` (`R/fct_policy_metric_decompose.R:87-140`):
  - checks `status`;
  - checks that the 4 duplicated vectors equal the pipeline's;
  - validates `row_index`/`prediction_row_id`;
  - reads `table[[key]][row_index]` for the survey join keys, `timestamp` and the weather columns.
- `.policy_metric_pipeline()` (`:506`) requires baseline and policy pipelines to have `identical()` `weather_exposure` (alignment check).
- Lines `:186-204`, `:335`, `:351` read `table` columns at `row_index`, and `nrow(table)`.
- Step 3 policy pipelines are produced by `run_sim_pipeline()` too, so they carry their own `weather_exposure`.
- No Step 2 consumer (Results, Diagnostics, exports) reads it. That needs confirming with a grep at implementation time, including `fct_export.R` and `fct_provenance.R`.

## 4. Options

**A. Store a recipe, not the table (recommended).**

The pipeline keeps a small `weather_exposure` with:
- `status`, `available` and `reason`;
- `row_index` and `prediction_row_id`;
- `weather_columns`: the column names passed to `.policy_exposure_table()`;
- `schema = 2L`.

It drops `table` and the 4 vectors duplicated from the pipeline. A resolver, `step2_exposure_resolve(pipeline, weather_raw)`, rebuilds the full v1 structure on demand:
- It rebuilds `table` with `.policy_exposure_table(weather_raw, weather_columns)`. `weather_raw` is the member's frame, re-joined to the scenario's `weather_shared` keys when shared, exactly as the existing weather-share resolver does for other consumers.
- It copies the 4 vectors from the pipeline.

Step 3 calls the resolver once per pipeline per decomposition context, and caches it in the run-owned decomposition context, which is already per run.

- Saves about 27 MB per member, roughly 55-60% of the result.
- Costs a rebuild of about 310k rows (`.add_sim_timestamp_fields()` plus a column subset) per member when Step 3 decomposes. That is tens of ms per member and needs measuring. It is paid only for scenarios Step 3 actually decomposes, not at Step 2 adoption.

**B. Share the key table per scenario.**

Store the 7 key columns once per scenario, plus per-member weather columns, following the existing `step2_weather_share_members()` pattern.
- Saves about 16-17 MB per member (keys only).
- The structure is more complex and the saving smaller than A, and it duplicates information `weather_raw` already holds.

**C. Share `row_index` and `prediction_row_id` per scenario.**

This is an add-on to A or B, saving about 1.7 MB per member. Do it only if Step 3 can take the scenario-level copy cleanly; otherwise skip it, since it's small.

**D. Do nothing to the payload and load lazily instead** (per-scenario artifacts read on demand). This addresses main-process memory but not worker write time or the total bytes. It is complementary to A and much larger in scope.

## 5. Recommended implementation (A, then optionally C)

1. **Producer.** In `run_sim_pipeline()`, keep computing the full mapping, since validation of the integer ids there is valuable. Then return the compact v2 form. Keep a `legacy_exposure = FALSE` switch only if existing tests need the v1 form; otherwise convert the tests.
2. **Resolver** (`R/fct_step2_payload.R`, next to the weather-share helpers). It must reproduce the v1 `table` *exactly*: column order, types, `timestamp` class and tz, and row order. Base it on the same `weather_raw` object the pipeline was built from. Watch for two things:
   - `weather_raw` may be the value-only shared form, so resolve shared keys first;
   - in `weather_storage = "reference"` mode the frame is in the run store and must be resolved through the existing lease.
3. **Step 3 consumers.** Replace direct `pipeline$weather_exposure` reads with the resolved form, obtained once per pipeline in the decomposition context.
   - The baseline/policy alignment check at `:506` compares the compact v2 objects plus the pipeline vectors. That is equivalent, because the table is a deterministic function of `weather_raw`, which must also match. Assert that `weather_raw` identity (or a digest) matches as part of the check.
4. **Policy pipelines.** Step 3's own `run_sim_pipeline()` calls produce v2 too, which shrinks Step 3 memory as well.
5. **Payload.** `compact_step2_result()` needs no change, because the shrink happens at the source.

## 6. Tests

- **Parity (critical):** for OLS and RIF fixtures, `step2_exposure_resolve(pipeline, weather_raw)` is `identical()` to the v1 `weather_exposure` currently produced. Cover:
  - shared and unshared `weather_raw`;
  - memory and reference weather storage;
  - historical and future members.
- The Step 3 decomposition outputs are unchanged. These tests already exist: `test-fct-policy-metric-decompose.R`, `test-policy-decomposition-uncertainty.R`, `test-w3-b-future-decomposition-characterization.R` and the policy snapshot tests. They must pass without snapshot updates.
- Payload size regression test: the per-member `weather_exposure` object size is below a small bound (for example, under 5% of the member pipeline).

## 7. Benchmarks (`review/optimization_guidelines.md` section 9)

Measure before and after, on BFA (2x1 and 3x3) and IRN (1x1 and a multi-SSP case):
- the in-memory result size;
- qs2 file size and write/read time;
- peak RSS of the main process after adoption;
- Step 3 decomposition elapsed time, which must not regress materially, so the rebuild cost needs measuring.

## 8. Risks

- **Exact reconstruction.** Any difference in timestamp tz, factor/character type or row order breaks Step 3's strict identity checks. They fail loudly ("ordering mismatch"), not silently, which is good, but the parity test is mandatory.
- **Reference weather storage.** The resolver must work through the store lease. Step 3 may run after the store has been released if the lease lifecycle is wrong, so check `step2_weather_store_release()` timing against Step 3 usage.
- **Imported or exported runs.** If saved runs serialize pipelines, add a `schema` check so v1 payloads still resolve (pass-through) and v2 payloads are handled.
- **Step 3 compute moves from memory to CPU.** It is acceptable if the rebuild is in the tens of ms per member. Measure it first.

## 9. Related, smaller items (already done or separate)

- qs2 result artifacts now use 2 threads (`.WISE_STEP2_QS2_THREADS`, `R/fct_step2_async.R`). Measured 1 -> 2 threads: write 5.2 -> 2.2 s, read 3.9 -> 2.3 s on the 196 MB BFA file.
- Lazy per-scenario loading (option D) would be a separate plan if memory is still a problem after A.
