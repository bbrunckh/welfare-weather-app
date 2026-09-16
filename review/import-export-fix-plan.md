# Import/Export Review Fix Plan

## Scope

Fix import/export correctness and output alignment across Steps 0-3 without changing the data-source boundary.

The imported configuration will not apply connection settings, credentials, source paths, or the Overview connection action. Users must connect the intended data source before importing, as the current UI instructs. The import flow will restore analysis controls and replay the analysis against the already-active source.

## Implementation Plan

### 1. Make import replay deterministic and safe

- Preserve data-source independence:
  - Keep connection/source controls excluded from replay.
  - Keep the existing import prerequisite message.
  - Add validation or status text making clear that the active source is intentionally not restored.
- Restore the exported `random_seed` into the import run context.
  - Extend the pipeline/import callback path to carry a per-import seed.
  - Pass that seed into Step 1, Step 2, and Step 3 computation paths that currently use `WISEAPP_DEFAULT_SEED`.
  - Ensure the imported seed affects all stochastic operations while manual runs retain the existing default behavior.
  - Record the effective seed in provenance and ensure it is visible in the exported README.
- Prevent the pipeline from firing while deferred controls remain pending.
  - Add a pending-control readiness check to the pipeline settle phase.
  - Continue retrying dynamic controls until they are applied or the existing retry deadline is reached.
  - On deadline, fail or explicitly stop the replay rather than silently running with defaults.
  - Keep manual, non-pipeline imports unchanged: apply available settings and report deferred settings.
- Cancel deferred replay when import is cancelled or the modal closes.
  - Clear `import_state$pending` and wake/retract the retry observer.
  - Ensure a later dynamic UI render cannot apply a cancelled configuration.
- Treat `sendInputMessage()` failures as pending/failed rather than applied.
  - Track failed IDs separately or return them in `pending` with an error note.
  - Surface a warning naming controls that could not be restored.
  - Preserve retry for controls that may become valid after UI creation.
- Validate JSON shape before `.import_validate()` accesses `$` fields.
  - Reject non-object JSON, missing/non-list `inputs`, invalid input names, and malformed version fields with a controlled import status.
  - Add tests for `[]`, scalar JSON, and malformed `inputs`.

### 2. Apply stale protection consistently

- Generalize export registry stale handling from Step 3 to Steps 1-3.
  - Allow each registration to receive the relevant stale reactive.
  - Provide module-level stale references for Step 1 and Step 2, including import-induced stale state.
  - Make `.export_write_item()` suppress materialization for any stale registered item.
- Gate direct CSV handlers consistently.
  - Pass Step 2 stale state to `threshold_csv` and any other direct handlers.
  - Verify Step 1/2/3 direct downloads fail cleanly and do not create files while stale.
- Keep stale artefacts visible in the UI with the existing banners, but exclude them from bundles and direct downloads.
- Ensure provenance/README clearly marks stale runs when stale artefacts are skipped.

### 3. Synchronize dynamic registry lifecycle

- Add an explicit registry removal/reconciliation API, such as `wise_export_remove()` or `wise_export_retain(prefix, keys)`.
- Reconcile dynamic registrations after every successful reactive rebuild:
  - Step 3 diagnostic `policy_before_after_<var>` figures: remove variables absent from the current `manipulated_vars`.
  - Step 1 weather exports: remove slots no longer present after a weather selection changes.
  - Remove hidden continuous-distribution entries when the current variable is not binned.
  - Remove Step 1 model entries for plots intentionally absent from the UI, including non-RIF homogeneity notes and hidden RIF coefficient plots.
- Ensure failed runs do not replace or delete the last successful registry closures unless the corresponding current output is intentionally unavailable.
- Add registry lifecycle tests covering variable removal, weather-count shrinkage, model-family changes, and failed reruns.

### 4. Align exported figures with visible plots

Use shared plot-builder functions or shared argument helpers so each exported figure calls the same builder and options as its visible renderer.

- Step 3:
  - Replace annual trajectory export inputs with the same `timeseries_curves_rv()` data used by the visible plot.
  - Pass the same aggregation, deviation, labels, uncertainty, scenario, and return-period settings.
  - Replace adverse-period export inputs with `adverse_dot_data_rv()` and the visible plot arguments.
  - Keep paired-effect exports only when they correspond to a separately visible paired-effect surface, with distinct keys/labels.
- Step 2 diagnostics:
  - Pass `tc$ens_q` to trajectory exports.
  - Pass weather labels and `log_x` to density exports.
  - Use the same selected variables/scenarios as the visible renderers.
- Step 1:
  - Pass `wave_labels` to outcome and weather plots.
  - Use the same residual axis label builder as the visible residual plots.
  - Match all visible plot flags, labels, and selection-dependent options.
- Add focused parity tests that capture the plot-builder calls or compare plot layer/data/labels for representative Step 1, Step 2, and Step 3 cases.

### 5. Align exported tables and direct CSVs with visible tables

- Extract visible table data-frame builders where needed and use them for:
  - Step 2 weather support summary.
  - Step 3 policy input diagnostics.
  - Step 3 transfer summary.
  - Step 3 treatment assignment.
  - Step 3 component summary.
- Decide and document the export contract:
  - Bundle CSVs should use the same displayed/formatted table data where the feature promises output parity.
  - Raw analytical data should remain available under explicitly named `*_data` exports.
- Make direct CSV handlers use the same flattening/formatting policy as bundle writers, including list and matrix columns.
- Ensure direct CSV availability and stale gating match the visible DT button state.
- Add tests comparing the data frame passed to the visible table and its bundle/direct CSV export.

### 6. Fix naming and bundle consistency

- Use a stable registry order to assign filenames before filtering by `include`, or derive filenames from stable item identity rather than filtered position.
- Ensure the same artefact has the same filename in all, tables-only, and figures-only bundles.
- Update filename tests and README wording if the contract is intentionally changed; preferred behavior is stable names.

### 7. Preserve clear import/export semantics

- Keep source identity/provenance in exports, but never restore credentials or connection settings.
- Document in the import status/README that source settings are intentionally excluded and must be configured first.
- Include the effective replay seed and whether the run used imported or default seed in provenance.
- Do not export UI-only controls or surfaces that are not currently rendered.

## Verification Plan

Run focused tests for:

- Configuration validation and input application.
- Imported seed propagation and deterministic replay.
- Deferred-control readiness, timeout, cancellation, and failed input messages.
- Stale Step 1/2/3 bundle and direct CSV suppression.
- Dynamic registry cleanup across reruns.
- Stable filenames across bundle modes.
- Step 1/2/3 visible/export table and figure parity.

Then run:

- The complete export/import test set.
- The complete Step 1-3 simulation and output contract tests.
- The full package test suite.
- Syntax parsing and `git diff --check`.

No implementation changes are included in this plan-only step.
