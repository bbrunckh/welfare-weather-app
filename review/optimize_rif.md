# Speed opportunities: RIF quantile regression (`fct_rif_sim.R`)

**Status: candidate — unvalidated, low priority. Consider for implementation; not enforced (see `optimization_guidelines.md`). Objective: raw speed.**

Most speed work has already landed: RIF columns are computed once per tau and reused
across the three progressive fits (`fct_fit_model.R` ~241, PERF-03), and the `dens=`
hook shares one KDE across taus. Only pursue the items below after profiling shows
bandwidth/density/quantile evaluation is actually hot on a Section 9 payload.

## 1. Bandwidth selection is the slow primitive

`stats::bw.SJ()` (with `nrd0` fallback) does cross-validated bandwidth selection — the
most expensive call on large samples. Options, in increasing invasiveness:

- Compute `bw.SJ()` on a subsample above a size threshold; document the tolerance and
  verify RIF outputs stay within it (values still must be deterministic — seed the
  subsample or use a deterministic stride).
- Switch the default to `bw.nrd0()` for large n. This is a parity break with
  `test-fct_rif_sim.R` expectations — a deliberate decision requiring test updates and
  a sign-off that downstream RIF quantile estimates are insensitive.

## 2. Quantile extraction micro

`stats::quantile()` → `collapse::fquantile()` (quickselect). Micro; safe. Note: the
original fragment's `fnquantile()` does not exist.

## Traps

- Do not replace the KDE grid + scale-aware floor with a single-point
  `density(n = 2)` evaluation — it changes density values and breaks parity.
- Do not recompute density per tau (the shared-`dens` path exists).
- The multi-tau grid path (~line 317) already amortizes across taus; don't duplicate it.
