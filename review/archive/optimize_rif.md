# Speed opportunities: RIF quantile regression (`fct_rif_sim.R`)

**Status: candidate — low priority. Consider for implementation; not enforced (see `optimization_guidelines.md`). Objective: raw speed.**

Most speed work has already landed: RIF columns are computed once per tau and reused
across the three progressive fits (`fct_fit_model.R` ~241, PERF-03), and the `dens=`
hook shares one KDE across taus.

**2026-09-18 update: profiled and implemented on real Section 9 payloads
(BFA n=14,186; IRN n=305,794 welfare rows). `compute_rif_multi()` now runs the
quantile extraction through `collapse::fquantile()` (quickselect) and replaces the
9-pass per-tau arithmetic loop with a single `findInterval()` lookup plus a
(K+1) x K pattern gather; `compute_rif()` shares the quantile change. Measured
(median, `dev/bench_rif_kernels.R`): IRN rif_multi 48.3ms -> 22.9ms, full
`prepare_outcome` 56.6ms -> 37.4ms (-34%), transient allocations -24% (124 -> 94MB);
BFA prepare_outcome 3.2ms -> 2.4ms. Parity: bit-identical on the model scale
(log welfare) and on all tied/constant/discrete edge cases; last-ulp
(rel ~1.7e-16) differences only between distinct order statistics on raw
continuous inputs — no indicator flips observed (verified zero exact ties at
the quantiles on both payloads). The kernels run once per model fit inside
`synchronous` `observeEvent` fitting, so the reduction directly reduces
main-process blocking.**

## 1. Bandwidth selection is the slow primitive — MEASURED, NOT HOT; SUBSAMPLING REJECTED

`stats::bw.SJ()` (with `nrd0` fallback) does cross-validated bandwidth selection.
Profiled on real payloads it is **not hot**: ~10ms at n=306k (binned
implementation), ~0.7ms at n=14k — dwarfed by the surrounding fit. Measured options:

- ~~Compute `bw.SJ()` on a subsample above a size threshold~~ **rejected on
  evidence**: a deterministic stride subsample of ~100k from the IRN welfare
  payload changes the bandwidth by **+136.9%** (108.2 vs 45.7), and even worse at
  smaller subsample sizes (up to ~17,000% at 25k) because stride sampling misses
  the tail structure of skewed welfare distributions. The resulting RIF values
  would be materially wrong, not just noisy. Do not revisit without a
  distribution-aware subsample design.
- ~~Switch the default to `bw.nrd0()` for large n~~ — unnecessary: `bw.SJ()` is
  ~10ms at the largest Section 9 payload; a parity break is not justified by any
  measured win.

## 2. Quantile extraction micro — ADOPTED 2026-09-18

`stats::quantile()` → `collapse::fquantile()` (quickselect): 2.7x on the
quantile stage at n=306k (20.3ms -> 7.6ms median). Validation notes found during
implementation:

- `collapse::fquantile()` **requires ascending probabilities** (errors otherwise),
  so `compute_rif_multi()` sorts taus, computes in sorted space, and permutes the
  result columns back to the caller's tau order. `stats::quantile()` accepted any
  order.
- Values are bit-identical to `stats::quantile()` type 7 on the log model scale
  and on tied/discrete data; between distinct order statistics on raw continuous
  inputs the interpolation differs in the last ulp (max rel 1.7e-16 measured on
  the IRN raw payload). Downstream RIF coefficients shift at the ~1e-15 relative
  level — negligible for displays and cards.
- Watch for: the RIF indicator `y <= q` is only sensitive to ulp differences when
  an observation exactly equals a quantile; verified zero exact ties at quantiles
  on both payloads and bit-identical output on the tie-heavy test cases.
- Bonus found while profiling: the dominant cost of the old kernel was the
  per-tau construction loop (~6 n-sized allocations per tau). The pattern-gather
  rewrite cut the shared-dens `compute_rif_multi` stage from 48.3ms to 22.9ms
  (IRN) and 1.77ms to 1.00ms (BFA).

## Traps

- Do not replace the KDE grid + scale-aware floor with a single-point
  `density(n = 2)` evaluation — it changes density values and breaks parity.
- Do not recompute density per tau (the shared-`dens` path exists).
- The multi-tau grid path (~line 317 in the pre-2026-09 file, now the pattern
  gather in `compute_rif_multi()`) already amortizes across taus; don't duplicate it.
- When touching `compute_rif()` and `compute_rif_multi()`, change both together:
  the test suite asserts `identical()` between the single-tau and multi-tau
  paths, so a quantile-implementation split between them breaks the suite.
- `findInterval()` grouping must use `left.open = TRUE` to reproduce the
  `y <= q` indicator exactly (it returns `#{q < y}`; default ties-to-the-right
  semantics give the wrong group for observations exactly at a quantile).
