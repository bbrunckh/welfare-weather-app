# Speed opportunities: welfare statistics aggregation (`fct_aggregation.R`)

**Status: candidate — unvalidated. Consider for implementation; not enforced (see `optimization_guidelines.md`). Objective: raw speed; parity with `resolve_agg_fn()` is mandatory.**

## 1. One sort per group instead of one sort per (group × method)

Today every statistic (`resolve_agg_fn()` ~42–145) independently orders and scans the
group: `median` and `gini` each call `order()`, `mean`/`gap`/`prosperity_gap`/
`avg_poverty` each rescan. With G methods × groups × simulation draws, that is
G redundant sorts and passes per group. Compute all statistics in one pass + one sort
per group. Two implementations, benchmark both:

- **`collapse` grouped functions** (`gmean`/`gmedian`/… with weights): no compiled code,
  likely most of the win. Benchmark these native collapse functions as well — they may
  match or beat the custom kernel without the compile dependency.
- **Rcpp kernel**: single-pass accumulators + single sort (below). Note `wiseapp` has no
  `src/` yet — this creates the first compile dependency (allowed by Section 2).

## 2. Corrected Rcpp kernel (starting point)

Nine statistics: `mean`, `median`, `total`, `headcount_ratio`, `poverty_gap`,
`poverty_severity` (the app's `fgt2`), `gini`, `prosperity_gap`, `avg_poverty`.
Thresholds are arguments: `pov_line` (headcount), `gap_line` (poverty gap),
`sev_line` (default 3), `prosp_line` (default 28). `resolve_agg_fn()` uses a single
`pov_line` for headcount, gap, and severity, so the kernel reproduces it exactly when
`gap_line == sev_line == pov_line`; distinct thresholds are a superset. NA rules differ
per statistic (`gini` drops NAs and renormalizes weights, `avg_poverty` filters to
`y > 0` and renormalizes, `median` propagates NA) — mirror each exactly:

```cpp
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector welfare_stats_cpp(NumericVector y, NumericVector w,
                                double pov_line, double gap_line,
                                double sev_line = 3.0, double prosp_line = 28.0) {
  std::vector<std::pair<double, double>> yw;
  yw.reserve(y.size());
  double W = 0, WY = 0, poor_w = 0, gap_w = 0, sev_w = 0,
         pros_w = 0, inv_w = 0, inv_W = 0;
  for (int i = 0; i < y.size(); ++i) {
    double yi = y[i], wi = w[i];
    if (!R_finite(yi)) continue;              // mean/total/gini drop NAs
    W += wi; WY += wi * yi;
    if (yi < pov_line) poor_w += wi;                  // headcount_ratio
    if (yi < gap_line) {                              // poverty_gap: normalized shortfall
      double g = (gap_line - yi) / gap_line;
      gap_w += wi * g;
    }
    if (yi < sev_line) {                              // poverty_severity: squared gap
      double g = (sev_line - yi) / sev_line;
      sev_w += wi * g * g;
    }
    pros_w += wi * std::max(prosp_line / yi, 1.0);   // prosperity_gap: pmax(prosp_line/y, 1)
    if (yi > 0.0) { inv_w += wi / yi; inv_W += wi; } // avg_poverty: weighted mean 1/y
    yw.emplace_back(yi, wi);
  }
  std::sort(yw.begin(), yw.end());
  double mean = WY / W;
  double cum = 0.0, median = yw.empty() ? NA_REAL : yw.back().first;
  for (auto& p : yw) { cum += p.second / W; if (cum >= 0.5) { median = p.first; break; } }
  double cum2 = 0.0, s = 0.0;                 // gini: covariance form F_i = cumsum(w)-w/2
  for (auto& p : yw) {
    double wn = p.second / W; cum2 += wn;
    s += wn * p.first * (cum2 - 0.5 * wn);
  }
  return NumericVector::create(
    _["mean"] = mean, _["median"] = median, _["total"] = WY,
    _["headcount_ratio"] = poor_w / W, _["poverty_gap"] = gap_w / W,
    _["poverty_severity"] = sev_w / W, _["gini"] = 2.0 * s / mean - 1.0,
    _["prosperity_gap"] = pros_w / W, _["avg_poverty"] = inv_w / inv_W);
}
```

Common formula errors to avoid (found in the original fragment): Gini sign-inverted
(`1 − …` vs `… − 1`; y={1,2} gives −1/6 vs correct +1/6); prosperity gap as shortfall
`1 − y/25` instead of multiplier `pmax(28/y, 1)`; average poverty as gap-ratio
approximation instead of weighted mean `1/y`; missing poverty severity.

Dispatch per group the same way `resolve_agg_fn()` is dispatched today; keep the R path
as fallback for uncovered methods.

## 3. Chunking across simulation years

If per-group R-side work dominates and draws span many years, chunk by year through the
existing `fct_step2_async.R` singleton (Section 8) — never a second daemon pool. Size
chunks so per-chunk serialization overhead doesn't eat the win; measure before adopting.

## 4. Keep the bounded preparation cache

The existing bounded preparation cache is the right shape — extend it (e.g. reuse sorted
orders across methods) rather than adding new cache layers.

## Traps

- Float accumulation order differs from R — check `test-determinism.R` tolerances.
- `avg_poverty` (`1/y`) is defined without a threshold — don't parameterize it. Keep
  `prosp_line = 28` as the default (matches the app's hardcoded $28/day) and
  `sev_line = 3` unless the UI changes them.
- Threshold parity: with `gap_line = sev_line = pov_line` the kernel must match
  `resolve_agg_fn()` exactly (to determinism tolerance); distinct thresholds are a
  superset the R path does not implement yet — wire them through the UI deliberately.
- Edge groups need parity coverage: all-NA, single row, zero/negative welfare.
