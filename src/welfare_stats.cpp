// Welfare statistics kernel ----
// Single-pass per-group computation of the nine aggregation statistics used
// by `resolve_agg_fn()` (fct_aggregation.R): mean, median, total,
// headcount_ratio, poverty_gap, poverty_severity (fgt2), gini,
// prosperity_gap, avg_poverty.
//
// Parity contract (mandatory, see review/optimize_aggregation_welfare.md):
// each statistic mirrors the exact NA/Inf/weight rules of its
// `resolve_agg_fn()` implementation:
//   mean/headcount/prosperity_gap use
//     stats::weighted.mean(x, w, na.rm = TRUE) / mean(x, na.rm = TRUE)
//     filtered on !is.na(welfare);
//   gap/fgt2 filter on !is.na(normalised shortfall) - equal to the welfare
//     filter except at (line == 0, welfare == 0) where the shortfall is NaN;
//   total is sum(welfare * weights, na.rm = TRUE);
//   avg_poverty filters is.finite(welfare) & welfare > 0;
//   weighted median = first sorted non-missing welfare value where the
//     cumulative normalised weight reaches 0.5 (missing welfare rows are
//     dropped first; NA weights poison the cumulative sum from their
//     position onward);
//   unweighted median = stats::median(x, na.rm = TRUE) (even-n mean of the
//     two middle values);
//   gini drops is.na(welfare) first, requires n >= 2, and uses the weighted
//     covariance form (equal weights when unweighted).
// Naive double accumulation differs from R's long-double `sum()` in the
// last ulps; callers must compare with determinism tolerance, not identical().

#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>

using namespace Rcpp;

namespace {

// Accumulator reproducing R's sum() (no na.rm) poisoning semantics: any NA
// contribution wins over NaN, NaN over finite.
struct PoisonSum {
  double s;
  bool has_na;
  bool has_nan;

  PoisonSum() : s(0.0), has_na(false), has_nan(false) {}

  inline void add(double v) {
    if (ISNAN(v)) {
      if (R_IsNA(v)) has_na = true; else has_nan = true;
    } else {
      s += v;
    }
  }

  inline void add_prod(double a, double b) {
    const double v = a * b;
    if (ISNAN(v)) {
      // Resolve NA-vs-NaN from the operands: NaN payloads may be
      // canonicalised by the hardware on multiplication.
      if (R_IsNA(a) || R_IsNA(b)) has_na = true; else has_nan = true;
    } else {
      s += v;
    }
  }

  inline double value() const {
    if (has_na) return NA_REAL;
    if (has_nan) return R_NaN;
    return s;
  }

  inline bool poisoned() const { return has_na || has_nan; }
};

// Sort entry mirroring order(x): ascending value, NaN then NA last, ties in
// original row order (stable). The missing rank is precomputed in the scan
// pass so the comparator never calls R_IsNA/isnan - std::sort evaluates it
// ~N log N times and the R API calls dominated the kernel's runtime.
struct SortEntry {
  double y;
  double w;
  int idx;
  int rank; // 0 = finite, 1 = NaN, 2 = NA
};

inline bool entry_less(const SortEntry& a, const SortEntry& b) {
  if (a.rank != b.rank) return a.rank < b.rank;
  if (a.rank > 0) return a.idx < b.idx;
  if (a.y != b.y) return a.y < b.y;
  return a.idx < b.idx;
}

} // anonymous namespace

//' Compute All Nine Welfare Aggregation Statistics in One Pass
//'
//' Single-pass kernel mirroring \code{resolve_agg_fn()} exactly (to float
//' tolerance): one scan in row order for the scan statistics plus one
//' (value, index)-stable sort shared by the weighted/unweighted median and
//' the Gini. The stable sort order is attached as attribute \code{"order"}
//' so callers can reuse it for downstream gradient computations.
//'
//' @param y Numeric welfare values.
//' @param w Numeric weights, or an empty vector for unweighted statistics.
//' @param pov_line Headcount threshold; NA yields NA for headcount_ratio.
//' @param gap_line Poverty-gap threshold; NA yields NA for poverty_gap.
//' @param sev_line Poverty-severity threshold; NA yields NA for fgt2.
//' @param prosp_line Prosperity threshold, hardcoded $28/day by the app.
//'
//' @return Named numeric vector of length 9 with the statistics in the fixed
//'   order mean, median, total, headcount_ratio, poverty_gap,
//'   poverty_severity, gini, prosperity_gap, avg_poverty, plus attribute
//'   \code{"order"}: the 1-based stable sort index of \code{y}.
//'
//' @noRd
// [[Rcpp::export]]
NumericVector welfare_stats_all(NumericVector y, NumericVector w,
                                double pov_line, double gap_line,
                                double sev_line, double prosp_line) {
  const int n = y.size();
  if (w.size() != 0 && w.size() != n) {
    stop("'y' and 'w' must have the same length");
  }
  const bool has_w = w.size() == n;

  // Scan-pass accumulators (row order, mirroring R's sums).
  PoisonSum W;        // weighted.mean denominator / unweighted count
  PoisonSum WY;       // mean numerator: sum(w * y)
  PoisonSum W_total;  // total: sum(y * w) with na.rm = TRUE semantics
  PoisonSum W_gap;    // poverty-gap denominator (shortfall filter)
  PoisonSum gap_w;    // poverty-gap numerator
  PoisonSum W_sev;    // severity denominator (shortfall filter)
  PoisonSum sev_w;    // severity numerator
  PoisonSum pros_w;   // prosperity numerator (shares W denominator)
  PoisonSum ap_num;   // avg_poverty numerator: sum(w / y)
  PoisonSum W_ap;     // avg_poverty denominator
  PoisonSum poor_w;   // headcount numerator (shares W denominator)

  const bool pov_na = ISNAN(pov_line);
  const bool gap_na = ISNAN(gap_line);
  const bool sev_na = ISNAN(sev_line);

  std::vector<SortEntry> sorted;
  sorted.reserve(n);

  for (int i = 0; i < n; ++i) {
    const double yi = y[i];
    const double wi = has_w ? w[i] : 1.0;
    const bool y_missing = ISNAN(yi);

    if (!y_missing) {
      // weighted.mean family (mean/headcount/prosperity): the R filter is
      // !is.na(welfare); NA weights poison the sums from here.
      if (ISNAN(wi)) {
        if (R_IsNA(wi)) W.has_na = true; else W.has_nan = true;
      } else {
        W.s += wi;
        WY.add_prod(wi, yi);
      }

      // total: sum(y * w, na.rm = TRUE) drops NA/NaN products (NA weights,
      // Inf * 0) and keeps everything else, including infinities.
      if (!ISNAN(wi)) {
        const double prod = yi * wi;
        if (!ISNAN(prod)) W_total.s += prod;
      }

      // headcount_ratio contribution (y < pov); denominator shares W.
      if (!pov_na && yi < pov_line) {
        poor_w.add(wi);
      }

      // poverty_gap: normalised shortfall below gap_line.
      if (!gap_na) {
        if (yi < gap_line) {
          const double g = (gap_line - yi) / gap_line;
          if (ISNAN(g)) {
            W_gap.has_nan = true; // R filter drops the NaN shortfall row
          } else {
            W_gap.add(wi);
            gap_w.add_prod(wi, g);
          }
        } else {
          W_gap.add(wi);
        }
      }

      // poverty_severity: squared normalised shortfall below sev_line.
      if (!sev_na) {
        if (yi < sev_line) {
          const double g2 = (sev_line - yi) / sev_line;
          if (ISNAN(g2)) {
            W_sev.has_nan = true;
          } else {
            W_sev.add(wi);
            sev_w.add_prod(wi, g2 * g2);
          }
        } else {
          W_sev.add(wi);
        }
      }

      // prosperity_gap: pmax(prosp_line / y, 1); finite y only, so the
      // R filter (!is.na(pg)) equals the welfare filter here.
      {
        const double t = prosp_line / yi;
        const double pg = t > 1.0 ? t : 1.0;
        pros_w.add_prod(wi, pg);
      }

      // avg_poverty: is.finite(y) & y > 0, weighted mean of 1 / y.
      if (R_FINITE(yi) && yi > 0.0) {
        if (ISNAN(wi)) {
          if (R_IsNA(wi)) W_ap.has_na = true; else W_ap.has_nan = true;
        } else {
          W_ap.s += wi;
          ap_num.add_prod(wi, 1.0 / yi);
        }
      }
    }

    SortEntry e;
    e.y = yi;
    e.w = wi;
    e.idx = i;
    e.rank = y_missing ? (R_IsNA(yi) ? 2 : 1) : 0;
    sorted.push_back(e);
  }

  std::sort(sorted.begin(), sorted.end(), entry_less);

  // Non-missing prefix of the sorted array; shared by both medians and the
  // Gini (missing welfare sorts last, so rows [0, n_valid) are the values
  // R obtains from x[!is.na(x)] followed by order()).
  int n_valid = 0;
  while (n_valid < n && sorted[n_valid].rank == 0) ++n_valid;

  // --- median ----
  double median_v = NA_REAL;
  if (has_w) {
    // Weighted: R drops missing welfare rows, then the denominator is
    // sum(weights, na.rm = TRUE) - NA weights are dropped from the
    // denominator but retained in the cumulative sum, poisoning it from
    // their sorted position onward.
    double denom_med = 0.0;
    for (int i = 0; i < n_valid; ++i) {
      const double wi = sorted[i].w;
      if (ISNAN(wi)) {
        continue; // na.rm = TRUE drops NA/NaN from the denominator sum
      }
      denom_med += wi;
    }
    if (n_valid > 0) {
      double cumw = 0.0;
      for (int i = 0; i < n_valid; ++i) {
        const double wn = sorted[i].w / denom_med;
        if (ISNAN(wn)) break; // NA weight: cumulative sum poisoned onward
        cumw += wn;
        if (cumw >= 0.5) {
          median_v = sorted[i].y;
          break;
        }
      }
    }
  } else {
    // Unweighted: stats::median(y, na.rm = TRUE) type-7 semantics.
    if (n_valid == 0) {
      median_v = NA_REAL;
    } else if (n_valid % 2 == 1) {
      median_v = sorted[(n_valid - 1) / 2].y;
    } else {
      median_v = (sorted[n_valid / 2 - 1].y + sorted[n_valid / 2].y) / 2.0;
    }
  }

  // --- gini ----
  double gini_v = NA_REAL;
  if (n_valid >= 2) {
    // Unweighted rows carry w = 1 (equal weights), so both cases share
    // the one Gini definition.
    // R: w <- weights[valid][ord] / sum(weights[valid], na.rm = TRUE);
    //    2 * sum(w * y * (cumsum(w) - w / 2)) / sum(w * y) - 1
    // An NA weight keeps its NA into the products, so the R result is NA.
    double denom_g = 0.0;
    for (int i = 0; i < n_valid; ++i) {
      const double wi = sorted[i].w;
      if (ISNAN(wi)) {
        continue; // denominator drops NA/NaN weights (na.rm = TRUE)
      }
      denom_g += wi;
    }
    if (!ISNAN(denom_g)) {
      PoisonSum s_g;  // sum(w * y * F_i) with normalised w
      PoisonSum wy_g; // sum(w * y)
      double cum = 0.0;
      bool poison_na = false;
      bool poison_nan = false;
      for (int i = 0; i < n_valid; ++i) {
        const double wn = sorted[i].w / denom_g;
        if (ISNAN(wn)) {
          // NA weight -> R products become NA (gini NA); a NaN quotient
          // (0/0, Inf/Inf) becomes NaN (gini NaN). Mirrors R exactly.
          if (R_IsNA(wn)) poison_na = true; else poison_nan = true;
          break;
        }
        cum += wn;
        const double F = cum - 0.5 * wn;
        s_g.add_prod(wn * sorted[i].y, F);
        wy_g.add_prod(wn, sorted[i].y);
      }
      if (!poison_na && !poison_nan) {
        gini_v = 2.0 * s_g.value() / wy_g.value() - 1.0;
      } else if (poison_na) {
        gini_v = NA_REAL;
      } else {
        gini_v = R_NaN;
      }
    }
  }

  // --- assemble ----
  // Plain IEEE division reproduces R's ratio behaviour for zero/poisoned
  // denominators (0/0 = NaN, x/0 = +-Inf, NA/x = NA).
  const double mean_v = WY.value() / W.value();
  const double total_v = W_total.value();
  const double hc_v = pov_na ? NA_REAL : poor_w.value() / W.value();
  const double gap_v = gap_na ? NA_REAL : gap_w.value() / W_gap.value();
  const double sev_v = sev_na ? NA_REAL : sev_w.value() / W_sev.value();
  const double pg_v = pros_w.value() / W.value();
  const double ap_v = ap_num.value() / W_ap.value();

  NumericVector out = NumericVector::create(
    _["mean"] = mean_v,
    _["median"] = median_v,
    _["total"] = total_v,
    _["headcount_ratio"] = hc_v,
    _["gap"] = gap_v,          // resolve_agg_fn's poverty_gap
    _["fgt2"] = sev_v,         // resolve_agg_fn's poverty_severity
    _["gini"] = gini_v,
    _["prosperity_gap"] = pg_v,
    _["avg_poverty"] = ap_v
  );

  IntegerVector order(n);
  for (int i = 0; i < n; ++i) {
    order[i] = sorted[i].idx + 1;
  }
  out.attr("order") = order;
  return out;
}
