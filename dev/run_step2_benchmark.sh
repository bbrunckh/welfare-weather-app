#!/usr/bin/env bash
# Run the Step 2 benchmark with an external peak-RSS measurement.
#
# Usage:
#   WISEAPP_DATA_PATH=/path/to/data \
#   WISEAPP_STEP2_PAYLOAD_MODE=legacy|compact \
#   WISEAPP_STEP2_WEATHER_STORAGE=memory|reference \
#   WISEAPP_STEP2_WEATHER_COLLECT=fast|bounded \
#     dev/run_step2_benchmark.sh
#
# Remote data instead of a local directory (reads the DATABRICKS_* variables):
#   WISEAPP_STEP2_DATA_SOURCE=databricks dev/run_step2_benchmark.sh
#
# Opt-in Step 3 fixture:
#   WISEAPP_STEP2_INCLUDE_STEP3=1 \
#   WISEAPP_STEP3_POLICIES=covariate,targeted_sp,combined \
#     dev/run_step2_benchmark.sh
#
# Opt-in per-pipeline memory profile (serialises every pipeline, so it slows
# the run and should not be combined with timing comparisons):
#   WISEAPP_STEP2_MEMORY_PROFILE=1 dev/run_step2_benchmark.sh
#
# Fabricated smoke-only fixture, never production-path evidence:
#   WISEAPP_STEP2_FIXTURE=smoke WISEAPP_STEP2_INCLUDE_STEP3=1 \
#     dev/run_step2_benchmark.sh
#
# The R harness writes structured reports. This wrapper adds the complete
# process-tree peak RSS from /usr/bin/time (-l on macOS, -v with GNU time) to
# console.log.

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
output_dir="${WISEAPP_STEP2_OUTPUT_DIR:-$repo_root/dev/outputs/step2-benchmark}"
mkdir -p "$output_dir"

if [[ "$(uname -s)" == "Darwin" ]]; then
  time_flag="-l"
  rss_pattern='maximum resident set size'
  rss_to_bytes=1          # macOS reports bytes
else
  time_flag="-v"
  rss_pattern='Maximum resident set size'
  rss_to_bytes=1024       # GNU time reports kilobytes
fi

cd "$repo_root"
/usr/bin/time "$time_flag" Rscript dev/bench_step2.R 2>&1 | tee "$output_dir/console.log"

external_rss="$(awk -v pat="$rss_pattern" '
  index($0, pat) { value = ($1 ~ /^[0-9]+$/) ? $1 : $NF }
  END { print value }' "$output_dir/console.log")"
if [[ -n "$external_rss" ]]; then
  printf 'scope,metric,value,unit\nbenchmark_process,maximum_resident_set_size,%s,bytes\n' \
    "$((external_rss * rss_to_bytes))" > "$output_dir/external_process_metrics.csv"
fi
