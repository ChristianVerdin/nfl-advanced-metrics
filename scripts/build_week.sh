#!/bin/zsh
# Weekly refresh: a new data snapshot, then the week-by-week W charts and P4, in light (out/) and dark (out_dark/).
#   scripts/build_week.sh            # pull fresh nflverse data, then the charts
#   scripts/build_week.sh --charts   # charts only, from the existing data/ snapshot
set -e
cd "$(dirname "$0")/.."
[ -f scripts/local.env ] && . scripts/local.env
RS=${RSCRIPT:-Rscript}
if [ "${1:-}" != "--charts" ]; then
  $RS R/01_data.R
  $RS R/01b_history.R
  $RS R/01c_extra.R
  $RS R/01d_weekly.R
fi
LOG=/tmp/nfl_viz_week.log
: > $LOG
for theme in light dark; do
  NFL_VIZ_THEME=$theme $RS R/run_all.R weekly 2>&1 | tee -a $LOG | grep -E "wrote|FAILED|TOO LONG"
  NFL_VIZ_THEME=$theme $RS R/run_all.R wind_passing 2>&1 | tee -a $LOG | grep -E "wrote|FAILED|TOO LONG"
done
if grep -q FAILED $LOG; then echo "WEEKLY BUILD HAD FAILURES"; exit 1; fi
echo "weekly build complete: $(ls out/W*.png | wc -l | tr -d ' ') W charts"
