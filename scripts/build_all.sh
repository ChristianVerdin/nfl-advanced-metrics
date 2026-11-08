#!/bin/zsh
# One command: data snapshot, history, headshot sprites, every chart, the gt board screenshot.
#   scripts/build_all.sh            # full rebuild (pulls fresh nflverse data)
#   scripts/build_all.sh --charts   # charts only, from the existing data/ snapshot
set -e
cd "$(dirname "$0")/.."
RS=${RSCRIPT:-Rscript}
PW=${PLAYWRIGHT_PY:-python3}
if [ "${1:-}" != "--charts" ]; then
  $RS R/01_data.R
  $RS R/01b_history.R
  $RS R/02_headshots.R
fi
$RS R/run_all.R 2>&1 | tee /tmp/nfl_viz_build.log | grep -E "wrote|FAILED|TOO LONG|shrunk"
$PW scripts/shoot.py "$PWD/out/14_qb_efficiency_board.html" "$PWD/out/14_qb_efficiency_board.png" 1400
if grep -q FAILED /tmp/nfl_viz_build.log; then echo "BUILD HAD FAILURES"; exit 1; fi
echo "build complete: $(ls out/*.png | wc -l | tr -d ' ') charts"
