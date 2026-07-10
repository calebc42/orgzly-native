#!/usr/bin/env bash
# Run the orgzly-native ERT suites against the vendored jetpacs core.
set -euo pipefail
cd "$(dirname "$0")/.."

EMACS="${EMACS:-emacs}"

"$EMACS" --batch \
  -L jetpacs/emacs/core \
  -L emacs/apps/orgzly \
  -L test \
  -l orgzly-query-test \
  -l orgzly-data-test \
  -l orgzly-agenda-test \
  -l orgzly-views-test \
  -f ert-run-tests-batch-and-exit </dev/null
