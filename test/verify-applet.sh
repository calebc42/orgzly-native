#!/usr/bin/env bash
set -euo pipefail
orgzly_root=$(cd -- "$(dirname "$0")/.." && pwd)
repositories_root=${JETPACS_REPOSITORIES_ROOT:-$(dirname "$orgzly_root")}
jetpacs_root=${JETPACS_ROOT:-$repositories_root/jetpacs-poc}
output=${1:-$(mktemp -d /tmp/orgzly-verification.XXXXXX)}
mkdir -p "$output"
export JETPACS_ROOT="$jetpacs_root"
export ORGZLY_SOURCE_ROOT="$orgzly_root" ORGZLY_VERIFY_OUTPUT="$output" ORGZLY_FIXTURE_OUTPUT="$output/screens"
emacs -Q --batch -L "$repositories_root/ebp.el/lisp" -L "$jetpacs_root/emacs" \
  -L "$jetpacs_root/jetpacs-applet-mcp" \
  -L "$jetpacs_root/jetpacs-components/lisp/jetpacs-components" \
  -L "$orgzly_root/emacs/apps/orgzly" -L "$orgzly_root/test" \
  -l "$orgzly_root/test/verify-applet.el"
rg -q 'Status: PASS \(0 errors, 0 warnings\)' "$output/jetpacs-applet-tooling-validate-applet.txt"
rg -q 'Status: STABLE' "$output/render-determinism.txt"
printf 'Orgzly verification reports and screens: %s\n' "$output"
