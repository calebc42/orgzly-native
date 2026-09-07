#!/usr/bin/env bash
set -euo pipefail
orgzly_root=$(cd -- "$(dirname "$0")/.." && pwd)
repositories_root=${JETPACS_REPOSITORIES_ROOT:-$(dirname "$orgzly_root")}
jetpacs_root=${JETPACS_ROOT:-$repositories_root/jetpacs-poc}
paths=(-L "$repositories_root/ebp.el/lisp" -L "$jetpacs_root/emacs"
       -L "$jetpacs_root/jetpacs-components/lisp/jetpacs-components"
       -L "$orgzly_root/emacs/apps/orgzly" -L "$orgzly_root/test")
"${EMACS:-emacs}" -Q --batch "${paths[@]}" --eval '(setq load-prefer-newer t)' \
  -l orgzly-query-test -l orgzly-data-test -l orgzly-agenda-test -l orgzly-current-test \
  -f ert-run-tests-batch-and-exit
compile_dir=$(mktemp -d /tmp/orgzly-byte-compile.XXXXXX)
trap 'rm -rf "$compile_dir"' EXIT
ORGZLY_SOURCE_ROOT="$orgzly_root" "${EMACS:-emacs}" -Q --batch "${paths[@]}" \
  --eval '(setq load-prefer-newer t)' -l orgzly \
  --eval '(progn (require (quote checkdoc)) (let ((files (directory-files (concat (getenv "ORGZLY_SOURCE_ROOT") "/emacs/apps/orgzly") t (rx ".el" eos)))) (unless files (error "No source files for Checkdoc")) (dolist (file files) (checkdoc-file file))))' \
  > "$compile_dir/checkdoc.log" 2>&1
if test -s "$compile_dir/checkdoc.log"; then
  cat "$compile_dir/checkdoc.log"
  exit 1
fi
ORGZLY_COMPILE_DIR="$compile_dir" "${EMACS:-emacs}" -Q --batch "${paths[@]}" \
  --eval '(setq load-prefer-newer t byte-compile-error-on-warn t byte-compile-dest-file-function (lambda (file) (expand-file-name (concat (file-name-nondirectory file) "c") (getenv "ORGZLY_COMPILE_DIR"))))' \
  -f batch-byte-compile "$orgzly_root"/emacs/apps/orgzly/*.el
