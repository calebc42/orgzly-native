#!/usr/bin/env bash
#
# Deploy the orgzly-native bundle (and optionally the companion APK) to the
# connected Android device. The bash sibling of deploy.ps1, for Linux/macOS
# (or WSL) where emacs is on PATH directly — no WSL path translation.
#
# Default: rebuild orgzly.el from emacs/apps/orgzly/*.el via Emacs, then
# adb-push it to /sdcard/Download/orgzly.el. Termux is not debuggable, so
# adb cannot write into /data/data/com.termux directly — the jetpacs
# starter init (jetpacs/docs/starter-init.el) adopts a newer staged bundle
# from /sdcard/Download (or /sdcard/Documents) on Emacs startup.
#
# The bundle `require`s jetpacs-core — pass --core to also deploy the
# vendored jetpacs/jetpacs-core.el alongside it (first deploy, or after a
# jetpacs update).
#
# Usage:
#   ./deploy.sh                 # rebuild + stage to /sdcard/Download
#   ./deploy.sh --core          # also stage jetpacs-core.el
#   ./deploy.sh --ssh           # rebuild + scp straight into Termux's ~/.emacs.d/elisp
#   ./deploy.sh --apk           # also build + install the companion APK
#   ./deploy.sh --ssh --core --apk
#
# --ssh one-time setup inside Termux:
#   pkg install openssh && passwd && sshd
# Optional passwordless push: append ~/.ssh/id_ed25519.pub to Termux's
# ~/.ssh/authorized_keys. sshd must be running on the device when you deploy.

set -euo pipefail

use_ssh=0
use_apk=0
use_core=0
for arg in "$@"; do
  case "$arg" in
    --ssh) use_ssh=1 ;;
    --apk) use_apk=1 ;;
    --core) use_core=1 ;;
    -h|--help) grep '^#' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "Unknown option: $arg" >&2; exit 2 ;;
  esac
done

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
bundle="$repo/orgzly.el"
core="$repo/jetpacs/jetpacs-core.el"

# In WSL without a Linux adb, fall back to the Windows one on PATH.
adb=adb
command -v adb >/dev/null 2>&1 || adb=adb.exe

echo '-- Rebuilding orgzly.el from emacs/apps/orgzly/*.el ...'
emacs --batch -l "$repo/emacs/build-bundle.el"

echo '-- Checking device ...'
"$adb" get-state >/dev/null

if [ "$use_ssh" -eq 1 ]; then
  echo '-- Pushing directly into Termux home via sshd (port 8022) ...'
  "$adb" forward tcp:8022 tcp:8022 >/dev/null
  ssh -p 8022 termux@127.0.0.1 'mkdir -p .emacs.d/elisp'
  scp -P 8022 "$bundle" termux@127.0.0.1:.emacs.d/elisp/orgzly.el
  if [ "$use_core" -eq 1 ]; then
    scp -P 8022 "$core" termux@127.0.0.1:.emacs.d/elisp/jetpacs-core.el
  fi
  echo '   Installed to ~/.emacs.d/elisp/ - reload or restart Emacs.'
else
  echo '-- Staging to /sdcard/Download (adopted by the starter init on Emacs restart) ...'
  "$adb" push "$bundle" /sdcard/Download/orgzly.el
  if [ "$use_core" -eq 1 ]; then
    "$adb" push "$core" /sdcard/Download/jetpacs-core.el
  fi
  echo '   Staged. Restart Emacs on the device (or eval the adopt snippet) to pick it up.'
fi

if [ "$use_apk" -eq 1 ]; then
  echo '-- Building + installing the jetpacs companion APK ...'
  "$repo/jetpacs/gradlew" -p "$repo/jetpacs" installDebug
fi

echo 'Deploy complete.'
