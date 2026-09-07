# Orgzly Native implementation guide

This is the recovered standalone Orgzly applet, not Jetpacs foundation.
Follow `../jetpacs-poc/AGENTS.md` and its applet guide. Protocol authority is
`../ebp/SPEC.md`. Run `test/run-tests.sh` before handing off.

Runtime source is `emacs/apps/orgzly/`. `legacy/` and `docs/LEGACY-PARITY.md`
are historical evidence and must never be added to the runtime load path.
Builders read the explicit snapshot only. Refresh owns disk reads and reference
creation. Wire actions carry opaque revision-bound IDs, never file paths.
Mutations recheck containment and freshness and finish saving before accepted.
Foundation primitives belong in `../jetpacs-poc/jetpacs-components`; Orgzly colors, layout,
queries, and notebook policy belong here. Do not require Material3 components.
Do not execute Babel or alter a user's real notebooks during tests.
