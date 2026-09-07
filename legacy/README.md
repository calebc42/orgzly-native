# Historical implementation evidence

These files were recovered from Orgzly Native commit `9a28575`. They target the
retired vendored Jetpacs core and must not be loaded into the current runtime.
The live app is `../emacs/apps/orgzly/`; current verification is in `../test/`.
Keeping the old forms and tests here preserves implementation details for
subsequent feature ports without silently shipping obsolete action handlers.
