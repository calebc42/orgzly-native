# Orgzly Native

Orgzly's notebook, outline, search, agenda and note-editing workflows as an
optional Elisp applet for the current Jetpacs POC. Org files remain the database.
Presentation uses Jetpacs Compose Foundation components and the declarative
Styles API, with light and dark palettes based on
[Orgzly Revived's Android resources](https://github.com/orgzly-revived/orgzly-android-revived).

The original Git history is preserved through `9a28575`. The old bundled core,
generated single-file distribution and deployment scripts have been retired.
Historical UI and tests are in `legacy/`, excluded from the live load path and APK.
See [restoration provenance](docs/RESTORATION.md) and [current coverage](docs/PARITY.md).

## Local setup

Keep these sibling checkouts under the same workspace:

```text
workspace/
  jetpacs-poc/
    jetpacs-components/
  ebp.el/
  orgzly-native/
```

With the current Jetpacs runtime and components already on `load-path`:

```elisp
(add-to-list 'load-path "~/workspace/orgzly-native/emacs/apps/orgzly")
(setq orgzly-directory "~/org") ; Direct .org files; no recursive scanning.
(require 'orgzly)
(orgzly)
```

The Companion must enable its experimental Foundation/design renderer support.
Orgzly declares `jetpacs.components` and `jetpacs.design` as required extensions.
The APK and tablet onboarding payload include it in the optional Apps inventory;
installing Jetpacs leaves it inactive. Enable Orgzly from Apps after enabling
Foundation support. No `jetpacs.material3` component package is required.
The stock Companion still supplies the generic scaffold, insets and dialogs.
Orgzly's plain toolbar, note rows, fields and controls render through Foundation.

Use Refresh after editing files externally. Unsaved Emacs buffers are preserved
and must be saved before refreshing or accepting a device edit. Edits use opaque
revision IDs and are refused when the notebook changes. Forms capture their
fields together and save once before returning `accepted`.

Search accepts dotted queries such as `i.todo`, `t.work`, `b.inbox`, `s.today`,
boolean expressions and sort orders. `ad.N` produces agenda sections, bounded to
31 days. Outline/search pages contain 40 notes; agenda shows the first 40
occurrences. The supported envelope is 64 local notebooks, 1 MiB each, with
2,000 total headings. Symlink notebook aliases and path traversal are refused.

Appearance starts with Orgzly Light or Dark. A Design Lab profile derived from
Orgzly can override it; unrelated global profiles do not remove required styles.
`M-x customize-group RET orgzly` configures the Org defaults. Unloading the
feature calls `orgzly-unregister` and removes its registrations and pending timers.

## Verification

```sh
./test/run-tests.sh
```

This runs the recovered model/query/agenda tests, current applet contract tests,
and warning-as-error compilation into a temporary directory. No source `.elc`
files are produced. All file mutation tests use disposable notebooks.

`test/verify-applet.sh` runs static inspect/manifest/validation and the trusted
render checker on those same disposable fixtures. Reports and generated screen
JSON go to a caller-selected temporary output directory. The Companion's
`OrgzlyPresentedScreenTest` renders these screens on a device, checks note and fold
actions, and captures light/dark images. See [verification](docs/VERIFICATION.md).

GPL-3.0-or-later; see [LICENSE](LICENSE).
