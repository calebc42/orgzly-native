# Verification of the Foundation restoration

Verified locally on 2026-09-05, using installed Emacs 31.1 and the connected
Pixel Tablet running Android 17. All notebook mutations used temporary fixtures.
The device renderer tests used the Companion test activity, not the user's
live Org session or notebooks.

| Gate | Result |
|---|---|
| Orgzly ERT | 80 passed: recovered data/query/agenda/reminder calculation plus current actions and builders |
| Checkdoc and warning-as-error compilation | All eight live source modules passed; bytecode written only to a removed temporary directory |
| Static applet inspect, manifest and validation | PASS, zero errors/warnings; 19 owner-scoped actions |
| Trusted render determinism | Two identical canonical outputs; fixed structural bounds and real offline variant/identity gates passed |
| Foundation Kotlin module | 77 unit tests passed, including canonical rich-span attributes, Unicode offsets and link dispatch |
| Foundation Elisp / projections | 12 tests passed; projection reproducibility and byte compilation passed |
| Companion application | 66 unit tests passed; debug APK and instrumentation APK built |
| Optional-app startup boundary | Three ERT tests passed; requiring Jetpacs leaves Orgzly unloaded and inactive |
| Native screen checks | Four passed: notebook action, note/fold targets, dark outline, editable title publication and matching Save capture IDs |
| Onboarding payload | All eight packaged Orgzly source files matched their source bytes |

The determinism envelope is the fixed July 9, 2026 fixture clock, explicit
notebook snapshot, fold/query/page/appearance state, and offline compact/medium
builder context. It does not prove live negotiated frame/capability admission
or a real session's end-to-end durable delivery. The production sender remains
that authority. Device instrumentation verifies native rendering, editing and
action handoff; ERT verifies the filesystem effects separately.

## Repeat the checks

From `orgzly-native`:

```sh
./test/run-tests.sh
./test/verify-applet.sh /tmp/orgzly-verification
```

The verification script uses byte-identical copies of the current applet and
platform provider sources for static reconciliation, reads the real sibling EBP
contract, and then explicitly loads the trusted original applet against temporary
notebooks. It does not expand MCP source allowlists or execute legacy sources.

The script also exports six current-builder screen fixtures. To update device
fixtures intentionally, copy `/tmp/orgzly-verification/screens/*.json` into
`../jetpacs-poc/companion/app/src/androidTest/assets/orgzly/`, review the changes,
and run from `jetpacs-poc/companion`:

```sh
./gradlew :renderer:jetpacs:testDebugUnitTest :app:testDebugUnitTest :app:assembleDebug
./gradlew :app:connectedDebugAndroidTest \
  -Pandroid.testInstrumentationRunnerArguments.class=com.calebc42.jetpacs.companion.OrgzlyPresentedScreenTest
```

Fixture opaque IDs are bound to their temporary notebook locations and
revisions; they are not persistent demo data or a second UI representation.
No screenshot reference-update task was used. The images below were reviewed
from real device captures. Full POC, Glasspane, widget, Babel and live-session
end-to-end suites were not run for this restoration.

## Reviewed device captures

[Light outline](screenshots/outline-light.png) ·
[Dark outline](screenshots/outline-dark.png) ·
[Notebooks](screenshots/notebooks-light.png) ·
[Note editor](screenshots/note-light.png)

## Orgzly Revived look, 2026-10-25

The applet was restyled against the installed Orgzly Revived 1.23 on the S21
Ultra (dark mode, dynamic colors, 1080×2400 at 2.8125 px/dp), loading the same
`Getting Started with Orgzly` notebook in both apps. Screenshots were compared
pixel by pixel and by ink bands per text line:

| Screen | Result |
|---|---|
| Notebooks | Toolbar, hamburger, search, overflow, card, card title and FAB on the same pixels; card details within 1–2 px; the last-action text differs by content |
| Outline | Every text line at Revived's position; 0.17% of pixels differ: the toolbar title's glyph advances and bullet antialiasing |
| Outline, folded | Links, wrapping and the folded ring match; 0.17% of pixels differ |
| Drawer | Every label and icon on Revived's pixels; scrim color identical |

Gates: Orgzly ERT 86 passed with Checkdoc and warning-free compilation;
Companion `testDebugUnitTest`, `testAndroidHostTest`, `jvmTest` and
`:app:assembleDebug`; `ebp-kmp` `jvmTest`; `ebp-java` 52 tests; `ebp`
`validate.py`. On the device: fold, Fold/Unfold All, the back gesture from a
notebook to the notebook list (amendment #200) and the overflow menu. Link
opening was verified by ERT with `browse-url` stubbed.

[Notebooks](screenshots/revived-notebooks-dark.png) ·
[Outline](screenshots/revived-outline-dark.png) ·
[Drawer](screenshots/revived-drawer-dark.png)

The earlier captures above show the previous blue-grey look.

## Remaining screens and the Pixel Tablet, 2026-10-25

The note editor, pickers, search, agenda, Searches, settings, menus and the
new-notebook dialog were then matched on the phone (0.06–0.63% of pixels
differ; the settings rows within 1–2 px). The timestamp dialog and the light
palette were matched on the Pixel Tablet (light mode, 2.0 px/dp), with
Revived and the Companion side by side in split screen so both panes are
the same 635×800 dp window:

| Screen | Result |
|---|---|
| Notebooks, outline | Toolbar, hamburger, rows and FAB on Revived's pixels; 0.86% of outline pixels differ, mostly the toolbar title's glyphs |
| New note | Same rows; every line within 2 px (the TextView metrics are the phone's) |
| Timestamp dialog | Width, heading, labels, pills, checkboxes and buttons aligned; on the tablet Revived leaves 16dp more above the buttons |

On the tablet: Tomorrow re-shows the dialog with the new day, ticking Time
enables End time and adds it to the heading, the Day pill opens the date
picker seeded with the note's day, Set writes `<2026-10-26 Mon 11:55>` to the
draft, and the toolbar's insert-timestamp button appends today's stamp to
the edited title. Revived's light colors come from the tablet's wallpaper,
so the light palette is the tablet's and the dark palette the phone's.

The Tags dropdown was matched the same way: it spans the field, ends at
the "..." button, opens 7.5dp over the field's bottom with Revived's 16dp
popup shadow, and lists ARCHIVE, tag1, tag2, tag3 in Revived's order; every
edge and row within 2 px. Picking tag1 then tag2 gave `tag1 tag2 `, the
clear button appeared after the first pick and the dropdown narrowed to
end at the moved "..." button, as Revived's does.

Gates: Orgzly ERT 90 passed (new: the dialog's round trip, the inserted
timestamp's field reset, and the tags dropdown) with Checkdoc and
warning-free compilation; Companion `testDebugUnitTest`,
`testAndroidHostTest` and `jvmTest`; the full jetpacs-poc runner.

Remaining differences are listed in [REVIVED-GAPS.md](REVIVED-GAPS.md).
