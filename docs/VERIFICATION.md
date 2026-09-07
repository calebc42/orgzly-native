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
