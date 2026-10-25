# Current Orgzly coverage

This describes the current Foundation applet. The old version's larger parity
claims are preserved separately in [LEGACY-PARITY.md](LEGACY-PARITY.md).
They are historical evidence, not acceptance results for this port.

| Area | Current Foundation applet |
|---|---|
| Notebooks | Cards with title, "Not modified" and the snapshot's load time, create from the FAB, preface and Org file keyword editing, rename, confirmed deletion |
| Outline | Notebook preface, item_head rows (31.6dp for one line), indent guides, independent 48dp fold button (– / +) for notes with children or content, Fold/Unfold All, state/priority/tags, planning lines, content with emphasis and opaque-id links, pagination; folding re-pushes the screen (rich titles cannot be bound by a template) |
| Note editor | Revived's note screen: breadcrumbs, title and content shown as text until tapped, collapsible metadata, state and priority pickers, Revived's timestamp dialog for scheduled and deadline times (day, Today/Tomorrow/Next Week, time and end time, repeater and delay behind checkboxes), the toolbar's insert-timestamp button while a field is edited; one atomic save, repeater/warning/end-time round trips; new root and child notes |
| Note operations | Done/TODO, promote/demote, move up/down, copy, paste below, confirmed subtree deletion |
| Search | Recovered dotted parser, matcher and ordering; saved queries can be added/replaced/deleted and persisted |
| Agenda | Overdue and day groups, repeaters and warning periods, saved `ad.N` queries, fixed snapshot clock |
| Appearance | Orgzly Revived 1.23 (Material 3): notebook cards, outline rows with bullets, folded rings, indent guides and fold buttons, TODO/DONE colors, 45% fade for done and archived notes, smaller tags, linked content, drawer with saved searches and notebooks, add FAB. Notebooks, outline and drawer match the installed app on the reference phone to within glyph antialiasing (0.17% of outline pixels differ) |
| Reminders | Scheduled, deadline and event reminders on the phone through the Foundation's reminder projection: each note's next occurrence, the soonest `orgzly-reminders-max-count` (64), opaque ids, cleared when `orgzly-reminders-enabled` is off; a tap runs `orgzly.home` and opens the app; no Done/Snooze buttons |
| Lifecycle | Optional packaged app, required Foundation extensions, owner registrations, teardown, bounded snapshot refresh and stale edit refusal |

Historical extras are retained as reference rather than exposed through obsolete
handlers: multi-select batch UI, refile/archive dialogs, clock controls, property
editor, in-place table/Babel/block editing,
home-screen widgets and Android share capture. The recovered data APIs remain
available, but the current UI does not claim those integrations. Reminder
settings are Customize variables; there is no settings screen yet. Rich content is currently edited as Org source
in Foundation fields; no Babel execution is triggered by the applet.

Synchronization remains the user's existing Org-file workflow. Orgzly Native
does not reproduce Orgzly Android's remote repository/conflict database.
