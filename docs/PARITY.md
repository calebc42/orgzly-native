# Current Orgzly coverage

This describes the current Foundation applet. The old version's larger parity
claims are preserved separately in [LEGACY-PARITY.md](LEGACY-PARITY.md).
They are historical evidence, not acceptance results for this port.

| Area | Current Foundation applet |
|---|---|
| Notebooks | List with title/file/count, create, preface and Org file keyword editing, rename, confirmed deletion |
| Outline | Flat dense rows, hierarchy indentation, independent 48dp fold target, state/priority/tags, planning and content previews, pagination |
| Note editor | Captured title/state/priority/tags/scheduled/deadline/content; one atomic save, repeater/warning/end-time round trips; new root and child notes |
| Note operations | Done/TODO, promote/demote, move up/down, copy, paste below, confirmed subtree deletion |
| Search | Recovered dotted parser, matcher and ordering; saved queries can be added/replaced/deleted and persisted |
| Agenda | Overdue and day groups, repeaters and warning periods, saved `ad.N` queries, fixed snapshot clock |
| Appearance | Orgzly blue-grey chrome, flat rows, red TODO and green DONE, light/dark Foundation Styles profiles |
| Reminders | Scheduled, deadline and event reminders on the phone through the Foundation's reminder projection: each note's next occurrence, the soonest `orgzly-reminders-max-count` (64), opaque ids, cleared when `orgzly-reminders-enabled` is off; a tap runs `orgzly.home` and opens the app; no Done/Snooze buttons |
| Lifecycle | Optional packaged app, required Foundation extensions, owner registrations, teardown, bounded snapshot refresh and stale edit refusal |

Historical extras are retained as reference rather than exposed through obsolete
handlers: multi-select batch UI, refile/archive dialogs, clock controls, property
editor, timestamp picker dialogs, in-place table/Babel/block editing,
home-screen widgets and Android share capture. The recovered data APIs remain
available, but the current UI does not claim those integrations. Reminder
settings are Customize variables; there is no settings screen yet. Rich content is currently edited as Org source
in Foundation fields; no Babel execution is triggered by the applet.

Synchronization remains the user's existing Org-file workflow. Orgzly Native
does not reproduce Orgzly Android's remote repository/conflict database.
