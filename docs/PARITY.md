# Feature parity with Orgzly Revived

**Reference:** orgzly-android-revived (local checkout under
`~/pkb/resources/emacs/orgzly-android-revived`), surveyed from its query
package, reminders package, preference XMLs, and DB seed.

**Constraint honoured throughout: zero Kotlin.** The companion is the
stock jetpacs renderer; every feature below is elisp in
`emacs/apps/orgzly/`, and plain org files are the database.

## The matrix

| Orgzly area | Orgzly capability | orgzly-native status |
|---|---|---|
| Notebooks | list, create, rename, delete, preface, default notebook | ✅ `orgzly-books`/`orgzly-data`: books = `.org` files in `orgzly-directory`; list shows counts + mtime; create/rename/delete via menus (bridged prompts); preface viewer/editor; default book (★) via menu |
| Note list | outline per book, fold, state/priority/tags shown, content line count | ✅ `book` drill-in: level-indented foldable tree (client-side fold state), colored states, `[#A]` cookies, tag suffix, planning + line-count meta row |
| Note editor | title, state, priority, schedule/deadline with time + repeater, tags, properties, content | ✅ `note` view: rename (bridged prompt), state/priority chip rows from org config, per-planning-line date picker + time picker + repeater + clear, multi-select tag list with add, property add/delete, org-highlighted content editor with the org keyboard toolbar |
| Structure ops | promote/demote, move up/down, cut/copy/paste, delete, refile to book, archive | ✅ all, from the per-note menu; delete confirms; refile via bridged book picker; paste below note or into book |
| Multi-select | batch state/schedule/deadline/refile/archive/delete | ✅ select mode toggle on the book view: per-row checkboxes, count bar with State / Done / Schedule / Deadline / Refile / Archive / Delete (delete bottom-up so positions stay valid) |
| Search language | `b. i. it. p. ps. t. tn. s. d. e. c. cr.` with `eq/ne/lt/le/gt/ge`, intervals (`none now today tomorrow yesterday ±Nh/d/w/m/y`), `.`‑negation, quoting, parens, and/or with AND-precedence, `o.*` sort, `ad.N` | ✅ `orgzly-query.el` is a port of QueryTokenizer/QueryParser/DottedQueryParser incl. the implicit-AND-inside-OR regrouping; matcher mirrors SqliteQueryBuilder (LIKE-style substring tags/text, default priority for `p.`, state classes from org config, event min/max, per-unit boundary truncation) — 26 dedicated tests |
| Saved searches | seeded defaults, CRUD, ordering | ✅ same four defaults (`Agenda`, `Next 3 days`, `Scheduled`, `To Do`); Searches drill-in with add/rename/edit query/reorder/delete; persisted via Customize |
| Agenda | `ad.N` day buckets, repeater expansion, overdue into today, hide empty days | ✅ agenda tab with saved-search chips; occurrences expanded (incl. `+`/`++`/`.+` repeaters, calendar-aware month/year steps); overdue items grouped under Today with "Nd ago"; `orgzly-agenda-hide-empty-days` |
| Reminders | scheduled/deadline/event classes with per-class toggles, daily reminder time for date-only notes | ✅ `reminders.set` replace-set (reboot-persisted by the companion), per-class defcustoms, date-only notes remind at `orgzly-reminders-daily-time`, done notes excluded, repeaters expanded. ⚠ **No Done/Snooze buttons on the notification** — the jetpacs reminder frame carries title/body/time only and adding buttons would need Kotlin; tap opens the app |
| Home-screen widget | saved-search list widget, checkmark cycles state, day headers | ✅ `widget:agenda` surface: one view per saved search (header-switchable, offline from cache), day dividers for `ad.N` views, todo-cycle button per row. Widget colors/opacity/font-size knobs are companion-side rendering — N/A by design |
| Share / quick capture | share text into a notebook, quick note | ✅ `share.text` (queued offline) → note in the default book, subject → title, body → content; drawer "New note" quick capture |
| Settings | preference screens, states config, new-note defaults, agenda + reminder prefs | ✅ schema-driven settings screen (registry = wire allowlist): Notebooks, New note (initial state, prepend, created-property), Agenda, Reminders, Org file format (archive location, log done, priorities) |
| Sync | Dropbox / Git+SSH / WebDAV / SAF repos, auto-sync, conflict detection | ✅ **by architecture, not replication**: org files live in on-device Emacs, so sync is whatever Emacs/the OS does (git, syncthing, Termux ssh). Orgzly's repo/conflict model is deliberately not rebuilt — with a single source of truth there is nothing to two-way sync |
| External API / Tasker | broadcast intents to edit/search | ✅ exceeded: the whole app is a live Emacs — any elisp, plus the jetpacs device-trigger/capability catalog |
| Calendar provider sync | mirror query hits into an Android calendar | ❌ needs a Kotlin device capability the companion doesn't ship; excluded by the no-Kotlin constraint |
| Getting-started notebook | seeded demo book | ➖ not seeded; the empty state points at creating a book |

## Documented divergences (all deliberate)

1. **`ne` relation.** Upstream compiles `s.ne.today` to an unsatisfiable
   SQL `AND`; here it means "outside the period" — the evident intent.
2. **Month/year interval truncation.** Upstream's
   `Calendar.set(DAY_OF_MONTH, 0)` quirk lands on the last day of the
   *previous* month; here `Nm`/`Ny` boundaries truncate to the first of
   the month / Jan 1.
3. **Titles keep org markup.** A headline's timestamp stays visible in
   the title (org is the storage format, not a projection of it).
4. **Default sort.** Book then position, with scheduled/deadline time
   prepended when the query uses `s.`/`d.` — matches upstream's
   practical ordering without replicating its SQL ORDER BY verbatim.
5. **Drawer lists views, not every book/search** — books and searches
   are one tap away on their tabs (chips for saved searches).
6. **Client-settings bucket** (theme, fonts, list density, widget
   colors, swipe actions): rendering-side knobs of the fixed companion;
   out of scope for an elisp Tier 1.

## Test coverage

`test/run-tests.sh` — 57 ERT tests: the query language ported semantics
(26), the file-backed data layer incl. mutations and repeater-aware
toggling (17), agenda expansion + reminder windows (7), and a
jetpacs-lint pass that builds **every registered view in every drill-in
state** and validates the produced widget specs against the wire format
(7).
