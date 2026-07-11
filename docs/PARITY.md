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
| Notebooks | list, create, rename, delete, preface, `#+TITLE` cards, file tags, default notebook | ✅ `orgzly-books`/`orgzly-data`: books = `.org` files in `orgzly-directory`; Orgzly's book cards (`#+TITLE` headline with filename subtitle, icon-led mtime/count lines); create/rename/delete via menus (bridged prompts); preface viewer/editor; File tags dialog writing `#+FILETAGS:` (inherited by every note, `t.` searches see them); default book (★) via menu |
| Note list | outline per book, fold, state/priority/tags shown, planning under title, content shown inline, content line count | ✅ `book` drill-in follows `item_head`: flat foldable outline (client-side fold state, chevron ▸/▾ per note), `STATE  #A  Title  tags` titles with Orgzly's red-open/green-done keywords, done/archived rows faded (45%-alpha emulation), icon-led planning lines (scheduled/deadline/event/closed, Orgzly time format), inline org-highlighted content preview that folds with the subtree, line count when content hidden |
| Note popup | swipe a note → quick-action buttons (schedule, deadline, state, done, new, refile, clock, focus, delete…) | ✅ swipe or long-press any note row — on the book outline, search results, and the agenda — → quick-action dialog: Schedule/Deadline date pickers, State, Done, New above/under/below, Refile, Archive, Focus (jump to the note in its book, scrolled into view), Clock in/out/cancel, Cut/Copy/Paste below, Delete |
| Note editor | breadcrumbs, notebook row, inline title, icon metadata rows with ×-clear, timestamp dialog, tags, properties, content | ✅ `note` view mirrors `fragment_note`: tappable breadcrumbs (book › ancestors), notebook row (tap = refile), inline title field, icon-led metadata rows (tags / state / priority / scheduled / deadline / closed) each with × to clear, Orgzly-style timestamp dialog (date + time + repeater + clear), tag-picker dialog with add, property add/delete, org-highlighted content editor with the org keyboard toolbar; the timestamp dialog carries date (with Today/Tomorrow/Next-week shortcuts), time (×-drop keeps the date), end time, repeater, and the warning/delay cookie, every cookie round-tripping through edits; the CLOSED row edits and clears like upstream's; clock in/out/cancel in the overflow menu. ⚠ no save/discard-modified flow (mutations commit instantly) |
| Structure ops | promote/demote, move up/down, cut/copy/paste, delete, refile to book, archive | ✅ all, from the editor's overflow menu and the quick-action popup; delete confirms; refile via bridged book picker; paste below note or into book |
| Multi-select | batch state/schedule/deadline/refile/archive/delete + the "move" CAB (promote/demote/up/down) + cut/copy, count in the top bar | ✅ select mode toggle on the book, search, and agenda views: per-row checkboxes, "N selected" top-bar title (book), action bar with State / Done / Schedule / Deadline / Refile / Archive / Delete plus Promote / Demote / Up / Down / Cut / Copy / Clock in/out/cancel (order-aware so positions stay valid; batch cut/copy is one paste-able block) |
| Search language | `b. i. it. p. ps. t. tn. s. d. e. c. cr.` with `eq/ne/lt/le/gt/ge`, intervals (`none now today tomorrow yesterday ±Nh/d/w/m/y`), `.`‑negation, quoting, parens, and/or with AND-precedence, `o.*` sort, `ad.N` | ✅ `orgzly-query.el` is a port of QueryTokenizer/QueryParser/DottedQueryParser incl. the implicit-AND-inside-OR regrouping; matcher mirrors SqliteQueryBuilder (LIKE-style substring tags/text, default priority for `p.`, state classes from org config, per-event matching with range ends, per-unit boundary truncation, upstream's timed-before-all-day and priority-tiebreak sort order) — 27 dedicated tests |
| Saved searches | seeded defaults, CRUD, ordering | ✅ same four defaults (`Agenda`, `Next 3 days`, `Scheduled`, `To Do`); Searches drill-in with add/rename/edit query/reorder/delete; persisted via Customize |
| Agenda | `ad.N` day buckets, repeater expansion, leading Overdue section, group-scheduled-with-today, hide empty days | ✅ agenda tab with saved-search chips; occurrences expanded (incl. `+`/`++`/`.+` repeaters, calendar-aware month/year steps); Overdue section first with "N days overdue" in red, `orgzly-agenda-group-scheduled-with-today` moves overdue scheduled under Today; rows are the standard note rows showing only the responsible time, with the quick-action popup and multi-select (as on search results); deadline warning cookies surface the note at max(today, due − warning); `orgzly-agenda-hide-empty-days` |
| Reminders | scheduled/deadline/event classes with per-class toggles, daily reminder time for date-only notes | ✅ `reminders.set` replace-set (reboot-persisted by the companion), per-class defcustoms, date-only notes remind at `orgzly-reminders-daily-time`, done notes excluded, repeaters expanded. ⚠ **No Done/Snooze buttons on the notification** — the jetpacs reminder frame carries title/body/time only and adding buttons would need Kotlin; tap opens the app. All three classes default on; each note's next future occurrence is armed unbounded (upstream's model), and deadline/event reminders lead by the warning cookie |
| Home-screen widget | saved-search list widget, checkmark cycles state, day headers | ✅ `widget:agenda` surface: one view per saved search (header-switchable, offline from cache), day dividers for `ad.N` views, todo-cycle button per row. Widget colors/opacity/font-size knobs are companion-side rendering — N/A by design. ⚠ one switchable surface, not per-widget-instance searches; no "+" new-note / open-search-in-app buttons |
| Share / quick capture | share text into a notebook, quick note | ✅ `share.text` (queued offline) → note in the default book, subject → title, body → content; drawer "New note" quick capture |
| Settings | preference screens, states config, new-note defaults, display prefs, agenda + reminder prefs | ✅ schema-driven settings screen (registry = wire allowlist): Notebooks, New note (initial state, prepend, created-property, auto-`:ID:`, schedule-today, shared-text placement), Display (content in lists, line count, planning times, book name placement hide/before/under), Agenda, Reminders, Org file format (archive location, log done, LAST_REPEAT logging, priorities) |
| Sync | Dropbox / Git+SSH / WebDAV / SAF repos, auto-sync, conflict detection | ✅ **by architecture, not replication**: org files live in on-device Emacs, so sync is whatever Emacs/the OS does (git, syncthing, Termux ssh). Orgzly's repo/conflict model is deliberately not rebuilt — with a single source of truth there is nothing to two-way sync |
| External API / Tasker | broadcast intents to edit/search | ✅ exceeded: the whole app is a live Emacs — any elisp, plus the jetpacs device-trigger/capability catalog |
| Calendar provider sync | mirror query hits into an Android calendar | ❌ needs a Kotlin device capability the companion doesn't ship; excluded by the no-Kotlin constraint |
| Getting-started notebook | seeded demo book, Settings > App re-import | ✅ `orgzly-data-import-getting-started`: Orgzly's tutorial book, reworded where this app deliberately diverges; offered by the empty books state and a top-bar button (the schema settings screen has no action rows), with a confirm before resetting an existing copy. Not auto-seeded on first run — writing unasked into the user's org directory would be rude; the empty state offers it instead |

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
5. **Fold affordance sits on the left.** The companion's collapsible
   draws its chevron before the row (org-outline style) rather than
   Orgzly's right-edge `+`/`–` button, and leaf notes show a static ▸
   where Orgzly shows a bullet.  Tap the chevron to fold; tapping the
   title always opens the note.
6. **Multi-select is a toolbar toggle, not long-press** — long-press
   (like swipe) opens the quick-action popup instead; batch selection
   lives behind the checklist icon.
7. **New note prompts for the title first** (a native dialog), then
   opens the editor — Orgzly opens an empty editor directly.
8. **Client-settings bucket** (theme, fonts, list density, widget
   colors, swipe-button sets): rendering-side knobs of the fixed
   companion; the Display section covers the data-side subset (content
   in lists, line count, planning times, book name in results).
9. **Query case forgiveness.** `p.A` matches priority A (upstream's
   one-sided `LOWER()` makes any uppercase priority query match
   nothing) and `s.5D` parses as an interval (upstream falls back to
   free-text search) — both look like upstream quirks, not intent.

## Remaining gaps (adversarial re-check 2026-07-11; fix pass same day)

Four deep comparison passes over the reference checkout verified the ✅
rows at sub-feature level.  The fix pass closed the whole
data-loss/workflow tier: planning cookies (end time, warning/delay)
now parse, render, edit, and round-trip; warning periods drive agenda
placement and reminder lead; search/agenda hits carry the quick popup
and multi-select; reminders default all three classes on and arm each
note's next occurrence unbounded; the selection bar gained the "move"
CAB ops and batch cut/copy; the popup gained New above/below; and the
query matcher/sorter edges were fixed (per-event matching with range
ends, timed-before-all-day order, event/priority sort tiebreaks) or
blessed (divergence 9).  What remains:

### Missing features (verified against upstream menus/handlers)

A second fix round closed clocking (per-note CLOCK: lines in LOGBOOK,
Orgzly's semantics, on all three surfaces), focus-in-book (popup →
book scrolled to the note via the collapsible scroll target), CLOSED
editing, timestamp-dialog date shortcuts, `#+TITLE` book cards, the
`#+FILETAGS:` dialog, the new-note prefs (`:ID:`, schedule-today,
shared-text placement), notebook-name placement (hide/before/under),
the whole-row fade (desaturated keyword hues + muted content span —
span colors are hex-only, so a faded preview trades away its
highlighting), and the content-preview cap (default off; Orgzly shows
it whole).  Time-shift logging turned out already-covered: every state
change runs `org-todo`, so org itself writes `LAST_REPEAT` per
`org-log-repeat` (now a settings row).  Still open:

- **Narrow to subtree / widen**; **cycle visibility** (whole-book fold
  cycle — fold state lives on-device, so this needs an id-generation
  scheme to reset it).
- **Editor** — save/discard flow with modified detection (mutations
  commit instantly; breadcrumbs navigate without a guard); inline
  property editing + name autocomplete; sort content lines; insert
  inline timestamp (needs the client-side cursor); rendered view-mode
  (checkbox toggle / link taps in content).
- **Saved-searches import/export**; **share note as text** (needs an
  outbound-share device capability).
- **Book export** — ➖ N/A by architecture: a notebook already is a
  plain org file the user owns on disk.
- **Agenda** — multi-day `<a>--<b>` ranges expand per day upstream;
  port shows the start day only.  Smaller edges: done notes' overdue
  occurrences are suppressed (upstream leaves that to the query); a
  repeating overdue-scheduled note can show twice under Today with
  group-with-today.
- **Widget** — per-widget-instance saved searches; "+" new-note and
  header→open-search-in-app buttons.

Android-side (excluded by the no-Kotlin constraint, recorded for
completeness): org-protocol/`orgzly://` deep links, launcher shortcuts
and direct-share targets, the ongoing quick-note notification, reminder
snooze/alarm-sound classes, inline image rendering, settings
export/import, the app-logs screen, keep-screen-on.  Upstream has **no
org-attach/attachments feature** — only inline image links — so none is
owed here either.

## Test coverage

`test/run-tests.sh` — 78 ERT tests: the query language ported semantics
incl. per-event matching and the sort tiebreaks (27), the file-backed
data layer incl. mutations, cookie parsing and round-trip, CLOSED
editing, clocking, file tags, sibling creation, batch cut/copy,
repeater-aware toggling, preface front matter and the Getting Started
import (26), agenda expansion incl. the Overdue section, deadline
warning periods and reminder arming/lead (10), and a jetpacs-lint pass
that builds **every registered view in every drill-in state** plus the
quick-action/timestamp/tags/filetags dialogs and validates the produced
widget specs against the wire format, along with title-generator,
preface-abstraction, book-card-title, query-hit quick-action and drawer
semantics (15).
