# orgzly-native — Orgzly Revived, entirely in elisp

Orgzly Revived rebuilt as a [Jetpacs](jetpacs/README.md) Tier 1:
**zero Kotlin**. The Android side is the stock app-agnostic jetpacs
companion (vendored under `jetpacs/`); every screen, the dotted query
language, the agenda, reminders, the home-screen widget, and every
mutation are elisp — and your org files on disk are the database.

Feature coverage against
[orgzly-android-revived](https://github.com/orgzly-revived/orgzly-android-revived)
is tracked line-by-line in [docs/PARITY.md](docs/PARITY.md).

## What you get

- **Books** — notebooks are `.org` files in `orgzly-directory`:
  create/rename/delete, preface editing, a default notebook for capture.
- **Notes** — foldable outline per book, a full note editor (state and
  priority chips, schedule/deadline with time + repeater pickers, tags,
  properties, org-highlighted content with the org keyboard toolbar),
  every structure op, and Orgzly's multi-select batch toolbar.
- **Search** — Orgzly's dotted query language, ported faithfully
  (`b.gtd i.todo t.work s.ge.today .it.done o.p ad.7`, quoting, parens,
  and/or), plus the four seeded saved searches with full management.
- **Agenda** — day-bucketed saved searches with repeater expansion and
  overdue-into-today grouping.
- **Reminders** — exact alarms for scheduled/deadline/event times with
  per-class toggles and a daily time for date-only notes; persisted
  across reboots by the companion.
- **Widget & capture** — a saved-search home-screen widget with
  todo-cycle buttons; share-sheet text lands in the default notebook.
- **Settings** — Orgzly's preference screens as a schema-driven,
  Customize-persisted settings view.

## Layout

- `jetpacs/` — the vendored Jetpacs foundation (protocol, elisp core,
  Android companion). Untouched by this app.
- `emacs/apps/orgzly/` — the app: `orgzly-query.el` (query language),
  `orgzly-data.el` (file-backed data layer), `orgzly-ui.el`
  (books/notes/editor views), `orgzly-agenda.el`, `orgzly-search.el`,
  `orgzly-reminders.el`, `orgzly-widget.el`, `orgzly-capture.el`,
  `orgzly-settings.el`, and the `orgzly.el` entry point.
- `test/` — 57 ERT tests, including a jetpacs-lint pass over every view.
- `docs/PARITY.md` — the feature matrix and deliberate divergences.

## Getting started

1. Build and install the companion APK from `jetpacs/` and pair it
   (see [jetpacs/README.md](jetpacs/README.md) — the companion listens,
   Emacs dials in).
2. Load the app in the Emacs the companion talks to:

   ```elisp
   (add-to-list 'load-path "~/src/orgzly-native/jetpacs/emacs/core")
   (add-to-list 'load-path "~/src/orgzly-native/emacs/apps/orgzly")
   (require 'orgzly)
   (setq orgzly-directory "~/org")   ; where your notebooks live
   M-x jetpacs-connect
   ```

Development is live: `eval-buffer` against a connected phone updates
the app in place.

## Tests

```sh
./test/run-tests.sh          # emacs --batch + ERT, no device needed
```

## License

GPL-3.0-or-later, matching the Jetpacs foundation it builds on.
