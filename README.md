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

- `orgzly.el` — the single-file bundle, generated (do not edit by hand)
- `jetpacs/` — the vendored Jetpacs foundation (protocol, elisp core,
  Android companion). Untouched by this app.
- `emacs/apps/orgzly/` — the app: `orgzly-query.el` (query language),
  `orgzly-data.el` (file-backed data layer), `orgzly-ui.el`
  (books/notes/editor views), `orgzly-agenda.el`, `orgzly-search.el`,
  `orgzly-reminders.el`, `orgzly-widget.el`, `orgzly-capture.el`,
  `orgzly-settings.el`, and the `orgzly.el` entry point.
- `emacs/build-bundle.el` — regenerates the root `orgzly.el` (app-only;
  the bundle opens with `(require 'jetpacs-core)`)
- `test/` — 57 ERT tests, including a jetpacs-lint pass over every view.
- `docs/PARITY.md` — the feature matrix and deliberate divergences.

## Getting started

First, build and install the companion APK from `jetpacs/` and pair it
(see [jetpacs/README.md](jetpacs/README.md) — the companion listens,
Emacs dials in). Then load the app in the Emacs the companion talks to,
by any of the three routes below. All of them need the Jetpacs core
first — the bundle `(require 's it, never copies it, so one installed
`jetpacs-core.el` serves every Jetpacs app.

**Single-file bundle.** Grab `orgzly.el` from this repo's root and
`jetpacs-core.el` from the jetpacs repo's root, put both somewhere on
`load-path`, and:

```elisp
(require 'jetpacs-core)
(require 'orgzly)
(setq orgzly-directory "~/org")   ; where your notebooks live
M-x jetpacs-connect
```

**On the phone (/sdcard adoption).** If your device init is the Jetpacs
starter init (`jetpacs/docs/starter-init.el`), just download `orgzly.el`
on the phone — the browser saves to `/sdcard/Download`, or copy it to
`/sdcard/Documents` — then add `"orgzly.el"` to the starter init's
bundle adopt list and `(require 'orgzly)` after the core. On startup the
newest staged copy is adopted into `~/.emacs.d/elisp/` automatically;
updating the app is just downloading the file again.

**From source.** Point `load-path` at the checkout:

```elisp
(add-to-list 'load-path "~/src/orgzly-native/jetpacs/emacs/core")
(add-to-list 'load-path "~/src/orgzly-native/emacs/apps/orgzly")
(require 'orgzly)
(setq orgzly-directory "~/org")
M-x jetpacs-connect
```

Development is live: `eval-buffer` against a connected phone updates
the app in place. After editing sources, regenerate the bundle:

```sh
emacs --batch -l emacs/build-bundle.el   # rewrites orgzly.el
```

## Tests

```sh
./test/run-tests.sh          # emacs --batch + ERT, no device needed
```

## License

GPL-3.0-or-later, matching the Jetpacs foundation it builds on.
