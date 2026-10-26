# Remaining gaps to Orgzly Revived 1.23

Status at the pause on 2026-10-25. What was matched and measured is in
[VERIFICATION.md](VERIFICATION.md); this lists what still differs from the
installed Revived, roughly in the order it would be worth doing.

Reference devices: the S21 Ultra (dark mode, 2.8125 px/dp) for every screen
first matched, and the Pixel Tablet (light mode, 635x800 dp split-screen
panes) for the timestamp dialog, the tags dropdown and the light palette.

## Behaviour

Closed on 2026-10-25: the "Note modified / Save changes?" prompt on Back,
Revived's overflow menus item for item (Notebooks, notebook, note; Orgzly
Native's own items follow only while `orgzly-menu-extras` is on), notebook
Tags (`#+FILETAGS`), Clock in/out/cancel and Sort note on the content,
Share, the platform DatePickerDialog and TimePickerDialog (the picker
slot's `platform_dialog`), and the Repeat, Delay and Warning period
dialogs on `jetpacs.wheel`, Android's NumberPicker.

Closed later on 2026-10-25:
- The Clock, Metadata and Share submenus (EBP #203 `MenuItem.items`,
  feature `menu.submenu`), cascading beside the menu on a display at
  least 720dp across and replacing it on a phone, as AppCompat does.
  Metadata sets Revived's visibility: Show all, Show selected
  (`orgzly-selected-metadata`, default tags, state and scheduled time),
  Always show set.
- Tag suggestions while typing (EBP #202 `text_input.suggestions`): the
  Companion completes the word at its own cursor; the caret never
  reaches Emacs.
- Back after a tap outside the tags dropdown now leaves the note; the
  menu no longer pulls focus back to its button after a touch.
- The period dialogs' hint follows the type wheel as it turns
  (`jetpacs.state_switch`), with no round trip.

Closed later on 2026-10-26: **the note popup.** A fling on a note row
opens Revived's popup — a floating row of tonal icon buttons beside the
finger — through EBP #207 popup swipe sides (`presentation: popup` on a
`jetpacs.pressable`, which now carries swipe sides). Right: Schedule,
Deadline, State, Done (the Revived defaults); left in a notebook: Delete,
New above, New under, New below (Revived's fifth default, Refile, has no
Orgzly Native counterpart yet). Schedule and Deadline open the timestamp
dialog and write the planning line at once; State offers the keywords;
New above/below place the note against the row's sibling. Search and
agenda rows carry only the right side: Revived's single left button
(Focus) opens the note's notebook, and with one button Revived performs it
without a popup, which EBP's popup does not model. Open: the popup's
chrome is drawn from Material's 40dp tonal icon button, not measured
against Revived's; the four-action limit per side; the row's `on_long_tap`
(Revived's multi-select) remains missing.

Closed later on 2026-10-26: **the content is an `editor` node** (SPEC 19,
`editor.sync`): the Companion opens a session on the form's own document
(`orgzly:content:<scope>`), every delta feeds the draft through the
mirror, Done reads the draft, and Insert timestamp splices the stamp at
the device caret with the caret after it, as Revived's
`insertStringAtCursorPosition` does.  Clock in/out/cancel and Sort replace
the editor's text in one splice.  EBP #206 `editor.hint` carries Revived's
"Content" placeholder; the Foundation editor wears the content field's
TextView metrics through the `editor.text`, `editor.chromeless` and
`editor.placeholder` slots.  A Companion without `editor.sync` keeps the
captured text field.  Org editing features in the content (completion,
structure commands) can now follow on the same session.

Closed on 2026-10-26: the overflow popup's position on the tablet (8dp
more inset on sw600, as Revived's toolbar), checkable rows' radio buttons
and check box at the row's end in the accent color with no highlight (EBP
#204 `unchecked_icon`, design `check_at_end`/`check_color`), 24dp menu
icons 8dp in with the label 8dp after (`icon_size`, `icon_start`,
`icon_gap`), and the tags Clear button following the field's text live.

Still open:

- **Keep screen on.** Revived's note menu has a checkable Keep screen on;
  Orgzly Native has no such item (it needs a platform operation).
- **Checkable rows close Revived's menu?** SPEC 17.4 keeps a checkable
  row's menu open; Revived's behaviour on a tap was not compared.
- **Suggestions and Back.** Back while the tag suggestions are open
  leaves the note; Revived first closes the list.
- **Import Org file.** Revived opens Android's document picker; EBP has no
  document-picker builtin, so Orgzly Native lists the Org files of
  `orgzly-import-directory`.
- **Searches menu.** Revived's Import and Export of saved searches are
  missing.
- **Insert timestamp into the title.** Revived inserts the stamp at the
  title's cursor; Orgzly Native appends it to the captured title.  The
  content is done (below); the title stays a one-line text field.

## Layout still off

- **Phone re-check.** The timestamp dialog, the new-note insets (placeholder
  padding fix) and the tags dropdown were measured on the tablet only; the
  phone was in use and has not been re-checked since.
- **Text metrics on the tablet.** The TextView metric table was measured at
  the phone's 2.8125 px/dp; on the tablet text rows land within 2px.
- **Glyph details.** The toolbar search and cycle icons differ by a pixel;
  glyph advances round differently from TextView (±1px).

## Colors

Closed on 2026-10-26: **wallpaper colors.** The Companion reports the
device's Material You scheme (EBP #205, `colors.changed` and the welcome's
`colors`), and `orgzly-design.el` derives both palettes from it the way
Revived's theme does — each color a Material role, a role at Android's
alpha over the surface behind it, or a fixed resource (TODO, DONE, the
switch thumbs) — with the measured palettes as the fallback. The preset
follows the device's appearance until the theme is toggled. Checked on
the Pixel Tablet in both polarities (`cmd uimode night yes` flips the
device, the Companion reports, Orgzly Native re-authors): the Notebooks
screen's window, card, FAB, title and body colors are Revived's exact
pixels in light and dark. Open: the same check on the phone, where the
measured dark palette's `strong` (#FCFAFF) is brighter than `on_surface`
— Samsung's own `textColorPrimary`, which the derivation does not
reproduce (it takes `on_surface`, as the tablet draws).

- **Light palette on the phone** and **dark palette on the tablet** were
  never measured; with the device scheme both now derive from the same
  roles, which the device check above covers.

## Measurement artefacts (not styling differences)

- **Shadows in split screen.** Android lights shadows from the screen's top
  centre, so a popup's or card's shadow falls the opposite way in the other
  pane; compare both apps in the same pane.
- **Status and gesture bars** differ between captures and are excluded from
  the comparisons.
