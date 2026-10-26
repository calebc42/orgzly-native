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

Still open:

- **Keep screen on.** Revived's note menu has a checkable Keep screen on;
  Orgzly Native has no such item (it needs a platform operation).
- **Checkable menu rows.** Revived draws a radio button or check box at
  the end of a checkable row; Jetpacs swaps the leading icon for a check.
- **Suggestions and Back.** Back while the tag suggestions are open
  leaves the note; Revived first closes the list.
- **Import Org file.** Revived opens Android's document picker; EBP has no
  document-picker builtin, so Orgzly Native lists the Org files of
  `orgzly-import-directory`.
- **Searches menu.** Revived's Import and Export of saved searches are
  missing.
- **Insert timestamp.** Revived inserts the stamp at the cursor of the field
  being edited; Orgzly Native appends it to the end of the content while
  content is edited, else the title.  Planned: the content field becomes
  an `editor` node (SPEC 19), whose session carries the cursor.

## Layout still off

- **Phone re-check.** The timestamp dialog, the new-note insets (placeholder
  padding fix) and the tags dropdown were measured on the tablet only; the
  phone was in use and has not been re-checked since.
- **Text metrics on the tablet.** The TextView metric table was measured at
  the phone's 2.8125 px/dp; on the tablet text rows land within 2px.
- **Glyph details.** The toolbar search and cycle icons differ by a pixel;
  glyph advances round differently from TextView (±1px).

## Colors

- **Wallpaper colors.** Revived follows Material You dynamic colors; Orgzly
  Native's palettes are fixed: dark measured on the phone, light measured on
  the tablet. A device with a different wallpaper shows different Revived
  colors.
- **Light palette on the phone** and **dark palette on the tablet** were
  never measured.

## Measurement artefacts (not styling differences)

- **Shadows in split screen.** Android lights shadows from the screen's top
  centre, so a popup's or card's shadow falls the opposite way in the other
  pane; compare both apps in the same pane.
- **Status and gesture bars** differ between captures and are excluded from
  the comparisons.
