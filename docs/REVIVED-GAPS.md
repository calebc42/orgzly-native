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

- **Submenus.** Revived's Clock, Metadata and Share open cascading popup
  submenus; Jetpacs menus have no submenus, so Clock and Share open a
  choice dialog and Metadata still folds the metadata section.
- **Import Org file.** Revived opens Android's document picker; EBP has no
  document-picker builtin, so Orgzly Native lists the Org files of
  `orgzly-import-directory`.
- **Searches menu.** Revived's Import and Export of saved searches are
  missing.
- **Insert timestamp.** Revived inserts the stamp at the cursor of the field
  being edited; Orgzly Native appends it to the end of the content while
  content is edited, else the title.  A `text_input` caret
  (SPEC 14.6.1 `report_caret`, today dropdown-only) would close it.
- **Tags dropdown filtering.** Revived's dropdown lists only tags starting
  with the word being typed; Orgzly Native always lists every known tag
  (a pick still completes the typed word).
- **Period type hint.** Revived's line under the period wheels follows the
  type wheel; Orgzly Native's shows the hint of the type the dialog opened
  with.

## Layout still off

- **Timestamp dialog, tablet.** On the tablet Revived leaves 16dp more above
  Clear, Cancel and Set than Orgzly Native.
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
  pane.
- **Status and gesture bars** differ between captures and are excluded from
  the comparisons.
