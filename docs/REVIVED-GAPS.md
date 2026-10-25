# Remaining gaps to Orgzly Revived 1.23

Status at the pause on 2026-10-25. What was matched and measured is in
[VERIFICATION.md](VERIFICATION.md); this lists what still differs from the
installed Revived, roughly in the order it would be worth doing.

Reference devices: the S21 Ultra (dark mode, 2.8125 px/dp) for every screen
first matched, and the Pixel Tablet (light mode, 635x800 dp split-screen
panes) for the timestamp dialog, the tags dropdown and the light palette.

## Behaviour

- **"Note modified" on Back.** Revived asks "Save changes?" (Cancel,
  Discard, Save) when leaving an edited note with Back; Orgzly Native leaves
  without asking and keeps the draft.
- **Book and note overflow menus.** The notebook and note screens' overflow
  menus were not compared item by item with Revived's.
- **Insert timestamp.** Revived inserts the stamp at the cursor of the field
  being edited; Orgzly Native appends it to the end of the content while
  content is edited, else the title.
- **Tags dropdown filtering.** Revived's dropdown lists only tags starting
  with the word being typed; Orgzly Native always lists every known tag
  (a pick still completes the typed word).
- **Repeater and delay.** Revived opens its own repeater and delay pickers;
  Orgzly Native asks in a text dialog (`.+1w`, `-1d`).
- **Date and time pickers.** The timestamp dialog's Day and Time buttons
  open Compose Material pickers, not Android's DatePickerDialog and
  TimePickerDialog that Revived opens.

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
