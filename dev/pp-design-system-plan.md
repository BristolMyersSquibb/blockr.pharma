# Patient profile on the design system

Plan written 2026-09-26. The patient profile moves onto the blockr.ui design
system: `vignettes/articles/design-system.Rmd` in blockr.ui, newest copy on
`feat/shared-controls`. The target is drawn in three mockups, agreed with
Christoph on 2026-09-26:

- `_scratch/pp-design/header.html`: header row, count and reset, gear tray,
  download menu
- `_scratch/pp-design/panels.html`: panel heading, sentence and slots, Find,
  legend, canvas, info card, empty panel
- `_scratch/pp-design/sidebar.html`: search, the panel list, the list of
  patients

Chart tooltip content (which rows, badges, the high/low flag) is parked until
the rest of the block is done.

## What it builds on

The profile loads no blockr.ui code today. It takes blockr.dplyr's
`blockr_blocks_css_dep()` and `blockr_select_dep()` and calls none of the
shared components. The work assumes blockr.ui `feat/shared-controls` (PR 3)
as merged: `Blockr.Select` (with `menu`, `labelFirst`, `title`, multi),
`Blockr.checkbox`, `Blockr.segmented`, `Blockr.gearTray`, `Blockr.tooltip`,
`Blockr.icons.chevron`, all loaded by `blockr.ui::controls_dep()`. The
profile switches to `controls_dep()`; the dependency names are the ones
blockr.dplyr used, so a page never loads two copies.

Select stays as it is. Nothing in the profile needs group titles or a
free-text row in a Select menu.

Downloads use a new action menu in blockr.ui (`_scratch/pp-design/action-menu.html`):
an R function that builds the rows (a `downloadLink` or an `actionLink` is a
row) and a small JS part that opens, places (`Blockr.place`) and closes it.
Built on blockr.ui `feat/action-menu` (`action_menu()`, `menu_item()`,
`menu_section()`, `menu_divider()`, `tool_button()`), the follow-up PR after
PR 3. R markup gets the light tooltip through `data-blockr-tooltip`. blockr.viz
(table and chart downloads) and blockr.dock ("…" menu) move to it too.

## Stages

Work on a branch `feat/design-system` in a worktree
(`_worktrees/pharma-ds`), because other sessions commit to pharma main.
Each stage bumps the version, since the CSS and JS are served under it.

### 1. Tokens, type, shape (CSS only)

`inst/assets/css/patient-profile.css`, no markup or behaviour change.

- Legacy aliases to meaning tokens (`text-primary`, `text-secondary`,
  `text-subtle`, `primary`, `primary-bg`, `primary-hover`, `border`,
  `border-hover`).
- Palette reads (`--blockr-grey-*`, `--blockr-blue-*`) to meaning tokens.
- rgba and hex literals to tokens or `color-mix()` from a meaning token:
  about 40 sites. `#ffffff` backgrounds become `bg-surface` or `bg-raised`.
- Font sizes (9 to 14px in 13 steps) to `xs` / `sm` / `base` / `lg`, plus
  11px for counts and badges. One section-title rule for the uppercase
  labels (six letter-spacings today).
- Radii 5px and 7px to 4 / 6 / 8.
- The chart area goes white (`bg-surface`), not grey-50.
- Delete: the second and third `.pp-chart-header` blocks, the duplicate
  `.pp-add-on`, the repeated comment block, the empty section comments,
  `pp_icon_html()` (no callers), the unused check `<symbol>`, and the radio
  segment rules (no viz declares one).
- `test-pp-css-tokens.R` fails on a palette read, a legacy alias or a colour
  literal.
- Check light and dark.

### 2. Header row and gear tray (page 1)

`R/pp-ui.R`, `R/pp-header-ui.R`, `R/pp-cohort.R` (`pp_subject_facts_ui`),
`inst/js/pp-header.js`.

- The subject id becomes the output title (16px, 600). The facts become one
  13px muted sentence under the row: arm, "F, 80 years", "27 days on
  treatment", "6 adverse events, worst Severe" with the swatch. It wraps.
- Count and reset: one joined 26px segment. The count is an xs secondary
  button with the chevron, pointing left while the list is open and right
  while it is shut. The reset is an xs main button, disabled when there is
  no drill-down. The "shut" tint goes.
- Download: a 26px tool opening the action menu (see above). The menu has two
  section titles: "This patient" and "Cohort · N patients".
- Gear: `.blockr-gear-btn`, last in the row. It opens `Blockr.gearTray` in
  flow, which replaces `.pp-gear-popover`.
  - "Display" section: Timeline as `Blockr.segmented` (Date / Relative day,
    Relative day disabled with its reason as a tooltip); "Hide data before
    day −30" (checked by default, two grid columns: what
    `show_prestudy = FALSE` does, the axis starts 30 days before treatment
    start) and "Smooth lines" as `Blockr.checkbox`. The ctor argument keeps
    its name.
  - "Study variables": read-only, name then label as meta.
  - "Not available in this study": the panels this study cannot draw, with
    the missing table as meta.
- Every `title=` in this part goes to `Blockr.tooltip`.

### 3. Panels (page 2)

`R/pp-slot-ui.R`, `R/pp-controls-ui.R`, `R/pp-chart-area-ui.R`,
`R/pp-records.R`, `R/pp-lanes.R`, `R/viz-patient-info.R`,
`inst/js/pp-panels.js`, `inst/js/pp-find.js`.

- **Heading.** The decode at 14px/600, with the code in its tooltip.
- **Sentence.** One 13px muted sentence per panel. The pptx and HTML
  exports (`pp_static_*`) print the same text.
- **Slots.** Lanes and Value become slots in the sentence. A click opens
  `Select.menu` with `labelFirst` and a title naming the role ("Lanes",
  "Value"). The slot CSS comes from blockr.ui; if the slot rules have not
  moved there yet, that move comes first.
- **Retired controls.**
  - The cycle pill (`.pp-ctrl-pill`) and the LANES label go.
  - The ADAS switch becomes a checkbox.
  - The Items and Visits chips become a multi-value slot.
- **Find** becomes a word in the sentence: "6 events by *preferred term*,
  *all shown*". A click opens a plain multi `Select.menu` over the values of
  the level the lanes show; after picks the word lists them ("*Syncope,
  Agitation*", three at most, then "+N more"). No cross-level search, no
  free-text row, no Find tool. The `pp-find.js` popover and its Done button
  go.
- **Download and remove.** 26px tools shown on hover. Remove turns
  `text-danger` on hover. No grip, as in the sidebar: the heading drags,
  with the grab cursor.
- **Band ring.** It goes from the panel header.
- **Card chrome.** A rule between panels. The border that sits on a
  `display: contents` wrapper today moves to an element that draws.
- **Legend.** An HTML band with 25 x 14px swatches, 11px text, and items as
  buttons.
- **Empty panel.** An HTML line, centred, italic, muted, in place of the
  ECharts title from `pp_empty_chart()`.
- **Patient info.** The arm becomes a badge in its data colour, and the
  columns are at least 230px wide.

### 4. The list of patients and panels (page 3)

`R/pp-ui.R`, `R/pp-band.R`, `R/pp-cohort-ui.R`, `R/pp-cohort.R`,
`inst/js/pp-picker.js`, `inst/js/pp-cohort.js`.

- **Search.** A 30px field, 13px, with the field focus ring.
- **On the profile.**
  - Rows are 32px menu rows at 14px, with no colour dots.
  - A parameter shows its code, then its decode as meta; other panels show
    their name in sentence case.
  - No grip: the whole row drags, and Alt+Up/Down moves the keyboard row.
  - The × shows on hover and turns red.
- **Catalogue results.** Section titles per domain, the accent check on
  panels already on the profile, the match in 600.
- **Patients.**
  - A section title with the count, and one sentence: "Adverse events, by
    <slot>". The sort slot opens `Select.menu`; the ▾ cycle button goes.
  - The strip's filter is not shown here: `.pp-cohort-bandcap-find` goes.
- **Patient rows.**
  - Hover is `bg-hover`. The picked patient is `bg-selected` with
    `text-accent`, with no border.
  - The keyboard row takes the hover look.
  - The arm is a capsule badge in its data colour.
  - The strip's track and diamond read tokens.

### 5. Canvas ink (can go with stage 1)

`R/patient-profile-vizs.R` and the `viz-*.R` draw functions.

- Axis and lane labels, grid lines and axis lines read the tokens at render,
  as blockr.viz 23608ba does (`INK` + `readInk()`). Labels are `text-muted`
  at 11px; grid lines are solid `border-default`; the font is the body face.
  Exports keep the light values.
- `system-ui` and the hex greys (`#666`, `#ccc`, `#d1d5db`, `#4b5563`) go.
- The questionnaire heatmap's 140px gutter is a separate question. It does
  not line up with the other panels today.
- Vitals with several readings on one day: the line runs through the day's
  derived mean, and every reading is a dot. This changes what is drawn, so
  it gets its own commit.

### 6. Parked

- Chart tooltip content per viz (`pp_tooltip()` and the heatmap's inline
  copy).
- Colour fallbacks (severity, response, arm, domain) moving to blockr.theme.

## Tests

- `tests/js` (happy-dom): the header, picker, cohort and find tests change
  with stages 2 to 4. Re-render the fixtures with
  `Rscript tests/js/fixtures/render.R`.
- The 12 shinytest2 pins select by class. Update them in the same commit as
  the markup, and run them after `R CMD INSTALL` (they load the installed
  package).
- `test-pp-css-tokens.R` tightened in stage 1.
- Every stage: a chromote screenshot at 1440px and at 760px next to the
  mockup, light and dark (`_scratch/pp-design/shot.R`).
