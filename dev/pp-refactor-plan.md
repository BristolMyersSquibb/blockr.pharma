# Patient profile: staged refactor plan

Status: Stage 0 done 2026-09-08 (`tests/testthat/test-shinytest2.R`, 12
tests, 84 expectations, about 50s, green three times standalone and in the
full suite). Stage 1 done the same day: `inst/js/patient-profile.js` (0.0.0.9051), the
R file down to 2,356 lines, body byte for byte apart from the 35 header
lines that now derive from the mount config. Stage 2 done the same day: `tests/js/` (harness.js + 9 test files, 68
tests, 2.6s, run from testthat by `test-js.R`), fixtures rendered by
`tests/js/fixtures/render.R`. Stage 3a done the same day: 358 lines of the old card sidebar cut from
the JS (1,980 to 1,622 lines) and 38 dead classes with 566 lines from the
CSS (2,441 to 1,875). Stage 3b done the same day: five files, `pp-core.js` (registry + mount)
and one part each for header, cohort, picker, panels, registered with
`PatientProfile.part()`; parts share nothing but the document. Stage 3c dropped after reading the code: only the patient pick is a
timed guard; the panel pick and the reorder share `renderAddOn()`'s
same-rows check and the find box uses recency, so there is nothing to unify.
Stage 3f (tokens) done the same day: 157 token references with fallbacks,
one pre-existing drifted fallback caught by the new
`test-pp-css-tokens.R`; the literals left are white, two blue tints with
no token, one green data colour. Stages 3d and 3e not started. Left in on purpose: the
`pick_param` / `sync_params` path (R still owns per-parameter items on a
multi-parameter panel; no markup reaches it today, so it is a design
question for the R side, not dead by accident). Written against `3d152dd` (version
0.0.0.9050).

## What we are dealing with

| File | Lines | Note |
|---|---|---|
| `R/patient-profile-block.R` | 4,301 | 1,955 of them are one JS string (lines 2291 to 4245) inside `paste0()` |
| `R/pp-cohort.R` | 1,777 | band geometry, marks, sort |
| `R/patient-profile-vizs.R` | 1,747 | echarts panels |
| `inst/assets/css/patient-profile.css` | 2,441 | 155 classes, 17 without markup |
| `tests/testthat/` | 5,689 lines of R tests | no JS tests, no browser tests |

The JS string takes 33 `ns()` interpolations (all Shiny ids), one glyph
(`pp_grip_glyph()`) and one constant (`pp_cohort_band_h_spans`). It uses jQuery
throughout. It registers 6 custom message handlers and sets 9 Shiny inputs. The
server module has 29 observers, 16 `reactiveVal`s, 15 reactives, 13 outputs.

Everything fixed in the last week lives in the JS half: pick debounce and echo
guard (`sendPick`, `pickPending`, `lastSelected` in 15 places), ghost waiting for
a canvas, FLIP move in the picker, well sizing from the first scrolling
ancestor, first-hit marking. None of it has a test.

## Principle

Not a rewrite. A staged refactor where every stage leaves behaviour identical
and proves it before the next stage starts. The one behaviour change allowed is
deleting code nothing reaches.

## Stage 0: pin today's behaviour in the browser (1 day)

Browser tests are the only thing that can prove Stage 1 changed nothing.

- Runner: shinytest2, as in `blockr.viz/tests/testthat/test-shinytest2.R`. One
  fixture app under `tests/testthat/apps/pp-e2e/app.R` built from
  `dev/patient-profile-cohort.R`, one shared `AppDriver`, skips when chromote
  is unavailable. Key presses and mouse go through
  `app$get_chromote_session()` (CDP), which is what the arrow-walk tests need.
  Runner-up: Playwright, because 45 scratch drivers in `_scratch/pp-*.mjs`
  already encode the measurements. They are the source for the assertions
  either way.
- shinytest2 loads the installed package, so the dev loop needs
  `R CMD INSTALL --no-docs` before each run.

What to pin, one test each, all taken from drivers that ran this week:

1. Click a patient row: header names the patient; `chart_area` receives one
   `shiny:value`, not two (`pp-flicker-probe`).
2. 20 ArrowDown presses: selection is the 21st shown row in DOM order, the
   server receives one `pick_subject`, the well has no focus ring
   (`pp-fastwalk`, `pp-arrow-verify`).
3. Fast walk then wait 300ms: the server's `sync_subject` equals the client
   selection, no jump-back.
4. Pick a panel in the sidebar: its row moves to the On list, the count
   updates, the search resets and patients are visible again; remove moves it
   back (`pp-move-verify`, `pp-reset`).
5. Enter in the search picks the first hit, panel before patient; Escape
   clears (`pp-enter`).
6. At a dock panel height of 400px the well is at most host minus gap and never
   264px in 235px of space (`pp-well-fit`).
7. At 420px panel width nothing in the picker is clipped (`pp-pop-fit`).
8. Switching patients dims the panel and undims only after a canvas exists
   (`pp-ghost-timing`).
9. Sort by peak: caption reads `by peak`, each row prints its value, order
   changes (`pp-sortby-verify`).
10. Series rows: median line inside the row, reference rect between hi and lo,
    no grey track (`pp-kinds-verify`, `pp-ref-check`).
11. Drag a panel above another: `reorder_viz` arrives with the new order.
12. Sidebar collapse and expand (`pp-collapse`).

What Stage 0 established, and the pins encode:

- On a plain board the well's host is the block's bslib card body, so the
  pin shrinks the host rather than the window. In a dock it is the panel.
- The board's sticky toolbar floats over the top of the sidebar; a drag
  scrolls the On list clear of it first.
- A lab panel is one On row per parameter, so three panels are five rows.
- Only a panel pick resets the search box; a patient pick keeps the query.
- The ghost has three early exits besides a painted canvas: no widget, a
  scroll, the 1500ms deadline. The pin records which one fired.
- A one-visit series row draws a dot, not a path; rows draw as they scroll
  into view.
- A key burst timed from R cannot stay under the 250ms debounce, so the
  burst is dispatched inside the page; single presses go through CDP.

## Stage 1: move the JS to a file, byte for byte (half a day)

- New `inst/js/patient-profile.js`, served by an `htmlDependency` exactly like
  `flag_filter_block_dep()` in `R/flag-filter-block.R`. Bump `Version` (the
  served URL embeds it, which is what busts the browser cache).
- The 33 ids collapse to one: every `ns("x")` is `<id>-x`, so the file derives
  them from the block id. The bootstrap that stays inline is one line,
  `PatientProfile.mount("<id>")`, kept inside the existing `$(function(){})`
  and `shiny:value` retry so a late dock panel still mounts.
- `pp_grip_glyph()` moves into the markup (a hidden `<template>` next to the
  existing check-mark `<svg>`), `pp_cohort_band_h_spans` becomes a
  `data-band-h` attribute on the well. Those two are the whole R to JS
  coupling besides ids.
- Proof of "byte for byte": have R `cat()` the string it emits today to a
  temp file, substitute the ids, and `diff` against the new file. The
  `—` and escaped backslashes inside the string are why this has to be
  scripted rather than pasted.
- Gate: Stage 0 suite green, R suite green, dev app by eye.

Payoff before any refactor: the double-quote-in-a-JS-comment trap that broke
`parse()` five times this week is gone, eslint can run, and Stage 2 becomes
possible.

## Stage 2: unit tests for the JS in happy-dom (2 to 3 days)

- `tests/js/harness.js` in the shape of `blockr.dplyr/tests/js/harness.js`:
  a happy-dom window, stubbed `Shiny.addCustomMessageHandler` and
  `Shiny.setInputValue`, fake timers from `node:test` for the 250ms debounce,
  stubs for `ResizeObserver` and `requestAnimationFrame`. Unlike dplyr, the
  file uses real jQuery, so the harness loads Shiny's bundled
  `www/shared/jquery.min.js` into the window rather than stubbing it.
- Fixture markup comes from R, not by hand: an R script renders the sidebar,
  picker, well and one panel via the same functions the module uses
  (`pp_add_picker_ui()`, the cohort rows, the band caption) to
  `tests/js/fixtures/*.html`. Regenerating them makes markup drift show up as a
  git diff, and anything the fixtures never contain is provably dead.
- `package.json` with happy-dom as the only devDependency,
  `tests/testthat/test-js.R` running `node --test` and skipping without node
  or `node_modules`, `^node_modules$` in `.Rbuildignore`. Same as dplyr.

Tests, ordered by how much each behaviour cost us:

1. `sendPick`: five picks inside 250ms send one input; a `sync_subject` with a
   stale id while a pick is pending is ignored; one after settle is applied;
   `now = true` bypasses the timer.
2. `cohortStep`: walks visible rows in DOM order, skips `is-filtered-out`,
   clamps at both ends, `absolute` for Home and End, moves `is-selected`
   without focusing the well.
3. `renderAddOn`, `filterAdd`, `markFirstHit`: on and off lists, the count,
   "Nothing matches" only when both lists are empty, first hit marked across
   panels then patients, Enter picks it, Escape clears, search resets after a
   pick, and `filterAdd` runs after the patient filter.
4. FLIP: `addFlipRects` then `addFlipPlay` sets a transform and clears it;
   reduced motion skips it; a `sync_selected` echo with the same rows does not
   replay.
5. `sizeWell`: host rect minus gap, floor 132, re-attaches on `shiny:value`,
   the window path and the observer path agree.
6. `unghostPanel`: fades at once with no widget, waits for a canvas otherwise,
   gives up at 1500ms.
7. `drawBand`, `drawSeries`, `drawSpans`: from a row's data attributes,
   the expected SVG nodes, reference rect from hi to lo, tick height from the
   row's own svg, no track on series rows.
8. Sort cycle, sidebar toggle, drag reorder emitting `reorder_viz` in the new
   order, `applyParamChecks`, `sync_params`.
9. Every message handler accepts the payload shape R sends: `subject_picker`
   count, `dl_menu_state`, `sync_band`, `sync_selected`, `sync_params`,
   `sync_subject`.

Gate: every named function in the file has a test except pure DOM glue.

## Stage 3: refactor, one seam per commit, all three suites green after each

a. Delete dead code. `buildResults()`, the `.pp-category-group` walks in
   `applyFilter()` and `applyParamChecks()`, the 17 orphaned CSS classes, about
   60 JS lines. The fixtures from Stage 2 are the proof.

b. Split the file by concern, plain IIFEs with a `Depends on:` header so the
   dplyr harness convention applies unchanged: `pp-core.js` (ids, messaging,
   mount), `pp-cohort.js` (well, step, pick, bands), `pp-picker.js` (add-on,
   filter, FLIP, first hit), `pp-panels.js` (ghost, drag reorder, chart
   resize, controls). No bundler, per design decision 0001.

c. One optimistic-update helper. Patient pick, panel pick, reorder and viz
   settings each carry a hand-rolled guard against their own server echo. A
   single `optimistic(inputId, apply, settleMs)` returning `{send, isEcho}`
   replaces the four. Tests 1, 3, 4 and 8 from Stage 2 stay as written; that
   is the point of writing them first.

d. Drop jQuery file by file, optional. Shiny still ships it, so there is no
   urgency; do it where a file is being rewritten anyway.

e. R side, after the JS leaves the file is about 2,300 lines. Split into
   `pp-ui.R`, `pp-server-cohort.R`, `pp-server-panels.R`,
   `pp-server-downloads.R`. Keep the 29 observers, group them by the reactive
   they serve. The one structural change: the six `sendCustomMessage`
   channels go through one `pp_send()` with a roxygen table documenting each
   channel's payload, mirrored by the handler table in `pp-core.js`.

f. CSS. Replace the 173 literal colours (22 distinct, `#9ca3af` 37 times)
   with `var(--blockr-color-*, #literal)` per the token contract, with a
   test that every fallback equals the token's value in
   `blockr-dock.css`. Then rename state classes to modifiers
   (`is-selected` to `pp-pt--selected`) per naming-conventions.md. That
   rename touches JS, CSS, R and test selectors at once, which is why it is
   last: with fixtures and tests in place it is mechanical.

## Out of scope

- Adopting blockr.ui's row, pill and add-row primitives instead of `pp-*`.
  That layer is mid-migration per the design-system README; do it when
  blockr.ui owns it.
- The echarts `htmlwidgets::JS()` formatter snippets in the viz files. They
  are separate, small, and covered by the `pp_js_str()` tests.
- Speed. Patient change is about 300ms end to end and 150ms of it is echarts
  creating canvases. The DOM is 7,232 nodes at 254 patients. Nothing in this
  plan is expected to move those numbers.

## Effort and sequencing

| Stage | Days | Gate |
|---|---|---|
| 0 browser pins | 1 | 12 shinytest2 tests green on the current code |
| 1 extract | 0.5 | diff of emitted JS empty, all suites green |
| 2 JS tests | 2 to 3 | every function covered |
| 3a to 3d JS refactor | 2 | all suites green per commit |
| 3e to 3f R and CSS | 1 to 2 | all suites green per commit |

Branch `refactor/pp-js`, one commit series per stage, `Version` bumped at each
stage that touches `inst/js`. Push after Stage 1 so prod gets the extraction
early and any cache or dependency problem surfaces before the refactor starts.
