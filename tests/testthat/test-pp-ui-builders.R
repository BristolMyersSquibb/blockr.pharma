# The UI builders that left the server module take everything they need as
# arguments. None of them may reach for `session`: they run wherever a
# namespace function is handed in, and the server hands in `session$ns`.
# The empty chart area is the case the fixtures never render, because the
# fixture picks a patient first, and it is where "object 'session' not
# found" showed up on the dev app.

ns <- function(x) paste0("t-", x)
html <- function(x) as.character(htmltools::renderTags(x)$html)

test_that("the chart area renders both empty states and the stack", {
  none <- html(pp_chart_area_ui(ns, single = FALSE, character()))
  expect_match(none, "blockr-empty")
  expect_match(none, 'id="t-pp_empty_hint"')

  bare <- html(pp_chart_area_ui(ns, single = TRUE, character()))
  expect_match(bare, "No panels on the profile")

  stack <- html(pp_chart_area_ui(ns, TRUE, c("patient_overview", "ae_gantt")))
  expect_match(stack, 'id="t-viz_slot_patient_overview"')
  expect_match(stack, "pp-treatment-strip")
  expect_match(stack, 'id="t-viz_slot_ae_gantt"')
})

test_that("the header row, the tray, the sort clause and caption take their namespace", {
  head <- html(pp_head_ui(ns))
  expect_match(head, 'id="t-pp_cohort_seg"')
  expect_match(head, 'id="t-subject_title"')
  expect_match(head, 'id="t-subject_facts"')
  # The gear is the design system's: the one framed square, last.
  expect_match(head, 'class="blockr-gear-btn" id="t-pp_gear_btn"', fixed = TRUE)
  # The list toggle is the muted glyph; the cohort's status beside it is
  # painted by pp-header.js (and shown only while the list is shut).
  expect_match(head, '<button class="pp-list-toggle" id="t-pp_cohort_count"',
               fixed = TRUE)
  expect_match(head, 'class="pp-cohort-status" id="t-pp_head_status"', fixed = TRUE)
  expect_no_match(head, "pp-cohort-reset", fixed = TRUE)

  tray <- html(pp_gear_tray_ui(ns))
  expect_match(tray, 'blockr-settings blockr-settings--beak pp-gear-tray', fixed = TRUE)
  expect_match(tray, 'id="t-pp_gear_display"')
  expect_match(tray, 'id="t-gear_coverage"')

  sorter <- pp_cohort_sort_ui(c(id = "Patient id", worst = "Worst severity"),
                              "worst", ns)
  expect_match(sorter, 'id="t-cohort_sort_by"', fixed = TRUE)
  expect_match(sorter, 'data-input="cohort_sort"', fixed = TRUE)
  expect_match(sorter, "^by <button")
  expect_match(sorter, ">severity</button>", fixed = TRUE)
  expect_null(pp_cohort_sort_ui(c(id = "Patient id"), "id", ns))

  cap <- html(pp_band_caption_ui(
    list(viz_id = "ae_gantt", title = "t", caption = "Adverse events"),
    sorter
  ))
  expect_match(cap, "<span>Patients</span>", fixed = TRUE)
  # The count is the status slot, painted by pp-header.js.
  expect_match(cap, '<span class="pp-cohort-status"></span>', fixed = TRUE)
  expect_no_match(cap, "pp-cohort-prefix", fixed = TRUE)
  expect_match(cap, "Adverse events, by <button", fixed = TRUE)
  # The strip's filter is not repeated here; the panel that sets it says so.
  expect_no_match(cap, "bandcap-find")

  # A parameter's strip names the parameter after its code.
  lab <- html(pp_band_caption_ui(
    list(viz_id = "x", title = "t", caption = "ALB", sub = "Albumin (g/L)"),
    NULL
  ))
  expect_match(lab, 'ALB<span class="pp-gear-meta">Albumin (g/L)</span>', fixed = TRUE)
})

test_that("the block UI mounts the client with the namespace it was given", {
  ui <- html(pp_block_ui("t"))
  expect_match(ui, 'id="t-pp_layout"')
  expect_match(ui, 'PatientProfile.mount\\(\\{"id":"t"', fixed = FALSE)
})

cm_ctrl_dm <- function(rows = 1L) {
  cm <- data.frame(
    USUBJID = "S-1", CMTRT = "ASPIRIN", CMDECOD = "ASPIRIN",
    CMCLAS = "ANALGESIC", ASTDY = 1, stringsAsFactors = FALSE
  )
  pp_normalize_dm(dm::dm(
    adsl = data.frame(USUBJID = "S-1", stringsAsFactors = FALSE),
    adcm = cm[seq_len(rows), , drop = FALSE]
  ))
}

# The filter's word alone.
find_word <- function(out) {
  m <- regmatches(out, regexpr('<button[^>]*data-kind="find"[^>]*>', out))
  if (!length(m)) "" else m
}

test_that("a panel's sentence names what it draws, each setting a live word", {
  out <- html(pp_controls_ui(cm_gantt_viz, "cm_gantt", cm_ctrl_dm(), list()))
  expect_match(out, '<span class="pp-chart-sentence">1 medication by <button')
  expect_match(out, '>coded name</button>, showing <button', fixed = TRUE)
  # The lanes word lists every level the data has, labels first in the menu.
  expect_match(out, 'data-title="Lanes"', fixed = TRUE)
  expect_match(out, "Drug class", fixed = TRUE)
})

test_that("the filter's word carries this patient's terms at the lanes' level", {
  out <- html(pp_controls_ui(cm_gantt_viz, "cm_gantt", cm_ctrl_dm(), list()))
  w <- find_word(out)
  expect_match(w, 'data-col="CMDECOD"', fixed = TRUE)
  expect_match(w, "ASPIRIN", fixed = TRUE)
  expect_match(w, 'data-title="Show"', fixed = TRUE)

  # A level of its own: the class, not the name.
  by_class <- html(pp_controls_ui(cm_gantt_viz, "cm_gantt", cm_ctrl_dm(),
                                  list(lanes = "CMCLAS")))
  expect_match(find_word(by_class), "ANALGESIC", fixed = TRUE)

  # With picks: the word lists them, and the sentence says how many of this
  # patient's records are left.
  picked <- html(pp_controls_ui(
    cm_gantt_viz, "cm_gantt", cm_ctrl_dm(),
    list(find = list(list(col = "CMDECOD", value = "ASPIRIN")))
  ))
  expect_match(picked, ">Aspirin</button> (1 of 1)", fixed = TRUE)
})

test_that("a patient with no records in the table gets no controls", {
  # Nothing to filter is not a filter, and no rows means no levels to group
  # by: no options, no control.
  out <- html(pp_controls_ui(cm_gantt_viz, "cm_gantt", cm_ctrl_dm(0L),
                             list()))
  expect_identical(out, "")
})

test_that("an on/off control is a checkbox, not a switch", {
  viz <- structure(list(id = "x", tables = "t", controls = list(
    chg = list(type = "toggle", label = "Change from baseline", default = FALSE)
  )), class = c("pp_viz", "list"))
  out <- html(pp_controls_ui(viz, "x", dm::dm(t = data.frame(a = 1)), list(chg = TRUE)))
  expect_match(out, 'class="blockr-checkbox pp-ctrl-check"', fixed = TRUE)
  expect_match(out, 'data-param="chg" checked', fixed = TRUE)
  expect_match(out, "Change from baseline")
})

test_that("the download menu is an action menu of Shiny download links", {
  menu <- pp_download_menu_ui(ns)
  out <- html(menu)
  expect_match(out, 'class="blockr-action-menu" data-align="end"', fixed = TRUE)
  expect_match(out, 'id="t-pp_dl_root"')
  # Hidden until dl_menu_state says there is something to offer.
  expect_identical(htmltools::tagGetAttribute(menu, "hidden"), NA)
  expect_match(out, 'id="t-dl_cohort_xlsx"')
  expect_match(out, 'data-scope="cohort"')
  expect_match(out, '<span class="blockr-menu__meta">.xlsx</span>', fixed = TRUE)
  expect_match(out, 'id="t-pp_dl_label_cohort"')
})

test_that("the gear tray names each study variable with its label", {
  adsl <- data.frame(USUBJID = "a", ACTARM = "X", TRTSDT = as.Date("2020-01-01"))
  attr(adsl$ACTARM, "label") <- "Actual Arm"
  adae <- data.frame(USUBJID = "a", ASEV = "MILD")
  dm_obj <- dm::dm(adsl = adsl, adae = adae)
  roles <- list(arm = "ACTARM", severity = "ASEV", timeline = NULL)
  out <- html(pp_gear_coverage_ui(
    list(list(id = "x", label = "NPI-X Radar", reason = "needs adqsnpix")),
    roles, dm_obj
  ))
  expect_match(out, "Study variables")
  expect_match(out, 'ACTARM\\s*<span class="pp-gear-meta">Actual Arm</span>')
  # No label, or none that differs from the name: the name alone.
  expect_match(out, '<div class="pp-gear-value">ASEV</div>', fixed = TRUE)
  expect_match(out, "None; relative days are off")
  expect_match(out, "Not available in this study")
  expect_match(out, 'NPI-X Radar\\s*<span class="pp-gear-meta">needs adqsnpix</span>')

  # Every panel drawable: no such section at all.
  expect_no_match(html(pp_gear_coverage_ui(list(), roles, dm_obj)),
                  "Not available")
})

test_that("the header's sentence says the facts in words and drops what is missing", {
  red <- function(x) "#dc2626"
  f <- data.frame(USUBJID = "a", SEX = "F", AGE = 80, TRTDURD = 27,
                  AE_N = 6, AE_WORST = "SEVERE")
  out <- html(pp_subject_sentence_ui(f, 1L, "Arm <x>", red))
  expect_match(out, paste0(
    "Arm &lt;x&gt; \u00b7 F, 80 years \u00b7 27 days on treatment \u00b7 ",
    "6 adverse events, worst <span"
  ))
  expect_match(out, "background:#dc2626")

  one <- f
  one$TRTDURD <- 1
  one$AE_N <- 1
  expect_match(html(pp_subject_sentence_ui(one, 1L, NA, red)),
               "1 day on treatment \u00b7 1 adverse event, worst")

  none <- f
  none$AE_N <- 0
  expect_match(html(pp_subject_sentence_ui(none, 1L, NA, red)),
               "no adverse events")

  bare <- data.frame(USUBJID = "a")
  expect_null(pp_subject_sentence_ui(bare, 1L, NA, red))

  expect_match(html(pp_subject_title_ui("01-701-1015")), ">01-701-1015<")
  expect_null(pp_subject_title_ui(NULL))
})
