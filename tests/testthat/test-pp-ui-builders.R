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
  expect_match(none, "pp-empty-state")
  expect_match(none, 'id="t-pp_empty_hint"')

  bare <- html(pp_chart_area_ui(ns, single = TRUE, character()))
  expect_match(bare, "No visualizations selected")

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
  # The reset is there from the start, disabled until a drill.
  expect_match(head, '<button class="pp-cohort-reset" id="t-pp_cohort_reset" type="button" disabled',
               fixed = TRUE)

  tray <- html(pp_gear_tray_ui(ns))
  expect_match(tray, 'blockr-settings blockr-settings--beak pp-gear-tray', fixed = TRUE)
  expect_match(tray, 'id="t-pp_gear_display"')
  expect_match(tray, 'id="t-gear_coverage"')

  sorter <- pp_cohort_sort_ui(c(id = "Patient id", worst = "Worst"), "worst", ns)
  expect_match(html(sorter), 'id="t-cohort_sort_by"')
  expect_match(html(sorter), 'data-index="1"')
  expect_null(pp_cohort_sort_ui(c(id = "Patient id"), "id", ns))

  cap <- html(pp_band_caption_ui(
    list(viz_id = "ae_gantt", title = "t", caption = "Adverse Events"),
    sorter, pre = "01-701-",
    picks = list(list(col = "*", value = "rash"))
  ))
  expect_match(cap, "pp-cohort-bandcap-find")
  expect_match(cap, "01-701-")

  # One pick is named.
  expect_match(cap, "\u201crash\u201d", fixed = TRUE)

  # Several are counted, not named: there is no width in this caption for
  # "General disorders and administration site conditions +2", which
  # ellipsized to "G.." and said less than a number does. The tooltip names
  # them all either way.
  many <- html(pp_band_caption_ui(
    list(viz_id = "ae_gantt", title = "t", caption = "Adverse Events"),
    sorter, pre = "",
    picks = list(list(col = "AEBODSYS", value = "CARDIAC DISORDERS"),
                 list(col = "AEDECOD", value = "PNEUMONIA"))
  ))
  expect_match(many, ">2 filters<", fixed = TRUE)
  expect_match(many, "Cardiac disorders, Pneumonia", fixed = TRUE)

  # No picks, no chip: the caption is not a control that does nothing.
  bare <- html(pp_band_caption_ui(
    list(viz_id = "ae_gantt", title = "t", caption = "Adverse Events"),
    sorter, pre = "", picks = list()
  ))
  expect_false(grepl("pp-cohort-bandcap-find", bare, fixed = TRUE))
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

# The find trigger alone. The lanes pill beside it carries every level in its
# own data-values, CMTRT included, so a grep over the whole header cannot say
# what the PICKER offers.
find_trigger <- function(out) {
  m <- regmatches(out, regexpr("<button class=\"pp-ctrl-find[^>]*>", out))
  if (!length(m)) "" else m
}

test_that("the find group is named, so the narrow row has one item that gives", {
  # On a narrow panel the controls take a row of their own and the find
  # trigger absorbs what the pill beside it leaves. The stylesheet needs to
  # know WHICH group that is, and `:has(> .pp-ctrl-find)` is not an option
  # here (blockr.ui#41: it restyles the whole document under Shiny), so the
  # class is written at the source.
  out <- html(pp_controls_ui(cm_gantt_viz, "cm_gantt", cm_ctrl_dm(), list()))
  expect_match(out, "pp-ctrl-group pp-ctrl-group--find", fixed = TRUE)
  # The pill's group is NOT named: it is the fixed half of the row.
  expect_match(out, '<div class="pp-ctrl-group">', fixed = TRUE)
})

test_that("the trigger carries the options and the picks the popover reads", {
  # The list travels WITH the header rather than being fetched when the
  # popover opens: a few hundred bytes against a nine-kilobyte slot payload,
  # and a control that waits a round trip before it can show anything is the
  # one thing this control cannot be.
  out <- html(pp_controls_ui(cm_gantt_viz, "cm_gantt", cm_ctrl_dm(), list()))
  expect_match(out, "pp-ctrl-find", fixed = TRUE)
  expect_match(out, "data-options", fixed = TRUE)
  expect_match(out, "Drug class", fixed = TRUE)
  expect_match(out, "ANALGESIC", fixed = TRUE)
  # The verbatim level is not offered as a row: it is close to one distinct
  # value per record, so it is the level that would grow the list with the
  # patient. It stays reachable by typing.
  expect_false(grepl("CMTRT", find_trigger(out), fixed = TRUE))

  # With picks: the count, the clear button and the terms in the tooltip.
  picked <- html(pp_controls_ui(
    cm_gantt_viz, "cm_gantt", cm_ctrl_dm(),
    list(find = list(list(col = "CMDECOD", value = "ASPIRIN")))
  ))
  expect_match(picked, "pp-ctrl-find is-active", fixed = TRUE)
  expect_match(picked, "pp-ctrl-find-badge", fixed = TRUE)
  expect_match(picked, "pp-ctrl-find-clear", fixed = TRUE)
  expect_match(picked, "Filtering on Aspirin", fixed = TRUE)
  # And the honest hit count: 1 of this patient's 1 record.
  expect_match(picked, "1/1", fixed = TRUE)
})

test_that("a patient with no records in the table gets no find control", {
  # Nothing to filter is not a filter. The panel says "no medication
  # records" and the header does not offer a control over an empty table --
  # the rule the gear already follows: no options, no control.
  out <- html(pp_controls_ui(cm_gantt_viz, "cm_gantt", cm_ctrl_dm(0L),
                             list()))
  expect_false(grepl("pp-ctrl-find", out, fixed = TRUE))
  # And with the lanes pill gone for the same reason (no rows, no levels to
  # group by), the header carries no controls row at all.
  expect_identical(out, "")
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
  expect_match(html(pp_subject_title_ui(NULL)), "is-none")
})
