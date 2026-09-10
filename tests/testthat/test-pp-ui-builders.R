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

test_that("the header bar, sort clause and caption take their namespace as an argument", {
  bar <- html(pp_header_bar_ui(ns, gear_disabled = FALSE, mode = "rday",
                               prestudy = FALSE, smooth = "auto"))
  expect_match(bar, 'id="t-pp_gear_btn"')
  expect_match(bar, 'id="t-pp_dl_root"')
  expect_match(bar, 'data-tl-mode="rday"')

  sorter <- pp_cohort_sort_ui(c(id = "Patient id", worst = "Worst"), "worst", ns)
  expect_match(html(sorter), 'id="t-cohort_sort_by"')
  expect_match(html(sorter), 'data-index="1"')
  expect_null(pp_cohort_sort_ui(c(id = "Patient id"), "id", ns))

  cap <- html(pp_band_caption_ui(
    list(viz_id = "ae_gantt", title = "t", caption = "Adverse Events"),
    sorter, pre = "01-701-", search = "rash"
  ))
  expect_match(cap, "pp-cohort-bandcap-find")
  expect_match(cap, "01-701-")
})

test_that("the block UI mounts the client with the namespace it was given", {
  ui <- html(pp_block_ui("t"))
  expect_match(ui, 'id="t-pp_layout"')
  expect_match(ui, 'PatientProfile.mount\\(\\{"id":"t"', fixed = FALSE)
})

test_that("the find box's group is named, so the narrow row has one item that gives", {
  # On a panel under 460px the controls take a row of their own and the find
  # box absorbs what the pill beside it leaves. The stylesheet needs to know
  # WHICH group that is, and `:has(> .pp-ctrl-search)` is not an option here
  # (blockr.ui#41: it restyles the whole document under Shiny), so the class
  # is written at the source.
  dm_obj <- pp_normalize_dm(dm::dm(
    adsl = data.frame(USUBJID = "S-1", stringsAsFactors = FALSE),
    adcm = data.frame(USUBJID = "S-1", CMTRT = "ASPIRIN",
                      CMDECOD = "ASPIRIN", CMCLAS = "ANALGESIC", ASTDY = 1,
                      stringsAsFactors = FALSE)
  ))
  out <- html(pp_controls_ui(cm_gantt_viz, "cm_gantt", dm_obj, list()))
  expect_match(out, "pp-ctrl-group pp-ctrl-group--search", fixed = TRUE)
  # The pill's group is NOT named: it is the fixed half of the row.
  expect_match(out, '<div class="pp-ctrl-group">', fixed = TRUE)
})
