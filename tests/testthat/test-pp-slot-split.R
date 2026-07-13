# The chart area is split into a stable panel skeleton (one uiOutput shell
# per selected viz) and per-viz slot outputs. Switching patient re-renders
# only the slots; the skeleton, sidebar and viz availability are derived
# from the unscoped cohort dm and stay put.

test_that("pp_no_patient_rows flags a patient with no rows in any viz table", {
  skip_if_not_installed("dm")

  d <- dm::dm(
    adsl = data.frame(USUBJID = "a"),
    adae = data.frame(USUBJID = character(), AEDECOD = character()),
    advs = data.frame(USUBJID = character(), PARAMCD = character())
  )
  expect_true(pp_no_patient_rows(d, "adae"))
  expect_true(pp_no_patient_rows(d, c("adae", "advs")))
  # one non-empty table is enough to hand off to the renderer
  expect_false(pp_no_patient_rows(d, c("adsl", "adae")))
  # tables absent from the dm are not "empty"
  expect_false(pp_no_patient_rows(d, "adqsadas"))
})

test_that("availability is cohort-based and stable across patient switches", {
  skip_if_not_installed("dm")

  # Patient "a" has vitals rows, patient "b" has none. Under patient-scoped
  # availability the vitals findings viz would vanish when "b" is picked;
  # cohort-scoped availability must keep it listed for both.
  co <- dm::dm(
    adsl = data.frame(
      USUBJID = c("a", "b"),
      TRTSDT = as.Date("2013-01-01") + c(0, 5),
      TRTEDT = as.Date("2013-06-01") + c(0, 5)
    ),
    advs = data.frame(
      USUBJID = "a",
      PARAMCD = "SYSBP",
      PARAM = "Systolic Blood Pressure",
      AVAL = 120,
      ADT = as.Date("2013-02-01")
    )
  ) |>
    dm::dm_add_pk(adsl, USUBJID) |>
    dm::dm_add_fk(advs, USUBJID, adsl)

  blk <- new_patient_profile_block()
  shiny::testServer(
    blk[["expr_server"]],
    args = list(data = function() co),
    {
      session$flushReact()
      avail0 <- names(r_available())
      expect_true("patient_overview" %in% avail0)
      vitals_ids <- setdiff(
        avail0, names(patient_profile_static_vizs())
      )
      expect_true(length(vitals_ids) >= 1L)

      session$setInputs(pp_subject = "a")
      expect_identical(names(r_available()), avail0)
      expect_true(isTRUE(r_pick_state()$single))

      session$setInputs(pp_subject = "b")
      expect_identical(names(r_available()), avail0)
      expect_true(isTRUE(r_pick_state()$single))

      # the scoped dm did narrow to the picked patient
      scoped <- r_scoped_dm()
      adsl <- as.data.frame(dm::dm_get_tables(scoped$dm)$adsl)
      expect_identical(adsl$USUBJID, "b")
    }
  )
})

test_that("pick state dedups so the skeleton skips patient A -> B switches", {
  skip_if_not_installed("dm")

  co <- dm::dm(
    adsl = data.frame(
      USUBJID = c("a", "b"),
      TRTSDT = as.Date("2013-01-01") + c(0, 5),
      TRTEDT = as.Date("2013-06-01") + c(0, 5)
    )
  )

  blk <- new_patient_profile_block()
  shiny::testServer(
    blk[["expr_server"]],
    args = list(data = function() co),
    {
      session$flushReact()
      expect_false(isTRUE(r_pick_state()$single))

      session$setInputs(pp_subject = "a")
      st_a <- r_pick_state()
      expect_true(isTRUE(st_a$single))

      session$setInputs(pp_subject = "b")
      # identical value: reactiveVal must not have produced a new object,
      # which is what keeps the chart-area skeleton out of the redraw path
      expect_identical(r_pick_state(), st_a)
    }
  )
})

test_that("chart area renders slot shells; empty-for-patient viz says so", {
  skip_if_not_installed("dm")

  co <- dm::dm(
    adsl = data.frame(
      USUBJID = c("a", "b"),
      TRTSDT = as.Date("2013-01-01") + c(0, 5),
      TRTEDT = as.Date("2013-06-01") + c(0, 5)
    ),
    advs = data.frame(
      USUBJID = "a",
      PARAMCD = "SYSBP",
      PARAM = "Systolic Blood Pressure",
      AVAL = 120,
      ADT = as.Date("2013-02-01")
    )
  ) |>
    dm::dm_add_pk(adsl, USUBJID) |>
    dm::dm_add_fk(advs, USUBJID, adsl)

  blk <- new_patient_profile_block()
  shiny::testServer(
    blk[["expr_server"]],
    args = list(data = function() co),
    {
      session$setInputs(pp_subject = "a")
      session$flushReact()

      vitals_id <- setdiff(
        names(r_available()), names(patient_profile_static_vizs())
      )[[1L]]
      r_selected(unique(c(r_selected(), vitals_id)))
      session$flushReact()

      # skeleton: one shell per selected+available viz, panel class on the
      # uiOutput container itself
      skel_a <- output$chart_area
      expect_match(skel_a$html, "viz_slot_patient_overview", fixed = TRUE)
      expect_match(skel_a$html, paste0("viz_slot_", vitals_id), fixed = TRUE)
      expect_match(skel_a$html, "pp-chart-panel", fixed = TRUE)

      # patient "a" has vitals rows: the slot holds a chart, not the notice
      slot_a <- output[[paste0("viz_slot_", vitals_id)]]
      expect_no_match(slot_a$html, "No data for this patient", fixed = TRUE)

      session$setInputs(pp_subject = "b")
      session$flushReact()

      # skeleton unchanged, slot now carries the no-data notice
      expect_identical(output$chart_area$html, skel_a$html)
      slot_b <- output[[paste0("viz_slot_", vitals_id)]]
      expect_match(slot_b$html, "No data for this patient", fixed = TRUE)
    }
  )
})
