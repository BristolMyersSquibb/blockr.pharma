# Extracted from test-pp-slot-split.R:143

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "blockr.pharma", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
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
sent <- list()
withr::local_options(list(
    blockr.pharma.pp_message_sink = function(channel, payload) {
      sent[[length(sent) + 1L]] <<- list(channel = channel, payload = payload)
    }
  ))
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

      # A patient switch does not re-render the slot: the panel is
      # updated in place through a `slot` message that carries the new
      # header and chart option (pp_slot_update()).
      sent <<- list()
      session$setInputs(pp_subject = "b")
      session$flushReact()

      # skeleton unchanged, the slot output too
      expect_identical(output$chart_area$html, skel_a$html)
      expect_identical(output[[paste0("viz_slot_", vitals_id)]]$html, slot_a$html)
      # the message is what carries the no-data notice
      slots <- Filter(function(m) identical(m$channel, "slot"), sent)
      ids <- vapply(slots, function(m) m$payload$viz_id, character(1L))
      expect_true(vitals_id %in% ids)
      msg <- slots[[match(vitals_id, ids)]]$payload
      expect_match(msg$opts_json, "No data for this patient", fixed = TRUE)
      expect_match(msg$header, "pp-chart-header", fixed = TRUE)
      expect_true(is.numeric(msg$height))
      expect_true(is.list(msg$evals))
    }
  )
