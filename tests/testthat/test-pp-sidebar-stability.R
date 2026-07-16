# The sidebar's card list must survive upstream dm updates that change no
# card. The production wiring feeds the profile ONE patient per drill click
# (pt_semi -> profile), so "upstream update" usually means "another single
# patient": the catalog object handed to the sidebar renderUI must be the
# SAME object across such updates (reactiveVal skips identical writes), or
# the whole card list re-renders on every drill.

skip_if_not_installed("shiny")

pp_one_patient <- function(id, pcs = c("ALT", "AST")) {
  dm::dm(
    adsl = data.frame(
      USUBJID = id, ACTARM = "Placebo",
      TRTSDT = as.Date("2020-01-01"), TRTEDT = as.Date("2020-06-01"),
      stringsAsFactors = FALSE
    ),
    adae = data.frame(
      USUBJID = id, AEDECOD = "HEADACHE",
      ASTDT = as.Date("2020-02-01"), AESEV = "MILD",
      stringsAsFactors = FALSE
    ),
    adlbc = data.frame(
      USUBJID = id, PARAMCD = pcs, AVAL = seq_along(pcs),
      ADT = as.Date("2020-02-01"), stringsAsFactors = FALSE
    )
  )
}

test_that("the card catalog object survives a patient switch upstream", {
  blk <- new_patient_profile_block(selected = c("patient_overview",
                                                "liver_panel"))
  srv <- blk[["expr_server"]]

  r_in <- shiny::reactiveVal(pp_one_patient("A", c("ALT", "AST")))

  shiny::testServer(srv, args = list(data = function() r_in()), {
    session$flushReact()
    cat_a <- r_available_val()
    expect_true(!is.null(cat_a))
    expect_true("liver_panel" %in% names(cat_a))

    # another single patient, same tables, other params within the group:
    # the catalog object must be the SAME (no sidebar invalidation)
    r_in(pp_one_patient("B", c("ALT", "BILI")))
    session$flushReact()
    expect_identical(cat_a, r_available_val())

    # and a genuine catalog change (a new table) still gets through
    with_vs <- pp_one_patient("C")
    tbls <- dm::dm_get_tables(with_vs)
    tbls$advs <- data.frame(
      USUBJID = "C", PARAMCD = "SYSBP", AVAL = 120,
      ADT = as.Date("2020-02-01"), stringsAsFactors = FALSE
    )
    r_in(do.call(dm::dm, tbls))
    session$flushReact()
    cat_c <- r_available_val()
    expect_false(identical(cat_a, cat_c))
    expect_true("blood_pressure" %in% names(cat_c))
  })
})
