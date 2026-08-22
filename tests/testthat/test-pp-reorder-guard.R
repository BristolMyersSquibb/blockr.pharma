# Dragging a card only reaches the server if the server accepts the payload.
# The guard used to compare the drag against the WHOLE selection, which the
# client can never reproduce: `selected` may name vizs the current data cannot
# offer, the sidebar renders intersect(sel, avail), and an invisible card
# cannot come back in the payload. One such id and every drag was refused --
# silently, forever, on exactly the boards where it matters (a real study whose
# findings cards come from its own PARCAT1 / LBCAT categories, restored against
# a patient that does not produce one of them).

skip_if_not_installed("shiny")

pp_reorder_dm <- function() {
  dm::dm(
    adsl = data.frame(
      USUBJID = "A", ACTARM = "P",
      TRTSDT = as.Date("2020-01-01"), TRTEDT = as.Date("2020-06-01"),
      stringsAsFactors = FALSE
    ),
    adae = data.frame(
      USUBJID = "A", AEDECOD = "HEADACHE",
      ASTDT = as.Date("2020-02-01"), AESEV = "MILD",
      stringsAsFactors = FALSE
    )
  )
}

test_that("a reorder applies when every selected viz is available", {
  blk <- new_patient_profile_block(
    selected = c("patient_overview", "ae_gantt")
  )

  shiny::testServer(blk[["expr_server"]],
                    args = list(data = function() pp_reorder_dm()), {
    session$flushReact()
    session$setInputs(reorder_viz = list("ae_gantt", "patient_overview"))
    session$flushReact()
    expect_equal(r_selected(), c("ae_gantt", "patient_overview"))
  })
})

test_that("a selected viz the data cannot offer does not veto the reorder", {
  # vital_signs needs advs, which this dm has not got: it stays in `selected`
  # and never appears as a card.
  blk <- new_patient_profile_block(
    selected = c("patient_overview", "ae_gantt", "vital_signs")
  )

  shiny::testServer(blk[["expr_server"]],
                    args = list(data = function() pp_reorder_dm()), {
    session$flushReact()
    avail <- names(r_available())
    expect_false("vital_signs" %in% avail)

    # The client can only send what it renders.
    session$setInputs(reorder_viz = list("ae_gantt", "patient_overview"))
    session$flushReact()

    # The visible pair is reordered ...
    expect_equal(r_selected()[1:2], c("ae_gantt", "patient_overview"))
    # ... and the unavailable id survives, rather than being dropped from the
    # board because one patient happened not to have the data.
    expect_true("vital_signs" %in% r_selected())
  })
})

test_that("an unavailable viz keeps its slot rather than being shunted", {
  # Reordering what you can see must not move what you cannot.
  blk <- new_patient_profile_block(
    selected = c("patient_overview", "vital_signs", "ae_gantt")
  )

  shiny::testServer(blk[["expr_server"]],
                    args = list(data = function() pp_reorder_dm()), {
    session$flushReact()
    session$setInputs(reorder_viz = list("ae_gantt", "patient_overview"))
    session$flushReact()
    # vital_signs was second and is still second.
    expect_equal(r_selected(), c("ae_gantt", "vital_signs", "patient_overview"))
  })
})

test_that("a payload that is not a permutation of the visible cards is refused", {
  blk <- new_patient_profile_block(
    selected = c("patient_overview", "ae_gantt", "vital_signs")
  )

  shiny::testServer(blk[["expr_server"]],
                    args = list(data = function() pp_reorder_dm()), {
    session$flushReact()
    before <- r_selected()

    # Drops a visible card
    session$setInputs(reorder_viz = list("ae_gantt"))
    session$flushReact()
    expect_equal(r_selected(), before)

    # Invents one
    session$setInputs(reorder_viz = list("ae_gantt", "patient_overview", "nope"))
    session$flushReact()
    expect_equal(r_selected(), before)
  })
})
