# Boards saved with the swimmer or the old timeline. The fixtures are the
# JSON a board wrote for each block, with its whole table state, before the
# blocks were removed.

read_saved <- function(name) {
  jsonlite::fromJSON(test_path("fixtures", name), simplifyDataFrame = FALSE,
                     simplifyMatrix = FALSE)
}

test_that("a saved swimmer opens as the efficacy timeline", {
  blk <- blockr.core::blockr_deser(read_saved("saved-swimmer.json"))
  expect_s3_class(blk, "efficacy_timeline_block")
  sv <- blockr.core::blockr_ser(blk)$payload$script_values
  # It drew con meds, the reason tags and death.
  expect_identical(as.character(unlist(sv$layers)),
                   c("Response", "Treatment", "Discontinuation and death",
                     "Con meds"))
  expect_identical(as.character(unlist(sv$group)), "SEX")
  expect_identical(as.character(unlist(sv$doses)), "Plain")
  expect_true(isTRUE(unlist(sv$dose_tags)))
  # The renamed title stays.
  expect_identical(blockr.core::blockr_ser(blk)$payload$title,
                   "Response by patient")
  # It saves as the efficacy timeline and draws from the data.
  rt <- board_round_trip(blk, do.call(dm::dm, pt_toy()))
  expect_identical(attr(attr(rt$block, "ctor"), "fun"),
                   "new_efficacy_timeline_block")
  expect_equal(plain_state(rt$restored), plain_state(rt$saved),
               ignore_attr = TRUE)
  f <- patient_timeline_frame(do.call(dm::dm, pt_toy()),
                              layers = sv$layers, group = "SEX")
  expect_true(all(c("Response", "Con meds") %in% f$.lane))
})

test_that("a saved timeline opens as the safety timeline", {
  blk <- blockr.core::blockr_deser(read_saved("saved-timeline.json"))
  expect_s3_class(blk, "safety_timeline_block")
  p <- blockr.core::blockr_ser(blk)$payload
  sv <- p$script_values
  expect_identical(as.character(unlist(sv$layers)),
                   c("Treatment", "Discontinuation and death",
                     "Adverse events", "Con meds"))
  expect_identical(as.character(unlist(sv$treatment_as)), "bars by dose")
  expect_identical(as.character(unlist(sv$ae_colour)), "AESEV")
  expect_identical(as.character(unlist(sv$cm_colour)), "CMCLAS")
  expect_identical(p$open, "all")
  # The old default title gives way to the preset's.
  expect_identical(p$title, "Safety timeline")
  rt <- board_round_trip(blk, do.call(dm::dm, pt_toy()))
  expect_identical(attr(attr(rt$block, "ctor"), "fun"),
                   "new_safety_timeline_block")
  expect_equal(plain_state(rt$restored), plain_state(rt$saved),
               ignore_attr = TRUE)
})

test_that("the old constructors build the patient timeline from defaults", {
  expect_s3_class(new_swimmer_block(), "efficacy_timeline_block")
  expect_s3_class(new_timeline_block(), "safety_timeline_block")
  sv <- blockr.core::blockr_ser(
    new_swimmer_block(by = "USUBJID", reasons = FALSE, events = "Progression")
  )$payload$script_values
  expect_identical(as.character(unlist(sv$layers)), c("Response", "Treatment"))
  expect_identical(as.character(unlist(sv$group)), PT_NO_GROUP)
  sv <- blockr.core::blockr_ser(
    new_timeline_block(show = "Con meds", death = FALSE)
  )$payload$script_values
  expect_identical(as.character(unlist(sv$layers)), "Con meds")
})
