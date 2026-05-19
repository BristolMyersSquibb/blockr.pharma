test_that("new_patient_profile_block constructs with expected class", {
  blk <- new_patient_profile_block()
  expect_s3_class(blk, "patient_profile_block")
  expect_s3_class(blk, "block")
})

test_that("constructor accepts selected and viz_settings", {
  blk <- new_patient_profile_block(
    selected = c("patient_overview", "ae_gantt"),
    viz_settings = list(adas_trajectory = list(items = "ACTOT"))
  )
  expect_s3_class(blk, "patient_profile_block")
})

test_that("timeline_mode is validated", {
  expect_error(new_patient_profile_block(timeline_mode = "nope"))
  expect_s3_class(
    new_patient_profile_block(timeline_mode = "rday"),
    "patient_profile_block"
  )
})

test_that("a single-patient pharmaverseadam dm can feed the block", {
  skip_if_not_installed("pharmaverseadam")
  skip_if_not_installed("dm")

  adsl <- pharmaverseadam::adsl
  adae <- pharmaverseadam::adae
  advs <- pharmaverseadam::advs

  one <- adsl$USUBJID[1]
  pp_dm <- dm::dm(
    adsl = adsl[adsl$USUBJID == one, ],
    adae = adae[adae$USUBJID == one, ],
    advs = advs[advs$USUBJID == one, ]
  )

  tbls <- dm::dm_get_tables(pp_dm)
  expect_true(all(c("adsl", "adae", "advs") %in% names(tbls)))
  expect_identical(unique(as.data.frame(tbls$adsl)$USUBJID), one)

  expect_s3_class(
    new_patient_profile_block(selected = "patient_overview"),
    "patient_profile_block"
  )
})
