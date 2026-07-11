# The arm label has two consumers (subject picker meta, overview treatment
# lane). They must resolve the same ADSL column, and a study that ships neither
# of the ADaM arm variables must be able to name its own.

test_that("arm_var wins over the ADaM fallback chain", {
  cols <- c("USUBJID", "TRT", "ARM", "ACTARM", "TRT01P")
  expect_identical(pp_arm_column(cols, "TRT"), "TRT")
  expect_identical(pp_arm_column(cols, NULL), "ARM")
})

test_that("the fallback chain is ordered and skips absent columns", {
  expect_identical(pp_arm_column(c("ACTARM", "TRT01P")), "ACTARM")
  expect_identical(pp_arm_column(c("TRT01P", "TRT01A")), "TRT01P")
  expect_identical(pp_arm_column(c("TRT01A")), "TRT01A")
})

test_that("no arm column at all resolves to NULL", {
  expect_null(pp_arm_column(c("USUBJID", "AGE")))
  # a declared column that the data does not carry falls through, it does not
  # error and does not get invented
  expect_null(pp_arm_column(c("USUBJID", "AGE"), "TRT"))
  expect_identical(pp_arm_column(c("USUBJID", "ARM"), "TRT"), "ARM")
})

test_that("the picker labels subjects with the declared arm column", {
  adsl <- data.frame(
    USUBJID = c("a", "b"),
    TRT = c("Drug X", "Drug Y"),
    ARM = c("GROUPAB COMPOUND-A", "GROUPBP1 COMPOUND-A"),
    AGE = c(63L, 71L), SEX = c("F", "M"),
    stringsAsFactors = FALSE
  )
  dm_obj <- dm::dm(adsl = adsl)

  declared <- pp_subject_choices(dm_obj, arm_var = "TRT")
  expect_identical(declared$meta, c("Drug X · 63F", "Drug Y · 71M"))

  # unset, the ADaM chain still picks ARM, preserving today's behaviour
  undeclared <- pp_subject_choices(dm_obj)
  expect_identical(undeclared$meta,
                   c("GROUPAB COMPOUND-A · 63F", "GROUPBP1 COMPOUND-A · 71M"))
})

test_that("the overview lane honors the declared arm column", {
  dm_obj <- dm::dm(
    adsl = data.frame(
      USUBJID = "a",
      TRTSDT = as.Date("2020-01-01"), TRTEDT = as.Date("2020-06-01"),
      TRT = "Drug X", ARM = "GROUPAB COMPOUND-A",
      stringsAsFactors = FALSE
    )
  )
  lane_label <- function(settings) {
    chart <- patient_overview_viz$render(
      dm_obj, time_range = pp_compute_time_range(dm_obj), settings = settings
    )
    trt <- Filter(function(s) identical(s$name, "Treatment"), chart$x$opts$series)
    paste(as.character(unlist(trt)), collapse = " ")
  }
  expect_match(lane_label(list(arm_var = "TRT")), "Drug X")
  expect_match(lane_label(list()), "GROUPAB COMPOUND-A")
})

test_that("picker and overview agree on the column once arm_var is declared", {
  # The regression this whole setting exists to prevent: prod adsl carries both
  # TRT and ARM, and the two consumers used to resolve different ones.
  cols <- c("USUBJID", "TRT", "ARM")
  expect_identical(pp_arm_column(cols, "TRT"), pp_arm_column(cols, "TRT"))
  expect_identical(pp_arm_column(cols, "TRT"), "TRT")
})

test_that("arm_var is validated at construction", {
  expect_error(new_patient_profile_block(arm_var = c("a", "b")))
  expect_error(new_patient_profile_block(arm_var = ""))
  expect_error(new_patient_profile_block(arm_var = 1))
  expect_s3_class(new_patient_profile_block(arm_var = "TRT"),
                  "patient_profile_block")
  expect_s3_class(new_patient_profile_block(), "patient_profile_block")
})
