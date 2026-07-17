# The default timeline hides deep pre-treatment history: the range floor is
# a 30-day screening window before treatment start, so a medication started
# years earlier does not stretch every axis to it, while baselines stay.

test_that("the range floor is the screening window before the reference", {
  ref <- pp_ms_ts(as.Date("2020-06-01"))
  tr <- as.Date(c("2015-01-01", "2020-12-01"))

  clipped <- pp_clip_prestudy(tr, ref)
  expect_identical(clipped, as.Date(c("2020-05-02", "2020-12-01")))

  # a range already inside the window passes through untouched
  near <- as.Date(c("2020-05-20", "2020-12-01"))
  expect_identical(pp_clip_prestudy(near, ref), near)
})

test_that("clipping is total and never inverts the range", {
  tr <- as.Date(c("2015-01-01", "2020-12-01"))
  expect_identical(pp_clip_prestudy(tr, NA_real_), tr)  # no reference
  expect_null(pp_clip_prestudy(NULL, pp_ms_ts(Sys.Date())))

  # a study entirely before the reference keeps a valid (point) range
  ref <- pp_ms_ts(as.Date("2021-06-01"))
  old <- as.Date(c("2015-01-01", "2015-03-01"))
  clipped <- pp_clip_prestudy(old, ref)
  expect_true(clipped[1] <= clipped[2])
  expect_identical(clipped[2], old[2])
})

test_that("show_prestudy is a validated constructor state field", {
  expect_true("show_prestudy" %in% names(formals(new_patient_profile_block)))
  expect_s3_class(new_patient_profile_block(show_prestudy = TRUE),
                  "patient_profile_block")
  expect_error(new_patient_profile_block(show_prestudy = "yes"))
})
