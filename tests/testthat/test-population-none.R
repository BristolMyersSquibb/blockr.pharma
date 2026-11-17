test_that("an ungrouped board keeps Group, NA in every row, on the parent", {
  adsl <- data.frame(USUBJID = c("a", "b", "c"), TRT = c("x", "y", "x"))
  ae <- data.frame(USUBJID = c("a", "a", "b"), AEDECOD = c("p", "q", "p"))
  d <- dm::dm(adsl = adsl, ae = ae) |>
    dm::dm_add_pk(adsl, USUBJID) |>
    dm::dm_add_fk(ae, USUBJID, adsl)

  out <- dm_stamp_none(d)
  expect_identical(out$adsl$Group, rep(NA_character_, 3))
  expect_false("Group" %in% names(out$ae))

  expect_identical(dm_stamp_none(adsl)$Group, rep(NA_character_, 3))
})

test_that("group_by_args() gives no columns for an all-NA group", {
  adsl <- data.frame(USUBJID = c("a", "b"), TRT = c("x", "y"))
  expect_null(group_by_args(dm_stamp_none(adsl)))
  expect_identical(
    group_by_args(dm_stamp_group(adsl, "TRT"))$levels, c("x", "y")
  )
})
