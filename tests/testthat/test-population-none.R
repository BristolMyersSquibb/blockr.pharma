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

test_that("group_by_args() gives one all-subjects column for an all-NA group", {
  adsl <- data.frame(USUBJID = c("a", "b"), TRT = c("x", "y"))
  none <- dm_stamp_none(adsl)
  all_patients <- list(
    variable = "Group", levels = "All Patients",
    pools = list(`All Patients` = NULL)
  )
  expect_identical(group_by_args(none), all_patients)
  expect_identical(group_by_args(none, total = "All Patients"), all_patients)
  expect_identical(group_by_args(none, total = "Everyone")$levels, "Everyone")
  expect_error(group_by_args(none, total = "Total"), "named \"Total\"")
  expect_identical(
    group_by_args(dm_stamp_group(adsl, "TRT"))$levels, c("x", "y")
  )
})

test_that("an empty pin stamps NA only while the gear offers (none)", {
  make_dm <- function() {
    adsl <- data.frame(USUBJID = c("a", "b"), TRT = c("x", "y"),
                       SEX = c("F", "M"))
    ae <- data.frame(USUBJID = c("a", "b"), AEDECOD = c("p", "q"))
    dm::dm(adsl = adsl, ae = ae) |>
      dm::dm_add_pk(adsl, USUBJID) |>
      dm::dm_add_fk(ae, USUBJID, adsl)
  }
  blk <- new_population_filter_block(featured = c("TRT", "SEX"), pinned = "")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() make_dm())),
    {
      session$flushReact()
      result <- eval(session$returned$expr(), list(data = make_dm()))
      expect_false("Group" %in% names(result$adsl))

      session$setInputs(`expr-set_group_none` = TRUE)
      session$flushReact()
      result <- eval(session$returned$expr(), list(data = make_dm()))
      expect_true(all(is.na(result$adsl$Group)))
      expect_true(session$returned$state$group_none())
    }
  )
})
