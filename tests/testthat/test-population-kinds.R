test_that("the stamp marks Group and Subgroup as group columns", {
  skip_if_not_installed("blockr.viz")
  adsl <- data.frame(USUBJID = c("a", "b", "c"), TRT = c("P", "A", "A"),
                     SEX = c("F", "M", "F"))
  ae <- data.frame(USUBJID = c("a", "b"), AEDECOD = c("x", "y"))
  attr(adsl, "blockr_kinds") <- c(TRT = "group", SEX = "group", USUBJID = "id")
  attr(ae, "blockr_kinds") <- c(TRT = "group", USUBJID = "id", AEDECOD = "id")
  d <- dm::dm_add_fk(
    dm::dm_add_pk(dm::dm(adsl = adsl, ae = ae), adsl, USUBJID),
    ae, USUBJID, adsl
  )

  s <- dm_stamp_group(dm_stamp_group(d, "TRT"), "SEX", "Subgroup")
  kinds <- blockr.viz::column_kinds(s$adsl)
  expect_equal(unname(kinds[c("Group", "Subgroup")]), c("group", "group"))

  # A flatten keeps the start table's marks, so the event table is marked too.
  flat <- blockr.viz::column_kinds(dm::dm_flatten_to_tbl(s, ae))
  expect_equal(unname(flat[c("Group", "Subgroup")]), c("group", "group"))
  expect_equal(nrow(dm::dm_get_all_fks(s)), 1L)
})

test_that("a board without marks gets none from the stamp", {
  plain <- dm::dm(adsl = data.frame(USUBJID = "a", TRT = "P"))
  expect_null(attr(dm_stamp_group(plain, "TRT")$adsl, "blockr_kinds"))
  df <- data.frame(TRT = "P")
  expect_null(attr(dm_stamp_group(df, "TRT"), "blockr_kinds"))
})
