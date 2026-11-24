subject_dm <- function(subjid = TRUE) {
  adsl <- data.frame(USUBJID = c("01-a", "01-b"), TRT = c("P", "A"))
  if (subjid) adsl$SUBJID <- c("1", "2")
  ae <- data.frame(USUBJID = c("01-a", "01-b", "01-b"),
                   AEDECOD = c("Rash", "Fever", "Rash"))
  dm::dm_add_fk(dm::dm_add_pk(dm::dm(adsl = adsl, ae = ae), adsl, USUBJID),
                ae, USUBJID, adsl)
}

display_of <- function(x) attr(x, "blockr_display", exact = TRUE)

test_that("every USUBJID column names SUBJID as its display column", {
  out <- dm_label_subjects(subject_dm())
  expect_identical(display_of(out$adsl$USUBJID), "SUBJID")
  expect_identical(display_of(out$ae$USUBJID), "SUBJID")
  expect_identical(as.vector(out$ae$USUBJID), subject_dm()$ae$USUBJID)
  expect_identical(dm::dm_get_all_fks(out), dm::dm_get_all_fks(subject_dm()))
})

test_that("the mark reaches a flattened event table", {
  flat <- dm::dm_flatten_to_tbl(dm_label_subjects(subject_dm()), ae, adsl)
  expect_identical(display_of(flat$USUBJID), "SUBJID")
  expect_true("SUBJID" %in% names(flat))
})

test_that("a study without SUBJID is left alone", {
  out <- dm_label_subjects(subject_dm(subjid = FALSE))
  expect_null(display_of(out$adsl$USUBJID))
  df <- data.frame(USUBJID = "a")
  expect_identical(dm_label_subjects(df), df)
  marked <- dm_label_subjects(data.frame(USUBJID = "a", SUBJID = "1"))
  expect_identical(display_of(marked$USUBJID), "SUBJID")
})

test_that("the population filter's expression labels subjects", {
  blk <- new_population_filter_block(featured = "TRT", pinned = "TRT")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() subject_dm())),
    {
      session$flushReact()
      txt <- paste(deparse(session$returned$expr(), width.cutoff = 500L),
                   collapse = "")
      expect_match(txt, "blockr.pharma::dm_label_subjects", fixed = TRUE)
      result <- eval(session$returned$expr(), list(data = subject_dm()))
      expect_identical(display_of(result$ae$USUBJID), "SUBJID")
      expect_identical(as.vector(result$adsl$Group), result$adsl$TRT)
    }
  )
})
