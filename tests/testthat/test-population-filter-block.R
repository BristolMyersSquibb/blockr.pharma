make_study_dm <- function() {
  adsl <- data.frame(
    USUBJID = c("a", "b", "c"),
    TRT = c("Placebo", "Active", "Active"),
    SEX = c("F", "M", "F"),
    stringsAsFactors = FALSE
  )
  ae <- data.frame(
    USUBJID = c("a", "b", "b"),
    AEDECOD = c("Headache", "Nausea", "Rash"),
    AESEV = c("MILD", "SEVERE", "MILD"),
    stringsAsFactors = FALSE
  )
  dm::dm_add_fk(
    dm::dm_add_pk(dm::dm(adsl = adsl, ae = ae), adsl, USUBJID),
    ae, USUBJID, adsl
  )
}

test_that("dm_stamp_group copies the pick onto the parent table only", {
  out <- dm_stamp_group(make_study_dm(), "TRT")

  expect_true("Group" %in% colnames(out$adsl))
  expect_equal(as.vector(out$adsl$Group), out$adsl$TRT)
  # Not the event table: a Group on both sides makes dm_flatten_to_tbl() rename
  # the pair to Group.adsl / Group.ae, and every chart binding Group loses it.
  expect_false("Group" %in% colnames(out$ae))
  # Keys survive, so downstream cascading still works.
  expect_equal(nrow(dm::dm_get_all_pks(out)), 1L)
  expect_equal(nrow(dm::dm_get_all_fks(out)), 1L)
})

test_that("the copy remembers the column it came from", {
  # Composer tables and drill claims have to report the real study variable,
  # not "Group"; they read it back off this attribute.
  out <- dm_stamp_group(make_study_dm(), "TRT")
  expect_equal(attr(out$adsl$Group, "blockr_source"), "TRT")

  df <- data.frame(TRT = c("Placebo", "Active"), stringsAsFactors = FALSE)
  expect_equal(attr(dm_stamp_group(df, "TRT")$Group, "blockr_source"), "TRT")
})

test_that("dm_stamp_group is a no-op for an absent or empty column", {
  d <- make_study_dm()
  expect_equal(colnames(dm_stamp_group(d, "NOSUCHCOL")$adsl), colnames(d$adsl))
  expect_equal(colnames(dm_stamp_group(d, NULL)$adsl), colnames(d$adsl))
  expect_equal(colnames(dm_stamp_group(d, "")$adsl), colnames(d$adsl))
})

test_that("dm_stamp_group handles a plain data frame", {
  df <- data.frame(TRT = c("Placebo", "Active"), stringsAsFactors = FALSE)
  expect_equal(as.vector(dm_stamp_group(df, "TRT")$Group), df$TRT)
  expect_equal(as.vector(dm_stamp_group(df, "TRT", as = "Split")$Split), df$TRT)
})

test_that("the stamp survives a filter and keeps column marks", {
  # The kinds attribute is stamped upstream and read by the chart bands; a
  # mutate on a zoomed table must not drop it.
  d <- make_study_dm()
  tbls <- dm::dm_get_tables(d)
  attr(tbls$adsl, "blockr_kinds") <- c(TRT = "group")
  d <- do.call(dm::dm_mutate_tbl, c(list(d), tbls))

  out <- dm_stamp_group(dm::dm_filter(d, adsl = SEX == "F"), "TRT")
  expect_equal(attr(out$adsl, "blockr_kinds"), c(TRT = "group"))
  expect_equal(nrow(out$adsl), 2L)
})

test_that("new_population_filter_block is a crossfilter block with a subclass", {
  blk <- new_population_filter_block(
    featured = c("TRT", "SEX"),
    pinned = "TRT"
  )
  expect_s3_class(blk, "population_filter_block")
  expect_s3_class(blk, "crossfilter_block")

  state <- blockr.core:::initial_block_state(blk)
  expect_equal(state$featured, c("TRT", "SEX"))
  expect_equal(state$pinned, "TRT")
})

test_that("the block stamps Group into its expression", {
  blk <- new_population_filter_block(
    featured = c("TRT", "SEX"),
    pinned = "TRT"
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() make_study_dm())),
    {
      session$flushReact()
      txt <- paste(
        deparse(session$returned$expr(), width.cutoff = 500L),
        collapse = ""
      )
      expect_match(txt, "blockr.pharma::dm_stamp_group", fixed = TRUE)

      result <- eval(session$returned$expr(), list(data = make_study_dm()))
      expect_equal(as.vector(result$adsl$Group), result$adsl$TRT)

      # Moving the pin re-points the stamp: this is what re-splits the board.
      session$setInputs(`expr-set_pinned` = "SEX")
      session$flushReact()
      result <- eval(session$returned$expr(), list(data = make_study_dm()))
      expect_equal(as.vector(result$adsl$Group), result$adsl$SEX)
    }
  )
})

test_that("the stamp wraps the filter rather than replacing it", {
  blk <- new_population_filter_block(
    featured = "TRT",
    pinned = "TRT",
    filters = list(adsl = list(SEX = "F"))
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() make_study_dm())),
    {
      session$flushReact()
      result <- eval(session$returned$expr(), list(data = make_study_dm()))
      expect_equal(result$adsl$USUBJID, c("a", "c"))
      expect_equal(as.vector(result$adsl$Group), c("Placebo", "Active"))
    }
  )
})

test_that("stamp_as = NULL leaves the crossfilter expression alone", {
  blk <- new_population_filter_block(
    featured = "TRT",
    pinned = "TRT",
    stamp_as = NULL
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() make_study_dm())),
    {
      session$flushReact()
      txt <- paste(
        deparse(session$returned$expr(), width.cutoff = 500L),
        collapse = ""
      )
      expect_no_match(txt, "dm_stamp_group", fixed = TRUE)
    }
  )
})
