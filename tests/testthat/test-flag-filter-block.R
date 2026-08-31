flag_df <- function() {
  d <- data.frame(
    USUBJID = c("1", "2", "3", "4"),
    PREFL   = c(TRUE, FALSE, FALSE, FALSE),
    TRTEMFL = c(FALSE, TRUE, TRUE, FALSE),
    FUPFL   = c(FALSE, FALSE, TRUE, FALSE),
    AESER   = c("Y", "N", "", NA),
    TRT     = c("Placebo", "Active", "Active", "Placebo"),
    stringsAsFactors = FALSE
  )
  attr(d$AESER, "label") <- "Serious Event"
  d
}

ev <- function(e, d) eval(e, list(data = d, . = identity))

test_that("no ticked flag passes everything through", {
  e <- make_flag_filter_expr(character(), flag_input_shape(flag_df()))
  expect_identical(e, quote(dplyr::filter(.(data), TRUE)))
  expect_equal(nrow(ev(e, flag_df())), 4L)
})

test_that("unticking is never a negative", {
  # The whole reason this block exists: with N / U / "" / NA all meaning
  # different flavours of not-flagged, an unticked box must add nothing.
  d <- flag_df()
  e <- make_flag_filter_expr("PREFL", flag_input_shape(d))
  expect_equal(nrow(ev(e, d)), 1L)
  # TRTEMFL is simply absent from the expression, not negated.
  expect_false(grepl("TRTEMFL", deparse(e), fixed = TRUE))
})

test_that("ticked flags union, and a row with two flags counts once", {
  d <- flag_df()
  e <- make_flag_filter_expr(c("TRTEMFL", "FUPFL"), flag_input_shape(d))
  # Row 3 carries both; the union must not double it.
  expect_equal(nrow(ev(e, d)), 2L)
  e2 <- make_flag_filter_expr(c("PREFL", "TRTEMFL"), flag_input_shape(d))
  expect_equal(nrow(ev(e2, d)), 3L)
})

test_that("mutually exclusive flags would be empty under AND", {
  # Why the block unions rather than offering a combinator.
  d <- flag_df()
  expect_equal(sum(d$PREFL & d$TRTEMFL), 0L)
  e <- make_flag_filter_expr(c("PREFL", "TRTEMFL"), flag_input_shape(d))
  expect_equal(nrow(ev(e, d)), 3L)
})

test_that("the emitted condition keys on TYPE, not on values", {
  d <- flag_df()
  shape <- flag_input_shape(d)
  expect_identical(flag_condition_expr("PREFL", shape), quote(PREFL %in% TRUE))
  # The affirmative set is spliced in as a VALUE, so it deparses as
  # `c("Y", "y")` in exported code but is not a `c()` call in the AST.
  expect_identical(flag_condition_expr("AESER", shape),
                   bquote(AESER %in% .(c("Y", "y"))))
  # A 0-row frame must give the SAME answer, or the filter would change
  # meaning when an upstream filter empties the table.
  empty <- flag_input_shape(d[0L, , drop = FALSE])
  expect_identical(flag_condition_expr("PREFL", empty), quote(PREFL %in% TRUE))
  expect_identical(flag_condition_expr("AESER", empty),
                   bquote(AESER %in% .(c("Y", "y"))))
})

test_that("a Y/N text flag filters like a logical one", {
  d <- flag_df()
  e <- make_flag_filter_expr("AESER", flag_input_shape(d))
  expect_equal(nrow(ev(e, d)), 1L)
})

test_that("pointing it at a non-flag column yields no rows, not an error", {
  # Decision 1a: the picker offers every column and does not guard. Nonsense
  # is visible as a zero count plus the expression, not as a crash.
  d <- flag_df()
  e <- make_flag_filter_expr("TRT", flag_input_shape(d))
  expect_equal(deparse(e), 'dplyr::filter(.(data), TRT %in% c("Y", "y"))')
  expect_equal(nrow(ev(e, d)), 0L)
})

test_that("a flag no longer in the data is dropped from the expression", {
  d <- flag_df()
  e <- make_flag_filter_expr(c("PREFL", "GONE"), flag_input_shape(d))
  expect_identical(e, quote(dplyr::filter(.(data), PREFL %in% TRUE)))
})

test_that("column metadata carries the label and the would-keep count", {
  meta <- flag_column_meta(flag_df(), c("PREFL", "AESER", "TRT"))
  by <- stats::setNames(meta, vapply(meta, function(m) m$name, ""))
  expect_equal(by$PREFL$count, 1L)
  expect_equal(by$PREFL$total, 4L)
  expect_equal(by$AESER$label, "Serious Event")
  expect_equal(by$AESER$count, 1L)
  # A non-flag column reports zero, which is what puts 0 on screen.
  expect_equal(by$TRT$count, 0L)
})

test_that("the picker offers every column, unfiltered", {
  # Decision 1a again: filtering to flag-shaped columns would need values,
  # and the picker would go empty whenever the upstream did.
  expect_equal(flag_all_columns(flag_df()), names(flag_df()))
  expect_equal(flag_all_columns(flag_df()[0L, , drop = FALSE]),
               names(flag_df()))
})

test_that("the constructor keeps `selected` inside `columns`", {
  blk <- new_flag_filter_block(
    columns = c("PREFL", "TRTEMFL"), selected = c("TRTEMFL", "NOPE")
  )
  st <- blockr.core::blockr_ser(blk)$payload
  expect_equal(unlist(st$columns), c("PREFL", "TRTEMFL"))
  expect_equal(unlist(st$selected), "TRTEMFL")
})

test_that("the block round-trips through board JSON", {
  blk <- new_flag_filter_block(
    columns = c("PREFL", "TRTEMFL", "FUPFL"), selected = "TRTEMFL"
  )
  ser <- blockr.core::blockr_ser(blk)
  back <- jsonlite::fromJSON(jsonlite::toJSON(ser, null = "null"),
                             simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  blk2 <- blockr.core::blockr_deser(back)
  expect_s3_class(blk2, "flag_filter_block")
})

test_that("the matched count is a union, not a sum", {
  d <- flag_df()
  # Row 3 carries TRTEMFL and FUPFL. Summing gives 3, the union gives 2.
  expect_equal(sum(d$TRTEMFL) + sum(d$FUPFL), 3L)
  expect_equal(flag_matched_rows(d, c("TRTEMFL", "FUPFL")), 2L)
  # And it can never exceed the table, which the client-side sum could.
  expect_lte(flag_matched_rows(d, names(d)), nrow(d))
})

test_that("no selection matches every row, like the pass-through expr", {
  d <- flag_df()
  expect_equal(flag_matched_rows(d, character()), nrow(d))
  expect_equal(flag_matched_rows(d, "GONE"), nrow(d))
  expect_null(flag_matched_rows(NULL, "PREFL"))
})

test_that("the picker's choices carry the column labels", {
  ch <- flag_choice_meta(flag_df())
  expect_length(ch, 6L)
  expect_identical(vapply(ch, `[[`, character(1), "value"), names(flag_df()))
  labs <- vapply(ch, `[[`, character(1), "label")
  expect_identical(labs[[5L]], "Serious Event")
  # A column without a label attribute renders as the bare name.
  expect_identical(labs[[1L]], "")
  expect_identical(flag_choice_meta(NULL), list())
})

# --- Filtering one table of a dm --------------------------------------------
# A patient profile takes a dm, so a filter that only speaks data frames can
# never reach it. What `table` must not do is reduce the COHORT: dropping the
# non-treatment-emergent AEs is the point, dropping the patients who have none
# is a bug, and in a safety review those patients are a finding.

flag_dm <- function() {
  dm::dm(
    adsl = data.frame(USUBJID = c("A", "B", "C"), AGE = c(61, 74, 55),
                      stringsAsFactors = FALSE),
    adae = data.frame(USUBJID = c("A", "A", "B"),
                      TRTEMFL = c("Y", "N", "N"),
                      AEDECOD = c("Headache", "Rash", "Nausea"),
                      stringsAsFactors = FALSE)
  ) |>
    dm::dm_add_pk(adsl, USUBJID) |>
    dm::dm_add_fk(adae, USUBJID, adsl)
}

flag_eval <- function(expr, data) {
  eval(expr, list(data = data, `.` = function(x) x))
}

test_that("a table-scoped filter reduces that table and no other", {
  d <- flag_dm()
  shape <- flag_target_df(d, "adae")[0L, , drop = FALSE]
  out <- flag_eval(make_flag_filter_expr("TRTEMFL", shape, "adae"), d)
  tbls <- dm::dm_get_tables(out)

  expect_identical(nrow(tbls$adae), 1L)
  # C has no AE rows at all and B has only a non-emergent one. Both keep
  # their ADSL row: this is the whole reason the block cannot use
  # dm::dm_filter(), whose FK cascade takes adsl from 3 rows to 1.
  expect_identical(nrow(tbls$adsl), 3L)
  expect_identical(tbls$adsl$USUBJID, c("A", "B", "C"))
})

test_that("the keys survive, which rebuilding the dm would not", {
  d <- flag_dm()
  shape <- flag_target_df(d, "adae")[0L, , drop = FALSE]
  out <- flag_eval(make_flag_filter_expr("TRTEMFL", shape, "adae"), d)

  expect_identical(nrow(dm::dm_get_all_pks(out)), 1L)
  expect_identical(nrow(dm::dm_get_all_fks(out)), 1L)
})

test_that("dm_filter is the wrong call, and this is why", {
  # Pinned as a regression: if a future refactor reaches for dm_filter()
  # this is the behaviour it would silently reintroduce.
  cascaded <- dm::dm_filter(flag_dm(), adae = TRTEMFL == "Y")
  expect_identical(nrow(dm::dm_get_tables(cascaded)$adsl), 1L)
})

test_that("frame mode is untouched by the new argument", {
  shape <- flag_df()[0L, , drop = FALSE]
  expect_identical(
    make_flag_filter_expr("TRTEMFL", shape, NULL),
    make_flag_filter_expr("TRTEMFL", shape)
  )
  expect_match(deparse1(make_flag_filter_expr("TRTEMFL", shape)),
               "^dplyr::filter")
})

test_that("no ticked box is no constraint, in dm mode too", {
  d <- flag_dm()
  shape <- flag_target_df(d, "adae")[0L, , drop = FALSE]
  out <- flag_eval(make_flag_filter_expr(character(), shape, "adae"), d)
  tbls <- dm::dm_get_tables(out)
  expect_identical(nrow(tbls$adae), 3L)
  expect_identical(nrow(tbls$adsl), 3L)
})

test_that("the table argument is normalized and bounded to one", {
  expect_null(flag_validate_table(NULL))
  expect_null(flag_validate_table(""))
  expect_null(flag_validate_table(character()))
  expect_identical(flag_validate_table("adae"), "adae")
  expect_identical(flag_validate_table(list("adae")), "adae")
  expect_error(flag_validate_table(c("adae", "adcm")), "ONE table")
})

test_that("the block reads flags from the named table, not the dm", {
  d <- flag_dm()
  # the dm itself is not a data frame; without `table` there is nothing to
  # read, with it the adae columns come through
  expect_null(flag_target_df(d, NULL))
  expect_identical(names(flag_target_df(d, "adae")),
                   c("USUBJID", "TRTEMFL", "AEDECOD"))
  # a table the dm does not carry is no metadata, not an error: the block's
  # dat_valid raises that, where the user can read it
  expect_null(flag_target_df(d, "advs"))
})

test_that("the block rejects the input that cannot answer", {
  blk <- new_flag_filter_block(columns = "TRTEMFL", table = "adae")
  expect_s3_class(blk, "flag_filter_block")
  valid <- environment(blk[["dat_valid"]])
  expect_error(blk[["dat_valid"]](flag_df()), "must be a dm")
  expect_error(blk[["dat_valid"]](flag_dm() |> dm::dm_select_tbl(adsl)),
               "no table")
  expect_silent(blk[["dat_valid"]](flag_dm()))

  # and without `table` it still wants a frame
  expect_error(new_flag_filter_block()[["dat_valid"]](flag_dm()),
               "data frame")
})
