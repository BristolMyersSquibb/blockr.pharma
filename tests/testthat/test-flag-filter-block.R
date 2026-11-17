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

# The filtering call inside the filter-trail wrapper. A block that selects
# something wraps its expression in `blockr.dm::add_filter_trail()` so the
# result records what was filtered; these tests are about the filtering, so
# they look through the wrapper. See `blockr.dm::filter_trail()`.
flag_inner <- function(e) {
  if (is.call(e) && identical(e[[1L]], quote(blockr.dm::add_filter_trail))) {
    return(e[[2L]])
  }
  e
}

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
  expect_false(grepl("TRTEMFL", deparse1(e), fixed = TRUE))
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
  expect_equal(deparse1(flag_inner(e)),
               'dplyr::filter(.(data), TRT %in% c("Y", "y"))')
  expect_equal(nrow(ev(e, d)), 0L)
})

test_that("a flag no longer in the data is dropped from the expression", {
  d <- flag_df()
  e <- make_flag_filter_expr(c("PREFL", "GONE"), flag_input_shape(d))
  expect_identical(flag_inner(e), quote(dplyr::filter(.(data), PREFL %in% TRUE)))
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
  expect_match(deparse1(flag_inner(make_flag_filter_expr("TRTEMFL", shape))),
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

test_that("the block rejects only inputs no mode can read", {
  # The table is picked at runtime, so dat_valid cannot know the mode. The
  # table checks run in the server, against the live table.
  for (blk in list(new_flag_filter_block(),
                   new_flag_filter_block(table = "adae"))) {
    expect_silent(blk[["dat_valid"]](flag_df()))
    expect_silent(blk[["dat_valid"]](flag_dm()))
    expect_error(blk[["dat_valid"]](1:3), "data frame or a dm")
  }
  expect_silent(flag_check_input(flag_df(), NULL))
  expect_silent(flag_check_input(flag_dm(), "adae"))
  expect_error(flag_check_input(flag_dm(), "advs"), "no table \"advs\"")
  expect_error(flag_check_input(dm::dm_select_tbl(flag_dm(), adsl), NULL),
               "no table with Y/N flag")
})

test_that("the preview follows the result, not the constructor", {
  # A dm cannot go through the table preview -- nrow(<dm>) is NULL and the
  # preview's slice arithmetic ends in `if (NA)`. The table is picked at
  # runtime, so the class cannot settle it: block_output dispatches on the
  # result, and the container is a uiOutput in both modes.
  expect_false(inherits(new_flag_filter_block(table = "adae"), "dm_block"))
  blk <- new_flag_filter_block()
  ui <- as.character(blockr.core::block_ui("b", blk))
  expect_match(ui, "shiny-html-output")
  expect_true(is.function(
    utils::getS3method("block_output", "dm_block",
                       envir = asNamespace("blockr.dm"))
  ))

  shiny::testServer(function(id) {
    shiny::moduleServer(id, function(input, output, session) {})
  }, {
    out_dm <- blockr.core::block_output(blk, flag_dm(), session)
    expect_s3_class(out_dm, "shiny.render.function")
    out_df <- blockr.core::block_output(blk, flag_df(), session)
    expect_s3_class(out_df, "shiny.render.function")
  })
})

# --- Picking the table at runtime --------------------------------------------

period_dm <- function() {
  dm::dm(
    adsl = data.frame(USUBJID = c("A", "B", "C"), SAFFL = c("Y", "Y", "N"),
                      AGE = c(61, 74, 55), stringsAsFactors = FALSE),
    adae = data.frame(USUBJID = c("A", "A", "B", "B"),
                      PREFL   = c("Y", "", "", ""),
                      TRTEMFL = c("", "Y", "Y", ""),
                      FUPFL   = c("", "", "", "Y"),
                      AEDECOD = c("Headache", "Rash", "Nausea", "Cough"),
                      stringsAsFactors = FALSE),
    adcm = data.frame(USUBJID = "A", ONTRTFL = "Y", CMDECOD = "Aspirin",
                      stringsAsFactors = FALSE),
    advs = data.frame(USUBJID = "A", AVAL = 120, PARAMCD = "SYSBP",
                      stringsAsFactors = FALSE)
  ) |>
    dm::dm_add_pk(adsl, USUBJID) |>
    dm::dm_add_fk(adae, USUBJID, adsl) |>
    dm::dm_add_fk(adcm, USUBJID, adsl)
}

# The block's server, flushed, with the state and the evaluated result.
flag_live <- function(blk, data, code = NULL) {
  input <- data
  res <- NULL
  shiny::testServer(
    blockr.core:::block_expr_server(blk),
    args = list(data = shiny::reactive(input)),
    {
      session$flushReact()
      if (!is.null(code)) code(session)
      session$flushReact()
      ret <- session$getReturned()
      st <- lapply(ret$state, function(x) shiny::isolate(x()))
      e <- shiny::isolate(ret$expr())
      res <<- list(state = st, expr = e)
    }
  )
  # Outside testServer, whose mask binds `data` to the reactive.
  res$out <- flag_eval(res$expr, input)
  res
}

test_that("a flag column is logical or holds only Y/N/U", {
  expect_true(flag_is_flag_col(c(TRUE, NA)))
  expect_true(flag_is_flag_col(c("Y", "", NA)))
  expect_true(flag_is_flag_col(c("N", "n", "U")))
  expect_true(flag_is_flag_col(factor(c("Y", "N"))))
  expect_false(flag_is_flag_col(c("Y", "Headache")))
  expect_false(flag_is_flag_col(c("", NA)))
  expect_false(flag_is_flag_col(1:3))
  # a value past the first rows still disqualifies
  expect_false(flag_is_flag_col(c(rep("Y", 2000), "Rash")))
  expect_identical(flag_flag_columns(flag_df()),
                   c("PREFL", "TRTEMFL", "FUPFL", "AESER"))
})

test_that("the Table select offers the tables with flags, adae first pick", {
  d <- period_dm()
  expect_identical(flag_tables(d), c("adsl", "adae", "adcm"))
  expect_identical(flag_default_table(d), "adae")
  # without adae, the first table with a flag
  expect_identical(flag_default_table(dm::dm_select_tbl(d, -adae)), "adsl")
  expect_null(flag_default_table(dm::dm_select_tbl(d, advs)))
  # a saved table without flags stays listed, so the select can show it
  expect_identical(flag_tables(d, "advs"), c("advs", "adsl", "adae", "adcm"))
})

test_that("handed a dm without a table, the block settles on adae", {
  res <- flag_live(new_flag_filter_block(), period_dm())
  expect_identical(res$state$table, "adae")
  expect_identical(res$state$columns, c("PREFL", "TRTEMFL", "FUPFL"))
  expect_identical(res$state$selected, character())
  expect_s3_class(res$out, "dm")
})

test_that("dm in with a table narrows that table only", {
  d <- period_dm()
  res <- flag_live(
    new_flag_filter_block(columns = c("PREFL", "TRTEMFL", "FUPFL"),
                          selected = "TRTEMFL", table = "adae"),
    d
  )
  tbls <- dm::dm_get_tables(res$out)
  expect_identical(nrow(tbls$adae), 2L)
  for (t in c("adsl", "adcm", "advs")) {
    expect_identical(tbls[[t]], dm::dm_get_tables(d)[[t]])
  }
  expect_identical(dm::dm_get_all_pks(res$out), dm::dm_get_all_pks(d))
  expect_identical(dm::dm_get_all_fks(res$out), dm::dm_get_all_fks(d))
  expect_length(blockr.dm::filter_trail(res$out, tables = "adsl"), 0L)
  expect_length(blockr.dm::filter_trail(res$out, tables = "adae"), 1L)
})

test_that("frame in is unchanged: no table, the frame filtered", {
  res <- flag_live(
    new_flag_filter_block(columns = c("PREFL", "TRTEMFL"),
                          selected = "TRTEMFL"),
    flag_df()
  )
  expect_null(res$state$table)
  expect_identical(res$state$columns, c("PREFL", "TRTEMFL"))
  expect_identical(res$out$USUBJID, c("2", "3"))
})

test_that("handed a frame after a dm, the block drops the table", {
  res <- flag_live(
    new_flag_filter_block(columns = "TRTEMFL", selected = "TRTEMFL",
                          table = "adae"),
    flag_df()
  )
  expect_null(res$state$table)
  expect_identical(res$state$selected, "TRTEMFL")
  expect_identical(res$out$USUBJID, c("2", "3"))
})

test_that("picking another table clears the ticks and lists its flags", {
  res <- flag_live(
    new_flag_filter_block(columns = c("PREFL", "TRTEMFL", "FUPFL"),
                          selected = "TRTEMFL", table = "adae"),
    period_dm(),
    function(session) {
      session$setInputs(flags = list(
        columns = list("PREFL", "TRTEMFL", "FUPFL"),
        selected = list(), footnotes = list(), table = "adcm"
      ))
    }
  )
  expect_identical(res$state$table, "adcm")
  expect_identical(res$state$columns, "ONTRTFL")
  expect_identical(res$state$selected, character())
  # nothing ticked: every table as it came in
  expect_identical(nrow(dm::dm_get_tables(res$out)$adae), 4L)
})

test_that("the table survives a save, and a code board restores on it", {
  d <- period_dm()
  rt <- board_round_trip(
    new_flag_filter_block(columns = c("PREFL", "TRTEMFL", "FUPFL"),
                          selected = "TRTEMFL", table = "adae"),
    d
  )
  expect_identical(rt$saved$table, "adae")
  expect_identical(rt$restored$table, "adae")
  expect_identical(rt$restored$selected, "TRTEMFL")

  # A block added from the UI saves the table it settled on.
  rt2 <- board_round_trip(new_flag_filter_block(), d)
  expect_identical(rt2$saved$table, "adae")
  expect_identical(rt2$restored$table, "adae")

  # And a frame block saves no table.
  rt3 <- board_round_trip(new_flag_filter_block(columns = "TRTEMFL"),
                          flag_df())
  expect_null(rt3$restored$table)
})

test_that("the block records what it filtered, and nothing when it filters nothing", {
  shape <- flag_df()[0L, , drop = FALSE]

  # Frame mode with nothing ticked is left alone: no clause to add, and
  # dplyr::filter() carries an incoming trail through on its own.
  expect_identical(
    make_flag_filter_expr(character(), shape),
    quote(dplyr::filter(.(data), TRUE))
  )

  e <- make_flag_filter_expr(c("TRTEMFL", "FUPFL"), shape, NULL, "ae_flags")
  out <- ev(e, flag_df())
  expect_identical(
    blockr.dm::filter_trail(out),
    c(ae_flags = "TRTEMFL or FUPFL")
  )

  # An incoming trail is preserved, and this block's own clause is keyed by
  # block id, so re-evaluating replaces it rather than appending a second copy.
  d <- blockr.dm::add_filter_trail(flag_df(), "global_filter", "SEX = F")
  out <- ev(e, d)
  expect_identical(
    blockr.dm::filter_trail(out),
    c(global_filter = "SEX = F", ae_flags = "TRTEMFL or FUPFL")
  )
})

test_that("in dm mode the clause goes on the table it narrowed only", {
  d <- flag_dm()
  shape <- flag_target_df(d, "adae")[0L, , drop = FALSE]
  out <- flag_eval(
    make_flag_filter_expr("TRTEMFL", shape, "adae", "ae_flags"),
    d
  )

  expect_identical(blockr.dm::filter_trail(out), c(ae_flags = "TRTEMFL"))
  expect_identical(blockr.dm::filter_trail(out, tables = "adae"),
                   c(ae_flags = "TRTEMFL"))

  # What a pull block gets on each branch below this filter: adsl alone
  # does not carry the clause, adae does. The block narrowed adae and
  # nothing else, so a demographics table must not caption itself "TRTEMFL".
  expect_null(blockr.dm::filter_trail(dm::pull_tbl(out, adsl)))
  expect_identical(blockr.dm::filter_trail(dm::pull_tbl(out, adae)),
                   c(ae_flags = "TRTEMFL"))

  # The expression names the table.
  e <- make_flag_filter_expr("TRTEMFL", shape, "adae", "ae_flags")
  expect_identical(e[["tables"]], "adae")

  # Frame mode: the clause goes on the frame.
  fr <- ev(
    make_flag_filter_expr("TRTEMFL", flag_df()[0L, , drop = FALSE], NULL,
                          "ae_flags"),
    flag_df()
  )
  expect_identical(blockr.dm::filter_trail(fr), c(ae_flags = "TRTEMFL"))
})

test_that("the clause uses the footnote text, then the label, then the name", {
  d <- flag_df()
  attr(d$TRTEMFL, "label") <- "Treatment Emergent Analysis Flag"
  attr(d$FUPFL, "label") <- "Follow-Up Analysis Flag"

  # The live block builds its shape from the full frame, and base `[` drops
  # plain-vector attributes, so the shape has to carry the label itself.
  shape <- flag_input_shape(d)
  expect_identical(attr(shape$TRTEMFL, "label"),
                   "Treatment Emergent Analysis Flag")

  notes <- list(TRTEMFL = "Treatment-emergent (AEs on or after the first dose)")
  out <- ev(make_flag_filter_expr(c("TRTEMFL", "FUPFL", "PREFL"), shape, NULL,
                                  "ae_flags", notes), d)
  expect_identical(
    blockr.dm::filter_trail(out),
    c(ae_flags = paste("Treatment-emergent (AEs on or after the first dose)",
                       "or Follow-Up Analysis Flag or PREFL"))
  )

  # A cleared field falls back to the label.
  expect_identical(
    flag_filter_clause("TRTEMFL", shape, list(TRTEMFL = "  ")),
    "Treatment Emergent Analysis Flag"
  )
})

test_that("footnotes are state that survives a save", {
  blk <- new_flag_filter_block(
    columns = c("PREFL", "TRTEMFL"),
    footnotes = c(TRTEMFL = "Treatment-emergent", PREFL = "")
  )
  ser <- blockr.core::blockr_ser(blk)
  back <- jsonlite::fromJSON(jsonlite::toJSON(ser, null = "null"),
                             simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  blk2 <- blockr.core::blockr_deser(back)
  expect_identical(
    flag_clean_footnotes(blockr.core::blockr_ser(blk2)$payload$footnotes),
    list(TRTEMFL = "Treatment-emergent")
  )
})
