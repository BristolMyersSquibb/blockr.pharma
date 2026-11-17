# The dm form of the population join: one events table full-joined to the
# dm's subject-level table, so a board needs no pull or flatten in front of
# it. The two-data-frame form is covered in test-population-join-block.R and
# must not change.

dm_adsl <- data.frame(
  USUBJID = c("S1", "S2", "S3"),
  TRT01A = c("A", "B", "B"),
  SAFFL = "Y",
  stringsAsFactors = FALSE
)
attr(dm_adsl$TRT01A, "label") <- "Actual Treatment for Period 01"
dm_adae <- data.frame(
  USUBJID = c("S1", "S1", "S2"),
  TRT01A = c("A", "A", "B"),
  AEDECOD = c("Diarrhoea", "Nausea", "Rash"),
  stringsAsFactors = FALSE
)
attr(dm_adae$AEDECOD, "label") <- "Dictionary-Derived Term"
dm_adcm <- data.frame(USUBJID = "S3", CMDECOD = "Paracetamol",
                      stringsAsFactors = FALSE)

pj_dm <- function() {
  dm::dm(adsl = dm_adsl, adae = dm_adae, adcm = dm_adcm) |>
    dm::dm_add_pk(adsl, USUBJID) |>
    dm::dm_add_fk(adae, USUBJID, adsl) |>
    dm::dm_add_fk(adcm, USUBJID, adsl)
}

# Runs the block server, makes the given picks, and returns what it ended on.
pj_run <- function(blk, data, population = NULL, picks = list()) {
  # An unconnected input reaches the server as a reactive returning NULL, as
  # blockr.core's upstream_result() hands it on a board.
  args <- list(data = function() data, population = function() population)
  out <- NULL
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      for (p in picks) {
        do.call(session$makeScope("expr")$setInputs, p)
        session$flushReact()
      }
      out <<- list(
        result = session$returned$result(),
        expr = paste(deparse(session$returned$expr()), collapse = " "),
        id = session$returned$state$id(),
        table = session$returned$state$table()
      )
    },
    args = list(x = blk, data = args)
  )
  out
}

# --- join_population_dm() -----------------------------------------------------

test_that("the dm form equals join_population() on the two tables", {
  d <- pj_dm()
  expect_equal(join_population_dm(d, "adae"),
               join_population(dm_adae, dm_adsl))
})

test_that("N counts subjects without an event", {
  out <- join_population_dm(pj_dm(), "adae")
  expect_equal(length(unique(out$USUBJID)), 3L)
  expect_true(is.na(out$AEDECOD[out$USUBJID == "S3"]))
  # A column both tables carry is kept once and filled on every row.
  expect_false(any(grepl("\\.(adae|adsl|x|y)$", names(out))))
  expect_equal(out$TRT01A[out$USUBJID == "S3"], "B")
})

test_that("labels are kept from both tables", {
  out <- join_population_dm(pj_dm(), "adae")
  expect_equal(attr(out$AEDECOD, "label"), "Dictionary-Derived Term")
  expect_equal(attr(out$TRT01A, "label"), "Actual Treatment for Period 01")
})

test_that("defaults: adae, adsl, and the identifier from the foreign key", {
  d <- pj_dm()
  expect_equal(join_population_dm(d), join_population_dm(d, "adae"))
  expect_equal(dm_population_table(d), "adsl")
  expect_equal(dm_events_default(d, "adsl"), "adae")
  expect_equal(dm_population_id(d, "adae", "adsl", dm_adae, dm_adsl),
               "USUBJID")
})

test_that("another events table works the same way", {
  out <- join_population_dm(pj_dm(), "adcm")
  expect_equal(sort(unique(out$USUBJID)), c("S1", "S2", "S3"))
  expect_equal(sum(!is.na(out$CMDECOD)), 1L)
})

test_that("a dm without adsl finds the subject table by foreign key", {
  d <- dm::dm(sl = dm_adsl, ae = dm_adae) |>
    dm::dm_add_pk(sl, USUBJID) |>
    dm::dm_add_fk(ae, USUBJID, sl)
  expect_equal(dm_population_table(d), "sl")
  expect_equal(join_population_dm(d, "ae"),
               join_population(dm_adae, dm_adsl))
})

test_that("a missing table is refused, saying which", {
  expect_error(join_population_dm(pj_dm(), "adlb"), "adlb")
  expect_error(join_population_dm(dm::dm(adae = dm_adae)), "adsl")
  expect_error(join_population_dm(dm_adae, "adae"), "must be a dm")
})

test_that("the dm's filter trail and provenance reach the result", {
  d <- blockr.dm::add_filter_trail(pj_dm(), "pop", "SAFFL == \"Y\"",
                                   tables = "adsl")
  ae <- dm::pull_tbl(d, adae)
  attr(ae, "blockr_provenance") <- list(name = "demo")
  d <- dm::dm_mutate_tbl(d, adae = ae)
  out <- join_population_dm(d, "adae")
  expect_equal(blockr.dm::filter_trail(out)[["pop"]], "SAFFL == \"Y\"")
  expect_equal(attr(out, "blockr_provenance")$name, "demo")
})

test_that("on real data the dm form gives the same frame as two pulls", {
  skip_if_not_installed("pharmaverseadam")
  adsl <- pharmaverseadam::adsl
  adae <- pharmaverseadam::adae
  d <- dm::dm(adsl, adae) |>
    dm::dm_add_pk(adsl, USUBJID) |>
    dm::dm_add_fk(adae, USUBJID, adsl)
  out <- join_population_dm(d, "adae")
  expect_equal(out, join_population(adae, adsl))
  expect_equal(length(unique(out$USUBJID)), nrow(adsl))
})

# --- the block ----------------------------------------------------------------

test_that("a dm in gives the joined frame, without a population input", {
  out <- pj_run(new_population_join_block(), pj_dm())
  expect_equal(nrow(out$result), 4L)
  expect_true("S3" %in% out$result$USUBJID)
  expect_equal(out$table, "adae")
  expect_match(out$expr, "join_population_dm(.(data), table = \"adae\")",
               fixed = TRUE)
})

test_that("the events table can be picked, and a restored one is kept", {
  out <- pj_run(new_population_join_block(), pj_dm(),
                picks = list(list(table = "adcm")))
  expect_equal(out$table, "adcm")
  expect_equal(sum(!is.na(out$result$CMDECOD)), 1L)
  out <- pj_run(new_population_join_block(table = "adcm"), pj_dm())
  expect_equal(out$table, "adcm")
})

test_that("a table the dm does not offer is not taken for a pick", {
  out <- pj_run(new_population_join_block(), pj_dm(),
                picks = list(list(table = ""), list(table = "adsl")))
  expect_equal(out$table, "adae")
})

test_that("a dm plus a connected population is refused", {
  blk <- new_population_join_block()
  expect_error(
    blockr.core::validate_data_inputs(
      blk, list(data = pj_dm(), population = dm_adsl)
    ),
    "Disconnect"
  )
  expect_null(
    blockr.core::validate_data_inputs(blk, list(data = pj_dm()))
  )
})

test_that("the population input is optional, the data input is not", {
  blk <- new_population_join_block()
  expect_equal(blockr.core:::block_optional_inputs(blk), "population")
  expect_setequal(blockr.core::block_inputs(blk), c("data", "population"))
})

test_that("two data frames still emit the old call and leave table unset", {
  out <- pj_run(new_population_join_block(), dm_adae, dm_adsl)
  expect_match(out$expr, "blockr.pharma::join_population(.(data), .(population)",
               fixed = TRUE)
  expect_equal(out$table, "")
  expect_equal(out$result, join_population(dm_adae, dm_adsl))
})

test_that("a saved block round-trips, with and without a table", {
  for (blk in list(new_population_join_block(table = "adcm"),
                   new_population_join_block(id = "SUBJ"))) {
    ser <- blockr.core::blockr_ser(blk)
    back <- blockr.core::blockr_deser(ser)
    expect_s3_class(back, "population_join_block")
    expect_identical(blockr.core::blockr_ser(back)$payload, ser$payload)
  }
  # A board saved before `table` existed restores into frame mode.
  old <- blockr.core::blockr_ser(new_population_join_block())
  old$payload$table <- NULL
  out <- pj_run(blockr.core::blockr_deser(old), dm_adae, dm_adsl)
  expect_equal(out$id, "USUBJID")
  expect_equal(nrow(out$result), 4L)
  # A restored dm block keeps its table on a dm.
  ser <- blockr.core::blockr_ser(new_population_join_block(table = "adcm"))
  out <- pj_run(blockr.core::blockr_deser(ser), pj_dm())
  expect_equal(out$table, "adcm")
})

test_that("on a board, a dm link alone runs the block", {
  # The population input is left unconnected, which only a board exercises:
  # blockr.core has to treat it as optional and hand the server a NULL.
  board <- blockr.core::new_board(
    blocks = c(src = blockr.core::new_static_block(pj_dm()),
               pj = new_population_join_block()),
    links = blockr.core::new_link("src", "pj", "data")
  )
  shiny::testServer(
    blockr.core:::get_s3_method("board_server", board),
    {
      session$flushReact()
      res <- rv$blocks$pj$server$result()
      expect_equal(res, join_population(dm_adae, dm_adsl))
    },
    args = list(x = board)
  )
})
