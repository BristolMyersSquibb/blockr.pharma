# The block around join_population(): two data inputs, the identifier picker,
# and the emitted expression. The maths is covered in test-population-join.R;
# what is guarded here is the block contract.

pj_state <- function(blk, field, ev, pop) {
  out <- NULL
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      out <<- session$returned$state[[field]]()
    },
    args = list(x = blk,
                data = list(data = function() ev,
                            population = function() pop))
  )
  out
}

ev <- data.frame(USUBJID = c("S1", "S2"), AEDECOD = c("D", "N"),
                 stringsAsFactors = FALSE)
pop <- data.frame(USUBJID = c("S1", "S2", "S3"), TRT01A = "A",
                  stringsAsFactors = FALSE)

test_that("the identifier defaults to a column both inputs share", {
  expect_equal(pj_state(new_population_join_block(), "id", ev, pop), "USUBJID")
})

test_that("the block evaluates to the joined frame", {
  blk <- new_population_join_block()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      res <- session$returned$result()
      expect_equal(nrow(res), 3L)
      expect_true("S3" %in% res$USUBJID)
      expect_true(is.na(res$AEDECOD[res$USUBJID == "S3"]))
    },
    args = list(x = blk, data = list(data = function() ev,
                                     population = function() pop))
  )
})

test_that("the emitted call carries BOTH inputs as slots", {
  # A bare `population` symbol works in the app and dies in exported code --
  # the trap data_slot() exists for. So the expression must name both.
  blk <- new_population_join_block()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      txt <- paste(deparse(session$returned$expr()), collapse = " ")
      expect_match(txt, "join_population", fixed = TRUE)
      expect_match(txt, ".(data)", fixed = TRUE)
      expect_match(txt, ".(population)", fixed = TRUE)
    },
    args = list(x = blk, data = list(data = function() ev,
                                     population = function() pop))
  )
})

test_that("no shared identifier passes the events through, and does not error", {
  # An unconfigured block returns its input rather than erroring
  # (blockr.docs design-system/ux-principles.md).
  odd <- data.frame(SUBJ = "S1", TRT01A = "A", stringsAsFactors = FALSE)
  blk <- new_population_join_block()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(nrow(session$returned$result()), nrow(ev))
    },
    args = list(x = blk, data = list(data = function() ev,
                                     population = function() odd))
  )
})

test_that("a non-frame input is refused, saying which one", {
  # dat_valid names the offending side, because "must be a data frame" on a
  # two-input block leaves you guessing which link is wrong.
  blk <- new_population_join_block()
  expect_error(
    blockr.core::validate_data_inputs(blk, list(data = 1, population = pop)),
    "events"
  )
  expect_error(
    blockr.core::validate_data_inputs(blk, list(data = ev, population = 1)),
    "subject-level"
  )
})
