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

# --- the identifier has to survive a save and a pick -------------------------
# Both halves were broken on the first cut. The constructor formal is `id` and
# so is the server's FIRST PARAMETER, which is the module id -- so the formal
# was shadowed the moment the closure was entered and never read. blockr.core
# restores a block by re-calling the constructor with the saved state, so a
# shadowed formal does not look like a bug, it looks like a block that quietly
# forgets its setting. The default masked it: the fallback picks the first
# shared column, which on real data IS USUBJID.
#
# Note the scope. A block's inputs live under "expr"; setting them on the
# outer session silently does nothing and every assertion here passes for the
# wrong reason.

two_ids_ev <- data.frame(SUBJ = "S1", USUBJID = "S1", AEDECOD = "D",
                         stringsAsFactors = FALSE)
two_ids_pop <- data.frame(SUBJ = c("S1", "S2"), USUBJID = c("S1", "S2"),
                          TRT01A = "A", stringsAsFactors = FALSE)

pj_id <- function(blk, pick = NULL) {
  out <- NULL
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      if (!is.null(pick)) {
        session$makeScope("expr")$setInputs(id = pick)
        session$flushReact()
      }
      out <<- session$returned$state$id()
    },
    args = list(x = blk, data = list(data = function() two_ids_ev,
                                     population = function() two_ids_pop))
  )
  out
}

test_that("the constructor's identifier is used, not the first shared column", {
  # SUBJ comes first in both frames, so a block that ignores its argument
  # answers SUBJ here and looks fine on data where USUBJID happens to be first.
  expect_equal(pj_id(new_population_join_block()), "USUBJID")
})

test_that("a restored identifier survives", {
  expect_equal(pj_id(new_population_join_block(id = "SUBJ")), "SUBJ")
})

test_that("a user pick reaches state, and can override a restored value", {
  expect_equal(pj_id(new_population_join_block(), pick = "SUBJ"), "SUBJ")
  expect_equal(pj_id(new_population_join_block(id = "SUBJ"), pick = "USUBJID"),
               "USUBJID")
})

test_that("an empty intersection does not wipe a good value", {
  # Data arrives late, and a panel not yet opened can see an empty frame
  # first. Clearing the setting there loses it for good, because the clear is
  # what gets saved.
  blk <- new_population_join_block(id = "SUBJ")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(session$returned$state$id(), "SUBJ")
    },
    args = list(x = blk,
                data = list(data = function() data.frame(A = 1),
                            population = function() data.frame(B = 2)))
  )
})

# --- the picker has to survive a panel that opens LATE ------------------------
# A block on a dock panel nobody has opened still runs its server, because a
# visible block downstream needs its data, and its UI does not exist yet. The
# picker used to be a static selectInput() filled in by updateSelectInput(),
# and that push resolves against a bound element: with no element it is
# dropped, silently. So the block was right, the picker was empty, and the
# first thing the empty picker did when the panel was finally opened was
# report "" back -- which the input observer took for a user pick. The
# identifier went, the join fell back to pass-through, and every percentage
# downstream lost its denominator. The clear is also what gets saved.

# blockr.core wraps the block module in a proxy scope, so the output's full
# name carries a prefix that is not ours to know. Match the tail.
pj_control <- function(session) {
  outs <- names(session$.__enclos_env__$private$outs)
  nm <- grep("expr-control$", outs, value = TRUE)
  paste(as.character(session$getOutput(nm[[1L]])), collapse = "")
}

test_that("the picker is rendered, so a late mount gets it configured", {
  blk <- new_population_join_block(id = "USUBJID")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      html <- pj_control(session)
      expect_match(html, "<option value=\"USUBJID\" selected>", fixed = TRUE)
      expect_match(html, "SUBJ", fixed = TRUE)
    },
    args = list(x = blk, data = list(data = function() two_ids_ev,
                                     population = function() two_ids_pop))
  )
})

test_that("an empty picker binding does not clear the identifier", {
  blk <- new_population_join_block(id = "USUBJID")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      # what a select with no options reports the first time it binds
      session$makeScope("expr")$setInputs(id = "")
      session$flushReact()
      expect_equal(session$returned$state$id(), "USUBJID")
      expect_match(paste(deparse(session$returned$expr()), collapse = " "),
                   "join_population", fixed = TRUE)
    },
    args = list(x = blk, data = list(data = function() two_ids_ev,
                                     population = function() two_ids_pop))
  )
})

test_that("a value the inputs do not share is not taken for a pick", {
  # Same shape as the empty echo: the widget is a view, the block state is
  # the truth.
  blk <- new_population_join_block(id = "USUBJID")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      session$makeScope("expr")$setInputs(id = "AEDECOD")
      session$flushReact()
      expect_equal(session$returned$state$id(), "USUBJID")
    },
    args = list(x = blk, data = list(data = function() two_ids_ev,
                                     population = function() two_ids_pop))
  )
})
