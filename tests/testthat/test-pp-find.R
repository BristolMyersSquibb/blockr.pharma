# The find control's model: what a pick is, what it matches, what the picker
# is offered, and what a board saved before it restores as.

find_tbl <- function() {
  data.frame(
    USUBJID = "S-1",
    AETERM = c("aspiration pneumonia", "chest infection", "loose stools",
               "itching"),
    # PNEUMONIA and PNEUMONIA ASPIRATION are two terms, and one is a
    # substring of the other: the pair the exact match exists for.
    AEDECOD = c("PNEUMONIA ASPIRATION", "PNEUMONIA",
                "DIARRHOEA", "PRURITUS"),
    AEHLT = c("Lower respiratory tract infections",
              "Lower respiratory tract infections",
              "Diarrhoea (excl infective)", "Pruritus NEC"),
    AEBODSYS = c("INFECTIONS AND INFESTATIONS", "INFECTIONS AND INFESTATIONS",
                 "GASTROINTESTINAL DISORDERS",
                 "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"),
    stringsAsFactors = FALSE
  )
}

test_that("a pick list survives every shape it arrives in", {
  # From the client: an array of objects, so a list of lists after Shiny has
  # decoded it.
  wire <- list(list(col = "AEBODSYS", value = "CARDIAC DISORDERS"))
  expect_identical(pp_find_picks(wire), wire)

  # From a board saved before this control existed: one string, which is
  # what that box always was.
  expect_identical(pp_find_picks("pneumonia"),
                   list(list(col = "*", value = "pneumonia")))

  # Nothing is nothing, in each of its spellings.
  expect_identical(pp_find_picks(NULL), list())
  expect_identical(pp_find_picks(list()), list())
  expect_identical(pp_find_picks(""), list())
  expect_identical(pp_find_picks("   "), list())

  # The same term picked twice is one filter, keyed on BOTH halves: the same
  # word can legitimately be a preferred term and a high level term.
  dup <- pp_find_picks(list(
    list(col = "AEDECOD", value = "PNEUMONIA"),
    list(col = "AEDECOD", value = "pneumonia"),
    list(col = "AEHLT", value = "PNEUMONIA")
  ))
  expect_length(dup, 2L)

  # Junk from an older or newer client is dropped, not fatal: a saved board
  # is data from another version, and a filter that cannot be read is a
  # filter that should not be applied.
  expect_identical(pp_find_picks(list(list(col = "X"), 42, NULL)), list())
})

test_that("picks are an OR, and a coded pick matches its own column exactly", {
  tbl <- find_tbl()
  cols <- c("AETERM", "AEDECOD", "AEHLT", "AEBODSYS")

  # No picks is the unfiltered panel.
  expect_true(all(pp_find_match(tbl, list(), cols)))

  soc <- list(list(col = "AEBODSYS", value = "INFECTIONS AND INFESTATIONS"))
  expect_identical(sum(pp_find_match(tbl, soc, cols)), 2L)

  # Two picks, two arms: the question one term could never ask.
  two <- list(list(col = "AEDECOD", value = "PNEUMONIA"),
              list(col = "AEDECOD", value = "DIARRHOEA"))
  expect_identical(sum(pp_find_match(tbl, two, cols)), 2L)

  # Mixed levels in one filter is the normal case.
  mixed <- list(list(col = "AEBODSYS", value = "GASTROINTESTINAL DISORDERS"),
                list(col = "AEDECOD", value = "PRURITUS"))
  expect_identical(sum(pp_find_match(tbl, mixed, cols)), 2L)

  # EXACT, not substring. This is the whole reason a pick carries its column
  # and is matched with `==`: PNEUMONIA is a substring of PNEUMONIA
  # ASPIRATION, so a reader who picked one term off the list would silently
  # get two -- which is the failure the free-text box could never avoid.
  expect_identical(
    sum(pp_find_match(tbl, list(list(col = "AEDECOD", value = "PNEUMONIA")),
                      cols)),
    1L
  )
  # And the typed pick is still a substring, deliberately: it is the box, and
  # its reach across the coding levels is why it was kept.
  expect_identical(
    sum(pp_find_match(tbl, list(list(col = "*", value = "PNEUMONIA")), cols)),
    2L
  )

  # A body system's name is not one of its preferred terms, and the old box
  # could not tell the two apart either.
  wrong_col <- list(list(col = "AEDECOD",
                         value = "INFECTIONS AND INFESTATIONS"))
  expect_identical(sum(pp_find_match(tbl, wrong_col, cols)), 0L)

  # Case and padding are the study's business, not the reader's.
  expect_identical(
    sum(pp_find_match(tbl, list(list(col = "AEDECOD", value = " pneumonia ")),
                      cols)),
    1L
  )
})

test_that("a free-text pick is the old box, and reaches every level", {
  tbl <- find_tbl()
  cols <- c("AETERM", "AEDECOD", "AEHLT", "AEBODSYS")
  free <- function(v) list(list(col = "*", value = v))

  # The verbatim report, which no coded level carries.
  expect_identical(sum(pp_find_match(tbl, free("chest"), cols)), 1L)
  # The body system, which no preferred term contains -- the reach that made
  # the box worth keeping.
  expect_identical(sum(pp_find_match(tbl, free("infestations"), cols)), 2L)
  # And it ORs with a coded pick like anything else.
  expect_identical(
    sum(pp_find_match(tbl, c(free("stools"),
                             list(list(col = "AEDECOD", value = "PRURITUS"))),
                      cols)),
    2L
  )
})

test_that("a pick naming a column the study lacks matches nothing", {
  # Deliberately NOT pp_search_match()'s rule, which ignores a search it has
  # no column for. A study carrying none of the coding levels cannot answer a
  # typed term at all; a pick that names AEHLT is a specific question about a
  # column that has gone. The panel blanks, and its empty state names the
  # picks and offers to clear them.
  tbl <- find_tbl()[, c("USUBJID", "AEDECOD")]
  gone <- list(list(col = "AEHLT", value = "Pruritus NEC"))
  expect_identical(sum(pp_find_match(tbl, gone, "AEDECOD")), 0L)
})

test_that("the picker is offered the coding levels, coarsest first", {
  opts <- pp_find_options(find_tbl(), PP_AE_LEVELS)
  expect_identical(vapply(opts, `[[`, character(1), "col"),
                   c("AEBODSYS", "AEHLT", "AEDECOD"))
  expect_identical(opts[[1]]$label, "Body system")

  # Counts are this patient's records, which is what the popover answers
  # "which of my events are these" with before anything redraws.
  soc <- opts[[1]]$options
  n <- vapply(soc, `[[`, integer(1), "n")
  names(n) <- vapply(soc, `[[`, character(1), "value")
  expect_identical(unname(n[["INFECTIONS AND INFESTATIONS"]]), 2L)
  expect_identical(sum(n), 4L)

  # Ordered the way the chart orders its lanes, so the list a reader scans is
  # in the order of the panel they are reading.
  expect_identical(names(n), sort(names(n)))

  # A level the study does not carry is skipped, not offered empty.
  thin <- pp_find_options(find_tbl()[, c("AEDECOD", "AEBODSYS")], PP_AE_LEVELS)
  expect_identical(vapply(thin, `[[`, character(1), "col"),
                   c("AEBODSYS", "AEDECOD"))

  # Blanks are not a term.
  blank <- find_tbl()
  blank$AEBODSYS[1] <- ""
  blank$AEBODSYS[2] <- NA_character_
  expect_length(pp_find_options(blank, PP_AE_LEVELS)[[1]]$options, 2L)

  # Nothing to offer at all is an empty list, not a group with no rows.
  expect_identical(pp_find_options(find_tbl()[0, ], PP_AE_LEVELS), list())
})

test_that("a level with more values than the cap reports what it left out", {
  many <- data.frame(AEDECOD = paste0("TERM ", sprintf("%03d", 1:50)),
                     stringsAsFactors = FALSE)
  g <- pp_find_options(many, c(AEDECOD = "Preferred term"),
                       max_options = 10L)[[1]]
  expect_length(g$options, 10L)
  expect_identical(g$truncated, 40L)
  # The cap takes the first of the ORDER, so the list is still the head of
  # what the reader would have scanned.
  expect_identical(g$options[[1]]$value, "TERM 001")
})

test_that("the option payload is small enough to ride the header", {
  # The list travels WITH the panel header rather than being fetched when the
  # popover opens. Measured against the AE slot payload, which is about 9.6kB.
  skip_if_not_installed("pharmaverseadam")
  adae <- as.data.frame(pharmaverseadam::adae)
  per <- split(adae, adae$USUBJID)
  bytes <- vapply(per, function(d) {
    nchar(as.character(jsonlite::toJSON(pp_find_options(d, PP_AE_LEVELS),
                                        auto_unbox = TRUE)), type = "bytes")
  }, numeric(1))
  expect_lt(stats::median(bytes), 1024)
  expect_lt(max(bytes), 8 * 1024)
})

test_that("labels and the empty sentence name what is being filtered on", {
  picks <- list(list(col = "AEBODSYS", value = "CARDIAC DISORDERS"),
                list(col = "*", value = "erythem"))
  # A coded term gets the panel's own casing; a fragment is quoted, because
  # printing it bare would read as a term the study uses.
  expect_identical(pp_find_labels(picks),
                   c("Cardiac disorders", "\u201cerythem\u201d"))
  # The caption has room for one and a count of the rest.
  expect_identical(pp_find_summary(picks), "Cardiac disorders +1")
  expect_identical(pp_find_summary(picks[1]), "Cardiac disorders")
  expect_identical(pp_find_summary(list()), "")

  # The empty state says how many records were filtered away and what did it,
  # because picks survive a patient switch and this is a state a reviewer
  # paging through a cohort has to be able to get out of.
  msg <- pp_find_empty_msg(18L, picks, "event")
  expect_match(msg, "None of this patient's 18 events", fixed = TRUE)
  expect_match(msg, "any of your 2 filters", fixed = TRUE)
  expect_match(msg, "Cardiac disorders", fixed = TRUE)
  # One filter is not "any of your 1 filters".
  expect_false(grepl("any of your", pp_find_empty_msg(3L, picks[1], "event")))
  # And one record is not "1 events".
  expect_match(pp_find_empty_msg(1L, picks[1], "event"), "1 event match",
               fixed = TRUE)
})

test_that("a board saved before the picker restores as one typed pick", {
  before <- list(
    ae_gantt = list(search = "pneumonia", lanes = "AEDECOD"),
    cm_gantt = list(search = ""),
    chem = list(items = c("ALT", "ALB"))
  )
  after <- pp_migrate_viz_settings(before)

  expect_identical(after$ae_gantt$find,
                   list(list(col = "*", value = "pneumonia")))
  expect_identical(after$ae_gantt$lanes, "AEDECOD")
  # The old key is dropped rather than kept in step: a setting that exists in
  # two places is a setting that will disagree in one of them.
  expect_null(after$ae_gantt$search)
  # An empty box was not a filter, so it does not become one.
  expect_null(after$cm_gantt$find)
  expect_null(after$cm_gantt$search)
  # Everything else is left alone.
  expect_identical(after$chem, before$chem)
  expect_identical(pp_migrate_viz_settings(list()), list())

  # And the CONSTRUCTOR does it, so nothing downstream ever sees the old
  # shape. Read off the server closure, which is where the argument lands.
  blk <- new_patient_profile_block(viz_settings = before)
  restored <- environment(blk$expr_server)$viz_settings
  expect_identical(restored$ae_gantt$find,
                   list(list(col = "*", value = "pneumonia")))
  expect_null(restored$ae_gantt$search)
})

test_that("the cohort vocabulary counts patients, not records", {
  # The list that rides the header is this patient's, which is what makes its
  # counts mean something and what makes it useless for the question "does
  # this study code anything as pneumonia". For a term this patient has none
  # of, the useful number is how much of the cohort does.
  coh <- data.frame(
    USUBJID = c("S-1", "S-1", "S-2", "S-3"),
    AEDECOD = c("PNEUMONIA", "PNEUMONIA", "PNEUMONIA", "DIARRHOEA"),
    AEBODSYS = c("INFECTIONS AND INFESTATIONS", "INFECTIONS AND INFESTATIONS",
                 "INFECTIONS AND INFESTATIONS", "GASTROINTESTINAL DISORDERS"),
    stringsAsFactors = FALSE
  )
  v <- pp_find_vocab(coh, PP_AE_LEVELS)
  pt <- v[[which(vapply(v, `[[`, character(1), "col") == "AEDECOD")]]
  n <- vapply(pt$options, `[[`, integer(1), "n")
  names(n) <- vapply(pt$options, `[[`, character(1), "value")
  # Four records, three patients: PNEUMONIA is two of them, not three.
  expect_identical(unname(n[["PNEUMONIA"]]), 2L)
  expect_identical(unname(n[["DIARRHOEA"]]), 1L)

  # Same shape as the header's list, so the client draws both the same way.
  expect_named(pt, c("col", "label", "options", "truncated"))
  expect_identical(pt$label, "Preferred term")

  # Levels the study does not carry are skipped, and an empty table has no
  # vocabulary rather than an empty group.
  expect_identical(pp_find_vocab(coh[0, ], PP_AE_LEVELS), list())
  expect_identical(pp_find_vocab(NULL, PP_AE_LEVELS), list())
})

test_that("the cohort vocabulary is bounded by the dictionary, not the study", {
  skip_if_not_installed("pharmaverseadam")
  adae <- as.data.frame(pharmaverseadam::adae)
  bytes <- function(x) {
    nchar(as.character(jsonlite::toJSON(x, auto_unbox = TRUE)), type = "bytes")
  }
  one <- pp_find_vocab(adae, PP_AE_LEVELS)
  # Five times the records, the same vocabulary: the list is a function of
  # the coding dictionary, not of how many rows the study has. That is why it
  # is worth caching on the client behind a token, and why it must NOT ride
  # the panel header the way the patient's own list does.
  five <- pp_find_vocab(rbind(adae, adae, adae, adae, adae), PP_AE_LEVELS)
  n_of <- function(v) sum(vapply(v, function(g) length(g$options), numeric(1)))
  expect_identical(n_of(one), n_of(five))
  # And it is an order of magnitude bigger than one patient's list, which is
  # the whole reason for the split.
  expect_gt(bytes(one), 8 * 1024)
})
