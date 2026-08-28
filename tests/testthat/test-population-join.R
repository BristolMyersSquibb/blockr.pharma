# join_population(): restore the subjects an event table never mentions.
#
# The numbers here are the reason the block exists. On pharmaverseadam, 29 of
# 254 safety subjects have no adverse event, and they are not spread evenly:
# dividing by "subjects in ADAE" instead of "subjects treated" inflates placebo
# by ~4.8 points and the high-dose arm by ~0.7. Same direction every time --
# it shrinks the apparent difference between arms.

ev <- data.frame(
  USUBJID = c("S1", "S1", "S2"),
  AEDECOD = c("Diarrhoea", "Diarrhoea", "Nausea"),
  AESEV   = c("MILD", "SEVERE", "MILD"),
  stringsAsFactors = FALSE
)
pop <- data.frame(
  USUBJID = c("S1", "S2", "S3", "S4"),
  TRT01A  = c("A", "A", "B", "B"),
  SAFFL   = "Y",
  stringsAsFactors = FALSE
)

test_that("absent subjects come back, with no event columns", {
  out <- join_population(ev, pop)
  expect_equal(nrow(out), 5L)
  expect_setequal(unique(out$USUBJID), c("S1", "S2", "S3", "S4"))
  added <- out[out$USUBJID %in% c("S3", "S4"), ]
  expect_true(all(is.na(added$AEDECOD)))
  expect_true(all(is.na(added$AESEV)))
  # ...and WITH the population's columns, or they could not be grouped.
  expect_equal(added$TRT01A, c("B", "B"))
})

test_that("the events themselves are untouched", {
  out <- join_population(ev, pop)
  kept <- out[!is.na(out$AEDECOD), c("USUBJID", "AEDECOD", "AESEV")]
  rownames(kept) <- NULL
  expect_equal(kept, ev)
})

test_that("counting distinct subjects gives the population, not the events", {
  out <- join_population(ev, pop)
  by_arm <- tapply(out$USUBJID, out$TRT01A, function(x) length(unique(x)))
  expect_equal(as.integer(by_arm[["A"]]), 2L)
  expect_equal(as.integer(by_arm[["B"]]), 2L)  # neither B subject has an event

  # The bug this replaces. Counting from the events alone, arm B does not
  # exist at all -- its two subjects are only in the population -- so a
  # per-arm rate there is either a division by zero or a missing panel.
  ev_arm <- merge(ev, pop[, c("USUBJID", "TRT01A")], by = "USUBJID")
  expect_false("B" %in% ev_arm$TRT01A)
})

test_that("a column the events already carry is NOT joined or renamed", {
  # On ADAE this is the whole point: it carries its own TRT01A and SAFFL, so a
  # blind join produces a .x/.y pair on exactly the two columns the caller
  # needs, and nothing says which to group by.
  ev2 <- transform(ev, TRT01A = "A", SAFFL = "Y")
  out <- join_population(ev2, pop)
  expect_false(any(grepl("\\.(x|y)$", names(out))))
  expect_equal(sum(names(out) == "TRT01A"), 1L)
  # The appended rows still get theirs, from the population side.
  expect_equal(out$TRT01A[out$USUBJID == "S3"], "B")
})

test_that("nothing to add is a no-op, not an error", {
  out <- join_population(ev, pop[pop$USUBJID %in% c("S1", "S2"), ])
  expect_equal(nrow(out), nrow(ev))
})

test_that("a missing identifier is refused, in either input", {
  expect_error(join_population(ev, pop, id = "SUBJID"), "must be present")
  expect_error(join_population(ev[, -1], pop), "must be present")
  expect_error(join_population(ev, "not a frame"), "data frames")
})

test_that("the frame is a valid source for a subject-level denominator", {
  # Two consumers read this frame differently -- a chart counts distinct
  # subjects per panel, a table derives one row per subject and counts those --
  # and they have to agree, or one view's percentages contradict another's.
  # Deduplicating on the identifier is what the table side does.
  out <- join_population(ev, pop)

  chart <- tapply(out$USUBJID, out$TRT01A, function(x) length(unique(x)))

  denom <- unique(out[, c("USUBJID", "TRT01A", "SAFFL")])
  expect_equal(nrow(denom), length(unique(pop$USUBJID)))
  expect_false(any(duplicated(denom$USUBJID)))   # genuinely subject-level
  expect_false(anyNA(denom$TRT01A))              # every subject has an arm
  tbl <- table(denom$TRT01A)

  expect_equal(as.integer(tbl[["A"]]), as.integer(chart[["A"]]))
  expect_equal(as.integer(tbl[["B"]]), as.integer(chart[["B"]]))
})

test_that("the real gap is uneven across arms, which is why it matters", {
  skip_if_not_installed("pharmaverseadam")
  adsl <- pharmaverseadam::adsl
  adae <- pharmaverseadam::adae
  p <- adsl[adsl$SAFFL == "Y", c("USUBJID", "TRT01A", "SAFFL")]
  out <- join_population(adae[, c("USUBJID", "AEDECOD")], p)
  expect_equal(length(unique(out$USUBJID)), nrow(p))
  # Every safety subject present; the ones ADAE never mentions came back.
  expect_equal(sum(is.na(out$AEDECOD)),
               sum(!p$USUBJID %in% adae$USUBJID))
  n_pop <- tapply(out$USUBJID, out$TRT01A, function(x) length(unique(x)))
  n_ev <- tapply(out$USUBJID[!is.na(out$AEDECOD)],
                 out$TRT01A[!is.na(out$AEDECOD)],
                 function(x) length(unique(x)))
  expect_gt(as.integer(n_pop[["Placebo"]]) - as.integer(n_ev[["Placebo"]]),
            as.integer(n_pop[["Xanomeline High Dose"]]) -
              as.integer(n_ev[["Xanomeline High Dose"]]))
})
