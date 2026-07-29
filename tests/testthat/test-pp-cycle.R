# Cycle anchors. The load-bearing claims are (a) a real D1 row always wins
# over a back-calculation, and (b) a study without the vocabulary degrades to
# NULL rather than erroring.

lb_cycles <- function(...) {
  rows <- list(...)
  data.frame(
    USUBJID = vapply(rows, `[[`, character(1L), 1L),
    AVISIT  = vapply(rows, `[[`, character(1L), 2L),
    ADT     = as.Date(vapply(rows, `[[`, character(1L), 3L)),
    stringsAsFactors = FALSE
  )
}

test_that("pp_parse_cycle_visits reads the vocabulary and skips the rest", {
  p <- pp_parse_cycle_visits(c(
    "CYCLE 1 DAY 1", "CYCLE 12 DAY 15", "cycle 2 day 8",
    "CYCLE 3 DAY 1 PRE-DOSE", "SCREENING", "WEEK 2", NA
  ))
  expect_equal(p$cycle, c(1L, 12L, 2L, 3L, NA, NA, NA))
  expect_equal(p$day, c(1L, 15L, 8L, 1L, NA, NA, NA))
})

test_that("every sponsor spelling of the same fact is the same fact", {
  # No CDISC standard says how to write this, so the BAND has to recognise it
  # across studies: separators, punctuation, case, and the short form. Labels
  # never come through here -- they are printed, not parsed.
  p <- pp_parse_cycle_visits(c(
    "Cycle 2, Day 8", "CYCLE 2/DAY 8", "CYCLE_2_DAY_8", "C2D8", "c2 d8"
  ))
  expect_equal(p$cycle, rep(2L, 5L))
  expect_equal(p$day, rep(8L, 5L))
  # A day before the cycle opened keeps its sign
  expect_equal(pp_parse_cycle_visits("C1D-1")$day, -1L)
})

test_that("labels that are not cycles are left alone, not guessed at", {
  p <- pp_parse_cycle_visits(c(
    "SCREENING", "BASELINE", "WEEK 2", "DAY -7", "UNSCHEDULED", "VISIT 3",
    "CYCLE 1", "END OF TREATMENT", "FOLLOW-UP", "CD4 PANEL", "AC1D1"
  ))
  expect_true(all(is.na(p$cycle)))
  expect_true(all(is.na(p$day)))
})

test_that("the D1 row is the anchor, not an average with slipped visits", {
  # C1D1 on the 1st; C1D8 drawn a day LATE (the 9th) but still labelled D8.
  # Back-calculating from it would say the cycle started on the 2nd. It did
  # not: the D1 row is the fact.
  dm_obj <- dm::dm(adlb = lb_cycles(
    list("S1", "CYCLE 1 DAY 1", "2014-01-01"),
    list("S1", "CYCLE 1 DAY 8", "2014-01-09")
  ))
  a <- pp_cycle_anchors(dm_obj)
  expect_equal(nrow(a), 1L)
  expect_equal(a$cycle_start, as.Date("2014-01-01"))
  expect_false(a$estimated)
})

test_that("a missing D1 falls back to back-calculation and says so", {
  dm_obj <- dm::dm(adlb = lb_cycles(
    list("S1", "CYCLE 1 DAY 1", "2014-01-01"),
    list("S1", "CYCLE 2 DAY 8", "2014-01-29")   # no C2D1 row
  ))
  a <- pp_cycle_anchors(dm_obj)
  expect_equal(a$cycle, c(1L, 2L))
  expect_equal(a$estimated, c(FALSE, TRUE))
  expect_equal(a$cycle_start[2], as.Date("2014-01-22"))  # 29th minus 7
})

test_that("many lab rows per visit are duplicates, not votes", {
  # One visit is ~30 rows (one per test) sharing a date; the anchor must not
  # care how many there are.
  dm_obj <- dm::dm(adlb = lb_cycles(
    list("S1", "CYCLE 1 DAY 1", "2014-01-01"),
    list("S1", "CYCLE 1 DAY 1", "2014-01-01"),
    list("S1", "CYCLE 1 DAY 1", "2014-01-01")
  ))
  a <- pp_cycle_anchors(dm_obj)
  expect_equal(nrow(a), 1L)
  expect_equal(a$cycle_start, as.Date("2014-01-01"))
})

test_that("cycles are per subject", {
  dm_obj <- dm::dm(adlb = lb_cycles(
    list("S1", "CYCLE 1 DAY 1", "2014-01-01"),
    list("S2", "CYCLE 1 DAY 1", "2014-03-05")
  ))
  a <- pp_cycle_anchors(dm_obj)
  expect_equal(a$USUBJID, c("S1", "S2"))
  expect_equal(a$cycle_start, as.Date(c("2014-01-01", "2014-03-05")))
})

test_that("a cycle runs until the next one starts", {
  dm_obj <- dm::dm(adlb = lb_cycles(
    list("S1", "CYCLE 1 DAY 1", "2014-01-01"),
    list("S1", "CYCLE 2 DAY 1", "2014-01-29")   # held a week: 28-day gap
  ))
  a <- pp_cycle_anchors(dm_obj)
  expect_equal(a$cycle_end[1], as.Date("2014-01-28"))
})

test_that("no vocabulary / no table / no dm yields NULL, never a condition", {
  expect_null(pp_cycle_anchors(dm::dm(adlb = lb_cycles(
    list("S1", "SCREENING", "2014-01-01"),
    list("S1", "WEEK 2", "2014-01-15")
  ))))
  expect_null(pp_cycle_anchors(dm::dm(adsl = data.frame(USUBJID = "S1"))))
  expect_null(pp_cycle_anchors("not a dm"))
})

# ---------------------------------------------------------------------------
# Where the BAND reads its cycles from
#
# The dosing table first. Reported from a study review: doses given on the
# protocol's Day 1 read D2/D3/D4, because safety labs for a "CYCLE n DAY 1"
# visit are drawn pre-dose and the band opened at the blood draw. The dose
# rows said "CYCLE 2 DAY 1" the whole time.
# ---------------------------------------------------------------------------

ex_doses <- function(..., visits = NULL) {
  dates <- as.Date(c(...))
  out <- data.frame(
    USUBJID = "S1", ASTDT = dates, EXTRT = "DRUG",
    stringsAsFactors = FALSE
  )
  if (!is.null(visits)) out$AVISIT <- visits
  out
}

test_that("the band opens on the infusion, not on the pre-dose labs", {
  a <- pp_cycle_anchors(dm::dm(
    adlb = lb_cycles(
      list("S1", "CYCLE 1 DAY 1", "2014-01-01"),
      list("S1", "CYCLE 2 DAY 1", "2014-01-20")   # drawn two days early
    ),
    adex = ex_doses("2014-01-01", "2014-01-22",
                    visits = c("CYCLE 1 DAY 1", "CYCLE 2 DAY 1"))
  ))
  expect_equal(a$cycle_start, as.Date(c("2014-01-01", "2014-01-22")))
})

test_that("dosing without the vocabulary falls through to the labs", {
  # A study dosed in cycles whose ex rows say "WEEK 2". No correction is
  # attempted: the band is the labs' answer, and the dose bars still report
  # whatever their own rows say (nothing, here).
  tbls <- list(
    adlb = lb_cycles(list("S1", "CYCLE 1 DAY 1", "2014-01-01")),
    adex = ex_doses("2014-01-04", visits = "WEEK 2")
  )
  expect_equal(pp_cycle_source(tbls)$table, "adlb")
  tbls$adex$AVISIT <- "CYCLE 1 DAY 1"
  expect_equal(pp_cycle_source(tbls)$table, "adex")
  expect_null(pp_cycle_source(list()))
})

test_that("a study whose cycles live outside adlb still gets a band", {
  # Vitals-only, or a vendor table: the vocabulary is what counts, not which
  # domain shipped it. Gating on adlb would give this study no lane at all.
  vs <- data.frame(
    USUBJID = "S1", AVISIT = c("CYCLE 1 DAY 1", "CYCLE 2 DAY 1"),
    ADT = as.Date(c("2014-01-01", "2014-01-22")),
    PARAMCD = "SYSBP", AVAL = c(120, 118), stringsAsFactors = FALSE
  )
  expect_equal(pp_cycle_source(list(advs = vs))$table, "advs")
  a <- pp_cycle_anchors(dm::dm(advs = vs))
  expect_equal(a$cycle_start, as.Date(c("2014-01-01", "2014-01-22")))

  # A table with no dated visits is not a source
  expect_null(pp_cycle_source(list(
    adsl = data.frame(USUBJID = "S1", AVISIT = "CYCLE 1 DAY 1")
  )))
})

test_that("no dose date is ever consulted to place a band", {
  # The anchor is the labelled row, full stop. Nothing snaps it onto the
  # infusion three days later -- that heuristic is what this design drops.
  a <- pp_cycle_anchors(dm::dm(
    adlb = lb_cycles(list("S1", "CYCLE 1 DAY 1", "2014-01-01")),
    adex = ex_doses("2014-01-04")
  ))
  expect_equal(a$cycle_start, as.Date("2014-01-01"))
})

test_that("pp_cycle_span measures the study rather than assuming 21", {
  a <- data.frame(
    USUBJID = c("S1", "S1", "S1"),
    cycle = 1:3,
    cycle_start = as.Date(c("2014-01-01", "2014-01-15", "2014-01-29"))
  )
  expect_equal(pp_cycle_span(a), 14)
  # One cycle measures nothing -> documented default
  expect_equal(pp_cycle_span(a[1, ]), 21)
  expect_equal(pp_cycle_span(NULL), 21)
})

# ---------------------------------------------------------------------------
# Labels: the visit the row names, printed as the study wrote it
# ---------------------------------------------------------------------------

anchors_2 <- function() {
  pp_cycle_anchors(dm::dm(adlb = lb_cycles(
    list("S1", "CYCLE 1 DAY 1", "2014-01-01"),
    list("S1", "CYCLE 2 DAY 1", "2014-01-22")
  )))
}

test_that("the visit rides behind the existing label, never replaces it", {
  ref <- pp_ms_ts(as.Date("2014-01-01"))
  expect_equal(
    pp_xlabel(as.Date("2014-01-22"), ref, "rday", "CYCLE 2 DAY 1"),
    "D22 (CYCLE 2 DAY 1)"
  )
  # Date mode too: "in addition to the date" was the actual request
  expect_equal(
    pp_xlabel(as.Date("2014-01-22"), ref, "date", "CYCLE 2 DAY 1"),
    "2014-01-22 (CYCLE 2 DAY 1)"
  )
  expect_equal(pp_day_label(22, "CYCLE 2 DAY 1"), "D22 (CYCLE 2 DAY 1)")
})

test_that("the label is not interpreted, whatever it says", {
  # Cycle vocabulary, weeks, an unscheduled draw and a sponsor's own wording
  # all reach the tooltip the same way: as text. Nothing is renumbered,
  # abbreviated or dropped for failing to look like a cycle.
  expect_equal(pp_with_visit("2014-03-05", "UNSCHEDULED"),
               "2014-03-05 (UNSCHEDULED)")
  expect_equal(pp_with_visit("2014-03-05", "WEEK 24"),
               "2014-03-05 (WEEK 24)")
  expect_equal(pp_with_visit("2014-03-05", "Cycle 2, Day 8 (pre-dose)"),
               "2014-03-05 (Cycle 2, Day 8 (pre-dose))")
  expect_equal(pp_with_visit("2014-03-05", "RETEST / EARLY TERMINATION"),
               "2014-03-05 (RETEST / EARLY TERMINATION)")
})

test_that("a delayed administration reports both facts", {
  # Filed as CYCLE 1 DAY 8, infused on day 9. The date is what happened, the
  # label is what it was; neither is adjusted to agree with the other.
  ref <- pp_ms_ts(as.Date("2014-01-01"))
  expect_equal(
    pp_xlabel(as.Date("2014-01-09"), ref, "date", "CYCLE 1 DAY 8"),
    "2014-01-09 (CYCLE 1 DAY 8)"
  )
  expect_equal(pp_day_label(9, "CYCLE 1 DAY 8"), "D9 (CYCLE 1 DAY 8)")
})

test_that("no visit label leaves every label exactly as it was", {
  ref <- pp_ms_ts(as.Date("2014-01-01"))
  expect_equal(pp_xlabel(as.Date("2014-01-22"), ref, "rday"), "D22")
  expect_equal(pp_xlabel(as.Date("2014-01-22"), ref, "rday", NA), "D22")
  expect_equal(pp_with_visit("2014-03-05", ""), "2014-03-05")
  expect_equal(pp_with_visit("2014-03-05", "   "), "2014-03-05")
  expect_equal(pp_day_label(22), "D22")
  expect_equal(pp_day_label(22, NA), "D22")
})

test_that("a label is drawn on one line, so whitespace folds", {
  # A study's own visit column may carry line breaks the ADaM variables never
  # do. Folding them is the only thing done to the string.
  expect_equal(pp_with_visit("D8", "CYCLE 1\n  DAY 8 "), "D8 (CYCLE 1 DAY 8)")
})

test_that("no treatment start still leaves the band dateable", {
  a <- anchors_2()
  expect_equal(a$cycle_start[1], as.Date("2014-01-01"))
  expect_equal(pp_xlabel(as.Date("2014-01-01"), NA_real_, "date"), "2014-01-01")
})

# ---------------------------------------------------------------------------
# The viz
# ---------------------------------------------------------------------------

test_that("the cycle lane declares the source the study answered to", {
  expect_equal(cycle_viz$id, "cycle_lane")
  expect_true("cycle" %in% cycle_viz$uses)

  lab_only <- pp_cycle_vizs(dm::dm(adlb = lb_cycles(
    list("S1", "CYCLE 1 DAY 1", "2014-01-01")
  )))[[1L]]
  expect_equal(lab_only$tables, "adlb")
  expect_setequal(lab_only$requires$adlb, c("AVISIT", "ADT"))

  dosed <- pp_cycle_vizs(dm::dm(
    adlb = lb_cycles(list("S1", "CYCLE 1 DAY 1", "2014-01-01")),
    adex = ex_doses("2014-01-01", visits = "CYCLE 1 DAY 1")
  ))[[1L]]
  expect_equal(dosed$tables, "adex")
  expect_setequal(dosed$requires$adex, c("AVISIT", "ASTDT"))
})

test_that("the band is what consumes the anchors", {
  # Nothing else may derive a timepoint: a viz reads the labels on its own
  # rows and stops. The CM gantt drops its declaration with the indication
  # work in flight, so it is not asserted here.
  expect_equal(cycle_viz$uses, "cycle")
  expect_false("cycle" %in% ae_gantt_viz$uses)
  expect_false("cycle" %in% patient_overview_viz$uses)
})

test_that("the cycle lane is generated, never statically registered", {
  # Static registration would park a permanently empty card on every study
  # that is not dosed in cycles -- adlb/AVISIT/ADT are all present there, so
  # no schema-based gate can catch it.
  expect_false("cycle_lane" %in% names(patient_profile_static_vizs()))
})

test_that("a study speaking cycles gets the lane", {
  dm_obj <- dm::dm(adlb = lb_cycles(
    list("S1", "CYCLE 1 DAY 1", "2014-01-01")
  ))
  expect_equal(names(pp_cycle_vizs(dm_obj)), "cycle_lane")
})

test_that("a study not dosed in cycles gets no lane at all", {
  # The whole point: adlb, AVISIT and ADT are all PRESENT here. Only the
  # values are week-based, which no schema check can see.
  expect_equal(pp_cycle_vizs(dm::dm(adlb = lb_cycles(
    list("S1", "SCREENING", "2013-12-20"),
    list("S1", "WEEK 2", "2014-01-15")
  ))), list())
  expect_equal(pp_cycle_vizs(dm::dm(adsl = data.frame(USUBJID = "S1"))), list())
  expect_equal(pp_cycle_vizs("not a dm"), list())
})

test_that("availability is cohort-wide, not per patient", {
  # One subject speaks cycles, the other does not: the lane exists, and the
  # sidebar must not shuffle as you page between them.
  dm_obj <- dm::dm(adlb = lb_cycles(
    list("S1", "CYCLE 1 DAY 1", "2014-01-01"),
    list("S2", "WEEK 2", "2014-01-15")
  ))
  expect_equal(names(pp_cycle_vizs(dm_obj)), "cycle_lane")
})

test_that("the AI effect does not call the cycle lane a typo", {
  # It is generated, so it is absent from the static ids that check reads.
  desc <- config_effect.patient_profile_block(
    NULL, list(selected = c("cycle_lane", "ae_gantt"))
  )
  expect_no_match(desc, "INVALID")
  # ... while a real typo is still caught
  expect_match(
    config_effect.patient_profile_block(NULL, list(selected = "nonsense_viz")),
    "INVALID"
  )
})

test_that("the cycle lane renders bands, and says so when it cannot", {
  a <- anchors_2()
  tr <- as.Date(c("2014-01-01", "2014-02-15"))
  ref <- pp_ms_ts(as.Date("2014-01-01"))
  dm_obj <- dm::dm(adlb = lb_cycles(
    list("S1", "CYCLE 1 DAY 1", "2014-01-01"),
    list("S1", "CYCLE 2 DAY 1", "2014-01-22")
  ))
  chart <- cycle_viz$render(dm_obj, tr, list(cycle_anchors = a), ref, "rday")
  expect_s3_class(chart, "echarts4r")

  empty <- cycle_viz$render(dm_obj, tr, list(cycle_anchors = NULL), ref, "rday")
  expect_s3_class(empty, "echarts4r")
})
