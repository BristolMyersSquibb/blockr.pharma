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
# Labels
# ---------------------------------------------------------------------------

anchors_2 <- function() {
  # TRTSDT = 2014-01-01 -> C1 starts D1, C2 starts D22 (21-day cycle)
  ref <- pp_ms_ts(as.Date("2014-01-01"))
  a <- pp_cycle_anchors(dm::dm(adlb = lb_cycles(
    list("S1", "CYCLE 1 DAY 1", "2014-01-01"),
    list("S1", "CYCLE 2 DAY 1", "2014-01-22")
  )))
  pp_cycle_anchor_days(a, ref)
}

test_that("anchors convert into the same day space the axis uses", {
  a <- anchors_2()
  expect_equal(a$cycle_start_day, c(1, 22))
})

test_that("pp_cycle_label finds the cycle covering a day", {
  a <- anchors_2()
  expect_equal(pp_cycle_label(1, a), "C1 D1")
  expect_equal(pp_cycle_label(8, a), "C1 D8")
  expect_equal(pp_cycle_label(22, a), "C2 D1")
  expect_equal(pp_cycle_label(25, a), "C2 D4")
})

test_that("a day outside every cycle gets no label", {
  a <- anchors_2()
  expect_equal(pp_cycle_label(-5, a), "")    # screening, before C1
  expect_equal(pp_cycle_label(500, a), "")   # long after the last dose
  expect_equal(pp_cycle_label(NA, a), "")
  expect_equal(pp_cycle_label(5, NULL), "")
})

test_that("the last cycle is bounded so a late death is not C2 D190", {
  a <- anchors_2()
  expect_equal(pp_cycle_label(42, a), "C2 D21")  # last day of the span
  expect_equal(pp_cycle_label(43, a), "")        # past it
})

test_that("cycle rides behind the existing label, never replaces it", {
  a <- anchors_2()
  ref <- pp_ms_ts(as.Date("2014-01-01"))
  expect_equal(
    pp_xlabel(as.Date("2014-01-22"), ref, "rday", a), "D22 (C2 D1)"
  )
  # Date mode too: "in addition to the date" was the actual request
  expect_equal(
    pp_xlabel(as.Date("2014-01-22"), ref, "date", a), "2014-01-22 (C2 D1)"
  )
  expect_equal(pp_day_label(22, a), "D22 (C2 D1)")
})

test_that("no anchors leaves every label exactly as it was", {
  ref <- pp_ms_ts(as.Date("2014-01-01"))
  expect_equal(pp_xlabel(as.Date("2014-01-22"), ref, "rday"), "D22")
  expect_equal(pp_xlabel(as.Date("2014-01-22"), ref, "rday", NULL), "D22")
  expect_equal(pp_day_label(22), "D22")
  expect_equal(pp_day_label(22, NULL), "D22")
})

test_that("no treatment start leaves the anchors dateable but unlabellable", {
  a <- pp_cycle_anchor_days(pp_cycle_anchors(dm::dm(adlb = lb_cycles(
    list("S1", "CYCLE 1 DAY 1", "2014-01-01")
  ))), NA_real_)
  expect_true(all(is.na(a$cycle_start_day)))
  expect_equal(a$cycle_start, as.Date("2014-01-01"))  # the lane still works
  expect_equal(pp_cycle_label(1, a), "")
  expect_equal(pp_xlabel(as.Date("2014-01-01"), NA_real_, "date", a),
               "2014-01-01")
})

test_that("pp_cycle_anchor_days is total on empty input", {
  expect_null(pp_cycle_anchor_days(NULL, 0))
})

# ---------------------------------------------------------------------------
# The viz
# ---------------------------------------------------------------------------

test_that("the cycle lane declares against canonical names only", {
  expect_equal(cycle_viz$id, "cycle_lane")
  expect_equal(cycle_viz$tables, "adlb")
  expect_true("cycle" %in% cycle_viz$uses)
  expect_setequal(cycle_viz$requires$adlb, c("AVISIT", "ADT"))
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
