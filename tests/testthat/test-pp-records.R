# The record-selection seam. See R/pp-records.R for why derived records are
# not one kind of thing.

fnd <- function(aval, dtype, adt = as.Date("2024-01-05")) {
  data.frame(USUBJID = "S1", PARAMCD = "NEUT", ADT = adt,
             AVAL = aval, DTYPE = dtype, stringsAsFactors = FALSE)
}

test_that("a table without DTYPE passes through untouched", {
  # SDTM findings domains have no derived records by construction.
  tbl <- data.frame(USUBJID = "S1", PARAMCD = "NEUT", AVAL = 2.7)
  out <- pp_select_records(tbl)
  expect_equal(nrow(out), 1L)
  expect_equal(attr(out, "pp_suppressed"), 0L)
})

test_that("restatements of a collected record are dropped", {
  # Real pharmaverseadam shape: MINIMUM/MAXIMUM/LOV re-stamp an observed
  # value onto a pseudo-visit. The collected row survives.
  tbl <- rbind(
    fnd(2.7, NA), fnd(2.7, "MINIMUM"), fnd(2.7, "MAXIMUM"), fnd(2.7, "LOV")
  )
  out <- pp_select_records(tbl)
  expect_equal(out$AVAL, 2.7)
  expect_equal(attr(out, "pp_suppressed"), 3L)
})

test_that("an AVERAGE replaces the replicates it summarizes", {
  # Two BP readings plus their ADaM-stored mean. Keeping all three counts the
  # mean twice; dropping it shows raw readings and no summary.
  tbl <- rbind(fnd(180, NA), fnd(140, NA), fnd(160, "AVERAGE"))
  out <- pp_select_records(tbl)
  expect_equal(out$AVAL, 160)
  expect_true(all(attr(out, "pp_marked")))
})

test_that("an AVERAGE only displaces replicates sharing its key", {
  tbl <- rbind(
    fnd(180, NA, as.Date("2024-01-05")),
    fnd(160, "AVERAGE", as.Date("2024-01-05")),
    fnd(120, NA, as.Date("2024-02-05"))
  )
  out <- pp_select_records(tbl)
  expect_equal(out$AVAL, c(160, 120))
})

test_that("carried-forward values are drawn but marked", {
  # LOCF carries information the collected rows do not, so it is shown --
  # but a reviewer must be able to tell it from a measurement.
  tbl <- rbind(fnd(2.7, NA), fnd(3.1, "LOCF", as.Date("2024-02-05")))
  out <- pp_select_records(tbl)
  expect_equal(out$AVAL, c(2.7, 3.1))
  expect_equal(attr(out, "pp_marked"), c(FALSE, TRUE))
})

test_that("an unrecognized DTYPE is drawn and marked, not dropped", {
  # Where "in doubt" actually applies. A sponsor-specific derivation is the
  # case we understand least, so the reviewer decides whether it counts --
  # dropping it would make that call for them, silently.
  tbl <- rbind(fnd(2.7, NA), fnd(0, "SPONSORTHING"))
  out <- pp_select_records(tbl)
  expect_equal(out$AVAL, c(2.7, 0))
  expect_equal(attr(out, "pp_marked"), c(FALSE, TRUE))
  expect_equal(attr(out, "pp_suppressed"), 0L)
})

test_that("known restatements stay dropped -- they are not doubtful", {
  # Contrast with the test above: a MAXIMUM row is a value-identical copy of
  # a collected row at the same date. Showing it draws a point on top of a
  # point. That is de-duplication, not hiding.
  tbl <- rbind(fnd(2.7, NA), fnd(2.7, "MAXIMUM"))
  out <- pp_select_records(tbl)
  expect_equal(out$AVAL, 2.7)
  expect_equal(attr(out, "pp_suppressed"), 1L)
})

test_that("pp_as_numeric never turns an unparseable value into 0", {
  expect_equal(pp_as_numeric(c("2.7", "3.1")), c(2.7, 3.1))
  expect_true(is.na(pp_as_numeric("not a number")))
  # as.numeric() on a factor returns LEVEL CODES -- a silently wrong chart,
  # which is worse than the crash it would replace.
  expect_equal(pp_as_numeric(factor(c("2.7", "3.1"))), c(2.7, 3.1))
})

test_that("the seam holds on real ADaM data", {
  skip_if_not_installed("pharmaverseadam")
  d <- as.data.frame(pharmaverseadam::adlb)
  d <- d[d$USUBJID == d$USUBJID[1] & d$PARAMCD == "BUN", ]
  out <- pp_select_records(d)
  # Every drawn row is a collected measurement; the derived restatements go.
  expect_true(all(is.na(out$DTYPE)))
  expect_gt(attr(out, "pp_suppressed"), 0L)
  # No measurement was lost.
  expect_equal(nrow(out), sum(is.na(d$DTYPE)))
})
