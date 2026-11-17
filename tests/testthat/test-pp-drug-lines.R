# The patient profile and the patient timeline draw a drug's ADEX records
# the same way and infer nothing: a period is a line, a one-day record a
# ring on its day, records of one dose that touch join.

dl_adsl <- function() {
  data.frame(
    USUBJID = "x", ACTARM = "Arm A",
    TRTSDT = as.Date("2020-01-01"), TRTEDT = as.Date("2020-06-16"),
    stringsAsFactors = FALSE
  )
}

# An infusion every 21 days, cycle 5 skipped, and an oral drug in periods.
dl_inf <- c(1, 22, 43, 64, 106, 127, 148)
dl_adex <- function() {
  t0 <- as.Date("2020-01-01")
  ex <- rbind(
    data.frame(USUBJID = "x", EXTRT = "DRUG A", EXDOSE = 200, EXDOSU = "mg",
               ASTDT = t0 + dl_inf - 1, AENDT = t0 + dl_inf - 1),
    data.frame(USUBJID = "x", EXTRT = "DRUG B", EXDOSE = c(54, 81, 54),
               EXDOSU = "mg", ASTDT = t0 + c(0, 14, 120),
               AENDT = t0 + c(13, 119, 167))
  )
  ex$ASTDY <- as.numeric(ex$ASTDT - t0) + 1
  ex$AENDY <- as.numeric(ex$AENDT - t0) + 1
  ex
}

dl_render <- function(dm_obj, mode) {
  patient_overview_viz$render(
    dm_obj, time_range = pp_compute_time_range(dm_obj),
    settings = list(roles = list(arm = "ACTARM")),
    ref_ms = pp_compute_ref_ms(dm_obj), mode = mode
  )
}

dl_exposure <- function(chart) {
  Filter(function(s) identical(s$name, "Exposure"), chart$x$opts$series)[[1]]
}

dl_runs <- function(start, end, dose = 10, open = FALSE) {
  pp_ex_runs(data.frame(USUBJID = "x", drug = "D", start = start, end = end,
                        dose = dose, unit = "mg", open = open,
                        stringsAsFactors = FALSE))
}

test_that("a drug given in cycles is a ring per dose, nothing between", {
  p <- pp_drug_lines(dl_adex(), dl_adsl(), use_day = TRUE)
  a <- p[p$drug == "DRUG A", ]
  expect_equal(a$u0, dl_inf)
  expect_equal(a$u1, dl_inf)
  expect_true(all(a$dot))
  b <- p[p$drug == "DRUG B", ]
  expect_equal(b$u0, c(1, 15, 121))
  expect_equal(b$u1, c(14, 120, 168))
  expect_false(any(b$dot))
  expect_equal(b$after, c("", "from 54 mg", "from 81 mg"))
})

test_that("records of one dose that touch join, nothing else does", {
  # Daily one-day records, a missed day, then a new dose.
  r <- dl_runs(c(1, 2, 3, 5, 6, 7), c(1, 2, 3, 5, 6, 7),
               dose = c(10, 10, 10, 10, 10, 20))
  expect_equal(r$start, c(1, 5, 7))
  expect_equal(r$end, c(3, 6, 7))
  expect_equal(r$daily, c(TRUE, TRUE, FALSE))
  expect_equal(r$dot, c(FALSE, FALSE, TRUE))
  expect_equal(r$changed, c(FALSE, FALSE, TRUE))
  # Periods that touch at the same dose join; a one-day gap does not.
  r <- dl_runs(c(1, 15, 31), c(14, 29, 40))
  expect_equal(r$start, c(1, 31))
  expect_equal(r$end, c(29, 40))
})

test_that("dates and study days give the same lines", {
  by_day <- pp_drug_lines(dl_adex(), dl_adsl(), use_day = TRUE)
  by_date <- pp_drug_lines(dl_adex(), dl_adsl(), use_day = FALSE)
  t0 <- as.numeric(as.Date("2020-01-01"))
  expect_equal(by_date$u0 - t0 + 1, by_day$u0)
  expect_equal(by_date$u1 - t0 + 1, by_day$u1)
})

test_that("records repeated per parameter or unit draw once", {
  ex <- dl_adex()
  ex <- rbind(transform(ex, PARAMCD = "DOSE"), transform(ex, PARAMCD = "ADJ"))
  expect_equal(nrow(pp_drug_lines(ex, dl_adsl(), use_day = TRUE)), 10L)
})

test_that("the profile lane draws rings and lines in both axis modes", {
  d <- dm::dm(adsl = dl_adsl(), adex = dl_adex())
  ex <- dl_exposure(dl_render(d, "rday"))
  dot <- vapply(ex$data, function(r) isTRUE(r$value[[9]]), NA)
  x0 <- vapply(ex$data, function(r) r$value[[1]], 0)
  expect_equal(sort(x0[dot]), dl_inf)
  expect_equal(sum(!dot), 3L)
  ex <- dl_exposure(dl_render(d, "date"))
  expect_equal(sum(vapply(ex$data, function(r) isTRUE(r$value[[9]]), NA)),
               7L)
})

test_that("the patient timeline draws the same records", {
  p <- pp_drug_lines(dl_adex(), dl_adsl(), use_day = TRUE)
  f <- patient_timeline_frame(list(adsl = dl_adsl(), adex = dl_adex()),
                              layers = "Treatment")
  f <- f[!is.na(f$Start) & f$Type %in% "Treatment", ]
  key <- function(name, s, e) sort(paste(name, s, e))
  expect_equal(key(f$.lane, f$Start, f$End), key(p$name, p$u0, p$u1))
  expect_equal(sum(f$.style %in% "dot"), 7L)
  expect_true(all(f$Days[f$.style %in% "dot"] ==
                    paste("Day", dl_inf)))
})

test_that("a saved timeline with the old doses setting still restores", {
  blk <- new_timeline_block(layers = "Treatment", doses = "Plain")
  sv <- blockr.core::blockr_ser(blk)$payload$script_values
  expect_null(sv$doses)
  blk <- new_timeline_block(layers = "Treatment",
                            script_values = list(doses = "Gaps"))
  expect_s3_class(blk, "timeline_block")
})
