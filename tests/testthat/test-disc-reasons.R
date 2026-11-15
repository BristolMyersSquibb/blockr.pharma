# The two discontinuation reasons: treatment (DCTREAS) at the last dose, end
# of study (DCSREAS) at the end of study, each from its own column.

disc_toy <- function(adsl_extra = list()) {
  d <- pt_toy()
  d$adsl$DCTREAS <- NULL
  for (nm in names(adsl_extra)) d$adsl[[nm]] <- adsl_extra[[nm]]
  d
}

disc_tags <- function(f) {
  f[!is.na(f$.tag) & !f$Event %in% "Dose change",
    c("USUBJID", ".event_x", "Event", ".tag", "Type")]
}

test_that("the study reason never stands in for the treatment reason", {
  d <- disc_toy(list(
    DCSREAS = c("WITHDRAWAL BY SUBJECT", NA, NA),
    EOSDT = as.Date(c("2024-03-20", NA, NA))
  ))
  tg <- disc_tags(patient_timeline_frame(d))
  expect_identical(nrow(tg), 1L)
  expect_identical(tg$Type, "End of study")
  expect_identical(as.character(tg$Event),
                   "End of study: Withdrawal by subject")
  # Day 80: 2024-03-20 from 2024-01-01, day 1 the first dose.
  expect_identical(tg$.event_x, 80)
})

test_that("both reasons draw, each at its own date", {
  d <- disc_toy(list(
    DCTREAS = c("ADVERSE EVENT", NA, NA),
    DCSREAS = c("WITHDRAWAL BY SUBJECT", NA, NA),
    EOSDT = as.Date(c("2024-03-20", NA, NA))
  ))
  f <- patient_timeline_frame(d)
  tg <- disc_tags(f)
  expect_identical(tg$.tag, c("AE", "WD"))
  expect_identical(tg$.event_x, c(60, 80))
  expect_identical(tg$Type, c("Treatment discontinuation", "End of study"))
  # Treatment reasons head the legend's tags.
  lv <- levels(f$Event)
  expect_lt(match("Adverse event", lv),
            match("End of study: Withdrawal by subject", lv))

  trt <- disc_tags(patient_timeline_frame(d, disc_show = "treatment"))
  expect_identical(trt$.tag, "AE")
  eos <- disc_tags(patient_timeline_frame(d, disc_show = "end of study"))
  expect_identical(eos$.tag, "WD")
})

test_that("an end-of-study reason that repeats the treatment one is left out", {
  d <- disc_toy(list(
    DCTREAS = c("ADVERSE EVENT", NA, NA),
    DCSREAS = c("ADVERSE EVENT", "COMPLETED", NA),
    EOSDT = as.Date(c("2024-03-05", "2024-01-25", NA))
  ))
  tg <- disc_tags(patient_timeline_frame(d))
  expect_identical(tg$.tag, "AE")
  expect_identical(tg$Type, "Treatment discontinuation")
})

test_that("the end of study falls back to RFENDT and respects the cutoff", {
  d <- disc_toy(list(
    DCSREAS = c("LOST TO FOLLOW-UP", NA, NA),
    RFENDT = as.Date(c("2024-03-10", NA, NA))
  ))
  tg <- disc_tags(patient_timeline_frame(d))
  expect_identical(tg$.event_x, 70)
  cut <- patient_timeline_frame(d, cutoff = as.Date("2024-03-01"))
  expect_identical(nrow(disc_tags(cut)), 0L)
})

test_that("reasons under other names are found by their labels", {
  adsl <- data.frame(
    USUBJID = "A",
    TRTDCRS = "ADVERSE EVENT",
    TRTDCRSP = "rash",
    STDYDCR = "WITHDRAWAL BY SUBJECT"
  )
  attr(adsl$TRTDCRS, "label") <- "Reason for Discontinuation of Treatment"
  attr(adsl$TRTDCRSP, "label") <-
    "Reason Spec for Discont of Treatment"
  attr(adsl$STDYDCR, "label") <- "Reason for Discontinuation from Study"
  out <- pp_normalize_tables(list(adsl = adsl))$tables$adsl
  expect_identical(as.vector(out$DCTREAS), "ADVERSE EVENT")
  expect_identical(as.vector(out$DCSREAS), "WITHDRAWAL BY SUBJECT")

  # Without a treatment column, the study one stays where it is.
  only <- pp_normalize_tables(list(adsl = adsl[, c("USUBJID", "STDYDCR")]))
  expect_false("DCTREAS" %in% names(only$tables$adsl))
})

test_that("a treatment disposition is the treatment reason", {
  # As a study may ship it: the study reason under DSTERM without a label,
  # the treatment reason labelled as a disposition, and a death column
  # that also holds "ADVERSE EVENT".
  d <- disc_toy(list(
    DSTERM = c("WITHDRAWAL BY SUBJECT", NA, NA),
    DDTRTCOD = c("ADVERSE EVENT", NA, NA),
    DSTRTCOD = c("ADVERSE EVENT", NA, NA),
    EOSDT = as.Date(c("2024-03-20", NA, NA))
  ))
  attr(d$adsl$DDTRTCOD, "label") <- "Death during Treatment"
  attr(d$adsl$DSTRTCOD, "label") <- "Treatment Disposition"
  out <- pp_normalize_tables(list(adsl = d$adsl))$tables$adsl
  expect_identical(as.vector(out$DCTREAS)[[1L]], "ADVERSE EVENT")
  expect_identical(as.vector(out$DCSREAS)[[1L]], "WITHDRAWAL BY SUBJECT")
  tg <- disc_tags(patient_timeline_frame(d))
  expect_identical(paste(tg$.tag, tg$Type),
                   c("AE Treatment discontinuation", "WD End of study"))

  # A death column alone is no reason.
  d$adsl$DSTRTCOD <- NULL
  out <- pp_normalize_tables(list(adsl = d$adsl))$tables$adsl
  expect_false("DCTREAS" %in% names(out))
})

test_that("the profile reads the two reasons apart", {
  sl <- data.frame(
    USUBJID = "A",
    TRTEDT = as.Date("2024-02-29"),
    EOSDT = as.Date("2024-03-20"),
    DCSREAS = "WITHDRAWAL BY SUBJECT"
  )
  expect_null(pp_disc_reason(sl))
  eos <- pp_eos_reason(sl)
  expect_identical(eos$tag, "WD")
  expect_identical(eos$date, as.Date("2024-03-20"))

  sl$DCTREAS <- "ADVERSE EVENT"
  expect_identical(pp_disc_reason(sl)$tag, "AE")
  expect_identical(pp_eos_reason(sl)$tag, "WD")
  sl$DCSREAS <- "ADVERSE EVENT"
  expect_null(pp_eos_reason(sl))
})

test_that("the repeat check compares each patient's own reasons", {
  d <- disc_toy(list(
    DCTREAS = c("ADVERSE EVENT", "WITHDRAWAL BY SUBJECT", NA),
    DCSREAS = c("WITHDRAWAL BY SUBJECT", "WITHDRAWAL BY SUBJECT", NA),
    EOSDT = as.Date(c("2024-03-20", "2024-01-22", NA))
  ))
  tg <- disc_tags(patient_timeline_frame(d))
  expect_identical(paste(tg$USUBJID, tg$.tag),
                   c("A AE", "A WD", "B WD"))
})

test_that("the profile's axis reaches the end-of-study tag", {
  d <- disc_toy(list(
    DCTREAS = c("ADVERSE EVENT", NA, NA),
    DCSREAS = c("WITHDRAWAL BY SUBJECT", NA, NA),
    EOSDT = as.Date(c("2024-06-01", NA, NA))
  ))
  d$adsl <- d$adsl[1, ]
  for (nm in c("adae", "adcm", "adex", "adrs")) {
    d[[nm]] <- d[[nm]][d[[nm]]$USUBJID == "A", ]
  }
  dm_obj <- pp_normalize_dm(do.call(dm::dm, d))
  expect_identical(pp_compute_time_range(dm_obj)[[2L]], as.Date("2024-06-01"))
})
