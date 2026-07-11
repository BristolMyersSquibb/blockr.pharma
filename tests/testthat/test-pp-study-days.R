# Relative-day mode plots study days. A study that ships ASTDY but no ASTDT
# must render from the day directly rather than have a date reconstructed for
# it, and must land on exactly the same axis positions as a study that ships
# both. The trap is the scale: pp_xval()'s rday output is continuous (treatment
# start = 1, the day before = 0) while an ADaM *DY skips zero (the day before =
# -1), so a raw *DY plotted as-is sits one day left across the whole
# pre-treatment region.

test_that("pp_day_to_x maps *DY onto the continuous axis", {
  # on and after treatment start the two scales coincide...
  expect_equal(pp_day_to_x(c(1, 2, 10)), c(1, 2, 10))
  # ...before it, *DY skips zero and the continuous scale does not
  expect_equal(pp_day_to_x(c(-1, -2, -10)), c(0, -1, -9))
  expect_true(is.na(pp_day_to_x(NA)))
})

test_that("the day-native axis agrees with the date-derived one", {
  skip_if_not_installed("pharmaverseadam")
  sl <- as.data.frame(pharmaverseadam::adsl)
  ae <- as.data.frame(pharmaverseadam::adae)
  ok <- !is.na(ae$ASTDT) & !is.na(ae$ASTDY)
  refs <- pp_ms_ts(sl$TRTSDT[match(ae$USUBJID, sl$USUBJID)])

  from_date <- mapply(function(d, r) pp_xval(d, r, "rday"),
                      ae$ASTDT[ok], refs[ok])
  from_day <- pp_day_to_x(ae$ASTDY[ok])

  # the whole point: they must not diverge anywhere, least of all on the
  # pre-treatment events, which is where the two scales disagree
  expect_gt(sum(ae$ASTDY[ok] < 0), 0)
  expect_equal(as.numeric(from_date), as.numeric(from_day))

  # and the naive version (plotting *DY raw) would have been wrong there
  expect_gt(sum(as.numeric(ae$ASTDY[ok]) != as.numeric(from_date)), 0)
})

# A study shaped like prod: AE study days, no AE dates.
day_only_dm <- function() {
  dm::dm(
    adsl = data.frame(
      USUBJID = "x",
      TRTSDT = as.Date("2020-01-01"), TRTEDT = as.Date("2020-06-01"),
      stringsAsFactors = FALSE
    ),
    adae = data.frame(
      USUBJID = "x",
      AEDECOD = c("HEADACHE", "NAUSEA"),
      ASTDY = c(10L, -3L), AENDY = c(14L, -1L),
      AESEV = c("SEVERE", "MILD"),
      stringsAsFactors = FALSE
    )
  )
}

test_that("the AE gantt renders from study days with no dates present", {
  res <- pp_resolve_requires(day_only_dm(), ae_gantt_viz)
  expect_true(isTRUE(res$ok))

  chart <- ae_gantt_viz$render(res$dm, time_range = NULL, settings = list(),
                               ref_ms = NA_real_, mode = "rday")
  expect_s3_class(chart, "htmlwidget")

  starts <- sort(vapply(chart$x$opts$series[[1]]$data,
                        function(d) d$value[[1]], numeric(1)))
  # D10 sits at 10; D-3 sits at -2 on the continuous scale, not -3
  expect_equal(starts, c(-2, 10))
})

test_that("AESTDY/AEENDY are accepted as the SDTM spelling", {
  dm_obj <- day_only_dm()
  ae <- as.data.frame(dm::dm_get_tables(dm_obj)$adae)
  names(ae)[names(ae) == "ASTDY"] <- "AESTDY"
  names(ae)[names(ae) == "AENDY"] <- "AEENDY"
  dm2 <- dm::dm(adsl = as.data.frame(dm::dm_get_tables(dm_obj)$adsl), adae = ae)

  res <- pp_resolve_requires(dm2, ae_gantt_viz)
  expect_true(isTRUE(res$ok))
  expect_true("ASTDY" %in% colnames(dm::dm_get_tables(res$dm)$adae))
})

test_that("the overview AE lane also plots study days", {
  res <- pp_resolve_requires(day_only_dm(), patient_overview_viz)
  expect_true(isTRUE(res$ok))
  chart <- patient_overview_viz$render(
    res$dm, time_range = NULL, settings = list(),
    ref_ms = pp_ms_ts(as.Date("2020-01-01")), mode = "rday"
  )
  ae <- Filter(function(s) identical(s$name, "Adverse Events"),
               chart$x$opts$series)
  expect_length(ae, 1L)
  starts <- sort(vapply(ae[[1]]$data, function(d) d$value[[1]], numeric(1)))
  expect_equal(starts, c(-2, 10))
})

test_that("date mode says so rather than plotting a study-day-only AE table", {
  res <- pp_resolve_requires(day_only_dm(), ae_gantt_viz)
  chart <- ae_gantt_viz$render(res$dm, time_range = NULL, settings = list(),
                               ref_ms = NA_real_, mode = "date")
  expect_match(paste(unlist(chart$x$opts$title), collapse = " "),
               "relative day")
})

test_that("a table with neither a date nor a day is reported as missing both", {
  dm_obj <- dm::dm(
    adsl = data.frame(USUBJID = "x", TRTSDT = as.Date("2020-01-01"),
                      TRTEDT = as.Date("2020-06-01")),
    adae = data.frame(USUBJID = "x", AEDECOD = "HEADACHE")
  )
  res <- pp_resolve_requires(dm_obj, ae_gantt_viz)
  expect_false(res$ok)
  expect_match(res$msg, "one of ASTDT, ASTDY in adae")
})

test_that("the axis range covers events that only have a study day", {
  # An AE at D400 with no ASTDT must still be inside the axis, even though the
  # treatment window closes long before it.
  dm_obj <- dm::dm(
    adsl = data.frame(USUBJID = "x",
                      TRTSDT = as.Date("2020-01-01"),
                      TRTEDT = as.Date("2020-02-01")),
    adae = data.frame(USUBJID = "x", AEDECOD = "LATE AE",
                      ASTDY = 400L, AENDY = 410L)
  )
  tr <- pp_compute_time_range(dm_obj)
  ref <- pp_compute_ref_ms(dm_obj)
  axis <- pp_time_axis(tr, ref, "rday")
  expect_gte(axis$max, 400)
  expect_lte(axis$min, 1)
})
