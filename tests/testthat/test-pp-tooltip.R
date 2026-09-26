# The tooltip builders (R/pp-tooltip.R). What each panel puts in them is
# tested with the panel.

test_that("a row with nothing to say drops out", {
  expect_null(pp_tip_row("Outcome", NA))
  expect_null(pp_tip_row("Outcome", ""))
  tip <- pp_tip("Syncope", rows = list(pp_tip_row("Outcome", NA),
                                       pp_tip_row("Severity", "Severe")))
  expect_length(tip$rows, 1L)
  expect_null(tip$color)
  expect_null(tip$sub)
})

test_that("a day reads as the timeline shows it, the other form as meta", {
  ref <- as.numeric(as.POSIXct("2013-10-01", tz = "UTC")) * 1000
  rday <- pp_tip_when(as.Date("2013-10-17"), 17, ref, "rday")
  expect_identical(rday$main, "D17")
  expect_identical(rday$meta, "2013-10-17")
  date <- pp_tip_when(as.Date("2013-10-17"), 17, ref, "date")
  expect_identical(date$main, "2013-10-17")
  expect_identical(date$meta, "D17")
  # no day column: derived from the date, as the axis does
  expect_identical(pp_tip_when(as.Date("2013-10-17"), NA, ref, "rday")$main,
                   "D17")
})

test_that("a span is From and To with its length, one day is Day", {
  from <- pp_tip_when(as.Date("2013-12-02"), 78, NA, "rday")
  to <- pp_tip_when(as.Date("2014-01-01"), 108, NA, "rday")
  rows <- pp_tip_span(from, to)
  expect_identical(vapply(rows, `[[`, "", "label"), c("From", "To"))
  expect_identical(rows[[2]]$meta, "31 days")

  one <- pp_tip_span(from, from)
  expect_length(one, 1L)
  expect_identical(one[[1]]$label, "Day")

  open <- pp_tip_span(from, pp_tip_when(), open = TRUE)
  expect_identical(open[[2]]$value, "ongoing")
})

test_that("a span across treatment start counts no day zero", {
  rows <- pp_tip_span(pp_tip_when(NA, -1), pp_tip_when(NA, 1))
  expect_identical(rows[[2]]$meta, "2 days")
})

test_that("capitals become sentence case, mixed case is kept", {
  expect_identical(pp_tip_case("NOT RECOVERED/NOT RESOLVED"),
                   "Not recovered/not resolved")
  expect_identical(pp_tip_case("eGFR"), "eGFR")
  expect_identical(pp_ae_sev_word("3"), "Grade 3")
  expect_identical(pp_ae_sev_word("MODERATE"), "Moderate")
})

test_that("numbers keep three significant figures, a change its sign", {
  expect_identical(pp_tip_num(190), "190")
  expect_identical(pp_tip_num(3.14159), "3.14")
  expect_identical(pp_tip_num(7, signed = TRUE), "+7")
  expect_identical(pp_tip_num(-2.5, signed = TRUE), "-2.5")
})

test_that("the AE panel carries its tooltip on each bar", {
  adae <- data.frame(
    USUBJID = "S-1", AEDECOD = "AGITATION", AEBODSYS = "PSYCHIATRIC DISORDERS",
    AESEV = "SEVERE", AESER = "N", AEOUT = "NOT RECOVERED/NOT RESOLVED",
    ASTDT = as.Date("2024-02-01"), ASTDY = 32, stringsAsFactors = FALSE
  )
  adsl <- data.frame(USUBJID = "S-1", TRTSDT = as.Date("2023-12-31"),
                     stringsAsFactors = FALSE)
  d <- pp_normalize_dm(dm::dm(adsl = adsl, adae = adae))
  roles <- pp_resolve_roles(d)
  chart <- ae_gantt_viz$render(d, pp_compute_time_range(d),
                               list(roles = roles), pp_compute_ref_ms(d),
                               "rday")
  tip <- chart$x$opts$series[[1]]$data[[1]]$tip
  expect_identical(tip$head, "Agitation")
  expect_identical(tip$sub, "Psychiatric disorders")
  labels <- vapply(tip$rows, `[[`, "", "label")
  expect_identical(labels, c("Severity", "From", "To", "Outcome"))
  expect_false("Serious" %in% labels)
  expect_identical(tip$rows[[2]]$value, "D32")
  expect_identical(tip$rows[[3]]$value, "ongoing")
})
