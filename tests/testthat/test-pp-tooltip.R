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

test_that("a span is one Days row, as the timeline writes it", {
  from <- pp_tip_when(as.Date("2013-12-02"), 78, NA, "rday")
  to <- pp_tip_when(as.Date("2014-01-01"), 108, NA, "rday")
  rows <- pp_tip_span(from, to)
  expect_length(rows, 1L)
  expect_identical(rows[[1]]$label, "Days")
  expect_identical(rows[[1]]$value, "Day 78 to 108")
  expect_identical(rows[[1]]$meta, "31 days")

  one <- pp_tip_span(from, from)
  expect_identical(one[[1]]$value, "Day 78")
  expect_null(one[[1]]$meta)

  open <- pp_tip_span(from, pp_tip_when(), open = TRUE)
  expect_identical(open[[1]]$value, "Day 78 to ongoing")

  after <- pp_tip_span(from, to, after = "from 54 mg")
  expect_identical(after[[1]]$value, "Day 78 to 108, from 54 mg")

  # Date mode: the dates, the duration after them.
  rows <- pp_tip_span(pp_tip_when(as.Date("2013-12-02"), 78, NA, "date"),
                      pp_tip_when(as.Date("2014-01-01"), 108, NA, "date"))
  expect_identical(rows[[1]]$label, "Dates")
  expect_identical(rows[[1]]$value, "2013-12-02 to 2014-01-01")
})

test_that("a point is a Days row, the date after it", {
  row <- pp_tip_day(pp_tip_when(as.Date("2013-12-02"), 78, NA, "rday"))
  expect_identical(row$label, "Days")
  expect_identical(row$value, "Day 78")
  expect_identical(row$meta, "2013-12-02")
  row <- pp_tip_day(pp_tip_when(as.Date("2013-12-02"), 78, NA, "date"))
  expect_identical(row$label, "Date")
  expect_identical(row$value, "2013-12-02")
})

test_that("a span across treatment start counts no day zero", {
  rows <- pp_tip_span(pp_tip_when(NA, -1, mode = "rday"),
                      pp_tip_when(NA, 1, mode = "rday"))
  expect_identical(rows[[1]]$value, "Day -1 to 1")
  expect_identical(rows[[1]]$meta, "2 days")
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
  # The timeline's parts: the colour's level, then Type, Details, Days.
  expect_identical(tip$sub, "Severe")
  labels <- vapply(tip$rows, `[[`, "", "label")
  expect_identical(labels, c("Type", "Details", "Days"))
  expect_identical(tip$rows[[1]]$value, "Adverse event")
  expect_identical(tip$rows[[2]]$value,
                   "Psychiatric disorders, Not recovered/not resolved")
  expect_identical(tip$rows[[3]]$value, "Day 32 to ongoing")
})
