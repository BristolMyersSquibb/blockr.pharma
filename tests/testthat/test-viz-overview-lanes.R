# The Patient Overview's data-gated lanes: exposure (adex) and the visit
# ruler (reconstructed from the findings tables). Absent tables drop their
# lane; nothing else changes.
#
# ONE treatment lane. TRTSDT/TRTEDT are the min/max of the exposure records
# (100% / 98.4% on pharmaverseadam), so the envelope bar is what exposure
# already says, minus the holds -- it draws only as a fallback when the study
# ships no adex. Milestones share the lane rather than owning one: the two
# they used to lead with were the envelope's own endpoints as circles.

ov_dm <- function(...) {
  dm::dm(
    adsl = data.frame(
      USUBJID = "x", ACTARM = "Placebo",
      TRTSDT = as.Date("2020-01-01"), TRTEDT = as.Date("2020-06-01"),
      stringsAsFactors = FALSE
    ),
    ...
  )
}

ov_render <- function(dm_obj, mode = "date") {
  patient_overview_viz$render(
    dm_obj, time_range = pp_compute_time_range(dm_obj),
    settings = list(roles = list(arm = "ACTARM")),
    ref_ms = pp_compute_ref_ms(dm_obj), mode = mode
  )
}

series_names <- function(chart) {
  vapply(chart$x$opts$series, `[[`, character(1L), "name")
}

test_that("the exposure lane draws deduped dosing periods with the dose", {
  adex <- data.frame(
    USUBJID = "x",
    EXTRT = "XANOMELINE", EXDOSE = c(54, 54, 81), EXDOSU = "mg",
    ASTDT = as.Date(c("2020-01-01", "2020-01-01", "2020-03-01")),
    AENDT = as.Date(c("2020-02-28", "2020-02-28", "2020-06-01")),
    stringsAsFactors = FALSE
  )
  chart <- ov_render(ov_dm(adex = adex))

  expect_true("Exposure" %in% series_names(chart))
  # Exposure IS the treatment lane -- not a second one beside it
  expect_setequal(unlist(chart$x$opts$yAxis$data), "TRT")

  ex <- Filter(function(s) identical(s$name, "Exposure"),
               chart$x$opts$series)[[1]]
  # the two identical parameterized rows collapse into one period
  expect_length(ex$data, 2L)
  doses <- vapply(ex$data, function(d) as.character(d$value[[4]]), "")
  expect_setequal(doses, c("54 mg", "81 mg"))
})

ov_exposure <- function(chart) {
  Filter(function(s) identical(s$name, "Exposure"), chart$x$opts$series)[[1]]
}

test_that("a combination regimen draws both drugs, in a slot each", {
  # Same day, same dose string. Keying periods on (start, end, dose) made
  # these ONE bar and dropped a drug outright; clinical review caught it as a
  # Day 1 tooltip that answered for one of the two drugs given that day.
  adex <- data.frame(
    USUBJID = "x",
    EXTRT = c("STUDY DRUG A", "STUDY DRUG B"),
    EXDOSE = 200, EXDOSU = "mg",
    ASTDT = as.Date("2020-01-06"), AENDT = as.Date("2020-01-06"),
    stringsAsFactors = FALSE
  )
  ex <- ov_exposure(ov_render(ov_dm(adex = adex)))

  expect_length(ex$data, 2L)
  drugs <- vapply(ex$data, function(d) as.character(d$value[[5]]), "")
  expect_setequal(drugs, c("STUDY DRUG A", "STUDY DRUG B"))
  # Same lane, different slot within it -- so both are hoverable
  expect_setequal(vapply(ex$data, function(d) d$value[[3]], numeric(1L)), 0)
  expect_setequal(vapply(ex$data, function(d) d$value[[9]], integer(1L)),
                  c(0L, 1L))
  expect_true(all(vapply(ex$data, function(d) d$value[[8]], integer(1L)) == 2L))
})

test_that("one drug keeps the single slot it always had", {
  adex <- data.frame(
    USUBJID = "x", EXTRT = "XANOMELINE", EXDOSE = 54, EXDOSU = "mg",
    ASTDT = as.Date("2020-01-01"), AENDT = as.Date("2020-06-01"),
    stringsAsFactors = FALSE
  )
  ex <- ov_exposure(ov_render(ov_dm(adex = adex)))
  expect_equal(ex$data[[1]]$value[[8]], 1L)
  expect_equal(ex$data[[1]]$value[[9]], 0L)
})

test_that("one administration recorded in two units reports both", {
  # The only route to "show mg/kg as well as mg": the study says it and we
  # stop throwing the second row away. Nothing is derived from a weight.
  adex <- data.frame(
    USUBJID = "x", EXTRT = "STUDY DRUG A",
    EXDOSE = c(800, 10), EXDOSU = c("mg", "mg/kg"),
    ASTDT = as.Date("2020-01-06"), AENDT = as.Date("2020-01-06"),
    stringsAsFactors = FALSE
  )
  ex <- ov_exposure(ov_render(ov_dm(adex = adex)))
  expect_length(ex$data, 1L)
  expect_equal(as.character(ex$data[[1]]$value[[10]]), "800 mg · 10 mg/kg")
  # The bar itself is labelled with the first, which is what fits in it
  expect_equal(as.character(ex$data[[1]]$value[[4]]), "800 mg")
})

test_that("a dose bar reports the visit its own row names", {
  # Filed as CYCLE 1 DAY 8, infused a day late. Both facts, neither adjusted.
  adex <- data.frame(
    USUBJID = "x", EXTRT = "STUDY DRUG A", EXDOSE = 800, EXDOSU = "mg",
    AVISIT = "CYCLE 1 DAY 8",
    ASTDT = as.Date("2020-01-09"), AENDT = as.Date("2020-01-09"),
    stringsAsFactors = FALSE
  )
  ex <- ov_exposure(ov_render(ov_dm(adex = adex)))
  expect_equal(as.character(ex$data[[1]]$value[[6]]), "2020-01-09 (CYCLE 1 DAY 8)")
})

test_that("a dosing row's own label is printed, cycle or not", {
  adex <- data.frame(
    USUBJID = "x", EXTRT = "XANOMELINE", EXDOSE = 54, EXDOSU = "mg",
    AVISIT = "WEEK 2",
    ASTDT = as.Date("2020-01-09"), AENDT = as.Date("2020-01-09"),
    stringsAsFactors = FALSE
  )
  ex <- ov_exposure(ov_render(ov_dm(adex = adex)))
  expect_equal(as.character(ex$data[[1]]$value[[6]]), "2020-01-09 (WEEK 2)")
})

test_that("exposure present -> no envelope bar drawn beside it", {
  adex <- data.frame(
    USUBJID = "x", EXTRT = "XANOMELINE", EXDOSE = 54, EXDOSU = "mg",
    ASTDT = as.Date("2020-01-01"), AENDT = as.Date("2020-06-01"),
    stringsAsFactors = FALSE
  )
  chart <- ov_render(ov_dm(adex = adex))
  expect_false("Treatment" %in% series_names(chart))
})

test_that("no exposure -> the envelope bar is the fallback, and carries the arm", {
  chart <- ov_render(ov_dm())
  expect_true("Treatment" %in% series_names(chart))
  trt <- Filter(function(s) identical(s$name, "Treatment"),
                chart$x$opts$series)[[1]]
  expect_length(trt$data, 1L)
  expect_match(as.character(trt$renderItem), "Placebo")
})

test_that("treatment start/end are not redrawn as milestones", {
  # They were circles sitting under the ends of the bar that drew them.
  chart <- ov_render(ov_dm())
  ms <- Filter(function(s) identical(s$name, "Milestones"), chart$x$opts$series)
  expect_length(ms, 0L)
})

test_that("real milestones ride on the treatment lane, painted over the bars", {
  adex <- data.frame(
    USUBJID = "x", EXTRT = "XANOMELINE", EXDOSE = 54, EXDOSU = "mg",
    ASTDT = as.Date("2020-01-01"), AENDT = as.Date("2020-06-01"),
    stringsAsFactors = FALSE
  )
  d <- dm::dm(
    adsl = data.frame(
      USUBJID = "x", ACTARM = "Placebo",
      TRTSDT = as.Date("2020-01-01"), TRTEDT = as.Date("2020-06-01"),
      RFENDT = as.Date("2020-07-01"), DTHDT = as.Date("2020-08-01"),
      stringsAsFactors = FALSE
    ),
    adex = adex
  )
  chart <- ov_render(d)
  expect_setequal(unlist(chart$x$opts$yAxis$data), "TRT")

  ms <- Filter(function(s) identical(s$name, "Milestones"),
               chart$x$opts$series)[[1]]
  kinds <- vapply(ms$data, function(x) as.character(x$value[[3]]), "")
  expect_setequal(kinds, c("eos", "death"))
  # ... on the same lane as the dose bars
  lanes <- unique(vapply(ms$data, function(x) as.numeric(x$value[[2]]), 0))
  expect_equal(lanes, 0)
  # ... and after them in series order, or the bars would paint over them
  expect_gt(which(series_names(chart) == "Milestones"),
            which(series_names(chart) == "Exposure"))
})

test_that("the visit ruler ticks the visits found in findings tables", {
  advs <- data.frame(
    USUBJID = "x", PARAMCD = "SYSBP", AVAL = 120,
    AVISIT = c("Baseline", "Week 2", "Week 2"),
    ADT = as.Date(c("2020-01-01", "2020-01-15", "2020-01-16")),
    stringsAsFactors = FALSE
  )
  chart <- ov_render(ov_dm(advs = advs))

  expect_true("Visits" %in% series_names(chart))
  vis <- Filter(function(s) identical(s$name, "Visits"),
                chart$x$opts$series)[[1]]
  expect_length(vis$data, 2L)  # one tick per visit, earliest date wins
  labels <- vapply(vis$data, function(d) as.character(d$value[[3]]), "")
  expect_setequal(labels, c("Baseline", "Week 2"))
})

test_that("absent tables drop their lanes", {
  # ADSL alone: one lane, and it is the fallback envelope. No exposure lane,
  # no milestone lane -- the three that all described the treatment span are
  # one now.
  chart <- ov_render(ov_dm())
  expect_setequal(unlist(chart$x$opts$yAxis$data), "TRT")
})

test_that("pp_visit_schedule merges tables and prefers the earliest date", {
  tbls <- list(
    advs = data.frame(AVISIT = c("Week 2", ""), ADT = as.Date("2020-01-20"),
                      stringsAsFactors = FALSE),
    adlb = data.frame(AVISIT = "Week 2", ADT = as.Date("2020-01-15"),
                      ADY = 15, stringsAsFactors = FALSE)
  )
  out <- pp_visit_schedule(tbls)
  expect_identical(out$visit, "Week 2")
  expect_identical(out$date, as.Date("2020-01-15"))
})
