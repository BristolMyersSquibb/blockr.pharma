# The Patient Overview's data-gated lanes: exposure (adex) and the visit
# ruler (reconstructed from the findings tables). Absent tables drop their
# lane; nothing else changes.

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
  expect_true("EX" %in% unlist(chart$x$opts$yAxis$data))

  ex <- Filter(function(s) identical(s$name, "Exposure"),
               chart$x$opts$series)[[1]]
  # the two identical parameterized rows collapse into one period
  expect_length(ex$data, 2L)
  doses <- vapply(ex$data, function(d) as.character(d$value[[4]]), "")
  expect_setequal(doses, c("54 mg", "81 mg"))
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
  chart <- ov_render(ov_dm())
  expect_setequal(unlist(chart$x$opts$yAxis$data), c("TRT", "MS"))
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
