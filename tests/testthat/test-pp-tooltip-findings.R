# The tooltip of a dot on a lab or vital-sign card (pp_findings_tip()).

sysbp_row <- function(...) {
  r <- data.frame(
    USUBJID = "S1",
    PARAMCD = "SYSBP",
    PARAM = "SYSTOLIC BLOOD PRESSURE (mmHg)",
    AVAL = 167, BASE = 160, CHG = 7, PCHG = 4.375,
    A1LO = 90, A1HI = 140, ANRIND = "H",
    ADT = as.Date("2013-10-01"), ADY = 16, AVISIT = "Week 2",
    DTYPE = NA_character_,
    stringsAsFactors = FALSE
  )
  args <- list(...)
  for (nm in names(args)) r[[nm]] <- args[[nm]]
  r
}

rows_of <- function(tip) {
  lapply(tip$rows, function(r) c(r$label, r$value, r$meta %||% ""))
}

test_that("a measured value reads name, value with unit, range with flag", {
  tip <- pp_findings_tip(sysbp_row(AVAL = 190), "AVAL", color = "#dc2626",
                         mode = "rday")
  expect_identical(tip$head, "Systolic blood pressure")
  expect_identical(tip$color, "#dc2626")
  expect_identical(rows_of(tip), list(
    c("Analysis value", "190 mmHg", ""),
    c("Normal range", "90 to 140", "high"),
    c("Baseline", "160", ""),
    c("Day", "D16", "2013-10-01"),
    c("Visit", "Week 2", "")
  ))
  expect_null(tip$note)
})

test_that("date mode puts the date first and the day after it", {
  tip <- pp_findings_tip(sysbp_row(), "AVAL", mode = "date")
  day <- Filter(function(r) r$label == "Day", tip$rows)[[1]]
  expect_identical(c(day$value, day$meta), c("2013-10-01", "D16"))
})

test_that("a change scale leads with the signed change, then value and baseline", {
  tip <- pp_findings_tip(sysbp_row(), "CHG", mode = "rday")
  expect_identical(rows_of(tip)[1:3], list(
    c("Change from baseline", "+7 mmHg", ""),
    c("Analysis value", "167 mmHg", ""),
    c("Baseline", "160", "")
  ))
  # The range and the flag describe the measured value, not a change.
  labels <- vapply(tip$rows, `[[`, "", "label")
  expect_false("Normal range" %in% labels)
  expect_false(any(vapply(tip$rows, function(r) identical(r$meta, "high"),
                          logical(1))))

  pct <- pp_findings_tip(sysbp_row(), "PCHG")
  expect_identical(pct$rows[[1]]$label, "Percent change from baseline")
  expect_identical(pct$rows[[1]]$value, "+4.38%")
  neg <- pp_findings_tip(sysbp_row(CHG = -12), "CHG")
  expect_identical(neg$rows[[1]]$value, "-12 mmHg")
})

test_that("a derived record says so in the note", {
  tip <- pp_findings_tip(sysbp_row(DTYPE = "AVERAGE"), "AVAL")
  expect_identical(tip$note, "Derived by the study (average), not measured")
})

test_that("names, never column codes", {
  tip <- pp_findings_tip(sysbp_row(), "CHG")
  txt <- unlist(tip)
  expect_false(any(grepl("\\b(AVAL|CHG|PCHG|BASE|ANRIND|A1LO)\\b", txt)))
  expect_false(any(grepl("Ref:|:", vapply(tip$rows, `[[`, "", "label"))))
})

test_that("the flag rides on the value when the study gives no range", {
  tip <- pp_findings_tip(sysbp_row(A1LO = NA_real_, ANRIND = "LOW"), "AVAL")
  expect_identical(rows_of(tip)[[1]], c("Analysis value", "167 mmHg", "low"))
  expect_false("Normal range" %in% vapply(tip$rows, `[[`, "", "label"))
})

test_that("a parameter without a unit or a PARAM still reads", {
  tip <- pp_findings_tip(
    sysbp_row(PARAM = NA_character_, BASE = NA_real_, AVISIT = NA_character_),
    "AVAL"
  )
  expect_identical(tip$head, "SYSBP")
  expect_identical(tip$rows[[1]]$value, "167")
  labels <- vapply(tip$rows, `[[`, "", "label")
  expect_false(any(c("Baseline", "Visit") %in% labels))
})

test_that("the dots carry the tip and the mean line has no tooltip", {
  dm_obj <- pp_scope_subject(pp_normalize_dm(dm::dm(
    adsl = data.frame(USUBJID = "S1", TRTSDT = as.Date("2013-09-16")),
    advs = rbind(sysbp_row(), sysbp_row(AVAL = 175, ADY = 17,
                                        ADT = as.Date("2013-10-02")))
  )), "S1")
  viz <- pp_findings_vizs(dm_obj)[[1]]
  chart <- viz$render(dm_obj, as.Date(c("2013-09-01", "2013-12-31")),
                      settings = list(), mode = "rday")
  series <- chart$x$opts$series
  dots <- Filter(function(s) identical(s$type, "scatter"), series)[[1]]
  expect_identical(dots$tooltip$formatter, PP_TIP_FORMATTER)
  expect_identical(dots$data[[2]]$tip$rows[[1]]$value, "175 mmHg")
  expect_null(dots$data[[1]]$tooltip_text)
  line <- Filter(function(s) identical(s$type, "line") && length(s$data),
                 series)[[1]]
  expect_false(line$tooltip$show)
})

test_that("the long ANRIND spelling colours the dots like the short one", {
  expect_identical(pp_anrind_code(c("HIGH", "low", "Normal", "H", NA, "X")),
                   c("H", "L", "N", "H", NA, "X"))
})
