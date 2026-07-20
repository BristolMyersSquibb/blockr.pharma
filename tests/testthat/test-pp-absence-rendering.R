# An absence must not render as an observation.
#
# Same contract as test-pp-findings-values.R, in the two places the value is
# missing outright rather than derived: an event with no end date, and a
# questionnaire domain with no record at a visit.

test_that("an AE with no end date runs to the window edge, not one day", {
  # A missing AENDT is the ADaM encoding for "still ongoing at data cut" --
  # the state a safety reviewer scans a gantt for. Drawn as start + 1 day it
  # reads as a resolved same-day blip.
  window <- as.Date(c("2024-01-01", "2024-06-30"))
  adae <- data.frame(
    USUBJID = "S1", AEDECOD = "HEADACHE", AESEV = "MILD",
    ASTDT = as.Date("2024-02-01"), AENDT = as.Date(NA),
    stringsAsFactors = FALSE
  )
  dm_obj <- pp_normalize_dm(dm::dm(
    adsl = data.frame(USUBJID = "S1", TRTSDT = as.Date("2024-01-01")),
    adae = adae
  ))
  opts <- ae_gantt_viz$render(dm_obj, window)$x$opts
  v <- opts$series[[1]]$data[[1]]$value

  start <- v[[1]]
  end <- v[[2]]
  one_day <- 86400000
  expect_gt(end - start, one_day)
  expect_equal(end, pp_x_bounds(window)[[2]])

  # The tooltip must not restate the start as the end ("D12 -> D12").
  expect_equal(v[[10]], "ongoing")
  expect_true(isTRUE(v[[13]]))
})

test_that("an AE with an end date is unaffected", {
  window <- as.Date(c("2024-01-01", "2024-06-30"))
  adae <- data.frame(
    USUBJID = "S1", AEDECOD = "HEADACHE", AESEV = "MILD",
    ASTDT = as.Date("2024-02-01"), AENDT = as.Date("2024-02-10"),
    stringsAsFactors = FALSE
  )
  dm_obj <- pp_normalize_dm(dm::dm(
    adsl = data.frame(USUBJID = "S1", TRTSDT = as.Date("2024-01-01")),
    adae = adae
  ))
  v <- ae_gantt_viz$render(dm_obj, window)$x$opts$series[[1]]$data[[1]]$value
  expect_equal(v[[2]], pp_xval(as.Date("2024-02-10")))
  expect_false(isTRUE(v[[13]]))
  expect_false(identical(v[[10]], "ongoing"))
})

test_that("pp_gantt_open_end falls back only on an unbounded axis", {
  s <- pp_xval(as.Date("2024-02-01"))
  bounded <- pp_gantt_open_end(s, as.Date(c("2024-01-01", "2024-06-30")))
  expect_equal(bounded, pp_x_bounds(as.Date(c("2024-01-01", "2024-06-30")))[[2]])
  # No edge to run to -- a stub is all that is left.
  expect_equal(pp_gantt_open_end(s, NULL), s + 86400000)
})

test_that("an unassessed NPI-X domain is a gap, not a score of zero", {
  # NPI-X domains are 0-anchored ("symptom absent"), so a fabricated 0 is
  # pixel-identical to a recorded 0 and the tooltip reads "0" either way.
  adqsnpix <- data.frame(
    USUBJID = "S1",
    PARAMCD = c("NPITM01S", "NPITM02S", "NPITM01S"),
    AVISIT  = c("BASELINE", "BASELINE", "WEEK 4"),
    AVAL    = c(3, 5, 2),
    stringsAsFactors = FALSE
  )
  dm_obj <- pp_normalize_dm(dm::dm(adqsnpix = adqsnpix))
  opts <- npix_radar_viz$render(dm_obj, NULL, settings = list())$x$opts
  series <- opts$series[[1]]$data

  wk4 <- Filter(function(s) identical(s$name, "WEEK 4"), series)[[1]]
  # Domain 2 was never assessed at Week 4. It must be a hole in the polygon.
  expect_equal(wk4$value[[1]], 2)
  expect_null(wk4$value[[2]])

  base <- Filter(function(s) identical(s$name, "BASELINE"), series)[[1]]
  expect_equal(base$value[[1]], 3)
  expect_equal(base$value[[2]], 5)
})

test_that("a recorded zero still renders as a zero", {
  # The other half of the contract: suppressing fabricated zeros must not
  # suppress real ones.
  adqsnpix <- data.frame(
    USUBJID = "S1", PARAMCD = c("NPITM01S", "NPITM02S"),
    AVISIT = "BASELINE", AVAL = c(0, 5), stringsAsFactors = FALSE
  )
  dm_obj <- pp_normalize_dm(dm::dm(adqsnpix = adqsnpix))
  v <- npix_radar_viz$render(dm_obj, NULL, settings = list())$x$opts$series[[1]]$data[[1]]$value
  expect_equal(v[[1]], 0)
  expect_equal(v[[2]], 5)
})
