# Value-level contract for the findings charts (labs / vitals).
#
# Every other findings test asserts which CARDS get built
# (test-pp-findings-plans.R) or which lanes appear. Nothing asserted what
# a card actually PLOTS. These do -- a clinician reads the number, not the
# card list, and a wrong number is worse than a missing chart.

# Y-values of every line series in a rendered findings chart, in draw order.
plotted_values <- function(chart) {
  series <- chart$x$opts$series
  line <- Filter(function(s) identical(s$type, "line") && length(s$data), series)
  unlist(lapply(line, function(s) {
    vapply(s$data, function(d) as.numeric(d$value[[2]]), numeric(1))
  }))
}

render_findings_card <- function(adlb, id = "adlb_neut") {
  dm_obj <- dm::dm(
    adsl = data.frame(USUBJID = "S1", TRTSDT = as.Date("2024-01-01")),
    adlb = adlb
  )
  dm_obj <- pp_scope_subject(pp_normalize_dm(dm_obj), "S1")
  vizs <- pp_findings_vizs(dm_obj)
  expect_true(id %in% names(vizs))
  vizs[[id]]$render(dm_obj, as.Date(c("2024-01-01", "2024-12-31")))
}

neut_adlb <- function(...) {
  base <- data.frame(
    USUBJID = "S1",
    PARAMCD = "NEUT",
    PARAM   = "Neutrophils (10^9/L)",
    AVAL    = c(2.7, 3.1, 2.9),
    ADT     = as.Date(c("2024-01-05", "2024-02-05", "2024-03-05")),
    stringsAsFactors = FALSE
  )
  args <- list(...)
  for (nm in names(args)) base[[nm]] <- args[[nm]]
  base
}

test_that("a plain numeric adlb plots its AVALs verbatim", {
  # Control. If this ever fails the rest of the file is meaningless.
  expect_equal(plotted_values(render_findings_card(neut_adlb())), c(2.7, 3.1, 2.9))
})

test_that("AVAL resolves from LBSTRESN when the canonical column is absent", {
  adlb <- neut_adlb()
  adlb$LBSTRESN <- adlb$AVAL
  adlb$AVAL <- NULL
  expect_equal(plotted_values(render_findings_card(adlb)), c(2.7, 3.1, 2.9))
})

test_that("a carried-forward value is drawn, but not as a measurement", {
  # LOCF is "mark", not "drop": it puts a value on a date where nothing was
  # measured, which is information the collected rows do not carry. It is
  # shown -- hollow, and named in the tooltip -- so the reviewer decides what
  # it is worth. Silently deleting it would make that call for them.
  observed <- neut_adlb(DTYPE = NA_character_)
  carried <- neut_adlb(
    AVAL = 9.9, DTYPE = "LOCF", ADT = as.Date("2024-04-05")
  )[1, ]
  chart <- render_findings_card(rbind(observed, carried))

  expect_equal(plotted_values(chart), c(2.7, 3.1, 2.9, 9.9))

  pts <- Filter(function(s) identical(s$type, "scatter"), chart$x$opts$series)[[1]]$data
  expect_equal(vapply(pts, function(p) p$symbol, character(1)),
               c("circle", "circle", "circle", "emptyCircle"))
  expect_match(pts[[4]]$tooltip_text, "LOCF")
  expect_match(pts[[4]]$tooltip_text, "not measured")
})

test_that("a character AVAL does not kill the card", {
  # SAS/CSV-sourced labs arrive as character. max()/round() on character
  # aborts the whole render, so the card errors instead of degrading.
  expect_equal(
    plotted_values(render_findings_card(neut_adlb(AVAL = c("2.7", "3.1", "2.9")))),
    c(2.7, 3.1, 2.9)
  )
})

test_that("character reference ranges do not kill the card", {
  # NOT a spec-conformance issue: LBSTNRLO/LBSTNRHI are Num per the SDTM IG
  # (it is LBORNRLO/LBORNRHI and LBSTNRC that are Char), so pp_column_catalog()
  # mapping them as "identity" is correct against the standard.
  #
  # The exposure is non-conformant delivery -- a CSV or SAS round-trip that
  # stringifies numerics, which is how study data actually arrives. round() at
  # patient-profile-vizs.R:872 then aborts the whole card.
  adlb <- neut_adlb()
  adlb$A1LO <- "1.5"
  adlb$A1HI <- "8.0"
  expect_equal(plotted_values(render_findings_card(adlb)), c(2.7, 3.1, 2.9))
})


