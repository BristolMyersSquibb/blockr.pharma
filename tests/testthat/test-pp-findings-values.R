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

test_that("ADaM derived records (DTYPE) are not plotted as observations", {
  # THE REPORTED BUG. An ADaM adlb routinely carries derived analysis rows
  # alongside the observed one for the same PARAMCD and date -- LOCF carry-
  # forwards, averages of replicates, and imputed records that a study fills
  # with 0. ADaM flags them with DTYPE; the observed record is DTYPE == NA.
  #
  # Before pp_select_records() existed, pp_render_findings() drew every row.
  # The 0-valued derived record landed on the same x as the real 2.7 and the
  # line collapsed to the floor between every pair of real points; on screen
  # the neutrophil count read 0.
  observed <- neut_adlb(DTYPE = NA_character_)
  derived <- neut_adlb(AVAL = 0, DTYPE = "CALCULATION")
  adlb <- rbind(observed, derived)

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

test_that("a card says how many derived records it held back", {
  adlb <- rbind(
    neut_adlb(DTYPE = NA_character_),
    neut_adlb(AVAL = 0, DTYPE = "MAXIMUM")
  )
  chart <- render_findings_card(adlb)
  titles <- vapply(chart$x$opts$title, function(t) t$text %||% "", character(1))
  expect_true(any(grepl("3 derived records hidden", titles)))
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

# ---------------------------------------------------------------------------
# The same defect class in the aggregating vizs.
#
# pp_render_findings() draws one point per row, so "mark" is enough there.
# The vizs below collapse rows with mean(), where marking cannot help -- a
# blended observed-plus-derived number appears in no record. They go through
# pp_prefer_collected(): a cell holding any measurement ignores the derived
# rows in it.
# ---------------------------------------------------------------------------

test_that("ortho BP does not average a replicate against its derived mean", {
  # ADaM stores the replicate mean as a DTYPE="AVERAGE" row NEXT TO the
  # replicates. viz-ortho-bp.R:111 averages all three, weighting the derived
  # value at 1/3. Orthostatic protocols repeat on an abnormal reading, so
  # duplicates here are expected, not exotic.
  pos <- "AFTER STANDING FOR 1 MINUTE"
  advs <- data.frame(
    USUBJID = "x",
    PARAMCD = "SYSBP",
    ATPT    = pos,
    AVISIT  = "BASELINE",
    AVAL    = c(180, 140, 160),
    DTYPE   = c(NA, NA, "AVERAGE"),
    stringsAsFactors = FALSE
  )
  dm_obj <- pp_normalize_dm(dm::dm(advs = advs))
  opts <- ortho_bp_viz$render(dm_obj, time_range = NULL, settings = list())$x$opts
  s <- Filter(function(s) grepl("^Systolic", s$name %||% ""), opts$series)[[1]]
  drawn <- vapply(s$data, function(d) as.numeric(d$value[[2]]), numeric(1))

  # AVERAGE is "keep": the study already summarized its own replicates, so
  # the summary is drawn and the replicates it consumed are not. Drawing all
  # three would average the mean back in and weight it at 1/3.
  expect_equal(drawn, 160)
})

test_that("questionnaire heatmap does not blend observed with derived", {
  adqsadas <- data.frame(
    USUBJID = "x",
    PARAMCD = "ACTOT",
    AVISIT  = "WEEK 8",
    AVAL    = c(20, 30),
    DTYPE   = c(NA, "LOCF"),
    stringsAsFactors = FALSE
  )
  dm_obj <- pp_normalize_dm(dm::dm(adqsadas = adqsadas))
  opts <- questionnaire_heatmap_viz$render(
    dm_obj, time_range = NULL, settings = list(domain = "adqsadas")
  )$x$opts
  cells <- Filter(function(s) identical(s$type, "heatmap"), opts$series)[[1]]$data
  vals <- vapply(cells, function(d) as.numeric(d[[3]]), numeric(1))

  # 25 would be the mean of an observed 20 and a carried-forward 30 -- a
  # score the patient never had. A cell holding any measurement ignores the
  # derived rows in it; marking cannot help here, since there is no
  # half-including a value in an average.
  expect_equal(vals, 20)
})

test_that("the lab reference band ignores derived rows", {
  # patient-profile-vizs.R:932 takes median(A1LO) over ROWS, not over distinct
  # reference ranges, so LOCF rows carrying a stale range outvote the real one.
  observed <- neut_adlb(DTYPE = NA_character_, A1LO = 1.5, A1HI = 8.0)
  derived <- neut_adlb(DTYPE = "LOCF", A1LO = 0.5, A1HI = 3.0)
  adlb <- rbind(observed, derived, derived)

  chart <- render_findings_card(adlb)
  band <- Filter(function(s) grepl(" ref$", s$name %||% ""), chart$x$opts$series)
  expect_length(band, 1L)
  area <- band[[1]]$markArea$data[[1]]
  expect_equal(c(area[[1]]$yAxis, area[[2]]$yAxis), c(1.5, 8.0))
})
