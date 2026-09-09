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

# The fixture ships no category column, so the whole table is one card.
# A findings card is one PARAMETER now, so these render the card for the
# parameter under test rather than the group card that used to hold it. The
# chart itself is unchanged: pp_render_findings() always drew one grid per
# code and is handed one instead of three.
render_findings_card <- function(adlb, id = NULL, ref_ms = NA_real_,
                                 settings = list()) {
  dm_obj <- dm::dm(
    adsl = data.frame(USUBJID = "S1", TRTSDT = as.Date("2024-01-01")),
    adlb = adlb
  )
  dm_obj <- pp_scope_subject(pp_normalize_dm(dm_obj), "S1")
  vizs <- pp_findings_vizs(dm_obj)
  if (is.null(id)) {
    id <- names(vizs)[[1L]]
  }
  expect_true(id %in% names(vizs))
  vizs[[id]]$render(dm_obj, as.Date(c("2024-01-01", "2024-12-31")),
                    settings = settings, ref_ms = ref_ms)
}

tooltips <- function(chart) {
  pts <- Filter(function(s) identical(s$type, "scatter"),
                chart$x$opts$series)[[1]]$data
  vapply(pts, function(p) p$tooltip_text, character(1))
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

test_that("a findings point reports the visit its own row names", {
  # AVISIT on a findings row describes that row's timepoint, so it belongs
  # beside its date -- printed, not interpreted: see pp-cycle.R.
  adlb <- neut_adlb(AVISIT = c("CYCLE 1 DAY 1", "Cycle 2, Day 8", "UNSCHEDULED"))
  chart <- render_findings_card(adlb)
  pts <- Filter(function(s) identical(s$type, "scatter"),
                chart$x$opts$series)[[1]]$data
  expect_match(pts[[1]]$tooltip_text, "2024-01-05 \\(CYCLE 1 DAY 1\\)")
  expect_match(pts[[2]]$tooltip_text, "2024-02-05 \\(Cycle 2, Day 8\\)")
  # An off-protocol draw says so, which is the point of printing the label
  expect_match(pts[[3]]$tooltip_text, "2024-03-05 \\(UNSCHEDULED\\)")
})

test_that("a study without AVISIT keeps the date it always had", {
  pts <- Filter(function(s) identical(s$type, "scatter"),
                render_findings_card(neut_adlb())$x$opts$series)[[1]]$data
  expect_match(pts[[1]]$tooltip_text, "2024-01-05<")
})

test_that("a lab point reports the day on treatment its own row carries", {
  # The ask from clinical review: AE bars read "D43" - "D50" while labs read
  # a date and a visit, so the two cannot be lined up by eye. ADY is the
  # record's own number and is printed as such.
  adlb <- neut_adlb(ADY = c(5, 36, 65),
                    AVISIT = c("CYCLE 1 DAY 1", "Cycle 2, Day 8", "UNSCHEDULED"))
  tt <- tooltips(render_findings_card(adlb))
  expect_match(tt[1], "Day:</span> D5")
  expect_match(tt[3], "Day:</span> D65")
  # Added, never substituted -- the date and the visit are still there.
  expect_match(tt[3], "2024-03-05 \\(UNSCHEDULED\\)")
})

test_that("LBDY reaches the tooltip as the day", {
  # SDTM ships the day as LBDY; pp_column_catalog() maps it onto ADY, so a
  # raw-SDTM study gets the same line an ADaM one does.
  tt <- tooltips(render_findings_card(neut_adlb(LBDY = c(5, 36, 65))))
  expect_match(tt[2], "Day:</span> D36")
})

test_that("a study shipping no *DY takes the day from the axis reference", {
  # Same arithmetic and same skip-zero rule as the relative-day axis, so the
  # tooltip cannot disagree with the axis under it. Treatment starts
  # 2024-01-01, so the draw on 2024-01-05 is D5.
  ref_ms <- as.numeric(as.POSIXct(as.Date("2024-01-01"))) * 1000
  tt <- tooltips(render_findings_card(neut_adlb(), ref_ms = ref_ms))
  expect_match(tt[1], "Day:</span> D5")
})

test_that("no *DY and no reference leaves the tooltip as it was", {
  # Nothing to report is reported as nothing: no invented day, no empty row.
  tt <- tooltips(render_findings_card(neut_adlb()))
  expect_false(grepl("Day:", tt[1], fixed = TRUE))
  expect_match(tt[1], "2024-01-05<")
})

# ---------------------------------------------------------------------------
# Which analysis value the card draws
#
# A card used to be able to draw AVAL and nothing else, while the cohort
# charts beside it offered AVAL, CHG and PCHG on the same parameter. These
# assert the pill's contract: the numbers are the requested column verbatim,
# the two things that are statements about the MEASURED value stop being
# drawn, and a request the study cannot answer never turns into a wrong
# chart.
# ---------------------------------------------------------------------------

# A frame carrying the change columns beside the value, arithmetically
# consistent with a baseline of 2.5 so a wrong column cannot pass by
# coincidence.
chg_adlb <- function(...) {
  base <- neut_adlb(...)
  base$BASE <- 2.5
  base$CHG <- base$AVAL - 2.5
  base$PCHG <- round(100 * base$CHG / 2.5, 4)
  base
}

mark_areas <- function(chart) {
  Filter(Negate(is.null),
         lapply(chart$x$opts$series, function(s) s$markArea))
}
mark_lines <- function(chart) {
  Filter(Negate(is.null),
         lapply(chart$x$opts$series, function(s) s$markLine))
}
point_colors <- function(chart) {
  pts <- Filter(function(s) identical(s$type, "scatter"),
                chart$x$opts$series)[[1]]$data
  vapply(pts, function(p) p$itemStyle$color %||% "", character(1))
}

test_that("a card draws the change column when the pill asks for it", {
  expect_equal(
    plotted_values(render_findings_card(chg_adlb(),
                                        settings = list(value = "CHG"))),
    c(0.2, 0.6, 0.4)
  )
})

test_that("a card draws percent change when the pill asks for it", {
  expect_equal(
    plotted_values(render_findings_card(chg_adlb(),
                                        settings = list(value = "PCHG"))),
    c(8, 24, 16)
  )
})

test_that("no setting still draws the measured value", {
  # The default has to survive every board saved before the pill existed.
  expect_equal(plotted_values(render_findings_card(chg_adlb())),
               c(2.7, 3.1, 2.9))
})

test_that("a value column the study does not carry falls back to AVAL", {
  # A board saved against a richer study names PCHG; this one ships none.
  # The control that produced the setting is data-conditional, so this must
  # degrade rather than error -- same contract as pp_lane_column().
  expect_equal(
    plotted_values(render_findings_card(neut_adlb(),
                                        settings = list(value = "PCHG"))),
    c(2.7, 3.1, 2.9)
  )
})

test_that("a value outside the ladder falls back to AVAL", {
  # Nothing stops board code or the ctrl channel writing an arbitrary string
  # into viz_settings.
  expect_equal(
    plotted_values(render_findings_card(chg_adlb(),
                                        settings = list(value = "AETOXGR"))),
    c(2.7, 3.1, 2.9)
  )
  expect_equal(
    plotted_values(render_findings_card(chg_adlb(),
                                        settings = list(value = NULL))),
    c(2.7, 3.1, 2.9)
  )
})

test_that("the reference band is drawn on AVAL and not on a change", {
  # The normal range is stated in the measurement's own units. Subtracting a
  # baseline moves the values off it, so a band drawn there would be a limit
  # for a number it was never computed for.
  adlb <- chg_adlb(A1LO = 1.5, A1HI = 8)
  expect_length(mark_areas(render_findings_card(adlb)), 1)
  expect_length(
    mark_areas(render_findings_card(adlb, settings = list(value = "CHG"))), 0
  )
})

test_that("a change scale states zero, and AVAL does not", {
  adlb <- chg_adlb(A1LO = 1.5, A1HI = 8)
  expect_length(mark_lines(render_findings_card(adlb)), 0)
  zero <- mark_lines(render_findings_card(adlb,
                                          settings = list(value = "PCHG")))
  expect_length(zero, 1)
  expect_equal(zero[[1]]$data[[1]]$yAxis, 0)
})

test_that("ANRIND stops colouring the markers on a change scale", {
  # ANRIND says where AVAL sat against its range. On a change axis the same
  # red dot would read as "this CHANGE is abnormal", which the flag does not
  # say.
  adlb <- chg_adlb(ANRIND = c("H", "N", "L"))
  expect_equal(point_colors(render_findings_card(adlb)),
               c("#dc2626", "#059669", "#2563eb"))
  flat <- point_colors(render_findings_card(adlb,
                                            settings = list(value = "CHG")))
  expect_equal(length(unique(flat)), 1L)
  expect_false("#dc2626" %in% flat)
})

test_that("the tooltip names the column it drew", {
  tt <- tooltips(render_findings_card(chg_adlb(),
                                      settings = list(value = "PCHG")))
  expect_match(tt[1], "PCHG:</span> <b>8%</b>")
  expect_false(grepl("AVAL:</span> <b>", tt[1], fixed = TRUE))
})

test_that("a change tooltip also states the value and the baseline", {
  # "How far has this moved" is answered by the chart; "from what, and to
  # what" is the reader's next question and has nowhere else to go.
  tt <- tooltips(render_findings_card(chg_adlb(BASETYPE = "LAST"),
                                      settings = list(value = "CHG")))
  expect_match(tt[1], "CHG:</span> <b>0.2</b>")
  expect_match(tt[1], "AVAL:</span> 2.7")
  expect_match(tt[1], "Baseline:</span> 2.5")
  # Which baseline: ADaM lets one parameter carry more than one definition,
  # so the rule rides behind the number.
  expect_match(tt[1], "\\(LAST\\)")
})

test_that("an AVAL tooltip is unchanged", {
  tt <- tooltips(render_findings_card(chg_adlb()))
  expect_match(tt[1], "AVAL:</span> <b>2.7</b>")
  expect_false(grepl("Baseline:", tt[1], fixed = TRUE))
})

test_that("a parameter with no change records says so", {
  # The column exists table-wide (so the pill offers it) but this parameter
  # has none. Silently swapping back to AVAL while the pill still reads
  # "% change" is the failure this prevents.
  adlb <- rbind(
    transform(chg_adlb(), PARAMCD = "ALT", PARAM = "Alanine (U/L)"),
    transform(neut_adlb(), BASE = NA_real_, CHG = NA_real_, PCHG = NA_real_)
  )
  chart <- render_findings_card(adlb, id = "adlb_all__NEUT",
                                settings = list(value = "PCHG"))
  # The empty placeholder carries its message as the chart title and has no
  # series at all.
  expect_null(chart$x$opts$series)
  expect_match(chart$x$opts$title$text, "PCHG")
})

test_that("the Value pill is offered only where there is something to pick", {
  plain <- pp_findings_vizs(dm::dm(adlb = neut_adlb()))[[1]]
  with_chg <- pp_findings_vizs(dm::dm(adlb = chg_adlb()))[[1]]
  # Declared on every card...
  expect_named(plain$controls, "value")
  # ...and drawn only where the data can answer it: choices_present filters
  # to the columns the study carries, and a pill with one rung is not a
  # choice.
  dm_plain <- pp_normalize_dm(dm::dm(adlb = neut_adlb()))
  dm_chg <- pp_normalize_dm(dm::dm(adlb = chg_adlb()))
  expect_null(pp_controls_ui(plain, plain$id, dm_plain, list()))
  expect_false(is.null(pp_controls_ui(with_chg, with_chg$id, dm_chg, list())))
})

test_that("the printed twin draws the value the screen drew", {
  skip_if_not_installed("ggplot2")
  dm_obj <- pp_scope_subject(
    pp_normalize_dm(dm::dm(
      adsl = data.frame(USUBJID = "S1", TRTSDT = as.Date("2024-01-01")),
      adlb = chg_adlb(A1LO = 1.5, A1HI = 8)
    )),
    "S1"
  )
  viz <- pp_findings_vizs(dm_obj)[[1]]
  tr <- as.Date(c("2024-01-01", "2024-12-31"))

  aval <- viz$exhibit(dm_obj, tr, list())
  pchg <- viz$exhibit(dm_obj, tr, list(value = "PCHG"))
  expect_equal(ggplot2::ggplot_build(aval$plot %||% aval)$data[[2]]$y,
               c(2.7, 3.1, 2.9))
  # Layer 1 on the change plot is the zero line, so the value layer shifts.
  built <- ggplot2::ggplot_build(pchg$plot %||% pchg)
  ys <- unlist(lapply(built$data, function(d) if ("y" %in% names(d)) d$y))
  expect_true(all(c(8, 24, 16) %in% ys))
  expect_true(0 %in% unlist(lapply(built$data, function(d) d$yintercept)))
})

test_that("the value pill carries no dimension label", {
  # It stood in front of the pill on every findings card, and a profile is a
  # stack of them. The pill's own text says which value, the header says
  # which parameter, and the tooltip says what a click does.
  dm_chg <- pp_normalize_dm(dm::dm(adlb = chg_adlb()))
  viz <- pp_findings_vizs(dm_chg)[[1]]
  html <- as.character(pp_controls_ui(viz, viz$id, dm_chg, list()))

  expect_false(grepl("pp-ctrl-label", html, fixed = TRUE))
  expect_match(html, "pp-ctrl-pill")
  expect_match(html, "AVAL")
  # The action still has somewhere to live.
  expect_match(html, "Switch to CHG")
})

test_that("a control that declares a label still draws one", {
  # The gantts' lane pill is a dimension AND a setting: "Preferred term" on
  # its own does not say what it is the term FOR.
  ctrl <- pp_lane_control(PP_CM_LANES, default = "CMDECOD")
  viz <- structure(
    list(id = "x", controls = ctrl, tables = "adcm"),
    class = c("pp_viz", "list")
  )
  dm_obj <- dm::dm(adcm = data.frame(
    CMTRT = "a", CMDECOD = "b", CMCLAS = "c", stringsAsFactors = FALSE
  ))
  html <- as.character(pp_controls_ui(viz, "x", dm_obj, list()))
  expect_match(html, "pp-ctrl-label")
  expect_match(html, "Lanes")
})

test_that("the pill is named after the columns it picks", {
  # Three words cost eight characters of a 311px card header, and the pill
  # reserves its widest rung, so they came off the parameter's name beside
  # it. The column names are what the tooltip prints anyway.
  expect_identical(names(PP_FINDINGS_VALUES), unname(PP_FINDINGS_VALUES))
  expect_identical(unname(PP_FINDINGS_VALUES), c("AVAL", "CHG", "PCHG"))
})

test_that("an empty change panel names the column once", {
  # The message used to read "No PCHG (PCHG) records" once the rung and the
  # column were spelled the same.
  adlb <- rbind(
    transform(chg_adlb(), PARAMCD = "ALT", PARAM = "Alanine (U/L)"),
    transform(neut_adlb(), BASE = NA_real_, CHG = NA_real_, PCHG = NA_real_)
  )
  chart <- render_findings_card(adlb, id = "adlb_all__NEUT",
                                settings = list(value = "PCHG"))
  expect_identical(chart$x$opts$title$text, "No PCHG records")
})
