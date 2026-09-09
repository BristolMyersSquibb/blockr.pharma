# Tumour response: which cards exist, where the bars go, and what colours them.
#
# adrs is the first table the profile reads whose analysis value is a WORD,
# and the first whose intervals are not in the data: they are the gaps between
# point-in-time assessments. Both of those are places to get something quietly
# wrong, so they are asserted on values rather than on the card list.

rs <- function(codes, avalc, dates, subj = "S1", param = NULL) {
  data.frame(
    USUBJID = subj,
    PARAMCD = codes,
    PARAM = param %||% codes,
    AVALC = avalc,
    ADT = as.Date(dates),
    stringsAsFactors = FALSE
  )
}

ovr <- function(avalc = c("SD", "PR", "PD"),
                dates = c("2024-02-01", "2024-04-01", "2024-06-01"),
                subj = "S1") {
  rs("OVR", avalc, dates, subj, param = "Overall Response by Investigator")
}

resp_dm <- function(adrs, subj = "S1") {
  pp_scope_subject(
    pp_normalize_dm(dm::dm(
      adsl = data.frame(USUBJID = unique(adrs$USUBJID),
                        TRTSDT = as.Date("2024-01-01")),
      adrs = adrs
    )),
    subj
  )
}

WINDOW <- as.Date(c("2024-01-01", "2024-12-31"))

# ---------------------------------------------------------------------------
# Which parameters get a lane
# ---------------------------------------------------------------------------

test_that("only repeatedly-assessed parameters get a lane", {
  # adrs pools a dozen parameters and most are one record per subject: best
  # overall response, death, measurable disease at baseline. Those are facts
  # about the subject, not a course of assessments, and a lane of one bar
  # would say something about duration that the record does not.
  tbl <- rbind(
    ovr(),
    rs("BOR", "PR", "2024-06-01"),
    rs("DEATH", "N", "2024-06-01")
  )
  expect_identical(pp_resp_lane_params(tbl), "OVR")
})

test_that("repeated is judged across the cohort, not per patient", {
  # A patient with one assessment must not lose the card every other patient
  # has: the catalog is a property of the study.
  tbl <- rbind(
    ovr(subj = "S1"),
    ovr(avalc = "SD", dates = "2024-02-01", subj = "S2")
  )
  expect_identical(pp_resp_lane_params(tbl), "OVR")
})

test_that("blank categories do not make a parameter longitudinal", {
  tbl <- rs("OVR", c("SD", NA, ""), c("2024-02-01", "2024-04-01",
                                      "2024-06-01"))
  expect_identical(pp_resp_lane_params(tbl), character())
})

test_that("a subject-level response table yields no cards at all", {
  dm_obj <- resp_dm(rbind(rs("BOR", "PR", "2024-06-01"),
                          rs("DEATH", "N", "2024-06-01")))
  expect_length(pp_response_vizs(dm_obj), 0L)
})

test_that("a dm without a response table yields no cards", {
  expect_length(
    pp_response_vizs(dm::dm(adsl = data.frame(USUBJID = "S1"))),
    0L
  )
})

test_that("a card is built per parameter, named after the study's code", {
  dm_obj <- resp_dm(rbind(ovr(), rs("BOR", "PR", "2024-06-01")))
  vizs <- pp_response_vizs(dm_obj)
  expect_named(vizs, "response_OVR")
  viz <- vizs[["response_OVR"]]
  expect_identical(viz$label, "OVR")
  expect_identical(viz$sublabel, "Overall Response by Investigator")
  expect_identical(unname(viz$params), "Overall Response by Investigator")
  # It cannot collide with a findings card, whose ids always carry "__".
  expect_false(grepl("__", viz$id, fixed = TRUE))
  # No cohort strip: its intervals are derived from the gaps between records,
  # which pp_band_spans() has no way to declare.
  expect_null(viz$band)
})

test_that("a response card is reachable from the parameter search", {
  dm_obj <- resp_dm(ovr())
  idx <- pp_param_index(pp_response_vizs(dm_obj))
  expect_length(idx, 1L)
  expect_identical(idx[[1]]$code, "OVR")
  expect_identical(idx[[1]]$viz_id, "response_OVR")
})

# ---------------------------------------------------------------------------
# Where the bars go
# ---------------------------------------------------------------------------

test_that("each assessment runs to the next one", {
  seg <- pp_resp_segments(ovr(), WINDOW)
  expect_equal(nrow(seg), 3L)
  expect_equal(seg$resp, c("SD", "PR", "PD"))
  expect_equal(seg$end[1], seg$start[2])
  expect_equal(seg$end[2], seg$start[3])
})

test_that("the last assessment is open-ended, not a same-day blip", {
  # A missing "next" is not a resolved event. Drawing it as a stub would say
  # the response ended the day it was recorded.
  seg <- pp_resp_segments(ovr(), WINDOW)
  expect_equal(seg$ongoing, c(FALSE, FALSE, TRUE))
  expect_gt(seg$end[3], seg$start[3])
  # It runs to the edge of the window, the same encoding an unresolved
  # adverse event gets.
  expect_equal(seg$end[3], pp_x_bounds(WINDOW)[2])
  expect_identical(seg$e_lab[3], PP_ONGOING_LABEL)
})

test_that("segments come out in time order whatever order the table is in", {
  shuffled <- ovr()[c(3, 1, 2), ]
  seg <- pp_resp_segments(shuffled, WINDOW)
  expect_equal(seg$resp, c("SD", "PR", "PD"))
  expect_true(all(diff(seg$start) > 0))
})

test_that("consecutive equal responses stay separate assessments", {
  # Three SD assessments are three assessments. The profile's mark radius is
  # chosen so abutting bars keep a visible seam for exactly this.
  seg <- pp_resp_segments(ovr(avalc = c("SD", "SD", "SD")), WINDOW)
  expect_equal(nrow(seg), 3L)
})

test_that("records with no date or no category are dropped, not placed", {
  tbl <- ovr(avalc = c("SD", NA, "PD"))
  tbl$ADT[3] <- NA
  seg <- pp_resp_segments(tbl, WINDOW)
  expect_equal(nrow(seg), 1L)
  expect_equal(seg$resp, "SD")
  expect_true(seg$ongoing)
})

test_that("relative-day mode places bars on days, not on timestamps", {
  ref <- as.numeric(as.POSIXct(as.Date("2024-01-01"))) * 1000
  seg <- pp_resp_segments(ovr(), WINDOW, ref_ms = ref, mode = "rday")
  # 2024-02-01 is 31 days after treatment start, and the axis skips no zero.
  expect_equal(seg$start[1], 32)
  expect_match(seg$s_lab[1], "^D32$")
})

test_that("a study shipping ADY uses it rather than re-deriving a day", {
  tbl <- ovr()
  tbl$ADY <- c(32, 92, 153)
  ref <- as.numeric(as.POSIXct(as.Date("2024-01-01"))) * 1000
  seg <- pp_resp_segments(tbl, WINDOW, ref_ms = ref, mode = "rday")
  expect_equal(seg$start, c(32, 92, 153))
})

# ---------------------------------------------------------------------------
# Colour
# ---------------------------------------------------------------------------

test_that("the built-in RECIST palette answers when no board map does", {
  cols <- pp_resp_scale_colors(NULL, resp_dm(ovr()), paramcds = "OVR")
  expect_equal(unname(cols[c("SD", "PR", "PD")]),
               unname(pp_resp_colors[c("SD", "PR", "PD")]))
})

test_that("the legend is ordered best to worst, not by the data", {
  # A legend that reshuffles between patients is one a reader re-reads every
  # time.
  cols <- pp_resp_scale_colors(
    NULL, resp_dm(ovr(avalc = c("PD", "CR", "SD"))), paramcds = "OVR"
  )
  expect_identical(names(cols), c("CR", "SD", "PD"))
})

test_that("a board binding wins over the built-in palette", {
  skip_if_not_installed("blockr.theme")
  map <- blockr.theme::new_scale_map(
    blockr.theme::scale_binding("AVALC", color = c(SD = "#123456"))
  )
  cols <- pp_resp_scale_colors(map, resp_dm(ovr()), paramcds = "OVR")
  expect_identical(unname(cols[["SD"]]), "#123456")
  # ...and the categories it leaves open keep their conventional colour
  # rather than dropping to grey.
  expect_identical(unname(cols[["PD"]]), unname(pp_resp_colors[["PD"]]))
})

test_that("a category this package has no opinion about goes grey", {
  cols <- pp_resp_scale_colors(NULL, resp_dm(ovr(avalc = c("SD", "PR", "ZZ"))),
                               paramcds = "OVR")
  expect_identical(unname(cols[["ZZ"]]), "#9ca3af")
})

test_that("the legend covers the drawn parameter only", {
  # Resolving table-wide put "Y", "N" and "MISSING" -- the Y/N flags of every
  # other adrs parameter -- in the response legend beside CR and PD.
  tbl <- rbind(ovr(), rs("DEATH", "N", "2024-06-01"),
               rs("BOR", "MISSING", "2024-06-01"))
  cols <- pp_resp_scale_colors(NULL, resp_dm(tbl), paramcds = "OVR")
  expect_setequal(names(cols), c("SD", "PR", "PD"))
})

test_that("the colours reach the render and the legend from one vector", {
  dm_obj <- resp_dm(ovr())
  viz <- pp_response_vizs(dm_obj)[["response_OVR"]]
  settings <- pp_viz_exhibit_settings(viz, list(), list(), dm_obj)
  expect_setequal(names(settings$resp_colors), c("SD", "PR", "PD"))

  chart <- viz$render(dm_obj, WINDOW, settings)
  bars <- chart$x$opts$series[[1]]$data
  expect_equal(vapply(bars, function(b) b$itemStyle$color, character(1)),
               unname(settings$resp_colors[c("SD", "PR", "PD")]))

  legend <- viz$legend_ui(dm_obj, settings)
  expect_false(is.null(legend))
  expect_length(legend$children[[1]], 3L)
})

# ---------------------------------------------------------------------------
# The rendered lane
# ---------------------------------------------------------------------------

test_that("the lane draws one bar per assessment, all on one row", {
  dm_obj <- resp_dm(ovr())
  viz <- pp_response_vizs(dm_obj)[["response_OVR"]]
  chart <- viz$render(dm_obj, WINDOW, list())
  bars <- chart$x$opts$series[[1]]$data
  expect_length(bars, 3L)
  expect_true(all(vapply(bars, function(b) b$value[[3]], integer(1)) == 0L))
  # No on-bar label. One lane, and the card header already reads
  # "OVR  Overall Response by Investigator"; the other gantts label their
  # lanes because they have many and hide the axis.
  expect_equal(vapply(bars, function(b) b$value[[7]], character(1)),
               c("", "", ""))
})

test_that("the tooltip says which category and how long it held", {
  dm_obj <- resp_dm(ovr())
  viz <- pp_response_vizs(dm_obj)[["response_OVR"]]
  bars <- viz$render(dm_obj, WINDOW, list())$x$opts$series[[1]]$data
  expect_equal(vapply(bars, function(b) b$value[[4]], character(1)),
               c("SD", "PR", "PD"))
  expect_equal(bars[[1]]$value[[5]], "2024-02-01")
  expect_equal(bars[[1]]$value[[6]], "2024-04-01")
  expect_true(bars[[3]]$value[[8]])
})

test_that("date mode without an analysis date says what to switch to", {
  tbl <- ovr()
  tbl$ADT <- NULL
  tbl$ADY <- c(32, 92, 153)
  dm_obj <- pp_scope_subject(
    pp_normalize_dm(dm::dm(
      adsl = data.frame(USUBJID = "S1", TRTSDT = as.Date("2024-01-01")),
      adrs = tbl
    )),
    "S1"
  )
  viz <- pp_response_vizs(dm_obj)[["response_OVR"]]
  chart <- viz$render(dm_obj, WINDOW, list(), mode = "date")
  expect_null(chart$x$opts$series)
  expect_match(chart$x$opts$title$text, "relative day")
})

test_that("a patient with no assessments in the window says so", {
  dm_obj <- resp_dm(ovr())
  viz <- pp_response_vizs(dm_obj)[["response_OVR"]]
  chart <- viz$render(dm_obj, as.Date(c("2020-01-01", "2020-12-31")), list())
  expect_null(chart$x$opts$series)
  expect_match(chart$x$opts$title$text, "time range")
})

test_that("the printed twin places the same bars as the screen", {
  skip_if_not_installed("ggplot2")
  dm_obj <- resp_dm(ovr())
  viz <- pp_response_vizs(dm_obj)[["response_OVR"]]
  settings <- pp_viz_exhibit_settings(viz, list(), list(), dm_obj)

  screen <- viz$render(dm_obj, WINDOW, settings)$x$opts$series[[1]]$data
  printed <- viz$exhibit(dm_obj, WINDOW, settings)
  expect_s3_class(printed, "ggplot")

  rects <- ggplot2::ggplot_build(printed)$data[[1]]
  expect_equal(nrow(rects), 3L)
  expect_equal(rects$xmin, vapply(screen, function(b) b$value[[1]], numeric(1)))
  # A page cannot be hovered, so the category is written on the bar.
  txt <- Filter(function(d) "label" %in% names(d),
                ggplot2::ggplot_build(printed)$data)
  expect_setequal(txt[[1]]$label, c("SD", "PR", "PD"))
})

# ---------------------------------------------------------------------------
# Best overall response, in the info table
# ---------------------------------------------------------------------------

test_that("the best overall response is reported as a patient fact", {
  dm_obj <- resp_dm(rbind(ovr(), rs("BOR", "PR", "2024-06-01")))
  expect_identical(pp_resp_bor_field(dm_obj),
                   list(label = "Best overall response", value = "PR"))
  info <- pp_patient_info_fields(dm_obj)
  expect_true("Response" %in% info$Field)
  expect_identical(info$Value[info$Field == "Response"], "PR")
})

test_that("ADaM's literal MISSING is not reported as a response", {
  # ADaM fills BOR for every subject, so an untreated or unassessed patient
  # carries the string "MISSING". Printing that as a finding is worse than
  # printing nothing.
  dm_obj <- resp_dm(rs("BOR", "MISSING", "2024-06-01"))
  expect_null(pp_resp_bor_field(dm_obj))
  expect_false("Response" %in% pp_patient_info_fields(dm_obj)$Field)
})

test_that("a study with no response table keeps its info card", {
  dm_obj <- pp_scope_subject(
    pp_normalize_dm(dm::dm(
      adsl = data.frame(USUBJID = "S1", AGE = 62, TRTSDT = as.Date("2024-01-01"))
    )),
    "S1"
  )
  expect_null(pp_resp_bor_field(dm_obj))
  # `tables` stays "adsl", so the card itself is untouched.
  expect_identical(patient_profile_static_vizs()[["patient_info"]]$tables,
                   "adsl")
  expect_gt(nrow(pp_patient_info_fields(dm_obj)), 0L)
})

# ---------------------------------------------------------------------------
# The info card's layout, which is data
# ---------------------------------------------------------------------------

test_that("the facts say which of them will not sit two to a line", {
  # The card is a grid of pairs. Which values take a whole row is decided
  # here, not by a length heuristic in CSS: the treatment period is a date
  # range and the ethnicity is a sentence, and both are long whatever the
  # study calls them.
  dm_obj <- resp_dm(rbind(ovr(), rs("BOR", "PR", "2024-06-01")))
  info <- pp_patient_info_fields(dm_obj, list(roles = list(arm = NULL)))
  expect_true(all(c("Field", "Value", "Span", "Tint") %in% names(info)))
  wide <- info$Field[info$Span]
  expect_true("Treatment period" %in% wide)
  expect_false("Subject" %in% wide)
})

test_that("the arm and the response carry the board's colours, nothing else does", {
  dm_obj <- resp_dm(rbind(ovr(), rs("BOR", "PR", "2024-06-01")))
  info <- pp_patient_info_fields(dm_obj, list(
    roles = list(arm = "ARM"),
    arm_colors = c(Placebo = "#2563eb"),
    resp_colors = c(PR = "#FFD700")
  ))
  # No ARM column on this fixture, so only the response tints; what matters
  # is that nothing else ever does.
  expect_identical(info$Tint[info$Field == "Response"],
                   pp_cohort_chip_style("#FFD700"))
  expect_true(all(info$Tint[!info$Field %in% c("Arm", "Response")] == ""))
})

test_that("a level the board has no colour for gets no chip", {
  dm_obj <- resp_dm(rs("BOR", "PR", "2024-06-01"))
  info <- pp_patient_info_fields(dm_obj, list(resp_colors = c(CR = "#006400")))
  expect_identical(info$Tint[info$Field == "Response"], "")
})

test_that("the export is label and value, not the layout", {
  # Span and Tint are how the card lays the facts out. A download that
  # carried them would be exporting CSS.
  dm_obj <- resp_dm(rs("BOR", "PR", "2024-06-01"))
  viz <- patient_profile_static_vizs()[["patient_info"]]
  out <- viz$exhibit(dm_obj, NULL, list())
  expect_named(out, c("Field", "Value"))
})

test_that("a coloured fact renders as the cohort list's chip", {
  dm_obj <- resp_dm(rs("BOR", "PR", "2024-06-01"))
  viz <- patient_profile_static_vizs()[["patient_info"]]
  html <- as.character(htmltools::doRenderTags(
    viz$render(dm_obj, NULL, list(resp_colors = c(PR = "#FFD700")),
               NA_real_, "date")
  ))
  expect_match(html, "pp-info-chip")
  # The very style pp_cohort_chip_style() computes, not a second tint.
  expect_match(html, "rgba(255,215,0,0.12)", fixed = TRUE)
})
