# The cohort band follows the panels, and a panel search filters both.
#
# Two behaviours meet in the sidebar strip, and each has a way of going wrong
# quietly:
#
#   the band SOURCE -- the strip used to be the AE band whatever the profile
#     was showing. It now draws the first selected viz that declares a band
#     form, and the ones that cannot be reduced to 176px of one row are
#     stepped over. The failure to guard against is a rule that changes the
#     default picture: patient overview is first in every default selection
#     and must be skipped, or every existing board's sidebar changes.
#
#   the SEARCH -- the panel's find box filters the strip too, so the two show
#     one subset. The failure to guard against is disagreement: a panel
#     showing two events beside a strip painting twenty-five.

band_adsl <- function() {
  data.frame(
    USUBJID = c("S-1", "S-2", "S-3"),
    ACTARM = c("Placebo", "Active", "Active"),
    ACTARMCD = c("Pbo", "A10", "A10"),
    TRTSDT = as.Date(c("2024-01-01", "2024-01-10", "2024-02-01")),
    TRTEDT = as.Date(c("2024-06-28", "2024-02-08", "2024-03-01")),
    stringsAsFactors = FALSE
  )
}

band_adae <- function() {
  data.frame(
    USUBJID = c("S-1", "S-1", "S-1", "S-2"),
    AEDECOD = c("PNEUMONIA", "HEADACHE", "PNEUMONIA ASPIRATION", "HEADACHE"),
    AEBODSYS = c("INFECTIONS AND INFESTATIONS", "NERVOUS SYSTEM DISORDERS",
                 "INFECTIONS AND INFESTATIONS", "NERVOUS SYSTEM DISORDERS"),
    ASTDY = c(5, 20, 60, 3),
    AENDY = c(9, 24, 70, 12),
    AESEV = c("SEVERE", "MILD", "MODERATE", "MILD"),
    stringsAsFactors = FALSE
  )
}

band_adlbc <- function() {
  # Three visits each for S-1 and S-2, one for S-3 (a single point is a
  # legitimate series and must not draw as nothing), and one patient running
  # far above the rest so the clipped scale has something to clip.
  data.frame(
    USUBJID = c(rep("S-1", 3), rep("S-2", 3), "S-3"),
    PARAMCD = "ALT",
    PARAM = "Alanine Aminotransferase",
    AVAL = c(20, 30, 40, 25, 90, 300, 33),
    ADY = c(1, 30, 60, 1, 30, 60, 1),
    A1LO = 7, A1HI = 56,
    stringsAsFactors = FALSE
  )
}

band_dm <- function(...) pp_normalize_dm(dm::dm(...))

viz_stub <- function(id, band = NULL, tbl = "adsl") {
  new_pp_viz(
    id = id, label = toupper(id), domain = "Test", icon = "x",
    color = "#000", description = "", tables = tbl, band = band,
    render = function(...) NULL
  )
}

# --- which panel drives the band --------------------------------------------

test_that("the band skips panels that have no strip form", {
  avail <- list(
    patient_overview = viz_stub("patient_overview"),
    ae_gantt = viz_stub("ae_gantt", pp_band_ae(), "adae")
  )
  src <- pp_cohort_band_source(c("patient_overview", "ae_gantt"), avail)

  # The default selection leads with the overview, which declares no band.
  # If it ever drew, every existing board's sidebar would change on upgrade.
  expect_identical(src$viz_id, "ae_gantt")
  expect_identical(src$caption, "AE_GANTT")
})

test_that("reordering the panels reorders what the band draws", {
  avail <- list(
    patient_overview = viz_stub("patient_overview"),
    ae_gantt = viz_stub("ae_gantt", pp_band_ae(), "adae"),
    chem = viz_stub("chem",
                    pp_band_series("adlbc", "ALT", "Alanine Aminotransferase"),
                    "adlbc")
  )
  ae_first <- pp_cohort_band_source(
    c("patient_overview", "ae_gantt", "chem"), avail
  )
  chem_first <- pp_cohort_band_source(
    c("patient_overview", "chem", "ae_gantt"), avail
  )

  expect_identical(ae_first$viz_id, "ae_gantt")
  expect_identical(chem_first$viz_id, "chem")
  # A series band names its parameter: the panel alone would not say which of
  # a dozen the line is. The CODE in the caption, which fits a 232px sidebar,
  # and the full name in the tooltip, which does not.
  expect_identical(chem_first$caption, "CHEM · ALT")
  expect_match(chem_first$title, "Alanine Aminotransferase", fixed = TRUE)
})

test_that("nothing drawable selected leaves an empty track, not the AE band", {
  avail <- list(patient_info = viz_stub("patient_info"))
  expect_null(pp_cohort_band_source("patient_info", avail))

  # And the marks agree: no band means no geometry, not a fallback to
  # adverse events from a panel that is not on screen.
  d <- band_dm(adsl = band_adsl(), adae = band_adae())
  m <- pp_cohort_marks(d, pp_resolve_roles(d), band = NULL)
  expect_identical(m$kind, "none")
  expect_length(m$subjects, 0L)
})

test_that("a selected viz the data cannot offer is passed over", {
  # Availability is what the chart area draws; a saved board naming a viz
  # this study lacks must not take the band down with it.
  avail <- list(ae_gantt = viz_stub("ae_gantt", pp_band_ae(), "adae"))
  src <- pp_cohort_band_source(c("not_here", "ae_gantt"), avail)
  expect_identical(src$viz_id, "ae_gantt")
})

# --- the series band ---------------------------------------------------------

test_that("a series band draws one parameter per patient on the shared axis", {
  d <- band_dm(adsl = band_adsl(), adlbc = band_adlbc())
  m <- pp_cohort_marks(d, pp_resolve_roles(d),
                       band = pp_band_series("adlbc", "ALT", "ALT"))

  expect_identical(m$kind, "series")
  expect_named(m$subjects, c("S-1", "S-2", "S-3"))
  expect_identical(m$subjects[["S-1"]]$series$value, c(20, 30, 40))
  # In day order, not record order: a line is drawn in the order its points
  # arrive and a findings table is in neither.
  expect_identical(m$subjects[["S-1"]]$series$day, c(1, 30, 60))
  # One patient, one visit: a point, and not an absence.
  expect_identical(nrow(m$subjects[["S-3"]]$series), 1L)
})

test_that("the value scale is shared and clipped, and the outlier is ticked", {
  d <- band_dm(adsl = band_adsl(), adlbc = band_adlbc())
  m <- pp_cohort_marks(d, pp_resolve_roles(d),
                       band = pp_band_series("adlbc", "ALT", "ALT"))

  # The ceiling is the cohort's 95th percentile, NOT its maximum: one patient
  # at 300 would otherwise flatten the six values between 20 and 40 into the
  # bottom pixel of every other row.
  expect_lt(m$vhi, 300)
  expect_identical(unname(m$limit), 56)

  # The clipped value is drawn at the edge and ticked, never dropped: a line
  # that silently leaves out its highest point is the one reading a clinician
  # must not get from a liver enzyme.
  g <- pp_cohort_series_geom(m$subjects[["S-2"]], m)
  expect_length(g$clip, 1L)
  expect_match(g$path, "^M[0-9.]+ [0-9.]+")

  # Every row measures the limit against the same scale, so the hairline sits
  # at one height across the list.
  g1 <- pp_cohort_series_geom(m$subjects[["S-1"]], m)
  expect_identical(g1$limit, g$limit)
})

test_that("a one-visit series ships as a point, not as an empty band", {
  d <- band_dm(adsl = band_adsl(), adlbc = band_adlbc())
  m <- pp_cohort_marks(d, pp_resolve_roles(d),
                       band = pp_band_series("adlbc", "ALT", "ALT"))
  attr <- pp_cohort_band_attr(m$subjects[["S-3"]], m, pp_cohort_sev_color())

  expect_match(attr$dot, "^[0-9.]+,[0-9.]+$")
  svg <- pp_cohort_band_svg(m$subjects[["S-3"]], m, pp_cohort_sev_color())
  expect_match(svg, "<circle", fixed = TRUE)
})

test_that("a patient with no rows for the parameter keeps an empty track", {
  d <- band_dm(adsl = band_adsl(),
               adlbc = band_adlbc()[band_adlbc()$USUBJID != "S-2", ])
  m <- pp_cohort_marks(d, pp_resolve_roles(d),
                       band = pp_band_series("adlbc", "ALT", "ALT"))

  expect_null(m$subjects[["S-2"]]$series)
  attr <- pp_cohort_band_attr(m$subjects[["S-2"]], m, pp_cohort_sev_color())
  expect_identical(attr$band, "")
  # The track is still drawn: an absent row would read as broken, an empty
  # one reads as "nothing measured", which is a finding.
  svg <- pp_cohort_band_svg(m$subjects[["S-2"]], m, pp_cohort_sev_color())
  expect_match(svg, "^<svg")
})

# --- the search --------------------------------------------------------------

test_that("the search matches every coding level the study carries", {
  ae <- band_adae()

  # The verbatim term, and the one that only the body system carries.
  expect_identical(sum(pp_search_match(ae, c("AEDECOD", "AEBODSYS"),
                                       "pneumonia")), 2L)
  expect_identical(sum(pp_search_match(ae, c("AEDECOD", "AEBODSYS"),
                                       "infections")), 2L)
  # Case-insensitive, and a blank box matches everything.
  expect_identical(sum(pp_search_match(ae, "AEDECOD", "HeAdAcHe")), 2L)
  expect_true(all(pp_search_match(ae, "AEDECOD", "")))

  # A term matching nothing matches NOTHING. Falling back to everything
  # would answer a question that was not asked.
  expect_false(any(pp_search_match(ae, "AEDECOD", "xyz")))

  # A study carrying none of the declared columns cannot answer, and
  # blanking its panel would be a worse answer than ignoring the box.
  expect_true(all(pp_search_match(ae, "AEHLT", "pneumonia")))
})

test_that("the search filters the band and counts what it kept", {
  d <- band_dm(adsl = band_adsl(), adae = band_adae())
  roles <- pp_resolve_roles(d)

  all_ev <- pp_cohort_marks(d, roles)
  hit <- pp_cohort_marks(d, roles, search = "pneumonia")

  expect_identical(nrow(all_ev$subjects[["S-1"]]$events), 3L)
  expect_identical(nrow(hit$subjects[["S-1"]]$events), 2L)
  # S-2's only event is a headache, so its band goes empty -- and its count
  # says zero rather than nothing, which is what tells "none of what you
  # asked for" apart from "no data".
  expect_identical(nrow(hit$subjects[["S-2"]]$events), 0L)
  expect_identical(unname(hit$hits[["S-2"]]), 0L)
  expect_identical(unname(hit$hits[["S-1"]]), 2L)

  # No search, no counts: the row prints a number only while one is running.
  expect_null(all_ev$hits)
})

test_that("the search reaches a band through the body system alone", {
  d <- band_dm(adsl = band_adsl(), adae = band_adae())
  m <- pp_cohort_marks(d, pp_resolve_roles(d), search = "infections")
  expect_identical(nrow(m$subjects[["S-1"]]$events), 2L)
})

test_that("a band declaring no search columns ignores the term", {
  d <- band_dm(adsl = band_adsl(), adae = band_adae())
  band <- pp_band_spans("adae", c("ASTDY", "ASTDT"), c("AENDY", "AENDT"))
  m <- pp_cohort_marks(d, pp_resolve_roles(d), band = band,
                       search = "pneumonia")
  expect_identical(nrow(m$subjects[["S-1"]]$events), 3L)
})

test_that("the AE panel and the cohort band filter the same records", {
  # The disagreement this pair exists to prevent: a panel showing two events
  # beside a strip painting three.
  d <- band_dm(adsl = band_adsl(), adae = band_adae())
  ae <- as.data.frame(dm::dm_get_tables(d)$adae)
  panel_n <- sum(pp_search_match(ae[ae$USUBJID == "S-1", ],
                                 c("AETERM", "AEDECOD", "AEHLT", "AEBODSYS"),
                                 "pneumonia"))
  band_n <- nrow(
    pp_cohort_marks(d, pp_resolve_roles(d),
                    search = "pneumonia")$subjects[["S-1"]]$events
  )
  expect_identical(band_n, panel_n)
})

test_that("the hit count is the patient's records, not the cohort's", {
  d <- band_dm(adsl = band_adsl(), adae = band_adae())
  scoped <- pp_scope_subject(d, "S-1")
  ctrl <- list(columns = c("AEDECOD", "AEBODSYS"))

  hits <- pp_ctrl_search_hits(ctrl, scoped, "adae", "pneumonia")
  expect_identical(hits$n, 2L)
  expect_identical(hits$total, 3L)
  # Nothing typed, nothing reported: the box shows a count only while it is
  # filtering.
  expect_null(pp_ctrl_search_hits(ctrl, scoped, "adae", ""))
})

test_that("the series band draws the panel's first CHART, not its first chip", {
  # The chips are ordered by parameter NAME and the charts by PARAMCD
  # (pp_render_findings() sorts them), so the two disagree whenever the
  # alphabet does. On safetyData's chemistry card that put "Chemistry . ALT"
  # in the sidebar caption above a panel whose top plot was ALB -- the band
  # and the panel naming different parameters for the same card.
  lb <- data.frame(
    USUBJID = rep(c("S-1", "S-2"), each = 4L),
    PARAMCD = rep(c("ZAL", "ABC"), times = 4L),
    PARAM = rep(c("Aaa First By Name", "Zzz Last By Name"), times = 4L),
    AVAL = c(10, 20, 11, 21, 12, 22, 13, 23),
    ADY = rep(c(1, 1, 30, 30), times = 2L),
    ADT = as.Date("2024-01-01") + rep(c(0, 0, 29, 29), times = 2L),
    stringsAsFactors = FALSE
  )
  d <- band_dm(adsl = band_adsl()[1:2, ], adlbc = lb)
  viz <- pp_findings_vizs(d)[["adlbc_all"]]

  # By name the card leads with "Aaa First By Name" (ZAL); by code the panel
  # draws ABC first. The band follows the panel.
  expect_identical(names(viz$params)[[1L]], "ZAL")
  expect_identical(viz$band$paramcd, "ABC")
  expect_identical(viz$band$param, "Zzz Last By Name")
})

# --- the curve ---------------------------------------------------------------

test_that("the strip rounds its corners the way the panel does", {
  d <- band_dm(adsl = band_adsl(), adlbc = band_adlbc())
  m <- pp_cohort_marks(d, pp_resolve_roles(d),
                       band = pp_band_series("adlbc", "ALT", "ALT"))
  sub <- m$subjects[["S-1"]]

  curved <- pp_cohort_series_geom(sub, m, smooth = TRUE)$path
  straight <- pp_cohort_series_geom(sub, m, smooth = FALSE)$path

  # Cubic segments when the gear says Smooth, line segments when it says
  # Straight. The panels read the same setting.
  expect_match(curved, "C", fixed = TRUE)
  expect_false(grepl("C", straight, fixed = TRUE))
  expect_match(straight, "L", fixed = TRUE)

  # Both start at the same first point: smoothing changes the joins, never
  # where a measurement sits.
  expect_identical(sub("[CL].*$", "", curved), sub("L.*$", "", straight))
})

test_that("the curve never overshoots a recorded value", {
  # Monotone cubic, not Catmull-Rom. A plain spline dips below a patient's
  # lowest reading on the way into a trough, which on a lab strip is a value
  # nobody measured.
  x <- c(0, 40, 80, 120)
  y <- c(10, 2, 2, 10)
  dstr <- pp_monotone_path(x, y)
  ctrl <- as.numeric(regmatches(dstr, gregexpr("-?[0-9.]+", dstr))[[1]])
  # Every coordinate the path names, control points included, stays inside
  # the envelope of the data.
  ys <- ctrl[seq(2L, length(ctrl), by = 2L)]
  expect_true(all(ys >= min(y) - 1e-9))
  expect_true(all(ys <= max(y) + 1e-9))
})

test_that("two records on one day do not put a vertical inside the curve", {
  # A zero-width interval is an infinite slope; the first record is kept.
  expect_match(pp_monotone_path(c(0, 0, 50, 100), c(5, 9, 6, 7)),
               "^M0 5C")
  # And a series that collapses to one x is a point, not a path.
  expect_identical(pp_monotone_path(c(20, 20), c(4, 8)), "M20 4")
})

test_that("the band follows the chips, not the card's declaration", {
  # Deselect everything but one parameter and the panel leads with it; the
  # strip has to name the same one, or the caption points at a chart that is
  # no longer on screen.
  avail <- list(chem = viz_stub("chem",
                                pp_band_series("adlbc", "ALB", "Albumin"),
                                "adlbc"))
  avail$chem$params <- c(ALB = "Albumin", ALP = "Alkaline Phosphatase",
                         ALT = "Alanine Aminotransferase")

  declared <- pp_cohort_band_source("chem", avail)
  expect_identical(declared$band$paramcd, "ALB")

  picked <- pp_cohort_band_source("chem", avail,
                                  list(chem = list(items = "ALT")))
  expect_identical(picked$band$paramcd, "ALT")
  expect_identical(picked$caption, "CHEM \u00b7 ALT")
  expect_match(picked$title, "Alanine Aminotransferase", fixed = TRUE)

  # Several chips on: the panel sorts its charts by PARAMCD, so the strip
  # takes the lowest of them and not the one clicked first.
  many <- pp_cohort_band_source("chem", avail,
                                list(chem = list(items = c("ALT", "ALP"))))
  expect_identical(many$band$paramcd, "ALP")

  # Nothing selected keeps the declared parameter: the panel draws nothing in
  # that state and a strip is more use than an empty track.
  none <- pp_cohort_band_source("chem", avail,
                                list(chem = list(items = character())))
  expect_identical(none$band$paramcd, "ALB")

  # A stale saved selection naming a parameter this card does not cover is
  # ignored rather than followed off a cliff.
  stale <- pp_cohort_band_source("chem", avail,
                                 list(chem = list(items = "NOPE")))
  expect_identical(stale$band$paramcd, "ALB")
})

