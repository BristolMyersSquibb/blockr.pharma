# The study-specific panels' tooltips: response lane, treatment cycles,
# ADAS-Cog, orthostatic BP, NPI-X radar and the questionnaire heatmap. Each
# point carries its tooltip as `tip` (pp-tooltip.R), so the content is
# asserted here, on the rendered chart's data.

tip_rows <- function(tip) {
  stats::setNames(
    vapply(tip$rows, function(r) {
      if (is.null(r$meta)) r$value else paste(r$value, r$meta, sep = " | ")
    }, character(1)),
    vapply(tip$rows, `[[`, character(1), "label")
  )
}

series_tips <- function(chart, i = 1L) {
  lapply(chart$x$opts$series[[i]]$data, `[[`, "tip")
}

uses_tip_formatter <- function(chart) {
  all(vapply(chart$x$opts$series, function(s) {
    identical(as.character(s$tooltip$formatter),
              as.character(PP_TIP_FORMATTER))
  }, logical(1))) && is.null(chart$x$opts$tooltip$formatter)
}

# ---------------------------------------------------------------------------
# Response lane
# ---------------------------------------------------------------------------

resp_tip_dm <- function(ady = TRUE) {
  adrs <- data.frame(
    USUBJID = "S1", PARAMCD = "OVR",
    PARAM = "Overall Response by Investigator",
    AVALC = c("SD", "PR", "PD"),
    ADT = as.Date(c("2024-02-01", "2024-04-01", "2024-06-01")),
    stringsAsFactors = FALSE
  )
  if (ady) adrs$ADY <- c(32, 92, 153)
  pp_scope_subject(
    pp_normalize_dm(dm::dm(
      adsl = data.frame(USUBJID = "S1", TRTSDT = as.Date("2024-01-01")),
      adrs = adrs
    )),
    "S1"
  )
}

test_that("a response bar names its category in words and its span", {
  dm_obj <- resp_tip_dm()
  viz <- pp_response_vizs(dm_obj)[["response_OVR"]]
  ref <- pp_ms_ts(as.Date("2024-01-01"))
  chart <- viz$render(dm_obj, as.Date(c("2024-01-01", "2024-12-31")),
                      list(), ref, "rday")
  tips <- series_tips(chart)

  expect_equal(vapply(tips, `[[`, character(1), "head"),
               c("Stable disease", "Partial response", "Progressive disease"))
  expect_equal(tips[[2]]$sub, "Overall response by investigator")
  # The swatch is the bar's own colour.
  expect_equal(tips[[2]]$color, chart$x$opts$series[[1]]$data[[2]]$value[[9]])
  expect_equal(tip_rows(tips[[1]]),
               c(From = "D32 | 2024-02-01", To = "D92 | 60 days"))
  # The last assessment holds until the next one, which has not happened.
  expect_equal(tip_rows(tips[[3]]),
               c(From = "D153 | 2024-06-01", To = "ongoing"))
  expect_true(uses_tip_formatter(chart))
})

test_that("a response bar in date mode leads with the date", {
  dm_obj <- resp_tip_dm(ady = FALSE)
  viz <- pp_response_vizs(dm_obj)[["response_OVR"]]
  chart <- viz$render(dm_obj, as.Date(c("2024-01-01", "2024-12-31")),
                      list(), mode = "date")
  tips <- series_tips(chart)
  expect_equal(tip_rows(tips[[1]]),
               c(From = "2024-02-01", To = "2024-04-01 | 60 days"))
})

test_that("a response bar cut by the window still ends at the next one", {
  dm_obj <- resp_tip_dm()
  viz <- pp_response_vizs(dm_obj)[["response_OVR"]]
  ref <- pp_ms_ts(as.Date("2024-01-01"))
  # From D100 on, only the PR bar (D92 to D153) and the PD bar are in view.
  chart <- viz$render(dm_obj, as.Date(c("2024-04-10", "2024-12-31")),
                      list(), ref, "rday")
  tips <- series_tips(chart)
  expect_equal(tips[[1]]$head, "Partial response")
  expect_equal(tip_rows(tips[[1]])[["To"]], "D153 | 61 days")
})

test_that("response codes read as words, unknown ones as written", {
  expect_equal(pp_resp_word("NON-CR/NON-PD"), "Non-CR/non-PD")
  expect_equal(pp_resp_word("ne"), "Not evaluable")
  expect_equal(pp_resp_word("NR"), "NR")
  expect_equal(pp_resp_word("NOT DONE"), "Not done")
})

test_that("a parameter name keeps its codes in sentence case", {
  expect_equal(pp_tip_param_words("Best Overall Response of CR/PR by Investigator"),
               "Best overall response of CR/PR by investigator")
  expect_equal(pp_tip_param_words("WORD RECALL TASK"), "Word recall task")
  expect_equal(pp_tip_param_words("Word Recall (Delayed)"),
               "Word recall (delayed)")
})

# ---------------------------------------------------------------------------
# Treatment cycles
# ---------------------------------------------------------------------------

cycle_anchor_rows <- function(estimated = c(FALSE, TRUE)) {
  data.frame(
    USUBJID = "S1", cycle = 1:2,
    cycle_start = as.Date(c("2014-01-01", "2014-01-22")),
    cycle_end = as.Date(c("2014-01-21", "2014-02-11")),
    estimated = estimated
  )
}

test_that("a cycle band says which cycle, from when, for how long", {
  ref <- pp_ms_ts(as.Date("2014-01-01"))
  chart <- cycle_viz$render(dm::dm(), as.Date(c("2014-01-01", "2014-02-28")),
                            list(cycle_anchors = cycle_anchor_rows()),
                            ref, "rday")
  tips <- series_tips(chart)
  expect_equal(tips[[1]]$head, "Cycle 1")
  expect_equal(tip_rows(tips[[1]]),
               c(From = "D1 | 2014-01-01", To = "D21 | 21 days"))
  expect_null(tips[[1]]$note)
  expect_equal(tips[[2]]$note,
               "The Day 1 visit is missing: the start is inferred")
  expect_true(uses_tip_formatter(chart))
})

# ---------------------------------------------------------------------------
# ADAS-Cog trajectory
# ---------------------------------------------------------------------------

adas_tip_dm <- function() {
  pp_normalize_dm(dm::dm(adqsadas = data.frame(
    USUBJID = "S1",
    PARAMCD = c("ACTOT", "ACTOT", "ACITM01", "ACITM01"),
    PARAM = c("Adas-Cog(11) Subscore", "Adas-Cog(11) Subscore",
              "Word Recall Task", "Word Recall Task"),
    AVISIT = c("BASELINE", "WEEK 8", "BASELINE", "WEEK 8"),
    ADT = as.Date(c("2014-01-02", "2014-02-27", "2014-01-02", "2014-02-27")),
    ADY = c(2, 58, 2, 58),
    AVAL = c(21, 24, 5, 6),
    CHG = c(NA, 3, NA, 1),
    stringsAsFactors = FALSE
  )))
}

test_that("an ADAS-Cog point names the score, its change and its visit", {
  ref <- pp_ms_ts(as.Date("2014-01-01"))
  chart <- adas_trajectory_viz$render(
    adas_tip_dm(), as.Date(c("2014-01-01", "2014-03-31")),
    list(items = c("ACTOT", "ACITM01")), ref, "rday"
  )
  s <- chart$x$opts$series
  total <- Filter(function(x) identical(x$lineStyle$width, 2.5), s)[[1]]
  item <- Filter(function(x) identical(x$lineStyle$width, 1.5), s)[[1]]

  tip <- total$data[[2]]$tip
  expect_equal(tip$head, "ADAS-Cog total")
  expect_equal(tip$color, total$lineStyle$color)
  expect_equal(tip_rows(tip), c(
    "Analysis value" = "24", "Change from baseline" = "+3",
    Visit = "Week 8", Day = "D58 | 2014-02-27"
  ))
  # Baseline has no change to state.
  expect_false("Change from baseline" %in% names(tip_rows(total$data[[1]]$tip)))
  expect_equal(item$data[[1]]$tip$head, "Word recall task")
  expect_true(uses_tip_formatter(chart))
})

# ---------------------------------------------------------------------------
# Orthostatic BP
# ---------------------------------------------------------------------------

test_that("an orthostatic BP point names parameter, position and visit", {
  advs <- data.frame(
    USUBJID = "S1", PARAMCD = "SYSBP", AVISIT = "WEEK 2",
    ATPT = c("AFTER LYING DOWN FOR 5 MINUTES", "AFTER STANDING FOR 1 MINUTE"),
    AVAL = c(120, 112), CHG = c(-2, 4), stringsAsFactors = FALSE
  )
  chart <- ortho_bp_viz$render(pp_normalize_dm(dm::dm(advs = advs)), NULL,
                               list())
  tips <- series_tips(chart)
  expect_equal(tips[[2]]$head, "Systolic blood pressure, standing 1 minute")
  expect_equal(tips[[2]]$color, chart$x$opts$series[[1]]$lineStyle$color)
  expect_equal(tip_rows(tips[[2]]), c(
    "Analysis value" = "112 mmHg", "Change from baseline" = "+4 mmHg",
    Visit = "Week 2"
  ))
  expect_equal(tip_rows(tips[[1]])[["Change from baseline"]], "-2 mmHg")
  expect_true(uses_tip_formatter(chart))
})

# ---------------------------------------------------------------------------
# NPI-X radar
# ---------------------------------------------------------------------------

test_that("an NPI-X polygon names its visit and lists the domains", {
  adqsnpix <- data.frame(
    USUBJID = "S1",
    PARAMCD = c("NPITM01S", "NPITM02S", "NPITM01S"),
    AVISIT = c("BASELINE", "BASELINE", "WEEK 4"),
    AVAL = c(3, 0, 2.5),
    stringsAsFactors = FALSE
  )
  chart <- npix_radar_viz$render(pp_normalize_dm(dm::dm(adqsnpix = adqsnpix)),
                                 NULL, list())
  polys <- chart$x$opts$series[[1]]$data
  base <- Filter(function(p) identical(p$name, "BASELINE"), polys)[[1]]$tip
  wk4 <- Filter(function(p) identical(p$name, "WEEK 4"), polys)[[1]]$tip
  expect_equal(base$head, "Baseline")
  expect_equal(tip_rows(base), c(Delusions = "3", Hallucinations = "0"))
  # A domain not assessed at the visit has no row.
  expect_equal(tip_rows(wk4), c(Delusions = "2.5"))
  expect_true(uses_tip_formatter(chart))
})

# ---------------------------------------------------------------------------
# Questionnaire heatmap
# ---------------------------------------------------------------------------

heat_tip_dm <- function() {
  row <- function(tbl_items) {
    data.frame(
      USUBJID = "S1", PARAMCD = tbl_items, PARAM = paste(tbl_items, "Item"),
      AVISIT = c("BASELINE", "WEEK 8"), AVISITN = c(0, 8),
      AVAL = c(5, 7), CHG = c(0, 2), stringsAsFactors = FALSE
    )
  }
  adqsadas <- row("ACITM01")
  adqsadas$PARAM <- "Word Recall Task"
  pp_normalize_dm(dm::dm(adqsadas = adqsadas, adqsnpix = row("NPITM01S")))
}

test_that("a heatmap cell names the item, its value and its visit", {
  chart <- questionnaire_heatmap_viz$render(heat_tip_dm(), NULL, list())
  cells <- chart$x$opts$series[[1]]$data
  expect_equal(cells[[2]]$value, list(1L, 0L, 7))
  expect_equal(cells[[2]]$tip$head, "Word recall task")
  expect_equal(tip_rows(cells[[2]]$tip),
               c("Analysis value" = "7", Visit = "Week 8"))
  expect_true(uses_tip_formatter(chart))
  # The card's own styling, no inline card of its own.
  expect_equal(chart$x$opts$tooltip, pp_tooltip())

  chg <- questionnaire_heatmap_viz$render(heat_tip_dm(), NULL,
                                          list(value = "CHG"))
  expect_equal(tip_rows(chg$x$opts$series[[1]]$data[[2]]$tip),
               c("Change from baseline" = "+2", Visit = "Week 8"))
})
