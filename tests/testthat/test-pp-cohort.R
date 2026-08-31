# The cohort as data: the frame the sidebar list and the download are both
# built from, and the event geometry the row band draws.
#
# The split is the point. pp_cohort_frame() is the seam a study replaces to
# change what gets exported; pp_cohort_marks() is drawing input that must
# never reach a spreadsheet. Tests here pin the contract of each, and the
# handful of cases where "absent" and "zero" mean different things.

cohort_dm <- function(adsl, adae = NULL) {
  if (is.null(adae)) {
    pp_normalize_dm(dm::dm(adsl = adsl))
  } else {
    pp_normalize_dm(dm::dm(adsl = adsl, adae = adae))
  }
}

test_adsl <- function() {
  data.frame(
    USUBJID = c("S-1", "S-2", "S-3"),
    ACTARM = c("Placebo", "Active 10mg", "Active 10mg"),
    ACTARMCD = c("Pbo", "A10", "A10"),
    SEX = c("F", "M", "F"),
    AGE = c(61, 74, 55),
    TRTSDT = as.Date(c("2024-01-01", "2024-01-10", "2024-02-01")),
    TRTEDT = as.Date(c("2024-06-28", "2024-02-08", "2024-03-01")),
    stringsAsFactors = FALSE
  )
}

test_adae <- function() {
  data.frame(
    USUBJID = c("S-1", "S-1", "S-2"),
    AEDECOD = c("Headache", "Nausea", "Rash"),
    ASTDY = c(5, 40, 3),
    AENDY = c(9, 44, 12),
    AESEV = c("MILD", "SEVERE", "MODERATE"),
    stringsAsFactors = FALSE
  )
}

test_that("the frame is one row per patient, in cohort order", {
  d <- cohort_dm(test_adsl(), test_adae())
  roles <- pp_resolve_roles(d, list(arm = "ACTARM"))
  f <- pp_cohort_frame(d, roles)

  expect_identical(nrow(f), 3L)
  expect_identical(f$USUBJID, c("S-1", "S-2", "S-3"))
  expect_identical(f$ARM, c("Placebo", "Active 10mg", "Active 10mg"))
  expect_identical(f$ARMCD, c("Pbo", "A10", "A10"))
  # Days on treatment is derived, because two dates are not the number a
  # reader wants.
  expect_identical(f$TRTDURD[[2]], 30)
})

test_that("a column the study does not carry is absent, not NA", {
  adsl <- test_adsl()
  adsl$AGE <- NULL
  f <- pp_cohort_frame(cohort_dm(adsl), pp_resolve_roles(cohort_dm(adsl)))
  expect_false("AGE" %in% names(f))
  # ...and a missing arm code drops the column rather than erroring: the
  # sidebar falls back to the arm colour.
  adsl$ACTARMCD <- NULL
  d <- cohort_dm(adsl)
  f2 <- pp_cohort_frame(d, pp_resolve_roles(d))
  expect_false("ARMCD" %in% names(f2))
})

test_that("no adae drops the AE columns; no events is a zero", {
  d_none <- cohort_dm(test_adsl())
  f_none <- pp_cohort_frame(d_none, pp_resolve_roles(d_none))
  expect_false("AE_N" %in% names(f_none))

  d <- cohort_dm(test_adsl(), test_adae())
  f <- pp_cohort_frame(d, pp_resolve_roles(d, list(arm = "ACTARM")))
  # S-3 has no events. That is a zero and an NA severity, never a dropped row.
  expect_identical(f$AE_N, c(2L, 1L, 0L))
  expect_identical(f$AE_WORST, c("SEVERE", "MODERATE", NA_character_))
})

test_that("worst severity ranks words and grades, and unknowns lose", {
  expect_identical(pp_sev_rank(c("MILD", "SEVERE", "MODERATE")), c(1, 3, 2))
  expect_identical(pp_sev_rank(c("1", "4", "2")), c(1, 4, 2))
  # anything unranked sorts below a real severity rather than winning
  expect_identical(pp_sev_rank(c("MILD", "UNKNOWN")), c(1, 0))
})

test_that("marks put every patient on one axis", {
  d <- cohort_dm(test_adsl(), test_adae())
  m <- pp_cohort_marks(d, pp_resolve_roles(d, list(arm = "ACTARM")))

  # The axis reaches the longest-treated patient, so shorter rows are read
  # against them rather than each being stretched to its own width.
  expect_identical(m$days, 180)
  expect_identical(names(m$subjects), c("S-1", "S-2", "S-3"))
  expect_identical(nrow(m$subjects[["S-1"]]$events), 2L)
  # A patient with no events keeps a row with none, never absent.
  expect_identical(nrow(m$subjects[["S-3"]]$events), 0L)
  expect_identical(m$subjects[["S-2"]]$trt_end, 30)
})

test_that("an event with no end day is ongoing, and runs to the axis end", {
  # The AE gantt draws a missing end as an open bar reaching the axis end
  # (pp_gantt_open_end()). The band has to agree: a patient whose profile
  # shows an event running the whole study must not show a one-day tick in
  # the list that opened it.
  adae <- test_adae()
  adae$AENDY <- c(NA, 44, NA)
  d <- cohort_dm(test_adsl(), adae)
  m <- pp_cohort_marks(d, pp_resolve_roles(d, list(arm = "ACTARM")))
  expect_identical(m$subjects[["S-1"]]$events$end[[1]], m$days)
  # a real end is left alone
  expect_identical(m$subjects[["S-1"]]$events$end[[2]], 44)
  # ...and an open event does not itself define the axis, or the two would
  # be circular
  expect_identical(m$days, 180)
})

test_that("an end before the start is treated as no end, not a backwards bar", {
  adae <- test_adae()
  adae$AENDY <- c(1, 44, 12)
  d <- cohort_dm(test_adsl(), adae)
  m <- pp_cohort_marks(d, pp_resolve_roles(d, list(arm = "ACTARM")))
  ev <- m$subjects[["S-1"]]$events
  expect_gte(ev$end[[1]], ev$start[[1]])
})

test_that("the band paints overlaps once, by worst severity", {
  d <- cohort_dm(test_adsl(), test_adae())
  m <- pp_cohort_marks(d, pp_resolve_roles(d, list(arm = "ACTARM")))
  col <- pp_cohort_sev_color(NULL)

  svg <- pp_cohort_band_svg(m$subjects[["S-1"]], m$days, col)
  expect_match(svg, "^<svg")
  # the track plus one rect per painted bin -- not one per event
  expect_gt(length(gregexpr("<rect", svg)[[1]]), 1)
  # SEVERE is in there (the second event), by its constant
  expect_match(svg, pp_sev_fallback_color("SEVERE"), fixed = TRUE)

  # An eventless patient still draws: the track alone reads as "nothing
  # happened", which is a finding, where a missing band reads as broken.
  empty <- pp_cohort_band_svg(m$subjects[["S-3"]], m$days, col)
  expect_match(empty, "^<svg")
  expect_identical(length(gregexpr("<rect", empty)[[1]]), 1L)
})

test_that("the end-of-treatment diamond is drawn only when it ended early", {
  d <- cohort_dm(test_adsl(), test_adae())
  m <- pp_cohort_marks(d, pp_resolve_roles(d, list(arm = "ACTARM")))
  col <- pp_cohort_sev_color(NULL)
  # S-2 stopped at day 30 of a 180-day axis
  expect_match(pp_cohort_band_svg(m$subjects[["S-2"]], m$days, col), "<path")
  # S-1 ran to the end of the axis, so there is nothing to mark
  expect_false(grepl("<path", pp_cohort_band_svg(m$subjects[["S-1"]], m$days,
                                                 col)))
})

test_that("sorting offers only keys the data supports, and signals descend", {
  d <- cohort_dm(test_adsl(), test_adae())
  f <- pp_cohort_frame(d, pp_resolve_roles(d, list(arm = "ACTARM")))

  # THREE at most: the control is a click-through pill, so every extra rung
  # is another click to walk past.
  expect_named(pp_cohort_sort_choices(f), c("id", "worst", "ae"))
  # no adae: only id survives, and a one-rung pill is not a choice -- the
  # control draws nothing rather than offering a sort that cannot change
  d2 <- cohort_dm(test_adsl())
  expect_named(pp_cohort_sort_choices(pp_cohort_frame(d2, pp_resolve_roles(d2))),
               "id")

  expect_identical(f$USUBJID[pp_cohort_order(f, "id")],
                   c("S-1", "S-2", "S-3"))
  # worst first: the point of sorting by severity is to see the severe ones
  expect_identical(f$USUBJID[pp_cohort_order(f, "worst")][[1]], "S-1")
  expect_identical(f$USUBJID[pp_cohort_order(f, "ae")][[1]], "S-1")
  # the orders behind the dropped rungs still work: pp_cohort_order() is what
  # the DOWNLOAD sorts by too, and a saved sort could name either
  expect_identical(f$USUBJID[pp_cohort_order(f, "duration")][[1]], "S-1")
  expect_identical(f$USUBJID[pp_cohort_order(f, "arm")][[1]], "S-2")
  # an unknown key is the default order, never an error
  expect_identical(pp_cohort_order(f, "nonsense"), pp_cohort_order(f, "id"))
})

test_that("an empty cohort yields an empty frame, not an error", {
  adsl <- test_adsl()[0, ]
  d <- cohort_dm(adsl)
  f <- pp_cohort_frame(d, NULL)
  expect_identical(nrow(f), 0L)
  expect_true("USUBJID" %in% names(f))
  expect_identical(pp_cohort_order(f, "id"), integer())

  m <- pp_cohort_marks(d, NULL)
  expect_length(m$subjects, 0L)
  # the axis is never zero, or every x would divide by it
  expect_gt(m$days, 0)
})

test_that("arm colours come from the scale map when the board binds one", {
  d <- cohort_dm(test_adsl(), test_adae())
  roles <- pp_resolve_roles(d, list(arm = "ACTARM"))
  f <- pp_cohort_frame(d, roles)

  # no map: a stable palette over the sorted levels
  bare <- pp_cohort_arm_colors(f, NULL, d, roles$arm)
  expect_named(bare, c("Active 10mg", "Placebo"))
  expect_match(bare[["Placebo"]], "^#")

  map <- blockr.theme::new_scale_map(
    blockr.theme::scale_binding(
      "ACTARM", color = c("Placebo" = "#123456", "Active 10mg" = "#654321")
    )
  )
  mapped <- pp_cohort_arm_colors(f, map, d, roles$arm)
  expect_identical(unname(mapped[["Placebo"]]), "#123456")
  expect_identical(unname(mapped[["Active 10mg"]]), "#654321")
})

# --- The click guard --------------------------------------------------------
# The whole reason the cohort moved into this block: the row click stays
# inside it. No control channel, so nothing to gate off when a sender block
# leaves the screen. What is left to get wrong is which ids it accepts.

test_that("a row click only selects a patient the cohort holds", {
  ids <- c("S-1", "S-2", "S-3")
  expect_identical(pp_cohort_pick("S-2", ids), "S-2")
  # stale (the cohort narrowed under the click) or forged: dropped, never
  # written, so the block cannot wedge on a patient it cannot render
  expect_null(pp_cohort_pick("S-999", ids))
  expect_null(pp_cohort_pick("", ids))
  expect_null(pp_cohort_pick(NULL, ids))
  expect_null(pp_cohort_pick(NA, ids))
  expect_null(pp_cohort_pick(c("S-1", "S-2"), ids))
  # an empty cohort accepts nothing
  expect_null(pp_cohort_pick("S-1", character()))
})

test_that("the sort key is not block state", {
  # It changes what you see and nothing that serializes: a board restored
  # into another study must not come back sorted by a column that study does
  # not have.
  blk <- new_patient_profile_block()
  expect_false("cohort_sort" %in% names(formals(new_patient_profile_block)))
  expect_false("cohort_sort" %in% names(blockr.core::blockr_ser(blk)$payload))
})

test_that("the arm chip is a light tint, not a solid block", {
  # The house badge is a tinted background carrying the colour as text. A
  # solid chip with white text turns a 200-row list into a wall of paint.
  st <- pp_cohort_chip_style("#2563eb")
  expect_match(st, "background:rgba\\(37,99,235,0\\.12\\)")
  expect_match(st, "color:#")
  # the text colour is the same hue, darkened -- not black, not the raw hue
  expect_false(grepl("color:#2563eb", st))
  expect_false(grepl("color:#000000", st))
  # anything unparseable falls back to the neutral badge rather than erroring
  expect_match(pp_cohort_chip_style("not a colour"), "background:#f3f4f6")
})

# --- The pre-treatment floor ------------------------------------------------
# Study days go negative and ADaM occurrence data carries medical history
# among them (pharmaverseadam has AE onsets at day -13469). The band's axis
# used to start at day 0, so those events landed in the wrong place or
# vanished, and the list disagreed with the panel it opens.

hist_adae <- function() {
  data.frame(
    USUBJID = c("S-1", "S-1", "S-2", "S-3"),
    AEDECOD = c("History", "Screening rash", "Headache", "Old injury"),
    ASTDY = c(-4000, -12, 5, -900),
    AENDY = c(-3990, -2, 12, -880),
    AESEV = c("MILD", "MODERATE", "SEVERE", "MILD"),
    stringsAsFactors = FALSE
  )
}

test_that("the floor bounds how far back the axis reaches", {
  d <- cohort_dm(test_adsl(), hist_adae())
  m <- pp_cohort_marks(d, pp_resolve_roles(d, list(arm = "ACTARM")))

  # A BOUND, not a fixed start: the axis begins at the earliest event that
  # survives the floor (-12), rather than always spending 30 days of width on
  # a screening window the cohort may not use. What it must not be is -4000 --
  # one medical-history record squeezing every on-study event into a sliver
  # on all 254 rows.
  expect_identical(m$day0, -12)
  expect_identical(m$days, 180)
})

test_that("a straddling event enters from the edge, an older one drops", {
  d <- cohort_dm(test_adsl(), hist_adae())
  m <- pp_cohort_marks(d, pp_resolve_roles(d, list(arm = "ACTARM")))

  ev <- m$subjects[["S-1"]]$events
  # the -4000 history is gone, the -12 screening rash stays
  expect_identical(nrow(ev), 1L)
  expect_identical(ev$start[[1]], -12)
  # S-3's only event is entirely before the floor, so the patient keeps a row
  # with an empty band rather than losing one
  expect_identical(nrow(m$subjects[["S-3"]]$events), 0L)
  expect_true("S-3" %in% names(m$subjects))
})

test_that("an event straddling the floor is clamped, not dropped", {
  adae <- hist_adae()
  adae$AENDY <- c(-3990, 40, 12, -880)
  adae$ASTDY <- c(-4000, -900, 5, -900)   # runs from -900 to day 40
  d <- cohort_dm(test_adsl(), adae)
  m <- pp_cohort_marks(d, pp_resolve_roles(d, list(arm = "ACTARM")))
  ev <- m$subjects[["S-1"]]$events
  expect_identical(nrow(ev), 1L)
  expect_identical(ev$start[[1]], -30)    # enters from the left edge
  expect_identical(ev$end[[1]], 40)
})

test_that("the full history is available, and then the axis really reaches it", {
  d <- cohort_dm(test_adsl(), hist_adae())
  m <- pp_cohort_marks(d, pp_resolve_roles(d, list(arm = "ACTARM")),
                       prestudy_days = Inf)
  expect_identical(m$day0, -4000)
  expect_identical(nrow(m$subjects[["S-1"]]$events), 2L)
})

test_that("the band maps from day0, not from zero", {
  d <- cohort_dm(test_adsl(), hist_adae())
  m <- pp_cohort_marks(d, pp_resolve_roles(d, list(arm = "ACTARM")))
  col <- pp_cohort_sev_color(NULL)

  # S-1's only surviving event is at day -12, which is in the FIRST tenth of
  # a -30..180 axis. Anchored at zero it would have been dropped entirely.
  svg <- pp_cohort_band_svg(m$subjects[["S-1"]], m$days, col, day0 = m$day0)
  xs <- as.numeric(regmatches(svg, gregexpr('(?<=x=")[0-9.]+', svg,
                                            perl = TRUE))[[1]])
  painted <- xs[-1]                       # drop the track
  expect_gt(length(painted), 0)
  expect_lt(max(painted), 176 * 0.2)

  # a cohort with no pre-treatment events still starts at day 1
  d2 <- cohort_dm(test_adsl(), test_adae())
  expect_identical(pp_cohort_marks(d2, pp_resolve_roles(d2))$day0, 1)
})
