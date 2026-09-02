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

test_that("an ongoing event stops at the PATIENT's end, not the cohort's", {
  # Where the band and the panel disagreed. pp_gantt_open_end() ends an open
  # bar at the patient's own axis end, because the panel is drawn on one
  # patient's time range; the band shares one axis across the cohort, so
  # stretching to THAT end paints the event across every day the
  # longest-treated patient was on study.
  #
  # S-2 stops at day 30 on a cohort axis of 180. An ongoing event of theirs
  # must end at 30, or their strip reads as five months of illness.
  adae <- data.frame(
    USUBJID = c("S-2", "S-1"),
    AEDECOD = c("Rash", "Headache"),
    ASTDY = c(3, 5), AENDY = c(NA, NA),
    AESEV = c("MILD", "MILD"),
    stringsAsFactors = FALSE
  )
  d <- cohort_dm(test_adsl(), adae)
  m <- pp_cohort_marks(d, pp_resolve_roles(d, list(arm = "ACTARM")))

  expect_identical(m$days, 180)
  expect_identical(m$subjects[["S-2"]]$events$end[[1]], 30)
  # ...and the patient who really does run to the end still does
  expect_identical(m$subjects[["S-1"]]$events$end[[1]], 180)
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

test_that("the band draws one span per event, in adae order", {
  d <- cohort_dm(test_adsl(), test_adae())
  m <- pp_cohort_marks(d, pp_resolve_roles(d, list(arm = "ACTARM")))
  col <- pp_cohort_sev_color(NULL)

  svg <- pp_cohort_band_svg(m$subjects[["S-1"]], m$days, col)
  expect_match(svg, "^<svg")
  fills <- regmatches(svg, gregexpr('fill="#[0-9A-Fa-f]{6}', svg))[[1]]
  # S-1 has two events, so two spans -- not a bin count
  expect_length(fills, 2L)
  expect_match(svg, pp_sev_fallback_color("SEVERE"), fixed = TRUE)

  # An eventless patient still draws: the track alone reads as "nothing
  # happened", which is a finding, where a missing band reads as broken.
  empty <- pp_cohort_band_svg(m$subjects[["S-3"]], m$days, col)
  expect_match(empty, "^<svg")
  expect_identical(length(gregexpr("<rect", empty)[[1]]), 1L)
})

test_that("a severe event does not repaint the mild ones underneath it", {
  # The band used to paint each bin with the WORST severity active that day.
  # A patient whose events are mostly grade 1 and 2 then came out almost
  # entirely grade-3 amber, while the profile's AE lane -- which draws one
  # bar per event in row order -- showed the same patient as mostly teal.
  # Two different-looking patients from one set of records.
  adae <- data.frame(
    USUBJID = rep("S-1", 4),
    AEDECOD = c("Long severe", "Mild a", "Mild b", "Mild c"),
    ASTDY = c(2, 10, 40, 90), AENDY = c(170, 30, 70, 130),
    AESEV = c("SEVERE", "MILD", "MILD", "MILD"),
    stringsAsFactors = FALSE
  )
  d <- cohort_dm(test_adsl(), adae)
  m <- pp_cohort_marks(d, pp_resolve_roles(d, list(arm = "ACTARM")))
  svg <- pp_cohort_band_svg(m$subjects[["S-1"]], m$days,
                            pp_cohort_sev_color(NULL))
  fills <- regmatches(svg, gregexpr('fill="#[0-9A-Fa-f]{6}', svg))[[1]]

  # four events, four spans, and the three mild ones survive the severe one
  expect_length(fills, 4L)
  mild <- sum(grepl(pp_sev_fallback_color("MILD"), fills, fixed = TRUE))
  expect_identical(mild, 3L)
})

# The row builder htmltools would have written, kept as the reference the
# fast one is checked against. pp_cohort_rows_html() exists only because this
# costs 0.92s at 1251 patients where the sprintf costs 0.01s; the markup is
# supposed to be the same, and this is what says so.
reference_rows <- function(frame, ord, disp, marks, color, arm_col,
                           picked = NULL) {
  has <- function(col) col %in% names(frame)
  rows <- lapply(ord, function(i) {
    id <- frame$USUBJID[[i]]
    arm <- if (has("ARM")) as.character(frame$ARM[[i]]) else ""
    code <- if (has("ARMCD")) as.character(frame$ARMCD[[i]]) else ""
    demo <- paste(c(if (has("SEX")) as.character(frame$SEX[[i]]),
                    if (has("AGE")) as.character(frame$AGE[[i]])),
                  collapse = " ")
    band <- pp_cohort_band_attr(
      marks$subjects[[id]] %||% list(events = NULL, trt_end = NA),
      marks$days, color, day0 = marks$day0)
    tint <- unname(arm_col[[arm]] %||% "#9ca3af")
    badge <- if (nzchar(code)) {
      shiny::span(class = "pp-pt-code", title = arm,
                  style = pp_cohort_chip_style(tint), code)
    } else if (nzchar(arm)) {
      shiny::span(class = "pp-pt-swatch", title = arm,
                  style = paste0("background:", tint))
    }
    shiny::div(
      class = paste("pp-pt", if (identical(id, picked)) "is-selected"),
      `data-usubjid` = id,
      `data-search-text` = tolower(paste(id, demo, arm, code)),
      `data-band` = band$band, `data-eot` = band$eot,
      title = if (nzchar(arm)) paste0(id, " · ", arm) else id,
      shiny::div(class = "pp-pt-line",
        shiny::span(class = "pp-pt-id", disp$short[[i]]),
        if (nzchar(demo)) shiny::span(class = "pp-pt-demo", demo),
        shiny::span(class = "pp-pt-gap"), badge),
      shiny::tags$svg(class = "pp-pt-band", width = 176, height = 7,
        viewBox = "0 0 176 7", preserveAspectRatio = "none",
        `aria-hidden` = "true",
        shiny::tags$rect(x = 0, y = 0, width = 176, height = 7, rx = 2,
                         fill = "var(--pp-cohort-track, #f3f4f6)")))
  })
  as.character(htmltools::renderTags(shiny::tagList(rows))$html)
}

# Three differences are allowed, and only these three: htmltools indents
# between tags, it wrote `class="pp-pt "` with a trailing space on every
# unselected row (paste() with a NULL branch), and it closes the track with
# `</rect>` where the string writes `/>`. None of the three reaches a parser.
normalise_rows <- function(x) {
  x <- gsub(">[ \t\r\n]+<", "><", x)
  x <- gsub('class="pp-pt "', 'class="pp-pt"', x, fixed = TRUE)
  x <- gsub('"></rect>', '"/>', x, fixed = TRUE)
  gsub("^[ \t\r\n]+|[ \t\r\n]+$", "", x)
}

rows_agree <- function(adsl, adae, picked = NULL) {
  d <- cohort_dm(adsl, adae)
  roles <- pp_resolve_roles(d, list(arm = "ACTARM"))
  frame <- pp_cohort_frame(d, roles)
  marks <- pp_cohort_marks(d, roles)
  color <- pp_cohort_sev_color(NULL)
  arm_col <- pp_cohort_arm_colors(frame, NULL, d, roles$arm)
  ord <- pp_cohort_order(frame, "id")
  disp <- pp_cohort_id_display(frame$USUBJID)
  expect_identical(
    normalise_rows(as.character(
      pp_cohort_rows_html(frame, ord, disp, marks, color, arm_col, picked))),
    normalise_rows(
      reference_rows(frame, ord, disp, marks, color, arm_col, picked))
  )
}

test_that("the fast row builder writes the markup htmltools would have", {
  rows_agree(test_adsl(), test_adae())
  rows_agree(test_adsl(), test_adae(), picked = "S-2")

  # No arm code: the chip becomes a swatch
  no_code <- test_adsl()
  no_code$ACTARMCD <- NULL
  rows_agree(no_code, test_adae())

  # The demography span disappears rather than rendering empty
  no_sex <- test_adsl(); no_sex$SEX <- NULL
  rows_agree(no_sex, test_adae())
  no_age <- test_adsl(); no_age$AGE <- NULL
  rows_agree(no_age, test_adae())
  bare <- test_adsl(); bare$SEX <- NULL; bare$AGE <- NULL
  rows_agree(bare, test_adae())

  # No adae: every band is empty, every row still renders
  rows_agree(test_adsl(), NULL)
})

test_that("study text is escaped, in attributes and in content alike", {
  # htmltools did this; writing the HTML by hand means doing it by hand, and
  # an arm label is free text a study controls.
  nasty <- test_adsl()
  nasty$USUBJID <- c('S"1', "S<2>", "S&3")
  nasty$ACTARM <- c('Placebo "high"', "A<b>10</b>", "R&D")
  nasty$ACTARMCD <- c("", "", "")
  adae <- test_adae()
  adae$USUBJID <- nasty$USUBJID[[1]]
  rows_agree(nasty, adae)

  d <- cohort_dm(nasty, adae)
  roles <- pp_resolve_roles(d, list(arm = "ACTARM"))
  frame <- pp_cohort_frame(d, roles)
  html <- as.character(pp_cohort_rows_html(
    frame, pp_cohort_order(frame, "id"), pp_cohort_id_display(frame$USUBJID),
    pp_cohort_marks(d, roles), pp_cohort_sev_color(NULL),
    pp_cohort_arm_colors(frame, NULL, d, roles$arm)))

  # Nothing a study wrote closes an attribute or opens a tag
  expect_match(html, 'data-usubjid="S&quot;1"', fixed = TRUE)
  expect_match(html, 'title="S&lt;2&gt; · A&lt;b&gt;10&lt;/b&gt;"',
               fixed = TRUE)
  expect_false(grepl("<b>10</b>", html, fixed = TRUE))
  expect_match(html, "S&amp;3", fixed = TRUE)
})

test_that("the row ships the band's geometry, and it matches the drawn one", {
  # The client draws the band from data-band when the row scrolls into view,
  # so the attribute and the SVG have to be the same picture.
  d <- cohort_dm(test_adsl(), test_adae())
  m <- pp_cohort_marks(d, pp_resolve_roles(d, list(arm = "ACTARM")))
  col <- pp_cohort_sev_color(NULL)

  attr <- pp_cohort_band_attr(m$subjects[["S-1"]], m$days, col)
  svg <- pp_cohort_band_svg(m$subjects[["S-1"]], m$days, col)

  # x,w,fill per span, and the first <rect> of the svg is the track
  from_svg <- regmatches(
    svg, gregexpr('<rect x="[^"]*" y="0" width="[^"]*"[^>]*fill="#[^"]*"', svg)
  )[[1]]
  triplets <- strsplit(attr$band, " ", fixed = TRUE)[[1]]
  expect_length(triplets, length(from_svg))
  for (i in seq_along(triplets)) {
    f <- strsplit(triplets[[i]], ",", fixed = TRUE)[[1]]
    expect_match(from_svg[[i]], paste0('x="', f[[1]], '"'), fixed = TRUE)
    expect_match(from_svg[[i]], paste0('width="', f[[2]], '"'), fixed = TRUE)
    expect_match(from_svg[[i]], paste0('fill="', f[[3]]), fixed = TRUE)
  }

  # S-2 stopped early, S-1 did not: the marker is its own attribute
  expect_null(attr$eot)
  expect_false(is.null(pp_cohort_band_attr(m$subjects[["S-2"]], m$days,
                                           col)$eot))

  # A patient with no events ships an empty band, not a missing one: the row
  # still renders the track.
  expect_identical(
    pp_cohort_band_attr(m$subjects[["S-3"]], m$days, col)$band, ""
  )
})

test_that("consecutive spans of one colour merge, and only those", {
  # Merging is what takes a 25-event patient from 25 rects to a handful. It
  # must never merge ACROSS a different colour: the band paints later over
  # earlier, so that would bury the one in the middle.
  same <- data.frame(
    USUBJID = rep("S-1", 3), AEDECOD = c("a", "b", "c"),
    ASTDY = c(2, 20, 40), AENDY = c(30, 50, 70),
    AESEV = rep("MILD", 3), stringsAsFactors = FALSE
  )
  d <- cohort_dm(test_adsl(), same)
  m <- pp_cohort_marks(d, pp_resolve_roles(d, list(arm = "ACTARM")))
  g <- pp_cohort_band_geom(m$subjects[["S-1"]], m$days,
                           pp_cohort_sev_color(NULL))
  # three overlapping mild events are one span, day 2 to day 70
  expect_length(g$x, 1L)

  # the same three with a severe one in the middle stay four spans
  split <- same
  split$AESEV <- c("MILD", "SEVERE", "MILD")
  d2 <- cohort_dm(test_adsl(), split)
  m2 <- pp_cohort_marks(d2, pp_resolve_roles(d2, list(arm = "ACTARM")))
  g2 <- pp_cohort_band_geom(m2$subjects[["S-1"]], m2$days,
                            pp_cohort_sev_color(NULL))
  expect_length(g2$x, 3L)

  # a gap between two same-colour events is a gap, not a merge
  apart <- same
  apart$ASTDY <- c(2, 100, 140)
  apart$AENDY <- c(10, 110, 150)
  d3 <- cohort_dm(test_adsl(), apart)
  m3 <- pp_cohort_marks(d3, pp_resolve_roles(d3, list(arm = "ACTARM")))
  g3 <- pp_cohort_band_geom(m3$subjects[["S-1"]], m3$days,
                            pp_cohort_sev_color(NULL))
  expect_length(g3$x, 3L)
})

test_that("merging a span that starts LEFT of the one before keeps its left", {
  # adae is in record order, not start order. Extending only the right edge
  # dropped the part sticking out on the left, and 10 of safetyData's 254
  # patients painted differently because of it.
  back <- data.frame(
    USUBJID = rep("S-1", 2), AEDECOD = c("late", "early"),
    ASTDY = c(100, 20), AENDY = c(150, 120),
    AESEV = rep("MILD", 2), stringsAsFactors = FALSE
  )
  d <- cohort_dm(test_adsl(), back)
  m <- pp_cohort_marks(d, pp_resolve_roles(d, list(arm = "ACTARM")))
  g <- pp_cohort_band_geom(m$subjects[["S-1"]], m$days,
                           pp_cohort_sev_color(NULL), day0 = m$day0)

  expect_length(g$x, 1L)
  # the merged span covers both events end to end: it starts where the
  # EARLIER-starting one does and runs to the far end of the other
  ev <- m$subjects[["S-1"]]$events
  span <- m$days - m$day0
  x_of <- function(d) round(((d - m$day0) / span) * 176, 2)
  expect_equal(g$x[[1]], x_of(min(ev$start)))
  expect_equal(g$x[[1]] + g$w[[1]], x_of(max(ev$end)), tolerance = 0.02)
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

# --- Long ids ---------------------------------------------------------------
# A prod USUBJID is around twenty characters against the demo's eleven, and
# the row overflowed a 232px sidebar into a horizontal scrollbar. Most of
# those characters are the study id, which is the same on every row of the
# board, so the fix is to lift the shared prefix out rather than to cut
# characters off an id.

test_that("the shared prefix is lifted, and only the shared part", {
  ids <- c("CA2440001-01-701-1115", "CA2440001-01-703-1129",
           "CA2440001-01-702-1122")
  d <- pp_cohort_id_display(ids)
  expect_identical(d$prefix, "CA2440001-01-")
  expect_identical(d$short, c("701-1115", "703-1129", "702-1122"))
  # nothing that distinguishes two patients is dropped
  expect_identical(length(unique(d$short)), length(unique(ids)))
})

test_that("the displayed id does not change as the cohort narrows", {
  # Filtered to one site the common prefix reaches into the subject number.
  # Without the two-segment floor a patient would read as 703-1129 in the
  # whole cohort and 1129 at their own site, and the id under the cursor
  # would change as you filter.
  one_site <- c("CA2440001-01-703-1129", "CA2440001-01-703-1130",
                "CA2440001-01-703-1141")
  d <- pp_cohort_id_display(one_site)
  expect_identical(d$prefix, "CA2440001-01-")
  expect_identical(d$short[[1]], "703-1129")
})

test_that("the cut lands on a separator, never mid-segment", {
  d <- pp_cohort_id_display(c("STUDY_01_701_1115", "STUDY_01_702_1122"))
  expect_identical(d$prefix, "STUDY_01_")
  expect_identical(d$short, c("701_1115", "702_1122"))
})

test_that("nothing is lifted when there is nothing to lift", {
  # short demo ids: the shared part is under min_prefix, so the row is not
  # meaningfully shorter and the header would gain a line for nothing
  demo <- c("01-701-1015", "01-702-1023", "01-703-1028")
  expect_identical(pp_cohort_id_display(demo)$prefix, "")
  expect_identical(pp_cohort_id_display(demo)$short, demo)

  # no shared prefix at all
  expect_identical(pp_cohort_id_display(c("AAA-1", "BBB-2"))$prefix, "")
  # ids with no separator are left alone rather than cut at a guessed point
  expect_identical(pp_cohort_id_display(c("ABC1115", "ABC1122"))$prefix, "")
  # one patient has nothing to compare against
  expect_identical(pp_cohort_id_display("CA2440001-01-703-1129")$prefix, "")
  expect_identical(pp_cohort_id_display(character())$short, character())
})

test_that("the download and the click keep the full id", {
  # The row shows the short form; everything that leaves the block must not.
  ids <- c("CA2440001-01-701-1115", "CA2440001-01-703-1129")
  short <- pp_cohort_id_display(ids)$short
  # the click guard is checked against the real cohort ids, so a short form
  # would be rejected -- which is what proves the row must send the full one
  expect_null(pp_cohort_pick(short[[1]], ids))
  expect_identical(pp_cohort_pick(ids[[1]], ids), ids[[1]])
})

# --- Studies that ship dates and no analysis days ---------------------------
# The band's axis is in study days, so it read ASTDY/AENDY and returned
# NOTHING when they were absent: every strip an empty track, on a study whose
# profile panels drew the same events perfectly. The panels do not have the
# problem because the AE gantt picks its source by MODE and in the default
# date mode never looks at a day column.

date_adae <- function() {
  data.frame(
    USUBJID = c("S-1", "S-1", "S-2"),
    AEDECOD = c("Headache", "Nausea", "Rash"),
    ASTDT = as.Date(c("2024-01-05", "2024-02-10", "2024-01-03")),
    AENDT = as.Date(c("2024-01-09", "2024-02-14", "2024-01-12")),
    AESEV = c("MILD", "SEVERE", "MODERATE"),
    stringsAsFactors = FALSE
  )
}

test_that("dates become study days when the study ships no day column", {
  d <- cohort_dm(test_adsl(), date_adae())
  m <- pp_cohort_marks(d, pp_resolve_roles(d, list(arm = "ACTARM")))
  ev <- m$subjects[["S-1"]]$events
  expect_identical(nrow(ev), 2L)
  # TRTSDT is 2024-01-01, so 2024-01-05 is day 5 (day 1 is treatment start)
  expect_identical(ev$start[[1]], 5)
  expect_identical(ev$end[[1]], 9)
})

test_that("a native day beats a date, and there is no day zero", {
  adae <- date_adae()
  adae$ASTDY <- c(99, 99, 99)
  adae$AENDY <- c(100, 100, 100)
  d <- cohort_dm(test_adsl(), adae)
  m <- pp_cohort_marks(d, pp_resolve_roles(d, list(arm = "ACTARM")))
  # the study's own derived day is authoritative; re-deriving from dates is
  # a lossy round trip past the same anchor
  expect_identical(m$subjects[["S-1"]]$events$start[[1]], 99)

  pre <- date_adae()
  pre$ASTDT[1] <- as.Date("2023-12-30")
  d2 <- cohort_dm(test_adsl(), pre)
  m2 <- pp_cohort_marks(d2, pp_resolve_roles(d2, list(arm = "ACTARM")))
  # two days before treatment start is -2, never 0
  expect_identical(m2$subjects[["S-1"]]$events$start[[1]], -2)
})

test_that("a part-filled day column falls back per ROW, not per column", {
  # Preferring a day column wholesale silently dropped every record it had no
  # value for, while the panel drew all of them from the dates. On grade data
  # that showed up as a strip missing exactly the severe events.
  adae <- date_adae()
  adae$ASTDY <- c(5, NA, NA)
  adae$AENDY <- c(9, NA, NA)
  d <- cohort_dm(test_adsl(), adae)
  m <- pp_cohort_marks(d, pp_resolve_roles(d, list(arm = "ACTARM")))
  ev <- m$subjects[["S-1"]]$events
  expect_identical(nrow(ev), 2L)
  expect_identical(ev$sev, c("MILD", "SEVERE"))
  expect_identical(ev$start, c(5, 41))
})

test_that("a study with neither days nor dates has no AE timeline", {
  adae <- date_adae()
  adae$ASTDT <- NULL
  adae$AENDT <- NULL
  d <- cohort_dm(test_adsl(), adae)
  m <- pp_cohort_marks(d, pp_resolve_roles(d, list(arm = "ACTARM")))
  expect_identical(nrow(m$subjects[["S-1"]]$events), 0L)
  # ...and every patient keeps their row
  expect_length(m$subjects, 3L)
})
