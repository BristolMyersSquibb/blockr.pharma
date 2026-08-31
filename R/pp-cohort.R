# The cohort, as data.
#
# The patient profile carries its whole cohort (pp_pick_subject() attaches it
# as `attr(, "pp_cohort")`), but until now nothing read it as a LIST: the
# picker took ids and labels, and the exhibit path re-rendered every patient
# in full. The sidebar cohort list needs a third shape -- one ROW per patient,
# a handful of scalars plus the marks a 176px band can draw.
#
# Two functions, deliberately separate:
#
#   pp_cohort_frame()  -- the seam. Cohort dm in, one row per patient out, as
#                         a plain data frame. This is what the download
#                         writes, and what a fancier column set later
#                         replaces. It knows nothing about drawing.
#   pp_cohort_marks()  -- the per-patient event geometry the band needs
#                         (AE intervals by grade, treatment end), which is a
#                         list column's worth of data and has no place in a
#                         downloadable table.
#
# Keeping them apart is what lets the download get more advanced (picked
# panels, per-measure reductions -- see the spec) without touching the
# sidebar, and the band get richer without widening the export.

#' One row per patient in the cohort
#'
#' The cohort as a flat table: the ADSL facts a clinician scans and exports.
#' This is the whole contract behind the sidebar list and the cohort
#' download, so a study wanting different columns replaces this one function.
#'
#' Every column is optional except `USUBJID`. A study missing `AGE` gets a
#' frame without an age column rather than a column of `NA`, so the download
#' never implies data that is not there.
#'
#' @param dm_obj A normalized `dm` (see [pp_normalize_dm()]).
#' @param roles Resolved roles (see `pp_resolve_roles()`), for the arm and
#'   arm-code columns.
#' @return A data frame, one row per subject, in cohort order. Zero rows
#'   (with the `USUBJID` column) when the dm carries no subjects.
#' @noRd
pp_cohort_frame <- function(dm_obj, roles = NULL) {

  ids <- pp_subject_ids(dm_obj)
  if (!length(ids)) {
    return(data.frame(USUBJID = character(), stringsAsFactors = FALSE))
  }

  tbls <- dm::dm_get_tables(dm_obj)
  adsl <- as.data.frame(tbls[[pp_subject_tbl_name(names(tbls))]])
  # One ADSL row per subject by construction; do not trust it. Taking the
  # first row per id is what keeps a duplicated ADSL from misaligning every
  # column against the ids (same guard as pp_subject_choices()).
  at <- match(ids, as.character(adsl$USUBJID))

  out <- data.frame(USUBJID = ids, stringsAsFactors = FALSE)

  take <- function(col, name = col) {
    if (is.null(col) || !col %in% colnames(adsl)) return(invisible(NULL))
    out[[name]] <<- adsl[[col]][at]
    invisible(NULL)
  }

  roles <- roles %||% list()
  take(roles$arm, "ARM")
  take(roles$arm_code, "ARMCD")
  take("SEX")
  take("AGE")
  take("TRTSDT")
  take("TRTEDT")
  # Days on treatment: derived when both dates are there, because it is the
  # number a reader wants and the two dates are the number they would have
  # to subtract themselves.
  if (all(c("TRTSDT", "TRTEDT") %in% names(out))) {
    out$TRTDURD <- as.numeric(
      as.Date(out$TRTEDT) - as.Date(out$TRTSDT)
    ) + 1
  }
  take("EOSSTT")
  take("DCSREAS")
  take("DTHFL")

  ae <- pp_cohort_ae_summary(tbls, ids, roles$severity)
  if (!is.null(ae)) {
    out$AE_N <- ae$n
    out$AE_WORST <- ae$worst
  }

  rownames(out) <- NULL
  out
}

#' Per-patient AE count and worst severity
#'
#' `NULL` when there is no adae, which is the difference between "this study
#' has no AE data" and "this patient had none" -- the first drops the columns,
#' the second is a zero.
#' @noRd
pp_cohort_ae_summary <- function(tbls, ids, sev_col = NULL) {

  if (!"adae" %in% names(tbls)) return(NULL)

  adae <- as.data.frame(tbls[["adae"]])
  if (!"USUBJID" %in% colnames(adae)) return(NULL)

  sub <- as.character(adae$USUBJID)
  n <- vapply(ids, function(id) sum(sub == id, na.rm = TRUE), integer(1L),
              USE.NAMES = FALSE)

  worst <- if (!is.null(sev_col) && sev_col %in% colnames(adae)) {
    sev <- as.character(adae[[sev_col]])
    vapply(ids, function(id) {
      v <- sev[sub == id]
      v <- v[!is.na(v) & nzchar(v)]
      if (!length(v)) return(NA_character_)
      # Grades order numerically, words by the severity vocabulary. Anything
      # unranked sorts last rather than winning by accident.
      ord <- pp_sev_rank(v)
      v[[which.max(ord)]]
    }, character(1L), USE.NAMES = FALSE)
  } else {
    rep(NA_character_, length(ids))
  }

  list(n = n, worst = worst)
}

#' Rank severity values so "worst" means worst
#'
#' Numeric grades rank by their value, the word vocabulary by its own order.
#' Unknown values rank 0, so they never beat a real severity.
#' @noRd
pp_sev_rank <- function(x) {
  s <- toupper(trimws(as.character(x)))
  words <- c(MILD = 1, MODERATE = 2, SEVERE = 3)
  vapply(s, function(v) {
    if (grepl("^[0-9]+$", v)) return(as.numeric(v))
    if (v %in% names(words)) return(unname(words[[v]]))
    0
  }, numeric(1L), USE.NAMES = FALSE)
}

#' Event geometry for the cohort band
#'
#' What the 176px band draws, per patient: adverse events as intervals on the
#' study-day axis with their severity, and the treatment end day for the
#' milestone. Study days, never dates -- the band's whole point is that every
#' patient sits on ONE axis, and a date axis would put a 2019 patient and a
#' 2021 patient in different halves of the same strip.
#'
#' Patients whose events carry no usable day are returned with no events
#' rather than dropped: an empty band is a legitimate reading, a missing row
#' is not.
#'
#' @section The pre-treatment floor:
#' Study days go negative, and ADaM occurrence data carries medical history
#' among them: pharmaverseadam has AE onsets at day -13469. An axis honest
#' enough to include that would squeeze every on-study event into a sliver at
#' the right edge of all 254 rows. The profile solved this once already --
#' [pp_clip_prestudy()] floors the timeline 30 days before treatment start
#' unless the user opts into the full history -- and the band follows the same
#' rule, in study days: the axis floors at `-prestudy_days`, an event
#' straddling the floor enters from the left edge, and only an event entirely
#' before it drops out.
#'
#' Before this the axis simply started at day 0, so a pre-treatment event was
#' drawn at the wrong place or not at all, and the list disagreed with the
#' panel it opens.
#'
#' @param dm_obj A normalized `dm`.
#' @param roles Resolved roles, for the severity column.
#' @param prestudy_days How far before treatment start the axis may reach.
#'   `Inf` for the full history (the block passes this when the user turns
#'   the profile's Pre-treatment toggle on, so the two agree).
#' @return `list(day0, days, subjects)` -- the axis bounds in study days and a
#'   named list, one entry per USUBJID, each
#'   `list(events = data.frame(start, end, sev), trt_end = <num or NA>)`.
#' @noRd
pp_cohort_marks <- function(dm_obj, roles = NULL, prestudy_days = 30) {

  ids <- pp_subject_ids(dm_obj)
  empty <- list(day0 = 0, days = 1,
                subjects = stats::setNames(list(), character()))
  if (!length(ids)) return(empty)

  tbls <- dm::dm_get_tables(dm_obj)
  roles <- roles %||% list()

  adsl <- as.data.frame(tbls[[pp_subject_tbl_name(names(tbls))]])
  at <- match(ids, as.character(adsl$USUBJID))
  trt_end <- if (all(c("TRTSDT", "TRTEDT") %in% colnames(adsl))) {
    as.numeric(as.Date(adsl$TRTEDT[at]) - as.Date(adsl$TRTSDT[at])) + 1
  } else {
    rep(NA_real_, length(ids))
  }

  ev <- pp_cohort_ae_events(tbls, roles$severity)

  # The floor, in study days. Day 1 is treatment start and there is no day 0,
  # so 30 days before it is day -30.
  floor_day <- if (is.finite(prestudy_days)) -abs(prestudy_days) else -Inf

  # Only events entirely before the floor drop out; one straddling it enters
  # from the left edge. Same rule pp_clip_prestudy() states for the panel.
  keep <- is.infinite(floor_day) | ev$end >= floor_day | ev$open
  ev <- lapply(ev, function(x) x[keep])
  ev$start <- pmax(ev$start, floor_day)

  # ONE axis for every row, so the rows compare. The bounds are the widest
  # anything reaches, floored below. Open-ended events are excluded from the
  # maximum and then stretched to it -- including them would be circular,
  # since where they end IS the axis.
  day0 <- suppressWarnings(min(c(ev$start, 1), na.rm = TRUE))
  if (!is.finite(day0)) day0 <- 0
  days <- suppressWarnings(max(c(trt_end, ev$end, ev$start, 1), na.rm = TRUE))
  if (!is.finite(days) || days <= day0) days <- day0 + 1

  # An event with no end date is ONGOING, and runs to the end of THAT
  # PATIENT'S timeline -- not to the end of the cohort axis.
  #
  # This is where the band and the panel disagreed. pp_gantt_open_end() ends
  # an open bar at the patient's own axis end, because the panel is drawn on
  # one patient's time range. The band shares one axis across the cohort, so
  # stretching an open event to THAT end paints it across every day the
  # longest-treated patient in the study was on it. Patient 701-1047 has two
  # ongoing MILD events from day 23 and two MODERATE ones on day 1: the panel
  # showed the mild pair ending at day 55, the band painted them to day 212
  # and 47 of 60 bins came out mild, burying the moderate. Same data, two
  # different-looking patients.
  own_end <- pp_cohort_subject_end(tbls, ids, trt_end, ev)
  ev$end[ev$open] <- own_end[match(ev$subject[ev$open], ids)]
  # A patient whose own end is unknowable keeps the axis end: an open event
  # with nothing to bound it really does run off the far side.
  ev$end[ev$open & is.na(ev$end)] <- days

  subjects <- lapply(seq_along(ids), function(i) {
    keep <- ev$subject == ids[[i]]
    list(
      events = data.frame(
        start = ev$start[keep], end = ev$end[keep], sev = ev$sev[keep],
        stringsAsFactors = FALSE
      ),
      trt_end = trt_end[[i]]
    )
  })
  names(subjects) <- ids

  list(day0 = day0, days = days, subjects = subjects)
}

#' The last study day each patient has any data for
#'
#' The band's per-patient bound for an ongoing event, and the cheap stand-in
#' for what the panel computes exactly. `pp_compute_time_range()` scans every
#' table's date and day columns for ONE patient at a time, which is the right
#' answer and the wrong cost at 300 rows; this takes the same columns but
#' groups by subject in one pass per table.
#'
#' It reads days only, never dates. The band's axis is in study days, and a
#' date would have to be converted back through the per-patient reference to
#' be comparable -- the lossy round trip pp_xval_pref_day() exists to avoid.
#'
#' @param tbls The dm's tables.
#' @param ids Cohort USUBJIDs.
#' @param trt_end Per-subject treatment end day (same order as `ids`).
#' @param ev Event list from [pp_cohort_ae_events()], for the days it already
#'   resolved.
#' @return Numeric, one per id; `NA` where the patient has no usable day.
#' @noRd
pp_cohort_subject_end <- function(tbls, ids, trt_end, ev = NULL) {

  day_cols <- c("ASTDY", "AENDY", "ADY")
  acc <- trt_end

  bump <- function(subject, day) {
    ok <- !is.na(day) & !is.na(subject)
    if (!any(ok)) return(invisible(NULL))
    at <- match(subject[ok], ids)
    d <- day[ok]
    keep <- !is.na(at)
    if (!any(keep)) return(invisible(NULL))
    # max per subject, without splitting the frame
    o <- order(at[keep], d[keep])
    a <- at[keep][o]
    v <- d[keep][o]
    last <- !duplicated(a, fromLast = TRUE)
    acc[a[last]] <<- pmax(acc[a[last]], v[last], na.rm = TRUE)
    invisible(NULL)
  }

  for (nm in names(tbls)) {
    tbl <- as.data.frame(tbls[[nm]])
    if (!"USUBJID" %in% colnames(tbl)) next
    sub <- as.character(tbl$USUBJID)
    for (col in intersect(day_cols, colnames(tbl))) {
      bump(sub, suppressWarnings(as.numeric(tbl[[col]])))
    }
  }

  # Events already resolved (an open one contributes its start, which is the
  # least it can be).
  if (!is.null(ev) && length(ev$subject)) {
    bump(ev$subject, ev$start)
    bump(ev$subject, ev$end)
  }

  acc
}

#' Adverse events as day intervals
#'
#' A missing end is reported as `open`, not resolved here: it means ongoing,
#' and an ongoing event runs to the end of the axis, which this function does
#' not know. [pp_cohort_marks()] closes them once the axis is fixed.
#' @noRd
pp_cohort_ae_events <- function(tbls, sev_col = NULL) {

  none <- list(subject = character(), start = numeric(), end = numeric(),
               sev = character(), open = logical())
  if (!"adae" %in% names(tbls)) return(none)

  adae <- as.data.frame(tbls[["adae"]])
  if (!all(c("USUBJID", "ASTDY") %in% colnames(adae))) return(none)

  start <- suppressWarnings(as.numeric(adae$ASTDY))
  end <- if ("AENDY" %in% colnames(adae)) {
    suppressWarnings(as.numeric(adae$AENDY))
  } else {
    rep(NA_real_, nrow(adae))
  }
  # An end before the start is data we cannot draw; treat it as no end at
  # all rather than as a backwards bar.
  open <- is.na(end) | end < start
  end[open] <- start[open]

  sev <- if (!is.null(sev_col) && sev_col %in% colnames(adae)) {
    as.character(adae[[sev_col]])
  } else {
    rep(NA_character_, nrow(adae))
  }

  keep <- !is.na(start)
  list(subject = as.character(adae$USUBJID)[keep], start = start[keep],
       end = end[keep], sev = sev[keep], open = open[keep])
}

# ---------------------------------------------------------------------------
# Drawing. The band is deliberately NOT a scaled-down gantt: at 176px for a
# whole study, one event is about a pixel, and drawing them as separate marks
# smears a patient with a dozen events into an unreadable picket fence. So the
# band bins the axis and paints each bin with the WORST severity active that
# day. Overlaps merge instead of fighting, and the colour always answers "how
# bad was it here". This is what the profile's own AE lane already looks like
# once the events overlap; the band just makes it explicit.
# ---------------------------------------------------------------------------

#' Severity colour resolver for the cohort band
#'
#' Same colours as everything else that draws severity: the board scale map
#' when it binds the severity column, the package constants otherwise.
#' @param scale_colors Named colour vector from [pp_sev_scale_colors()], or
#'   `NULL`.
#' @return A function of one severity value returning a hex colour.
#' @noRd
pp_cohort_sev_color <- function(scale_colors = NULL) {
  function(sev) {
    s <- as.character(sev)
    if (is.na(s) || !nzchar(s)) return("#9ca3af")
    if (!is.null(scale_colors) && s %in% names(scale_colors)) {
      return(unname(scale_colors[[s]]))
    }
    pp_sev_fallback_color(s)
  }
}

#' The AE band for one patient
#'
#' @param sub One entry of [pp_cohort_marks()]`$subjects`.
#' @param days Axis maximum (shared across every row).
#' @param color A resolver from [pp_cohort_sev_color()].
#' @param width,height Band geometry in px.
#' @param bins How many segments the axis is painted in. 60 over 176px is
#'   about 3px a segment, which is the width below which a fill stops reading
#'   as a colour.
#' @param day0 Axis minimum. Negative when the cohort has pre-treatment
#'   events (see [pp_cohort_marks()]); the band is NOT anchored at zero, and
#'   assuming it was is what put those events in the wrong place.
#' @return An HTML string (an `<svg>`).
#' @noRd
pp_cohort_band_svg <- function(sub, days, color, width = 176, height = 7,
                               bins = 60, day0 = 0) {

  esc <- function(x) gsub("\"", "&quot;", x, fixed = TRUE)
  h <- height
  parts <- c(sprintf(
    paste0('<rect x="0" y="0" width="%s" height="%s" rx="2" ',
           'fill="var(--pp-cohort-track, #f3f4f6)"/>'),
    width, h
  ))

  ev <- sub$events
  span <- days - day0
  if (!is.finite(span) || span <= 0) span <- 1
  if (is.data.frame(ev) && nrow(ev)) {
    step <- span / bins
    seg_w <- width / bins
    for (i in seq_len(bins)) {
      d0 <- day0 + (i - 1) * step
      d1 <- day0 + i * step
      hit <- ev$start <= d1 & ev$end >= d0
      if (!any(hit)) next
      sev <- ev$sev[hit]
      rank <- pp_sev_rank(sev)
      # An event with no severity still darkens the band: it happened, and
      # showing nothing would read as an uneventful stretch.
      worst <- if (all(rank == 0)) sev[[1L]] else sev[[which.max(rank)]]
      parts <- c(parts, sprintf(
        '<rect x="%s" y="0" width="%s" height="%s" fill="%s" opacity="0.92"/>',
        round((i - 1) * seg_w, 2), round(seg_w + 0.5, 2), h,
        esc(color(worst))
      ))
    }
  }

  # End of treatment: the one treatment fact that carries information once
  # the study is complete. A patient still on treatment has no marker.
  te <- sub$trt_end
  if (is.finite(te) && te > day0 && te < days) {
    cx <- round(((te - day0) / span) * width, 2)
    cy <- h / 2
    r <- min(3, h / 2 + 1)
    parts <- c(parts, sprintf(
      '<path d="M%s %sL%s %sL%s %sL%s %sZ" fill="var(--pp-cohort-eot, #6b7280)"/>',
      cx, cy - r, cx + r, cy, cx, cy + r, cx - r, cy
    ))
  }

  paste0(
    '<svg class="pp-pt-band" width="', width, '" height="', h,
    '" viewBox="0 0 ', width, ' ', h,
    '" preserveAspectRatio="none" aria-hidden="true">',
    paste(parts, collapse = ""), '</svg>'
  )
}

#' Order the cohort rows
#'
#' The list is only worth scanning if its top is; sorting is what puts the
#' interesting patients there. `id` is the default because it is the order a
#' clinician can predict.
#'
#' @param frame A [pp_cohort_frame()] result.
#' @param by One of `"id"`, `"worst"`, `"ae"`, `"duration"`, `"arm"`.
#' @return An integer index vector.
#' @noRd
pp_cohort_order <- function(frame, by = "id") {
  n <- nrow(frame)
  if (!n) return(integer())
  has <- function(col) col %in% names(frame)
  # Descending for every signal ordering: the point of sorting by AE burden
  # is to see the burdened patients, not the untouched ones.
  ord <- switch(
    by,
    worst = if (has("AE_WORST")) order(-pp_sev_rank(frame$AE_WORST),
                                       frame$USUBJID) else NULL,
    ae = if (has("AE_N")) order(-frame$AE_N, frame$USUBJID) else NULL,
    duration = if (has("TRTDURD")) order(-frame$TRTDURD, frame$USUBJID,
                                         na.last = TRUE) else NULL,
    arm = if (has("ARM")) order(as.character(frame$ARM), frame$USUBJID)
          else NULL,
    NULL
  )
  ord %||% order(frame$USUBJID)
}

#' The sort keys the list offers, given what the data supports
#'
#' THREE, at most, and that is a consequence of the control: the sort is a
#' click-through pill, so every extra rung is another click to walk past on
#' the way to the one you want. Id because it is the order a reader can
#' predict, then the two that answer "who should I look at" -- how bad it got
#' and how much of it there was.
#'
#' Days on treatment and arm were dropped rather than forgotten. The band
#' already draws the end-of-treatment diamond, so early stops are findable by
#' eye; and arm is a grouping, which is a different control from a sort.
#'
#' A key whose column is missing is not offered, rather than offered and
#' silently falling back to id.
#' @noRd
pp_cohort_sort_choices <- function(frame) {
  out <- c(id = "Patient id")
  if ("AE_WORST" %in% names(frame)) out <- c(out, worst = "Worst severity")
  if ("AE_N" %in% names(frame)) out <- c(out, ae = "Event count")
  out
}

#' Chip colours for an arm
#'
#' The house badge is LIGHT: a tinted background with the colour carrying the
#' text, not a saturated block with white text on it. A list of 200 rows each
#' carrying a solid colour chip reads as a wall of paint, and the chip is
#' meant to be the quietest thing in the row after the band.
#'
#' The tint is the arm colour at 12% over the sidebar, and the text is the
#' same colour darkened enough to hold contrast on it -- so the two are
#' visibly the same colour, and the colour still matches the charts.
#'
#' @param hex A colour.
#' @return A CSS declaration string for the chip's `style` attribute.
#' @noRd
pp_cohort_chip_style <- function(hex) {
  rgb <- tryCatch(grDevices::col2rgb(hex)[, 1L], error = function(e) NULL)
  if (is.null(rgb)) {
    return("background:#f3f4f6;color:#374151")
  }
  dark <- grDevices::rgb(
    t(round(rgb * 0.72)), maxColorValue = 255
  )
  paste0(
    "background:rgba(", rgb[["red"]], ",", rgb[["green"]], ",",
    rgb[["blue"]], ",0.12);color:", dark
  )
}

#' Colours for the arm levels in the cohort list
#'
#' The board scale map first, so an arm is the same colour in the sidebar as
#' in every chart on the board (CEDX registers `TRT` in `cedx_scale_map()`
#' precisely so the assignment is stable across views). Without a map, a
#' fixed palette keyed by the SORTED levels -- stable for one cohort, and
#' honestly not comparable across boards, which is the argument for
#' declaring the binding.
#'
#' @param frame A [pp_cohort_frame()] result.
#' @param map A board scale map, or `NULL`.
#' @param dm_obj The normalized dm (the map resolves against the real column).
#' @param arm_col The resolved arm column, or `NULL`.
#' @return A named character vector, arm level -> hex colour. Empty when the
#'   frame carries no arm.
#' @noRd
pp_cohort_arm_colors <- function(frame, map = NULL, dm_obj = NULL,
                                 arm_col = NULL) {

  if (!"ARM" %in% names(frame) || !nrow(frame)) {
    return(stats::setNames(character(), character()))
  }

  levels <- sort(unique(as.character(frame$ARM)))
  levels <- levels[!is.na(levels) & nzchar(levels)]
  if (!length(levels)) {
    return(stats::setNames(character(), character()))
  }

  if (!is.null(map) && !is.null(arm_col) && inherits(dm_obj, "dm")) {
    adsl <- tryCatch(
      dm::dm_get_tables(dm_obj)[[pp_subject_tbl_name(
        names(dm::dm_get_tables(dm_obj))
      )]],
      error = function(e) NULL
    )
    if (!is.null(adsl) && arm_col %in% colnames(adsl)) {
      res <- tryCatch(
        blockr.theme::resolve_scales_col(map, arm_col, adsl[[arm_col]])$color,
        error = function(e) NULL
      )
      if (length(res) && !is.null(names(res))) {
        hit <- levels[levels %in% names(res)]
        if (length(hit)) {
          out <- stats::setNames(rep("#9ca3af", length(levels)), levels)
          out[hit] <- unname(res[hit])
          return(out)
        }
      }
    }
  }

  pal <- c("#2563eb", "#059669", "#d97706", "#7c3aed", "#db2777", "#0891b2",
           "#65a30d", "#dc2626")
  stats::setNames(pal[((seq_along(levels) - 1L) %% length(pal)) + 1L], levels)
}

#' Validate a clicked patient against the cohort
#'
#' The whole guard behind the row click, as a function rather than a
#' condition buried in an observer, so the rule is testable without a
#' session: a click may only select someone the cohort actually holds. A
#' forged or stale id is dropped rather than wedging the block on a patient
#' it cannot render (the same rule `input$pp_subject` applies for the header
#' picker).
#'
#' @param sel The clicked value, as it arrives from the client.
#' @param ids The cohort's USUBJIDs.
#' @return The id when it is in the cohort, otherwise `NULL`.
#' @noRd
pp_cohort_pick <- function(sel, ids) {
  sel <- as.character(sel)
  if (length(sel) != 1L || is.na(sel) || !nzchar(sel)) return(NULL)
  if (!sel %in% ids) return(NULL)
  sel
}

#' Split a cohort's ids into the part that is shared and the part that varies
#'
#' A prod USUBJID is around twenty characters and most of them are constant:
#' `CA2440001-01-703-1129` is study, then site, then subject, and the study is
#' the same for every row of the board. Printing it 254 times is what pushes
#' the row past the sidebar's width.
#'
#' So this is not truncation. Nothing that distinguishes two patients is ever
#' hidden -- only a prefix that is byte-identical across the whole cohort is
#' lifted out, and the caller prints it once in the section header. An
#' ellipsis at either end would be worse in both directions: cutting the tail
#' hides the subject number, and cutting the head hides which site.
#'
#' @section Why it trims to a separator, and keeps two segments:
#' The common prefix of a cohort at one site is `CA2440001-01-703-1` -- one
#' character into the subject number. Cutting there would leave `129`, which
#' is not an id anyone recognises, so the prefix is trimmed back to the last
#' separator it contains.
#'
#' It also never strips so far that fewer than two segments remain. Without
#' that floor a patient reads as `703-1129` in the whole cohort and `1129`
#' once you filter to their site, and the id under the cursor changes as you
#' filter. Two segments is stable across every narrowing.
#'
#' @param ids Character vector of USUBJIDs.
#' @param min_prefix Shortest prefix worth lifting. Below this the row is not
#'   meaningfully shorter and the header gains a line for nothing.
#' @return `list(prefix, short)` -- the lifted prefix (`""` when nothing is
#'   shared) and the ids as they should be displayed.
#' @noRd
pp_cohort_id_display <- function(ids, min_prefix = 4L) {

  ids <- as.character(ids)
  none <- list(prefix = "", short = ids)
  if (length(ids) < 2L || anyNA(ids)) return(none)

  chars <- strsplit(ids, "", fixed = TRUE)
  n <- min(lengths(chars))
  if (n == 0L) return(none)

  first <- chars[[1L]]
  same <- vapply(seq_len(n), function(i) {
    all(vapply(chars, function(x) identical(x[[i]], first[[i]]), logical(1)))
  }, logical(1))
  k <- if (all(same)) n else which(!same)[[1L]] - 1L
  if (k <= 0L) return(none)

  prefix <- substr(ids[[1L]], 1L, k)

  # Back off to the last separator, so the cut never lands mid-segment.
  seps <- gregexpr("[-_./ ]", prefix)[[1L]]
  if (seps[[1L]] == -1L) return(none)
  prefix <- substr(prefix, 1L, max(seps))

  # ...and back off further while fewer than two segments would remain, so
  # the displayed id does not change as the cohort narrows.
  repeat {
    if (!nzchar(prefix)) return(none)
    short <- substring(ids, nchar(prefix) + 1L)
    if (min(lengths(strsplit(short, "[-_./ ]"))) >= 2L) break
    inner <- gregexpr("[-_./ ]", substr(prefix, 1L, nchar(prefix) - 1L))[[1L]]
    if (inner[[1L]] == -1L) return(none)
    prefix <- substr(prefix, 1L, max(inner))
  }

  if (nchar(prefix) < min_prefix) return(none)
  list(prefix = prefix, short = short)
}
