# Patient Profile Visualization Definitions
#
# Shared helpers and collector for patient profile visualizations.
# Each viz is defined in its own file (viz-*.R) and collected here.

# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------

#' Convert Date to millisecond timestamp for echarts
#' @param d Date or POSIXct value
#' @noRd
pp_ms_ts <- function(d) as.numeric(as.POSIXct(d)) * 1000

#' Convert a Date / POSIXct to the x-axis value used by patient-profile charts
#'
#' When `mode == "date"`, returns the millisecond timestamp consumed by
#' echarts time axes. When `mode == "rday"`, returns the relative day from
#' `ref_ms` (treatment start), positive after start, negative before, with
#' `ref_ms` itself mapping to 1 to match the ADaM \*DY convention.
#'
#' @param d Date / POSIXct (scalar or vector)
#' @param ref_ms Reference timestamp in ms (treatment start). May be NA.
#' @param mode "date" or "rday"
#' @noRd
pp_xval <- function(d, ref_ms = NA_real_, mode = "date") {
  ts <- pp_ms_ts(d)
  if (identical(mode, "rday") && !is.na(ref_ms)) {
    floor((ts - ref_ms) / 86400000) + 1
  } else {
    ts
  }
}

#' Convert an ADaM study day to the continuous relative-day axis
#'
#' `pp_xval()`'s relative-day output is a CONTINUOUS scale with no gap at zero:
#' treatment start is 1, the day before it is 0. An ADaM \*DY column skips zero
#' instead, so the day before treatment start is -1. The two therefore differ
#' by one across the whole pre-treatment region, and a native \*DY plotted raw
#' would sit a day left of everything derived from a date on the same shared
#' axis. Map it onto the continuous scale before plotting; [pp_day_label()]
#' reports the untouched \*DY value.
#'
#' @param dy ADaM study day (scalar or vector).
#' @return Numeric position on the relative-day axis.
#' @noRd
pp_day_to_x <- function(dy) {
  dy <- as.numeric(dy)
  ifelse(!is.na(dy) & dy > 0, dy, dy + 1)
}

#' Format an ADaM study day for tooltip display
#'
#' The value is already a \*DY, so it needs no remapping — unlike
#' [pp_xlabel()], which has to undo the continuous scale first.
#'
#' @param dy ADaM study day.
#' @param cycle A [pp_cycle_anchor_days()] frame, or `NULL`. When given, the
#'   cycle/day rides behind the study day as `D22 (C2 D1)`.
#' @noRd
pp_day_label <- function(dy, cycle = NULL) {
  if (is.na(dy)) return("")
  pp_with_cycle(paste0("D", dy), pp_day_to_x(dy), cycle)
}

#' x-axis value, preferring a study day the data already carries
#'
#' Relative-day mode plots study days, and many studies ship them outright
#' (`ASTDY`, `ADY`) while shipping no analysis date at all. Reconstructing a
#' date from such a day only to subtract it back into a day is a lossy round
#' trip: it has to assume an anchor and a day-zero convention, and getting
#' either wrong shifts every point by a day without looking wrong. So when a
#' native day is present and we are in relative-day mode, use it directly.
#'
#' Date mode still needs real dates; a study with no dates cannot render it,
#' which is why `day` is the preferred source rather than the only one.
#'
#' @param d Date / POSIXct (scalar or vector), or `NULL` when the study ships
#'   no analysis date.
#' @param day ADaM study day (scalar or vector), or `NULL` when the study ships
#'   none.
#' @param ref_ms Reference timestamp in ms (treatment start). May be NA.
#' @param mode "date" or "rday"
#' @noRd
pp_xval_pref_day <- function(d, day, ref_ms = NA_real_, mode = "date") {
  if (identical(mode, "rday") && !is.null(day)) return(pp_day_to_x(day))
  pp_xval(d, ref_ms, mode)
}

#' Format a value for tooltip display, respecting the timeline mode
#'
#' Returns the calendar date (`YYYY-MM-DD`) in date mode, or `D<n>` in
#' relative-day mode (skipping day 0, matching ADaM's \*DY convention).
#'
#' @param cycle A [pp_cycle_anchor_days()] frame, or `NULL`. When given, the
#'   cycle/day is appended in BOTH modes — the clinician's ask was to see the
#'   cycle day *in addition to* what is already there, not instead of it.
#' @noRd
pp_xlabel <- function(d, ref_ms = NA_real_, mode = "date", cycle = NULL) {
  base <- if (identical(mode, "rday") && !is.na(ref_ms)) {
    day <- pp_xval(d, ref_ms, "rday")
    if (is.na(day)) return("")
    if (day > 0) paste0("D", day) else paste0("D", day - 1)
  } else {
    if (is.na(d)) return("")
    format(as.Date(d))
  }
  # NA ref_ms leaves pp_xval() returning a millisecond timestamp, which would
  # be nonsense to look up against day-space anchors; there is no cycle to
  # report without a treatment start anyway.
  x_day <- if (is.na(ref_ms)) NA_real_ else pp_xval(d, ref_ms, "rday")
  pp_with_cycle(base, x_day, cycle)
}

#' Build shared echarts x-axis config for date OR relative-day mode
#'
#' @param time_range Length-2 Date vector (min, max)
#' @param ref_ms Reference timestamp in ms (TRTSDT) — required when mode = "rday"
#' @param mode "date" (calendar date axis) or "rday" (relative day axis)
#' @param show_labels Whether to show axis labels
#' @noRd
pp_time_axis <- function(time_range, ref_ms = NA_real_, mode = "date",
                         show_labels = TRUE) {
  if (identical(mode, "rday") && !is.na(ref_ms)) {
    list(
      type = "value",
      min = pp_xval(time_range[1], ref_ms, "rday"),
      max = pp_xval(time_range[2], ref_ms, "rday"),
      name = if (show_labels) "Day" else NULL,
      nameLocation = "end",
      nameGap = 8,
      nameTextStyle = list(color = PP_AXIS_LABEL_COLOR, fontSize = 11),
      axisLine = list(show = FALSE),
      axisTick = list(show = FALSE),
      axisLabel = list(
        color = PP_AXIS_LABEL_COLOR, fontSize = 11,
        show = show_labels,
        formatter = htmlwidgets::JS(
          "function(v) { return v > 0 ? 'D' + v : 'D' + (v - 1); }"
        )
      ),
      splitLine = list(
        show = TRUE,
        lineStyle = list(color = PP_SPLIT_LINE_COLOR, type = "dashed")
      )
    )
  } else {
    list(
      type = "time",
      min = pp_ms_ts(time_range[1]),
      max = pp_ms_ts(time_range[2]),
      axisLine = list(show = FALSE),
      axisTick = list(show = FALSE),
      axisLabel = list(
        color = PP_AXIS_LABEL_COLOR, fontSize = 11,
        show = show_labels
      ),
      splitLine = list(
        show = TRUE,
        lineStyle = list(color = PP_SPLIT_LINE_COLOR, type = "dashed")
      )
    )
  }
}

#' Shared echarts toolbox: small, muted icons matching the canonical
#' drill-down chart styling (blockr.viz/inst/js/drilldown-chart.js).
#'
#' @noRd
pp_toolbox <- function() {
  list(
    show = TRUE,
    right = 8,
    top = 4,
    itemSize = 11,
    feature = list(
      saveAsImage = list(title = "Save", pixelRatio = 2)
    ),
    iconStyle = list(borderColor = "#bbb")
  )
}

#' Canonical axis colors used by the drill-down chart family. Kept
#' centrally so all patient-profile vizs read consistent values.
#' @noRd
PP_AXIS_LABEL_COLOR <- "#666"
PP_AXIS_LINE_COLOR <- "#ccc"
PP_SPLIT_LINE_COLOR <- "#f3f4f6"

#' Encode a string as a JavaScript string literal
#'
#' Study data reaches the charts as text pasted into hand-written JS
#' (`renderItem`, tooltip formatters). Hand-escaping the quote is not enough:
#' a newline, a backslash or a control character in an arm label or a
#' parameter name breaks the literal, and a JS syntax error kills the whole
#' widget -- the panel renders its header and an empty body, with nothing in
#' the R log to explain it. Let the JSON encoder do it: it escapes every
#' character JS cares about and returns the surrounding quotes, so callers
#' interpolate the result WITHOUT wrapping it in quotes of their own.
#'
#' `pp_js_str()` always yields a single string literal, `pp_js_arr()` always an
#' array literal -- the shape must not depend on the length of the data, or a
#' one-item questionnaire would emit a bare string where the JS indexes an
#' array.
#'
#' @param x Character vector (or anything coercible). `NA` becomes `""`.
#' @return A length-1 string holding a JS literal, quotes included.
#' @noRd
pp_js_str <- function(x) {
  stopifnot(length(x) == 1L)
  as.character(jsonlite::toJSON(pp_js_chr(x), auto_unbox = TRUE))
}

#' @rdname pp_js_str
#' @noRd
pp_js_arr <- function(x) {
  as.character(jsonlite::toJSON(pp_js_chr(x), auto_unbox = FALSE))
}

#' @noRd
pp_js_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

#' Visit levels in visit order
#'
#' `AVISIT` values ordered by their numeric companion `AVISITN` when the
#' table carries one, lexically otherwise. Lexical order puts "Week 10"
#' before "Week 2", so any default of the form "the last two visits" picks
#' the wrong two without this -- with no cue that it did.
#'
#' @param tbl A findings data.frame.
#' @return Character vector of visit labels, in visit order.
#' @noRd
pp_visit_levels <- function(tbl) {
  if (!"AVISIT" %in% colnames(tbl)) return(character())
  v <- trimws(as.character(tbl$AVISIT))
  if ("AVISITN" %in% colnames(tbl)) {
    n <- suppressWarnings(as.numeric(tbl$AVISITN))
    pairs <- unique(data.frame(v = v, n = n, stringsAsFactors = FALSE))
    out <- unique(pairs[order(pairs$n, pairs$v), , drop = FALSE]$v)
  } else {
    out <- sort(unique(v))
  }
  out[!is.na(out) & nzchar(out)]
}

#' Build shared echarts tooltip config
#' @noRd
pp_tooltip <- function() {
  list(
    trigger = "item",
    confine = TRUE,
    backgroundColor = "rgba(255,255,255,0.98)",
    borderColor = "#d1d5db",
    borderWidth = 1,
    textStyle = list(color = "#1f2937", fontSize = 12),
    extraCssText = paste0(
      "box-shadow: 0 4px 12px rgba(0,0,0,0.08);",
      "border-radius: 6px; padding: 8px 12px;"
    )
  )
}

#' Generate colored square icon HTML for sidebar cards
#'
#' Creates a 40x40 colored square with a Bootstrap Icon SVG inside,
#' similar to blockr.dock's blk_icon_data_uri() but self-contained.
#'
#' @param icon_name Icon identifier (maps to Bootstrap Icons SVG paths)
#' @param color Hex color for icon fill and background tint
#' @noRd
pp_icon_html <- function(icon_name, color) {
  icons <- list(
    `exclamation-triangle` = paste0(
      '<path d="M7.938 2.016A.13.13 0 0 1 8.002 2a.13.13 0 0 1 .063.016',
      '.146.146 0 0 1 .054.057l6.857 11.667c.036.06.035.124.002.183a.163',
      '.163 0 0 1-.054.06.116.116 0 0 1-.066.017H1.146a.115.115 0 0 1-',
      '.066-.017.163.163 0 0 1-.054-.06.176.176 0 0 1 .002-.183L7.884 ',
      '2.073a.147.147 0 0 1 .054-.057zm1.044-.45a1.13 1.13 0 0 0-1.96 ',
      '0L.165 13.233c-.457.778.091 1.767.98 1.767h13.713c.889 0 1.438-',
      '.99.98-1.767z"/>',
      '<path d="M7.002 12a1 1 0 1 1 2 0 1 1 0 0 1-2 0zM7.1 5.995a.905',
      '.905 0 1 1 1.8 0l-.35 3.507a.552.552 0 0 1-1.1 0z"/>'
    ),
    droplet = paste0(
      '<path fill-rule="evenodd" d="M7.21.8C7.69.295 8 0 8 0q.164.544',
      '.371 1.038c.812 1.946 2.073 3.35 3.197 4.6C12.878 7.096 14 ',
      '8.345 14 10a6 6 0 0 1-12 0C2 6.668 5.58 2.517 7.21.8m.413 ',
      '1.021A31 31 0 0 0 5.171 4.9C3.806 6.583 3 8.29 3 10a5 5 0 0 ',
      '0 10 0c0-1.382-.87-2.501-2.029-3.769-.133-.145-.27-.296-.41-',
      '.449a29 29 0 0 1-3.349-4.045 25 25 0 0 1-.413-.916"/>'
    ),
    `droplet-half` = paste0(
      '<path fill-rule="evenodd" d="M7.21.8C7.69.295 8 0 8 0q.164.544',
      '.371 1.038c.812 1.946 2.073 3.35 3.197 4.6C12.878 7.096 14 ',
      '8.345 14 10a6 6 0 0 1-12 0C2 6.668 5.58 2.517 7.21.8M8 ',
      '1.632A30 30 0 0 0 5.252 4.82C3.86 6.532 3 8.266 3 10a5 5 0 0 ',
      '0 5 5zm0 0A30 30 0 0 1 10.748 4.82C12.14 6.532 13 8.266 13 ',
      '10a5 5 0 0 1-5 5z"/>'
    ),
    `heart-pulse` = paste0(
      '<path d="m8 2.748-.717-.737C5.6.281 2.514.878 1.4 3.053c-.523 ',
      '1.023-.641 2.5.314 4.385.92 1.815 2.834 3.989 6.286 6.357 ',
      '3.452-2.368 5.365-4.542 6.286-6.357.955-1.886.838-3.362.314-',
      '4.385C13.486.878 10.4.28 8.717 2.01zM8 15C-7.333 4.868 ',
      '3.279-3.04 7.824 1.143q.09.083.176.171a3 3 0 0 1 .176-.17C12.72',
      '-3.042 23.333 4.867 8 15"/>',
      '<path d="M5.966 9.463a.5.5 0 0 0-.416.222l-.98 1.373L3.084 ',
      '8.66a.5.5 0 0 0-.864.504l1.81 3.163a.5.5 0 0 0 .863.01l1.347-',
      '1.886 1.28 1.553a.5.5 0 0 0 .76.02l1.56-1.769 1.378 1.126a.5.5',
      ' 0 1 0 .634-.776L10.34 9.27a.5.5 0 0 0-.712.02L8.13 10.94l-',
      '1.385-1.681a.5.5 0 0 0-.344-.213z"/>'
    ),
    `graph-up` = paste0(
      '<path fill-rule="evenodd" d="M0 0h1v15h15v1H0zm14.817 3.113a.5',
      '.5 0 0 1 .07.704l-4.5 5.5a.5.5 0 0 1-.74.037L7.06 6.767l-3.656',
      ' 5.027a.5.5 0 0 1-.808-.588l4-5.5a.5.5 0 0 1 .758-.06l2.609 ',
      '2.61 4.15-5.073a.5.5 0 0 1 .704-.07"/>'
    ),
    `clipboard-pulse` = paste0(
      '<path fill-rule="evenodd" d="M10 .5a.5.5 0 0 0-.5-.5h-3a.5.5 ',
      '0 0 0-.5.5.5.5 0 0 1-.5.5.5.5 0 0 0-.5.5V2a.5.5 0 0 0 .5.5',
      'h5A.5.5 0 0 0 11 2v-.5a.5.5 0 0 0-.5-.5.5.5 0 0 1-.5-.5"/>',
      '<path d="M4.085 1H3.5A1.5 1.5 0 0 0 2 2.5v12A1.5 1.5 0 0 0 ',
      '3.5 16h9a1.5 1.5 0 0 0 1.5-1.5v-12A1.5 1.5 0 0 0 12.5 1h-',
      '.585q.084.236.085.5V2a1.5 1.5 0 0 1-1.5 1.5h-5A1.5 1.5 0 0 ',
      '1 4 2v-.5q.001-.264.085-.5M9.98 5.356 11.372 7h.128a.5.5 0 0 ',
      '1 0 1h-2a.5.5 0 0 1 0-1h.5L8.933 5.574 7.956 9.5a.5.5 0 0 ',
      '1-.956.044l-.5-3L5.275 8H4.5a.5.5 0 0 1 0-1h1.218a.5.5 0 0 ',
      '1 .38.173l.882 1.01 1.063-4.139a.5.5 0 0 1 .937.012"/>'
    ),
    capsule = paste0(
      '<path d="M1.828 8.9 8.9 1.827a4 4 0 1 1 5.657 5.657l-7.07 ',
      '7.071A4 4 0 1 1 1.827 8.9Zm9.128.771 2.893-2.893a3 3 0 1 0-',
      '4.243-4.242L6.713 5.429z"/>'
    ),
    `grid-3x3` = paste0(
      '<path d="M0 1.5A1.5 1.5 0 0 1 1.5 0h13A1.5 1.5 0 0 1 16 ',
      '1.5v13a1.5 1.5 0 0 1-1.5 1.5h-13A1.5 1.5 0 0 1 0 14.5zM1.5 ',
      '1a.5.5 0 0 0-.5.5V5h4V1zM5 6H1v4h4zm1 4h4V6H6zm-1 1H1v3.5a',
      '.5.5 0 0 0 .5.5H5zm1 0v4h4v-4zm5 0v4h3.5a.5.5 0 0 0 .5-.5V11',
      'zm0-1h4V6h-4zm0-5h4V1.5a.5.5 0 0 0-.5-.5H11zm-1 0V1H6v4z"/>'
    ),
    `arrows-vertical` = paste0(
      '<path d="M8.354 14.854a.5.5 0 0 1-.708 0l-2-2a.5.5 0 0 1 ',
      '.708-.708L7.5 13.293V2.707L6.354 3.854a.5.5 0 1 1-.708-.708',
      'l2-2a.5.5 0 0 1 .708 0l2 2a.5.5 0 0 1-.708.708L8.5 2.707v',
      '10.586l1.146-1.147a.5.5 0 0 1 .708.708z"/>'
    )
  )

  svg_path <- icons[[icon_name]]
  if (is.null(svg_path)) svg_path <- icons[["graph-up"]]

  hex <- sub("^#", "", color)
  r <- strtoi(substr(hex, 1, 2), 16L)
  g <- strtoi(substr(hex, 3, 4), 16L)
  b <- strtoi(substr(hex, 5, 6), 16L)
  bg_rgba <- sprintf("rgba(%d,%d,%d,0.15)", r, g, b)

  sprintf(
    paste0(
      '<div style="width:40px;height:40px;border-radius:7px;background:%s;',
      'display:flex;align-items:center;justify-content:center;flex-shrink:0">',
      '<svg xmlns="http://www.w3.org/2000/svg" width="20" height="20" ',
      'fill="%s" viewBox="0 0 16 16">%s</svg></div>'
    ),
    bg_rgba, color, svg_path
  )
}

#' Compute shared time range across all date columns in a dm
#'
#' Scans known date columns (ASTDT, AENDT, ADT, TRTSDT, TRTEDT) across
#' all tables in the dm and returns the overall min/max as a Date vector.
#'
#' @param dm_obj A dm object
#' @return Length-2 Date vector or NULL if no dates found
#' @noRd
pp_compute_ref_ms <- function(dm_obj, ref_col = NULL) {
  if (!inherits(dm_obj, "dm")) return(NA_real_)
  tbls <- dm::dm_get_tables(dm_obj)
  if (!"adsl" %in% names(tbls)) return(NA_real_)
  adsl <- as.data.frame(tbls[["adsl"]])
  ref_col <- ref_col %||% "TRTSDT"
  if (!ref_col %in% colnames(adsl) || nrow(adsl) == 0) return(NA_real_)
  v <- pp_as_date(adsl[[ref_col]][1])
  if (is.na(v)) return(NA_real_)
  pp_ms_ts(v)
}

#' The subject's visit schedule, from whatever findings tables carry it
#'
#' There is no subject-visit table in the profile's inputs (SDTM's SV is not
#' loaded), but every findings table records which visit each measurement
#' belongs to. Collecting the earliest measurement date (and study day) per
#' visit label across all of them reconstructs the schedule well enough for
#' a ruler: one tick per visit the subject actually attended.
#'
#' @param tbls Named list of tables (from `dm::dm_get_tables()`), subject-
#'   scoped.
#' @return A data.frame with `visit`, `date` (Date, may be NA) and `day`
#'   (numeric, may be NA), ordered by time; zero rows when nothing carries
#'   visits.
#' @noRd
pp_visit_schedule <- function(tbls) {
  empty <- data.frame(visit = character(), date = as.Date(character()),
                      day = numeric(), stringsAsFactors = FALSE)
  rows <- list()
  for (tbl_name in names(tbls)) {
    df <- as.data.frame(tbls[[tbl_name]])
    if (!"AVISIT" %in% colnames(df)) next
    visit <- trimws(as.character(df$AVISIT))
    keep <- !is.na(visit) & nzchar(visit)
    if (!any(keep)) next
    df <- df[keep, , drop = FALSE]
    visit <- visit[keep]
    date <- if ("ADT" %in% colnames(df)) pp_as_date(df$ADT) else
      as.Date(rep(NA, nrow(df)))
    day <- if ("ADY" %in% colnames(df)) {
      suppressWarnings(as.numeric(df$ADY))
    } else {
      rep(NA_real_, nrow(df))
    }
    rows[[length(rows) + 1L]] <- data.frame(
      visit = visit, date = date, day = day, stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) return(empty)

  all <- do.call(rbind, rows)
  all <- all[!is.na(all$date) | !is.na(all$day), , drop = FALSE]
  if (!nrow(all)) return(empty)

  min_or_na <- function(x) if (all(is.na(x))) x[1] else min(x, na.rm = TRUE)
  out <- do.call(rbind, lapply(split(all, all$visit), function(g) {
    data.frame(visit = g$visit[1], date = min_or_na(g$date),
               day = min_or_na(g$day), stringsAsFactors = FALSE)
  }))
  # A study is dated or day-only consistently; never mix the two scales in
  # one sort key (a Date's numeric is days since 1970, a *DY is ~tens).
  ord <- if (any(!is.na(out$date))) {
    order(out$date, out$day, na.last = TRUE)
  } else {
    order(out$day, na.last = TRUE)
  }
  out <- out[ord, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Clip the timeline's lower bound to the screening window
#'
#' One concomitant medication started years before the study must not
#' stretch every panel's axis to it. Unless the user opts into the full
#' pre-treatment history (the gear's "Pre-treatment" toggle), the shared
#' time range starts no earlier than `margin_days` before the reference
#' (treatment start) -- wide enough for the screening window, so baseline
#' labs and vitals stay on screen. Bars that started earlier but are still
#' running enter from the left axis edge; only events entirely before the
#' floor drop out.
#'
#' Total: without a reference (no treatment dates, or no patient on screen)
#' there is nothing to anchor the floor to, and the range passes through.
#'
#' @param time_range Length-2 Date vector, or `NULL`.
#' @param ref_ms Reference timestamp in ms (see [pp_compute_ref_ms()]).
#' @param margin_days Screening margin before the reference.
#' @return The (possibly clipped) time range.
#' @noRd
pp_clip_prestudy <- function(time_range, ref_ms, margin_days = 30L) {
  if (is.null(time_range) || is.na(ref_ms)) return(time_range)
  floor_date <- as.Date(as.POSIXct(ref_ms / 1000, origin = "1970-01-01",
                                   tz = "UTC")) - margin_days
  if (time_range[1] < floor_date) {
    # never invert the range (a study entirely pre-reference keeps its end)
    time_range[1] <- min(floor_date, time_range[2])
  }
  time_range
}

#' Can this study express relative-day mode at all?
#'
#' A study-level question (does ADSL carry a usable TRTSDT), asked by the
#' header gear on the UNSCOPED dm. Distinct from [pp_compute_ref_ms()],
#' which is per-patient (row 1 of a subject-scoped ADSL): asking the
#' per-patient function a study-level question let one arbitrary cohort
#' member with a missing treatment start disable relative-day mode for the
#' whole study.
#'
#' @param dm_obj A normalized `dm` (or anything; total).
#' @param ref_col The timeline role's resolved column (default `TRTSDT`).
#' @return TRUE when any ADSL row carries a non-missing reference.
#' @noRd
pp_has_ref <- function(dm_obj, ref_col = NULL) {
  if (!inherits(dm_obj, "dm")) return(FALSE)
  tbls <- dm::dm_get_tables(dm_obj)
  if (!"adsl" %in% names(tbls)) return(FALSE)
  adsl <- as.data.frame(tbls[["adsl"]])
  ref_col <- ref_col %||% "TRTSDT"
  ref_col %in% colnames(adsl) && any(!is.na(pp_as_date(adsl[[ref_col]])))
}

pp_compute_time_range <- function(dm_obj, ref_col = NULL) {
  tbls <- dm::dm_get_tables(dm_obj)

  # Canonical timeline columns, scanned across EVERY table. The dm has been
  # normalized (pp_normalize_dm()) before this runs, so the canonical names
  # are the only spellings left to find. The per-table map this replaces is
  # what silently clipped the axis for aliased studies: it scanned raw names
  # off a dm whose column aliases resolved later, per viz, so every event
  # outside the ADSL treatment window fell off the axis with no cue.
  date_cols <- c("ASTDT", "AENDT", "ADT", "TRTSDT", "TRTEDT")
  day_cols <- c("ASTDY", "AENDY", "ADY")

  all_dates <- do.call(c, lapply(names(tbls), function(tbl_name) {
    tbl <- as.data.frame(tbls[[tbl_name]])
    dates <- do.call(c, lapply(intersect(date_cols, colnames(tbl)),
      function(col) pp_as_date(tbl[[col]])
    ))
    if (is.null(dates)) return(as.Date(character()))
    dates[!is.na(dates)]
  }))

  # Study-day columns, for studies that ship a day but no analysis date. The
  # axis would otherwise never see those events and would clip them. Bounds
  # only: converting a day back to a date is the lossy round trip that
  # pp_xval_pref_day() exists to avoid, but an axis end landing a day wide is
  # invisible, where an event landing a day off is not.
  ref_ms <- pp_compute_ref_ms(dm_obj, ref_col)
  if (!is.na(ref_ms)) {
    anchor <- as.Date(as.POSIXct(ref_ms / 1000, origin = "1970-01-01",
                                 tz = "UTC"))
    from_days <- do.call(c, lapply(names(tbls), function(tbl_name) {
      tbl <- as.data.frame(tbls[[tbl_name]])
      ds <- do.call(c, lapply(intersect(day_cols, colnames(tbl)),
        function(col) {
          dy <- suppressWarnings(as.numeric(tbl[[col]]))
          dy <- dy[!is.na(dy)]
          if (!length(dy)) return(as.Date(character()))
          anchor + pp_day_to_x(dy) - 1
        }
      ))
      if (is.null(ds)) return(as.Date(character()))
      ds[!is.na(ds)]
    }))
    all_dates <- c(all_dates, from_days)
  }

  if (length(all_dates) == 0) return(NULL)
  as.Date(c(min(all_dates), max(all_dates)), origin = "1970-01-01")
}



# ---------------------------------------------------------------------------
# Shared findings chart renderer
# ---------------------------------------------------------------------------

#' Render a single-domain findings chart
#'
#' Builds a multi-PARAMCD line+scatter echarts chart for a single findings
#' domain (adlbc, adlbh, advs). Used by lab_chemistry_viz, lab_hematology_viz,
#' and vital_signs_viz.
#'
#' @param dm_obj A dm object
#' @param time_range Length-2 Date vector
#' @param table_name Domain table name (e.g., "adlbc")
#' @param label Display label for the domain
#' @param base_color Default line color
#' @noRd
pp_render_findings <- function(dm_obj, time_range, table_name, label,
                               base_color, paramcds = NULL,
                               ref_ms = NA_real_, mode = "date") {
  tbls <- dm::dm_get_tables(dm_obj)
  tbl <- as.data.frame(tbls[[table_name]])

  # Structural columns (PARAMCD, AVAL, ADT) are validated upstream by the
  # dispatcher via pp_resolve_requires(). This function only handles row
  # emptiness and value filtering.
  tbl <- tbl[!is.na(tbl$ADT) & !is.na(tbl$AVAL), , drop = FALSE]
  if (!is.null(paramcds)) {
    tbl <- tbl[tbl$PARAMCD %in% paramcds, , drop = FALSE]
  }
  if (nrow(tbl) == 0) return(pp_empty_chart(paste("No", label, "records")))

  anrind_colors <- list(H = "#dc2626", L = "#2563eb", N = "#059669")
  param_colors <- c(
    "#2563EB", "#dc2626", "#059669", "#D97706", "#7C3AED",
    "#0891B2", "#EA580C", "#374151", "#0D9488", "#BE123C"
  )

  params <- sort(unique(tbl$PARAMCD))
  has_anrind <- "ANRIND" %in% colnames(tbl)
  has_ref <- all(c("A1LO", "A1HI") %in% colnames(tbl))
  has_param <- "PARAM" %in% colnames(tbl)

  n_params <- length(params)
  grid_height <- max(120, floor(400 / max(n_params, 1)))
  grid_gap <- 50
  top_pad <- 10
  bot_pad <- 30
  total_height <- top_pad + n_params * (grid_height + grid_gap) + bot_pad

  grids <- list()
  titles <- list()
  x_axes <- list()
  y_axes <- list()
  all_series <- list()

  for (p_idx in seq_along(params)) {
    param <- params[p_idx]
    p_data <- tbl[tbl$PARAMCD == param, , drop = FALSE]
    p_data <- p_data[order(p_data$ADT), , drop = FALSE]
    grid_idx <- p_idx - 1L
    color <- param_colors[((p_idx - 1L) %% length(param_colors)) + 1L]

    grid_top <- top_pad + 20 + grid_idx * (grid_height + grid_gap)

    # Subtitle: PARAMCD with short PARAM description
    param_label <- if (has_param && nrow(p_data) > 0) {
      full <- as.character(p_data$PARAM[1])
      if (nchar(full) > 40) full <- paste0(substr(full, 1, 37), "...")
      paste0(param, " \u2014 ", full)
    } else {
      param
    }
    titles[[p_idx]] <- list(
      text = param_label,
      left = 60,
      top = grid_top - 18,
      textStyle = list(fontSize = 11, fontWeight = 400, color = "#6b7280")
    )

    grids[[p_idx]] <- list(
      left = 60, right = 20,
      top = grid_top, height = grid_height,
      borderColor = "transparent"
    )

    x_axes[[p_idx]] <- pp_time_axis(time_range, ref_ms, mode,
      show_labels = (p_idx == n_params)
    )
    x_axes[[p_idx]]$gridIndex <- grid_idx

    y_axes[[p_idx]] <- list(
      type = "value",
      gridIndex = grid_idx,
      axisLine = list(show = FALSE),
      axisTick = list(show = FALSE),
      axisLabel = list(color = PP_AXIS_LABEL_COLOR, fontSize = 11),
      splitLine = list(
        show = TRUE,
        lineStyle = list(color = PP_SPLIT_LINE_COLOR, type = "dashed",
                         opacity = 0.5)
      )
    )

    # Line data
    line_data <- lapply(seq_len(nrow(p_data)), function(i) {
      list(value = list(pp_xval(p_data$ADT[i], ref_ms, mode),
                        p_data$AVAL[i]))
    })

    # Scatter data with ANRIND coloring + rich tooltips
    scatter_data <- lapply(seq_len(nrow(p_data)), function(i) {
      val <- p_data$AVAL[i]
      dt <- pp_xval(p_data$ADT[i], ref_ms, mode)

      pt_color <- color
      if (has_anrind && !is.na(p_data$ANRIND[i])) {
        anr <- as.character(p_data$ANRIND[i])
        if (anr %in% names(anrind_colors)) pt_color <- anrind_colors[[anr]]
      }

      tt <- paste0(
        '<div style="min-width:160px">',
        '<div style="font-size:14px;font-weight:700;margin-bottom:2px">',
        param, '</div>'
      )
      if (has_param) {
        tt <- paste0(tt,
          '<div style="font-size:11px;color:#888;margin-bottom:4px">',
          p_data$PARAM[i], '</div>'
        )
      }
      if (has_anrind && !is.na(p_data$ANRIND[i])) {
        anr <- as.character(p_data$ANRIND[i])
        pill <- switch(anr,
          H = , HIGH = list(bg = "rgba(220,38,38,0.1)", fg = "#DC2626",
            bd = "rgba(220,38,38,0.15)"),
          L = , LOW = list(bg = "rgba(37,99,235,0.1)", fg = "#2563EB",
            bd = "rgba(37,99,235,0.15)"),
          N = , NORMAL = list(bg = "rgba(5,150,105,0.1)", fg = "#059669",
            bd = "rgba(5,150,105,0.15)"),
          list(bg = "rgba(107,114,128,0.1)", fg = "#6b7280",
            bd = "rgba(107,114,128,0.15)")
        )
        tt <- paste0(tt,
          '<span style="display:inline-block;background:', pill$bg,
          ';color:', pill$fg, ';border:1px solid ', pill$bd,
          ';padding:1px 6px;border-radius:4px;font-size:10px;',
          'font-weight:600;margin-bottom:4px">', anr, '</span><br/>'
        )
      }
      tt <- paste0(tt,
        '<div style="font-size:12px;line-height:1.6">',
        '<span style="color:#6b7280">Date:</span> ', format(p_data$ADT[i]),
        '<br/><span style="color:#6b7280">AVAL:</span> <b>',
        round(val, 2), '</b>'
      )
      if (has_ref && !is.na(p_data$A1LO[i]) && !is.na(p_data$A1HI[i])) {
        tt <- paste0(tt, '<br/><span style="color:#6b7280">Ref:</span> ',
          round(p_data$A1LO[i], 1), ' \u2013 ', round(p_data$A1HI[i], 1))
      }
      tt <- paste0(tt, '</div></div>')

      list(
        value = list(dt, val),
        itemStyle = list(color = pt_color),
        tooltip_text = tt
      )
    })

    # Area gradient: line color at 15% -> 0% opacity top-to-bottom
    hex <- sub("^#", "", color)
    cr <- strtoi(substr(hex, 1, 2), 16L)
    cg <- strtoi(substr(hex, 3, 4), 16L)
    cb <- strtoi(substr(hex, 5, 6), 16L)
    area_gradient <- list(
      type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
      colorStops = list(
        list(offset = 0.05, color = sprintf("rgba(%d,%d,%d,0.15)", cr, cg, cb)),
        list(offset = 0.95, color = sprintf("rgba(%d,%d,%d,0)", cr, cg, cb))
      )
    )

    # Line series
    all_series <- c(all_series, list(list(
      type = "line",
      name = param,
      xAxisIndex = grid_idx,
      yAxisIndex = grid_idx,
      data = line_data,
      smooth = TRUE,
      lineStyle = list(color = color, width = 2.5),
      areaStyle = list(color = area_gradient),
      itemStyle = list(color = color),
      symbol = "none",
      silent = TRUE,
      z = 1,
      tooltip = list(show = FALSE)
    )))

    # Scatter series
    all_series <- c(all_series, list(list(
      type = "scatter",
      name = param,
      xAxisIndex = grid_idx,
      yAxisIndex = grid_idx,
      data = scatter_data,
      symbolSize = 8,
      z = 2,
      itemStyle = list(borderWidth = 2, borderColor = "#ffffff"),
      tooltip = list(
        formatter = htmlwidgets::JS(
          "function(params) { return params.data.tooltip_text || ''; }"
        )
      )
    )))

    # Reference bands
    if (has_ref) {
      ref_lo <- stats::median(p_data$A1LO, na.rm = TRUE)
      ref_hi <- stats::median(p_data$A1HI, na.rm = TRUE)
      if (!is.na(ref_lo) && !is.na(ref_hi)) {
        all_series <- c(all_series, list(list(
          type = "line",
          name = paste0(param, " ref"),
          xAxisIndex = grid_idx,
          yAxisIndex = grid_idx,
          data = list(),
          silent = TRUE,
          showSymbol = FALSE,
          lineStyle = list(opacity = 0),
          markArea = list(
            silent = TRUE,
            itemStyle = list(
              color = "rgba(5,150,105,0.06)",
              borderWidth = 1,
              borderColor = "rgba(5,150,105,0.15)",
              borderType = "dashed"
            ),
            data = list(list(list(yAxis = ref_lo), list(yAxis = ref_hi))),
            label = list(show = FALSE)
          )
        )))
      }
    }
  }

  echarts4r::e_charts(height = total_height) |>
    echarts4r::e_list(list(
      backgroundColor = "transparent",
      tooltip = pp_tooltip(),
      toolbox = pp_toolbox(),
      title = titles,
      legend = list(show = FALSE),
      grid = grids,
      xAxis = x_axes,
      yAxis = y_axes,
      series = all_series
    )) |>
    echarts4r::e_text_style(fontFamily = "system-ui, -apple-system, sans-serif")
}


# ---------------------------------------------------------------------------
# Viz Collector
# ---------------------------------------------------------------------------

#' Extract default settings from a viz's controls spec
#'
#' @param viz A viz definition with optional `controls` field
#' @return Named list of default values, or empty list if no controls
#' @noRd
pp_viz_defaults <- function(viz) {
  controls <- viz$controls
  if (is.null(controls)) return(list())
  lapply(controls, `[[`, "default")
}

#' Collect static (non-findings) patient profile visualization definitions
#'
#' Returns a named list of viz definitions for vizs that don't depend on
#' dynamic PARAMCD discovery. Findings vizs are generated by
#' pp_findings_vizs() at runtime.
#'
#' @return Named list of viz definitions
#' @noRd
patient_profile_static_vizs <- function() {
  vizs <- list(
    patient_overview_viz,
    ae_gantt_viz,
    cm_gantt_viz,
    adas_trajectory_viz,
    npix_radar_viz,
    ortho_bp_viz,
    questionnaire_heatmap_viz
  )
  stats::setNames(vizs, vapply(vizs, `[[`, character(1L), "id"))
}

# ---------------------------------------------------------------------------
# Per-group findings vizs
# ---------------------------------------------------------------------------

#' Define clinically meaningful findings parameter groups
#'
#' Each group specifies the source table, a unique ID, human-readable label,
#' the PARAMCDs it covers, and display metadata. These templates are matched
#' against the actual PARAMCDs in the dm data to produce per-group viz cards.
#'
#' @return List of group template lists
#' @noRd
pp_findings_groups <- function() {
  list(
    # --- Lab Chemistry (adlbc, or combined adlb with LBCAT=CHEMISTRY) ---
    # paramcds include common synonyms (e.g. ALP/ALKPH, K/POTAS, CHOL/CHOLES)
    # so the group matches whichever coding the sponsor used.
    list(
      table = "adlbc", id = "liver_panel", label = "Liver Panel",
      paramcds = c("ALT", "AST", "BILI", "GGT", "ALP", "ALKPH"),
      domain = "Laboratory", icon = "droplet", color = "#2563EB",
      description = "ALT, AST, Bilirubin, GGT, ALP"
    ),
    list(
      table = "adlbc", id = "renal_panel", label = "Renal Panel",
      paramcds = c("BUN", "CREAT", "URATE"),
      domain = "Laboratory", icon = "droplet", color = "#2563EB",
      description = "BUN, Creatinine, Urate"
    ),
    list(
      table = "adlbc", id = "electrolytes", label = "Electrolytes",
      paramcds = c("SODIUM", "K", "POTAS", "CL", "CA", "PHOS"),
      domain = "Laboratory", icon = "droplet", color = "#2563EB",
      description = "Sodium, Potassium, Chloride, Calcium, Phosphorus"
    ),
    list(
      table = "adlbc", id = "metabolic", label = "Metabolic",
      paramcds = c("GLUC", "CHOL", "CHOLES", "PROT", "ALB"),
      domain = "Laboratory", icon = "droplet", color = "#2563EB",
      description = "Glucose, Cholesterol, Protein, Albumin"
    ),
    list(
      table = "adlbc", id = "muscle_enzymes", label = "Muscle Enzymes",
      paramcds = c("CK"),
      domain = "Laboratory", icon = "droplet", color = "#2563EB",
      description = "Creatine Kinase"
    ),
    # --- Lab Hematology (adlbh, or combined adlb with LBCAT=HEMATOLOGY) ---
    list(
      table = "adlbh", id = "cbc", label = "CBC",
      paramcds = c("WBC", "RBC", "HGB", "HCT", "PLAT"),
      domain = "Laboratory", icon = "droplet-half", color = "#059669",
      description = "WBC, RBC, Hemoglobin, Hematocrit, Platelets"
    ),
    list(
      table = "adlbh", id = "rbc_indices", label = "RBC Indices",
      paramcds = c("MCV", "MCH", "MCHC"),
      domain = "Laboratory", icon = "droplet-half", color = "#059669",
      description = "MCV, MCH, MCHC"
    ),
    list(
      table = "adlbh", id = "wbc_differential", label = "WBC Differential",
      paramcds = c("LYM", "LYMPH", "MONO", "EOS", "BASO"),
      domain = "Laboratory", icon = "droplet-half", color = "#059669",
      description = "Lymphocytes, Monocytes, Eosinophils, Basophils"
    ),
    list(
      table = "adlbh", id = "rbc_morphology", label = "RBC Morphology",
      paramcds = c("ANISO", "MACROCY", "MACROC", "MICROCY", "MICROC",
                   "POIKILO", "POIKIL", "POLYCHR", "POLYCH"),
      domain = "Laboratory", icon = "droplet-half", color = "#059669",
      description = "Anisocytosis, Macrocytes, Microcytes, Poikilocytes, Polychromasia"
    ),
    # --- Vital Signs (advs) ---
    list(
      table = "advs", id = "blood_pressure", label = "Blood Pressure",
      paramcds = c("SYSBP", "DIABP"),
      domain = "Vitals", icon = "heart-pulse", color = "#D97706",
      description = "Systolic & Diastolic Blood Pressure"
    ),
    list(
      table = "advs", id = "pulse", label = "Pulse",
      paramcds = c("PULSE"),
      domain = "Vitals", icon = "heart-pulse", color = "#D97706",
      description = "Pulse rate"
    ),
    list(
      table = "advs", id = "temperature", label = "Temperature",
      paramcds = c("TEMP"),
      domain = "Vitals", icon = "heart-pulse", color = "#D97706",
      description = "Body Temperature"
    ),
    list(
      table = "advs", id = "anthropometrics", label = "Anthropometrics",
      paramcds = c("HEIGHT", "WEIGHT"),
      domain = "Vitals", icon = "heart-pulse", color = "#D97706",
      description = "Height & Weight"
    )
  )
}

#' Dynamically generate findings viz definitions from dm data
#'
#' For each findings table (adlbc, adlbh, advs) present in the dm:
#' 1. Extracts available PARAMCDs, filtering out change-from-baseline variants
#'    (those starting with "_")
#' 2. Creates a viz card for each pre-defined group that has matching PARAMCDs
#' 3. Creates individual viz cards for ungrouped PARAMCDs
#'
#' Each viz gets an "items" checkbox control to toggle individual params.
#'
#' @param dm_obj A dm object
#' @return Named list of viz definitions
#' @noRd
pp_findings_vizs <- function(dm_obj) {
  tbls <- dm::dm_get_tables(dm_obj)
  groups <- pp_findings_groups()

  # Table-level metadata for ungrouped params
  table_meta <- list(
    adlbc = list(
      domain = "Laboratory", icon = "droplet", color = "#2563EB",
      label_prefix = "Chemistry"
    ),
    adlbh = list(
      domain = "Laboratory", icon = "droplet-half", color = "#059669",
      label_prefix = "Hematology"
    ),
    adlb = list(
      domain = "Laboratory", icon = "droplet", color = "#2563EB",
      label_prefix = "Lab"
    ),
    advs = list(
      domain = "Vitals", icon = "heart-pulse", color = "#D97706",
      label_prefix = "Vitals"
    )
  )

  # Decide which real table sources which group set. Sponsors ship labs
  # either split (adlbc + adlbh) or combined (adlb) — support both. When
  # adlb is present, it fills in for whichever split tables are missing;
  # when BOTH splits are present, adlb still gets a plan of its own (with no
  # groups), so a PARAMCD living only in adlb yields an individual card
  # instead of silently getting no viz and no coverage entry.
  source_plans <- list()
  add_plan <- function(table, group_tables, meta) {
    source_plans[[length(source_plans) + 1L]] <<- list(
      table = table, group_tables = group_tables, meta = meta
    )
  }
  if ("advs" %in% names(tbls)) {
    add_plan("advs", "advs", table_meta$advs)
  }
  adlb_fills <- character(0)
  if ("adlbc" %in% names(tbls)) {
    add_plan("adlbc", "adlbc", table_meta$adlbc)
  } else if ("adlb" %in% names(tbls)) {
    adlb_fills <- c(adlb_fills, "adlbc")
  }
  if ("adlbh" %in% names(tbls)) {
    add_plan("adlbh", "adlbh", table_meta$adlbh)
  } else if ("adlb" %in% names(tbls)) {
    adlb_fills <- c(adlb_fills, "adlbh")
  }
  if ("adlb" %in% names(tbls)) {
    add_plan("adlb", adlb_fills, table_meta$adlb)
  }

  vizs <- list()
  # Params already covered by an earlier plan: an adlb param that also
  # lives in a split table must not get a duplicate card.
  covered_paramcds <- character(0)

  for (plan in source_plans) {
    tbl_name <- plan$table
    tbl <- as.data.frame(tbls[[tbl_name]])
    if (!"PARAMCD" %in% colnames(tbl)) next

    all_paramcds <- sort(unique(as.character(tbl$PARAMCD)))
    # Filter out change-from-baseline variants (start with "_")
    all_paramcds <- all_paramcds[!grepl("^_", all_paramcds)]
    if (length(all_paramcds) == 0) next

    grouped_paramcds <- character(0)
    meta <- plan$meta

    # Create viz for each matching group (groups may span multiple virtual
    # source tables when a combined adlb fills in for both adlbc and adlbh)
    tbl_groups <- Filter(function(g) g$table %in% plan$group_tables, groups)
    for (g in tbl_groups) {
      present <- intersect(g$paramcds, all_paramcds)
      if (length(present) == 0) next
      grouped_paramcds <- c(grouped_paramcds, present)

      viz_id <- g$id
      vizs[[viz_id]] <- new_pp_viz(
        id = viz_id,
        label = g$label,
        domain = g$domain,
        icon = g$icon,
        color = g$color,
        description = g$description,
        tables = tbl_name,
        requires = stats::setNames(
          list(c("PARAMCD", "AVAL", "ADT")),
          tbl_name
        ),
        optional = stats::setNames(
          list(c("PARAM", "ANRIND", "A1LO", "A1HI", "AVISITN")),
          tbl_name
        ),
        # Choices resolve at DISPATCH (choices_from + choices_subset), never
        # baked into the definition: a baked `present` made every definition
        # patient-specific, so a drilled-in upstream (single-patient input)
        # changed the catalog signature on every patient and the whole
        # sidebar re-rendered per drill. With data-independent definitions,
        # two patients sharing the same groups compare identical and the
        # sidebar stays put.
        controls = list(
          items = list(
            type = "checkbox",
            label = "Items",
            choices_from = "PARAMCD",
            choices_subset = g$paramcds,
            default = NULL
          )
        ),
        render = local({
          .tbl_name <- tbl_name
          .label <- g$label
          .color <- g$color
          .default_paramcds <- g$paramcds
          function(dm_obj, time_range, settings = list(),
                   ref_ms = NA_real_, mode = "date") {
            paramcds <- settings$items %||% .default_paramcds
            pp_render_findings(
              dm_obj, time_range,
              table_name = .tbl_name,
              label = .label,
              base_color = .color,
              paramcds = paramcds,
              ref_ms = ref_ms, mode = mode
            )
          }
        })
      )
    }

    # Create individual viz cards for ungrouped PARAMCDs (skipping params an
    # earlier plan already covers, e.g. an adlb param that also lives in a
    # split table)
    ungrouped <- setdiff(all_paramcds, c(grouped_paramcds, covered_paramcds))
    for (pc in ungrouped) {
      viz_id <- paste0(tbl_name, "_", tolower(pc))
      vizs[[viz_id]] <- new_pp_viz(
        id = viz_id,
        label = paste0(meta$label_prefix, ": ", pc),
        domain = meta$domain,
        icon = meta$icon,
        color = meta$color,
        description = pc,
        tables = tbl_name,
        requires = stats::setNames(
          list(c("PARAMCD", "AVAL", "ADT")),
          tbl_name
        ),
        optional = stats::setNames(
          list(c("PARAM", "ANRIND", "A1LO", "A1HI", "AVISITN")),
          tbl_name
        ),
        render = local({
          .tbl_name <- tbl_name
          .pc <- pc
          .color <- meta$color
          function(dm_obj, time_range, settings = list(),
                   ref_ms = NA_real_, mode = "date") {
            pp_render_findings(
              dm_obj, time_range,
              table_name = .tbl_name,
              label = .pc,
              base_color = .color,
              paramcds = .pc,
              ref_ms = ref_ms, mode = mode
            )
          }
        })
      )
    }

    covered_paramcds <- c(covered_paramcds, grouped_paramcds, ungrouped)
  }

  vizs
}
