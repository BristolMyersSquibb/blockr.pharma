# The panels' data tooltips.
#
# A tooltip is built here, in R, as a small list riding on the data point
# (`tip` beside `value`), and drawn by one renderer, PatientProfile.tip() in
# pp-core.js. The content is then testable where it is decided, and the look
# lives in one place for every chart.
#
# The look is blockr.viz's (chart.js tipHead / tipRow, the design system's
# chart tooltip): a 13px/600 headline with a 10px swatch in the colour of
# the thing pointed at, then 12px rows with the name muted on the left and
# the value on the right at 500 with tabular figures. Names, not codes;
# days as the timeline shows them, with the other form as muted meta; spans
# written From / To with the duration after the end, one day as "Day".
#
# pp_tip()          -- one tooltip
# pp_tip_row()      -- one row, or NULL when there is nothing to say
# pp_tip_when()     -- a record's day and date, main and meta by timeline mode
# pp_tip_span()     -- the From / To (or Day) rows of an interval
# pp_tip_case()     -- sentence case for a study's capitals
# pp_tip_num()      -- a measured value, three significant figures
# PP_TIP_FORMATTER  -- the ECharts formatter that draws them

#' One data tooltip
#'
#' @param head The headline: what is pointed at, in words.
#' @param color The swatch in front of the headline (a data colour), or
#'   `NULL` for none.
#' @param sub A muted line under the headline (the body system under an
#'   adverse event), or `NULL`.
#' @param rows Rows from [pp_tip_row()] / [pp_tip_span()]; `NULL`s drop out.
#' @param note A muted closing sentence, or `NULL`.
#' @return A list for the data point's `tip` entry.
#' @noRd
pp_tip <- function(head, color = NULL, sub = NULL, rows = list(),
                   note = NULL) {
  rows <- Filter(Negate(is.null), rows)
  out <- list(head = pp_tip_str(head), rows = unname(rows))
  if (length(color) && !is.na(color) && nzchar(color)) out$color <- color
  sub <- pp_tip_str(sub)
  if (nzchar(sub)) out$sub <- sub
  note <- pp_tip_str(note)
  if (nzchar(note)) out$note <- note
  out
}

#' One tooltip row
#'
#' @param label The name, muted, on the left.
#' @param value The value, on the right.
#' @param meta Muted text after the value (the date after a day, the
#'   duration after an end), or `NULL`.
#' @return A row, or `NULL` when the value is missing or blank.
#' @noRd
pp_tip_row <- function(label, value, meta = NULL) {
  value <- pp_tip_str(value)
  if (!nzchar(value)) return(NULL)
  row <- list(label = label, value = value)
  meta <- pp_tip_str(meta)
  if (nzchar(meta)) row$meta <- meta
  row
}

#' A record's day and date, as the timeline shows them
#'
#' The timeline's own form is the value and the other one is muted meta: in
#' relative-day mode "D32" with the date after it, in date mode the date
#' with the day after it. The study's own day wins over one derived from the
#' date, the rule the axis follows.
#'
#' @param date The record's date, or `NA`.
#' @param day The record's study day, or `NA`.
#' @param ref_ms The patient's reference timestamp, for deriving a day.
#' @param mode `"rday"` or `"date"`.
#' @return `list(main, meta, day, date)`; `main` is `""` when neither is known.
#' @noRd
pp_tip_when <- function(date = NA, day = NA, ref_ms = NA_real_,
                        mode = "date") {
  date <- if (length(date) && !is.na(date)) as.Date(date) else NA
  day_lab <- if (length(day) && !is.na(day)) {
    paste0("D", day)
  } else if (!is.na(date) && !is.na(ref_ms)) {
    pp_xlabel(date, ref_ms, "rday")
  } else {
    ""
  }
  date_lab <- if (!is.na(date)) format(date) else ""
  if (identical(mode, "rday")) {
    main <- if (nzchar(day_lab)) day_lab else date_lab
    meta <- if (nzchar(day_lab)) date_lab else ""
  } else {
    main <- if (nzchar(date_lab)) date_lab else day_lab
    meta <- if (nzchar(date_lab)) day_lab else ""
  }
  list(main = main, meta = meta, day = day, date = date)
}

#' The rows of an interval
#'
#' "From" and "To" with the duration after the end, as blockr.viz's timeline
#' writes a span; a one-day span is a single "Day" row, and an open end
#' reads "ongoing".
#'
#' @param from,to [pp_tip_when()] results for the two ends.
#' @param open Whether the end is missing (ongoing).
#' @param end_inside Whether the end day belongs to the span. An event's
#'   last day does; a response held "until" the next assessment does not,
#'   since that day is the next response's first.
#' @return A list of rows.
#' @noRd
pp_tip_span <- function(from, to, open = FALSE, end_inside = TRUE) {
  if (!nzchar(from$main)) return(list())
  if (isTRUE(open)) {
    return(list(pp_tip_row("From", from$main, from$meta),
                pp_tip_row("To", PP_ONGOING_LABEL)))
  }
  if (!nzchar(to$main) || identical(from$main, to$main)) {
    return(list(pp_tip_row("Day", from$main, from$meta)))
  }
  list(pp_tip_row("From", from$main, from$meta),
       pp_tip_row("To", to$main, pp_tip_days(from, to, end_inside)))
}

#' A span's length, counting the end day when it belongs to the span
#' @noRd
pp_tip_days <- function(from, to, end_inside = TRUE) {
  n <- if (!is.na(from$date) && !is.na(to$date)) {
    as.numeric(to$date - from$date) + 1
  } else if (!is.na(from$day) && !is.na(to$day)) {
    # Study days skip zero: D-1 to D1 is two days, not three.
    to$day - from$day + 1 - (from$day < 0 && to$day > 0)
  } else {
    NA
  }
  if (!end_inside) n <- n - 1
  if (is.na(n) || n < 1) return("")
  paste(n, if (n == 1) "day" else "days")
}

#' Sentence case for a study's capitals
#'
#' ADaM text columns often arrive in capitals ("NOT RECOVERED/NOT
#' RESOLVED"); a tooltip speaks in sentence case. Text the study wrote in
#' mixed case is left as written.
#' @noRd
pp_tip_case <- function(x) {
  x <- pp_tip_str(x)
  if (!nzchar(x) || x != toupper(x)) return(x)
  paste0(substr(x, 1L, 1L), tolower(substring(x, 2L)))
}

#' A measured value, as a tooltip prints it
#'
#' blockr.viz's rule for a raw observation glanced at on hover (chart.js
#' ddNum3): integers whole, anything else to three significant figures.
#' `signed` puts a "+" on a positive change.
#' @noRd
pp_tip_num <- function(x, signed = FALSE) {
  if (!length(x) || is.na(x[[1L]]) || !is.numeric(x)) return(pp_tip_str(x))
  x <- x[[1L]]
  out <- if (x == round(x)) {
    format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  } else {
    format(signif(x, 3), big.mark = ",", scientific = FALSE, trim = TRUE,
           drop0trailing = TRUE)
  }
  if (signed && x > 0) paste0("+", out) else out
}

#' One string, or ""
#' @noRd
pp_tip_str <- function(x) {
  if (!length(x) || is.na(x[[1L]])) return("")
  trimws(as.character(x[[1L]]))
}

#' The formatter every panel's tooltip uses
#' @noRd
PP_TIP_FORMATTER <- htmlwidgets::JS(
  "function(p) { return PatientProfile.tip(p); }"
)
