# The panels' data tooltips.
#
# A tooltip is built here, in R, as a small list riding on the data point
# (`tip` beside `value`), and drawn by one renderer, PatientProfile.tip() in
# pp-core.js. The content is then testable where it is decided, and the look
# lives in one place for every chart.
#
# The look and the wording are the patient timeline's (blockr.viz's table
# tooltip, tooltip.css .vz-tt-*): a 13px/600 headline with a 10px swatch in
# the colour of the thing pointed at, a muted line naming that colour's
# level, then 12px "Name: value" rows with the name muted on the left and
# the value on the right at 500 with tabular figures. An event says its
# Type, its Details and its Days ("Day 16 to 168", the duration as muted
# meta), as the timeline's segments do. Only the hovered item's own fields.
#
# pp_tip()          -- one tooltip
# pp_tip_row()      -- one row, or NULL when there is nothing to say
# pp_tip_when()     -- a record's day and date, main and meta by timeline mode
# pp_tip_span()     -- the Days row of an interval
# pp_tip_day()      -- the Days row of a point
# pp_tip_case()     -- sentence case for a study's capitals
# pp_tip_num()      -- a measured value, three significant figures
# pp_tip_name()     -- a column's label, else the tooltip's word for it
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
  list(main = main, meta = meta, day = day, date = date, mode = mode)
}

# A day, as the timeline writes it after "Day": "D16" read as "16".
pp_tip_day_num <- function(when) sub("^D", "", when$main)

#' The Days row of a point
#'
#' "Days: Day 15" with the date as muted meta in relative-day mode, "Date"
#' with the day after it in date mode.
#'
#' @param when A [pp_tip_when()] result.
#' @return A row, or `NULL`.
#' @noRd
pp_tip_day <- function(when) {
  if (!nzchar(when$main)) return(NULL)
  if (pp_tip_is_day(when)) {
    pp_tip_row("Days", paste("Day", pp_tip_day_num(when)), when$meta)
  } else {
    pp_tip_row("Date", when$main, when$meta)
  }
}

# Whether a when's main form is a study day.
pp_tip_is_day <- function(when) grepl("^D-?[0-9]", when$main)

#' The Days row of an interval
#'
#' One row, as the timeline writes a segment's days: "Day 16 to 168", "Day
#' 44 to ongoing", "Day 10" for one day, with the duration as muted meta. In
#' date mode the row is "Dates" and reads the dates the same way.
#'
#' @param from,to [pp_tip_when()] results for the two ends.
#' @param open Whether the end is missing (ongoing).
#' @param end_inside Whether the end day belongs to the span. An event's
#'   last day does; a response held "until" the next assessment does not,
#'   since that day is the next response's first.
#' @param after Text after the days, as the timeline's ", from 54 mg".
#' @return A list of one row, or an empty list.
#' @noRd
pp_tip_span <- function(from, to, open = FALSE, end_inside = TRUE,
                        after = NULL) {
  if (!nzchar(from$main)) return(list())
  day <- pp_tip_is_day(from)
  lab <- function(w) if (day) pp_tip_day_num(w) else w$main
  single <- !isTRUE(open) && (!nzchar(to$main) || identical(from$main, to$main))
  value <- if (isTRUE(open)) {
    paste(lab(from), "to", PP_ONGOING_LABEL)
  } else if (single) {
    lab(from)
  } else {
    paste(lab(from), "to", lab(to))
  }
  if (day) value <- paste("Day", value)
  after <- pp_tip_str(after)
  if (nzchar(after)) value <- paste0(value, ", ", after)
  meta <- if (isTRUE(open) || single) "" else pp_tip_days(from, to, end_inside)
  label <- if (day) "Days" else if (single) "Date" else "Dates"
  list(pp_tip_row(label, value, meta))
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

#' What a tooltip row calls a column
#'
#' The column's variable label when the study gave one ("Analysis Value",
#' "Standard Toxicity Grade"), else the tooltip's own word for it. Read off
#' the panel's whole table: a one-row slice of a data.frame has lost its
#' labels.
#'
#' @param data The table the column is in, or `NULL`.
#' @param col The column name.
#' @param word The word to use without a label.
#' @noRd
pp_tip_name <- function(data, col, word) {
  pp_tip_label(data, col) %||% word
}

#' The severity row: the column's label and the study's value ("Standard
#' Toxicity Grade" 3), else "Severity" and the value in words ("Grade 3").
#' @noRd
pp_tip_sev <- function(data, col, sev) {
  lbl <- pp_tip_label(data, col)
  if (!is.null(lbl)) {
    pp_tip_row(lbl, pp_tip_case(sev))
  } else {
    pp_tip_row("Severity", pp_ae_sev_word(sev))
  }
}

#' A column's variable label, or `NULL`
#' @noRd
pp_tip_label <- function(data, col) {
  if (is.null(data) || is.null(col) || !col %in% names(data)) return(NULL)
  lbl <- attr(data[[col]], "label", exact = TRUE)
  if (!is.character(lbl) || length(lbl) != 1L || is.na(lbl)) return(NULL)
  lbl <- trimws(lbl)
  if (!nzchar(lbl) || identical(lbl, col)) NULL else lbl
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

#' An event's tooltip, in the patient timeline's parts
#'
#' The headline and swatch, a muted line naming the colour's level ("Severe",
#' "Started on treatment"), then "Type", "Details" and any rows of the
#' event's own, and the Days row last, as a timeline segment reads.
#'
#' @param head,color The headline and its swatch.
#' @param level What the colour stands for, or `NULL`.
#' @param type "Adverse event", "Con med", "Treatment", "Visit".
#' @param details Parts of the Details row; empty ones drop.
#' @param days The [pp_tip_span()] or [pp_tip_day()] row(s).
#' @param rows Further rows, between Details and Days.
#' @return A tooltip, as [pp_tip()].
#' @noRd
pp_tip_event <- function(head, color = NULL, level = NULL, type = NULL,
                         details = character(), days = list(),
                         rows = list()) {
  details <- vapply(details, pp_tip_str, "", USE.NAMES = FALSE)
  details <- details[nzchar(details)]
  if (!is.list(days) || !is.null(days$label)) days <- list(days)
  pp_tip(
    head, color = color, sub = level,
    rows = c(
      list(pp_tip_row("Type", type),
           pp_tip_row("Details", paste(details, collapse = ", "))),
      rows, days
    )
  )
}

#' An AE's tooltip: severity (and seriousness) as the level, body system and
#' outcome as the details.
#' @noRd
pp_tip_ae <- function(term, color, sev = NULL, serious = FALSE,
                      bodsys = NULL, outcome = NULL, days = list()) {
  sev <- pp_tip_str(sev)
  level <- c(if (nzchar(sev)) pp_ae_sev_word(sev),
             if (isTRUE(serious)) "serious")
  level <- paste(level, collapse = ", ")
  if (nzchar(level)) {
    level <- paste0(toupper(substr(level, 1L, 1L)), substring(level, 2L))
  }
  pp_tip_event(
    pp_tip_case(term), color = color, level = level, type = "Adverse event",
    details = c(pp_tip_case(bodsys), pp_tip_case(outcome)), days = days
  )
}

#' A con med's tooltip: started before or on treatment as the level, the
#' indication and class as the details, then the dose.
#' @noRd
pp_tip_cm <- function(name, color, on = TRUE, indication = NULL,
                      klass = NULL, dose = NULL, reported = NULL,
                      days = list()) {
  indication <- pp_tip_str(indication)
  pp_tip_event(
    name, color = color,
    level = if (isTRUE(on)) "Started on treatment" else
      "Started before treatment",
    type = "Con med",
    details = c(if (nzchar(indication)) paste("For", tolower(indication)),
                pp_tip_case(klass)),
    rows = list(pp_tip_row("Dose", dose),
                pp_tip_row("Reported as", pp_tip_case(reported))),
    days = days
  )
}
