#' Exposure records as drawn: the records, nothing inferred
#'
#' The patient profile and the patient timeline draw a drug's ADEX records
#' the same way. A record with a start and an end is a line from one to the
#' other. A one-day record (start = end) is one administration, drawn as a
#' ring on its day. Records of one drug at the same dose that touch or
#' overlap (a daily dose recorded day by day, a period split in two) join
#' into one line. Nothing else is joined: no interval is read off the gaps,
#' so a drug given every 21 days is a ring per dose.
#'
#' @param ex A data frame: `USUBJID`, `drug`, `start`, `end` (numbers on one
#'   day scale; an open record's end equals its start), `dose` (`NA`
#'   without), `unit` and `open`.
#' @return A data frame, one row per line, in order of patient, drug and
#'   start: `USUBJID`, `drug`, `start`, `end`, `open`, `dot` (one day),
#'   `n` (records joined), `daily` (one-day records joined), `txt` (the dose
#'   as printed), `prev` (the dose of the drug's line before, `""` on its
#'   first) and `changed` (the dose differs from `prev`).
#' @noRd
pp_ex_runs <- function(ex) {
  ex <- ex[!is.na(ex$start), , drop = FALSE]
  if (!nrow(ex)) return(NULL)
  ex$unit[is.na(ex$unit)] <- ""
  ex <- ex[order(ex$USUBJID, ex$drug, ex$start, ex$end, method = "radix"), ,
           drop = FALSE]
  # One administration per drug and day: ADaM ADEX repeats each record once
  # per parameter, and a study may record one dose in two units (800 mg,
  # 10 mg/kg). The first row stays and carries the other doses in its unit,
  # so the tooltip reads "800 mg · 10 mg/kg".
  key <- paste(ex$USUBJID, ex$drug, ex$start, ex$end, sep = "\r")
  txt <- swimmer_fmt_dose(ex$dose, ex$unit)
  for (k in unique(key[duplicated(key)])) {
    i <- which(key == k)
    more <- setdiff(unique(txt[i[-1L]]), c("", txt[i[[1L]]]))
    if (length(more)) {
      ex$unit[i[[1L]]] <- paste(c(ex$unit[i[[1L]]], more),
                                collapse = " \u00b7 ")
    }
  }
  ex <- ex[!duplicated(key), , drop = FALSE]
  txt <- swimmer_fmt_dose(ex$dose, ex$unit)
  m <- nrow(ex)
  grp <- paste(ex$USUBJID, ex$drug, sep = "\r")
  first <- c(TRUE, grp[-1L] != grp[-m])
  # The furthest end so far within the drug, for "touches or overlaps".
  reach <- stats::ave(ex$end, cumsum(first), FUN = cummax)
  prev_reach <- c(-Inf, reach[-m])
  prev_open <- c(FALSE, ex$open[-m])
  new <- first | ex$start > prev_reach + 1 | txt != c("", txt[-m]) |
    prev_open | ex$open
  run <- cumsum(new)
  rf <- which(new)
  one_day <- ex$end == ex$start & !ex$open
  n <- tabulate(run, length(rf))
  out <- data.frame(
    USUBJID = ex$USUBJID[rf], drug = ex$drug[rf],
    start = ex$start[rf],
    end = as.vector(tapply(ex$end, run, max)),
    open = ex$open[rf],
    n = n,
    daily = n > 1L & as.vector(tapply(one_day, run, all)),
    txt = txt[rf],
    stringsAsFactors = FALSE
  )
  out$dot <- out$end == out$start & !out$open
  g <- paste(out$USUBJID, out$drug, sep = "\r")
  k <- nrow(out)
  same <- c(FALSE, g[-1L] == g[-k])
  out$prev <- ifelse(same, c("", out$txt[-k]), "")
  out$changed <- same & nzchar(out$txt) & nzchar(out$prev) &
    out$txt != out$prev
  out
}

#' One patient's drug lines for the profile's treatment lane
#'
#' [pp_ex_runs()] on the patient's ADEX, on the axis the caller reads ADEX
#' on: continuous study days ([pp_day_to_x()]) when `use_day`, else days
#' since 1970 (a Date as a number).
#'
#' @param adex One patient's ADEX rows with a start.
#' @param sl The patient's ADSL row (`TRTEDT`, `TRTSDT`).
#' @param use_day Read `ASTDY`/`AENDY` instead of `ASTDT`/`AENDT`.
#' @return A data frame, one row per line: `drug` (ADEX `EXTRT`, trimmed),
#'   `name` (as the timeline names it), `u0`, `u1` (start and end on the
#'   axis above), `open`, `dot`, `details` (the drug and its dose) and
#'   `after` (the dose before, where it changed). `NULL` without any.
#' @noRd
pp_drug_lines <- function(adex, sl, use_day) {
  n <- nrow(adex)
  if (!n) return(NULL)
  col <- function(nm) if (nm %in% colnames(adex)) adex[[nm]] else rep(NA, n)
  num_date <- function(v) as.numeric(swimmer_date(v) %||% rep(NA, length(v)))
  ts <- num_date(sl$TRTSDT[1L])
  te <- num_date(sl$TRTEDT[1L])
  if (use_day) {
    s <- pp_day_to_x(col("ASTDY"))
    e <- pp_day_to_x(col("AENDY"))
    te <- te - ts + 1
  } else {
    s <- num_date(col("ASTDT"))
    e <- num_date(col("AENDT"))
  }
  raw <- trimws(as.character(col("EXTRT")))
  raw[is.na(raw)] <- ""
  # A zero dose (placebo) is no dose to show.
  dose <- suppressWarnings(as.numeric(col("EXDOSE")))
  dose[!is.na(dose) & dose == 0] <- NA
  # A record without an end stops at the last dose (swimmer_close_study()),
  # and stays open only while the patient has none.
  fix <- is.na(e) & !is.na(te)
  e[fix] <- pmax(s[fix], te)
  open <- is.na(e)
  e[open] <- s[open]
  r <- pp_ex_runs(data.frame(
    USUBJID = "p", drug = raw, start = s, end = e, dose = dose,
    unit = as.character(col("EXDOSU")), open = open,
    stringsAsFactors = FALSE
  ))
  if (is.null(r)) return(NULL)
  name <- ifelse(nzchar(r$drug), swimmer_drug_name(r$drug), "Exposure")
  data.frame(
    drug = r$drug, name = name, u0 = r$start, u1 = r$end,
    open = r$open, dot = r$dot,
    details = paste0(name, ifelse(nzchar(r$txt), paste0(", ", r$txt), ""),
                     ifelse(r$daily, " daily", "")),
    after = ifelse(r$changed, paste("from", r$prev), ""),
    stringsAsFactors = FALSE
  )
}

#' A drug line's end as the profile's tooltip reads it
#'
#' @param u A line's start or end from [pp_drug_lines()].
#' @inheritParams pp_drug_lines
#' @return A [pp_tip_when()] value.
#' @noRd
pp_drug_line_when <- function(u, use_day, ref_ms, mode) {
  if (use_day) {
    pp_tip_when(NA, if (u >= 1) u else u - 1, ref_ms, mode)
  } else {
    pp_tip_when(as.Date(u, origin = "1970-01-01"), NA, ref_ms, mode)
  }
}

#' A drug line's start or end on the profile's x axis
#'
#' @inheritParams pp_drug_line_when
#' @noRd
pp_drug_line_x <- function(u, use_day, ref_ms, mode) {
  if (use_day) return(u)
  pp_xval(as.Date(u, origin = "1970-01-01"), ref_ms, mode)
}
