# Treatment cycles: turning "CYCLE 7 DAY 1" into dates you can measure from.
#
# WHY ANCHORS AND NOT A COLUMN. Oncology is dosed in repeating cycles, and
# clinicians read the timeline in cycle/day ("C7 D1") because that is how the
# protocol is written. There is no CDISC-standard cycle variable; studies put
# the vocabulary in VISIT ("CYCLE 2 DAY 1"). But a visit label only covers
# scheduled assessments: ADAE carries no VISIT at all, and an AE starts on
# whatever day it starts. So a per-row label cannot answer the question that
# matters ("did this AE land just after an infusion?").
#
# What generalizes is the CYCLE START DATE. Read the DAY 1 rows as per-subject
# anchors and any date converts by subtracting the nearest preceding one --
# for AEs, con-meds, labs, anything. It also tracks reality: cycles get held
# for toxicity, so the nominal grid
#
#     cycle = floor((ADY - 1) / 21) + 1
#
# drifts out of phase exactly for the patients whose delays you care about.
# Two cycles held a week each and the arithmetic says C6D15 where the CRF says
# C7D1. Anchors are observed dates; they cannot drift.
#
# WHY D1 WINS AND IS NOT AVERAGED. A cycle holding both a D1 row and a D8 row
# offers two routes to the same start date, and they disagree when a visit
# slips (drawn D9, still labelled D8). The D1 row is the fact; the slipped D8
# is an artifact. Averaging them splits the difference between a right answer
# and a wrong one, so back-calculation is strictly a FALLBACK for a missing
# D1, and says so via `estimated`.
#
# Measured against real study data before any of this was written: the large
# majority of cycles carry a real D1 row, and where both exist the back-calc
# agrees with it closely -- i.e. the labels track the dates, with ordinary
# visit slippage in the tail. The fallback is an edge case, not the common
# path. Re-run the diagnostic per study rather than trusting these shapes;
# a study with sparser D1 coverage would make the fallback load-bearing.

#' The cycle/day vocabulary, as it appears in VISIT
#'
#' Permissive on separators and case; not anchored, so a trailing qualifier
#' ("CYCLE 1 DAY 1 PRE-DOSE") still parses.
#' @noRd
PP_CYCLE_PATTERN <- "CYCLE[ _]*([0-9]+)[ _]+DAY[ _]*(-?[0-9]+)"

#' Parse cycle and day out of visit labels
#'
#' @param visit Character vector of visit labels.
#' @return Data frame with integer `cycle` and `day`; `NA` where the label
#'   carries no cycle vocabulary (screening, unscheduled, end of treatment).
#' @noRd
pp_parse_cycle_visits <- function(visit) {
  visit <- as.character(visit)
  out <- data.frame(
    cycle = rep(NA_integer_, length(visit)),
    day   = rep(NA_integer_, length(visit))
  )
  if (!length(visit)) return(out)
  parts <- regmatches(visit, regexec(PP_CYCLE_PATTERN, visit, ignore.case = TRUE))
  ok <- lengths(parts) == 3L
  if (!any(ok)) return(out)
  out$cycle[ok] <- as.integer(vapply(parts[ok], `[`, "", 2L))
  out$day[ok] <- as.integer(vapply(parts[ok], `[`, "", 3L))
  out
}

#' Per-subject treatment cycle anchors
#'
#' Total on purpose: renders and reactives call this, so a study without the
#' cycle vocabulary yields `NULL` (no cycle lane, no cycle labels) rather than
#' a condition. Reads canonical names -- run [pp_normalize_dm()] first.
#'
#' @param dm_obj A normalized `dm`, subject-scoped or not.
#' @param table,visit_col,date_col Where the vocabulary lives. The lab table
#'   is the default because it is where studies schedule the D1/D8/D15 visits
#'   that make the anchors dense.
#' @return Data frame of `USUBJID`, `cycle`, `cycle_start`, `cycle_end`,
#'   `estimated`, ordered by subject and cycle; `NULL` when nothing parses.
#' @noRd
pp_cycle_anchors <- function(dm_obj, table = "adlb", visit_col = "AVISIT",
                             date_col = "ADT") {
  if (!inherits(dm_obj, "dm")) return(NULL)
  tbls <- dm::dm_get_tables(dm_obj)
  if (!table %in% names(tbls)) return(NULL)
  df <- as.data.frame(tbls[[table]])
  if (!all(c("USUBJID", visit_col, date_col) %in% colnames(df))) return(NULL)

  parsed <- pp_parse_cycle_visits(df[[visit_col]])
  dt <- pp_as_date(df[[date_col]])
  keep <- !is.na(parsed$cycle) & !is.na(parsed$day) & !is.na(dt)
  if (!any(keep)) return(NULL)

  d <- data.frame(
    USUBJID = as.character(df$USUBJID)[keep],
    cycle   = parsed$cycle[keep],
    dn      = as.numeric(dt[keep]),
    stringsAsFactors = FALSE
  )
  # Back-calculated start implied by each row: a D8 row drawn on the 1st puts
  # the cycle start seven days earlier.
  d$est <- d$dn - (parsed$day[keep] - 1)
  is_d1 <- parsed$day[keep] == 1L

  a_est <- stats::aggregate(est ~ USUBJID + cycle, data = d, FUN = min)
  a_d1 <- if (any(is_d1)) {
    stats::aggregate(dn ~ USUBJID + cycle, data = d[is_d1, , drop = FALSE],
                     FUN = min)
  } else {
    data.frame(USUBJID = character(), cycle = integer(), dn = numeric(),
               stringsAsFactors = FALSE)
  }

  out <- merge(a_est, a_d1, by = c("USUBJID", "cycle"), all.x = TRUE)
  out$estimated <- is.na(out$dn)
  out$cycle_start <- as.Date(
    ifelse(out$estimated, out$est, out$dn), origin = "1970-01-01"
  )
  out <- out[order(out$USUBJID, out$cycle), , drop = FALSE]

  # A cycle runs until the next one starts. The LAST cycle has no successor,
  # so it gets the study's typical span -- without that bound, a death six
  # months after the final dose would be labelled "C7 D190".
  span <- pp_cycle_span(out)
  out$cycle_end <- as.Date(unlist(lapply(
    split(as.numeric(out$cycle_start), out$USUBJID),
    function(st) c(st[-1] - 1, st[length(st)] + span - 1)
  ), use.names = FALSE), origin = "1970-01-01")

  out <- out[, c("USUBJID", "cycle", "cycle_start", "cycle_end", "estimated")]
  rownames(out) <- NULL
  out
}

#' The study's typical cycle length, in days
#'
#' Measured, not assumed: a protocol's nominal 21 days is the modal gap, but
#' the data is the authority and some studies run 14 or 28. `default` covers
#' the degenerate case where no subject reached a second cycle.
#'
#' @param anchors A `pp_cycle_anchors()` frame (needs `cycle_start`).
#' @param default Fallback span when no gap is measurable.
#' @return A single positive number of days.
#' @noRd
pp_cycle_span <- function(anchors, default = 21) {
  if (is.null(anchors) || !nrow(anchors)) return(default)
  gaps <- unlist(lapply(
    split(as.numeric(anchors$cycle_start), anchors$USUBJID),
    function(x) if (length(x) < 2L) numeric(0) else diff(sort(x))
  ), use.names = FALSE)
  gaps <- gaps[!is.na(gaps) & gaps > 0]
  if (!length(gaps)) return(default)
  # Rounded: an even number of gaps medians to a half day, and "the typical
  # cycle runs 24.5 days" is not a thing anyone means.
  round(stats::median(gaps))
}

#' Add relative-day positions to the anchors
#'
#' The anchors carry real dates; the labels have to work for rows that ship a
#' native \*DY and no date at all. Converting the ANCHOR into day space (once,
#' against the same TRTSDT the axis uses) lets both sides meet without ever
#' reconstructing a date from a day -- the lossy round trip
#' [pp_xval_pref_day()] exists to avoid.
#'
#' Cycles never precede treatment start, so the continuous relative-day scale
#' and the ADaM \*DY convention coincide over their whole range and no
#' [pp_day_to_x()] correction is needed here.
#'
#' @param anchors A `pp_cycle_anchors()` frame, or `NULL`.
#' @param ref_ms Reference timestamp in ms (TRTSDT). May be `NA`, in which
#'   case the day columns come back `NA` and only the dates stay usable.
#' @return The frame plus `cycle_start_day` / `cycle_end_day`, or `NULL`.
#' @noRd
pp_cycle_anchor_days <- function(anchors, ref_ms = NA_real_) {
  if (is.null(anchors) || !nrow(anchors)) return(NULL)
  if (is.na(ref_ms)) {
    anchors$cycle_start_day <- NA_real_
    anchors$cycle_end_day <- NA_real_
    return(anchors)
  }
  anchors$cycle_start_day <- pp_xval(anchors$cycle_start, ref_ms, "rday")
  anchors$cycle_end_day <- pp_xval(anchors$cycle_end, ref_ms, "rday")
  anchors
}

#' Cycle/day label for one position on the relative-day axis
#'
#' @param x_day A single position on the relative-day scale.
#' @param anchors A [pp_cycle_anchor_days()] frame for ONE subject, or `NULL`.
#' @return `"C7 D4"`, or `""` when there is no cycle covering `x_day`
#'   (screening, or long after the last dose).
#' @noRd
pp_cycle_label <- function(x_day, anchors) {
  if (is.null(anchors) || !nrow(anchors)) return("")
  if (length(x_day) != 1L || is.na(x_day)) return("")
  if (!"cycle_start_day" %in% colnames(anchors)) return("")
  hit <- which(
    !is.na(anchors$cycle_start_day) & anchors$cycle_start_day <= x_day &
      !is.na(anchors$cycle_end_day) & anchors$cycle_end_day >= x_day
  )
  if (!length(hit)) return("")
  j <- hit[which.max(anchors$cycle_start_day[hit])]
  paste0("C", anchors$cycle[j], " D",
         x_day - anchors$cycle_start_day[j] + 1)
}

#' Append a cycle/day label to a timeline label
#'
#' The "in addition to" of the original request: the study day or date stays
#' the label, and the cycle rides in parentheses behind it. Never replaces --
#' `C7 D1` alone loses the ability to compare across patients.
#'
#' @param base An already-formatted label (`"D143"`, `"2014-05-01"`).
#' @param x_day Position on the relative-day scale, or `NA`.
#' @param anchors A [pp_cycle_anchor_days()] frame, or `NULL` for no-op.
#' @return `base`, possibly with `" (C7 D4)"` appended.
#' @noRd
pp_with_cycle <- function(base, x_day, anchors = NULL) {
  if (is.null(anchors) || !nzchar(base)) return(base)
  lab <- pp_cycle_label(x_day, anchors)
  if (!nzchar(lab)) return(base)
  paste0(base, " (", lab, ")")
}
