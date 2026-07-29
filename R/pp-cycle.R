# Treatment cycles: the band, and why no label is computed from it.
#
# ONE RULE. A record's timepoint is the string its VISIT says, printed as the
# study wrote it: "CYCLE 2 DAY 1", "UNSCHEDULED", "WEEK 24", whatever it is.
# The profile does not parse it, shorten it, renumber it or decide what it
# means. To this package a visit label is text (pp_with_visit()), and it rides
# behind the date the row also carries.
#
# That applies to dosing and findings rows, where the visit IS that row's
# timepoint. Events are left out: an AE has an onset date of its own, and a
# study that puts AVISIT on its occurrence dataset means the visit the AE was
# collected at, which can be well after onset. So AE and con-med bars show the
# date and stop, rather than borrowing a label that describes something else.
#
# WHY SO LITERAL. The profile used to COMPUTE a cycle/day for every row from
# per-subject anchors, and clinical review caught it: doses given on the
# protocol's Day 1 were labelled D2, D3, D4. The anchors were built from lab
# dates, safety labs for a "CYCLE n DAY 1" visit are drawn BEFORE the infusion,
# and so the whole cycle read a lead time late. The dosing row said "CYCLE 2
# DAY 1" the entire time. Every correction available for that class of error
# (prefer the dosing table, snap a lab anchor onto the nearest infusion,
# reformat the label into C2D1) adds another rule that can be wrong in a study
# we have not seen. Printing the string cannot be.
#
# WHAT THE PARSE IS STILL FOR. Exactly one thing: the cycle BAND
# (viz-cycle.R). A band is an interval and no CDISC variable holds a cycle
# start or end, so it has to be read out of the D1 rows, which means reading
# the vocabulary. That is one lane, deletable in one file, and it is measured
# against dates the study recorded -- never a value pasted onto another row.
#
# WHY D1 WINS AND IS NOT AVERAGED. A cycle holding both a D1 row and a D8 row
# offers two routes to the same start date, and they disagree when a visit
# slips (drawn D9, still labelled D8). The D1 row is the fact; the slipped D8
# is an artifact. Averaging them splits the difference between a right answer
# and a wrong one, so back-calculation is strictly a FALLBACK for a missing
# D1, and says so via `estimated` (the band draws dashed).
#
# Measured against real study data before any of this was written: the large
# majority of cycles carry a real D1 row, and where both exist the back-calc
# agrees with it closely -- i.e. the labels track the dates, with ordinary
# visit slippage in the tail. The fallback is an edge case, not the common
# path. Re-run the diagnostic per study rather than trusting these shapes;
# a study with sparser D1 coverage would make the fallback load-bearing.

#' The cycle/day vocabulary, as it appears in VISIT
#'
#' Read by the BAND only (see the note at the top of this file); no label goes
#' through here. There is no CDISC-standard spelling, so it has to be
#' permissive to be worth anything: sponsors write "CYCLE 2 DAY 1",
#' "Cycle 2, Day 1", "CYCLE 2/DAY 1", "C2D1", any case, often with a qualifier
#' behind it ("CYCLE 1 DAY 1 PRE-DOSE"). All of those are the same fact and all
#' of them parse. What does NOT parse stays unparsed rather than being guessed
#' at.
#'
#' The abbreviated form is word-bounded on the left so a token ending in "C"
#' cannot start a match, and the long form requires the words. Days may be
#' negative (a screening "DAY -7" belongs to no cycle but is written this way).
#' @noRd
PP_CYCLE_PATTERN <- paste0(
  "(?:\\bCYCLE[ _]*([0-9]+)[ _,/-]*DAY[ _]*(-?[0-9]+)",   # CYCLE 2 DAY 1
  "|\\bC[ _]*([0-9]+)[ _,/-]*D[ _]*(-?[0-9]+)\\b)"        # C2D1
)

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
  parts <- regmatches(
    visit, regexec(PP_CYCLE_PATTERN, visit, ignore.case = TRUE, perl = TRUE)
  )
  ok <- lengths(parts) == 5L
  if (!any(ok)) return(out)
  # One alternative matched, so one pair of groups is empty: whichever is
  # filled is the answer.
  grab <- function(p, long, short) {
    v <- if (nzchar(p[long])) p[long] else p[short]
    if (nzchar(v)) as.integer(v) else NA_integer_
  }
  out$cycle[ok] <- vapply(parts[ok], grab, integer(1L), 2L, 4L)
  out$day[ok] <- vapply(parts[ok], grab, integer(1L), 3L, 5L)
  out
}

#' Append a record's own visit label to its timeline label
#'
#' The date or study day stays the label and the visit rides behind it in
#' parentheses, verbatim. Never replaces and never rewritten: a visit label
#' alone cannot be compared across patients, and reformatting it is how a
#' delay disappears. An infusion the study filed as `CYCLE 1 DAY 8` and gave
#' on day 9 reads `2014-01-09 (CYCLE 1 DAY 8)`, which is both facts at once
#' and needs no vocabulary to produce.
#'
#' Whitespace is folded because a label is drawn on one line and a study's own
#' column may carry line breaks; nothing else is touched.
#'
#' @param base An already-formatted label (`"D143"`, `"2014-05-01"`).
#' @param visit The visit label of the SAME record, or `NA`.
#' @return `base`, possibly with `" (WEEK 24)"` appended.
#' @noRd
pp_with_visit <- function(base, visit) {
  if (!nzchar(base)) return(base)
  if (length(visit) != 1L || is.na(visit)) return(base)
  lab <- trimws(gsub("[[:space:]]+", " ", as.character(visit)))
  if (!nzchar(lab)) return(base)
  paste0(base, " (", lab, ")")
}

#' Where the cycle BAND reads its vocabulary from, best source first
#'
#' Dosing first: an administration is the cycle's Day 1, so where the dosing
#' table labels its rows "CYCLE n DAY 1" the band opens on the infusion. The
#' lab table is the fallback. It is where studies schedule the dense
#' D1/D8/D15 visits, but its dates are blood draws and a pre-dose draw puts
#' the band a day or three left of the infusion that opens the cycle. Visibly
#' so, which is the point: a band is a ruler you sight along, not a value
#' stamped on a record.
#'
#' @return List of `list(table, visit, date)`, most authoritative first.
#' @noRd
pp_cycle_sources <- function() {
  list(
    list(table = "adex", visit = "AVISIT", date = "ASTDT"),
    list(table = "adlb", visit = "AVISIT", date = "ADT")
  )
}

#' The first source a study actually answers to
#'
#' Presence of the table and columns is not enough: a study can ship `adex`
#' with a visit column that speaks weeks, or nothing at all, and the lab table
#' behind it may still carry the cycles. So the vocabulary has to parse.
#'
#' Past the two preferences, ANY table that dates its visits is a candidate.
#' A study whose scheduled visits live in vitals, ECGs, tumour assessments or
#' a vendor table is not a different kind of study, and gating the band on
#' `adlb` would leave it with no band at all. Scanned in name order so the
#' answer does not depend on how the dm was assembled.
#'
#' @param tbls Named list of tables (from `dm::dm_get_tables()`).
#' @param sources Candidate sources, best first.
#' @return One source entry (`table`, `visit`, `date`), or `NULL`.
#' @noRd
pp_cycle_source <- function(tbls, sources = pp_cycle_sources()) {
  speaks_cycles <- function(src) {
    if (!src$table %in% names(tbls)) return(FALSE)
    df <- as.data.frame(tbls[[src$table]])
    if (!all(c("USUBJID", src$visit, src$date) %in% colnames(df))) return(FALSE)
    # DISTINCT labels: a dosing table is short but a lab table runs to
    # hundreds of thousands of rows against a few dozen visit names.
    visits <- unique(as.character(df[[src$visit]]))
    any(!is.na(pp_parse_cycle_visits(visits)$cycle))
  }

  for (src in sources) {
    if (speaks_cycles(src)) return(src)
  }

  named <- vapply(sources, `[[`, character(1L), "table")
  for (nm in sort(setdiff(names(tbls), named))) {
    cols <- colnames(as.data.frame(tbls[[nm]]))
    date <- intersect(c("ADT", "ASTDT"), cols)
    if (!length(date)) next
    src <- list(table = nm, visit = "AVISIT", date = date[[1L]])
    if (speaks_cycles(src)) return(src)
  }
  NULL
}

#' Per-subject treatment cycle anchors, for the band and nothing else
#'
#' Total on purpose: renders and reactives call this, so a study without the
#' cycle vocabulary yields `NULL` (no cycle lane) rather than a condition.
#' Reads canonical names -- run [pp_normalize_dm()] first.
#'
#' @param dm_obj A normalized `dm`, subject-scoped or not.
#' @param sources Candidate places to read the vocabulary from, best first;
#'   see [pp_cycle_sources()].
#' @return Data frame of `USUBJID`, `cycle`, `cycle_start`, `cycle_end`,
#'   `estimated`, ordered by subject and cycle; `NULL` when nothing parses.
#' @noRd
pp_cycle_anchors <- function(dm_obj, sources = pp_cycle_sources()) {
  if (!inherits(dm_obj, "dm")) return(NULL)
  tbls <- dm::dm_get_tables(dm_obj)
  src <- pp_cycle_source(tbls, sources)
  if (is.null(src)) return(NULL)
  df <- as.data.frame(tbls[[src$table]])

  parsed <- pp_parse_cycle_visits(df[[src$visit]])
  dt <- pp_as_date(df[[src$date]])
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
