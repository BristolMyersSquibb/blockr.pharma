# The AE timeline's frame: ADAE (and ADSL) in, one long frame out, which the
# spans column of new_ae_timeline_block() draws. One row per patient in the
# table; in the frame, one record per thing drawn:
#
#   AE         one bar per AE, study day of onset to resolution, coloured by
#              severity or grade, on a lane (stacked, by term, by body system
#              or one lane). An AE without an end is ongoing: it runs to the
#              patient's last contact or the data cutoff and ends open.
#   treatment  the treatment period, first to last dose, as the line
#   events     serious AE onset and death
#
# Every record carries the patient's listing values (number of AEs, ongoing,
# serious, worst grade), so the table's columns read them with `max`.

#' AE timeline frame from ADaM
#'
#' Turns ADAE, and ADSL when given, into the long frame the AE timeline block
#' draws ([new_ae_timeline_block()]). It is that block's prepare step and
#' needs no prep of its inputs.
#'
#' @param data A `dm` (or a named list of data frames) with `adae` and,
#'   optional, `adsl`; or the ADAE data frame alone. ADSL columns the frame
#'   reads (`TRTSDT`, `TRTEDT`, `LSTALVDT`, `DTHDT` and the group column) come
#'   from ADSL when it is there, else from ADAE, which carries them in ADaM.
#' @param events Which AEs: `"All AEs"`, `"Serious AEs"` or `"Ongoing AEs"`.
#' @param colour `"Severity"` (`AESEV`) or `"Grade"` (`AETOXGR`/`ATOXGR`).
#' @param lanes How overlapping AEs draw: `"stacked"` (packed into as few
#'   lanes as overlap needs), `"by term"`, `"by body system"`, or
#'   `"on one lane"`.
#' @param ongoing Where an AE without an end runs to: `"last contact"`
#'   (`LSTALVDT`, else the last treatment day) or `"data cutoff"` (no end,
#'   so the bar runs to the end of the axis, the last date in the data).
#' @param serious How a serious AE shows: `"outlined"` (a dark edge on its
#'   bar), `"marked at onset"` (a triangle at its start) or `"unmarked"`.
#' @param order_by Row order within a group: `"number of AEs"`,
#'   `"worst grade"`, `"ongoing AEs"`, `"first onset"` or `"subject"`.
#' @param term_labels Where the term shows: `"on the bars"` (written on the
#'   bar when it fits, else after it when there is room) or
#'   `"in the tooltip"` (only on hover).
#' @param treatment Draw the treatment period as a line.
#' @param treatment_emergent Keep treatment-emergent AEs only (`TRTEMFL`),
#'   when the column is there.
#' @param cutoff The data cutoff, a date. `NULL` or `NA`: automatic, the
#'   last date in the data. AEs starting after it are left out, an AE still
#'   open at it is ongoing, and death and last contact after it do not count.
#' @param group The column grouping the patients, e.g. `"TRT01A"`. Empty:
#'   none. The frame also keeps the other patient columns ADSL has of
#'   `TRT01P`, `ARM`, `ACTARM`, `SEX`, `AGEGR1` and `RACE`, so the table can
#'   group by them instead.
#' @return A data frame, one record per drawn piece; see the source for the
#'   columns.
#' @examples
#' if (requireNamespace("pharmaverseadam", quietly = TRUE)) {
#'   head(ae_timeline_frame(pharmaverseadam::adae))
#' }
#' @export
ae_timeline_frame <- function(data,
                              events = "All AEs",
                              colour = "Severity",
                              lanes = "stacked",
                              ongoing = "last contact",
                              serious = "outlined",
                              order_by = "number of AEs",
                              term_labels = "on the bars",
                              treatment = TRUE,
                              treatment_emergent = TRUE,
                              cutoff = NULL,
                              group = "TRT01A") {
  events <- match.arg(as.character(events),
                      c("All AEs", "Serious AEs", "Ongoing AEs"))
  colour <- match.arg(as.character(colour), c("Severity", "Grade"))
  lanes <- match.arg(as.character(lanes), AET_LANES)
  ongoing <- match.arg(as.character(ongoing), c("last contact",
                                                "data cutoff"))
  serious <- match.arg(as.character(serious), AET_SERIOUS)
  order_by <- match.arg(as.character(order_by), AET_ORDER)
  term_labels <- match.arg(as.character(term_labels), AET_TERM_LABELS)
  group <- as.character(unlist(group %||% character()))
  group <- group[!is.na(group) & nzchar(group)]
  group <- if (length(group)) group[[1L]]

  tb <- aet_tables(data)
  pcols <- aet_patient_cols(tb, group)
  # Row subsets drop a column's label; the header and sentence read it.
  src <- tb$adsl %||% tb$adae
  labels <- lapply(stats::setNames(pcols, pcols), function(nm) {
    attr(src[[nm]] %||% tb$adae[[nm]], "label", exact = TRUE)
  })
  ae <- tb$adae
  if (isTRUE(treatment_emergent) && "TRTEMFL" %in% names(ae)) {
    ae <- ae[ae$TRTEMFL %in% "Y", , drop = FALSE]
  }
  sl <- aet_subjects(tb$adsl, ae, pcols)
  ae <- ae[as.character(ae$USUBJID) %in% sl$USUBJID, , drop = FALSE]
  cutoff <- aet_date(cutoff)
  if (!length(cutoff) || is.na(cutoff[[1L]])) {
    cutoff <- aet_cutoff(ae, sl)
  }
  cutoff <- cutoff[[1L]]
  sl <- aet_cap_at(sl, cutoff)
  rec <- aet_records(ae, sl, colour)
  rec <- aet_ends(rec, sl, ongoing, cutoff)
  if (!nrow(rec)) {
    stop("No adverse event with an onset day to draw.", call. = FALSE)
  }
  rec <- switch(
    events,
    "Serious AEs" = rec[rec$Serious %in% "Yes", , drop = FALSE],
    "Ongoing AEs" = rec[rec$.open, , drop = FALSE],
    rec
  )
  if (!nrow(rec)) {
    stop("No ", tolower(events), " to draw.", call. = FALSE)
  }
  rec$.lane <- aet_lanes(rec, lanes)
  rec$.bar_label <- if (identical(term_labels, "on the bars")) rec$Term else
    NA_character_
  rec$.style <- if (identical(serious, "outlined")) {
    ifelse(rec$Serious %in% "Yes", "outline", NA_character_)
  } else {
    NA_character_
  }
  rec <- aet_fill_cols(rec)
  sl <- sl[sl$USUBJID %in% rec$USUBJID, , drop = FALSE]
  out <- rbind(
    rec,
    if (isTRUE(treatment)) aet_treatment_rows(sl, rec),
    aet_event_rows(rec, sl, serious)
  )
  # Past the cutoff the record has no end: it runs, open, to the axis end.
  if (identical(ongoing, "data cutoff")) out$End[out$.open] <- NA_real_
  out <- aet_finish(out, rec, sl, pcols, order_by, colour)
  for (nm in names(labels)) attr(out[[nm]], "label") <- labels[[nm]]
  out
}

# Patient-level columns a table can group by.
AET_PATIENT_COLS <- c("TRT01A", "TRT01P", "ARM", "ACTARM", "SEX", "AGEGR1",
                      "RACE")

# The grouping column first, then the patient columns the data has.
aet_patient_cols <- function(tb, group) {
  have <- union(names(tb$adsl), names(tb$adae))
  if (!is.null(group) && !group %in% have) {
    stop("Column \"", group, "\" is in neither adsl nor adae.",
         call. = FALSE)
  }
  unique(c(group, intersect(AET_PATIENT_COLS, have)))
}

# Nothing after the cutoff: death, last contact and the last dose past it
# move to it or leave.
aet_cap_at <- function(sl, cutoff) {
  if (is.na(cutoff)) return(sl)
  sl$DTHDT[!is.na(sl$DTHDT) & sl$DTHDT > cutoff] <- as.Date(NA)
  for (nm in c("LSTALVDT", "TRTEDT")) {
    late <- !is.na(sl[[nm]]) & sl[[nm]] > cutoff
    sl[[nm]][late] <- cutoff
  }
  sl
}

AET_TERM_LABELS <- c("on the bars", "in the tooltip")
AET_SERIOUS <- c("outlined", "marked at onset", "unmarked")
AET_LANES <- c("stacked", "by term", "by body system", "on one lane")
AET_ORDER <- c("number of AEs", "worst grade", "ongoing AEs", "first onset",
               "subject")

# Mockup palette: warm, darker with severity. Grades run past severe.
AET_COLOURS <- c(
  Mild = "#f0c35a", Moderate = "#e07b39", Severe = "#b8322a",
  "Grade 1" = "#f0c35a", "Grade 2" = "#e07b39", "Grade 3" = "#b8322a",
  "Grade 4" = "#7a1f1a", "Grade 5" = "#3b1210"
)

# --- inputs ----------------------------------------------------------------

aet_tables <- function(data) {
  if (is.data.frame(data)) {
    return(list(adae = as.data.frame(data), adsl = NULL))
  }
  tabs <- if (inherits(data, "dm")) dm::dm_get_tables(data) else data
  if (!is.list(tabs)) {
    stop("The AE timeline needs adae, or a dm with adae and adsl.",
         call. = FALSE)
  }
  names(tabs) <- tolower(names(tabs))
  if (is.null(tabs$adae)) {
    stop("The AE timeline needs adae in its input dm.", call. = FALSE)
  }
  list(adae = as.data.frame(tabs$adae),
       adsl = if (!is.null(tabs$adsl)) as.data.frame(tabs$adsl))
}

aet_col <- function(df, nms) {
  hit <- intersect(nms, names(df))
  if (length(hit)) df[[hit[[1L]]]]
}

aet_date <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "POSIXct")) return(as.Date(x))
  if (inherits(x, "Date")) return(x)
  as.Date(as.character(x))
}

# CDISC study day: day 1 is the first dose, no day 0.
aet_day <- function(d, t0) {
  k <- as.numeric(d) - as.numeric(t0)
  ifelse(k >= 0, k + 1, k)
}

# One row per patient: first and last dose, last contact, death, group. From
# ADSL when given, else the first ADAE row of each patient.
aet_subjects <- function(adsl, ae, pcols) {
  src <- if (!is.null(adsl)) adsl else ae[!duplicated(ae$USUBJID), ,
                                          drop = FALSE]
  for (nm in setdiff(pcols, names(src))) {
    src[[nm]] <- ae[[nm]][match(src$USUBJID, ae$USUBJID)]
  }
  n <- nrow(src)
  na_date <- rep(as.Date(NA), n)
  out <- data.frame(
    USUBJID = as.character(src$USUBJID),
    TRTSDT = aet_date(aet_col(src, "TRTSDT")) %||% na_date,
    TRTEDT = aet_date(aet_col(src, "TRTEDT")) %||% na_date,
    LSTALVDT = aet_date(aet_col(src, "LSTALVDT")) %||% na_date,
    DTHDT = aet_date(aet_col(src, "DTHDT")) %||% na_date,
    stringsAsFactors = FALSE
  )
  for (nm in pcols) out[[nm]] <- src[[nm]]
  out[!is.na(out$USUBJID) & out$USUBJID %in% as.character(ae$USUBJID), ,
      drop = FALSE]
}

aet_title <- function(x) {
  x <- as.character(x)
  up <- !is.na(x) & x == toupper(x)
  x[up] <- paste0(substr(x[up], 1L, 1L), tolower(substring(x[up], 2L)))
  x
}

# The colour column: severity words in clinical order, or grades in numeric
# order, as "Grade 3".
aet_grade <- function(ae, colour) {
  if (identical(colour, "Grade")) {
    g <- aet_col(ae, c("ATOXGR", "AETOXGR"))
    if (is.null(g)) {
      stop("adae has no toxicity grade (ATOXGR or AETOXGR); colour by ",
           "severity instead.", call. = FALSE)
    }
    g <- suppressWarnings(as.integer(as.character(g)))
    lv <- paste("Grade", 1:5)
    return(factor(ifelse(is.na(g), NA, paste("Grade", g)), levels = lv))
  }
  s <- aet_col(ae, c("ASEV", "AESEV"))
  if (is.null(s)) {
    stop("adae has no severity (ASEV or AESEV).", call. = FALSE)
  }
  factor(aet_title(toupper(as.character(s))),
         levels = c("Mild", "Moderate", "Severe"))
}

# One record per AE with an onset day.
aet_records <- function(ae, sl, colour) {
  t0 <- sl$TRTSDT[match(as.character(ae$USUBJID), sl$USUBJID)]
  st <- aet_col(ae, "ASTDY")
  en <- aet_col(ae, "AENDY")
  if (is.null(st)) st <- aet_day(aet_date(aet_col(ae, c("ASTDT", "AESTDTC"))),
                                 t0)
  if (is.null(en)) {
    d <- aet_date(aet_col(ae, c("AENDT", "AEENDTC")))
    en <- if (is.null(d)) rep(NA_real_, nrow(ae)) else aet_day(d, t0)
  }
  n <- nrow(ae)
  chr <- function(nms) {
    v <- aet_col(ae, nms)
    if (is.null(v)) rep(NA_character_, n) else as.character(v)
  }
  ser <- chr("AESER")
  out <- data.frame(
    USUBJID = as.character(ae$USUBJID),
    Start = as.numeric(st),
    End = as.numeric(en),
    Term = aet_title(chr(c("AEDECOD", "AETERM"))),
    "Body system" = aet_title(chr(c("AEBODSYS", "AESOC"))),
    Serious = ifelse(ser %in% "Y", "Yes", "No"),
    Outcome = aet_title(chr("AEOUT")),
    .fatal = chr("AESDTH") %in% "Y",
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out$.grade <- aet_grade(ae, colour)
  out[is.finite(out$Start), , drop = FALSE]
}

# The last date anywhere in the data.
aet_cutoff <- function(ae, sl) {
  d <- c(aet_date(aet_col(ae, "ASTDT")), aet_date(aet_col(ae, "AENDT")),
         sl$LSTALVDT, sl$TRTEDT, sl$DTHDT)
  d <- d[!is.na(d)]
  if (length(d)) max(d) else as.Date(NA)
}

# An AE without an end runs open to the patient's last contact or the data
# cutoff, never before its own start.
aet_ends <- function(rec, sl, ongoing, cutoff) {
  i <- match(rec$USUBJID, sl$USUBJID)
  t0 <- sl$TRTSDT[i]
  cut <- aet_day(rep(cutoff, length(i)), t0)
  keep <- !is.finite(cut) | rec$Start <= cut
  rec <- rec[keep, , drop = FALSE]
  i <- i[keep]
  t0 <- t0[keep]
  cut <- cut[keep]
  # Still open at the cutoff is ongoing.
  rec$End[is.finite(cut) & is.finite(rec$End) & rec$End > cut] <- NA_real_
  last <- if (identical(ongoing, "last contact")) {
    lc <- sl$LSTALVDT[i]
    lc[is.na(lc)] <- sl$TRTEDT[i][is.na(lc)]
    aet_day(lc, t0)
  } else {
    aet_day(rep(cutoff, length(i)), t0)
  }
  rec$.open <- !is.finite(rec$End)
  fill <- ifelse(is.finite(last), last, rec$Start)
  rec$End[rec$.open] <- pmax(fill[rec$.open], rec$Start[rec$.open])
  # A one-day AE is a day long, not a point.
  rec$End <- pmax(rec$End, rec$Start + 0.6)
  rec[["Study days"]] <- paste0("Day ", rec$Start, " to ",
                     ifelse(rec$.open, "ongoing", round(rec$End)))
  rec
}

# Lane per AE. Stacked packs each patient's AEs greedily by onset into the
# first lane free by then; term and body system lanes keep one value each.
aet_lanes <- function(rec, lanes) {
  if (identical(lanes, "on one lane")) {
    return(rep("1", nrow(rec)))
  }
  if (lanes %in% c("by term", "by body system")) {
    v <- if (identical(lanes, "by term")) rec$Term else rec[["Body system"]]
    return(v)
  }
  out <- integer(nrow(rec))
  for (idx in split(seq_len(nrow(rec)), rec$USUBJID)) {
    idx <- idx[order(rec$Start[idx], -rec$End[idx])]
    ends <- numeric()
    for (r in idx) {
      l <- which(ends < rec$Start[[r]])
      l <- if (length(l)) l[[1L]] else length(ends) + 1L
      ends[l] <- rec$End[[r]]
      out[[r]] <- l
    }
  }
  as.character(out)
}

# Records for the overlays, every AE column empty.
aet_blank <- function(USUBJID, rec) {
  n <- length(USUBJID)
  if (!n) return(NULL)
  out <- rec[rep(NA_integer_, n), , drop = FALSE]
  rownames(out) <- NULL
  out$USUBJID <- USUBJID
  out$.open <- FALSE
  out$.fatal <- FALSE
  out
}

aet_treatment_rows <- function(sl, rec) {
  s <- sl[!is.na(sl$TRTSDT) & !is.na(sl$TRTEDT), , drop = FALSE]
  out <- aet_blank(s$USUBJID, rec)
  if (is.null(out)) return(NULL)
  out$.trt_from <- 1
  out$.trt_to <- aet_day(s$TRTEDT, s$TRTSDT)
  out
}

# Serious onset and death. A fatal AE marks death at its end when ADSL has no
# death date.
aet_event_rows <- function(rec, sl, serious) {
  out <- list()
  if (identical(serious, "marked at onset")) {
    s <- rec[rec$Serious %in% "Yes", , drop = FALSE]
    out$ser <- aet_blank(s$USUBJID, rec)
    if (!is.null(out$ser)) {
      out$ser$.event_day <- s$Start
      out$ser$.event <- "Serious AE"
      out$ser$.lane <- s$.lane
    }
  }
  dth <- sl[!is.na(sl$DTHDT), , drop = FALSE]
  day <- aet_day(dth$DTHDT, dth$TRTSDT)
  fat <- rec[rec$.fatal & !rec$USUBJID %in% dth$USUBJID, , drop = FALSE]
  fat <- fat[!duplicated(fat$USUBJID), , drop = FALSE]
  ids <- c(dth$USUBJID, fat$USUBJID)
  out$dth <- aet_blank(ids, rec)
  if (!is.null(out$dth)) {
    out$dth$.event_day <- c(day, fat$End)
    out$dth$.event <- "Death"
    # A death date marks the whole row; a fatal AE's end, its own lane.
    out$dth$.lane <- c(rep(NA_character_, length(day)), fat$.lane)
  }
  out <- Filter(Negate(is.null), out)
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}

# Overlay columns on every record set, so the pieces bind.
aet_fill_cols <- function(x, ...) {
  for (nm in c(".trt_from", ".trt_to", ".event_day")) {
    if (is.null(x[[nm]])) x[[nm]] <- NA_real_
  }
  if (is.null(x[[".event"]])) x[[".event"]] <- NA_character_
  x
}

# Listing values per patient, row order, column types and colours.
aet_finish <- function(out, rec, sl, pcols, order_by, colour) {
  out <- aet_fill_cols(out)
  rec <- out[is.finite(out$Start), , drop = FALSE]
  ids <- sl$USUBJID
  per <- function(f) vapply(ids, function(id) f(rec[rec$USUBJID == id, ]),
                            numeric(1L))
  n_ae <- per(nrow)
  n_open <- per(function(r) sum(r$.open))
  n_ser <- per(function(r) sum(r$Serious %in% "Yes"))
  worst <- per(function(r) {
    g <- as.integer(r$.grade)
    if (all(is.na(g))) NA_real_ else max(g, na.rm = TRUE)
  })
  first <- per(function(r) min(r$Start))
  lv <- levels(rec$.grade)
  key <- switch(
    order_by,
    "number of AEs" = order(-n_ae, ids),
    "worst grade" = order(-worst, -n_ae, ids),
    "ongoing AEs" = order(-n_open, -n_ae, ids),
    "first onset" = order(first, ids),
    order(ids)
  )
  ids <- ids[key]
  i <- match(out$USUBJID, ids)
  out$AEs <- n_ae[out$USUBJID]
  out$Ongoing <- n_open[out$USUBJID]
  out[["Serious AEs"]] <- n_ser[out$USUBJID]
  w <- worst[out$USUBJID]
  out$Worst <- factor(ifelse(is.na(w), NA, lv[w]), levels = lv)
  for (nm in pcols) out[[nm]] <- sl[[nm]][match(out$USUBJID, sl$USUBJID)]
  out <- out[order(i, is.na(out$Start), out$Start), , drop = FALSE]
  rownames(out) <- NULL
  # The colour column: severity words or grades, with their own colours.
  names(out)[names(out) == ".grade"] <- "Severity"
  attr(out$Severity, "blockr_colors") <- AET_COLOURS[lv]
  attr(out$Worst, "blockr_colors") <- AET_COLOURS[lv]
  out$.lane <- factor(out$.lane, levels = aet_lane_levels(out$.lane, rec))
  # The level order picks the shapes: a filled triangle, an open triangle
  # (unused), a cross.
  out$Event <- factor(out$.event, levels = c("Serious AE", "Fatal AE",
                                             "Death"))
  attr(out$USUBJID, "label") <- "Patient"
  keep <- c(pcols, "USUBJID", "AEs", "Ongoing", "Serious AEs", "Worst",
            "Start", "End", ".open", "Severity", ".lane", ".style", "Term",
            "Body system", "Serious", "Outcome", "Study days", ".bar_label",
            ".trt_from", ".trt_to", ".event_day", "Event")
  out[, keep, drop = FALSE]
}

# Stacked lanes in number order; term and body system lanes by frequency.
aet_lane_levels <- function(lane, rec) {
  v <- lane[!is.na(lane)]
  if (all(grepl("^[0-9]+$", v))) {
    return(as.character(sort(unique(as.integer(v)))))
  }
  names(sort(table(v), decreasing = TRUE))
}
