# The timeline's frame: ADSL, ADAE, ADCM and ADEX in, one long frame out,
# which the spans column of new_timeline_block() draws. The table has a row
# per patient that opens into a row per lane (an AE term, a con med, a drug);
# in the frame, one record per thing drawn:
#
#   AE          a bar from onset to resolution, coloured by grade or severity
#   con med     a bar from start to end, coloured by whether it started before
#               or on treatment, by class or by indication
#   treatment   a bar per dose period from ADEX, in greys by dose level; without
#               ADEX one bar from first to last dose
#   events      serious AE onset, death
#   patient     one record per patient with no lane: what the patient row shows
#               and its rows do not (arm, days on treatment, death)
#
# A record's `.lane` is its source, so the closed patient row draws one stripe
# per source; `Row` is its row when the patient is opened. Without an end a
# record is ongoing and runs, open, to last contact or the cutoff.
#
# Every step works on whole columns: no subsetting per patient.

#' Timeline frame from ADaM
#'
#' Turns ADSL, ADAE, ADCM and ADEX into the long frame the timeline block
#' draws ([new_timeline_block()]). It is that block's prepare step and needs
#' no prep of its inputs. Tables and columns may be named as the patient
#' profile allows (`dm`, `ae`, `cm`, `ex`; SDTM columns such as `AESTDTC`).
#'
#' @param data A `dm` (or a named list of data frames) with `adsl` and the
#'   tables of what is shown; or the ADAE data frame alone.
#' @param show What to draw, any of `"Adverse events"`, `"Con meds"` and
#'   `"Treatment"`.
#' @param ae_records `"treatment-emergent"` (`TRTEMFL`) or `"all"`.
#' @param ae_show `"all AEs"`, `"serious AEs"` or `"ongoing AEs"`.
#' @param ae_colour `"grade or severity"` (grade when ADAE has one, else
#'   severity), `"severity"`, `"grade"` or `"one colour"`.
#' @param ae_lanes One row per `"preferred term"`, `"reported term"` or
#'   `"body system"` when a patient is opened.
#' @param serious `"outlined"`, `"marked at onset"` or `"unmarked"`.
#' @param death Mark death with a cross.
#' @param cm_records `"active on treatment"` (ends on or after the first dose,
#'   or ongoing) or `"all"` (the axis then starts 28 days before day 1).
#' @param cm_colour `"before or on treatment"`, `"class"`, `"indication"` or
#'   `"one colour"`. Classes and indications keep their most common levels
#'   and fold the rest into "Other", so the colours stay within what a
#'   legend can show (see [timeline_frame()]'s details).
#' @param cm_lanes One row per `"coded name"`, `"reported name"` or
#'   `"class"`.
#' @param cm_prior Con meds that start before the axis: `"clipped at the
#'   start"` or `"hidden"`.
#' @param trt_colour `"dose level"` (more than five doses fall into dose
#'   ranges), `"drug"` (more than five fold into "Other drug") or `"one
#'   colour"`.
#' @param dose_labels Write the dose on the treatment bars.
#' @param max_lanes Rows per opened patient before the rest of the con meds
#'   fold into "Other con meds".
#' @param origin Day 1 is the `"first dose"` (`TRTSDT`) or `"randomisation"`
#'   (`RANDDT`).
#' @param cutoff The data cutoff, a date. `NULL` or `NA`: the last date in
#'   the data.
#' @param ongoing A record without an end runs to `"last contact"`
#'   (`LSTALVDT`, else the last dose) or the `"data cutoff"`.
#' @param order_by Patient order: `"number of events"` (AEs and con meds
#'   shown), `"number of AEs"`, `"worst AE"`,
#'   `"ongoing AEs"`, `"first onset"`, `"number of con meds"` or
#'   `"subject"`.
#' @param group The patient column shown as the arm, e.g. `"TRT01A"`.
#' @details Time is in study days. The colour column holds at most 15 levels
#'   over all sources, the most the table draws: AEs take theirs (at most
#'   five grades), treatment at most five, con meds what is left, at most
#'   eight. Each level carries its default colour (`blockr_colors`) and the
#'   data variable and value it stands for (`blockr_scale_keys`, e.g. `ASEV`
#'   `"MILD"`, `CMCLAS`, `EXTRT`), so a colour the board's scale map sets for
#'   that value wins.
#' @return A data frame, one record per drawn piece; see the source for the
#'   columns.
#' @examples
#' if (requireNamespace("pharmaverseadam", quietly = TRUE)) {
#'   d <- list(adsl = pharmaverseadam::adsl, adae = pharmaverseadam::adae,
#'             adex = pharmaverseadam::adex)
#'   head(timeline_frame(d))
#' }
#' @export
timeline_frame <- function(data,
                           show = c("Adverse events", "Treatment"),
                           ae_records = "treatment-emergent",
                           ae_show = "all AEs",
                           ae_colour = "grade or severity",
                           ae_lanes = "preferred term",
                           serious = "outlined",
                           death = TRUE,
                           cm_records = "active on treatment",
                           cm_colour = "before or on treatment",
                           cm_lanes = "coded name",
                           cm_prior = "clipped at the start",
                           trt_colour = "dose level",
                           dose_labels = TRUE,
                           max_lanes = 12,
                           origin = "first dose",
                           cutoff = NULL,
                           ongoing = "last contact",
                           order_by = "number of events",
                           group = "TRT01A") {
  show <- intersect(TL_SHOW, as.character(unlist(show)))
  if (!length(show)) stop("Pick at least one thing to show.", call. = FALSE)
  arg <- function(x, choices) match.arg(as.character(x)[1L], choices)
  opt <- list(
    ae_records = arg(ae_records, TL_AE_RECORDS),
    ae_show = arg(ae_show, TL_AE_SHOW),
    ae_colour = arg(ae_colour, TL_AE_COLOUR),
    ae_lanes = arg(ae_lanes, names(TL_AE_LANES)),
    serious = arg(serious, TL_SERIOUS),
    death = isTRUE(as.logical(death)),
    cm_records = arg(cm_records, TL_CM_RECORDS),
    cm_colour = arg(tl_cm_colour_value(cm_colour), TL_CM_COLOUR),
    cm_lanes = arg(cm_lanes, names(TL_CM_LANES)),
    cm_prior = arg(cm_prior, TL_CM_PRIOR),
    trt_colour = arg(trt_colour, TL_TRT_COLOUR),
    dose_labels = isTRUE(as.logical(dose_labels)),
    max_lanes = max(1L, as.integer(max_lanes %||% 12L)[1L], na.rm = TRUE),
    origin = arg(origin, TL_ORIGIN),
    ongoing = arg(ongoing, c("last contact", "data cutoff")),
    order_by = arg(order_by, TL_ORDER)
  )
  group <- as.character(unlist(group %||% character()))
  group <- group[!is.na(group) & nzchar(group)]
  group <- if (length(group)) group[[1L]]

  tb <- tl_tables(data, show)
  sl <- tl_subjects(tb, group, opt$origin)
  cutoff <- aet_date(cutoff)
  if (!length(cutoff) || is.na(cutoff[[1L]])) cutoff <- tl_cutoff(tb, sl)
  cutoff <- cutoff[[1L]]
  sl <- aet_cap_at(sl, cutoff)

  parts <- list()
  if ("Adverse events" %in% show && !is.null(tb$adae)) {
    parts$ae <- tl_ae(tb$adae, sl, opt, cutoff)
  }
  if ("Treatment" %in% show) parts$trt <- tl_treatment(tb$adex, sl, opt, cutoff)
  lo <- tl_axis_start(parts)
  if ("Con meds" %in% show && !is.null(tb$adcm)) {
    # All con meds, prior-only ones included: the axis starts 28 days
    # before day 1, as the swimmer's con meds do.
    if (identical(opt$cm_records, "all")) lo <- min(lo, 1 - 28)
    parts$cm <- tl_cm(tb$adcm, sl, opt, cutoff, lo)
  }
  parts <- Filter(function(p) !is.null(p) && length(p$USUBJID), parts)
  if (!length(parts)) stop("Nothing to draw.", call. = FALSE)
  # Patients with an event (an AE or a con med) shown; treatment alone draws
  # every patient it has.
  ev <- unique(unlist(lapply(parts[intersect(c("ae", "cm"), names(parts))],
                             `[[`, "USUBJID")))
  ids <- if (length(ev)) ev else unique(parts$trt$USUBJID)
  pcols <- attr(sl, "pcols")
  sl <- sl[sl$USUBJID %in% ids, , drop = FALSE]
  attr(sl, "pcols") <- pcols
  parts <- lapply(parts, function(p) tl_keep(p, p$USUBJID %in% sl$USUBJID))
  if ("cm" %in% names(parts)) parts$cm <- tl_fold(parts, opt$max_lanes)
  parts <- tl_unique_rows(parts)
  rec <- tl_bind(parts)
  out <- tl_bind(list(rec, tl_event_rows(rec, sl, opt),
                      tl_patient_rows(sl, opt)))
  tl_finish(out, sl, opt, group)
}

TL_SHOW <- c("Adverse events", "Con meds", "Treatment")
TL_AE_RECORDS <- c("treatment-emergent", "all")
TL_AE_SHOW <- c("all AEs", "serious AEs", "ongoing AEs")
TL_AE_COLOUR <- c("grade or severity", "severity", "grade", "one colour")
TL_AE_LANES <- c("preferred term" = "AEDECOD", "reported term" = "AETERM",
                 "body system" = "AEBODSYS")
TL_SERIOUS <- c("outlined", "marked at onset", "unmarked")
TL_CM_RECORDS <- c("active on treatment", "all")
TL_CM_COLOUR <- c("before or on treatment", "class", "indication",
                  "one colour")
TL_CM_LANES <- c("coded name" = "CMDECOD", "reported name" = "CMTRT",
                 "class" = "CMCLAS")
TL_CM_PRIOR <- c("clipped at the start", "hidden")
TL_TRT_COLOUR <- c("dose level", "drug", "one colour")
TL_ORIGIN <- c("first dose", "randomisation")
TL_ORDER <- c("number of events", "number of AEs", "worst AE",
              "ongoing AEs", "first onset", "number of con meds", "subject")

# The most colour levels the table draws (blockr.viz's groups limit), and
# what treatment and con meds take of them at most.
TL_MAX_COLOURS <- 15L
TL_MAX_TRT_COLOURS <- 5L
TL_MAX_CM_COLOURS <- 8L
# Folded levels, drawn last and in grey.
TL_OTHER <- c("Other class", "Other indication", "Other", "Other drug",
              "Other dose", "Dose not given")

# The data variables a colour level stands for, for the board's scale map:
# a level's key is its value in the variable the record came from, and the
# map may bind either spelling. A response read is ADRS `AVALC`; a study map
# may carry the same vocabulary as the best overall response or as a
# swimmer's `response` column, and the first the map binds wins.
TL_SCALE_VARS <- list(grade = c("ATOXGR", "AETOXGR"),
                      severity = c("ASEV", "AESEV"), class = "CMCLAS",
                      indication = "CMINDC", drug = c("EXTRT", "CMDECOD"),
                      response = c("AVALC", "BEST_OVERALL_RESPONSE",
                                   "response"))

# Boards saved before the con-med colour default changed name the old one.
tl_cm_colour_value <- function(x) {
  x <- as.character(unlist(x))[1L]
  if (identical(x, "class, else before or on treatment")) {
    "before or on treatment"
  } else {
    x
  }
}

# Patient columns the listing can show, the arm first.
TL_PATIENT_COLS <- c("TRT01A", "TRT01P", "ARM", "ACTARM", "SEX", "AGE",
                     "AGEGR1", "RACE")

# Colours by source: warm for AEs (as the AE timeline had them), blues for con
# meds, greys for treatment.
TL_CM_BLUES <- c("#1f4e8c", "#3f6fb0", "#6b93c9", "#9db8de", "#2f7f9e",
                 "#5aa5c2", "#8cc4d8", "#b9cfe6")
TL_TRT_GREYS <- c("#c4c9d0", "#9aa1ab", "#6b7280", "#4b5361", "#2f3540")

# --- inputs ----------------------------------------------------------------

# The tables, normalised as the patient profile does (pp_normalize_tables()):
# SDTM table names and columns read as their ADaM counterparts.
tl_tables <- function(data, show = TL_SHOW) {
  if (is.data.frame(data)) data <- list(adae = data)
  tabs <- if (inherits(data, "dm")) dm::dm_get_tables(data) else data
  if (!is.list(tabs)) {
    stop("The timeline needs a dm with adsl and adae, adcm or adex.",
         call. = FALSE)
  }
  names(tabs) <- tolower(names(tabs))
  tabs <- pp_normalize_tables(tabs)$tables
  get <- function(nm) {
    x <- tabs[[nm]]
    if (!is.null(x)) as.data.frame(x)
  }
  out <- list(adsl = get("adsl"), adae = get("adae"), adcm = get("adcm"),
              adex = get("adex"))
  need <- c("Adverse events" = "adae", "Con meds" = "adcm")
  need <- need[names(need) %in% show]
  if (length(need) && all(vapply(out[need], is.null, NA)) &&
        !"Treatment" %in% show) {
    stop("The timeline needs ", paste(need, collapse = " or "),
         " in its input.", call. = FALSE)
  }
  if (is.null(out$adsl) && is.null(out$adae)) {
    stop("The timeline needs adsl in its input.", call. = FALSE)
  }
  out
}

# One row per patient: day 1, last dose, last contact, death and the patient
# columns. From ADSL, else the first ADAE row of each patient.
tl_subjects <- function(tb, group, origin) {
  src <- tb$adsl %||% tb$adae[!duplicated(tb$adae$USUBJID), , drop = FALSE]
  # A grouping column the data lacks leaves the arm empty.
  pcols <- intersect(unique(c(group, TL_PATIENT_COLS)), names(src))
  n <- nrow(src)
  na_date <- rep(as.Date(NA), n)
  date <- function(nm) aet_date(aet_col(src, nm)) %||% na_date
  out <- data.frame(
    USUBJID = as.character(src$USUBJID),
    TRTSDT = date("TRTSDT"), TRTEDT = date("TRTEDT"),
    LSTALVDT = date("LSTALVDT"), DTHDT = date("DTHDT"),
    RANDDT = date("RANDDT"),
    stringsAsFactors = FALSE
  )
  out$T0 <- out$TRTSDT
  if (identical(origin, "randomisation")) {
    has <- !is.na(out$RANDDT)
    out$T0[has] <- out$RANDDT[has]
  }
  for (nm in pcols) out[[nm]] <- src[[nm]]
  out$.arm <- if (!is.null(group) && group %in% pcols) {
    as.character(src[[group]])
  } else {
    rep(NA_character_, n)
  }
  out <- out[!is.na(out$USUBJID) & !duplicated(out$USUBJID), , drop = FALSE]
  attr(out, "pcols") <- pcols
  out
}

# The last date anywhere in the data.
tl_cutoff <- function(tb, sl) {
  d <- c(sl$LSTALVDT, sl$TRTEDT, sl$DTHDT)
  for (t in tb[c("adae", "adcm", "adex")]) {
    if (is.null(t)) next
    d <- c(d, aet_date(aet_col(t, "ASTDT")), aet_date(aet_col(t, "AENDT")))
  }
  d <- d[!is.na(d)]
  if (length(d)) max(d) else as.Date(NA)
}

# Study days of a table's records against the patient's day 1: the ADaM study
# days when day 1 is the first dose and the table has them, else from the
# dates.
tl_days <- function(df, i, sl, origin) {
  t0 <- sl$T0[i]
  from <- function(day, dt) {
    v <- aet_col(df, day)
    d <- aet_date(aet_col(df, dt))
    if (!is.null(v) && (identical(origin, "first dose") || is.null(d))) {
      return(as.numeric(v))
    }
    if (is.null(d)) return(rep(NA_real_, nrow(df)))
    aet_day(d, t0)
  }
  list(start = from("ASTDY", "ASTDT"), end = from("AENDY", "AENDT"))
}

# --- records by source ------------------------------------------------------

# Records as a list of columns. tl_bind() joins such lists and fills the
# columns a part does not have.
TL_COLS <- list(
  USUBJID = NA_character_, Row = NA_character_, .src = NA_character_,
  Start = NA_real_, End = NA_real_, .open = FALSE, .colour = NA_character_,
  .style = NA_character_, .label = NA_character_, Type = NA_character_,
  Details = NA_character_, Days = NA_character_, .bar_label = NA_character_,
  .event_x = NA_real_, .event = NA_character_, .ae = NA_real_,
  .ae_open = NA_real_, .ae_ser = NA_real_, .worst = NA_character_,
  .fatal = FALSE, .cm = NA_real_, .cm_new = NA_real_, .days_trt = NA_real_,
  .svar = NA_character_, .skey = NA_character_, .tag = NA_character_,
  .bor = NA_character_, .status = NA_character_, .lane_at = NA_character_
)

tl_bind <- function(parts) {
  parts <- Filter(function(p) !is.null(p) && length(p$USUBJID), parts)
  if (!length(parts)) return(NULL)
  out <- lapply(names(TL_COLS), function(nm) {
    unlist(lapply(parts, function(p) {
      v <- p[[nm]]
      if (is.null(v)) rep(TL_COLS[[nm]], length(p$USUBJID)) else v
    }), use.names = FALSE)
  })
  stats::setNames(out, names(TL_COLS))
}

tl_keep <- function(p, keep) {
  lapply(p, function(v) if (length(v) == length(keep)) v[keep] else v)
}

# Drop what starts after the cutoff, make what is open at it ongoing, and run
# every ongoing record to last contact or the cutoff (aet_ends()'s rule, on
# a part).
tl_ends <- function(p, sl, opt, cutoff) {
  i <- match(p$USUBJID, sl$USUBJID)
  t0 <- sl$T0[i]
  cut <- aet_day(rep(cutoff, length(i)), t0)
  keep <- !is.finite(cut) | p$Start <= cut
  p <- tl_keep(p, keep)
  i <- i[keep]
  t0 <- t0[keep]
  cut <- cut[keep]
  late <- is.finite(cut) & is.finite(p$End) & p$End > cut
  p$End[late] <- NA_real_
  last <- if (identical(opt$ongoing, "last contact")) {
    lc <- sl$LSTALVDT[i]
    lc[is.na(lc)] <- sl$TRTEDT[i][is.na(lc)]
    aet_day(lc, t0)
  } else {
    cut
  }
  p$.open <- !is.finite(p$End)
  fill <- ifelse(is.finite(last), last, p$Start)
  p$End[p$.open] <- pmax(fill[p$.open], p$Start[p$.open])
  # A one-day record is a day long, not a point.
  p$End <- pmax(p$End, p$Start + 0.6)
  p$Days <- paste0("Day ", p$Start, " to ",
                   ifelse(p$.open, "ongoing", round(p$End)))
  p
}

tl_ae <- function(ae, sl, opt, cutoff) {
  if (identical(opt$ae_records, "treatment-emergent") &&
        "TRTEMFL" %in% names(ae)) {
    ae <- ae[aet_flag(ae$TRTEMFL), , drop = FALSE]
  }
  i <- match(as.character(ae$USUBJID), sl$USUBJID)
  ae <- ae[!is.na(i), , drop = FALSE]
  i <- i[!is.na(i)]
  n <- nrow(ae)
  days <- tl_days(ae, i, sl, opt$origin)
  chr <- function(nms) {
    v <- aet_col(ae, nms)
    if (is.null(v)) rep(NA_character_, n) else as.character(v)
  }
  flag <- function(nm) {
    v <- aet_col(ae, nm)
    if (is.null(v)) rep(FALSE, n) else aet_flag(v)
  }
  term <- aet_title(chr(c("AEDECOD", "AETERM")))
  lane_col <- TL_AE_LANES[[opt$ae_lanes]]
  row <- aet_title(chr(c(lane_col, "AEDECOD", "AETERM")))
  row[is.na(row) | !nzchar(row)] <- PP_LANE_UNCODED
  ser <- flag("AESER")
  by <- aet_colour_by(ae, switch(opt$ae_colour,
                                 "grade or severity" = "grade or severity",
                                 severity = "Severity", grade = "Grade",
                                 "one colour" = "AE"))
  grade <- aet_grade(ae, by)
  # The value behind each level, for the board's scale map.
  svar <- switch(by, Grade = "grade", Severity = "severity", NA_character_)
  skey <- switch(by, Grade = chr(c("ATOXGR", "AETOXGR")),
                 Severity = chr(c("ASEV", "AESEV")), rep(NA_character_, n))
  # The worst AE reads grade or severity, whatever colours the bars.
  worst <- if (identical(by, "AE")) {
    aet_grade(ae, aet_colour_by(ae, "grade or severity"))
  } else {
    grade
  }
  body <- aet_title(chr("AEBODSYS"))
  out <- aet_title(chr("AEOUT"))
  det <- paste0(ifelse(ser, "Serious, ", ""),
                ifelse(is.na(body), "", body),
                ifelse(is.na(out), "", paste0(", ", out)))
  p <- list(
    USUBJID = as.character(ae$USUBJID), Row = row, .src = rep("AE", n),
    Start = days$start, End = days$end, .colour = as.character(grade),
    .label = term, Type = rep("Adverse event", n), Details = det,
    .ae = rep(1, n), .ae_ser = as.numeric(ser),
    .worst = as.character(worst), .fatal = flag("AESDTH"),
    .svar = rep(svar, n), .skey = skey
  )
  p <- tl_keep(p, is.finite(p$Start))
  p <- tl_ends(p, sl, opt, cutoff)
  p$.ae_open <- as.numeric(p$.open)
  ser <- p$.ae_ser == 1
  p <- switch(opt$ae_show,
              "serious AEs" = tl_keep(p, ser),
              "ongoing AEs" = tl_keep(p, p$.open),
              p)
  if (identical(opt$serious, "outlined")) {
    p$.style <- ifelse(p$.ae_ser == 1, "outline", NA_character_)
  }
  p
}

tl_cm <- function(cm, sl, opt, cutoff, lo) {
  i <- match(as.character(cm$USUBJID), sl$USUBJID)
  cm <- cm[!is.na(i), , drop = FALSE]
  i <- i[!is.na(i)]
  n <- nrow(cm)
  if (!n) return(NULL)
  days <- tl_days(cm, i, sl, opt$origin)
  chr <- function(nms) {
    v <- aet_col(cm, nms)
    if (is.null(v)) rep(NA_character_, n) else as.character(v)
  }
  # The coded name, else the reported one: most con meds of a study may be
  # uncoded, and dropping them (as the swimmer's lanes do) would hide them.
  coded <- chr("CMDECOD")
  reported <- chr("CMTRT")
  uncoded <- is.na(coded) | !nzchar(coded) | toupper(coded) == "UNCODED"
  name <- swimmer_drug_name(ifelse(uncoded, reported, coded))
  cls <- chr("CMCLAS")
  cls[!is.na(cls) & toupper(cls) == "UNCODED"] <- NA_character_
  row <- switch(opt$cm_lanes,
                "coded name" = name,
                "reported name" = swimmer_drug_name(ifelse(is.na(reported),
                                                           coded, reported)),
                class = aet_title(cls))
  row[is.na(row) | !nzchar(row)] <- PP_LANE_UNCODED
  # Day 1 of the patient's treatment, in the timeline's days.
  d1 <- aet_day(sl$TRTSDT[i], sl$T0[i])
  d1[!is.finite(d1)] <- 1
  ind <- aet_title(chr("CMINDC"))
  p <- list(
    USUBJID = as.character(cm$USUBJID), Row = row, .src = rep("CM", n),
    Start = days$start, End = days$end, .label = name,
    Type = rep("Con med", n),
    Details = paste0(ifelse(is.na(ind), "", paste0("For ", tolower(ind))),
                     ifelse(is.na(cls), "", paste0(
                       ifelse(is.na(ind), "", ", "), aet_title(cls)))),
    .cm = rep(1, n), .d1 = d1, .cls = aet_title(cls), .ind = ind,
    .cls_raw = cls, .ind_raw = chr("CMINDC")
  )
  p <- tl_keep(p, is.finite(p$Start))
  if (identical(opt$cm_records, "active on treatment")) {
    p <- tl_keep(p, !is.finite(p$End) | p$End >= p$.d1)
  }
  p$.cm_new <- as.numeric(p$Start >= p$.d1)
  p <- tl_ends(p, sl, opt, cutoff)
  # Before the axis: clipped at its start (the tooltip keeps the start day),
  # or left out.
  before <- p$Start < lo
  if (identical(opt$cm_prior, "hidden")) {
    p <- tl_keep(p, !before)
  } else {
    p <- tl_keep(p, p$End >= lo)
    p$Start <- pmax(p$Start, lo)
  }
  p <- tl_cm_colour(p, opt)
  p[c(".d1", ".cls", ".ind", ".cls_raw", ".ind_raw")] <- NULL
  p
}

# Con-med colours: started before or on treatment, the class, the
# indication, or one colour. A record without a class or indication is
# "Other"; tl_colour_factor() folds the rarer ones in too.
tl_cm_colour <- function(p, opt) {
  n <- length(p$USUBJID)
  by <- function(x, raw, other, var) {
    list(v = ifelse(is.na(x), other, x),
         svar = ifelse(is.na(x), NA_character_, var), skey = raw)
  }
  res <- switch(
    opt$cm_colour,
    "before or on treatment" = list(
      v = ifelse(p$.cm_new == 1, "Started on treatment",
                 "Started before treatment")
    ),
    class = by(p$.cls, p$.cls_raw, "Other class", "class"),
    indication = by(p$.ind, p$.ind_raw, "Other indication", "indication"),
    list(v = rep("Con med", n))
  )
  p$.colour <- res$v
  p$.svar <- res$svar %||% rep(NA_character_, n)
  p$.skey <- res$skey %||% rep(NA_character_, n)
  p
}

# Treatment: dose periods from ADEX (swimmer_exposure()), else one bar from
# first to last dose.
tl_treatment <- function(adex, sl, opt, cutoff) {
  ex <- if (!is.null(adex)) swimmer_exposure(adex, sl, "study")
  raw <- if (!is.null(adex)) {
    unique(as.character(aet_col(adex, c("EXTRT", "CMDECOD"))))
  }
  if (is.null(ex) || !nrow(ex)) {
    s <- sl[!is.na(sl$TRTSDT), , drop = FALSE]
    ex <- data.frame(USUBJID = s$USUBJID, drug = "Treatment",
                     start = s$TRTSDT, end = s$TRTEDT, dose = NA_real_,
                     unit = "", stringsAsFactors = FALSE)
  }
  if (!nrow(ex)) return(NULL)
  i <- match(ex$USUBJID, sl$USUBJID)
  # A dose record without an end stops at the last dose.
  open_end <- is.na(ex$end)
  ex$end[open_end] <- sl$TRTEDT[i][open_end]
  n <- nrow(ex)
  dose <- ifelse(is.na(ex$dose), "",
                 trimws(paste(format(ex$dose, trim = TRUE, drop0trailing = TRUE),
                              ex$unit)))
  p <- list(
    USUBJID = ex$USUBJID, Row = ex$drug, .src = rep("Treatment", n),
    Start = aet_day(ex$start, sl$T0[i]), End = aet_day(ex$end, sl$T0[i]),
    .label = ex$drug, Type = rep("Treatment", n),
    Details = ifelse(nzchar(dose), dose, NA_character_),
    .bar_label = if (opt$dose_labels) ifelse(nzchar(dose), dose,
                                             NA_character_),
    .dose = ex$dose, .dose_txt = dose, .unit = ex$unit
  )
  p <- tl_keep(p, is.finite(p$Start))
  p <- tl_ends(p, sl, opt, cutoff)
  n <- length(p$USUBJID)
  p$.colour <- switch(
    opt$trt_colour,
    "dose level" = tl_dose_colour(p$.dose, p$.dose_txt, p$.unit,
                                  TL_MAX_TRT_COLOURS),
    drug = tl_fold_top(p$Row, TL_MAX_TRT_COLOURS, "Other drug"),
    rep("Treatment", n)
  )
  if (identical(opt$trt_colour, "drug")) {
    # The drug as ADEX names it, before swimmer_drug_name() titled it.
    k <- match(p$.colour, swimmer_drug_name(raw))
    p$.svar <- ifelse(is.na(k), NA_character_, "drug")
    p$.skey <- raw[k]
  }
  p[c(".dose", ".dose_txt", ".unit")] <- NULL
  p
}

# Dose levels as colours, lightest the lowest: each dose while there are at
# most `cap`, else ranges of the most common unit. A dose in another unit is
# "Other dose", a record without one "Dose not given".
tl_dose_colour <- function(dose, txt, unit, cap) {
  given <- nzchar(txt)
  if (length(unique(txt[given])) + any(!given) <= cap) {
    return(ifelse(given, txt, "Dose not given"))
  }
  ok <- given & is.finite(dose)
  u <- names(which.max(table(unit[ok])))
  main <- ok & unit == u
  room <- cap - any(!main)
  d <- dose[main]
  br <- pretty(d, n = room)
  k <- room
  while (length(br) - 1L > room && k > 1L) {
    k <- k - 1L
    br <- pretty(d, n = k)
  }
  if (length(br) - 1L > room) br <- range(d)
  fmt <- format(br, trim = TRUE, drop0trailing = TRUE)
  lab <- trimws(paste(fmt[-length(fmt)], "to", fmt[-1L], u))
  bin <- findInterval(d, br, rightmost.closed = TRUE, all.inside = TRUE)
  out <- ifelse(given, "Other dose", "Dose not given")
  out[main] <- lab[bin]
  out
}

# Values past the `cap - 1` most common fold into `other`.
tl_fold_top <- function(v, cap, other) {
  lv <- unique(v[!is.na(v) & v != other])
  if (length(lv) + any(v %in% other) <= cap) return(v)
  tab <- table(factor(v[v %in% lv], levels = lv))
  keep <- names(sort(tab, decreasing = TRUE))[seq_len(cap - 1L)]
  ifelse(v %in% keep, v, other)
}

# Where the axis starts: day 1, or the first AE or dose before it. Con meds
# that start earlier are clipped to it.
tl_axis_start <- function(parts) {
  s <- unlist(lapply(parts, function(p) p$Start))
  s <- s[is.finite(s)]
  min(c(1, s))
}

# Con meds past `max_lanes` rows of a patient fold into "Other con meds": the
# rows of the other sources come first, then a patient's con meds by first
# start until the rows are used up. Each patient's own count.
tl_fold <- function(parts, max_lanes) {
  cm <- parts$cm
  key <- paste(cm$USUBJID, cm$Row, sep = "\r")
  first <- tapply(cm$Start, key, min)
  k <- data.frame(key = names(first), first = as.numeric(first),
                  stringsAsFactors = FALSE)
  k$id <- cm$USUBJID[match(k$key, key)]
  k <- k[order(k$id, k$first, k$key), , drop = FALSE]
  k$rank <- seq_len(nrow(k)) - match(k$id, k$id) + 1L
  others <- unlist(lapply(parts[setdiff(names(parts), "cm")], function(p) {
    unique(paste(p$USUBJID, p$Row, sep = "\r"))
  }), use.names = FALSE)
  used <- table(factor(sub("\r.*", "", others), levels = unique(k$id)))
  n_cm <- table(factor(k$id, levels = unique(k$id)))
  room <- pmax(0L, max_lanes - as.integer(used[k$id]))
  over <- as.integer(n_cm[k$id]) > room
  # One row goes to "Other con meds" when they do not all fit.
  fold <- over & k$rank > pmax(0L, room - 1L)
  cm$Row[key %in% k$key[fold]] <- "Other con meds"
  cm
}

# A row name used by two sources of one patient gets the source added, so
# an AE and a con med of the same name stay two rows.
tl_unique_rows <- function(parts) {
  seen <- character()
  for (nm in intersect(c("ae", "cm", "trt", "resp"), names(parts))) {
    p <- parts[[nm]]
    key <- paste(p$USUBJID, p$Row, sep = "\r")
    clash <- key %in% seen
    if (any(clash)) {
      p$Row[clash] <- paste0(p$Row[clash], switch(nm, cm = " (con med)",
                                                    trt = " (treatment)",
                                                    resp = " (response)", ""))
      parts[[nm]] <- p
    }
    seen <- c(seen, unique(paste(p$USUBJID, p$Row, sep = "\r")))
  }
  parts
}

# Serious onset and death. A death date marks the patient row; a fatal AE
# without one marks death at its end, on its row.
tl_event_rows <- function(rec, sl, opt) {
  out <- list()
  ae <- which(rec$.src %in% "AE")
  if (identical(opt$serious, "marked at onset")) {
    s <- ae[rec$.ae_ser[ae] %in% 1]
    if (length(s)) {
      out$ser <- list(USUBJID = rec$USUBJID[s], Row = rec$Row[s],
                      .src = rep("AE", length(s)),
                      .event_x = rec$Start[s],
                      .event = rep("Serious AE", length(s)))
    }
  }
  if (isTRUE(opt$death)) {
    dth <- sl[!is.na(sl$DTHDT), , drop = FALSE]
    if (nrow(dth)) {
      out$dth <- list(USUBJID = dth$USUBJID, .event_x = aet_day(dth$DTHDT,
                                                                  dth$T0),
                      .event = rep("Death", nrow(dth)))
    }
    f <- ae[rec$.fatal[ae] & !rec$USUBJID[ae] %in% dth$USUBJID]
    f <- f[!duplicated(rec$USUBJID[f])]
    if (length(f)) {
      out$fat <- list(USUBJID = rec$USUBJID[f], Row = rec$Row[f],
                      .src = rep("AE", length(f)), .event_x = rec$End[f],
                      .event = rep("Death", length(f)))
    }
  }
  if (length(out)) tl_bind(out)
}

# One record per patient with no row: days on treatment, and the patient
# columns (filled in tl_finish()).
tl_patient_rows <- function(sl, opt) {
  d <- as.numeric(sl$TRTEDT) - as.numeric(sl$TRTSDT) + 1
  list(USUBJID = sl$USUBJID, .days_trt = ifelse(is.finite(d), d, NA_real_))
}

# --- finish -----------------------------------------------------------------

# Row order, the colour and lane factors and the patient columns.
tl_finish <- function(out, sl, opt, group) {
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  ids <- sl$USUBJID
  wl <- tl_worst_levels()
  per <- aet_per_patient(out$USUBJID, ids)
  n_ae <- per$sum(out$.ae %in% 1)
  n_open <- per$sum(out$.ae_open %in% 1)
  n_cm <- per$sum(out$.cm %in% 1)
  worst <- per$max(match(out$.worst, wl))
  first <- per$min(ifelse(out$.src %in% c("AE", "CM"), out$Start, NA_real_))
  days <- per$max(out$.days_trt)
  bor <- per$min(match(out$.bor, SWIMMER_BOR))
  prog <- per$min(ifelse(out$.event %in% "Progression" & is.na(out$Row),
                         out$.event_x, NA_real_))
  key <- switch(
    opt$order_by,
    "number of events" = order(-(n_ae + n_cm), ids),
    "number of AEs" = order(-n_ae, ids),
    "worst AE" = order(-worst, -n_ae, ids),
    "ongoing AEs" = order(-n_open, -n_ae, ids),
    "first onset" = order(first, ids),
    "number of con meds" = order(-n_cm, ids),
    "days on treatment" = order(-days, ids),
    "best response" = order(bor, -days, ids),
    "first progression" = order(prog, -days, ids),
    order(ids)
  )
  pos <- match(out$USUBJID, ids[key])
  # Within a patient: the patient record, then AEs, con meds and treatment
  # (or the sources in `opt$src_order`), each source's rows by first start.
  src <- match(out$.src, opt$src_order %||% c("AE", "CM", "Treatment"))
  src[is.na(src)] <- 0L
  rk <- paste(out$USUBJID, out$Row, sep = "\r")
  row_first <- stats::ave(ifelse(is.finite(out$Start), out$Start, Inf), rk,
                   FUN = min)
  o <- order(pos, src, row_first, out$Row, out$Start, method = "radix")
  out <- out[o, , drop = FALSE]
  rownames(out) <- NULL

  # One colour column over every source, each with its own palette.
  out$Colour <- tl_colour_factor(out, opt)
  # A record's lane: its source's, or the one it names (`.lane_at`, a drug's
  # lane on the patient timeline), those lanes first.
  ln <- unname(c(AE = "Adverse events", CM = "Con meds",
                 Treatment = "Treatment", Response = "Response")[out$.src])
  at <- !is.na(out$.lane_at)
  ln[at] <- out$.lane_at[at]
  out$.lane <- factor(ln, levels = unique(c(opt$sub_lanes,
                                            opt$lanes %||% TL_SHOW)))
  if (!is.null(opt$full_lanes)) out$.full <- ln %in% opt$full_lanes
  out$.worst <- factor(out$.worst, levels = wl)
  out$Event <- factor(out$.event, levels = opt$events %||%
                        c("Serious AE", "Fatal AE", "Death"))
  pr <- is.na(out$Row)
  pcols <- attr(sl, "pcols")
  out$.arm <- ifelse(pr, sl$.arm[match(out$USUBJID, sl$USUBJID)],
                     NA_character_)
  for (nm in pcols) {
    v <- sl[[nm]][match(out$USUBJID, sl$USUBJID)]
    v[!pr] <- NA
    lab <- attr(sl[[nm]], "label", exact = TRUE)
    out[[nm]] <- v
    if (!is.null(lab)) attr(out[[nm]], "label") <- lab
  }
  attr(out$USUBJID, "label") <- "Patient"
  attr(out$Row, "label") <- "Row"
  # An event's tooltip names its position by this label, as the swimmer's.
  attr(out$.event_x, "label") <- "Study day"
  keep <- c("USUBJID", "Row", ".arm", pcols, ".ae", ".ae_open", ".ae_ser",
            ".worst", ".cm", ".cm_new", ".days_trt", "Start", "End",
            ".open", "Colour", ".lane", ".style", ".label", "Type",
            "Details", "Days", ".bar_label", ".event_x", "Event",
            opt$extra)
  out[, keep, drop = FALSE]
}

# The levels of the worst-AE column: grades or severities.
tl_worst_levels <- function() {
  c(paste("Grade", 1:5), "Mild", "Moderate", "Severe")
}

# The colour factor: AE levels, then con-med levels, then treatment levels,
# each with its palette as `blockr_colors` and the values the levels stand
# for as `blockr_scale_keys`. Con meds take the room AEs and treatment leave,
# their rarer levels folded into "Other".
tl_colour_factor <- function(out, opt) {
  drawn <- is.finite(out$Start) & !is.na(out$.colour)
  n_lv <- function(s) length(unique(out$.colour[drawn & out$.src %in% s]))
  cm <- drawn & out$.src %in% "CM"
  # With response colours too, treatment takes one colour when the doses
  # would leave con meds less than two.
  trt <- drawn & out$.src %in% "Treatment"
  pal <- opt$trt_palette
  if (!is.null(pal)) {
    # Drug colours keep their own: con meds, then the drugs, give way.
    if (n_lv("AE") + n_lv("Response") + n_lv("Treatment") + 2L * any(cm) >
          TL_MAX_COLOURS && any(cm)) {
      out$.colour[cm] <- "Con meds"
      out$.svar[cm] <- NA_character_
    }
    if (n_lv("AE") + n_lv("Response") + n_lv("Treatment") + n_lv("CM") >
          TL_MAX_COLOURS) {
      out$.colour[trt] <- "Treatment"
      out$.svar[trt] <- NA_character_
      pal <- NULL
    }
  } else if (any(trt) && n_lv("Response") &&
        n_lv("AE") + n_lv("Response") + n_lv("Treatment") + 2L * any(cm) >
          TL_MAX_COLOURS) {
    out$.colour[trt] <- "Treatment"
    out$.svar[trt] <- NA_character_
  }
  if (any(cm) && !all(out$.colour[cm] %in% "Con meds")) {
    room <- min(TL_MAX_CM_COLOURS,
                TL_MAX_COLOURS - n_lv("AE") - n_lv("Treatment") -
                  n_lv("Response"))
    other <- switch(opt$cm_colour, class = "Other class",
                    indication = "Other indication", "Other")
    v <- tl_fold_top(out$.colour[cm], max(2L, room), other)
    out$.svar[cm][v != out$.colour[cm]] <- NA_character_
    out$.colour[cm] <- v
  }
  lv <- character()
  col <- character()
  for (s in c("Response", "AE", "CM", "Treatment")) {
    v <- unique(out$.colour[drawn & out$.src %in% s])
    if (!length(v)) next
    drug <- identical(s, "Treatment") && !is.null(pal)
    ord <- if (drug) names(pal)[names(pal) %in% v] else tl_level_order(s, v)
    lv <- c(lv, ord)
    col <- c(col, if (drug) unname(pal[ord]) else
      tl_palette(s, ord, opt$ae_palette))
  }
  f <- factor(out$.colour, levels = unique(lv))
  attr(f, "blockr_colors") <- stats::setNames(col, lv)[unique(lv)]
  attr(f, "blockr_scale_keys") <- tl_scale_keys(out[drawn, , drop = FALSE])
  attr(f, "label") <- "Colour"
  f
}

# Per colour level, the data variables and values it stands for.
tl_scale_keys <- function(out) {
  k <- !is.na(out$.svar) & !is.na(out$.skey) & nzchar(out$.skey)
  k <- unique(data.frame(level = out$.colour[k], svar = out$.svar[k],
                         key = out$.skey[k], stringsAsFactors = FALSE))
  vars <- TL_SCALE_VARS[k$svar]
  data.frame(level = rep(k$level, lengths(vars)),
             var = unlist(vars, use.names = FALSE),
             key = rep(k$key, lengths(vars)), stringsAsFactors = FALSE)
}

tl_level_order <- function(src, v) {
  if (identical(src, "Treatment")) {
    # Doses by amount ("54 mg", "50 to 100 mg"), then the rest by name.
    num <- suppressWarnings(as.numeric(sub(" .*", "", v)))
    return(v[order(v %in% TL_OTHER, is.na(num), num, v)])
  }
  fixed <- switch(
    src,
    AE = c(paste("Grade", 1:5), "Mild", "Moderate", "Severe", "AE"),
    Response = names(SWIMMER_RESPONSE),
    CM = c("Started before treatment", "Started on treatment", "Con meds"),
    character()
  )
  rest <- setdiff(v, c(fixed, TL_OTHER))
  c(fixed[fixed %in% v], sort(rest), TL_OTHER[TL_OTHER %in% v])
}

# `ae`: the AE colours by level, AET_COLOURS by default.
tl_palette <- function(src, lv, ae = NULL) {
  n <- length(lv)
  switch(
    src,
    AE = {
      p <- (ae %||% AET_COLOURS)[lv]
      p[is.na(p)] <- "#e07b39"
      unname(p)
    },
    Response = {
      p <- SWIMMER_RESPONSE[lv]
      p[is.na(p)] <- "#c9ced6"
      unname(p)
    },
    CM = {
      if (identical(lv, c("Started before treatment",
                          "Started on treatment"))) {
        c("#9db8de", "#3f6fb0")
      } else if (identical(lv, "Started on treatment")) {
        "#3f6fb0"
      } else if (identical(lv, "Started before treatment")) {
        "#9db8de"
      } else if (identical(lv, "Con meds")) {
        "#6b93c9"
      } else {
        p <- rep_len(TL_CM_BLUES, n)
        p[lv %in% TL_OTHER] <- "#b4bac3"
        p
      }
    },
    {
      # Light to dark with the dose.
      if (n == 1L) "#6b7280" else
        TL_TRT_GREYS[round(seq(1, length(TL_TRT_GREYS), length.out = n))]
    }
  )
}

# --- helpers shared with the AE parts ---------------------------------------

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

# Mockup palette: warm, darker with severity. Grades run past severe.
AET_COLOURS <- c(
  Mild = "#f0c35a", Moderate = "#e07b39", Severe = "#b8322a",
  "Grade 1" = "#f0c35a", "Grade 2" = "#e07b39", "Grade 3" = "#b8322a",
  "Grade 4" = "#7a1f1a", "Grade 5" = "#3b1210",
  AE = "#9aa3ad"
)

aet_col <- function(df, nms) {
  hit <- intersect(nms, names(df))
  if (length(hit)) df[[hit[[1L]]]]
}

# An ADaM flag as TRUE/FALSE: "Y"/"N" as ADaM ships it, or logical or 1/0
# as a reader or adapter may turn it. Anything else, NA included, is FALSE.
aet_flag <- function(x) {
  if (is.null(x)) return(logical())
  if (is.logical(x)) return(!is.na(x) & x)
  if (is.numeric(x)) return(!is.na(x) & x == 1)
  toupper(trimws(as.character(x))) %in% c("Y", "YES", "TRUE", "T", "1")
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

aet_title <- function(x) {
  x <- as.character(x)
  up <- !is.na(x) & x == toupper(x)
  x[up] <- paste0(substr(x[up], 1L, 1L), tolower(substring(x[up], 2L)))
  x
}

# What the default colours by: grade when ADAE has one, else severity, else
# nothing ("AE", one neutral colour). A column with no values does not count.
aet_colour_by <- function(ae, colour) {
  if (!identical(colour, "grade or severity")) return(colour)
  has <- function(nms) {
    v <- aet_col(ae, nms)
    !is.null(v) && any(!is.na(v) & nzchar(as.character(v)))
  }
  if (has(c("ATOXGR", "AETOXGR"))) return("Grade")
  if (has(c("ASEV", "AESEV"))) return("Severity")
  "AE"
}

# The colour column: severity words in clinical order, grades in numeric
# order, as "Grade 3", or one level "AE" when there is neither.
aet_grade <- function(ae, colour) {
  if (identical(colour, "AE")) {
    return(factor(rep("AE", nrow(ae)), levels = "AE"))
  }
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

# Per-patient summaries of record columns, by grouping the records once:
# count, sum, max and min of a column, one value per id in `ids`, named by
# id. Records of ids not in `ids` are ignored; an id without records counts
# 0 and has no max or min. Works for any interval records keyed by patient.
aet_per_patient <- function(id, ids) {
  g <- match(id, ids)
  ok <- !is.na(g)
  g <- g[ok]
  n <- length(ids)
  named <- function(x) stats::setNames(as.numeric(x), ids)
  ext <- function(x, f) {
    x <- x[ok]
    keep <- !is.na(x)
    out <- rep(NA_real_, n)
    if (any(keep)) {
      v <- vapply(split(x[keep], g[keep]), f, numeric(1L))
      out[as.integer(names(v))] <- v
    }
    named(out)
  }
  list(
    count = function() named(tabulate(g, n)),
    sum = function(x) {
      out <- numeric(n)
      s <- rowsum(as.numeric(x[ok]), g, reorder = TRUE)
      out[as.integer(rownames(s))] <- s[, 1L]
      named(out)
    },
    max = function(x) ext(x, max),
    min = function(x) ext(x, min)
  )
}
