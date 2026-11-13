# The swimmer's frame: ADaM tables in, one long frame out, which the
# swimmer's spans column draws (new_swimmer_block()). One row per patient in
# the table; in the frame, one record per thing drawn:
#
#   response   ADRS overall response, assessment to next assessment, on the
#              main lane (lane NA), coloured by the response read
#   exposure   ADEX, one thin lane per drug, coloured by the drug
#   dose marks a dose cut (step), a late dose (hollow, from the planned to
#              the actual date), a skipped cycle (dashed) or one ring per
#              administration (dot), by the `doses` setting
#   strip      one record per administration or dose period, drawn nowhere,
#              listed in the drug's hover strip
#   events     first progression, serious AEs (ADAE) and death, on the main
#              lane
#
# Time is weeks since the first dose (ADSL TRTSDT).

#' Swimmer frame from ADaM
#'
#' Turns a study's ADaM tables into the long frame the swimmer block draws:
#' study weeks, drugs ongoing to the cutoff, response periods, best overall
#' response and the dose marks. It is the swimmer block's prepare step
#' ([new_swimmer_block()]) and needs no prep of the inputs.
#'
#' @param data A `dm` (or a named list of data frames) with `adsl`, `adex`
#'   and `adrs` and, optional, `adae` (serious AEs as events) and `adcm`
#'   (con meds, with `con_meds = TRUE`). Names match case-insensitively.
#'   SDTM-style names work as in the patient profile: `dm`, `ex`, `rs`, `ae`
#'   and `cm`, and SDTM columns such as `EXSTDTC` or `RFXSTDTC`.
#' @param doses How a drug lane shows its doses: `"Plain"` (the period bar),
#'   `"Exceptions"` (a dose cut as a step down, a late dose as a notch, a
#'   skipped cycle as a dashed stretch) or `"Dots"` (a ring per
#'   administration; a daily drug stays a bar). Without administration dates
#'   (dose periods only) both show the dose cuts only.
#' @param con_meds Draw con meds from `adcm` as lanes too.
#' @param max_lanes Lanes per patient before drugs fold into one grey
#'   "Other" lane, listed on hover. Study drugs (`adex`) keep theirs.
#' @param parameter The ADRS parameter of the overall response per visit.
#' @param cutoff The data cutoff. `NULL` or `NA`: the last date in the data.
#'   Response visits, doses and deaths after it are left out.
#' @param drugs The study drugs to draw, as [swimmer_drugs()] names them.
#'   `NULL`: all.
#' @param events The events drawn on the response bar: any of
#'   `"Progression"`, `"Serious AE"`, `"Death"`. `NULL`: all.
#' @param unit The time axis, `"weeks"` or `"days"` since the first dose.
#'   The listing's weeks on treatment stay weeks.
#' @return A data frame, one record per drawn piece; see the source for the
#'   columns.
#' @export
swimmer_frame <- function(data, doses = "Exceptions", con_meds = FALSE,
                          max_lanes = 4L, parameter = "OVR", cutoff = NULL,
                          drugs = NULL, events = NULL, unit = "weeks") {
  doses <- match.arg(as.character(doses), c("Exceptions", "Plain", "Dots"))
  unit <- match.arg(as.character(unit), c("weeks", "days"))
  if (!length(parameter) || is.na(parameter[[1L]]) || !nzchar(parameter[[1L]])) {
    parameter <- "OVR"
  }
  max_lanes <- max(1L, as.integer(max_lanes %||% 4L))
  if (!length(cutoff) || is.na(cutoff[[1L]])) cutoff <- NULL
  tb <- swimmer_tables(data)
  sl <- swimmer_subjects(tb$adsl)
  rs <- swimmer_response(tb$adrs, parameter)
  # Without a cutoff set, the study's own data cutoff when ADSL has one.
  if (is.null(cutoff) && any(!is.na(sl$DCUTDT))) {
    cutoff <- max(sl$DCUTDT, na.rm = TRUE)
  }
  if (!is.null(cutoff)) {
    cutoff <- as.Date(cutoff)
    rs <- rs[rs$ADT <= cutoff, , drop = FALSE]
    sl$DTHDT[!is.na(sl$DTHDT) & sl$DTHDT > cutoff] <- NA
  }
  # The response population: subjects with an assessment.
  sl <- sl[sl$USUBJID %in% rs$USUBJID, , drop = FALSE]
  if (!nrow(sl)) {
    stop("No subject has an overall response (ADRS PARAMCD \"", parameter,
         "\").", call. = FALSE)
  }
  ex <- swimmer_exposure(tb$adex, sl, role = "study")
  if (!is.null(drugs)) {
    ex <- ex[ex$drug %in% as.character(drugs), , drop = FALSE]
  }
  ex <- swimmer_close_study(ex, sl)
  if (isTRUE(con_meds) && !is.null(tb$adcm)) {
    ex <- rbind(ex, swimmer_exposure(swimmer_cm(tb$adcm), sl, role = "cm"))
  }
  cutoff <- swimmer_cutoff(cutoff, sl, ex[ex$role == "study", ], rs)
  # Nothing after the cutoff: a drug given past it is ongoing at it.
  ex <- ex[ex$start <= cutoff, , drop = FALSE]
  ex$end[!is.na(ex$end) & ex$end > cutoff] <- NA
  ex$open <- is.na(ex$end)
  ex$end[ex$open] <- cutoff
  t0 <- stats::setNames(sl$TRTSDT, sl$USUBJID)
  # A patient with an end of treatment is on no study drug.
  ex$on <- swimmer_ongoing(ex, cutoff) &
    is.na(sl$TRTEDT[match(ex$USUBJID, sl$USUBJID)])
  fu <- swimmer_follow_up(sl, ex, rs, cutoff)
  ex <- swimmer_clip_cm(ex, t0, fu, cutoff)
  ex <- swimmer_fold(ex, max_lanes, t0)

  parts <- list(
    swimmer_resp_rows(rs, fu, t0),
    swimmer_drug_rows(ex, t0, doses, cutoff),
    swimmer_event_rows(rs, sl, tb$adae, t0)
  )
  out <- list2DF(swimmer_bind(parts))
  if (!is.null(events)) {
    out <- out[is.na(out$event) | out$event %in% as.character(events), ,
               drop = FALSE]
  }
  out <- swimmer_finish(out, sl, rs, ex, fu, cutoff, tb$adsl)
  if (unit == "days") out <- swimmer_in_days(out)
  out
}

# The time axis in days since the first dose: the weeks times seven.
swimmer_in_days <- function(out) {
  for (nm in c("start", "end", "event_wk")) {
    lab <- attr(out[[nm]], "label")
    out[[nm]] <- out[[nm]] * 7
    attr(out[[nm]], "label") <- sub("weeks?", "days", sub("Week", "Day", lab))
  }
  out
}

# --- inputs ----------------------------------------------------------------

swimmer_tables <- function(data) {
  tabs <- if (inherits(data, "dm")) dm::dm_get_tables(data) else data
  if (!is.list(tabs) || is.data.frame(tabs)) {
    stop("The swimmer needs a dm with adsl, adex and adrs.", call. = FALSE)
  }
  names(tabs) <- tolower(names(tabs))
  # The patient profile's names: SDTM-style tables (dm, ex, rs, ae, cm) and
  # SDTM columns (EXSTDTC, RFXSTDTC, ...) as their ADaM counterparts.
  tabs <- pp_normalize_tables(tabs)$tables
  get <- function(nm) {
    x <- tabs[[nm]]
    if (!is.null(x)) as.data.frame(x)
  }
  out <- list(adsl = get("adsl"), adex = get("adex"), adrs = get("adrs"),
              adae = get("adae"), adcm = get("adcm"))
  miss <- c("adsl", "adex", "adrs")[vapply(out[c("adsl", "adex", "adrs")],
                                            is.null, NA)]
  if (length(miss)) {
    stop("The swimmer needs ", paste(miss, collapse = ", "),
         " in its input dm.", call. = FALSE)
  }
  out
}

swimmer_col <- function(df, nms) {
  hit <- intersect(nms, names(df))
  if (length(hit)) df[[hit[[1L]]]]
}

swimmer_date <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "POSIXct")) return(as.Date(x))
  if (inherits(x, "Date")) return(x)
  as.Date(as.character(x))
}

swimmer_subjects <- function(adsl) {
  n <- nrow(adsl)
  na_date <- rep(as.Date(NA), n)
  data.frame(
    USUBJID = as.character(adsl$USUBJID),
    ARM = as.character(swimmer_col(adsl, c("ARM", "TRT01A", "ACTARM")) %||%
                         rep("All", n)),
    TRTSDT = swimmer_date(swimmer_col(adsl, "TRTSDT")) %||% na_date,
    TRTEDT = swimmer_date(swimmer_col(adsl, "TRTEDT")) %||% na_date,
    DCUTDT = swimmer_date(swimmer_col(adsl, "DCUTDT")) %||% na_date,
    DTHDT = swimmer_date(swimmer_col(adsl, "DTHDT")) %||% na_date,
    LSTALVDT = swimmer_date(swimmer_col(adsl, "LSTALVDT")) %||% na_date,
    stringsAsFactors = FALSE
  )
}

# Overall response by visit, one row per assessment with a date.
swimmer_response <- function(adrs, parameter) {
  pc <- swimmer_col(adrs, "PARAMCD")
  r <- if (is.null(pc)) adrs else adrs[pc %in% parameter, , drop = FALSE]
  out <- data.frame(
    USUBJID = as.character(r$USUBJID),
    ADT = swimmer_date(swimmer_col(r, c("ADT", "ADTM"))),
    AVALC = toupper(as.character(r$AVALC)),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$ADT) & !is.na(out$AVALC) &
               !out$AVALC %in% c("", "MISSING"), , drop = FALSE]
  out[order(out$USUBJID, out$ADT), , drop = FALSE]
}

# Exposure records: drug, start, end (NA: ongoing), dose and unit. ADEX keeps
# its dose records (PARAMCD DOSE) when it has parameters.
swimmer_exposure <- function(adex, sl, role) {
  pc <- swimmer_col(adex, "PARAMCD")
  if (!is.null(pc) && "DOSE" %in% pc) adex <- adex[pc %in% "DOSE", ,
                                                    drop = FALSE]
  adex <- adex[as.character(adex$USUBJID) %in% sl$USUBJID, , drop = FALSE]
  start <- swimmer_date(swimmer_col(adex, c("ASTDT", "ASTDTM")))
  end <- swimmer_date(swimmer_col(adex, c("AENDT", "AENDTM")))
  dose <- swimmer_col(adex, c("EXDOSE", "CMDOSE")) %||% rep(NA_real_,
                                                            nrow(adex))
  out <- data.frame(
    USUBJID = as.character(adex$USUBJID),
    drug = swimmer_drug_name(as.character(swimmer_col(adex, c("EXTRT",
                                                              "CMDECOD")))),
    start = start, end = end,
    dose = suppressWarnings(as.numeric(dose)),
    unit = as.character(swimmer_col(adex, c("EXDOSU", "CMDOSU")) %||%
                          rep("", nrow(adex))),
    role = rep(role, nrow(adex)),
    stringsAsFactors = FALSE
  )
  out$unit[is.na(out$unit)] <- ""
  out <- out[!is.na(out$start) & !is.na(out$drug), , drop = FALSE]
  out[order(out$USUBJID, out$drug, out$start), , drop = FALSE]
}

#' The study drugs of a swimmer's input
#'
#' The drug names in ADEX as the swimmer draws them, for the drugs setting.
#'
#' @param data The swimmer's input dm.
#' @return A character vector, sorted.
#' @export
swimmer_drugs <- function(data) {
  tb <- tryCatch(swimmer_tables(data), error = function(e) NULL)
  if (is.null(tb)) return(character())
  ex <- tb$adex
  pc <- swimmer_col(ex, "PARAMCD")
  if (!is.null(pc) && "DOSE" %in% pc) ex <- ex[pc %in% "DOSE", , drop = FALSE]
  d <- swimmer_drug_name(as.character(swimmer_col(ex, c("EXTRT", "CMDECOD"))))
  sort(unique(d[!is.na(d)]))
}

# "XANOMELINE" reads as "Xanomeline"; mixed case stays. Each name once:
# toTitleCase() is slow, and a con med table repeats its names many times.
swimmer_drug_name <- function(x) {
  lv <- unique(x)
  nm <- lv
  up <- !is.na(lv) & lv == toupper(lv)
  nm[up] <- tools::toTitleCase(tolower(lv[up]))
  nm[match(x, lv)]
}

swimmer_cm <- function(adcm) {
  dec <- as.character(swimmer_col(adcm, c("CMDECOD", "CMTRT")))
  adcm <- adcm[!is.na(dec) & dec != "UNCODED", , drop = FALSE]
  adcm$CMDECOD <- as.character(swimmer_col(adcm, c("CMDECOD", "CMTRT")))
  # Ongoing con meds have no end.
  adcm
}

# A study drug record without an end stops at the patient's end of
# treatment (ADSL TRTEDT), or at its own start when it begins after it; it
# stays open only while the patient has no end of treatment.
swimmer_close_study <- function(ex, sl) {
  te <- sl$TRTEDT[match(ex$USUBJID, sl$USUBJID)]
  fix <- ex$role == "study" & is.na(ex$end) & !is.na(te)
  ex$end[fix] <- as.Date(pmax(as.numeric(ex$start[fix]), as.numeric(te[fix])),
                         origin = "1970-01-01")
  ex
}

swimmer_cutoff <- function(cutoff, sl, ex, rs) {
  if (!is.null(cutoff)) return(as.Date(cutoff))
  d <- c(ex$start, ex$end, rs$ADT, sl$LSTALVDT, sl$DTHDT)
  max(d, na.rm = TRUE)
}

# Each patient's end of follow-up: death, the cutoff while any study drug
# runs, else the end of treatment or the last assessment, whichever is
# later (the last alive date only when neither is known).
swimmer_follow_up <- function(sl, ex, rs, cutoff) {
  ids <- sl$USUBJID
  n <- length(ids)
  study <- ex$role == "study"
  on <- tabulate(match(ex$USUBJID[study & ex$on], ids), n) > 0L
  seen <- pmax(as.numeric(sl$TRTEDT),
               swimmer_grp_max(match(rs$USUBJID, ids), as.numeric(rs$ADT), n),
               swimmer_grp_max(match(ex$USUBJID[study], ids),
                               as.numeric(ex$end[study]), n),
               na.rm = TRUE)
  alive <- as.numeric(sl$LSTALVDT)
  alive[is.na(alive)] <- -Inf
  seen[is.na(seen)] <- alive[is.na(seen)]
  out <- ifelse(!is.na(sl$DTHDT), as.numeric(sl$DTHDT),
                ifelse(on, as.numeric(cutoff), seen))
  stats::setNames(out, ids)
}

# Whether each record's drug is still given at the cutoff: its last period
# has no end, or, given in cycles, the next cycle would fall after the
# cutoff.
swimmer_ongoing <- function(ex, cutoff) {
  if (!nrow(ex)) return(logical())
  gr <- swimmer_groups(ex)
  s <- as.numeric(ex$start)[gr$o]
  open <- ex$open[gr$o]
  every <- swimmer_every(s, as.numeric(ex$end)[gr$o], open, gr)
  on <- tabulate(gr$g[open], gr$n) > 0L |
    (!is.na(every) &
       swimmer_grp_max(gr$g, s, gr$n) + 1.5 * every >= as.numeric(cutoff))
  on[gr$id]
}

# --- groups ------------------------------------------------------------------

# The records of each patient's drug: `id` the group of each record, in the
# order the groups first appear, `o` the records sorted by group (keeping
# their order within it), `g` the group of each sorted record, `first` and
# `last` whether a sorted record starts or ends its group.
swimmer_groups <- function(ex) {
  key <- paste(ex$USUBJID, ex$drug, sep = "\r")
  id <- match(key, unique(key))
  o <- order(id, method = "radix")
  g <- id[o]
  m <- length(g)
  brk <- g[-1L] != g[-m]
  list(id = id, o = o, g = g, n = if (m) max(g) else 0L,
       first = c(TRUE, brk)[seq_len(m)], last = c(brk, TRUE)[seq_len(m)])
}

# Per group 1..n, the largest of x (NA where it has none).
swimmer_grp_max <- function(g, x, n) {
  out <- rep(NA_real_, n)
  ok <- !is.na(g) & !is.na(x)
  if (!any(ok)) return(out)
  g <- g[ok]
  x <- x[ok]
  o <- order(g, x, method = "radix")
  g <- g[o]
  top <- !duplicated(g, fromLast = TRUE)
  out[g[top]] <- x[o][top]
  out
}

# Per group, the planned interval of a drug given in cycles, NA when its
# records are dose periods: a cycle drug has one date per record (start =
# end), at least two, further apart than daily. The interval is the most
# common gap, the shorter on a tie, since a delay only lengthens one.
# `s`, `end`, `open`: the sorted records of swimmer_groups() `gr`.
swimmer_every <- function(s, end, open, gr) {
  g <- gr$g
  n <- gr$n
  every <- rep(NA_real_, n)
  cand <- tabulate(g, n) >= 2L & tabulate(g[open | end != s], n) == 0L
  use <- !gr$first & cand[g]
  if (!any(use)) return(every)
  gap <- (s - c(NA, s[-length(s)]))[use]
  gg <- g[use]
  o <- order(gg, gap, method = "radix")
  gg <- gg[o]
  gap <- gap[o]
  m <- length(gg)
  run <- c(TRUE, gg[-1L] != gg[-m] | gap[-1L] != gap[-m])
  cnt <- tabulate(cumsum(run))
  rg <- gg[run]
  rgap <- gap[run]
  o2 <- order(rg, -cnt, rgap, method = "radix")
  pick <- o2[!duplicated(rg[o2])]
  every[rg[pick]] <- rgap[pick]
  every[!is.na(every) & every <= 2] <- NA
  every
}

# Con meds within follow-up: from four weeks before the first dose to the
# end of follow-up, open only when that is the cutoff.
swimmer_clip_cm <- function(ex, t0, fu, cutoff) {
  cm <- which(ex$role == "cm")
  if (!length(cm)) return(ex)
  lo <- as.numeric(t0[ex$USUBJID[cm]]) - 28
  hi <- fu[ex$USUBJID[cm]]
  keep <- as.numeric(ex$end[cm]) >= lo & as.numeric(ex$start[cm]) <= hi
  past <- as.numeric(ex$end[cm]) > hi
  ex$start[cm] <- as.Date(pmax(as.numeric(ex$start[cm]), lo),
                          origin = "1970-01-01")
  ex$end[cm] <- as.Date(pmin(as.numeric(ex$end[cm]), hi),
                        origin = "1970-01-01")
  ex$open[cm] <- ex$open[cm] & past & hi >= as.numeric(cutoff)
  drop <- cm[!keep]
  if (length(drop)) ex[-drop, , drop = FALSE] else ex
}

# When a patient has more than `max_lanes` drugs, the con meds fold into
# one "Other" lane; study drugs always keep theirs. The rule is the same for
# every patient, so a drug keeps its place down the column. Lanes run study
# drugs first, the one that starts earliest (median first dose) on top.
swimmer_fold <- function(ex, max_lanes, t0) {
  first <- tapply(seq_len(nrow(ex)), ex$drug, function(i) {
    e <- ex[i, , drop = FALSE]
    s <- tapply(as.numeric(e$start), e$USUBJID, min)
    stats::median(s - as.numeric(t0[names(s)]))
  })
  role <- tapply(ex$role, ex$drug, function(x) x[[1L]])
  ord <- names(first)[order(role[names(first)] != "study", first,
                            names(first))]
  per <- tapply(ex$drug, ex$USUBJID, function(x) length(unique(x)))
  ex$lane <- ex$drug
  if (length(per) && max(per) > max_lanes && any(role != "study")) {
    keep <- ord[role[ord] == "study"]
    ex$lane[!ex$drug %in% keep] <- "Other"
    ord <- c(keep, "Other")
  }
  ex$lane <- factor(ex$lane, levels = ord)
  ex
}

# --- records ---------------------------------------------------------------

SWIMMER_NA_ROW <- list(
  start = NA_real_, end = NA_real_, colour = NA_character_,
  lane = NA_character_, mark = NA_character_, strip = NA_character_,
  label = NA_character_, event_wk = NA_real_, event = NA_character_,
  ongoing = NA, days = NA_character_, dose = NA_character_
)

# Records as a list of columns, every column present. swimmer_bind() joins
# such lists and swimmer_frame() makes the data frame once at the end: a
# data frame per drug and patient, bound with rbind(), took half the time.
swimmer_rows <- function(USUBJID, ...) {
  n <- length(USUBJID)
  if (!n) return(NULL)
  cols <- utils::modifyList(SWIMMER_NA_ROW, list(...))
  c(list(USUBJID = USUBJID), lapply(cols, function(v) rep_len(v, n)))
}

# A list of swimmer_rows() results as one, in order. NULLs are left out.
swimmer_bind <- function(sets) {
  sets <- sets[!vapply(sets, is.null, NA)]
  if (!length(sets)) return(NULL)
  nms <- names(sets[[1L]])
  stats::setNames(lapply(nms, function(nm) {
    unlist(lapply(sets, `[[`, nm), use.names = FALSE)
  }), nms)
}

# swimmer_bind() of `sets`, then in order of group and place: `g` and `at`
# per set, a value per row or one for all its rows.
swimmer_bind_in <- function(sets, g, at) {
  n <- vapply(sets, function(x) if (is.null(x)) 0L else length(x[[1L]]), 0L)
  out <- swimmer_bind(sets)
  if (is.null(out)) return(NULL)
  o <- order(unlist(Map(rep_len, g, n)), unlist(Map(rep_len, at, n)),
             method = "radix")
  lapply(out, `[`, o)
}

swimmer_wk <- function(d, t0) (as.numeric(d) - as.numeric(t0)) / 7

# CDISC study day: day 1 is the first dose, no day 0.
swimmer_day <- function(d, t0) {
  k <- as.numeric(d) - as.numeric(t0)
  ifelse(k >= 0, k + 1, k)
}

swimmer_days <- function(s, e, t0, open = FALSE) {
  open <- rep_len(open, length(s))
  paste0("Day ", swimmer_day(s, t0), " to ",
         ifelse(open, "ongoing", swimmer_day(e, t0)))
}

# Assessment to next assessment; before the first, "Not assessed"; the last
# runs to the end of follow-up. `rs` is sorted by patient and date.
swimmer_resp_rows <- function(rs, fu, t0) {
  m <- nrow(rs)
  if (!m) return(NULL)
  id <- rs$USUBJID
  tt <- as.numeric(t0[id])
  adt <- as.numeric(rs$ADT)
  first <- c(TRUE, id[-1L] != id[-m])
  last <- c(id[-1L] != id[-m], TRUE)
  nx <- c(adt[-1L], NA)
  nx[last] <- pmax(fu[id[last]], adt[last])
  pat <- cumsum(first)
  pre <- which(first & adt > tt)
  sets <- list(
    swimmer_rows(id[pre], start = 0, end = swimmer_wk(adt[pre], tt[pre]),
                 colour = "Not assessed", label = "Not assessed",
                 days = swimmer_days(tt[pre], adt[pre], tt[pre])),
    swimmer_rows(id, start = swimmer_wk(adt, tt), end = swimmer_wk(nx, tt),
                 colour = rs$AVALC, label = rs$AVALC,
                 days = swimmer_days(adt, nx, tt))
  )
  swimmer_bind_in(sets, list(pat[pre], pat), list(1L, 2L))
}

# First progression and death on the response lane; serious AEs when ADAE
# is there.
swimmer_event_rows <- function(rs, sl, adae, t0) {
  pd <- rs[rs$AVALC == "PD", , drop = FALSE]
  pd <- pd[!duplicated(pd$USUBJID), , drop = FALSE]
  out <- list(swimmer_rows(pd$USUBJID, event_wk = swimmer_wk(pd$ADT, t0[pd$USUBJID]),
                           event = "Progression"))
  if (!is.null(adae) && "AESER" %in% names(adae)) {
    ae <- adae[adae$AESER %in% "Y" & as.character(adae$USUBJID) %in% sl$USUBJID,
               , drop = FALSE]
    d <- swimmer_date(swimmer_col(ae, "ASTDT"))
    ok <- !is.na(d)
    if (any(ok)) {
      ids <- as.character(ae$USUBJID[ok])
      out <- c(out, list(swimmer_rows(
        ids, event_wk = swimmer_wk(d[ok], t0[ids]), event = "Serious AE"
      )))
    }
  }
  dth <- sl[!is.na(sl$DTHDT), , drop = FALSE]
  out <- c(out, list(swimmer_rows(
    dth$USUBJID, event_wk = swimmer_wk(dth$DTHDT, t0[dth$USUBJID]),
    event = "Death"
  )))
  swimmer_bind(out)
}

# A dose with its unit, "" without a dose. Each value is formatted on its
# own: a drug's doses do not change how one another print.
swimmer_fmt_dose <- function(dose, unit) {
  lv <- unique(dose[!is.na(dose)])
  txt <- vapply(lv, format, "", trim = TRUE, drop0trailing = TRUE)
  out <- trimws(paste(txt[match(dose, lv)], unit))
  out[is.na(dose)] <- ""
  out
}

# --- drug lanes ------------------------------------------------------------

# Every drug of every patient: its period bar, its dose marks by `doses`, and
# its strip. A folded con med draws in grey on the "Other" lane; a drug given
# in cycles (swimmer_every()) runs from the first dose to a cycle after the
# last, with its cuts, late doses and skipped cycles or a ring per dose; any
# other drug draws its dose periods, daily doses merged. All patients and
# drugs at once; the records come out by drug and patient as the drugs first
# appear in `ex`, each with its bar, marks and strip.
swimmer_drug_rows <- function(ex, t0, doses, cutoff) {
  if (!nrow(ex)) return(NULL)
  gr <- swimmer_groups(ex)
  x <- ex[gr$o, , drop = FALSE]
  g <- gr$g
  x$lane <- as.character(x$lane)
  tt <- as.numeric(t0[x$USUBJID])
  s <- as.numeric(x$start)
  e <- as.numeric(x$end)
  other <- (x$lane %in% "Other")[gr$first]
  every <- swimmer_every(s, e, x$open, gr)
  every[other] <- NA
  dose <- swimmer_fmt_dose(x$dose, x$unit)
  pieces <- c(
    swimmer_other_rows(x, which(other[g]), s, e, tt, dose, gr),
    swimmer_period_rows(x, which(!other[g] & is.na(every[g])), s, e, tt,
                        dose, gr, doses),
    swimmer_admin_rows(x, which(!is.na(every[g])), s, tt, dose, gr,
                       every[g], doses, cutoff)
  )
  swimmer_bind_in(lapply(pieces, `[[`, "rows"), lapply(pieces, `[[`, "g"),
                  lapply(pieces, `[[`, "at"))
}

# One piece of the drug lanes: rows, the group of each, and its place in the
# group (10 bar, 20 to 23 marks, 30 to 32 strip).
swimmer_piece <- function(rows, g, at) list(list(rows = rows, g = g, at = at))

# Folded drugs: their periods in grey on the "Other" lane, named in the
# strip. `i`: their sorted records.
swimmer_other_rows <- function(x, i, s, e, tt, dose, gr) {
  if (!length(i)) return(list())
  open <- x$open[i]
  c(
    swimmer_piece(swimmer_rows(
      x$USUBJID[i], start = swimmer_wk(s[i], tt[i]),
      end = swimmer_wk(e[i], tt[i]), colour = "Other", lane = "Other",
      label = x$drug[i], ongoing = gr$last[i] & open,
      days = swimmer_days(s[i], e[i], tt[i], open), dose = dose[i]
    ), gr$g[i], 10L),
    swimmer_piece(swimmer_rows(
      x$USUBJID[i], start = swimmer_wk(s[i], tt[i]), lane = "Other",
      strip = paste0(x$drug[i], "\nd ", swimmer_day(s[i], tt[i]), "\u2013",
                     ifelse(open, "ongoing", swimmer_day(e[i], tt[i])))
    ), gr$g[i], 30L)
  )
}

# Drugs given in dose periods. Daily administrations (start = end, two or
# more) first become periods: a run of the same dose without a gap is one,
# ending at its last day. A cut in the dose is a step (by `doses`).
swimmer_period_rows <- function(x, i, s, e, tt, dose, gr, doses) {
  if (!length(i)) return(list())
  g <- gr$g
  daily <- tabulate(g[i], gr$n) >= 2L &
    tabulate(g[i][which(e[i] != s[i])], gr$n) == 0L
  prev <- c(NA, s[-length(s)])
  prev_dose <- c(NA, x$dose[-length(s)])
  brk <- gr$first | !daily[g] | s - prev > 2 | x$dose != prev_dose
  brk <- brk[i] %in% TRUE | gr$first[i]
  run <- cumsum(brk)
  end <- e[i]
  merge <- daily[g[i]]
  end[merge] <- swimmer_grp_max(run, s[i], max(run))[run[merge]]
  k <- i[brk]
  end <- end[brk]
  gk <- g[k]
  m <- length(k)
  last <- c(gk[-1L] != gk[-m], TRUE)
  open <- x$open[k]
  lane <- x$lane[k]
  out <- c(
    swimmer_piece(swimmer_rows(
      x$USUBJID[k], start = swimmer_wk(s[k], tt[k]),
      end = swimmer_wk(end, tt[k]), colour = x$drug[k], lane = lane,
      label = x$drug[k], ongoing = last & open,
      days = swimmer_days(s[k], end, tt[k], open), dose = dose[k]
    ), gk, 10L),
    swimmer_piece(swimmer_rows(
      x$USUBJID[k], start = swimmer_wk(s[k], tt[k]), lane = lane,
      strip = paste0(ifelse(nzchar(dose[k]), paste0(dose[k], "\n"), ""), "d ",
                     swimmer_day(s[k], tt[k]), "\u2013",
                     ifelse(open, "ongoing", swimmer_day(end, tt[k])))
    ), gk, 30L)
  )
  if (identical(doses, "Exceptions") && m > 1L) {
    j <- which(c(FALSE, gk[-1L] == gk[-m] & x$dose[k][-1L] < x$dose[k][-m]))
    ci <- k[j]
    out <- c(out, swimmer_piece(swimmer_rows(
      x$USUBJID[ci], start = swimmer_wk(s[ci], tt[ci]), lane = lane[j],
      colour = x$drug[ci], mark = "step", label = x$drug[ci],
      days = paste("Day", swimmer_day(s[ci], tt[ci])),
      dose = paste0(dose[k][j - 1L], " to ", dose[ci])
    ), gk[j], 20L))
  }
  out
}

# Drugs given in cycles: the bar from the first dose to a cycle after the
# last (to the cutoff while ongoing), and by `doses` its cuts, late doses and
# skipped cycles, or a ring per dose. The strip lists every cycle. `every`:
# the interval of each sorted record's drug.
swimmer_admin_rows <- function(x, i, s, tt, dose, gr, every, doses, cutoff) {
  if (!length(i)) return(list())
  g <- gr$g[i]
  d <- s[i]
  tt <- tt[i]
  every <- every[i]
  dose <- dose[i]
  raw <- x$dose[i]
  lane <- x$lane[i]
  drug <- x$drug[i]
  id <- x$USUBJID[i]
  first <- gr$first[i]
  m <- length(i)
  start_at <- which(first)
  pos <- seq_len(m) - rep(start_at, diff(c(start_at, m + 1L))) + 1L
  # Per dose: cycles skipped before it, and how many days late it came.
  prev <- c(NA, d[-m])
  gap <- ifelse(first, every, d - prev)
  skip <- pmax(0, floor((gap + every / 2) / every) - 1)
  plan <- ifelse(first, d, prev + (skip + 1) * every)
  late <- round(d - plan)
  late[late <= 2] <- 0
  day <- swimmer_day(d, tt)
  # The bar, one per drug.
  f <- start_at
  l <- c(start_at[-1L] - 1L, m)
  n <- l - f + 1L
  ongoing <- x$on[i][f]
  last <- ifelse(ongoing, as.numeric(cutoff), d[l] + every[f])
  out <- swimmer_piece(swimmer_rows(
    id[f], start = swimmer_wk(d[f], tt[f]), end = swimmer_wk(last, tt[f]),
    colour = drug[f], lane = lane[f], label = drug[f], ongoing = ongoing,
    days = swimmer_days(d[f], last, tt[f], ongoing),
    dose = paste0(n, " doses, every ", every[f], " days")
  ), g[f], 10L)
  # Skipped cycles: after the dose before, one per interval.
  sk_i <- rep(seq_len(m), skip)
  sk <- prev[sk_i] + sequence(skip[skip > 0]) * every[sk_i]
  if (identical(doses, "Dots")) {
    out <- c(out, swimmer_piece(swimmer_rows(
      id, start = swimmer_wk(d, tt), lane = lane, colour = drug, mark = "dot",
      label = drug, days = paste("Day", day), dose = dose
    ), g, 21L))
  } else if (identical(doses, "Exceptions")) {
    cut <- which(!first & c(FALSE, raw[-1L] < raw[-m]))
    lt <- which(late > 0)
    out <- c(
      out,
      swimmer_piece(swimmer_rows(
        id[cut], start = swimmer_wk(d[cut], tt[cut]), lane = lane[cut],
        colour = drug[cut], mark = "step", label = drug[cut],
        days = paste("Day", day[cut]),
        dose = paste0(dose[cut - 1L], " to ", dose[cut])
      ), g[cut], 21L),
      swimmer_piece(swimmer_rows(
        id[lt], start = swimmer_wk(plan[lt], tt[lt]),
        end = swimmer_wk(d[lt], tt[lt]), lane = lane[lt],
        colour = "Late dose", mark = "hollow", label = "Late dose",
        days = paste0("Cycle ", pos[lt], ", day ", day[lt], ", ", late[lt],
                      " days late")
      ), g[lt], 22L),
      swimmer_piece(swimmer_rows(
        id[sk_i], start = swimmer_wk(sk, tt[sk_i]),
        end = swimmer_wk(sk + every[sk_i], tt[sk_i]), lane = lane[sk_i],
        colour = drug[sk_i], mark = "dashed", label = drug[sk_i],
        days = paste("Skipped, planned day", swimmer_day(sk, tt[sk_i]))
      ), g[sk_i], 23L)
    )
  }
  c(
    out,
    swimmer_piece(swimmer_rows(
      id, start = swimmer_wk(d, tt), lane = lane,
      strip = paste0("C", pos, "\n", dose, "\nd ", day,
                     ifelse(late > 0, paste0("\n+", late, " d"), "")),
      mark = ifelse(late > 0, "hollow", "solid")
    ), g, 31L),
    swimmer_piece(swimmer_rows(
      id[sk_i], start = swimmer_wk(sk, tt[sk_i]), lane = lane[sk_i],
      strip = paste0("skip\nd ", swimmer_day(sk, tt[sk_i])), mark = "dashed"
    ), g[sk_i], 32L)
  )
}

# --- finish ----------------------------------------------------------------

SWIMMER_RESPONSE <- c("Not assessed" = "#e4e7ec", CR = "#1d7a55",
                      PR = "#74bf95", SD = "#dcbd66",
                      "NON-CR/NON-PD" = "#ece2c6", PD = "#e3958c",
                      NE = "#c9ced6")
SWIMMER_DRUGS <- c("#3f63a6", "#2a9686", "#a8649a", "#c08a3e", "#7d8b4a",
                   "#5f8fa8", "#b0707a", "#8a7bb0")
SWIMMER_BOR <- c("CR", "PR", "SD", "NON-CR/NON-PD", "PD", "NE")

# The listing columns, the colour and lane levels with their colours, and
# the labels the tooltips print.
SWIMMER_EVENTS <- c("Progression", "Serious AE", "Death")

swimmer_finish <- function(out, sl, rs, ex, fu, cutoff, adsl = NULL) {
  ids <- sl$USUBJID
  n <- length(ids)
  # Best response: the best read in SWIMMER_BOR order.
  rank <- match(rs$AVALC, SWIMMER_BOR)
  best <- -swimmer_grp_max(match(rs$USUBJID, ids), -rank, n)
  bor <- SWIMMER_BOR[best]
  study <- ex[ex$role == "study", , drop = FALSE]
  at <- match(study$USUBJID, ids)
  has <- tabulate(at, n) > 0L
  on <- tabulate(at[study$on], n) > 0L
  # Time on treatment: to the cutoff while a study drug runs, else to the end
  # of treatment (the last dose without one).
  end <- ifelse(on, as.numeric(cutoff),
                ifelse(!is.na(sl$TRTEDT), as.numeric(sl$TRTEDT),
                       swimmer_grp_max(at, as.numeric(study$end), n)))
  wk <- round(swimmer_wk(end, sl$TRTSDT), 1)
  wk[!has & is.na(sl$TRTEDT)] <- NA_real_
  status <- ifelse(!is.na(sl$DTHDT), "Died",
                   ifelse(on, "On treatment", "Off treatment"))
  m <- match(out$USUBJID, ids)
  out$ARM <- sl$ARM[m]
  out$BOR <- bor[m]
  out$weeks <- wk[m]
  out$status <- status[m]
  # Columns the listing offers besides (age, sex, the first study dose).
  ids <- as.character(adsl$USUBJID)
  at <- match(out$USUBJID, ids)
  out$AGE <- swimmer_col(adsl, "AGE")[at] %||% rep(NA_real_, nrow(out))
  out$SEX <- as.character(swimmer_col(adsl, "SEX")[at] %||%
                            rep(NA_character_, nrow(out)))
  first <- study[order(study$USUBJID, study$start), , drop = FALSE]
  first <- first[!duplicated(first$USUBJID), , drop = FALSE]
  lvl <- stats::setNames(swimmer_fmt_dose(first$dose, first$unit), first$USUBJID)
  out$dose_level <- unname(lvl[out$USUBJID])
  out <- swimmer_adsl_groups(out, adsl)

  drugs <- levels(ex$lane)
  dcol <- stats::setNames(rep_len(SWIMMER_DRUGS, length(drugs)), drugs)
  dcol[grepl("placebo", drugs, ignore.case = TRUE)] <- "#8f9bb3"
  dcol[drugs == "Other"] <- "#a3a9b4"
  cm <- setdiff(unique(ex$drug), drugs)
  cols <- c(SWIMMER_RESPONSE, dcol, "Late dose" = "#d98a1e",
            stats::setNames(rep("#a3a9b4", length(cm)), cm))
  lv <- unique(c(names(SWIMMER_RESPONSE), drugs, "Late dose", cm))
  lv <- lv[lv %in% out$colour]
  out$colour <- factor(out$colour, levels = lv)
  attr(out$colour, "blockr_colors") <- cols[lv]
  out$lane <- factor(out$lane, levels = drugs)
  out$event <- factor(out$event, levels = SWIMMER_EVENTS)
  labels <- c(start = "Start, weeks", end = "End, weeks",
              colour = "Response / drug",
              lane = "Drug", mark = "Mark", strip = "Strip", label = "Label",
              event_wk = "Week", event = "Event", ongoing = "Ongoing",
              days = "Study days", dose = "Dose", BOR = "Best response",
              weeks = "Weeks on treatment", status = "Status", ARM = "Arm",
              USUBJID = "Subject", AGE = "Age", SEX = "Sex",
              dose_level = "First dose")
  for (nm in names(labels)) attr(out[[nm]], "label") <- labels[[nm]]
  rownames(out) <- NULL
  out
}

# ADSL's grouping columns on every record (TRT, RACE, a region ...), so the
# rows can be grouped by any of them: character, factor or logical columns
# with at most 50 values, besides those the frame already has.
swimmer_adsl_groups <- function(out, adsl) {
  if (is.null(adsl) || !nrow(adsl)) return(out)
  at <- match(out$USUBJID, as.character(adsl$USUBJID))
  for (nm in setdiff(names(adsl), names(out))) {
    x <- adsl[[nm]]
    if (!(is.character(x) || is.factor(x) || is.logical(x))) next
    if (length(unique(x)) > 50L) next
    v <- x[at]
    attr(v, "label") <- attr(x, "label", exact = TRUE)
    out[[nm]] <- v
  }
  out
}
