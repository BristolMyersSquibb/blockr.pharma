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
  ex$on <- swimmer_ongoing(ex, cutoff)
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
  start <- swimmer_date(swimmer_col(adex, c("ASTDT", "EXSTDT", "ASTDTM")))
  end <- swimmer_date(swimmer_col(adex, c("AENDT", "EXENDT", "AENDTM")))
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

swimmer_cutoff <- function(cutoff, sl, ex, rs) {
  if (!is.null(cutoff)) return(as.Date(cutoff))
  d <- c(ex$start, ex$end, rs$ADT, sl$LSTALVDT, sl$DTHDT)
  max(d, na.rm = TRUE)
}

# Each patient's end of follow-up: death, the cutoff while any study drug
# runs, else the last date seen.
swimmer_follow_up <- function(sl, ex, rs, cutoff) {
  vapply(sl$USUBJID, function(id) {
    s <- sl[sl$USUBJID == id, ]
    if (!is.na(s$DTHDT)) return(as.numeric(s$DTHDT))
    e <- ex[ex$USUBJID == id & ex$role == "study", , drop = FALSE]
    if (any(e$on)) return(as.numeric(cutoff))
    as.numeric(max(c(s$LSTALVDT, rs$ADT[rs$USUBJID == id], e$end),
                   na.rm = TRUE))
  }, numeric(1L))
}

# Whether each record's drug is still given at the cutoff: its last period
# has no end, or, given in cycles, the next cycle would fall after the
# cutoff.
swimmer_ongoing <- function(ex, cutoff) {
  key <- paste(ex$USUBJID, ex$drug, sep = "\r")
  on <- vapply(split(seq_len(nrow(ex)), key), function(i) {
    e <- ex[i, , drop = FALSE]
    if (any(e$open)) return(TRUE)
    adm <- swimmer_admin(e)
    !is.null(adm) &&
      max(as.numeric(adm$date)) + 1.5 * adm$every >= as.numeric(cutoff)
  }, NA)
  unname(on[key])
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
# runs to the end of follow-up.
swimmer_resp_rows <- function(rs, fu, t0) {
  out <- lapply(split(rs, rs$USUBJID), function(r) {
    id <- r$USUBJID[[1L]]
    tt <- t0[[id]]
    end <- as.Date(fu[[id]], origin = "1970-01-01")
    nx <- c(r$ADT[-1L], max(end, r$ADT[[nrow(r)]]))
    a <- swimmer_rows(
      r$USUBJID, start = swimmer_wk(r$ADT, tt), end = swimmer_wk(nx, tt),
      colour = r$AVALC, label = r$AVALC,
      days = swimmer_days(r$ADT, nx, tt)
    )
    pre <- if (r$ADT[[1L]] > tt) {
      swimmer_rows(id, start = 0, end = swimmer_wk(r$ADT[[1L]], tt),
                   colour = "Not assessed", label = "Not assessed",
                   days = swimmer_days(tt, r$ADT[[1L]], tt))
    }
    swimmer_bind(list(pre, a))
  })
  swimmer_bind(out)
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
    d <- swimmer_date(swimmer_col(ae, c("ASTDT", "AESTDT")))
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

swimmer_fmt_dose <- function(dose, unit) {
  ifelse(is.na(dose), "", trimws(paste(format(dose, trim = TRUE,
                                              drop0trailing = TRUE), unit)))
}

# --- drug lanes ------------------------------------------------------------

# Every drug of every patient: its period bar, its dose marks by `doses`, and
# its strip.
swimmer_drug_rows <- function(ex, t0, doses, cutoff) {
  key <- paste(ex$USUBJID, ex$drug, sep = "\r")
  out <- lapply(split(ex, factor(key, levels = unique(key))), function(e) {
    tt <- t0[[e$USUBJID[[1L]]]]
    if (identical(as.character(e$lane[[1L]]), "Other")) {
      return(swimmer_other_rows(e, tt))
    }
    adm <- swimmer_admin(e)
    if (is.null(adm)) {
      swimmer_period_rows(e, tt, doses)
    } else {
      swimmer_admin_rows(e, adm, tt, doses, cutoff)
    }
  })
  swimmer_bind(out)
}

# A drug's administrations, or NULL when its records are dose periods: one
# date each (start = end), at least two, further apart than daily. Adds the
# planned interval: the most common gap, the shorter on a tie, since a delay
# only lengthens one.
swimmer_admin <- function(e) {
  if (nrow(e) < 2L || any(e$open) || any(e$end != e$start)) return(NULL)
  gap <- diff(as.numeric(e$start))
  tab <- table(gap)
  every <- min(as.numeric(names(tab)[tab == max(tab)]))
  if (every <= 2) return(NULL)
  list(date = e$start, dose = e$dose, unit = e$unit, every = every)
}

swimmer_period_rows <- function(e, tt, doses) {
  e <- swimmer_merge_daily(e)
  n <- nrow(e)
  dose <- swimmer_fmt_dose(e$dose, e$unit)
  bar <- swimmer_rows(
    e$USUBJID, start = swimmer_wk(e$start, tt), end = swimmer_wk(e$end, tt),
    colour = e$drug, lane = as.character(e$lane), label = e$drug,
    ongoing = c(rep(FALSE, n - 1L), e$open[[n]]),
    days = swimmer_days(e$start, e$end, tt, e$open), dose = dose
  )
  strip <- swimmer_rows(
    e$USUBJID, start = swimmer_wk(e$start, tt), lane = as.character(e$lane),
    strip = paste0(ifelse(nzchar(dose), paste0(dose, "\n"), ""), "d ",
                   swimmer_day(e$start, tt), "\u2013",
                   ifelse(e$open, "ongoing", swimmer_day(e$end, tt)))
  )
  cut <- if (n > 1L && !identical(doses, "Plain") &&
               !identical(doses, "Dots")) {
    i <- which(e$dose[-1L] < e$dose[-n]) + 1L
    swimmer_rows(
      e$USUBJID[i], start = swimmer_wk(e$start[i], tt),
      lane = as.character(e$lane[i]), colour = e$drug[i], mark = "step",
      label = e$drug[i], days = paste("Day", swimmer_day(e$start[i], tt)),
      dose = paste0(dose[i - 1L], " to ", dose[i])
    )
  }
  swimmer_bind(list(bar, cut, strip))
}

# Daily administrations as dose periods: a run of the same dose without a
# gap is one period.
swimmer_merge_daily <- function(e) {
  if (nrow(e) < 2L || any(e$end != e$start, na.rm = TRUE)) return(e)
  s <- as.numeric(e$start)
  brk <- c(TRUE, diff(s) > 2 | e$dose[-1L] != e$dose[-nrow(e)])
  run <- cumsum(brk)
  first <- e[brk, , drop = FALSE]
  first$end <- as.Date(tapply(s, run, max), origin = "1970-01-01")
  first
}

# A drug given in cycles: the bar from the first dose to a cycle after the
# last, and by `doses` its cuts, late doses and skipped cycles, or a ring per
# dose. The strip lists every cycle.
swimmer_admin_rows <- function(e, adm, tt, doses, cutoff) {
  id <- e$USUBJID[[1L]]
  drug <- e$drug[[1L]]
  lane <- as.character(e$lane[[1L]])
  d <- as.numeric(adm$date)
  n <- length(d)
  every <- adm$every
  ongoing <- e$on[[1L]]
  last <- if (ongoing) as.numeric(cutoff) else d[[n]] + every
  dose <- swimmer_fmt_dose(adm$dose, adm$unit)
  # Per dose: cycles skipped before it, and how many days late it came.
  gap <- c(every, diff(d))
  skip <- pmax(0, floor((gap + every / 2) / every) - 1)
  plan <- c(d[[1L]], d[-n] + (skip[-1L] + 1) * every)
  late <- round(d - plan)
  late[late <= 2] <- 0
  day <- swimmer_day(d, tt)
  bar <- swimmer_rows(
    id, start = swimmer_wk(d[[1L]], tt), end = swimmer_wk(last, tt),
    colour = drug, lane = lane, label = drug, ongoing = ongoing,
    days = swimmer_days(d[[1L]], last, tt, ongoing),
    dose = paste0(n, " doses, every ", every, " days")
  )
  skipped <- unlist(lapply(which(skip > 0), function(i) {
    d[[i - 1L]] + seq_len(skip[[i]]) * every
  }))
  marks <- switch(doses,
    Plain = NULL,
    Dots = swimmer_rows(
      rep(id, n), start = swimmer_wk(d, tt), lane = lane, colour = drug,
      mark = "dot", label = drug, days = paste("Day", day), dose = dose
    ),
    Exceptions = {
      cut <- which(c(FALSE, adm$dose[-1L] < adm$dose[-n]))
      lt <- which(late > 0)
      swimmer_bind(list(
        swimmer_rows(
          rep(id, length(cut)), start = swimmer_wk(d[cut], tt), lane = lane,
          colour = drug, mark = "step", label = drug,
          days = paste("Day", day[cut]),
          dose = paste0(dose[cut - 1L], " to ", dose[cut])
        ),
        swimmer_rows(
          rep(id, length(lt)), start = swimmer_wk(plan[lt], tt),
          end = swimmer_wk(d[lt], tt), lane = lane, colour = "Late dose",
          mark = "hollow", label = "Late dose",
          days = paste0("Cycle ", lt, ", day ", day[lt], ", ", late[lt],
                        " days late")
        ),
        swimmer_rows(
          rep(id, length(skipped)), start = swimmer_wk(skipped, tt),
          end = swimmer_wk(skipped + every, tt), lane = lane, colour = drug,
          mark = "dashed", label = drug,
          days = paste("Skipped, planned day", swimmer_day(skipped, tt))
        )
      ))
    }
  )
  boxes <- c(
    paste0("C", seq_len(n), "\n", dose, "\nd ", day,
           ifelse(late > 0, paste0("\n+", late, " d"), "")),
    rep("skip", length(skipped))
  )
  bx_x <- c(d, skipped)
  bx_day <- c(day, swimmer_day(skipped, tt))
  boxes[-seq_len(n)] <- paste0("skip\nd ", bx_day[-seq_len(n)])
  strip <- swimmer_rows(
    rep(id, length(bx_x)), start = swimmer_wk(bx_x, tt), lane = lane,
    strip = boxes,
    mark = c(ifelse(late > 0, "hollow", "solid"), rep("dashed",
                                                       length(skipped)))
  )
  swimmer_bind(list(bar, marks, strip))
}

# A folded drug: its periods in grey on the "Other" lane, named in the strip.
swimmer_other_rows <- function(e, tt) {
  n <- nrow(e)
  swimmer_bind(list(
    swimmer_rows(
      e$USUBJID, start = swimmer_wk(e$start, tt), end = swimmer_wk(e$end, tt),
      colour = "Other", lane = "Other", label = e$drug,
      ongoing = c(rep(FALSE, n - 1L), e$open[[n]]),
      days = swimmer_days(e$start, e$end, tt, e$open),
      dose = swimmer_fmt_dose(e$dose, e$unit)
    ),
    swimmer_rows(
      e$USUBJID, start = swimmer_wk(e$start, tt), lane = "Other",
      strip = paste0(e$drug, "\nd ", swimmer_day(e$start, tt), "\u2013",
                     ifelse(e$open, "ongoing", swimmer_day(e$end, tt)))
    )
  ))
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
swimmer_finish <- function(out, sl, rs, ex, fu, cutoff, adsl = NULL) {
  bor <- vapply(split(rs$AVALC, rs$USUBJID), function(v) {
    hit <- SWIMMER_BOR[SWIMMER_BOR %in% v]
    if (length(hit)) hit[[1L]] else NA_character_
  }, "")
  study <- ex[ex$role == "study", , drop = FALSE]
  # Time on treatment: to the cutoff while a study drug runs.
  wk <- vapply(sl$USUBJID, function(id) {
    e <- study[study$USUBJID == id, , drop = FALSE]
    if (!nrow(e)) return(NA_real_)
    end <- if (any(e$on)) cutoff else max(e$end)
    round(swimmer_wk(end, sl$TRTSDT[sl$USUBJID == id]), 1)
  }, numeric(1L))
  on <- vapply(sl$USUBJID, function(id) any(study$on[study$USUBJID == id]),
               NA)
  status <- ifelse(!is.na(sl$DTHDT), "Died",
                   ifelse(on, "On treatment", "Off treatment"))
  m <- match(out$USUBJID, sl$USUBJID)
  out$ARM <- sl$ARM[m]
  out$BOR <- unname(bor[out$USUBJID])
  out$weeks <- unname(wk[m])
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
  out$event <- factor(out$event, levels = c("Progression", "Serious AE",
                                            "Death"))
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
