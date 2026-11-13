# The swimmer's frame: ADaM tables in, one long frame out, which the
# swimmer's spans column draws (new_swimmer_block()). One row per patient in
# the table; in the frame, one record per thing drawn:
#
#   response   ADRS overall response, a run of the same read from its first
#              assessment to the next different one, on the main lane (lane
#              NA), coloured by the response read
#   exposure   ADEX, one lane per drug, coloured by the drug: pieces of a
#              bar, a skipped cycle a gap between them, a reduced dose a
#              thinner piece (`mark`), by the `doses` setting
#   events     first progression, serious AEs (ADAE) and death on the main
#              lane; the discontinuation reason as a short tag (`tag`) where
#              the drug lines stop
#
# Every record carries its own tooltip (`tip`): what it is, then " \u00b7 ",
# then when and one fact.
#
# Time is the study day: day 1 is the first dose (ADSL TRTSDT), the day
# before it day 0.

#' Swimmer frame from ADaM
#'
#' Turns a study's ADaM tables into the long frame the swimmer block draws:
#' study days, drugs ongoing to the cutoff, response periods, best overall
#' response, the reason treatment stopped and the dose marks. It is the swimmer block's prepare step
#' ([new_swimmer_block()]) and needs no prep of the inputs.
#'
#' @param data A `dm` (or a named list of data frames) with `adsl`, `adex`
#'   and `adrs` and, optional, `adae` (serious AEs as events) and `adcm`
#'   (con meds, with `con_meds = TRUE`). Names match case-insensitively.
#'   SDTM-style names work as in the patient profile: `dm`, `ex`, `rs`, `ae`
#'   and `cm`, and SDTM columns such as `EXSTDTC` or `RFXSTDTC`.
#' @param doses How a drug lane shows its doses: `"Gaps and cuts"` (a
#'   skipped cycle leaves a gap, the bar thins after a dose cut and is a
#'   hairline at a dose of zero; late doses only in the tooltip) or
#'   `"Plain"` (the bar alone). The old `"Exceptions"` and `"Dots"` read as
#'   `"Gaps and cuts"`.
#' @param con_meds Draw con meds from `adcm` as lanes too.
#' @param max_lanes Lanes per patient before drugs fold into one grey
#'   "Other" lane, listed on hover. Study drugs (`adex`) keep theirs.
#' @param parameter The ADRS parameter of the overall response per visit.
#' @param cutoff The data cutoff. `NULL` or `NA`: the last date in the data.
#'   Response visits, doses and deaths after it are left out.
#' @param drugs The study drugs to draw, as [swimmer_drugs()] names them.
#'   `NULL`: all.
#' @param events The events drawn on the response bar: any of
#'   `"Progression"`, `"Serious AE"`, `"Death"`. `NULL`: all. Serious AEs
#'   are off by default.
#' @param reasons Tag where the drug lines stop with the discontinuation
#'   reason (ADSL `DCTREAS` and its stand-ins, see
#'   [new_swimmer_block()]): "AE", "PD", "WD", ... ([swimmer_reason_tag()]).
#' @return A data frame, one record per drawn piece; see the source for the
#'   columns.
#' @export
swimmer_frame <- function(data, doses = "Gaps and cuts", con_meds = FALSE,
                          max_lanes = 4L, parameter = "OVR", cutoff = NULL,
                          drugs = NULL,
                          events = c("Progression", "Death"),
                          reasons = TRUE) {
  doses <- as.character(doses)
  if (doses %in% c("Exceptions", "Dots")) doses <- "Gaps and cuts"
  doses <- match.arg(doses, c("Gaps and cuts", "Plain"))
  # One or more response parameters: the first is the main bar.
  parameter <- as.character(parameter)
  parameter <- unique(parameter[!is.na(parameter) & nzchar(parameter)])
  if (!length(parameter)) parameter <- "OVR"
  more <- parameter[-1L]
  max_lanes <- max(1L, as.integer(max_lanes %||% 4L))
  if (!length(cutoff) || is.na(cutoff[[1L]])) cutoff <- NULL
  tb <- swimmer_tables(data)
  sl <- swimmer_subjects(tb$adsl)
  rs <- swimmer_response(tb$adrs, parameter[[1L]])
  ra <- lapply(more, function(p) swimmer_response(tb$adrs, p))
  # Without a cutoff set, the study's own data cutoff when ADSL has one.
  if (is.null(cutoff) && any(!is.na(sl$DCUTDT))) {
    cutoff <- max(sl$DCUTDT, na.rm = TRUE)
  }
  if (!is.null(cutoff)) {
    cutoff <- as.Date(cutoff)
    rs <- rs[rs$ADT <= cutoff, , drop = FALSE]
    ra <- lapply(ra, function(r) r[r$ADT <= cutoff, , drop = FALSE])
    sl$DTHDT[!is.na(sl$DTHDT) & sl$DTHDT > cutoff] <- NA
  }
  # The response population: subjects with an assessment.
  sl <- sl[sl$USUBJID %in% rs$USUBJID, , drop = FALSE]
  # ADRS may hold patients ADSL does not (a population filter narrows ADSL
  # only): they are not drawn.
  rs <- rs[rs$USUBJID %in% sl$USUBJID, , drop = FALSE]
  if (!nrow(sl)) {
    stop("No subject has an overall response (ADRS PARAMCD \"",
         parameter[[1L]], "\").", call. = FALSE)
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

  drug <- swimmer_drug_rows(ex, t0, doses, cutoff)
  # With several parameters each is a full bar named by its PARAMCD, the
  # first the main one, each with its own progression.
  lane <- if (length(more)) parameter[[1L]] else NA_character_
  resp <- lapply(seq_along(more), function(k) {
    r <- ra[[k]][ra[[k]]$USUBJID %in% sl$USUBJID, , drop = FALSE]
    if (!nrow(r)) return(NULL)
    list(swimmer_resp_rows(r, fu, t0, more[[k]]),
         swimmer_pd_rows(r, t0, more[[k]]))
  })
  parts <- c(
    list(swimmer_resp_rows(rs, fu, t0, lane), drug,
         swimmer_event_rows(rs, sl, tb$adae, t0, lane)),
    unlist(resp, recursive = FALSE),
    list(if (isTRUE(reasons)) swimmer_reason_rows(drug, sl, ex))
  )
  out <- list2DF(swimmer_bind(parts))
  if (!is.null(events)) {
    # The discontinuation reasons are no events: the setting leaves them.
    out <- out[is.na(out$event) | !is.na(out$tag) |
                 out$event %in% as.character(events), , drop = FALSE]
  }
  out <- swimmer_finish(out, sl, rs, ex, fu, cutoff, tb$adsl, parameter)
  swimmer_bor_more(out, ra, more)
}

#' The response parameters in a swimmer's input
#'
#' The ADRS parameters whose values are overall responses (CR, PR, SD, PD,
#' NE and their iRECIST forms), the choices for the swimmer's response bars.
#' The usual investigator parameter comes first.
#'
#' @param data The swimmer's input dm.
#' @return A character vector of PARAMCDs, `"OVR"` when none is found.
#' @export
swimmer_parameters <- function(data) {
  tb <- tryCatch(swimmer_tables(data), error = function(e) NULL)
  pc <- if (!is.null(tb)) swimmer_col(tb$adrs, "PARAMCD")
  if (is.null(pc)) return("OVR")
  v <- swimmer_resp_value(tb$adrs$AVALC)
  ok <- tapply(v, as.character(pc), function(x) {
    mean(x %in% c(names(SWIMMER_RESPONSE), "NOT EVALUABLE")) > 0.8
  })
  out <- names(ok)[ok %in% TRUE]
  first <- intersect(c("OVR", "OVRINV", "OVRLRESP"), out)
  out <- c(first, sort(setdiff(out, first)))
  if (length(out)) out else "OVR"
}

# A response read as the swimmer draws it: upper case, iRECIST's `i` kept.
swimmer_resp_value <- function(x) {
  x <- toupper(as.character(x))
  i <- grepl("^I(CR|PR|SD|UPD|CPD)$", x)
  x[i] <- paste0("i", substring(x[i], 2L))
  x
}

# Per patient `ids`, the best read of `rs` in SWIMMER_BOR order, iRECIST's
# forms ranked as their RECIST ones (iUPD and iCPD as PD).
swimmer_bor_of <- function(rs, ids) {
  base <- sub("^i(UPD|CPD)$", "PD", sub("^i(CR|PR|SD)$", "\\1", rs$AVALC))
  rank <- match(base, SWIMMER_BOR)
  # A response of a patient not among `ids` (ADRS unfiltered while ADSL is)
  # has no place.
  ok <- !is.na(rank) & rs$USUBJID %in% ids
  g <- match(rs$USUBJID[ok], ids)
  o <- order(g, rank[ok], method = "radix")
  first <- o[!duplicated(g[o])]
  out <- rep(NA_character_, length(ids))
  out[g[first]] <- rs$AVALC[ok][first]
  out
}

# The best response of the second and third parameter, as listing columns
# labelled by their PARAMCD ("BOR OVRBICR"); empty when not drawn.
swimmer_bor_more <- function(out, ra, more) {
  ids <- unique(out$USUBJID)
  for (k in 2:3) {
    nm <- paste0("BOR_", k)
    v <- rep(NA_character_, nrow(out))
    if (k - 1L <= length(ra)) {
      v <- swimmer_bor_of(ra[[k - 1L]], ids)[match(out$USUBJID, ids)]
    }
    out[[nm]] <- v
    attr(out[[nm]], "label") <- if (k - 1L <= length(more)) {
      paste("BOR", more[[k - 1L]])
    } else {
      paste0("BOR (response ", k, ")")
    }
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
    reason = swimmer_reason(swimmer_col(adsl, "DCTREAS")) %||%
      rep(NA_character_, n),
    stringsAsFactors = FALSE
  )
}

# Why treatment stopped, as a reader says it: "ADVERSE EVENT" reads
# "Adverse event". A completed or ongoing disposition is no reason.
swimmer_reason <- function(x) {
  if (is.null(x)) return(NULL)
  x <- trimws(as.character(x))
  lv <- unique(x)
  nm <- lv
  up <- !is.na(lv) & lv == toupper(lv)
  nm[up] <- paste0(substr(lv[up], 1L, 1L), tolower(substring(lv[up], 2L)))
  out <- nm[match(x, lv)]
  out[is.na(out) | !nzchar(out) |
        toupper(out) %in% c("COMPLETED", "ONGOING", "NOT DISCONTINUED")] <-
    NA_character_
  out
}

# Overall response by visit, one row per assessment with a date.
swimmer_response <- function(adrs, parameter) {
  pc <- swimmer_col(adrs, "PARAMCD")
  r <- if (is.null(pc)) adrs else adrs[pc %in% parameter, , drop = FALSE]
  out <- data.frame(
    USUBJID = as.character(r$USUBJID),
    ADT = swimmer_date(swimmer_col(r, c("ADT", "ADTM"))),
    AVALC = swimmer_resp_value(r$AVALC),
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
  lane = NA_character_, mark = NA_character_, label = NA_character_,
  event_day = NA_real_, event = NA_character_, tag = NA_character_,
  ongoing = NA, tip = NA_character_, full = NA
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

# The position on the time axis: the study day, continuous through the
# first dose (day 1), so the day before it is day 0.
swimmer_x <- function(d, t0) as.numeric(d) - as.numeric(t0) + 1


# A study day as a tooltip says it: "Day 12", "ongoing" where open.
swimmer_day_txt <- function(x, open = FALSE) {
  ifelse(rep_len(open, length(x)), "ongoing",
         paste("Day", ifelse(x >= 1, x, x - 1)))
}

# A run of the same response, from its first assessment to the next
# different read; before the first, "Not assessed"; the last runs to the end
# of follow-up. `rs` is sorted by patient and date. `lane`: the parameter of
# a full bar (NA: the one response lane), named in the tooltips.
swimmer_resp_rows <- function(rs, fu, t0, lane = NA_character_) {
  full <- if (is.na(lane)) NA else TRUE
  who <- if (is.na(lane)) "" else paste0(" (", lane, ")")
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
  # Runs: a new one where the patient or the read changes.
  run <- cumsum(first | c(TRUE, rs$AVALC[-1L] != rs$AVALC[-m]))
  rf <- !duplicated(run)
  rl <- !duplicated(run, fromLast = TRUE)
  st <- swimmer_x(adt[rf], tt[rf])
  en <- swimmer_x(nx[rl], tt[rf])
  pre <- which(first & adt > tt)
  pe <- swimmer_x(adt[pre], tt[pre])
  sets <- list(
    swimmer_rows(id[pre], start = 1, end = pe, lane = lane, full = full,
                 colour = "Not assessed", label = "Not assessed",
                 tip = paste0("Not assessed", who, " \u00b7 Day 1 to ",
                              ifelse(pe >= 1, pe, pe - 1))),
    swimmer_rows(id[rf], start = st, end = en, lane = lane, full = full,
                 colour = rs$AVALC[rf], label = rs$AVALC[rf],
                 tip = paste0(rs$AVALC[rf], who, " \u00b7 ",
                              swimmer_day_txt(st),
                              " to ", ifelse(en >= 1, en, en - 1),
                              ifelse(en > st, paste0(", ", round(en - st),
                                                     " days"), "")))
  )
  swimmer_bind_in(sets, list(pat[pre], pat[rf]), list(1L, 2L))
}

# The first progression of one parameter (PD, or iRECIST's iUPD or iCPD),
# on its bar: `lane` its PARAMCD, NA on the one response lane.
swimmer_pd_rows <- function(rs, t0, lane = NA_character_) {
  pd <- rs[rs$AVALC %in% c("PD", "iUPD", "iCPD"), , drop = FALSE]
  pd <- pd[!duplicated(pd$USUBJID), , drop = FALSE]
  x <- swimmer_x(pd$ADT, t0[pd$USUBJID])
  who <- if (is.na(lane)) "" else paste0(" (", lane, ")")
  swimmer_rows(pd$USUBJID, event_day = x, event = "Progression", lane = lane,
               tip = paste0("Progression", who, " \u00b7 ",
                            swimmer_day_txt(x)))
}

# First progression and death on the response lane; serious AEs when ADAE
# is there. `lane`: the main bar's parameter with several, else NA.
swimmer_event_rows <- function(rs, sl, adae, t0, lane = NA_character_) {
  ev <- function(ids, d, what) {
    x <- swimmer_x(d, t0[ids])
    swimmer_rows(ids, event_day = x, event = what,
                 tip = paste0(what, " \u00b7 ", swimmer_day_txt(x)))
  }
  out <- list(swimmer_pd_rows(rs, t0, lane))
  if (!is.null(adae) && "AESER" %in% names(adae)) {
    ae <- adae[adae$AESER %in% "Y" & as.character(adae$USUBJID) %in% sl$USUBJID,
               , drop = FALSE]
    d <- swimmer_date(swimmer_col(ae, "ASTDT"))
    ok <- !is.na(d)
    if (any(ok)) {
      ids <- as.character(ae$USUBJID[ok])
      out <- c(out, list(ev(ids, d[ok], "Serious AE")))
    }
  }
  dth <- sl[!is.na(sl$DTHDT), , drop = FALSE]
  out <- c(out, list(ev(dth$USUBJID, dth$DTHDT, "Death")))
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

# Every drug of every patient as pieces of its bar. A folded con med draws in
# grey on the "Other" lane; a drug given in cycles (swimmer_every()) runs from
# the first dose to a cycle after the last, a skipped cycle leaving a gap; any
# other drug draws its dose periods, daily doses merged. With "Gaps and cuts"
# a piece given at less than the drug's first dose is "thinner", at zero a
# "hairline" (`mark`). Each piece carries its drug's tooltip. All patients and
# drugs at once; the records come out by drug and patient as the drugs first
# appear in `ex`.
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
    swimmer_other_rows(x, which(other[g]), s, e, tt, gr),
    swimmer_period_rows(x, which(!other[g] & is.na(every[g])), s, e, tt,
                        dose, gr, doses),
    swimmer_admin_rows(x, which(!is.na(every[g])), s, tt, dose, gr,
                       every[g], doses, cutoff)
  )
  swimmer_bind_in(lapply(pieces, `[[`, "rows"), lapply(pieces, `[[`, "g"),
                  lapply(pieces, `[[`, "at"))
}

# One piece of the drug lanes: rows, the group of each, and its place in the
# group.
swimmer_piece <- function(rows, g, at) list(list(rows = rows, g = g, at = at))

# The style of a piece by its dose against the drug's first dose: "thinner"
# below it, "hairline" at zero, NA (solid) otherwise or without doses.
swimmer_dose_mark <- function(dose, first, doses) {
  out <- rep(NA_character_, length(dose))
  if (!identical(doses, "Gaps and cuts")) return(out)
  ok <- !is.na(dose) & !is.na(first) & first > 0
  out[ok & dose < first] <- "thinner"
  out[ok & dose <= 0] <- "hairline"
  out
}

# The dose cuts of each group as tooltip text: ", cut to 50 mg on day 64",
# or ", 3 dose cuts". `cut` the cut records, `gk` their groups, `to` and `d`
# the new dose and its study day; `n` the number of groups.
swimmer_cut_txt <- function(cut, gk, to, d, n) {
  out <- character(n)
  if (!length(cut)) return(out)
  k <- tabulate(gk[cut], n)
  one <- cut[!duplicated(gk[cut])]
  out[gk[one]] <- paste0(", cut to ", to[one], " on day ", d[one])
  many <- k > 1L
  out[many] <- paste0(", ", k[many], " dose cuts")
  out
}

# Folded drugs: their periods in grey on the "Other" lane, each named in its
# tooltip. `i`: their sorted records.
swimmer_other_rows <- function(x, i, s, e, tt, gr) {
  if (!length(i)) return(list())
  open <- x$open[i]
  st <- swimmer_x(s[i], tt[i])
  en <- swimmer_x(e[i], tt[i])
  swimmer_piece(swimmer_rows(
    x$USUBJID[i], start = st, end = en, colour = "Other", lane = "Other",
    label = x$drug[i], ongoing = gr$last[i] & open,
    tip = paste0(x$drug[i], " · ", swimmer_day_txt(st), " to ",
                 sub("^Day ", "", swimmer_day_txt(en, open)))
  ), gr$g[i], 10L)
}

# Drugs given in dose periods. Daily administrations (start = end, two or
# more) first become periods: a run of the same dose without a gap is one,
# ending at its last day.
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
  st <- swimmer_x(s[k], tt[k])
  en <- swimmer_x(end, tt[k])
  # Per group: its first dose, its days, its cuts.
  fpos <- match(gk, gk)
  raw <- x$dose[k]
  # A cut: a step down to below the first dose (as the thinner line says).
  cut <- which(c(FALSE, gk[-1L] == gk[-m] & raw[-1L] < raw[-m]))
  cut <- cut[raw[cut] < raw[fpos][cut]]
  ctxt <- swimmer_cut_txt(cut, gk, dose[k], st, gr$n)
  lpos <- which(last)[match(gk, gk[last])]
  head <- paste0(x$drug[k], ifelse(nzchar(dose[k][fpos]),
                                   paste0(", ", dose[k][fpos]), ""),
                 ifelse(daily[gk], " daily", ""))
  tip <- paste0(head, " · ", swimmer_day_txt(st[fpos]), " to ",
                sub("^Day ", "", swimmer_day_txt(en[lpos], open[lpos])),
                ctxt[gk])
  swimmer_piece(swimmer_rows(
    x$USUBJID[k], start = st, end = en, colour = x$drug[k], lane = x$lane[k],
    label = x$drug[k], ongoing = last & open,
    mark = swimmer_dose_mark(raw, raw[fpos], doses), tip = tip
  ), gk, 10L)
}

# Drugs given in cycles: from the first dose to a cycle after the last (to
# the cutoff while ongoing). With "Gaps and cuts", one piece per dose up to
# the next, a skipped cycle cut out, then touching pieces of the same style
# joined. `every`: the interval of each sorted record's drug.
swimmer_admin_rows <- function(x, i, s, tt, dose, gr, every, doses, cutoff) {
  if (!length(i)) return(list())
  g <- gr$g[i]
  d <- s[i]
  tt <- tt[i]
  every <- every[i]
  dose <- dose[i]
  raw <- x$dose[i]
  first <- gr$first[i]
  m <- length(i)
  start_at <- which(first)
  # Per dose: cycles skipped before it, and how many days late it came.
  prev <- c(NA, d[-m])
  gap <- ifelse(first, every, d - prev)
  skip <- pmax(0, floor((gap + every / 2) / every) - 1)
  plan <- ifelse(first, d, prev + (skip + 1) * every)
  late <- round(d - plan)
  late[late <= 2] <- 0
  # Per drug: its bar's ends, doses, delays, skips and cuts.
  f <- start_at
  l <- c(start_at[-1L] - 1L, m)
  ng <- length(f)
  gi <- cumsum(first)
  ongoing <- x$on[i][f]
  last <- ifelse(ongoing, as.numeric(cutoff), d[l] + every[f])
  x0 <- swimmer_x(d, tt)
  cut <- which(!first & c(FALSE, raw[-1L] < raw[-m]))
  cut <- cut[raw[cut] < raw[f][gi[cut]]]
  ctxt <- swimmer_cut_txt(cut, gi, dose, x0, ng)
  n_late <- tabulate(gi[late > 0], ng)
  n_skip <- tabulate(rep(gi, skip), ng)
  xl <- swimmer_x(last, tt[f])
  tip <- paste0(
    x$drug[i][f], ifelse(nzchar(dose[f]), paste0(", ", dose[f]), ""),
    " every ", every[f], " days · ", swimmer_day_txt(x0[f]), " to ",
    sub("^Day ", "", swimmer_day_txt(xl, ongoing)), ", ", l - f + 1L,
    " doses", ifelse(n_late > 0, paste0(", ", n_late, " late"), ""),
    ifelse(n_skip > 0, paste0(", ", n_skip, ifelse(n_skip > 1, " cycles",
                                                    " cycle"), " skipped"),
           ""), ctxt
  )
  if (!identical(doses, "Gaps and cuts")) {
    return(swimmer_piece(swimmer_rows(
      x$USUBJID[i][f], start = x0[f], end = xl, colour = x$drug[i][f],
      lane = x$lane[i][f], label = x$drug[i][f], ongoing = ongoing, tip = tip
    ), g[f], 10L))
  }
  # One piece per dose: to the next dose, or a cycle on when the next one
  # skipped a cycle; the last to the bar's end.
  nskip <- c(skip[-1L], 0)
  nx <- c(d[-1L], NA)
  pe <- ifelse(nskip > 0, d + every, nx)
  is_last <- seq_len(m) %in% l
  pe[is_last] <- last[gi[is_last]]
  mk <- swimmer_dose_mark(raw, raw[f][gi], doses)
  # Join touching pieces of one drug in the same style.
  key <- ifelse(is.na(mk), "", mk)
  new <- first | c(TRUE, key[-1L] != key[-m] | abs(d[-1L] - pe[-m]) > 0.5)
  run <- cumsum(new)
  rf <- which(new)
  rl <- c(rf[-1L] - 1L, m)
  open <- ongoing[gi[rl]] & is_last[rl]
  swimmer_piece(swimmer_rows(
    x$USUBJID[i][rf], start = x0[rf], end = swimmer_x(pe[rl], tt[rf]),
    colour = x$drug[i][rf], lane = x$lane[i][rf], label = x$drug[i][rf],
    ongoing = open, mark = mk[rf], tip = tip[gi[rf]]
  ), g[rf], 10L + seq_along(rf) * 0)
}

# The discontinuation reason as a tag where the patient's drug lines stop,
# one event level per category (swimmer_reason_category()), which the legend
# lists with its tag; the tooltip says the category and the exact reason.
# A death needs none: the death mark is there. `drug`: swimmer_drug_rows().
swimmer_reason_rows <- function(drug, sl, ex) {
  if (is.null(drug)) return(NULL)
  ids <- sl$USUBJID
  study <- ex$role == "study"
  on <- tabulate(match(ex$USUBJID[study & ex$on], ids), length(ids)) > 0L
  why <- sl$reason
  why[on] <- NA_character_
  cat <- swimmer_reason_category(why)
  keep <- !is.na(cat$tag)
  if (!any(keep)) return(NULL)
  lanes <- unique(as.character(ex$lane[study]))
  r <- which(drug$lane %in% lanes & !is.na(drug$end) &
               drug$USUBJID %in% ids[keep])
  if (!length(r)) return(NULL)
  r <- r[order(drug$USUBJID[r], -drug$end[r], method = "radix")]
  r <- r[!duplicated(drug$USUBJID[r])]
  at <- match(drug$USUBJID[r], ids)
  x <- drug$end[r]
  # The exact reason after the category, unless it says no more.
  raw <- why[at]
  same <- tolower(raw) == tolower(cat$name[at])
  swimmer_rows(
    drug$USUBJID[r], event_day = x, event = cat$name[at],
    lane = drug$lane[r], tag = cat$tag[at],
    tip = paste0("Discontinued, ", tolower(swimmer_day_txt(x)), " \u00b7 ",
                 cat$name[at], ifelse(same, "", paste0(" \u00b7 ", raw)))
  )
}

#' Categories of discontinuation reasons
#'
#' The standard categories the swimmer sorts a study's discontinuation
#' reasons into, after the CDISC completion terms: a reason belongs to the
#' first category whose pattern its words match (case ignored), so the
#' three wordings of a withdrawal share one tag. A death has none (the death
#' mark is there); a reason no pattern matches is "Other".
#'
#' @return A data frame: `pattern` (a regular expression), `tag` (two
#'   letters) and `name` (the legend's words), in matching order.
#' @export
swimmer_reason_categories <- function() {
  data.frame(
    pattern = c("adverse", "progress|relapse|recurren|deteriorat",
                "withdr|consent", "physician|investigator", "lost to",
                "deviation|violation", "efficacy", "sponsor|terminat",
                "complian", "pregnan"),
    tag = c("AE", "PD", "WD", "ID", "LF", "DV", "LE", "ST", "NC", "PG"),
    name = c("Adverse event", "Progressive disease", "Withdrawal by subject",
             "Investigator decision", "Lost to follow-up",
             "Protocol deviation", "Lack of efficacy", "Study terminated",
             "Non-compliance", "Pregnancy"),
    stringsAsFactors = FALSE
  )
}

#' The category of each discontinuation reason
#'
#' Sorts reasons into [swimmer_reason_categories()]: the first pattern that
#' matches wins; a reason no pattern matches is "Other" (tag "OT"). A death
#' or a missing reason has no category.
#'
#' @param x Reasons, character.
#' @param categories The categories, as [swimmer_reason_categories()] gives
#'   them; a study may pass its own.
#' @return A list of two character vectors, `tag` and `name`, `NA` where a
#'   reason has no category.
#' @export
swimmer_reason_category <- function(x,
                                    categories = swimmer_reason_categories()) {
  t <- tolower(as.character(x))
  k <- rep(NA_integer_, length(t))
  for (j in rev(seq_len(nrow(categories)))) {
    k[grepl(categories$pattern[[j]], t)] <- j
  }
  tag <- ifelse(is.na(k), "OT", categories$tag[k])
  name <- ifelse(is.na(k), "Other", categories$name[k])
  none <- is.na(t) | !nzchar(t) | grepl("death|died", t)
  tag[none] <- NA_character_
  name[none] <- NA_character_
  list(tag = tag, name = name)
}

#' Two letters for a discontinuation reason
#'
#' The tag of a reason's category ([swimmer_reason_category()]).
#'
#' @param x Reasons, character.
#' @return A character vector of tags, `NA` for none.
#' @export
swimmer_reason_tag <- function(x) swimmer_reason_category(x)$tag

# --- finish ----------------------------------------------------------------

SWIMMER_RESPONSE <- c("Not assessed" = "#e4e7ec", CR = "#1d7a55",
                      PR = "#74bf95", SD = "#dcbd66",
                      "NON-CR/NON-PD" = "#ece2c6", PD = "#e3958c",
                      NE = "#c9ced6", iCR = "#1d7a55", iPR = "#74bf95",
                      iSD = "#dcbd66", iUPD = "#efbcb5", iCPD = "#e3958c")
SWIMMER_DRUGS <- c("#3f63a6", "#2a9686", "#a8649a", "#c08a3e", "#7d8b4a",
                   "#5f8fa8", "#b0707a", "#8a7bb0")
SWIMMER_BOR <- c("CR", "PR", "SD", "NON-CR/NON-PD", "PD", "NE")

# What each colour level stands for in the data, for the board's scale map
# (see timeline_frame()): a response is its ADRS `AVALC`, a drug its ADEX
# `EXTRT` and a con med its `CMDECOD`. swimmer_drug_name() titled names that
# were all capitals, so a drug is keyed as written and in capitals.
swimmer_scale_keys <- function(lv, resp, drugs, cm) {
  key <- function(x, var) {
    x <- intersect(x, lv)
    x <- setdiff(x, "Other")
    data.frame(level = c(x, x), var = rep(var, 2L * length(x)),
               key = c(x, toupper(x)), stringsAsFactors = FALSE)
  }
  k <- rbind(key(resp, "AVALC"), key(drugs, "EXTRT"), key(cm, "CMDECOD"))
  k[!duplicated(k), , drop = FALSE]
}

# The listing columns, the colour and lane levels with their colours, and
# the labels the tooltips print.
SWIMMER_EVENTS <- c("Progression", "Serious AE", "Death")

swimmer_finish <- function(out, sl, rs, ex, fu, cutoff, adsl = NULL,
                           parameter = NULL) {
  ids <- sl$USUBJID
  n <- length(ids)
  # Best response: the best read of the main parameter.
  bor <- swimmer_bor_of(rs, ids)
  study <- ex[ex$role == "study", , drop = FALSE]
  at <- match(study$USUBJID, ids)
  has <- tabulate(at, n) > 0L
  on <- tabulate(at[study$on], n) > 0L
  # Days on treatment: to the cutoff while a study drug runs, else to the end
  # of treatment (the last dose without one), first and last day counted.
  end <- ifelse(on, as.numeric(cutoff),
                ifelse(!is.na(sl$TRTEDT), as.numeric(sl$TRTEDT),
                       swimmer_grp_max(at, as.numeric(study$end), n)))
  dur <- swimmer_x(end, sl$TRTSDT)
  dur[!has & is.na(sl$TRTEDT)] <- NA_real_
  status <- ifelse(!is.na(sl$DTHDT), "Died",
                   ifelse(on, "On treatment", "Off treatment"))
  # The reason only for a patient off treatment.
  reason <- sl$reason
  reason[on] <- NA_character_
  m <- match(out$USUBJID, ids)
  out$ARM <- sl$ARM[m]
  out$BOR <- bor[m]
  out$duration <- dur[m]
  out$status <- status[m]
  out$reason <- reason[m]
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
  cols <- c(SWIMMER_RESPONSE, dcol,
            stats::setNames(rep("#a3a9b4", length(cm)), cm))
  lv <- unique(c(names(SWIMMER_RESPONSE), drugs, cm))
  lv <- lv[lv %in% out$colour]
  out$colour <- factor(out$colour, levels = lv)
  attr(out$colour, "blockr_colors") <- cols[lv]
  attr(out$colour, "blockr_scale_keys") <- swimmer_scale_keys(
    lv, setdiff(names(SWIMMER_RESPONSE), "Not assessed"), drugs, cm
  )
  # Drug lanes, then with several parameters their full bars in bar order.
  bars <- if (length(parameter) > 1L) parameter else character()
  out$lane <- factor(out$lane, levels = unique(c(drugs, bars)))
  out$full <- out$full %in% TRUE
  # The discontinuation reasons after the events, the most frequent first.
  why <- as.character(out$event[!is.na(out$tag)])
  lv <- sort(unique(why))
  why <- lv[order(-tabulate(match(why, lv), length(lv)), lv)]
  # A category is never also an event level (it would be one legend entry).
  why <- setdiff(why, SWIMMER_EVENTS)
  out$event <- factor(out$event, levels = c(SWIMMER_EVENTS, why))
  labels <- c(start = "Start, study day", end = "End, study day",
              colour = "Response / drug",
              lane = "Drug", mark = "Dose style", label = "Label",
              full = "Response bar",
              tag = "Discontinuation reason", tip = "Tooltip",
              event_day = "Study day", event = "Event", ongoing = "Ongoing",
              BOR = "Best response",
              duration = "Days on treatment", status = "Status",
              reason = "Discontinuation reason", ARM = "Arm",
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
