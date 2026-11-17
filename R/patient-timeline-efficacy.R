# The patient timeline's response, drug lines and discontinuation reasons
# (patient_timeline_frame()), written for the retired swimmer. Records:
#
#   response   ADRS overall response, a run of the same read from its first
#              assessment to the next different one, on the main lane (lane
#              NA), coloured by the response read
#   exposure   ADEX, one lane per drug, coloured by the drug: pieces of a
#              bar, a skipped cycle a gap between them (the `doses`
#              setting), a change of dose a new piece and, with
#              `dose_tags`, a tagged "Dose change" event
#   events     first progression, serious AEs (ADAE) and death on the main
#              lane; the discontinuation reason as a short tag (`tag`) where
#              the drug lines stop
#
# Every record carries its own tooltip (`tip`): what it is, then " \u00b7 ",
# then when and one fact.
#
# Time is the study day: day 1 is the first dose (ADSL TRTSDT), the day
# before it day 0.

# The response parameters of an ADRS table.
swimmer_parameters_of <- function(adrs) {
  pc <- if (!is.null(adrs)) swimmer_col(adrs, "PARAMCD")
  if (is.null(pc)) return("OVR")
  v <- swimmer_resp_value(adrs$AVALC)
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

# --- inputs ----------------------------------------------------------------

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

# "XANOMELINE" reads as "Xanomeline"; mixed case stays. Each name once:
# toTitleCase() is slow, and a con med table repeats its names many times.
swimmer_drug_name <- function(x) {
  lv <- unique(x)
  nm <- lv
  up <- !is.na(lv) & lv == toupper(lv)
  nm[up] <- tools::toTitleCase(tolower(lv[up]))
  nm[match(x, lv)]
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
# such lists and the caller makes the data frame once at the end: a
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
# other drug draws its dose periods, daily doses merged. A change of dose
# starts a new piece, and with "Gaps" and `tags` a tagged event too.
# Each piece's tooltip gives its own dose. All patients and
# drugs at once; the records come out by drug and patient as the drugs first
# appear in `ex`.
swimmer_drug_rows <- function(ex, t0, doses, cutoff, tags = FALSE) {
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
                        dose, gr, doses, tags),
    swimmer_admin_rows(x, which(!is.na(every[g])), s, tt, dose, gr,
                       every[g], doses, cutoff, tags)
  )
  swimmer_bind_in(lapply(pieces, `[[`, "rows"), lapply(pieces, `[[`, "g"),
                  lapply(pieces, `[[`, "at"))
}

# One piece of the drug lanes: rows, the group of each, and its place in the
# group.
swimmer_piece <- function(rows, g, at) list(list(rows = rows, g = g, at = at))

# How a drug's line shows its doses: the doses option's values, the old
# ones read as the first.
SWIMMER_DOSES <- c("Gaps", "Plain")
swimmer_doses_value <- function(x) {
  x <- as.character(x)[1L]
  old <- c("Exceptions", "Dots", "Gaps and cuts", "Gaps and dose changes")
  if (x %in% old) x <- SWIMMER_DOSES[[1L]]
  match.arg(x, SWIMMER_DOSES)
}

# "from 75 mg" after a piece whose dose differs from the piece before it of
# the same drug, "" otherwise. `g` the pieces' groups, `raw` their doses,
# `txt` the doses as printed. Returns the text and which pieces changed.
swimmer_from <- function(g, raw, txt) {
  m <- length(g)
  same <- c(FALSE, g[-1L] == g[-m])
  prev <- c(NA, raw[-m])
  ch <- same & !is.na(raw) & !is.na(prev) & raw != prev
  ptxt <- c("", txt[-m])
  list(txt = ifelse(ch, paste0(", from ", ptxt), ""), changed = ch,
       prev = ptxt)
}

# A dose change as a tagged event where the new dose starts, on its drug's
# lane: the tag is the new dose, the tooltip says from what to what. `k` the
# changed pieces.
swimmer_change_rows <- function(x, k, at, raw, txt, prev) {
  if (!length(k)) return(NULL)
  num <- vapply(raw, format, "", trim = TRUE, drop0trailing = TRUE)
  swimmer_rows(
    x$USUBJID[k], event_day = at, event = "Dose change", lane = x$lane[k],
    tag = num,
    tip = paste0("Dose change, ", tolower(swimmer_day_txt(at)), " \u00b7 ",
                 x$drug[k], ", ", prev, " to ", txt)
  )
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
    tip = paste0(x$drug[i], " \u00b7 ", swimmer_day_txt(st), " to ",
                 sub("^Day ", "", swimmer_day_txt(en, open)))
  ), gr$g[i], 10L)
}

# Drugs given in dose periods. Daily administrations (start = end, two or
# more) first become periods: a run of the same dose without a gap is one,
# ending at its last day. Each piece's tooltip gives its own dose and, after
# a change, the dose before; with "Gaps" and `tags` a change is also a
# tagged event.
swimmer_period_rows <- function(x, i, s, e, tt, dose, gr, doses,
                                tags = FALSE) {
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
  raw <- x$dose[k]
  fr <- swimmer_from(gk, raw, dose[k])
  tip <- paste0(x$drug[k], ifelse(nzchar(dose[k]), paste0(", ", dose[k]), ""),
                ifelse(daily[gk], " daily", ""), " \u00b7 ",
                swimmer_day_txt(st), " to ",
                sub("^Day ", "", swimmer_day_txt(en, last & open)), fr$txt)
  out <- swimmer_piece(swimmer_rows(
    x$USUBJID[k], start = st, end = en, colour = x$drug[k], lane = x$lane[k],
    label = x$drug[k], ongoing = last & open, tip = tip
  ), gk, 10L)
  if (!tags || !identical(doses, SWIMMER_DOSES[[1L]])) return(out)
  ch <- which(fr$changed)
  c(out, swimmer_piece(
    swimmer_change_rows(x[k, , drop = FALSE], ch, st[ch], raw[ch],
                        dose[k][ch], fr$prev[ch]),
    gk[ch], 11L
  ))
}

# The doses of a drug as its tooltip names them: the one dose, or the lowest
# to the highest.
swimmer_dose_range <- function(raw, txt, gi, ng) {
  lo <- hi <- character(ng)
  for (j in seq_len(ng)) {
    r <- raw[gi == j]
    t <- txt[gi == j]
    ok <- !is.na(r)
    if (!any(ok)) next
    lo[[j]] <- t[ok][which.min(r[ok])]
    hi[[j]] <- t[ok][which.max(r[ok])]
  }
  num <- sub(" .*$", "", lo)
  ifelse(lo == hi, lo, paste0(num, " to ", hi))
}

# Drugs given in cycles: from the first dose to a cycle after the last (to
# the cutoff while ongoing). With "Gaps", one piece per dose up to the
# next, a skipped cycle cut out, then touching pieces of the same dose
# joined: each such run says its dose, its days and its doses, and with
# `tags` a change of dose is a tagged event. `every`: the interval of each
# sorted record's drug.
swimmer_admin_rows <- function(x, i, s, tt, dose, gr, every, doses, cutoff,
                               tags = FALSE) {
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
  # Per drug: its bar's ends, doses, delays and skips.
  f <- start_at
  l <- c(start_at[-1L] - 1L, m)
  ng <- length(f)
  gi <- cumsum(first)
  ongoing <- x$on[i][f]
  last <- ifelse(ongoing, as.numeric(cutoff), d[l] + every[f])
  x0 <- swimmer_x(d, tt)
  n_late <- tabulate(gi[late > 0], ng)
  n_skip <- tabulate(rep(gi, skip), ng)
  xl <- swimmer_x(last, tt[f])
  course <- paste0(
    l - f + 1L, " doses",
    ifelse(n_late > 0, paste0(", ", n_late, " late"), ""),
    ifelse(n_skip > 0, paste0(", ", n_skip, ifelse(n_skip > 1, " cycles",
                                                    " cycle"), " skipped"),
           "")
  )
  xi <- x[i, , drop = FALSE]
  if (!identical(doses, SWIMMER_DOSES[[1L]])) {
    rng <- swimmer_dose_range(raw, dose, gi, ng)
    tip <- paste0(
      xi$drug[f], ifelse(nzchar(rng), paste0(", ", rng), ""),
      " every ", every[f], " days \u00b7 ", swimmer_day_txt(x0[f]), " to ",
      sub("^Day ", "", swimmer_day_txt(xl, ongoing)), ", ", course
    )
    return(swimmer_piece(swimmer_rows(
      xi$USUBJID[f], start = x0[f], end = xl, colour = xi$drug[f],
      lane = xi$lane[f], label = xi$drug[f], ongoing = ongoing, tip = tip
    ), g[f], 10L))
  }
  # One piece per dose: to the next dose, or a cycle on when the next one
  # skipped a cycle; the last to the bar's end.
  nskip <- c(skip[-1L], 0)
  nx <- c(d[-1L], NA)
  pe <- ifelse(nskip > 0, d + every, nx)
  is_last <- seq_len(m) %in% l
  pe[is_last] <- last[gi[is_last]]
  # Join touching pieces of one drug at the same dose.
  key <- ifelse(is.na(raw), "", format(raw))
  new <- first | c(TRUE, key[-1L] != key[-m] | abs(d[-1L] - pe[-m]) > 0.5)
  run <- cumsum(new)
  rf <- which(new)
  rl <- c(rf[-1L] - 1L, m)
  open <- ongoing[gi[rl]] & is_last[rl]
  en <- swimmer_x(pe[rl], tt[rf])
  fr <- swimmer_from(gi[rf], raw[rf], dose[rf])
  n_run <- rl - rf + 1L
  late_run <- tabulate(run[late > 0], length(rf))
  tip <- paste0(
    xi$drug[rf], ifelse(nzchar(dose[rf]), paste0(", ", dose[rf]), ""),
    " every ", every[rf], " days \u00b7 ", swimmer_day_txt(x0[rf]), " to ",
    sub("^Day ", "", swimmer_day_txt(en, open)), ", ", n_run,
    ifelse(n_run > 1L, " doses", " dose"),
    ifelse(late_run > 0, paste0(", ", late_run, " late"), ""), fr$txt,
    " \u00b7 Course: day ", x0[f][gi[rf]], " to ",
    sub("^Day ", "", swimmer_day_txt(xl[gi[rf]], ongoing[gi[rf]])), ", ",
    course[gi[rf]]
  )
  ch <- if (tags) which(fr$changed) else integer()
  c(
    swimmer_piece(swimmer_rows(
      xi$USUBJID[rf], start = x0[rf], end = en, colour = xi$drug[rf],
      lane = xi$lane[rf], label = xi$drug[rf], ongoing = open, tip = tip
    ), g[rf], 10L),
    swimmer_piece(
      swimmer_change_rows(xi[rf, , drop = FALSE], ch, x0[rf][ch], raw[rf][ch],
                          dose[rf][ch], fr$prev[ch]),
      g[rf][ch], 11L
    )
  )
}

#' Categories of discontinuation reasons
#'
#' The standard categories the patient timeline sorts a study's
#' discontinuation reasons into, after the CDISC completion terms: a reason belongs to the
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

# --- finish ----------------------------------------------------------------

SWIMMER_RESPONSE <- c("Not assessed" = "#e4e7ec", CR = "#1d7a55",
                      PR = "#74bf95", SD = "#dcbd66",
                      "NON-CR/NON-PD" = "#ece2c6", PD = "#e3958c",
                      NE = "#c9ced6", iCR = "#1d7a55", iPR = "#74bf95",
                      iSD = "#dcbd66", iUPD = "#efbcb5", iCPD = "#e3958c")
SWIMMER_DRUGS <- c("#3f63a6", "#2a9686", "#a8649a", "#c08a3e", "#7d8b4a",
                   "#5f8fa8", "#b0707a", "#8a7bb0")
SWIMMER_BOR <- c("CR", "PR", "SD", "NON-CR/NON-PD", "PD", "NE")

