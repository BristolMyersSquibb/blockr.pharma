# The patient timeline's frame: the timeline's frame (timeline_frame()) with
# the swimmer's layers added. One long frame, one record per thing drawn, for
# the spans column of new_patient_timeline_block():
#
#   Treatment                  a thin lane per drug: the swimmer's drug
#                              lines (swimmer_drug_rows()), or the
#                              timeline's dose bars
#   Response                   ADRS overall response, a run of the same read
#                              from its first assessment to the next
#                              different one, as on the swimmer; the first
#                              progression as a mark
#   Discontinuation and death  death as a cross, the discontinuation reason
#                              as a two-letter tag at the last dose
#   Adverse events             as on the timeline, coloured by the study's
#                              severity column (the "study_roles" board
#                              option, as the patient profile reads it)
#   Con meds                   as on the timeline
#
# AEs and con meds can be narrowed to picked terms, at any coding level, with
# the patient profile's find picks (pp_find_match()).

# The layers, in the order their stripes stack on a patient row.
PT_LAYERS <- c("Response", "Treatment", "Discontinuation and death",
               "Adverse events", "Con meds")
PT_AE_COLOUR <- c("study severity", "grade", "severity", "one colour")
PT_TRT_DRAW <- c("lines per drug", "bars by dose")
PT_DOSES <- c("Gaps and cuts", "Plain")
# Drugs with a lane of their own; the rest share "Other" (as the swimmer).
PT_MAX_DRUGS <- 4L
# The layers drawn as full bars under the drug lanes, the first the main one.
PT_FULL_LANES <- c("Response", "Adverse events", "Con meds")
PT_ORDER <- c("days on treatment", "number of AEs", "number of events",
              "worst AE", "best response", "first progression",
              "ongoing AEs", "first onset", "number of con meds", "subject")

#' Patient timeline frame from ADaM
#'
#' Turns ADSL, ADRS, ADEX, ADAE and ADCM into the long frame the patient
#' timeline block draws ([new_patient_timeline_block()]). It is that block's
#' prepare step and needs no prep of its inputs. Tables and columns may be
#' named as the patient profile allows.
#'
#' @param data A `dm` (or a named list of data frames) with `adsl` and the
#'   tables of the layers drawn.
#' @param layers Any of `"Response"`, `"Treatment"`, `"Discontinuation and
#'   death"`, `"Adverse events"` and `"Con meds"`.
#' @param group The ADSL column shown as the arm. Empty: the arm of the
#'   board's "study_roles" option, else `ACTARM`, `TRT01A` or `ARM`.
#' @param response The ADRS parameter of the overall response.
#' @param ae_colour `"study severity"` (the severity column of the board's
#'   "study_roles" option, else the first of `ASEV`, `AESEV`, `ATOXGR` and
#'   `AETOXGR`, as the patient profile picks it), `"grade"`, `"severity"` or
#'   `"one colour"`.
#' @param treatment_as `"lines per drug"`: a thin line per drug in its
#'   colour, as the swimmer draws it. `"bars by dose"`: each drug's dose
#'   records as bars coloured by `trt_colour`, as the timeline draws them.
#'   Either way each drug has a lane of its own.
#' @param doses With lines per drug: `"Gaps and cuts"` shows skipped cycles,
#'   delays and dose cuts on the line, `"Plain"` one solid line.
#' @param ae_terms,cm_terms Only the AEs or con meds matching any of these,
#'   as [patient_timeline_terms()] offers them ("Body system: Cardiac
#'   disorders"). Empty: all.
#' @param order_by Patient order, one of `"days on treatment"`, `"number of
#'   AEs"`, `"number of events"`, `"worst AE"`, `"best response"`, `"first
#'   progression"`, `"ongoing AEs"`, `"first onset"`, `"number of con meds"`
#'   and `"subject"`.
#' @param roles The study's roles, as [patient_timeline_roles()] reads them
#'   from the board. `NULL`: none declared.
#' @inheritParams timeline_frame
#' @return A data frame, one record per drawn piece.
#' @examples
#' if (requireNamespace("pharmaverseadam", quietly = TRUE)) {
#'   d <- list(adsl = pharmaverseadam::adsl, adae = pharmaverseadam::adae)
#'   head(patient_timeline_frame(d))
#' }
#' @export
patient_timeline_frame <- function(data,
                                   layers = c("Adverse events",
                                              "Discontinuation and death"),
                                   group = "",
                                   response = "OVR",
                                   ae_records = "treatment-emergent",
                                   ae_show = "all AEs",
                                   ae_colour = "study severity",
                                   ae_lanes = "preferred term",
                                   ae_terms = NULL,
                                   serious = "outlined",
                                   cm_records = "active on treatment",
                                   cm_colour = "before or on treatment",
                                   cm_lanes = "coded name",
                                   cm_terms = NULL,
                                   cm_prior = "clipped at the start",
                                   treatment_as = "lines per drug",
                                   doses = "Gaps and cuts",
                                   trt_colour = "dose level",
                                   dose_labels = TRUE,
                                   max_lanes = 12,
                                   origin = "first dose",
                                   cutoff = NULL,
                                   ongoing = "last contact",
                                   order_by = "number of AEs",
                                   roles = NULL) {
  layers <- intersect(PT_LAYERS, as.character(unlist(layers)))
  if (!length(layers)) stop("Pick at least one layer.", call. = FALSE)
  arg <- function(x, choices) match.arg(as.character(x)[1L], choices)
  roles <- roles %||% list()
  opt <- list(
    ae_records = arg(ae_records, TL_AE_RECORDS),
    ae_show = arg(ae_show, TL_AE_SHOW),
    ae_colour = arg(ae_colour, PT_AE_COLOUR),
    ae_lanes = arg(ae_lanes, names(TL_AE_LANES)),
    serious = arg(serious, TL_SERIOUS),
    death = "Discontinuation and death" %in% layers,
    cm_records = arg(cm_records, TL_CM_RECORDS),
    cm_colour = arg(tl_cm_colour_value(cm_colour), TL_CM_COLOUR),
    cm_lanes = arg(cm_lanes, names(TL_CM_LANES)),
    cm_prior = arg(cm_prior, TL_CM_PRIOR),
    trt_colour = arg(trt_colour, TL_TRT_COLOUR),
    treatment_as = arg(treatment_as, PT_TRT_DRAW),
    doses = arg(doses, PT_DOSES),
    dose_labels = isTRUE(as.logical(dose_labels)),
    max_lanes = max(1L, as.integer(max_lanes %||% 12L)[1L], na.rm = TRUE),
    origin = arg(origin, TL_ORIGIN),
    ongoing = arg(ongoing, c("last contact", "data cutoff")),
    order_by = arg(order_by, PT_ORDER),
    response = as.character(unlist(response %||% "OVR"))[1L],
    # What tl_finish() does differently for this frame.
    src_order = c("Response", "Treatment", "AE", "CM"),
    lanes = c("Treatment", PT_FULL_LANES),
    full_lanes = PT_FULL_LANES,
    extra = c(".tag", ".bor", ".status", ".full")
  )
  tb <- pt_tables(data, layers)
  group <- pt_group(group, roles, tb$adsl)
  sl <- tl_subjects(tb, group, opt$origin)
  cutoff <- aet_date(cutoff)
  if (!length(cutoff) || is.na(cutoff[[1L]])) cutoff <- tl_cutoff(tb, sl)
  cutoff <- cutoff[[1L]]
  sl <- aet_cap_at(sl, cutoff)

  parts <- list()
  if ("Response" %in% layers && !is.null(tb$adrs)) {
    parts$resp <- tl_response(tb$adrs, sl, opt, cutoff)
  }
  if ("Adverse events" %in% layers && !is.null(tb$adae)) {
    ae <- pt_ae_colour(tb$adae, opt, roles)
    opt$ae_colour <- attr(ae, "pt_colour")
    ae <- ae[pp_find_match(ae, pt_picks(ae_terms, PP_AE_LEVELS),
                           PP_AE_SEARCH), , drop = FALSE]
    parts$ae <- tl_ae(ae, sl, opt, cutoff)
  }
  if ("Treatment" %in% layers) {
    parts$trt <- if (identical(opt$treatment_as, "lines per drug")) {
      pt_drug_lines(tb, sl, opt, cutoff)
    } else {
      pt_dose_bars(tb$adex, sl, opt, cutoff)
    }
    opt$sub_lanes <- attr(parts$trt, "pt_lanes")
    opt$trt_palette <- attr(parts$trt, "pt_palette")
  }
  lo <- tl_axis_start(parts)
  if ("Con meds" %in% layers && !is.null(tb$adcm)) {
    if (identical(opt$cm_records, "all")) lo <- min(lo, 1 - 28)
    cm <- tb$adcm
    cm <- cm[pp_find_match(cm, pt_picks(cm_terms, PP_CM_LEVELS),
                           PP_CM_SEARCH), , drop = FALSE]
    if (nrow(cm)) parts$cm <- tl_cm(cm, sl, opt, cutoff, lo)
  }
  parts <- Filter(function(p) !is.null(p) && length(p$USUBJID), parts)
  # What the response part carries besides its records; tl_keep() drops it.
  resp_events <- attr(parts$resp, "pt_events")
  resp_bor <- attr(parts$resp, "pt_bor")

  # Patients with a response, an AE or a con med drawn; treatment alone, or
  # discontinuation and death alone, draw every patient they have.
  ev <- unique(unlist(lapply(parts[intersect(c("resp", "ae", "cm"),
                                             names(parts))],
                             `[[`, "USUBJID")))
  ids <- if (length(ev)) {
    ev
  } else if (!is.null(parts$trt)) {
    unique(parts$trt$USUBJID)
  } else if (opt$death) {
    sl$USUBJID
  }
  if (!length(ids)) stop("Nothing to draw.", call. = FALSE)
  pcols <- attr(sl, "pcols")
  sl <- sl[sl$USUBJID %in% ids, , drop = FALSE]
  attr(sl, "pcols") <- pcols
  parts <- lapply(parts, function(p) tl_keep(p, p$USUBJID %in% sl$USUBJID))
  if ("cm" %in% names(parts)) parts$cm <- tl_fold(parts, opt$max_lanes)
  parts <- tl_unique_rows(parts)
  rec <- tl_bind(parts)
  extra <- list(
    if (!is.null(rec)) tl_event_rows(rec, sl, opt),
    if (!is.null(resp_events)) {
      tl_keep(resp_events, resp_events$USUBJID %in% sl$USUBJID)
    },
    if (opt$death) pt_disc_rows(tb$adsl, sl, parts$trt),
    pt_patient_rows(sl, opt, resp_bor)
  )
  out <- tl_bind(c(list(rec), extra))
  # Death before progression, so it keeps the timeline's cross.
  opt$events <- c("Serious AE", "Fatal AE", "Death", "Progression",
                  pt_reason_levels(out$.event))
  tl_finish(out, sl, opt, group)
}

# --- inputs ----------------------------------------------------------------

# The tables, normalised as the patient profile does: ADSL and those of the
# layers drawn.
pt_tables <- function(data, layers) {
  if (is.data.frame(data)) data <- list(adae = data)
  tabs <- if (inherits(data, "dm")) dm::dm_get_tables(data) else data
  if (!is.list(tabs)) {
    stop("The patient timeline needs a dm with adsl.", call. = FALSE)
  }
  names(tabs) <- tolower(names(tabs))
  tabs <- pp_normalize_tables(tabs)$tables
  get <- function(nm) {
    x <- tabs[[nm]]
    if (!is.null(x)) as.data.frame(x)
  }
  out <- list(adsl = get("adsl"), adae = get("adae"), adcm = get("adcm"),
              adex = get("adex"), adrs = get("adrs"))
  if (is.null(out$adsl) && is.null(out$adae)) {
    stop("The patient timeline needs adsl in its input.", call. = FALSE)
  }
  need <- c(Response = "adrs", "Adverse events" = "adae",
            "Con meds" = "adcm")
  need <- need[names(need) %in% layers]
  others <- setdiff(layers, names(need))
  if (length(need) && !length(others) &&
        all(vapply(out[need], is.null, NA))) {
    stop("The patient timeline needs ", paste(need, collapse = " or "),
         " in its input.", call. = FALSE)
  }
  out
}

# The arm column: the setting, else the study's declared arm, else the first
# usual one ADSL has.
pt_group <- function(group, roles, adsl) {
  group <- as.character(unlist(group %||% character()))
  group <- group[!is.na(group) & nzchar(trimws(group))]
  if (length(group)) return(trimws(group[[1L]]))
  if (length(roles$arm) && nzchar(roles$arm)) return(roles$arm)
  hit <- intersect(c("ACTARM", "TRT01A", "ARM"), names(adsl))
  if (length(hit)) hit[[1L]]
}

# The study's roles from the board's "study_roles" option, NULL outside a
# board or when the board has none.

#' The study's roles, as the board declares them
#'
#' Reads the "study_roles" board option ([new_study_roles_option()]) of the
#' board the calling block sits on, so the patient timeline picks the arm
#' and severity columns the patient profile picks. Called in the block's
#' prepare script.
#'
#' @return A list (`arm`, `severity`, `timeline`, `indication`, `subject`;
#'   `NULL` where not declared), or `NULL` outside a board.
#' @export
patient_timeline_roles <- function() {
  session <- tryCatch(blockr.core::get_session(), error = function(e) NULL)
  if (is.null(session)) return(NULL)
  val <- tryCatch(
    blockr.core::get_board_option_or_null("study_roles", session),
    error = function(e) NULL
  )
  if (is.null(val)) return(NULL)
  val <- study_roles_normalize(val)
  lapply(val, function(v) if (length(v) && nzchar(v)) v)
}

# --- AEs: colour and term picks ---------------------------------------------

# Which column colours the AEs. "study severity" resolves the column as the
# patient profile does (pp_sev_column()) and copies it to where tl_ae()
# reads a grade (ATOXGR) or a severity (ASEV). The mode tl_ae() then takes is
# kept as the "pt_colour" attribute.
pt_ae_colour <- function(ae, opt, roles) {
  mode <- switch(opt$ae_colour, grade = "grade", severity = "severity",
                 "one colour" = "one colour", NULL)
  if (is.null(mode)) {
    col <- pp_sev_column(names(ae), roles$severity)
    if (is.null(col)) {
      mode <- "one colour"
    } else {
      v <- ae[[col]]
      grade <- col %in% c("ATOXGR", "AETOXGR") ||
        (!col %in% c("ASEV", "AESEV") &&
           all(grepl("^[0-9]+$", stats::na.omit(as.character(v)))))
      if (grade) {
        ae$ATOXGR <- v
        mode <- "grade"
      } else {
        ae$ASEV <- v
        mode <- "severity"
      }
    }
  }
  attr(ae, "pt_colour") <- mode
  ae
}

#' The terms a patient timeline can narrow its AEs or con meds to
#'
#' Every value of each coding level the study carries, coarsest first, as
#' "Body system: Cardiac disorders" or "Preferred term: Nausea": the choices
#' of the block's "Only these AEs" and "Only these con meds" rows. The
#' levels are the patient profile's find box levels.
#'
#' @param data The block's input dm.
#' @param what `"ae"` or `"cm"`.
#' @return A character vector; one placeholder when the table is missing.
#' @export
patient_timeline_terms <- function(data, what = c("ae", "cm")) {
  what <- match.arg(what)
  tb <- tryCatch(pt_tables(data, character()), error = function(e) NULL)
  tbl <- if (identical(what, "ae")) tb$adae else tb$adcm
  levels <- if (identical(what, "ae")) PP_AE_LEVELS else PP_CM_LEVELS
  groups <- if (!is.null(tbl)) pp_find_vocab(tbl, levels)
  out <- unlist(lapply(groups, function(g) {
    v <- vapply(g$options, `[[`, character(1L), "value")
    paste0(g$label, ": ", aet_title(v))
  }), use.names = FALSE)
  if (length(out)) out else paste("No", if (what == "ae") "AE" else "con med",
                                  "terms in the data")
}

# Term choices back to find picks: "Body system: Cardiac disorders" is
# list(col = "AEBODSYS", value = "Cardiac disorders").
pt_picks <- function(terms, levels) {
  terms <- as.character(unlist(terms))
  terms <- terms[!is.na(terms) & nzchar(terms)]
  if (!length(terms)) return(list())
  lab <- sub(": .*$", "", terms)
  val <- sub("^[^:]*: ", "", terms)
  col <- names(levels)[match(lab, unname(levels))]
  keep <- !is.na(col)
  unname(Map(function(c, v) list(col = c, value = v), col[keep], val[keep]))
}

# --- response ---------------------------------------------------------------

# Response runs from ADRS (swimmer_resp_rows()), on a row "Response". The
# first progression and each patient's best response travel as attributes:
# the progression marks the patient row, the best response its listing.
tl_response <- function(adrs, sl, opt, cutoff) {
  rs <- swimmer_response(adrs, opt$response)
  rs <- rs[rs$USUBJID %in% sl$USUBJID & !is.na(sl$T0[match(rs$USUBJID,
                                                          sl$USUBJID)]), ,
           drop = FALSE]
  if (!is.na(cutoff)) rs <- rs[rs$ADT <= cutoff, , drop = FALSE]
  if (!nrow(rs)) return(NULL)
  ids <- sl$USUBJID
  t0 <- stats::setNames(sl$T0, ids)
  # The last run ends at last contact (or the cutoff), never before the last
  # assessment.
  end <- if (identical(opt$ongoing, "last contact")) {
    lc <- sl$LSTALVDT
    lc[is.na(lc)] <- sl$TRTEDT[is.na(lc)]
    lc
  } else {
    rep(cutoff, length(ids))
  }
  fu <- stats::setNames(as.numeric(end), ids)
  last <- tapply(as.numeric(rs$ADT), rs$USUBJID, max)
  fu[names(last)] <- pmax(fu[names(last)], last, na.rm = TRUE)
  r <- swimmer_resp_rows(rs, fu, t0)
  n <- length(r$USUBJID)
  read <- as.character(r$colour)
  p <- list(
    USUBJID = r$USUBJID, Row = rep("Response", n),
    .src = rep("Response", n), Start = r$start,
    End = pmax(r$end, r$start + 0.6), .open = rep(FALSE, n),
    .colour = read, .label = r$label, Type = rep("Response", n),
    Details = ifelse(read == "Not assessed", NA_character_,
                     paste0(read, " (", opt$response, ")")),
    Days = paste0("Day ", r$start, " to ", round(r$end)),
    .svar = ifelse(read == "Not assessed", NA_character_, "response"),
    .skey = ifelse(read == "Not assessed", NA_character_, read)
  )
  p <- tl_keep(p, is.finite(p$Start))
  pd <- swimmer_pd_rows(rs, t0)
  if (!is.null(pd)) {
    m <- length(pd$USUBJID)
    attr(p, "pt_events") <- list(
      USUBJID = c(pd$USUBJID, pd$USUBJID),
      Row = c(rep(NA_character_, m), rep("Response", m)),
      .src = c(rep(NA_character_, m), rep("Response", m)),
      .event_x = c(pd$event_day, pd$event_day),
      .event = rep("Progression", 2L * m)
    )
  }
  attr(p, "pt_bor") <- stats::setNames(swimmer_bor_of(rs, ids), ids)
  p
}

# --- discontinuation, patient records ---------------------------------------

# The discontinuation reason (ADSL DCTREAS) as a tag at the last dose, in
# the swimmer's categories (swimmer_reason_category()). A patient still on
# treatment, or one whose reason is death, has none. With treatment drawn,
# the tag sits on the lane of the drug that ended last, as on the swimmer.
pt_disc_rows <- function(adsl, sl, trt = NULL) {
  why <- swimmer_reason(aet_col(adsl, "DCTREAS"))
  if (is.null(why)) return(NULL)
  why <- why[match(sl$USUBJID, as.character(adsl$USUBJID))]
  cat <- swimmer_reason_category(why)
  x <- aet_day(sl$TRTEDT, sl$T0)
  keep <- !is.na(cat$tag) & is.finite(x)
  if (!any(keep)) return(NULL)
  ids <- sl$USUBJID[keep]
  list(USUBJID = ids, .event_x = x[keep],
       .event = cat$name[keep], .tag = cat$tag[keep],
       .lane_at = pt_last_lane(trt, ids))
}

# Per patient in `ids`, the lane of its treatment record that ends last; NA
# without one.
pt_last_lane <- function(trt, ids) {
  if (is.null(trt) || !length(trt$USUBJID)) {
    return(rep(NA_character_, length(ids)))
  }
  o <- order(trt$USUBJID, -trt$End)
  first <- o[!duplicated(trt$USUBJID[o])]
  trt$.lane_at[first][match(ids, trt$USUBJID[first])]
}

# --- treatment --------------------------------------------------------------

# A thin line per drug, as the swimmer draws it: ADEX through the swimmer's
# exposure, folding and drug rows (swimmer_drug_rows()), moved from the
# swimmer's days (from the first dose) to the timeline's (from `opt$origin`).
# The drugs' order and colours travel as attributes.
pt_drug_lines <- function(tb, sl, opt, cutoff) {
  if (is.null(tb$adex)) return(NULL)
  ss <- swimmer_subjects(tb$adsl)
  ss <- ss[ss$USUBJID %in% sl$USUBJID & !is.na(ss$TRTSDT), , drop = FALSE]
  ex <- swimmer_exposure(tb$adex, ss, role = "study")
  if (is.null(ex) || !nrow(ex)) return(NULL)
  ex <- swimmer_close_study(ex, ss)
  if (!is.na(cutoff)) {
    ex <- ex[ex$start <= cutoff, , drop = FALSE]
    ex$end[!is.na(ex$end) & ex$end > cutoff] <- NA
  }
  if (!nrow(ex)) return(NULL)
  ex$open <- is.na(ex$end)
  ex$end[ex$open] <- if (is.na(cutoff)) ex$start[ex$open] else cutoff
  t0 <- stats::setNames(ss$TRTSDT, ss$USUBJID)
  on_at <- if (is.na(cutoff)) max(ex$end, na.rm = TRUE) else cutoff
  ex$on <- swimmer_ongoing(ex, on_at) &
    is.na(ss$TRTEDT[match(ex$USUBJID, ss$USUBJID)])
  ex <- swimmer_fold(ex, PT_MAX_DRUGS, t0)
  d <- swimmer_drug_rows(ex, t0, opt$doses, on_at)
  if (is.null(d) || !length(d$USUBJID)) return(NULL)
  d <- tl_keep(d, !is.na(d$start))
  i <- match(d$USUBJID, sl$USUBJID)
  # Swimmer day 1 is the first dose; the timeline's day 1 is the origin.
  shift <- aet_day(t0[d$USUBJID], sl$T0[i]) - 1
  lane <- as.character(d$lane)
  tip <- as.character(d$tip)
  n <- length(d$USUBJID)
  raw <- unique(as.character(aet_col(tb$adex, c("EXTRT", "CMDECOD"))))
  k <- match(lane, swimmer_drug_name(raw))
  p <- list(
    USUBJID = d$USUBJID, Row = lane, .src = rep("Treatment", n),
    Start = d$start + shift, End = d$end + shift,
    .open = d$ongoing %in% TRUE, .colour = lane,
    .style = as.character(d$mark), .label = lane,
    Type = rep("Treatment", n),
    Details = sub(" · .*$", "", tip),
    Days = ifelse(grepl(" · ", tip), sub("^.*? · ", "", tip),
                  NA_character_),
    .svar = ifelse(is.na(k), NA_character_, "drug"), .skey = raw[k],
    .lane_at = lane
  )
  drugs <- levels(ex$lane)
  drugs <- drugs[drugs %in% lane]
  attr(p, "pt_lanes") <- drugs
  attr(p, "pt_palette") <- pt_drug_palette(drugs)
  p
}

# The swimmer's drug colours: its drug palette in lane order, placebo and
# "Other" grey.
pt_drug_palette <- function(drugs) {
  col <- stats::setNames(rep_len(SWIMMER_DRUGS, length(drugs)), drugs)
  col[grepl("placebo", drugs, ignore.case = TRUE)] <- "#8f9bb3"
  col[drugs == "Other"] <- "#a3a9b4"
  col
}

# The timeline's dose bars (tl_treatment()), each drug on a lane of its own.
pt_dose_bars <- function(adex, sl, opt, cutoff) {
  p <- tl_treatment(adex, sl, opt, cutoff)
  if (is.null(p) || !length(p$USUBJID)) return(NULL)
  p$.lane_at <- p$Row
  attr(p, "pt_lanes") <- sort(unique(p$Row))
  p
}

# One record per patient with no row: days on treatment, status and best
# response.
pt_patient_rows <- function(sl, opt, bor = NULL) {
  p <- tl_patient_rows(sl, opt)
  p$.status <- ifelse(!is.na(sl$DTHDT), "Died",
                      ifelse(is.na(sl$TRTEDT), "On treatment",
                             "Off treatment"))
  p$.bor <- if (length(bor)) unname(bor[sl$USUBJID]) else
    rep(NA_character_, nrow(sl))
  p
}

# The discontinuation categories drawn, the most frequent first.
pt_reason_levels <- function(ev) {
  ev <- ev[!is.na(ev)]
  why <- setdiff(ev, c("Serious AE", "Fatal AE", "Progression", "Death"))
  if (!length(why)) return(character())
  tab <- table(why)
  names(tab)[order(-tab, names(tab))]
}
