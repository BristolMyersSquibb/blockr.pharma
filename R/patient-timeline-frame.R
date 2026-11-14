# The patient timeline's frame: the timeline's frame (timeline_frame()) with
# the swimmer's layers added. One long frame, one record per thing drawn, for
# the spans column of new_patient_timeline_block():
#
# Treatment and the response draw on the patient row only (no row of their
# own); AEs and con meds open into a row per term, under a section title.
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
PT_TRT_DRAW <- c("lines per drug", "bars by dose")
PT_DOSES <- c("Gaps and dose changes", "Plain")
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
#' @param group The ADSL column the patients are grouped by, in `.arm`, as
#'   [patient_timeline_groups()] offers them; `"(none)"`: no grouping.
#'   Empty: the population filter's `Group` column when ADSL has one, else
#'   the arm of the board's "study_roles" option, else `ACTARM`, `TRT01A` or
#'   `ARM`. Every ADSL column is on each patient's record.
#' @param response The ADRS parameter of the overall response.
#' @param ae_colour The field that colours the AEs: a column of ADAE as
#'   [timeline_colour_fields()] lists them, or `"one colour"`. `"study
#'   severity"`, and a column the data lacks: the severity column of the
#'   board's "study_roles" option, else the first of `ASEV`, `AESEV`,
#'   `ATOXGR` and `AETOXGR`, as the patient profile picks it. `"grade"` and
#'   `"severity"` still work.
#' @param scale_map The board's scale map ([timeline_scale_map()]): a
#'   colour field's levels draw in the order it pins them, the last the
#'   worst.
#' @param treatment_as `"lines per drug"`: a thin line per drug in its
#'   colour, as the swimmer draws it. `"bars by dose"`: each drug's dose
#'   records as bars coloured by `trt_colour`, as the timeline draws them.
#'   Either way each drug has a lane of its own.
#' @param doses With lines per drug: `"Gaps and dose changes"` shows skipped
#'   cycles as gaps and each change of dose as a small tag with the new dose,
#'   `"Plain"` one solid line. Either way each piece's tooltip gives its own
#'   dose. The old `"Gaps and cuts"` reads as `"Gaps and dose changes"`.
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
                                   doses = "Gaps and dose changes",
                                   trt_colour = "dose level",
                                   origin = "first dose",
                                   cutoff = NULL,
                                   ongoing = "last contact",
                                   order_by = "number of AEs",
                                   roles = NULL,
                                   scale_map = NULL) {
  layers <- intersect(PT_LAYERS, as.character(unlist(layers)))
  if (!length(layers)) stop("Pick at least one layer.", call. = FALSE)
  arg <- function(x, choices) match.arg(as.character(x)[1L], choices)
  roles <- roles %||% list()
  opt <- list(
    ae_records = arg(ae_records, TL_AE_RECORDS),
    ae_show = arg(ae_show, TL_AE_SHOW),
    ae_colour = as.character(unlist(ae_colour))[1L],
    ae_lanes = arg(ae_lanes, names(TL_AE_LANES)),
    serious = arg(serious, TL_SERIOUS),
    death = "Discontinuation and death" %in% layers,
    cm_records = arg(cm_records, TL_CM_RECORDS),
    cm_colour = tl_cm_word(tl_cm_colour_value(cm_colour)),
    cm_lanes = arg(cm_lanes, names(TL_CM_LANES)),
    cm_prior = arg(cm_prior, TL_CM_PRIOR),
    trt_colour = arg(trt_colour, TL_TRT_COLOUR),
    treatment_as = arg(treatment_as, PT_TRT_DRAW),
    doses = swimmer_doses_value(doses),
    origin = arg(origin, TL_ORIGIN),
    ongoing = arg(ongoing, c("last contact", "data cutoff")),
    order_by = arg(order_by, PT_ORDER),
    response = as.character(unlist(response %||% "OVR"))[1L],
    # What tl_finish() does differently for this frame.
    src_order = c("Response", "Treatment", "AE", "CM"),
    lanes = c("Treatment", PT_FULL_LANES),
    full_lanes = PT_FULL_LANES,
    ae_palette = pt_ae_palette(),
    extra = c(".tag", ".bor", ".status", ".full")
  )
  tb <- pt_tables(data, layers)
  opt$scale_map <- scale_map
  opt$cmfield <- tl_cm_field(tb$adcm, tl_cm_colour_value(cm_colour))
  group <- pt_group(group, roles, tb$adsl)
  sl <- tl_subjects(tb, group, opt$origin, all_cols = TRUE)
  cutoff <- aet_date(cutoff)
  if (!length(cutoff) || is.na(cutoff[[1L]])) cutoff <- tl_cutoff(tb, sl)
  cutoff <- cutoff[[1L]]
  sl <- aet_cap_at(sl, cutoff)

  parts <- list()
  records <- NULL
  if ("Response" %in% layers && !is.null(tb$adrs)) {
    parts$resp <- tl_response(tb$adrs, sl, opt, cutoff)
  }
  if ("Adverse events" %in% layers && !is.null(tb$adae)) {
    ae <- tb$adae
    opt$ae_field <- tl_ae_field(ae, ae_colour, roles)
    ae <- ae[pp_find_match(ae, pt_picks(ae_terms, PP_AE_LEVELS),
                           PP_AE_SEARCH), , drop = FALSE]
    parts$ae <- tl_ae(ae, sl, opt, cutoff)
    records <- pt_ae_records(ae, opt, roles)
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
  opt$field_palettes <- tl_field_palettes(parts)
  # What the response part carries besides its records; tl_keep() drops it.
  resp_events <- attr(parts$resp, "pt_events")
  trt_events <- attr(parts$trt, "pt_events")
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
  parts <- tl_unique_rows(parts)
  # Treatment and the response draw on the patient row only: an opened
  # patient lists its AE terms and con meds, which the stripes cannot name.
  for (nm in intersect(c("trt", "resp"), names(parts))) {
    parts[[nm]]$Row <- rep(NA_character_, length(parts[[nm]]$USUBJID))
  }
  rec <- tl_bind(parts)
  extra <- list(
    if (!is.null(rec)) tl_event_rows(rec, sl, opt),
    if (!is.null(resp_events)) {
      tl_keep(resp_events, resp_events$USUBJID %in% sl$USUBJID)
    },
    if (!is.null(trt_events)) {
      tl_keep(trt_events, trt_events$USUBJID %in% sl$USUBJID)
    },
    if (opt$death) pt_disc_rows(tb$adsl, sl, parts$trt),
    pt_patient_rows(sl, opt, resp_bor)
  )
  out <- tl_bind(c(list(rec), extra))
  # Death before progression, so it keeps the timeline's cross.
  opt$events <- c("Serious AE", "Fatal AE", "Death", "Progression",
                  pt_reason_levels(out$.event))
  out <- tl_finish(out, sl, opt, group)
  # The legend heads the tagged events (reasons, dose changes) with it.
  if (!is.null(out$.tag)) attr(out$.tag, "label") <- "Tag"
  # The section an opened row is listed under (the table's row_sections).
  open <- !is.na(out$Row) & out$.lane %in% c("Adverse events", "Con meds")
  out$.section <- ifelse(open, as.character(out$.lane), NA_character_)
  # The patient columns' labels name them in the column picker and headers;
  # subsetting the patients on the way dropped them.
  for (nm in intersect(attr(sl, "pcols"), names(tb$adsl))) {
    lab <- attr(tb$adsl[[nm]], "label", exact = TRUE)
    if (!is.null(lab) && nm %in% names(out)) attr(out[[nm]], "label") <- lab
  }
  pt_with_records(out, records, sl)
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

# The column the patients are grouped by: the setting (`PT_NO_GROUP`: none),
# else the population filter's `Group`, else the study's arm
# (pt_arm_column()).
pt_group <- function(group, roles, adsl) {
  group <- as.character(unlist(group %||% character()))
  group <- group[!is.na(group) & nzchar(trimws(group))]
  if (length(group)) {
    group <- trimws(group[[1L]])
    if (identical(group, PT_NO_GROUP)) return(NULL)
    return(group)
  }
  if ("Group" %in% names(adsl)) return("Group")
  pt_arm_column(roles, adsl)
}

# The study's arm: the declared one, else the first usual one ADSL has.
pt_arm_column <- function(roles, adsl) {
  if (length(roles$arm) && nzchar(roles$arm)) return(roles$arm)
  hit <- intersect(c("ACTARM", "TRT01A", "ARM"), names(adsl))
  if (length(hit)) hit[[1L]]
}

# The "Group by" value of no grouping.
PT_NO_GROUP <- "(none)"

#' What the patient timeline can group its patients by
#'
#' The "Group by" choices of the patient timeline's gear: the population
#' filter's `Group` column when ADSL has one, the study's arm, then every
#' other ADSL column, and no grouping. Each named by the column's label and
#' name.
#'
#' @param data The block's input dm.
#' @param roles The study's roles ([patient_timeline_roles()]).
#' @return A named character vector, the first the default.
#' @export
patient_timeline_groups <- function(data, roles = NULL) {
  adsl <- tryCatch(pt_tables(data, "Treatment")$adsl, error = function(e) NULL)
  cols <- names(adsl)
  first <- c(if ("Group" %in% cols) "Group", pt_arm_column(roles, adsl))
  cols <- unique(c(first, setdiff(cols, c("USUBJID", TL_FRAME_OWN))))
  lab <- vapply(cols, function(nm) {
    l <- attr(adsl[[nm]], "label", exact = TRUE)
    if (identical(nm, "Group")) return("Group (population filter)")
    if (is.character(l) && length(l) == 1L && nzchar(l) && l != nm) {
      paste0(l, " (", nm, ")")
    } else {
      nm
    }
  }, "")
  stats::setNames(c(cols, PT_NO_GROUP), c(lab, "No grouping"))
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

# The patient profile's severity colours (pp_sev_colors), under the
# timeline's level names: what both draw where the board's scale map does not
# bind the severity column.
pt_ae_palette <- function() {
  c(Mild = pp_sev_colors[["MILD"]], Moderate = pp_sev_colors[["MODERATE"]],
    Severe = pp_sev_colors[["SEVERE"]],
    stats::setNames(unname(pp_sev_colors[as.character(1:5)]),
                    paste("Grade", 1:5)),
    AE = AET_COLOURS[["AE"]])
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
      USUBJID = pd$USUBJID, Row = rep(NA_character_, m),
      .event_x = pd$event_day, .event = rep("Progression", m)
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
  # Every drug has its lane: swimmer_fold() only orders study drugs.
  ex <- swimmer_fold(ex, Inf, t0)
  d <- swimmer_drug_rows(ex, t0, opt$doses, on_at)
  if (is.null(d) || !length(d$USUBJID)) return(NULL)
  # Swimmer day 1 is the first dose; the timeline's day 1 is the origin.
  shift_of <- function(ids) {
    aet_day(t0[ids], sl$T0[match(ids, sl$USUBJID)]) - 1
  }
  ch <- tl_keep(d, !is.na(d$event_day))
  d <- tl_keep(d, !is.na(d$start))
  shift <- shift_of(d$USUBJID)
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
  attr(p, "pt_events") <- pt_change_events(ch, shift_of(ch$USUBJID))
  p
}

# The swimmer's dose changes (tagged events on a drug's lane) as the
# timeline's events, moved by `shift` days. NULL without any.
pt_change_events <- function(ch, shift) {
  m <- length(ch$USUBJID)
  if (!m) return(NULL)
  tip <- as.character(ch$tip)
  list(
    USUBJID = ch$USUBJID, Row = rep(NA_character_, m),
    .event_x = ch$event_day + shift,
    .event = as.character(ch$event), .tag = as.character(ch$tag),
    .lane_at = as.character(ch$lane), Type = rep("Treatment", m),
    Details = sub("^.*? \u00b7 ", "", tip),
    Days = paste("Day", ifelse(ch$event_day + shift >= 1,
                               ch$event_day + shift,
                               ch$event_day + shift - 1))
  )
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

# --- order -----------------------------------------------------------------

# The listing columns the patients can be sorted by, and how: the summary of
# patient_timeline_columns() named, its value per patient and its direction.
PT_SORTS <- list(
  `Days on treatment` = list(f = function(d) d$.days_trt, fn = "max",
                             desc = TRUE),
  `Best response` = list(f = function(d) match(d$.bor, SWIMMER_BOR),
                         fn = "min", desc = FALSE),
  Status = list(f = function(d) d$.status, fn = "first", desc = FALSE),
  AEs = list(f = function(d) d$.ae %in% 1, fn = "sum", desc = TRUE),
  Ongoing = list(f = function(d) d$.ae_open %in% 1, fn = "sum", desc = TRUE),
  Serious = list(f = function(d) d$.ae_ser %in% 1, fn = "sum", desc = TRUE),
  `Worst AE` = list(f = function(d) as.integer(d$.worst), fn = "max",
                    desc = TRUE),
  `Con meds` = list(f = function(d) d$.cm %in% 1, fn = "sum", desc = TRUE),
  `New on treatment` = list(f = function(d) d$.cm_new %in% 1, fn = "sum",
                            desc = TRUE)
)

# The column each saved "Sort patients by" now sorts by.
PT_ORDER_COLUMNS <- c(
  "days on treatment" = "Days on treatment", "number of AEs" = "AEs",
  "worst AE" = "Worst AE", "best response" = "Best response",
  "ongoing AEs" = "Ongoing", "number of con meds" = "Con meds"
)

#' Sort the patient timeline's patients by a listing column
#'
#' The last step of the patient timeline's prepare script: the patients in
#' the order of the first listing column, each patient's records kept
#' together and in order. The gear sets `by` to that column, so a column the
#' script adds sorts too.
#'
#' @param frame The frame of [patient_timeline_frame()], with any columns
#'   the script adds.
#' @param by A listing column of the gear ("AEs", "Worst AE", ...; counts,
#'   days and the worst AE largest first, the best response best first) or
#'   a column of `frame` (its first value per patient, smallest or first
#'   level first). Empty, or a column the frame lacks: by patient.
#' @return `frame`, its rows reordered.
#' @export
patient_timeline_sort <- function(frame, by = "") {
  by <- as.character(unlist(by %||% ""))[1L]
  id <- as.character(frame$USUBJID)
  if (!length(id)) return(frame)
  ids <- unique(id)
  first_of <- function(x) {
    has <- !is.na(x)
    x[has][match(ids, id[has])]
  }
  per <- function(x, fn) {
    if (fn == "first") return(first_of(x))
    x <- as.numeric(x)
    v <- vapply(split(x, factor(id, levels = ids)), function(z) {
      z <- z[!is.na(z)]
      if (!length(z)) return(NA_real_)
      switch(fn, sum = sum(z), max = max(z), min = min(z))
    }, numeric(1L))
    unname(v)
  }
  s <- if (!is.na(by) && nzchar(by)) PT_SORTS[[by]]
  key <- if (!is.null(s)) {
    v <- per(s$f(frame), s$fn)
    if (s$desc) {
      if (is.numeric(v)) -v else stats::setNames(-xtfrm(v), NULL)
    } else {
      v
    }
  } else if (!is.na(by) && nzchar(by) && by %in% names(frame)) {
    x <- frame[[by]]
    v <- first_of(x)
    if (is.factor(x)) as.integer(factor(v, levels = levels(x))) else v
  }
  ord <- if (is.null(key)) order(ids) else order(key, ids, na.last = TRUE)
  o <- order(match(id, ids[ord]), seq_along(id))
  out <- frame[o, , drop = FALSE]
  # `[` drops a column's label and colour attributes, and the frame's own.
  for (nm in names(frame)) {
    at <- attributes(frame[[nm]])
    at <- at[setdiff(names(at), c("names", "dim", "dimnames"))]
    if (length(at)) attributes(out[[nm]]) <- c(attributes(out[[nm]]), at[
      setdiff(names(at), names(attributes(out[[nm]])))])
  }
  keep <- setdiff(names(attributes(frame)), c("names", "row.names", "class"))
  for (a in keep) attr(out, a) <- attr(frame, a)
  rownames(out) <- NULL
  out
}
