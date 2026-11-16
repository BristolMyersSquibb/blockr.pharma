# The patient facts the profile marks as the patient timeline marks them:
# death, the discontinuation reason and last contact. One reading of each,
# shared by the overview, the cohort strip and the header, so the profile
# and new_timeline_block() agree on when they happened.

#' The ink of an event mark
#'
#' The colour blockr.viz's table draws its event shapes and tags in
#' (`--vz-ev-ink`), so a death cross reads the same in both blocks. The
#' overview draws with the theme's `--blockr-grey-600`, which this is in
#' the light scheme; the static image and the tooltips use this.
#' @noRd
PP_EV_INK <- "#4b5563"

#' A date column of a one-patient ADSL, as a Date
#' @noRd
pp_sl_date <- function(sl, col) {
  if (is.null(sl) || !nrow(sl) || !col %in% colnames(sl)) return(as.Date(NA))
  pp_as_date(sl[[col]][[1L]])
}

#' Last contact
#'
#' ADSL `LSTALVDT`, else the last dose (`TRTEDT`): where a record without an
#' end stops, as the patient timeline's default ("last contact") stops it.
#'
#' @param sl ADSL rows; the first is read.
#' @return A Date, `NA` when neither is known.
#' @noRd
pp_last_contact <- function(sl) {
  lc <- pp_sl_date(sl, "LSTALVDT")
  if (is.na(lc)) lc <- pp_sl_date(sl, "TRTEDT")
  lc
}

#' When the patient died
#'
#' ADSL `DTHDT`; without one, the end of the first AE flagged fatal
#' (`AESDTH`), as the patient timeline marks it. A death flag (`DTHFL`)
#' without a date is no position, so it is not drawn.
#'
#' @param sl ADSL rows; the first is read.
#' @param ae ADAE rows of the patient, or `NULL`.
#' @return `list(date, day)` (either may be `NA`, not both), or `NULL`.
#' @noRd
pp_death_at <- function(sl, ae = NULL) {
  dt <- pp_sl_date(sl, "DTHDT")
  if (!is.na(dt)) return(list(date = dt, day = NA_real_))
  if (is.null(ae) || !nrow(ae) || !"AESDTH" %in% colnames(ae)) return(NULL)
  f <- which(aet_flag(ae$AESDTH))
  if (!length(f)) return(NULL)
  at <- function(nm) if (nm %in% colnames(ae)) ae[[nm]][f] else rep(NA, length(f))
  date <- pp_as_date(at("AENDT"))
  day <- suppressWarnings(as.numeric(at("AENDY")))
  ok <- which(!is.na(date) | !is.na(day))
  if (!length(ok)) return(NULL)
  k <- ok[[1L]]
  list(date = date[[k]], day = day[[k]])
}

#' Whether the patient died without a date to draw
#' @noRd
pp_death_undated <- function(sl, ae = NULL) {
  if (!is.null(pp_death_at(sl, ae))) return(FALSE)
  "DTHFL" %in% colnames(sl) && nrow(sl) > 0L &&
    isTRUE(aet_flag(sl$DTHFL[[1L]]))
}

#' Why treatment stopped
#'
#' ADSL `DCTREAS` (or a column labelled as the treatment discontinuation
#' reason; never the study reason) in the patient timeline's categories ([swimmer_reason_category()]): a two-letter tag and
#' the reason as a reader says it. A completed or ongoing disposition, a
#' death, or no reason gives none.
#'
#' @param sl ADSL rows; the first is read.
#' @return `list(tag, name, reason)`, or `NULL`.
#' @noRd
pp_disc_reason <- function(sl, col = "DCTREAS") {
  if (is.null(sl) || !nrow(sl) || !col %in% colnames(sl)) return(NULL)
  why <- swimmer_reason(sl[[col]][1L])
  if (is.null(why) || is.na(why)) return(NULL)
  cat <- swimmer_reason_category(why)
  if (is.na(cat$tag[[1L]])) return(NULL)
  list(tag = cat$tag[[1L]], name = cat$name[[1L]], reason = why)
}

#' Each drug's own discontinuation reason, from DS
#'
#' The disposition events of SDTM DS (or ADaM ADDS) whose `DSSCAT` names a
#' drug: `DSCAT` "DISPOSITION EVENT" when the table has `DSCAT`. The reason
#' is `DSDECOD`, else `DSTERM`, in the patient timeline's categories
#' ([swimmer_reason_category()]); a completed or ongoing disposition, or a
#' death, gives none. A record naming no drug of the study is matched by
#' nothing downstream, so nothing is guessed.
#'
#' @param ds DS or ADDS, a data frame, or `NULL`.
#' @return A data frame, one row per patient and drug: `USUBJID`, `key`
#'   (the lowercased `DSSCAT`, to match ADEX `EXTRT`), `date` (`ASTDT`, else
#'   `DSSTDTC`, `NA` without), `tag`, `name` and `reason`. `NULL` without
#'   any.
#' @noRd
pp_ds_drug_reasons <- function(ds) {
  if (is.null(ds) || !nrow(ds)) return(NULL)
  ds <- as.data.frame(ds)
  scat <- aet_col(ds, "DSSCAT")
  raw <- aet_col(ds, c("DSDECOD", "DSTERM"))
  if (is.null(scat) || is.null(raw) || is.null(ds$USUBJID)) return(NULL)
  scat <- trimws(as.character(scat))
  ok <- !is.na(scat) & nzchar(scat)
  dcat <- aet_col(ds, "DSCAT")
  if (!is.null(dcat)) {
    ok <- ok & toupper(trimws(as.character(dcat))) %in% "DISPOSITION EVENT"
  }
  why <- swimmer_reason(raw)
  cat <- swimmer_reason_category(why)
  ok <- ok & !is.na(cat$tag)
  dt <- aet_col(ds, c("ASTDT", "DSSTDTC"))
  dt <- if (is.null(dt)) rep(as.Date(NA), nrow(ds)) else pp_as_date(dt)
  out <- data.frame(
    USUBJID = as.character(ds$USUBJID), key = tolower(scat), date = dt,
    tag = cat$tag, name = cat$name, reason = why, stringsAsFactors = FALSE
  )[ok, , drop = FALSE]
  out <- out[!duplicated(out[c("USUBJID", "key")]), , drop = FALSE]
  if (!nrow(out)) return(NULL)
  rownames(out) <- NULL
  out
}

#' The profile's per-drug reason tags
#'
#' One patient's [pp_ds_drug_reasons()] at the end of each drug's line, as
#' the treatment lane draws the lines: the last end of the drug's ADEX
#' records, a record without an end stopping at the last dose. A tag that
#' repeats the ADSL treatment reason ([pp_disc_reason()]) at the last dose is
#' left out, as on the patient timeline.
#'
#' @param tbls The profile's tables.
#' @param sl The patient's ADSL row.
#' @param drugs The drugs with a slot (ADEX `EXTRT`, trimmed).
#' @param src,use_day ADEX's start column and whether it is a study day.
#' @param trt_end The last dose's x.
#' @return A data frame: `drug`, `x`, `tag`, `name`, `reason`; `NULL`
#'   without any.
#' @noRd
pp_drug_reason_marks <- function(tbls, sl, drugs, src, use_day, trt_end,
                                 ref_ms, mode) {
  if (!length(drugs)) return(NULL)
  ds <- tbls[["adds"]] %||% tbls[["ds"]]
  r <- pp_ds_drug_reasons(ds)
  if (is.null(r)) return(NULL)
  r <- r[r$USUBJID %in% as.character(sl$USUBJID[[1L]]) &
           r$key %in% tolower(drugs), , drop = FALSE]
  ex <- as.data.frame(tbls[["adex"]])
  if (!nrow(r) || !src %in% names(ex)) return(NULL)
  ex <- ex[!is.na(ex[[src]]), , drop = FALSE]
  end_col <- if (use_day) "AENDY" else "AENDT"
  x_of <- function(v) {
    pp_xval_pref_day(if (use_day) NULL else v, if (use_day) v else NULL,
                     ref_ms, mode)
  }
  x <- vapply(seq_len(nrow(ex)), function(i) {
    x0 <- x_of(ex[[src]][i])
    e <- if (end_col %in% names(ex)) ex[[end_col]][i] else NA
    if (is.na(e)) max(trt_end, x0) else x_of(e)
  }, 0)
  key <- tolower(trimws(as.character(ex$EXTRT)))
  r$drug <- drugs[match(r$key, tolower(drugs))]
  r$x <- vapply(r$key, function(k) max(x[key %in% k]), 0, USE.NAMES = FALSE)
  trt <- pp_disc_reason(sl)
  if (!is.null(trt)) {
    r <- r[!(r$tag == trt$tag & r$x == trt_end), , drop = FALSE]
  }
  if (!nrow(r)) return(NULL)
  r[c("drug", "x", "tag", "name", "reason")]
}

#' Why the patient left the study
#'
#' ADSL `DCSREAS` (or `DCDECOD`, `DSDECOD`, a column labelled as the study
#' discontinuation reason) as [pp_disc_reason()] reads the treatment reason,
#' with its date: `EOSDT`, else `RFENDT`. `NULL` without a reason or a
#' date, and when it repeats the treatment reason's category.
#'
#' @param sl ADSL rows; the first is read.
#' @return `list(tag, name, reason, date)`, or `NULL`.
#' @noRd
pp_eos_reason <- function(sl) {
  why <- pp_disc_reason(sl, "DCSREAS")
  if (is.null(why)) return(NULL)
  dt <- pp_sl_date(sl, "EOSDT")
  if (is.na(dt)) dt <- pp_sl_date(sl, "RFENDT")
  if (is.na(dt)) return(NULL)
  trt <- pp_disc_reason(sl)
  if (!is.null(trt) && identical(trt$tag, why$tag)) return(NULL)
  why$date <- dt
  why
}

#' Keep the treatment-emergent AEs
#'
#' The patient timeline draws treatment-emergent AEs by default (ADAE
#' `TRTEMFL`), and so does the profile: its panels, header count and strip
#' all read the dm this returns. A study without `TRTEMFL` keeps every AE.
#'
#' @param dm_obj A normalized `dm`.
#' @return A `dm`; the same object when nothing was dropped.
#' @noRd
pp_teae_only <- function(dm_obj) {
  if (!inherits(dm_obj, "dm")) return(dm_obj)
  tbls <- dm::dm_get_tables(dm_obj)
  ae <- tbls[["adae"]]
  if (is.null(ae) || !"TRTEMFL" %in% colnames(ae)) return(dm_obj)
  ae <- as.data.frame(ae)
  keep <- aet_flag(ae$TRTEMFL)
  if (all(keep)) return(dm_obj)
  tbls <- lapply(tbls, as.data.frame)
  # dplyr_row_slice(), not `[`: it keeps the column labels the tooltips read.
  tbls[["adae"]] <- dplyr::dplyr_row_slice(ae, which(keep))
  do.call(dm::dm, tbls)
}

#' Whether a dm's AEs carry the treatment-emergent flag
#' @noRd
pp_has_teae_flag <- function(dm_obj) {
  if (!inherits(dm_obj, "dm")) return(FALSE)
  ae <- dm::dm_get_tables(dm_obj)[["adae"]]
  !is.null(ae) && "TRTEMFL" %in% colnames(ae)
}
