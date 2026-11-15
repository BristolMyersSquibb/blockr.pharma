# The patient facts the profile marks as the patient timeline marks them:
# death, the discontinuation reason and last contact. One reading of each,
# shared by the overview, the cohort strip and the header, so the profile
# and new_timeline_block() agree on when they happened.

#' The ink of an event mark
#'
#' The colour blockr.viz's table draws its event shapes and tags in
#' (`--vz-ev-ink`), so a death cross reads the same in both blocks.
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
