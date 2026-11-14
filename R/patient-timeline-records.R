# The ADAE records behind the patient timeline, for the table block's data
# view (blockr.viz::viz_records()): the table icon left of the download opens
# them under the timeline, narrowed to the patient clicked, and the download
# menu writes them as CSV and Excel.

# The columns the listing shows, each the first of its names the study
# carries, in this order. The grade is the board's severity column when the
# study roles declare one.
PT_AE_RECORD_COLS <- list(
  USUBJID = "USUBJID",
  seq = "AESEQ",
  start_day = c("ASTDY", "AESTDY"),
  end_day = c("AENDY", "AEENDY"),
  term = "AETERM",
  grade = c("ATOXGR", "AETOXGR", "ASEV", "AESEV"),
  related = c("AEREL", "AREL"),
  serious = "AESER",
  start = c("ASTDT", "AESTDTC"),
  end = c("AENDT", "AEENDTC")
)

#' The AE records a patient timeline lists
#'
#' The ADAE records the timeline draws (treatment-emergent ones when it
#' draws those, the terms and the serious or ongoing AEs it narrows to),
#' with the listing's columns: sequence number, start and stop day, reported
#' term, toxicity grade (or severity), relation to treatment, serious, start
#' and end date.
#'
#' @param ae ADAE, narrowed to the picked terms.
#' @param opt The frame's options (`ae_records`, `ae_show`).
#' @param roles The study roles (`severity`).
#' @return A data frame, or NULL when ADAE has no USUBJID.
#' @noRd
pt_ae_records <- function(ae, opt, roles = NULL) {
  if (!"USUBJID" %in% names(ae)) {
    return(NULL)
  }
  if (identical(opt$ae_records, "treatment-emergent") &&
        "TRTEMFL" %in% names(ae)) {
    ae <- ae[aet_flag(ae$TRTEMFL), , drop = FALSE]
  }
  if (identical(opt$ae_show, "serious AEs") && "AESER" %in% names(ae)) {
    ae <- ae[aet_flag(ae$AESER), , drop = FALSE]
  }
  end <- intersect(c("AENDT", "AEENDTC", "AENDY"), names(ae))
  if (identical(opt$ae_show, "ongoing AEs") && length(end)) {
    v <- ae[[end[[1L]]]]
    ae <- ae[is.na(v) | as.character(v) == "", , drop = FALSE]
  }
  cols <- PT_AE_RECORD_COLS
  sev <- roles$severity
  if (length(sev) == 1L && !is.na(sev) && nzchar(sev)) {
    cols$grade <- c(sev, cols$grade)
  }
  keep <- unique(unlist(lapply(cols, function(nms) {
    intersect(nms, names(ae))[1L]
  })))
  keep <- keep[!is.na(keep)]
  out <- as.data.frame(ae[keep], stringsAsFactors = FALSE)
  # By patient, then start day and sequence number, as a listing reads.
  by <- lapply(c("USUBJID", intersect(c(cols$start_day, cols$seq), keep)),
               function(nm) out[[nm]])
  out <- out[do.call(order, by), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# The frame with its AE records, those of the patients it draws.
pt_with_records <- function(out, records, sl) {
  if (is.null(records)) {
    return(out)
  }
  records <- records[as.character(records$USUBJID) %in% sl$USUBJID, ,
                     drop = FALSE]
  rownames(records) <- NULL
  blockr.viz::viz_records(out, records, "Adverse events")
}
