# Make the patient profile block legible to the blockr.ai assistant. Its result
# is a passthrough dm (the profile renders the single-patient dm but does not
# transform it), so blockr.ai's data effect is blind and reads as
# "N tables, UNCHANGED" -- which looks like the config did nothing even though it
# changed WHICH visualizations are shown. The meaningful artifact is the CONFIG
# (`selected` views + `viz_settings`), so this `config_effect()` method describes
# that instead, the same way the drilldown blocks do in blockr.viz.
#
# Registered onto blockr.ai's generic at load (defensive: no hard dependency on
# blockr.ai, no-op when it is absent or too old to export config_effect).

`%||%` <- function(a, b) if (is.null(a)) b else a

#' @noRd
config_effect.patient_profile_block <- function(block, args, data = NULL, ...) {
  sel <- as.character(unlist(args$selected, use.names = FALSE))
  sel <- sel[!is.na(sel) & nzchar(sel)]

  parts <- if (length(sel)) {
    sprintf("%d view%s: %s", length(sel),
            if (length(sel) == 1L) "" else "s", paste(sel, collapse = ", "))
  } else {
    "default views"
  }

  vs <- args$viz_settings
  if (is.list(vs) && length(vs)) {
    parts <- c(parts, paste0("custom settings for: ", paste(names(vs), collapse = ", ")))
  }

  tm <- as.character(args$timeline_mode %||% "")[1]
  if (nzchar(tm) && !identical(tm, "rday")) {
    parts <- c(parts, paste0("timeline=", tm))
  }

  # Which patient is on screen is the most consequential thing this block's
  # config decides, so report it, and report an id that is not in the cohort
  # as the no-op it will be: the block discards it and renders a placeholder.
  subj <- as.character(args$subject %||% "")[1]
  if (!is.na(subj) && nzchar(subj)) {
    known <- tryCatch(pp_subject_ids(data), error = function(e) character())
    if (length(known) && !subj %in% known) {
      parts <- c(parts, paste0(
        "patient=", subj, " -- NOT a USUBJID in adsl, so no patient will be ",
        "shown; pick one from unique(adsl$USUBJID)"
      ))
    } else {
      parts <- c(parts, paste0("patient=", subj))
    }
  }

  desc <- paste0("patient profile configured: ", paste(parts, collapse = "; "))

  # Flag IDs that are neither a known static viz nor a known findings group.
  # Findings groups are data-derived (and auto-generated adlb_* IDs are valid at
  # runtime), so an otherwise-unknown ID is most likely a typo or a raw
  # table/PARAMCD name the model should not have put in `selected`. The group
  # ids come from the group templates themselves -- a duplicated literal list
  # here once drifted from them.
  static_ids <- tryCatch(names(patient_profile_static_vizs()),
                         error = function(e) character())
  known_groups <- tryCatch(
    vapply(pp_findings_groups(), `[[`, character(1L), "id"),
    error = function(e) character()
  )
  bad <- setdiff(sel, c(static_ids, known_groups))
  # Auto-generated per-PARAMCD ids ("adlb_trig", "adlbc_alt", "advs_resp")
  # are valid at runtime.
  bad <- bad[!grepl("^(adlb[ch]?|advs)_", bad)]
  if (length(bad)) {
    desc <- paste0(
      desc, " -- possibly INVALID view ID(s): ", paste(bad, collapse = ", "),
      " (use only the documented viz IDs, not table names or PARAMCDs)"
    )
  }
  desc
}

#' Register the patient profile config_effect method on blockr.ai's generic.
#' @noRd
register_patient_profile_ai_effect <- function() {
  if (!requireNamespace("blockr.ai", quietly = TRUE)) {
    return(invisible(FALSE))
  }
  ns <- asNamespace("blockr.ai")
  if (!exists("config_effect", envir = ns, inherits = FALSE)) {
    return(invisible(FALSE))
  }
  registerS3method("config_effect", "patient_profile_block",
                   config_effect.patient_profile_block, envir = ns)
  invisible(TRUE)
}
