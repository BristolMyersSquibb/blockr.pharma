# Make the patient profile block legible to the blockr.ai assistant. Its result
# is a passthrough dm (the profile renders the single-patient dm but does not
# transform it), so blockr.ai's data effect is blind and reads as
# "N tables, UNCHANGED" -- which looks like the config did nothing even though it
# changed WHICH visualizations are shown. The meaningful artifact is the CONFIG
# (`selected` views + `viz_settings`), so this `config_effect()` method describes
# that instead, the same way the drilldown blocks do in blockr.bi.
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

  desc <- paste0("patient profile configured: ", paste(parts, collapse = "; "))

  # Flag IDs that are neither a known static viz nor a known findings group.
  # Findings groups are data-derived (and auto-generated adlb_* IDs are valid at
  # runtime), so an otherwise-unknown ID is most likely a typo or a raw
  # table/PARAMCD name the model should not have put in `selected`.
  static_ids <- tryCatch(names(patient_profile_static_vizs()),
                         error = function(e) character())
  known_groups <- c("liver_panel", "renal_panel", "electrolytes", "metabolic",
                    "muscle_enzymes", "cbc", "rbc_indices", "wbc_differential",
                    "rbc_morphology", "blood_pressure", "pulse", "temperature",
                    "anthropometrics")
  bad <- setdiff(sel, c(static_ids, known_groups))
  bad <- bad[!grepl("^adlb_", bad)]
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
