# Role resolution: which column plays which clinical part, decided ONCE per
# incoming dm and handed to every consumer, instead of each consumer
# re-detecting (and potentially disagreeing) on its own.
#
# A role exists only where the profile cannot ask the user: a composed view
# has no picker per visual, so it must be told. The study declares roles
# through the "study_roles" board option (see study-roles-option.R); every
# role falls back to the package convention when undeclared, and a declared
# column the data does not carry is a named error, never a fallback:
#
#   arm       -- ADSL treatment/arm label. Convention: ACTARM, or an error
#                (the data can carry several correct-looking answers at
#                once, so even the undeclared case never guesses beyond the
#                actual arm). See pp_arm_column().
#   severity  -- ADAE severity column. Convention: detect AETOXGR over
#                AESEV (distinguishable by their values); none is
#                legitimate (uncolored bars). See pp_sev_column().
#   timeline  -- ADSL column anchoring relative-day mode. Convention:
#                TRTSDT (which pp_normalize_dm() derives from
#                RFXSTDTC/RFSTDTC for SDTM-shaped studies).
#
# Table aliases are the option's fourth field but are not resolved here:
# they feed pp_normalize_dm() directly (a table name is not a per-render
# question).
#
# Vizs consume roles declaratively: new_pp_viz(uses = c("severity", ...))
# makes the block inject `settings$roles` (and, for the severity role, the
# board scale map's resolved `settings$sev_colors`) into that viz's render.
# No viz-id string matching anywhere.

#' Resolve the profile's roles against a normalized dm
#'
#' Total on purpose: reactives that observers consume call this, where a
#' raised condition would end the Shiny session. A role that does not
#' resolve lands in `$errors[[role]]`; the block's eval path re-raises the
#' first one loudly through [pp_roles_blocker()], and UI consumers degrade
#' (id-only picker labels, unlabelled treatment lane, uncolored bars) while
#' the named error is on screen.
#'
#' @param dm_obj A normalized `dm` object (or anything; non-dms yield empty
#'   roles).
#' @param declared Declared roles (the normalized "study_roles" option
#'   value, `NULL` entries for undeclared), or `NULL`.
#' @return List with `arm`, `severity`, `timeline` (column names or `NULL`)
#'   and `errors` (named list of conditions, empty when all resolve).
#' @noRd
pp_resolve_roles <- function(dm_obj, declared = NULL) {
  roles <- list(arm = NULL, severity = NULL, timeline = NULL,
                errors = list())
  if (!inherits(dm_obj, "dm")) return(roles)

  acc <- new.env(parent = emptyenv())
  acc$roles <- roles
  declared <- declared %||% list()
  tbls <- dm::dm_get_tables(dm_obj)

  take <- function(role, resolver) {
    res <- tryCatch(resolver(), error = function(e) e)
    if (inherits(res, "condition")) {
      acc$roles$errors[[role]] <- res
    } else {
      acc$roles[[role]] <- res
    }
  }

  if ("adsl" %in% names(tbls)) {
    adsl_cols <- colnames(tbls[["adsl"]])
    take("arm", function() pp_arm_column(adsl_cols, declared$arm))
    take("timeline", function() {
      pp_timeline_column(adsl_cols, declared$timeline)
    })
  }

  if ("adae" %in% names(tbls)) {
    take("severity", function() {
      pp_sev_column(colnames(tbls[["adae"]]), declared$severity)
    })
  }

  acc$roles
}

#' Resolve the ADSL column anchoring relative-day mode
#'
#' `timeline_var` is the study's declared choice and always wins; a declared
#' column the data does not carry is an error, never a fallback. Undeclared,
#' the anchor is `TRTSDT` -- absent, relative-day mode is simply unavailable
#' (`NULL`), which is legitimate (the timeline stays on calendar dates).
#'
#' @param cols Column names of ADSL.
#' @param timeline_var Study-declared column, or `NULL` when undeclared.
#' @return A single column name, or `NULL`; errors (classed
#'   `pp_timeline_var_missing`) when a declared column is absent.
#' @noRd
pp_timeline_column <- function(cols, timeline_var = NULL) {
  if (!is.null(timeline_var) && nzchar(timeline_var)) {
    if (timeline_var %in% cols) {
      return(timeline_var)
    }
    stop(errorCondition(
      sprintf(
        paste0(
          "Declared timeline reference \"%s\" is not in ADSL. Fix the ",
          "declaration in the board sidebar (Study > Timeline reference); ",
          "there is no fallback."
        ),
        timeline_var
      ),
      class = "pp_timeline_var_missing"
    ))
  }
  if ("TRTSDT" %in% cols) "TRTSDT" else NULL
}
