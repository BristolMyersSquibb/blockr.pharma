# Patient (subject) selection helpers for the patient profile block.
#
# pp_arm_column()       — which ADSL column holds the treatment/arm label
# pp_subject_ids()      — USUBJIDs present in a dm's adsl, cheap, no reqs
# pp_subject_choices()  — ids + human labels ("01-701-1015 · Placebo · 63F")
# pp_validate_subject() — ctor-side normalization of the `subject` argument
# pp_resolve_subject()  — which USUBJID the profile should render, if any

#' Resolve the ADSL column holding the treatment / arm label
#'
#' Every consumer of the arm label (the subject picker's meta line, the
#' overview's treatment lane) must agree on the column, or the same subject is
#' labelled two ways in one profile. `arm_var` is the study's declared choice
#' (the "study_roles" board option) and always wins; undeclared, the column is
#' `ACTARM` -- the *actual* arm, which is what a safety view wants.
#'
#' Either way, a column that the data does not carry is an error, never a
#' fallback. A study can carry several correct-looking arm columns at once
#' (planned, actual, abbreviated actual), so falling through to "whichever is
#' present" turns a loud failure into a quiet wrong answer. The old ADaM
#' chain (`ARM`, `ACTARM`, `TRT01P`, `TRT01A`) is gone for the same reason:
#' it tried the *planned* arm first.
#'
#' @param cols Column names of ADSL.
#' @param arm_var Study-declared arm column, or `NULL` when undeclared.
#' @return A single column name; errors (classed `pp_arm_var_missing` /
#'   `pp_arm_var_undeclared`) when the column is absent.
#' @noRd
pp_arm_column <- function(cols, arm_var = NULL) {
  if (!is.null(arm_var) && nzchar(arm_var)) {
    if (arm_var %in% cols) {
      return(arm_var)
    }
    stop(errorCondition(
      sprintf(
        paste0(
          "Declared arm column \"%s\" is not in ADSL. Fix the declaration ",
          "in the board sidebar (Study > Arm column); there is no fallback."
        ),
        arm_var
      ),
      class = "pp_arm_var_missing"
    ))
  }
  if ("ACTARM" %in% cols) {
    return("ACTARM")
  }
  stop(errorCondition(
    paste0(
      "ADSL carries no \"ACTARM\" and no arm column is declared. Set the ",
      "study's arm column in the board sidebar (Study > Arm column)."
    ),
    class = "pp_arm_var_undeclared"
  ))
}

#' Quoted role assertion for the block's eval path
#'
#' Returns `NULL` when every role resolves for `dm_obj`, otherwise a quoted
#' `stop()` call re-raising the first resolution error. The block's `expr`
#' reactive returns it so the failure surfaces through blockr.core's
#' condition capture as a named block error -- the loud, user-visible path.
#' Raising directly from `expr` (or from any reactive an observer consumes,
#' like the cohort labels) would instead escape as an unhandled observer
#' error and end the Shiny session, taking the sidebar the message points at
#' with it.
#'
#' Total like [pp_subject_ids()]: anything that is not a dm passes through.
#' The check runs on the block's RAW input, so tables are found through the
#' alias catalog -- an SDTM study ships `dm` and `ae`, not `adsl` and
#' `adae`.
#'
#' @param dm_obj Anything; only a `dm` is checked.
#' @param declared Declared roles (normalized "study_roles" value), or
#'   `NULL`.
#' @return `NULL`, or a quoted `stop()` call.
#' @noRd
pp_roles_blocker <- function(dm_obj, declared = NULL) {
  if (!inherits(dm_obj, "dm")) return(NULL)
  declared <- declared %||% list()
  tbls <- dm::dm_get_tables(dm_obj)

  find_tbl <- function(canonical) {
    hits <- c(canonical, pp_table_catalog()[[canonical]])
    hits <- hits[hits %in% names(tbls)]
    if (length(hits)) hits[[1L]] else NULL
  }

  err <- NULL
  check <- function(resolver) {
    if (!is.null(err)) return()
    err <<- tryCatch({resolver(); NULL}, error = function(e) e)
  }

  subj <- find_tbl("adsl")
  if (!is.null(subj)) {
    cols <- colnames(tbls[[subj]])
    check(function() pp_arm_column(cols, declared$arm))
    check(function() pp_timeline_column(cols, declared$timeline))
  }
  adae <- find_tbl("adae")
  if (!is.null(adae)) {
    check(function() pp_sev_column(colnames(tbls[[adae]]),
                                   declared$severity))
  }

  if (is.null(err)) return(NULL)

  bquote(
    stop(base::errorCondition(
      .(conditionMessage(err)),
      class = .(setdiff(class(err), c("error", "condition")))
    ))
  )
}

#' USUBJIDs present in a dm
#'
#' Deliberately total: returns `character()` for anything that isn't a dm
#' carrying a subject table with a `USUBJID` column, rather than erroring or
#' `req()`-ing. The block's `expr` reactive calls this, and a `req()` there
#' would stall the block instead of falling back to pass-through.
#'
#' The subject table is found via [pp_subject_tbl_name()] (`adsl`, or the
#' SDTM `dm` domain on a raw input), so a real SDTM study yields ids before
#' any normalization has run — checking the raw dm for a literal `adsl` is
#' what used to kill SDTM studies before the alias machinery ever saw them.
#'
#' @param dm_obj Anything; only a `dm` yields ids.
#' @return Character vector of unique USUBJIDs, in data order.
#' @noRd
pp_subject_ids <- function(dm_obj) {
  if (!inherits(dm_obj, "dm")) return(character())
  tbls <- dm::dm_get_tables(dm_obj)
  tbl_name <- pp_subject_tbl_name(names(tbls))
  if (is.null(tbl_name)) return(character())
  adsl <- as.data.frame(tbls[[tbl_name]])
  if (!"USUBJID" %in% colnames(adsl)) return(character())
  unique(as.character(adsl$USUBJID))
}

#' Subject ids paired with display labels and their parts
#'
#' Labels carry enough demography to tell two subject ids apart at a glance:
#' `"01-701-1015 · Placebo · 63F"`. Every component beyond the id is optional
#' and silently dropped when its column is absent, so a bare ADSL still yields
#' usable labels.
#'
#' `labels` is the flat form, used for the picker button and as the client-side
#' search haystack. `meta` is everything after the id (`"Placebo · 63F"`), kept
#' separate so a picker row can render the id as the value and the rest as a
#' muted secondary label, per the `.blockr-select__opt-label` primitive. It is
#' always the same length as `ids`, with `""` where no column was available.
#'
#' @param dm_obj A `dm` object (normalized; a raw SDTM dm also works via the
#'   subject-table lookup).
#' @param arm_col The arm role's RESOLVED column (see [pp_resolve_roles()]),
#'   or `NULL` when the arm did not resolve — labels then carry no arm. The
#'   resolution error itself is raised loudly on the block's eval path
#'   ([pp_roles_blocker()]); erroring here would end the session, since this
#'   runs inside reactives that observers consume.
#' @return `list(ids, labels, meta)`, three character vectors of equal length.
#' @noRd
pp_subject_choices <- function(dm_obj, arm_col = NULL) {
  ids <- pp_subject_ids(dm_obj)
  if (length(ids) == 0L) {
    return(list(ids = character(), labels = character(), meta = character()))
  }

  tbls <- dm::dm_get_tables(dm_obj)
  adsl <- as.data.frame(tbls[[pp_subject_tbl_name(names(tbls))]])
  # One ADSL row per subject by construction, but do not trust it: take the
  # first row per id so a duplicated ADSL cannot misalign labels against ids.
  first <- match(ids, as.character(adsl$USUBJID))

  col <- function(candidates) {
    hit <- candidates[candidates %in% colnames(adsl)]
    if (!length(hit)) return(NULL)
    out <- as.character(adsl[[hit[[1L]]]])[first]
    out[is.na(out)] <- ""
    out
  }

  blank <- rep("", length(ids))
  arm <- col(arm_col) %||% blank
  age <- col("AGE")
  sex <- col("SEX")
  demo <- if (!is.null(age) && !is.null(sex)) {
    trimws(paste0(age, sex))
  } else {
    age %||% sex %||% blank
  }

  meta <- vapply(seq_along(ids), function(i) {
    bits <- c(arm[[i]], demo[[i]])
    paste(bits[nzchar(bits)], collapse = " \u00b7 ")
  }, character(1L))

  labels <- ifelse(nzchar(meta), paste0(ids, " \u00b7 ", meta), ids)

  list(ids = ids, labels = labels, meta = meta)
}

#' Normalize the `subject` constructor argument
#'
#' Accepts `NULL`, `character(0)`, a length-1 character, or the `list()` /
#' `list("id")` shapes that come back through board (de)serialization. Empty
#' is a legitimate value (no patient chosen yet), so it normalizes to
#' `character(0)` rather than erroring.
#'
#' @param subject Raw constructor input.
#' @return `character(0)` or a length-1 character vector.
#' @noRd
pp_validate_subject <- function(subject) {
  if (is.null(subject)) return(character())
  subject <- as.character(unlist(subject, use.names = FALSE))
  subject <- subject[!is.na(subject) & nzchar(subject)]
  if (length(subject) > 1L) {
    stop("`subject` must be a single USUBJID, got ", length(subject),
         call. = FALSE)
  }
  subject
}

#' Quoted subject filter for the block's output expression
#'
#' Filters the subject table under its RAW name (`adsl`, or the SDTM `dm`
#' domain): the expression runs against the block's untouched input, where
#' downstream blocks want the keyed dm, not the profile's normalized flat
#' copy.
#'
#' A table literally named `dm` cannot be addressed through `dm_filter()`'s
#' named arguments -- they match its first FORMAL (also `dm`) before any
#' table -- so that case renames the subject table to its canonical `adsl`
#' first. `dm_rename_tbl()` keeps keys and FKs, so the cascade still scopes
#' every child table; downstream sees `adsl`, which is the name the rest of
#' the ecosystem declares against anyway.
#'
#' @param subj_tbl Subject table name on the raw dm (see
#'   [pp_subject_tbl_name()]).
#' @param subject A single USUBJID.
#' @return A quoted call over `data`.
#' @noRd
pp_subject_filter_expr <- function(subj_tbl, subject) {
  data_in <- if (identical(subj_tbl, "adsl")) {
    quote(data)
  } else {
    as.call(c(
      quote(dm::dm_rename_tbl),
      stats::setNames(list(quote(data), as.name(subj_tbl)), c("", "adsl"))
    ))
  }
  bquote(
    dm::dm_filter(.(data_in), adsl = USUBJID == .(subject)),
    list(data_in = data_in, subject = subject)
  )
}

#' Decide which subject the profile renders
#'
#' A dm carrying exactly one subject renders that subject regardless of
#' `subject`: an upstream drill-down has already committed to a patient and
#' the incoming dm is always the universe the picker selects within. With a
#' cohort, only an explicit `subject` that actually occurs in the cohort
#' renders. We never auto-pick: whose data this is must not be guessed.
#'
#' @param ids Character vector of cohort USUBJIDs.
#' @param subject `character(0)` or a length-1 character.
#' @return A length-1 character, or `NA_character_` when nothing renders.
#' @noRd
pp_resolve_subject <- function(ids, subject) {
  if (length(ids) == 1L) return(ids[[1L]])
  if (length(subject) == 1L && subject %in% ids) return(subject)
  NA_character_
}
