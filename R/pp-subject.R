# Patient (subject) selection helpers for the patient profile block.
#
# pp_subject_ids()      — USUBJIDs present in a dm's adsl, cheap, no reqs
# pp_subject_choices()  — ids + human labels ("01-701-1015 · Placebo · 63F")
# pp_validate_subject() — ctor-side normalization of the `subject` argument
# pp_resolve_subject()  — which USUBJID the profile should render, if any

#' USUBJIDs present in a dm
#'
#' Deliberately total: returns `character()` for anything that isn't a dm
#' carrying an `adsl` with a `USUBJID` column, rather than erroring or
#' `req()`-ing. The block's `expr` reactive calls this, and a `req()` there
#' would stall the block instead of falling back to pass-through.
#'
#' @param dm_obj Anything; only a `dm` yields ids.
#' @return Character vector of unique USUBJIDs, in data order.
#' @noRd
pp_subject_ids <- function(dm_obj) {
  if (!inherits(dm_obj, "dm")) return(character())
  tbls <- dm::dm_get_tables(dm_obj)
  if (!"adsl" %in% names(tbls)) return(character())
  adsl <- as.data.frame(tbls[["adsl"]])
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
#' @param dm_obj A `dm` object.
#' @return `list(ids, labels, meta)`, three character vectors of equal length.
#' @noRd
pp_subject_choices <- function(dm_obj) {
  ids <- pp_subject_ids(dm_obj)
  if (length(ids) == 0L) {
    return(list(ids = character(), labels = character(), meta = character()))
  }

  adsl <- as.data.frame(dm::dm_get_tables(dm_obj)[["adsl"]])
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
  arm <- col(c("ARM", "ACTARM", "TRT01P", "TRT01A")) %||% blank
  age <- col("AGE")
  sex <- col("SEX")
  demo <- if (!is.null(age) && !is.null(sex)) {
    trimws(paste0(age, sex))
  } else {
    age %||% sex %||% blank
  }

  meta <- vapply(seq_along(ids), function(i) {
    bits <- c(arm[[i]], demo[[i]])
    paste(bits[nzchar(bits)], collapse = " · ")
  }, character(1L))

  labels <- ifelse(nzchar(meta), paste0(ids, " · ", meta), ids)

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
