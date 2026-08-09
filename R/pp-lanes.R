# Lane granularity: which coding level a gantt draws one row per.
#
# AE and CM tables code the same event at several levels at once -- a
# reported verbatim term, a dictionary term, and one or more group levels
# above it (MedDRA SOC, ATC class). Which one belongs on the y axis is a
# reading decision, not a study fact: forty preferred-term lanes answer
# "what exactly happened", eight body-system lanes answer "what kind of
# thing happened", and the same patient needs both at different moments.
#
# So the level is a per-viz control (a radio in the chart header), and the
# choices are the levels THIS study carries -- a study shipping only
# AEDECOD gets no control at all, because there is no choice to make. That
# presence test is the same one the control UI runs, which is why it lives
# here and not in either gantt.
#
# The ladders below run granular -> coarse, which is the order the radio
# draws them in.

#' The lane ladder for adverse events
#'
#' Label -> column, most granular first. `AETERM` is the verbatim report,
#' `AEDECOD` the coded preferred term (the default, and the only rung the
#' viz requires), then the MedDRA group levels a study may or may not carry
#' on its ADAE. `AEBODSYS` is canonical here: [pp_normalize_dm()] has
#' already folded `AESOC` into it.
#' @noRd
PP_AE_LANES <- c(
  "Reported" = "AETERM",
  "Preferred term" = "AEDECOD",
  "High-level term" = "AEHLT",
  "Body system" = "AEBODSYS"
)

#' The lane ladder for concomitant medications
#'
#' `CMTRT` is the reported name (required by the viz), `CMDECOD` the coded
#' name (the default when present), `CMCLAS` the drug class.
#' @noRd
PP_CM_LANES <- c(
  "Reported" = "CMTRT",
  "Coded name" = "CMDECOD",
  "Drug class" = "CMCLAS"
)

#' Lane label for rows whose every ladder rung is blank
#' @noRd
PP_LANE_UNCODED <- "Not coded"

#' Does a column carry anything to group by?
#'
#' Presence in `colnames()` is not enough: a study that ships `AEBODSYS` as
#' an all-empty column would otherwise be offered a switch that collapses
#' every bar into one "Not coded" lane.
#' @noRd
pp_has_values <- function(x) {
  x <- as.character(x)
  any(!is.na(x) & nzchar(trimws(x)))
}

#' Columns of a viz's tables that carry values
#'
#' The control UI filters a radio's choices through this, so a level the
#' data does not carry is never offered. Reads the dm it is given, which on
#' the render path is the SUBJECT-scoped one: a level that exists study-wide
#' but is blank for the patient on screen would group nothing.
#'
#' @param dm_obj A normalized `dm`.
#' @param tables Character vector of table names (a viz's `tables`).
#' @return Character vector of column names, unique across the tables.
#' @noRd
pp_filled_columns <- function(dm_obj, tables) {
  tbls <- dm::dm_get_tables(dm_obj)
  out <- lapply(intersect(tables, names(tbls)), function(nm) {
    tbl <- as.data.frame(tbls[[nm]])
    Filter(function(col) pp_has_values(tbl[[col]]), colnames(tbl))
  })
  unique(unlist(out, use.names = FALSE)) %||% character()
}

#' Resolve which ladder rung a render should draw lanes from
#'
#' Total by construction: a `requested` level the study does not carry (a
#' board saved against a richer study, say) silently falls back rather than
#' erroring, because the control that produced it is itself conditional on
#' the data.
#'
#' @param tbl A data frame (the viz's already-scoped table).
#' @param ladder Named character vector of levels, granular first.
#' @param requested The persisted `settings$lanes`, or `NULL`.
#' @param default The rung to use when nothing is requested.
#' @return A single column name, or `NULL` when the table carries no rung.
#' @noRd
pp_lane_column <- function(tbl, ladder, requested = NULL, default = NULL) {
  present <- unname(ladder[vapply(
    ladder,
    function(col) col %in% colnames(tbl) && pp_has_values(tbl[[col]]),
    logical(1L)
  )])
  if (!length(present)) return(NULL)
  if (!is.null(requested) && requested %in% present) return(requested)
  if (!is.null(default) && default %in% present) return(default)
  present[1L]
}

#' Lane labels for every row of a table
#'
#' A partially coded table must not blank its lanes: a row with no value at
#' the chosen level falls back to the viz's base level (the one it requires,
#' so it is always there), exactly as the CM gantt's coded-name lanes have
#' always fallen back to the reported name.
#'
#' @param tbl A data frame.
#' @param col The chosen lane column.
#' @param fallback The viz's required base column.
#' @return Character vector, one label per row.
#' @noRd
pp_lane_values <- function(tbl, col, fallback) {
  val <- as.character(tbl[[col]])
  blank <- is.na(val) | !nzchar(trimws(val))
  if (any(blank) && !identical(col, fallback) &&
        fallback %in% colnames(tbl)) {
    fb <- as.character(tbl[[fallback]])
    val[blank] <- fb[blank]
    blank <- is.na(val) | !nzchar(trimws(val))
  }
  val[blank] <- PP_LANE_UNCODED
  val
}

#' Drop the choices this study does not carry
#'
#' For a control declaring `choices_present`, the choice VALUES are column
#' names, so a level the data does not carry is not a choice. Shared by the
#' toolbar's radio and pill branches, which both then draw nothing at all
#' when fewer than two survive.
#'
#' @param choices The control's declared choices.
#' @param ctrl The control declaration.
#' @param dm_obj A normalized `dm`.
#' @param tables The viz's declared tables.
#' @return `choices`, filtered.
#' @noRd
pp_ctrl_present_choices <- function(choices, ctrl, dm_obj, tables) {
  if (!isTRUE(ctrl$choices_present)) return(choices)
  choices[choices %in% pp_filled_columns(dm_obj, tables)]
}

#' The "Lanes" pill control for a gantt viz
#'
#' Declared identically by both gantts; the control UI drops the choices the
#' study does not carry and the whole control when fewer than two survive.
#'
#' A click-through pill, the house component for cycling a value in place
#' (blockr.docs design-system/components/blockr-row.md, "Pills"), rather than
#' a row of radios: four rungs of prose (`Reported | Preferred term |
#' High-level term | Body system`) crowd a panel header that also carries the
#' title, the severity legend and the panel actions, in a column the profile
#' does not get to widen. The pill obeys the label rule the same doc sets --
#' it names the current setting ("Body system"), never the mechanism -- and
#' the group label names the dimension.
#'
#' The cost is that the other rungs are invisible until you walk them, and
#' that walking is one-directional. That is the right trade for a control
#' most sessions never touch.
#'
#' @param ladder Named character vector of levels, granular first.
#' @param default The rung selected until the user picks another.
#' @return A `controls` list for [new_pp_viz()].
#' @noRd
pp_lane_control <- function(ladder, default) {
  list(lanes = list(
    type = "pill",
    label = "Lanes",
    default = default,
    choices = ladder,
    # Choice values are column names: offer only the levels the data has.
    choices_present = TRUE
  ))
}
