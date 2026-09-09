# Reading a findings table for display
#
# One seam for adlb/adlbc/adlbh, advs, adqs* and adeg when it gets a consumer.
#
# It does NOT filter. A study's derived records -- LOCF, replicate averages,
# per-patient extremes -- are there because a statistician followed a
# pre-specified rule, and a sponsor who prepared the ADaM knows their data
# better than this package does. Deciding which of those rows deserve to
# reach the screen is not a rendering decision.
#
# What it does is make the read total: a value the renderer will do
# arithmetic on comes back numeric, or NA, never a crash and never a level
# code.

#' Coerce a findings column to numeric without inventing values
#'
#' `pp_column_catalog()` copies `AVAL`, `A1LO`, `A1HI` and the `*DY` columns
#' through as-is, which is correct against CDISC -- those source variables are
#' Num per the SDTM IG. Real deliveries are not always conformant: a CSV or
#' SAS round-trip stringifies numerics, and `round()` on a character column
#' aborts the whole card.
#'
#' Goes through `as.character()` first on purpose. `as.numeric()` on a factor
#' returns level codes, which would replace a crash with a silently wrong
#' chart -- strictly the worse failure.
#'
#' @param x A vector that should be numeric.
#' @return A numeric vector; unparseable entries become `NA`, never 0.
#' @noRd
pp_as_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(as.numeric(as.character(x)))
}

#' Read a findings table out of a dm, ready to draw
#'
#' @param dm_obj A normalized dm.
#' @param table_name Table to read.
#' @param num_cols Columns the renderer does arithmetic on.
#' @return A data.frame, or `NULL` if the table is absent.
#' @noRd
pp_prepare_findings <- function(dm_obj, table_name,
                                num_cols = c("AVAL", "CHG", "PCHG", "BASE",
                                             "A1LO", "A1HI")) {
  tbls <- dm::dm_get_tables(dm_obj)
  if (!table_name %in% names(tbls)) return(NULL)

  tbl <- as.data.frame(tbls[[table_name]])
  for (nm in intersect(num_cols, colnames(tbl))) {
    tbl[[nm]] <- pp_as_numeric(tbl[[nm]])
  }
  tbl
}

# ---------------------------------------------------------------------------
# Which analysis value a findings card draws
# ---------------------------------------------------------------------------

#' The value variables a findings card can draw
#'
#' ADaM BDS ships several analysis values for one parameter, and the cohort
#' views already offer all three: the CEDX board marks `AVAL`, `CHG` and
#' `PCHG` as kind `value` (`transforms/column-kinds.R`), so every chart with
#' an exposed mapping band lists them. A card draws one at a time, and this
#' is the ladder its header pill steps through, ordered as a reader reads it:
#' what was measured, then what it moved by.
#'
#' `BASE` is deliberately not a rung. It is constant per subject and
#' parameter, so as a series it is a flat line saying one number; it is what
#' the others are measured AGAINST, which is why it is printed in the tooltip
#' whenever a change is on screen rather than offered as a series of its own.
#'
#' Names are the pill's labels, values the columns. The first rung is
#' "Absolute" rather than "Value": the control group is already labelled
#' VALUE, and a pill reading "VALUE  Value" says one word twice where the
#' gantt's "LANES  Preferred term" says a dimension and its current setting.
#' It also avoids claiming AVAL was measured, which a carried-forward record
#' was not.
#' @noRd
PP_FINDINGS_VALUES <- c(
  "Absolute" = "AVAL",
  "Change" = "CHG",
  "% change" = "PCHG"
)

#' The pill a findings card carries for its value variable
#'
#' `choices_present = TRUE`, so a study shipping no change columns is offered
#' no switch at all rather than a pill that draws an empty chart on its
#' second rung (see [pp_ctrl_present_choices()], and [pp_lane_control()] for
#' the same declaration on the gantts).
#' @noRd
pp_value_control <- function() {
  list(value = list(
    type = "pill",
    # No dimension label. "VALUE" stood in front of the pill on every
    # findings card, and a profile is a stack of them: the same five
    # characters repeated down a 600px rail, in front of a control whose own
    # text ("Absolute", "% change") already says which value it is showing,
    # under a header that already says which parameter. The tooltip carries
    # the action ("Switch to Change"), as it does on the gantts' pill.
    label = NULL,
    default = "AVAL",
    choices = PP_FINDINGS_VALUES,
    choices_present = TRUE
  ))
}

#' Resolve which analysis value a render should draw
#'
#' Total by construction, the same contract as [pp_lane_column()]: a
#' `requested` value the table does not carry falls back to `AVAL` rather
#' than erroring, because the control that produced it is itself conditional
#' on the data and a board saved against a richer study can name a column
#' this one lacks.
#'
#' Presence is judged on the COLUMN, not on whether it has values for the
#' parameter on screen. The pill's own choices come from
#' [pp_filled_columns()], which reads the whole table: a parameter with no
#' change records inside a table that has them must draw an honest "no
#' records" rather than silently swapping back to AVAL while the pill still
#' reads "% change".
#'
#' @param tbl A findings table (already read through [pp_prepare_findings()]).
#' @param requested The persisted `settings$value`, or `NULL`.
#' @param default The column to fall back to.
#' @return A single column name.
#' @noRd
pp_findings_value_column <- function(tbl, requested = NULL, default = "AVAL") {
  if (is.null(requested) || !length(requested)) return(default)
  requested <- as.character(requested)[[1L]]
  if (!requested %in% PP_FINDINGS_VALUES) return(default)
  if (!requested %in% colnames(tbl)) return(default)
  requested
}

#' The pill label for a value column ("% change" for `PCHG`)
#'
#' Falls back to the column name, so a value reached from board code rather
#' than from the pill still names itself.
#' @noRd
pp_findings_value_label <- function(value) {
  hit <- match(value, PP_FINDINGS_VALUES)
  if (is.na(hit)) return(value)
  names(PP_FINDINGS_VALUES)[[hit]]
}
