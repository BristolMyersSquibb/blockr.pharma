# Concomitant-medication indication helpers: the "why was this given" axis of
# the CM gantt, colored the way severity colors the AE gantt.
#
# The indication lives in CMINDC, an SDTM/ADaM name that survives
# pp_normalize_dm() untouched, and it is a role rather than a detected column
# for the same reason severity is: the CM gantt has nobody to ask, and the
# resolved name is load-bearing downstream -- the board scale map is looked up
# BY it, so a canonical rename would silently repoint the lookup at a binding
# the board does not carry (see pp-severity.R).
#
# What this file does NOT do, deliberately: know any indication values, or any
# colors. CMINDC has no controlled terminology -- sponsors word the same
# concepts their own way ("PROPHYLAXIS", "PROPHYLAXIS OR NON-THERAPEUTIC
# USE"), and plenty of studies put the verbatim condition being treated here
# instead -- so a vocabulary shipped in this package would be one sponsor's
# spellings masquerading as a standard. Levels are handed to blockr.theme and
# it answers with colors: the board's scale-map binding when it has one
# (that's the editable, board-level, cross-view answer), the theme's
# categorical palette otherwise. Unlike severity, there are no built-in
# constants to fall back to, because there is no closed level set to key them
# by.
#
# Colors only. The level keeps its own text everywhere (bars, tooltip,
# legend); rewriting a study's indications into a canonical vocabulary would
# be a clinical decision, out of scope for a render (the same line
# pp-severity.R draws between the two severity vocabularies).
#
# pp_indc_column()        — which adcm column codes the indication, or NULL
# pp_indc_scale_colors()  — level -> color for the patient, via blockr.theme
# pp_indc_legend_ui()     — the panel-header swatches

#' Resolve the ADCM column holding the medication indication
#'
#' `indc_var` is the study's declared choice (the "study_roles" board option)
#' and always wins; a declared column the data does not carry is an error,
#' never a fallback. Undeclared, the convention is `CMINDC` -- absent, the
#' medications simply have no indication (`NULL`), which is legitimate: the
#' gantt draws its bars in one color, as it always did.
#'
#' @param cols Column names of ADCM.
#' @param indc_var Study-declared indication column, or `NULL` when
#'   undeclared.
#' @return A single column name, or `NULL`; errors (classed
#'   `pp_indc_var_missing`) when a declared column is absent.
#' @noRd
pp_indc_column <- function(cols, indc_var = NULL) {
  if (!is.null(indc_var) && nzchar(indc_var)) {
    if (indc_var %in% cols) {
      return(indc_var)
    }
    stop(errorCondition(
      sprintf(
        paste0(
          "Declared indication column \"%s\" is not in ADCM. Fix the ",
          "declaration in the board sidebar (Study > Indication column); ",
          "there is no fallback."
        ),
        indc_var
      ),
      class = "pp_indc_var_missing"
    ))
  }
  if ("CMINDC" %in% cols) "CMINDC" else NULL
}

#' Colors for this patient's medication indications
#'
#' Asks blockr.theme to color the indication levels the patient's adcm
#' actually carries: the board scale map's binding for the indication column
#' first (fixed level -> color pairs, editable in the settings sidebar and
#' shared with every other view of the same variable), then the theme's
#' categorical palette for anything the binding leaves open. The palette
#' assignment is blockr.theme's own, keyed by the level rather than by its
#' position, so two patients carrying different subsets of indications still
#' agree on what each color means.
#'
#' `NULL` -- the caller then draws its bars in one color, as before -- when
#' there is no indication column, or when the levels this patient carries are
#' not something color can tell apart:
#'
#' * fewer than two of them. One indication colors nothing; it only turns the
#'   rows that left CMINDC blank grey, which reads as a finding and is not
#'   one.
#' * more of them than the palette holds. A study putting verbatim conditions
#'   in CMINDC has about as many levels as medications: the pool would repeat
#'   hues, and a legend of forty swatches says less than no legend at all.
#'
#' @param map The board scale map (`NULL` when the board carries none).
#' @param dm_obj Subject-scoped, normalized dm.
#' @param indc_col The indication role's resolved column, or `NULL`.
#' @return A named character vector of hex colors keyed by level, in the
#'   order blockr.theme resolved them (bound levels first), or `NULL`.
#' @noRd
pp_indc_scale_colors <- function(map, dm_obj, indc_col = NULL) {
  if (is.null(indc_col)) {
    return(NULL)
  }

  adcm <- tryCatch(
    dm::dm_get_tables(dm_obj)[["adcm"]],
    error = function(e) NULL
  )
  if (is.null(adcm) || !indc_col %in% colnames(adcm)) {
    return(NULL)
  }

  # Keep the column, not just its levels: resolve_scales_col() reads factor
  # levels and follows `blockr_source` provenance off it, so a copied or
  # renamed indication column inherits its source column's binding.
  column <- adcm[[indc_col]]
  keep <- !is.na(column) & nzchar(trimws(as.character(column)))
  column <- column[keep]
  if (!length(column)) {
    return(NULL)
  }

  pool <- blockr.theme::theme_palette("categorical")
  n_levels <- length(unique(as.character(column)))
  if (n_levels < 2L || n_levels > length(pool)) {
    return(NULL)
  }

  # The board's binding for this column, with the theme palette filling
  # whatever it leaves open ...
  res <- blockr.theme::resolve_scales_col(map, indc_col, column,
                                          palette = pool)
  # ... and, for a board with no binding at all, the palette on its own. An
  # unnamed color pool IS the "color these levels for me" request; letting
  # blockr.theme answer it keeps the level -> color assignment (and its
  # stability) in one place instead of two.
  res <- res %||% blockr.theme::resolve_scales_col(
    blockr.theme::new_scale_map(
      blockr.theme::scale_binding(indc_col, color = pool)
    ),
    indc_col, column
  )

  cols <- res$color
  if (is.null(cols) || !length(cols)) {
    return(NULL)
  }
  order <- intersect(res$order %||% names(cols), names(cols))
  cols[order]
}

#' Indication legend for the CM panel header
#'
#' One swatch per indication level present for this patient, in the resolved
#' order (levels the board pinned lead), from the very colors the bars use --
#' the vector is resolved once, in the block, and both consumers read it, so
#' legend and bars cannot drift apart. `NULL` when the bars are uniformly
#' colored, where a legend would claim a distinction the plot does not draw.
#'
#' @param indc_colors Resolved level -> color vector, or `NULL`.
#' @return A `shiny::div`, or `NULL`.
#' @noRd
pp_indc_legend_ui <- function(indc_colors = NULL) {
  if (is.null(indc_colors) || !length(indc_colors)) {
    return(NULL)
  }

  items <- lapply(names(indc_colors), function(lv) {
    shiny::span(
      class = "pp-legend-item",
      shiny::span(
        class = "pp-legend-swatch",
        style = paste0("background:", unname(indc_colors[[lv]]), ";")
      ),
      pp_term_label(lv)
    )
  })

  shiny::div(class = "pp-chart-legend", items)
}
