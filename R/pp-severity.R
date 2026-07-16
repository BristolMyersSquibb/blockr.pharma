# AE severity helpers shared by the severity-colored patient-profile vizs.
#
# Studies encode AE severity two ways: AETOXGR (CTCAE grade, "1".."5") or
# AESEV (MILD/MODERATE/SEVERE). Which column a study carries is study
# metadata, exactly like the arm column (see pp_arm_column()); until a study
# can declare it, the profile detects it — ONCE, as the "severity" role in
# pp_resolve_roles(), never per consumer. Detection prefers the grade: when
# both exist, AETOXGR is what the CDEX drilldown views aggregate and color
# by, so the profile must agree with them. (The two vocabularies are not
# interconvertible; canonicalising them is a clinical decision and out of
# scope — see the study-metadata spec.)
#
# pp_sev_column()         — which adae column codes severity, or NULL
# pp_sev_fallback_color() — built-in level -> color constants
# pp_sev_label()          — display form of a level ("Grade 3", "Severe")
# pp_sev_scale_colors()   — resolve the board scale map for the role's column

#' Resolve the ADAE column holding the AE severity
#'
#' Every severity consumer (the gantt bars, the overview AE lane, the panel
#' legend, the scale-map resolution) must agree on the column, or bars and
#' legend drift apart within one profile. `sev_var` is the study's declared
#' choice (the "study_roles" board option) and always wins; a declared
#' column the data does not carry is an error, never a fallback. Undeclared,
#' detection prefers the grade (see the header comment); a study with
#' neither column simply has no severity (`NULL`), which is legitimate --
#' bars draw uncolored.
#'
#' @param cols Column names of ADAE.
#' @param sev_var Study-declared severity column, or `NULL` when undeclared.
#' @return A single column name, or `NULL`; errors (classed
#'   `pp_sev_var_missing`) when a declared column is absent.
#' @noRd
pp_sev_column <- function(cols, sev_var = NULL) {
  if (!is.null(sev_var) && nzchar(sev_var)) {
    if (sev_var %in% cols) {
      return(sev_var)
    }
    stop(errorCondition(
      sprintf(
        paste0(
          "Declared severity column \"%s\" is not in ADAE. Fix the ",
          "declaration in the board sidebar (Study > Severity column); ",
          "there is no fallback."
        ),
        sev_var
      ),
      class = "pp_sev_var_missing"
    ))
  }
  hit <- intersect(c("AETOXGR", "AESEV"), cols)
  if (length(hit)) hit[[1L]] else NULL
}

# Built-in constants, used when no board scale map resolves. The word
# palette is the one the gantt has always used; the grade palette matches
# the CDEX template scale map (blockr.cdex::cedx_scale_map()), so the
# profile and the drilldown views agree even on a board without a map.
pp_sev_colors <- c(
  MILD = "#CA8A04", MODERATE = "#D97706", SEVERE = "#DC2626",
  "1" = "#43978D", "2" = "#264D59", "3" = "#c49102",
  "4" = "#D46C4E", "5" = "#FF0000"
)

#' Built-in color for a severity level
#' @param sev A single severity value (word or grade).
#' @return A hex color; grey for anything unknown.
#' @noRd
pp_sev_fallback_color <- function(sev) {
  s <- toupper(trimws(as.character(sev)))
  if (s %in% names(pp_sev_colors)) {
    unname(pp_sev_colors[[s]])
  } else {
    "#9ca3af"
  }
}

#' Display form of a severity level
#'
#' A bare grade reads as noise in a legend ("3"); words go through
#' [pp_term_label()] like the rest of the panel text.
#' @param sev A single severity value.
#' @return A display string.
#' @noRd
pp_sev_label <- function(sev) {
  s <- trimws(as.character(sev))
  if (grepl("^[0-9]+$", s)) paste("Grade", s) else pp_term_label(s)
}

#' Resolve AE severity colors from the board scale map
#'
#' Returns the resolved color vector for the severity levels present in the
#' patient's adae, using the map binding named after the severity role's
#' column (AETOXGR or AESEV; resolved once in pp_resolve_roles() and passed
#' in). `NULL` when there is no map, no severity column, or no binding for
#' that column — the vizs then use the built-in constants.
#' @noRd
pp_sev_scale_colors <- function(map, dm_obj, sev_col = NULL) {
  if (is.null(map) || is.null(sev_col)) {
    return(NULL)
  }

  adae <- tryCatch(
    dm::dm_get_tables(dm_obj)[["adae"]],
    error = function(e) NULL
  )
  if (is.null(adae) || !sev_col %in% colnames(adae)) {
    return(NULL)
  }

  sev <- unique(as.character(adae[[sev_col]]))
  sev <- sev[!is.na(sev) & nzchar(sev)]

  if (!length(sev)) {
    return(NULL)
  }

  blockr.theme::resolve_scales(map, sev_col, levels = sev)$color
}
