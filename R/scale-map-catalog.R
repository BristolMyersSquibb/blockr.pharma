# Default clinical scale-map catalog + the adapter helper for visit-type
# derivation. Content distilled from the BMS cdex.teal Settings classes (see
# blockr.design/open/cdex-attribute-map/teal-attribute-map-reference.md):
# the AE palettes are the de-facto org defaults (identical across study
# specs), the BOR palette is the CA_001_050 one (teal's base class ships
# none; studies override per study).

#' Default clinical scale map
#'
#' A starting [new_scale_map()] for clinical (ADaM) boards: fixed palettes
#' for best overall response and AE grade/severity, visit-type shapes, and
#' bare registrations (auto-assigned, but consistent across views) for the
#' usual stratifiers. Studies amend it by listing replacement bindings after
#' it in [new_scale_map()].
#'
#' @return A `scale_map`.
#'
#' @examples
#' study_map <- new_scale_map(
#'   default_clinical_map(),
#'   scale_binding("BEST_OVERALL_RESPONSE", # study palette replaces default
#'     color = c(CR = "#008000", PR = "#98FBCB", SD = "#FFED29",
#'               PD = "#FF2C2C", NE = "#eae0d5"))
#' )
#'
#' @export
default_clinical_map <- function() {
  new_scale_map(
    scale_binding(
      "BEST_OVERALL_RESPONSE",
      color = c(
        CR = "#006400", PR = "#FFD700", SD = "#FFA500", PD = "#8b0000",
        NE = "#6D8196", NR = "#595959", UN = "#858585"
      )
    ),
    scale_binding(
      "AETOXGR",
      color = c(
        "1" = "#43978D", "2" = "#264D59", "3" = "#c49102",
        "4" = "#D46C4E", "5" = "#FF0000"
      )
    ),
    scale_binding(
      "AESEV",
      color = c(MILD = "#CA8A04", MODERATE = "#D97706", SEVERE = "#DC2626")
    ),
    scale_binding(
      "VISIT_TYPE",
      color = c(BASELINE = "#bdbdbd"),
      shape = c(BASELINE = 19L, SCHEDULED = 19L, UNSCHEDULED = 19L, EOT = 18L)
    ),
    scale_binding("TRT"),
    scale_binding("SEX"),
    scale_binding("RACE"),
    scale_binding("ETHNIC"),
    scale_binding("COUNTRY"),
    scale_binding("REGION"),
    scale_binding("DSDIAG")
  )
}

#' Derive the visit type from visit labels
#'
#' Visit labels vary by study ("END OF TREATMENT (WEEK 12)", "EOT", ...), so
#' aesthetics cannot bind to them directly. This helper normalizes them into
#' the closed vocabulary the `VISIT_TYPE` binding of
#' [default_clinical_map()] addresses — call it in the study's adapter block
#' (`adsl$VISIT_TYPE <- derive_visit_type(adsl$AVISIT)`). The regexes are the
#' ones cdex.teal applied at aesthetic time.
#'
#' @param visit Character (or factor) vector of visit labels (AVISIT/VISIT)
#'
#' @return A factor with levels BASELINE, SCHEDULED, UNSCHEDULED, EOT.
#'
#' @export
derive_visit_type <- function(visit) {
  v <- toupper(as.character(visit))

  type <- rep("SCHEDULED", length(v))
  type[grepl("UNSCH", v)] <- "UNSCHEDULED"
  type[grepl("END OF TREAT|EOT", v)] <- "EOT"
  type[grepl("BASELINE", v)] <- "BASELINE"
  type[is.na(v)] <- NA_character_

  factor(type, levels = c("BASELINE", "SCHEDULED", "UNSCHEDULED", "EOT"))
}
