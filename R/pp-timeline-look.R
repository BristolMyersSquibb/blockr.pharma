# What the patient profile draws the way the patient timeline draws it, so
# one patient reads the same in both blocks: the AE, con-med and treatment
# marks of new_patient_timeline_block(), which blockr.viz's swimlane column
# draws, redrawn here in echarts on the profile's shared time axis.

#' Con-med colours: started before or on treatment
#'
#' The timeline's default con-med colours (tl_cm_colour(), its palette in
#' `patient-timeline-events.R`). A con med starting on or after day 1 started
#' on treatment.
#' @noRd
PP_CM_STARTED <- c(
  "Started before treatment" = "#9db8de",
  "Started on treatment" = "#3f6fb0"
)

#' Mark sizes in px
#'
#' AE and con-med bars are 12px, as on an opened timeline row, where only
#' the treatment is thinner: a drug is a 4px line in its colour (the
#' timeline's drug lines, coloured by pt_drug_palette()).
#' @noRd
PP_CM_BAR_H <- 12
PP_DRUG_LINE_H <- 4

#' Whether each con med started on treatment
#'
#' On or after day 1: the study day where the record carries one, else the
#' start date against the patient's first dose (ADSL `TRTSDT`). Unknown
#' reads as started on treatment, the darker colour.
#'
#' @param cm ADCM rows.
#' @param trtsdt The patient's first dose date, or `NA`.
#' @return A logical vector, one per row.
#' @noRd
pp_cm_started_on <- function(cm, trtsdt = NA) {
  n <- nrow(cm)
  day <- if ("ASTDY" %in% colnames(cm)) {
    suppressWarnings(as.numeric(cm$ASTDY))
  } else {
    rep(NA_real_, n)
  }
  on <- day >= 1
  if ("ASTDT" %in% colnames(cm) && length(trtsdt) && !is.na(trtsdt[[1L]])) {
    by_date <- as.Date(cm$ASTDT) >= as.Date(trtsdt[[1L]])
    on[is.na(on)] <- by_date[is.na(on)]
  }
  on[is.na(on)] <- TRUE
  on
}

#' The colour of each con med: started before or on treatment
#' @noRd
pp_cm_started_colour <- function(on) {
  unname(PP_CM_STARTED[ifelse(on, 2L, 1L)])
}

#' Con-med legend: the two colours this patient's records carry
#'
#' @param on [pp_cm_started_on()] for the patient's records.
#' @return A `shiny::div`, or `NULL` without records.
#' @noRd
pp_cm_started_legend_ui <- function(on) {
  if (!length(on)) return(NULL)
  lv <- names(PP_CM_STARTED)[sort(unique(ifelse(on, 2L, 1L)))]
  shiny::div(
    class = "pp-chart-legend",
    lapply(lv, function(l) {
      shiny::span(
        class = "pp-legend-item",
        shiny::span(class = "pp-legend-swatch",
                    style = paste0("background:", PP_CM_STARTED[[l]], ";")),
        l
      )
    })
  )
}

#' A con med's name, coded where the dictionary coded it
#'
#' The coded name, else the reported one: a dictionary's "UNCODED" is not a
#' name, and lumping every uncoded record into one lane hides them (the
#' timeline's rule, tl_cm()).
#'
#' @param tbl ADCM rows.
#' @param col The column read, `CMDECOD` or a coarser level.
#' @return A character vector, one per row.
#' @noRd
pp_cm_lane_values <- function(tbl, col) {
  val <- pp_lane_values(tbl, col, "CMTRT")
  unc <- toupper(trimws(val)) == "UNCODED"
  if (any(unc) && identical(col, "CMDECOD") && "CMTRT" %in% colnames(tbl)) {
    rep_name <- as.character(tbl$CMTRT)
    ok <- unc & !is.na(rep_name) & nzchar(trimws(rep_name))
    val[ok] <- rep_name[ok]
    unc <- unc & !ok
  }
  val[unc] <- PP_LANE_UNCODED
  val
}

#' Lanes in order of their first onset
#'
#' The timeline lists an opened patient's terms and con meds by first onset,
#' then by name.
#'
#' @param lane The lane of each record.
#' @param start Its start, in axis units.
#' @return The lanes, in order.
#' @noRd
pp_lanes_by_onset <- function(lane, start) {
  first <- tapply(as.numeric(start), lane, min, na.rm = TRUE)
  first[!is.finite(first)] <- Inf
  names(first)[order(first, names(first))]
}

#' The patient's first dose date, from ADSL
#' @param tbls The dm's tables.
#' @noRd
pp_cm_trtsdt <- function(tbls) {
  sl <- tbls[["adsl"]]
  if (is.null(sl) || !nrow(sl) || !"TRTSDT" %in% colnames(sl)) return(NA)
  sl$TRTSDT[[1L]]
}

#' A con-med lane's label
#'
#' A medication's name as the patient timeline writes it
#' (swimmer_drug_name(): "Con Med d" for "CON MED D"); a class lane in
#' sentence case.
#'
#' @param x Lane values.
#' @param lane_col The column the lanes come from, or `NULL`.
#' @noRd
pp_cm_lane_label <- function(x, lane_col = NULL) {
  if (identical(lane_col, "CMCLAS")) pp_term_label(x) else swimmer_drug_name(x)
}
