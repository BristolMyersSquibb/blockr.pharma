# Tumour response, as a lane.
#
# adrs is a BDS findings table whose analysis value is a WORD: `AVALC` carries
# the RECIST category (CR / PR / SD / PD / NE / NON-CR/NON-PD), and `AVAL` its
# numeric code. That is why it gets nothing from pp_findings_vizs() -- a value
# line needs a number to have a height -- and why it is drawn here instead, as
# the third gantt beside adverse events and concomitant medications.
#
# What the study ships decides which cards exist, the same rule the findings
# cards follow. adrs pools a dozen parameters and most of them are ONE record
# per subject (best overall response, death, measurable disease at baseline):
# subject-level facts, which belong in the patient info table, not on a time
# axis. The parameters worth a lane are the ones assessed repeatedly, and
# "assessed repeatedly" is a property of the data, not a list of codes this
# package would have to keep in step with a therapeutic area. See
# pp_resp_lane_params().
#
# A point-in-time assessment becomes an interval by running to the NEXT
# assessment; the last one has no next and runs to the edge of the window with
# the open-end caret, exactly as an unresolved adverse event does. That is the
# honest drawing: the last known response is what is known, and how long it
# held is not.
#
# Consecutive equal responses are NOT merged. Three SD assessments are three
# assessments, and the profile's mark radius is chosen so abutting bars keep a
# visible seam for precisely this reason (see PP_MARK_RADIUS).

#' Built-in colors for the RECIST response categories
#'
#' Read when the board carries no scale map, or none binding the response
#' column -- the same job [pp_sev_colors] does for adverse-event severity, and
#' the same keep-in-step relationship: CEDX's `cedx_scale_map()` declares this
#' palette for `BEST_OVERALL_RESPONSE`, so if the two drift the same category
#' reads as two colors depending on whether a board has a map.
#'
#' Ordered best to worst, which is the order the legend prints and the order a
#' clinician reads: complete response, partial response, stable disease,
#' non-target-only stable disease, progressive disease, then the two ways of
#' having no assessment.
#'
#' `NON-CR/NON-PD` is RECIST's verdict for a patient with non-target lesions
#' only. It is a "not progressing" reading, so it takes a colour in the stable
#' family rather than a warning one, muted because it is a weaker statement
#' than SD.
#' @noRd
pp_resp_colors <- c(
  "CR"            = "#006400",
  "PR"            = "#FFD700",
  "SD"            = "#FFA500",
  "NON-CR/NON-PD" = "#C2A878",
  "PD"            = "#8b0000",
  "NE"            = "#6D8196",
  "NR"            = "#595959",
  "UN"            = "#858585",
  "MISSING"       = "#858585"
)

#' Built-in color for one response category
#'
#' Grey for anything this package has no opinion about, the same fallback
#' [pp_sev_fallback_color()] uses. A study with its own vocabulary binds it in
#' the board scale map rather than being guessed at here.
#' @param resp A single response category.
#' @return A hex color.
#' @noRd
pp_resp_fallback_color <- function(resp) {
  s <- toupper(trimws(as.character(resp)))
  if (length(s) == 1L && !is.na(s) && s %in% names(pp_resp_colors)) {
    unname(pp_resp_colors[[s]])
  } else {
    "#9ca3af"
  }
}

#' Resolve response colors for the categories a patient's adrs carries
#'
#' The board scale map first, keyed on the response column (`AVALC`), so a
#' board that binds it wins and provenance is followed the way
#' [pp_indc_scale_colors()] follows it. The built-in constants fill whatever
#' the map leaves open, and stand alone on a board with no map.
#'
#' Unlike the indication colors this never returns `NULL` for want of a
#' palette: the categories are a fixed clinical vocabulary with fixed
#' meanings, so there is always something to say, and a grey lane would lose
#' the one distinction the panel exists to draw.
#'
#' Scoped to the PARAMETER the card draws, not to the whole table. adrs pools
#' a dozen parameters and most of them are Y/N flags, so resolving table-wide
#' put "Y", "N" and "MISSING" in the response legend beside CR and PD -- three
#' swatches for a vocabulary the lane does not use.
#'
#' @param map The board scale map, or `NULL`.
#' @param dm_obj A normalized `dm`.
#' @param paramcds The card's PARAMCDs, or `NULL` for the whole table.
#' @param table The response table.
#' @param value_col The column carrying the category.
#' @return Named character vector (category -> hex), in
#'   [pp_resp_colors] order with anything unrecognised last, or `NULL` when
#'   the table carries no categories at all.
#' @noRd
pp_resp_scale_colors <- function(map, dm_obj, paramcds = NULL,
                                 table = "adrs", value_col = "AVALC") {
  tbl <- tryCatch(dm::dm_get_tables(dm_obj)[[table]], error = function(e) NULL)
  if (is.null(tbl) || !value_col %in% colnames(tbl)) return(NULL)
  tbl <- as.data.frame(tbl)
  if (!is.null(paramcds) && length(paramcds) &&
        "PARAMCD" %in% colnames(tbl)) {
    tbl <- tbl[as.character(tbl$PARAMCD) %in% paramcds, , drop = FALSE]
  }
  if (nrow(tbl) == 0L) return(NULL)

  column <- tbl[[value_col]]
  keep <- !is.na(column) & nzchar(trimws(as.character(column)))
  column <- column[keep]
  if (!length(column)) return(NULL)

  levels <- unique(as.character(column))
  # Board first. resolve_scales_col() reads factor levels and follows
  # `blockr_source`, so a renamed response column inherits its source's
  # binding.
  bound <- if (!is.null(map)) {
    tryCatch(blockr.theme::resolve_scales_col(map, value_col, column)$color,
             error = function(e) NULL)
  }

  cols <- vapply(levels, function(lv) {
    hit <- if (!is.null(bound) && lv %in% names(bound)) bound[[lv]]
    if (!is.null(hit) && length(hit) == 1L && !is.na(hit)) {
      as.character(hit)
    } else {
      pp_resp_fallback_color(lv)
    }
  }, character(1L))

  # Clinical order, not data order: a legend that reshuffles between patients
  # is a legend a reader has to re-read every time.
  known <- intersect(names(pp_resp_colors), toupper(levels))
  ord <- c(levels[match(known, toupper(levels))],
           sort(setdiff(levels, levels[match(known, toupper(levels))])))
  cols[ord]
}

#' Which adrs parameters are assessed over time
#'
#' A lane needs more than one point. A parameter recorded once per subject is
#' a subject-level fact whatever it is called, and one recorded repeatedly is
#' a course of assessments -- so the split is read off the records rather than
#' from a list of PARAMCDs, which would have to be maintained per therapeutic
#' area and would silently drop any study that codes its endpoints
#' differently.
#'
#' Judged on the WHOLE cohort, because the card catalog is a property of the
#' study: a patient who happens to have one assessment must not lose the card
#' that every other patient has.
#'
#' @param tbl A data frame (the response table).
#' @return Character vector of PARAMCDs, in the order the table lists them.
#' @noRd
pp_resp_lane_params <- function(tbl) {
  need <- c("USUBJID", "PARAMCD", "AVALC")
  if (!all(need %in% colnames(tbl))) return(character())
  ok <- !is.na(tbl$AVALC) & nzchar(trimws(as.character(tbl$AVALC)))
  if (!any(ok)) return(character())
  tbl <- tbl[ok, , drop = FALSE]
  codes <- as.character(tbl$PARAMCD)
  key <- paste(codes, as.character(tbl$USUBJID), sep = "\r")
  per <- table(key)
  repeated <- sub("\r.*$", "", names(per)[per > 1L])
  if (!length(repeated)) return(character())
  # Table order, not table() order: the card list must not reshuffle because
  # a level count changed.
  intersect(unique(codes), repeated)
}

#' The intervals a response lane draws, from point-in-time assessments
#'
#' Shared by the interactive render and its static twin so the two cannot
#' disagree about where a bar starts or how long it runs.
#'
#' @param tbl One parameter's records for one patient.
#' @param time_range,ref_ms,mode The axis, as passed to a viz render.
#' @return A data frame with `start`, `end`, `ongoing`, `resp`, `s_lab` and
#'   `e_lab`, in time order; zero rows when nothing can be placed on the axis.
#' @noRd
pp_resp_segments <- function(tbl, time_range, ref_ms = NA_real_,
                             mode = "date") {
  empty <- data.frame(start = numeric(), end = numeric(),
                      ongoing = logical(), resp = character(),
                      s_lab = character(), e_lab = character(),
                      stringsAsFactors = FALSE)
  if (nrow(tbl) == 0L) return(empty)

  # The study's own day where it has one, the analysis date otherwise -- the
  # same preference every other panel applies, and the reason adrs tables
  # shipping no ADY still land on the shared axis.
  has_day <- "ADY" %in% colnames(tbl)
  use_day <- identical(mode, "rday") && has_day
  if (!use_day && !"ADT" %in% colnames(tbl)) return(empty)

  at <- if (use_day) pp_as_numeric(tbl$ADY) else tbl$ADT
  keep <- !is.na(at) & !is.na(tbl$AVALC) &
    nzchar(trimws(as.character(tbl$AVALC)))
  tbl <- tbl[keep, , drop = FALSE]
  at <- at[keep]
  if (nrow(tbl) == 0L) return(empty)

  o <- order(at)
  tbl <- tbl[o, , drop = FALSE]
  at <- at[o]

  x <- if (use_day) {
    pp_xval_pref_day(NULL, at, ref_ms, mode)
  } else {
    pp_xval_pref_day(at, NULL, ref_ms, mode)
  }
  labs <- if (use_day) {
    vapply(seq_along(at), function(i) pp_day_label(at[i]), character(1L))
  } else {
    vapply(seq_along(at), function(i) pp_xlabel(at[i], ref_ms, mode),
           character(1L))
  }

  day_unit <- if (identical(mode, "rday")) 1 else 86400000
  n <- length(x)
  # Each assessment holds until the next one. The last has no next: it runs to
  # the window edge and says so with the open-end caret, the same encoding an
  # adverse event with no end date gets.
  end <- c(x[-1L], NA_real_)
  ongoing <- is.na(end)
  if (any(ongoing)) {
    end[ongoing] <- pp_gantt_open_end(x[ongoing], time_range, ref_ms, mode,
                                      day_unit)
  }

  data.frame(
    start = x,
    end = end,
    ongoing = ongoing,
    resp = as.character(tbl$AVALC),
    s_lab = labs,
    e_lab = c(labs[-1L], PP_ONGOING_LABEL),
    stringsAsFactors = FALSE
  )
}

#' The patient's best overall response, for the info table
#'
#' `BOR` is ADaM oncology's code for it, in the same way `AVAL` is BDS's code
#' for the analysis value: a name the package may read directly rather than a
#' study choice to be declared. A study that does not ship it simply has no
#' row here.
#'
#' The category as the study wrote it, minus its way of writing "not
#' assessed": ADaM fills BOR for every subject, so a cohort's untreated or
#' unassessed patients carry a literal "MISSING", and printing that as a
#' finding would be worse than printing nothing.
#'
#' @param dm_obj A normalized `dm`, scoped to one subject.
#' @param table The response table.
#' @return `list(label, value)`, or `NULL`.
#' @noRd
pp_resp_bor_field <- function(dm_obj, table = "adrs") {
  tbl <- tryCatch(dm::dm_get_tables(dm_obj)[[table]], error = function(e) NULL)
  if (is.null(tbl)) return(NULL)
  tbl <- as.data.frame(tbl)
  if (!all(c("PARAMCD", "AVALC") %in% colnames(tbl))) return(NULL)

  hit <- tbl[as.character(tbl$PARAMCD) %in% "BOR", , drop = FALSE]
  if (nrow(hit) == 0L) return(NULL)
  val <- trimws(as.character(hit$AVALC[[1L]]))
  if (is.na(val) || !nzchar(val) || identical(toupper(val), "MISSING")) {
    return(NULL)
  }
  # A fixed short label, not the study's PARAM. This table is label/value
  # pairs in a 600px rail, and ADaM's own text for BOR runs to sixty
  # characters ("Best Overall Response by Investigator (confirmation not
  # required)"), which would set the column width for every other row.
  list(label = "Best overall response", value = val)
}
