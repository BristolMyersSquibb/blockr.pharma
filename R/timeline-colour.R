# What colours each track of the timeline and the patient timeline: a column
# of the track's table (ADAE, ADCM), a parameter (ADRS) or a fixed choice
# (ADEX). The choices come from the data, as the swimmer's response
# parameter does: a column the study lacks is not offered. The prepare
# script's select shows each field's label and stores its name.
#
# A field's levels draw in the order the board's scale map pins them (the
# theme sidebar), else in a known order for the CDISC vocabularies, else as
# they sort; the last is the worst, which the closed stripe shows where
# records overlap. Colours come from the scale map, bound by the column's
# own name, else from the palettes here.

# Toxicity grade and severity, in either spelling; the first present lists.
TL_AE_GRADE <- c("AETOXGR", "ATOXGR")
TL_AE_SEV <- c("AESEV", "ASEV")
# Further AE fields, after the action-taken columns (AEACN, and AEACNA,
# AEACNB, ... with more than one study drug).
TL_AE_MORE <- c("AESER", "AEREL", "AEOUT")
TL_CM_FIELDS <- c("CMCLAS", "CMINDC", "CMROUTE")
TL_ONE_COLOUR <- "one colour"
TL_CM_STARTED <- "before or on treatment"

# Labels for fields whose column carries none.
TL_FIELD_LABELS <- c(
  AETOXGR = "Toxicity grade", ATOXGR = "Toxicity grade",
  AESEV = "Severity", ASEV = "Severity",
  AEACN = "Action taken with study treatment", AESER = "Serious",
  AEREL = "Causality", AEOUT = "Outcome", CMCLAS = "Class",
  CMINDC = "Indication", CMROUTE = "Route"
)

# Known orders, best to worst, by field family, in the data's capitals.
TL_FIELD_ORDERS <- list(
  AEACN = c("NOT APPLICABLE", "DOSE NOT CHANGED", "DOSE INCREASED",
            "DRUG INTERRUPTED", "DOSE REDUCED", "DRUG WITHDRAWN", "MULTIPLE",
            "UNKNOWN"),
  AESER = c("N", "Y"),
  AEREL = c("NONE", "NOT RELATED", "UNLIKELY RELATED", "REMOTE", "POSSIBLE",
            "PROBABLE", "RELATED"),
  AEOUT = c("RECOVERED/RESOLVED", "RECOVERING/RESOLVING",
            "RECOVERED/RESOLVED WITH SEQUELAE", "NOT RECOVERED/NOT RESOLVED",
            "UNKNOWN", "FATAL")
)

# Colours for those orders, in order; a level outside the order is grey.
TL_FIELD_PALETTES <- list(
  AEACN = c("#c9ced6", "#2E8B57", "#4C8FBF", "#E0B83E", "#F0A04B", "#8B0000",
            "#7B5EA7", "#9aa3ad"),
  AESER = c("#bdbdbd", "#D7322A"),
  AEREL = c("#d0d0d0", "#d0d0d0", "#c6dbef", "#9ecae1", "#4292c6", "#2171b5",
            "#08519c"),
  AEOUT = c("#2E8B57", "#8DBB4F", "#B5A642", "#F0A04B", "#9aa3ad", "#1f1f1f")
)

# A palette for a field without a known order, light to dark.
TL_FIELD_SEQ <- c("#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08519c",
                  "#08306b", "#5aa5c2", "#2f7f9e")

#' The fields that can colour a track of the timeline
#'
#' What the "Colour by" row of a track offers, read off the data: the
#' columns of ADAE or ADCM, or the response parameters of ADRS, that the
#' study carries, each named by its label. AEs list the study's severity
#' column first (the board's "study_roles", as the patient profile reads
#' it), then the toxicity grade or severity, every action-taken column
#' (`AEACN`, `AEACNA`, `AEACNB`, ...), seriousness, causality and outcome.
#' Con meds list class, indication and route, then before or on treatment.
#' The response lists its parameters; treatment its dose level and drug.
#' AEs, con meds and treatment end with "one colour".
#'
#' @param data The block's input dm.
#' @param track `"ae"`, `"cm"`, `"rs"` or `"ex"`.
#' @param roles The study's roles ([patient_timeline_roles()]), or `NULL`.
#' @param grade_first List the toxicity grade first when ADAE has one (the
#'   timeline block's default), else the study's severity column.
#' @return A character vector of field names, named by their labels.
#' @export
timeline_colour_fields <- function(data, track = c("ae", "cm", "rs", "ex"),
                                   roles = NULL, grade_first = FALSE) {
  track <- match.arg(track)
  if (identical(track, "rs")) return(tl_rs_fields(data))
  if (identical(track, "ex")) {
    return(c(`Dose level` = "dose level", Drug = "drug",
             `One colour` = TL_ONE_COLOUR))
  }
  tb <- tryCatch(pt_tables(data, character()), error = function(e) NULL)
  tbl <- if (identical(track, "ae")) tb$adae else tb$adcm
  cols <- if (is.null(tbl)) character() else names(tbl)
  has <- function(x) {
    x[vapply(x, function(col) {
      v <- tbl[[col]]
      any(!is.na(v) & nzchar(trimws(as.character(v))))
    }, NA)]
  }
  if (identical(track, "cm")) {
    f <- has(intersect(TL_CM_FIELDS, cols))
    return(c(stats::setNames(f, tl_field_label(tbl, f)),
             `Started before or on treatment` = TL_CM_STARTED,
             `One colour` = TL_ONE_COLOUR))
  }
  grade <- has(intersect(TL_AE_GRADE, cols))[1L]
  sev <- has(intersect(TL_AE_SEV, cols))[1L]
  study <- tryCatch(pp_sev_column(cols, roles$severity),
                    error = function(e) NULL)
  # The study's column, listed under the spelling the list uses.
  first <- if (length(study) && study %in% TL_AE_GRADE) grade else
    if (length(study) && study %in% TL_AE_SEV) sev else study
  if (isTRUE(grade_first) && !is.na(grade)) first <- grade
  acn <- has(grep("^AEACN[A-Z]?$", cols, value = TRUE))
  f <- unique(stats::na.omit(c(first, grade, sev, acn,
                               has(intersect(TL_AE_MORE, cols)))))
  c(stats::setNames(f, tl_field_label(tbl, f)), `One colour` = TL_ONE_COLOUR)
}

# A column's label: its own, else the one above, else its name.
tl_field_label <- function(tbl, f) {
  vapply(f, function(col) {
    lab <- attr(tbl[[col]], "label", exact = TRUE)
    if (is.character(lab) && length(lab) == 1L && nzchar(lab)) return(lab)
    if (col %in% names(TL_FIELD_LABELS)) return(TL_FIELD_LABELS[[col]])
    if (startsWith(col, "AEACN")) return(paste("Action taken,", col))
    col
  }, "", USE.NAMES = FALSE)
}

# The response parameters, named by PARAM where ADRS carries it.
tl_rs_fields <- function(data) {
  tb <- tryCatch(pt_tables(data, character()), error = function(e) NULL)
  rs <- tb$adrs
  pv <- swimmer_parameters_of(rs)
  lab <- pv
  if (!is.null(rs) && all(c("PARAMCD", "PARAM") %in% names(rs))) {
    p <- as.character(rs$PARAM)[match(pv, as.character(rs$PARAMCD))]
    lab <- ifelse(is.na(p) | !nzchar(p), pv, paste0(p, " (", pv, ")"))
  }
  stats::setNames(pv, lab)
}

#' The board's scale map, as the prepare script reads it
#'
#' Reads the "scale_map" board option (the theme sidebar) of the board the
#' calling block sits on, so the timeline orders a field's levels by the
#' map's pins. Called in the block's prepare script.
#'
#' @return A scale map, or `NULL` outside a board or without one.
#' @export
timeline_scale_map <- function() {
  session <- tryCatch(blockr.core::get_session(), error = function(e) NULL)
  if (is.null(session)) return(NULL)
  val <- tryCatch(
    blockr.core::get_board_option_or_null("scale_map", session),
    error = function(e) NULL
  )
  if (is.null(val) || !length(val)) return(NULL)
  tryCatch(blockr.theme::as_scale_map(val), error = function(e) NULL)
}

# Saved words of the old fixed choices, as the fields they meant. A word
# the data decides ("grade or severity", "study severity") becomes NULL: the
# field list's first entry.
tl_colour_migrate <- function(x, track = c("ae", "cm")) {
  track <- match.arg(track)
  x <- as.character(unlist(x))
  if (!length(x)) return(NULL)
  x <- x[[1L]]
  map <- if (identical(track, "ae")) {
    c(grade = "AETOXGR", severity = "AESEV", AE = TL_ONE_COLOUR)
  } else {
    c(class = "CMCLAS", indication = "CMINDC",
      `class, else before or on treatment` = TL_CM_STARTED)
  }
  if (x %in% c("grade or severity", "study severity")) return(NULL)
  if (x %in% names(map)) unname(map[[x]]) else x
}

# How AEs colour for a field: "grade" and "severity" (the colouring the
# timeline always had, read from `col`), "one colour", or "column" (any other
# column, by its values). The old words still work; a field the data lacks
# falls back to the study's severity.
tl_ae_field <- function(ae, field, roles = NULL) {
  # The timeline's old default: grade where ADAE has one, else severity.
  if (identical(as.character(unlist(field))[1L], "grade or severity")) {
    by <- aet_colour_by(ae, "grade or severity")
    field <- switch(by, Grade = "AETOXGR", Severity = "AESEV", TL_ONE_COLOUR)
  }
  field <- tl_colour_migrate(field, "ae")
  cols <- names(ae)
  pick <- function(set) {
    hit <- intersect(set, cols)
    if (length(hit)) hit[[1L]]
  }
  if (identical(field, TL_ONE_COLOUR)) return(list(mode = "one"))
  if (!is.null(field) && field %in% c(TL_AE_GRADE, TL_AE_SEV) &&
        field %in% cols) {
    return(list(mode = if (field %in% TL_AE_GRADE) "grade" else "severity",
                col = field))
  }
  if (!is.null(field) && field %in% c(TL_AE_GRADE, TL_AE_SEV)) {
    set <- if (field %in% TL_AE_GRADE) TL_AE_GRADE else TL_AE_SEV
    col <- pick(set)
    if (!is.null(col)) {
      return(list(mode = if (field %in% TL_AE_GRADE) "grade" else "severity",
                  col = col))
    }
  }
  if (!is.null(field) && field %in% cols) {
    return(list(mode = "column", col = field))
  }
  study <- pp_sev_column(cols, roles$severity)
  if (is.null(study)) return(list(mode = "one"))
  # A declared column of another name is a grade when its values are.
  grade <- study %in% TL_AE_GRADE ||
    (!study %in% TL_AE_SEV &&
       all(grepl("^[0-9]+$", stats::na.omit(as.character(ae[[study]])))))
  list(mode = if (grade) "grade" else "severity", col = study)
}

# A field's levels in drawing order, worst last, and their colours: the
# scale map's pins for `col` first (its own colours apply in the table),
# then the field's known order, then the rest as they sort. `raw` the
# values as the data has them, `lv` as the legend shows them (same length).
tl_field_order <- function(col, raw, lv, scale_map = NULL) {
  pairs <- unique(data.frame(raw = as.character(raw), lv = as.character(lv),
                             stringsAsFactors = FALSE))
  pairs <- pairs[!is.na(pairs$raw), , drop = FALSE]
  fam <- if (startsWith(col, "AEACN")) "AEACN" else col
  pins <- tl_scale_pins(scale_map, col)
  known <- TL_FIELD_ORDERS[[fam]] %||% character()
  key <- toupper(pairs$raw)
  rank <- match(pairs$raw, pins)
  rank2 <- match(key, known)
  o <- order(is.na(rank), rank, is.na(rank2), rank2, pairs$lv)
  pairs <- pairs[o, , drop = FALSE]
  pal <- TL_FIELD_PALETTES[[fam]]
  col_of <- if (!is.null(pal)) {
    p <- pal[match(toupper(pairs$raw), known)]
    p[is.na(p)] <- "#9aa3ad"
    p
  } else {
    rep_len(TL_FIELD_SEQ, nrow(pairs))
  }
  stats::setNames(col_of, pairs$lv)
}

# The values a scale map pins for `col`, in its order; none without one.
tl_scale_pins <- function(scale_map, col) {
  if (is.null(scale_map)) return(character())
  m <- tryCatch(blockr.theme::as_scale_map(scale_map), error = function(e) NULL)
  spec <- if (!is.null(m)) m[[col]]$color
  nm <- names(spec)
  if (is.null(nm)) character() else nm[nzchar(nm)]
}

# The column that colours con meds, or NULL for before or on treatment and
# one colour (the words tl_cm_colour() reads). Old words still work; a
# column the data lacks falls back to before or on treatment.
tl_cm_field <- function(cm, field) {
  field <- tl_colour_migrate(field, "cm")
  if (is.null(field) || field %in% c(TL_CM_STARTED, TL_ONE_COLOUR)) {
    return(NULL)
  }
  if (!is.null(cm) && field %in% names(cm)) field
}

# The words tl_cm_colour() reads for a con-med field.
tl_cm_word <- function(field) {
  field <- tl_colour_migrate(field, "cm") %||% TL_CM_STARTED
  if (field %in% c(TL_CM_STARTED, TL_ONE_COLOUR)) field else TL_CM_STARTED
}

# The field orders and colours the parts carry (tl_field_order()), by the
# source names tl_colour_factor() reads.
tl_field_palettes <- function(parts) {
  out <- list(AE = attr(parts$ae, "tl_field"), CM = attr(parts$cm, "tl_field"))
  Filter(Negate(is.null), out)
}

# Saved colour picks of earlier versions as the fields they meant; one the
# data decides is dropped, so the field list's first entry applies.
tl_colour_values_migrate <- function(sv) {
  if (!is.null(sv$ae_colour)) {
    sv["ae_colour"] <- list(tl_colour_migrate(sv$ae_colour, "ae"))
  }
  if (!is.null(sv$cm_colour)) {
    sv["cm_colour"] <- list(
      tl_colour_migrate(tl_cm_colour_value(sv$cm_colour), "cm")
    )
  }
  Filter(Negate(is.null), sv)
}
