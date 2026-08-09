# Patient Profile Viz: Patient Info
#
# A key-value table of the subject's basic facts -- demographics, treatment
# arm, treatment period, study milestones, baseline measurements -- read off
# the subject's ADSL row. The card most reviews start on: who is this
# patient, before any timeline says what happened to them.
#
# One extraction helper feeds BOTH renderings, so the on-screen table and
# the exported one cannot drift:
#   * render   -- a design-system HTML table (no echarts; a table is not a
#                 chart).
#   * exhibit  -- the same rows as a data frame, which blockr.viz's DEFAULT
#                 exhibit methods typeset: a native, editable PowerPoint
#                 table on a slide, the display grid in HTML, a sheet in
#                 Excel. This is the profile's first `exhibit_kind =
#                 "table"` viz -- nothing here draws.
#
# Every field is optional except the subject id: the table shows what the
# study collected and nothing else (no derived values beyond the treatment
# duration, which is labelled as a span of its own dates).
#
# Data requirements (declared via new_pp_viz()):
#   adsl: required USUBJID; everything else optional
#   roles: arm (the ADSL arm column, settings$roles$arm)

#' The subject's basic facts, as label/value rows
#'
#' @param dm_obj Subject-scoped, normalized dm.
#' @param settings Injected settings (reads `roles$arm`).
#' @return Data frame with `Field` and `Value` character columns; zero rows
#'   when there is no ADSL row.
#' @noRd
pp_patient_info_fields <- function(dm_obj, settings = list()) {
  empty <- data.frame(Field = character(), Value = character(),
                      stringsAsFactors = FALSE)
  adsl <- tryCatch(
    as.data.frame(dm::dm_get_tables(dm_obj)[["adsl"]]),
    error = function(e) NULL
  )
  if (is.null(adsl) || nrow(adsl) == 0L) return(empty)
  sl <- adsl[1L, , drop = FALSE]

  # add() collects from inside a closure, so the accumulator lives in its own
  # environment instead of reaching up the call stack with `<<-`.
  acc <- new.env(parent = emptyenv())
  acc$fields <- list()
  add <- function(label, value) {
    value <- trimws(as.character(value))
    if (length(value) != 1L || is.na(value) || !nzchar(value)) return()
    acc$fields[[length(acc$fields) + 1L]] <- list(label, value)
  }
  chr <- function(col) {
    if (!col %in% colnames(sl)) return(NA_character_)
    v <- sl[[col]][1L]
    if (is.na(v)) NA_character_ else as.character(v)
  }
  num <- function(col, digits = 1L) {
    if (!col %in% colnames(sl)) return(NA_character_)
    v <- suppressWarnings(as.numeric(sl[[col]][1L]))
    if (is.na(v)) NA_character_ else as.character(round(v, digits))
  }
  date <- function(col) {
    if (!col %in% colnames(sl)) return(as.Date(NA))
    pp_as_date(sl[[col]][1L])
  }

  add("Subject", chr("USUBJID"))

  # Age with its unit, in the study's own words ("63 YEARS" -> "63 years").
  age <- num("AGE", 0L)
  if (!is.na(age)) {
    unit <- chr("AGEU")
    add("Age", if (is.na(unit)) age else paste(age, tolower(unit)))
  }
  add("Sex", chr("SEX"))
  add("Race", pp_title_case(chr("RACE")))
  add("Ethnicity", pp_title_case(chr("ETHNIC")))
  add("Country", chr("COUNTRY"))
  add("Site", chr("SITEID"))

  # The arm column is a role, resolved once by the block and injected --
  # same source as the overview lane, so the two cannot disagree.
  arm_col <- settings$roles$arm
  if (!is.null(arm_col)) {
    add("Treatment arm", chr(arm_col))
  }

  # Treatment period as one row: both dates, and the span they enclose.
  trt_s <- date("TRTSDT")
  trt_e <- date("TRTEDT")
  if (!is.na(trt_s)) {
    period <- if (!is.na(trt_e)) {
      sprintf("%s \u2192 %s (%d days)", format(trt_s), format(trt_e),
              as.integer(trt_e - trt_s) + 1L)
    } else {
      paste(format(trt_s), "\u2192 ongoing")
    }
    add("Treatment period", period)
  }

  eos <- date("RFENDT")
  if (!is.na(eos)) add("End of study", format(eos))
  dth <- date("DTHDT")
  if (!is.na(dth)) {
    add("Death", format(dth))
  } else if (identical(chr("DTHFL"), "Y")) {
    add("Death", "Yes (date unknown)")
  }

  # Baseline measurements, when the study derived them into ADSL.
  h <- num("HEIGHTBL")
  if (!is.na(h)) add("Height (baseline)", paste(h, "cm"))
  w <- num("WEIGHTBL")
  if (!is.na(w)) add("Weight (baseline)", paste(w, "kg"))
  b <- num("BMIBL")
  if (!is.na(b)) add("BMI (baseline)", b)

  data.frame(
    Field = vapply(acc$fields, `[[`, character(1L), 1L),
    Value = vapply(acc$fields, `[[`, character(1L), 2L),
    stringsAsFactors = FALSE
  )
}

#' Patient Info visualization definition
#' @noRd
patient_info_viz <- new_pp_viz(
  id = "patient_info",
  label = "Patient Info",
  domain = "Patient",
  icon = "person-vcard",
  color = "#374151",
  description = "Demographics, arm, treatment period & baseline facts",
  tables = "adsl",
  requires = list(adsl = "USUBJID"),
  optional = list(adsl = c(
    "AGE", "AGEU", "SEX", "RACE", "ETHNIC", "COUNTRY", "SITEID",
    "TRTSDT", "TRTEDT", "RFENDT", "DTHDT", "DTHFL",
    "HEIGHTBL", "WEIGHTBL", "BMIBL"
  )),
  uses = "arm",
  render = function(dm_obj, time_range, settings = list(),
                    ref_ms = NA_real_, mode = "date") {
    info <- pp_patient_info_fields(dm_obj, settings)
    if (nrow(info) == 0L) {
      return(pp_empty_chart("No subject-level record"))
    }
    htmltools::tags$table(
      class = "pp-info-table",
      htmltools::tags$tbody(
        lapply(seq_len(nrow(info)), function(i) {
          htmltools::tags$tr(
            htmltools::tags$th(scope = "row", info$Field[i]),
            htmltools::tags$td(info$Value[i])
          )
        })
      )
    )
  },
  exhibit = function(dm_obj, time_range, settings = list(),
                     ref_ms = NA_real_, mode = "date") {
    info <- pp_patient_info_fields(dm_obj, settings)
    if (nrow(info) == 0L) return(NULL)
    info
  },
  exhibit_kind = "table"
)
