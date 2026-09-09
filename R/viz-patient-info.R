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
  # `span`: the value is too long to sit two to a line, so its row takes the
  # full width. CSS grid auto-placement does the rest -- a full-width item
  # that would land in the second column starts a new row instead, so the
  # pairing heals itself whatever an optional field does.
  #
  # `tint`: a colour this fact carries elsewhere on the board, or "". Only
  # two facts have one, and both are drawn as the cohort list's chip.
  add <- function(label, value, span = FALSE, tint = "") {
    value <- trimws(as.character(value))
    if (length(value) != 1L || is.na(value) || !nzchar(value)) return()
    acc$fields[[length(acc$fields) + 1L]] <-
      list(label, value, isTRUE(span), tint)
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
  add("Site", chr("SITEID"))
  add("Race", pp_title_case(chr("RACE")))
  add("Country", chr("COUNTRY"))
  add("Ethnicity", pp_title_case(chr("ETHNIC")), span = TRUE)

  # The arm column is a role, resolved once by the block and injected --
  # same source as the overview lane, so the two cannot disagree.
  arm_col <- settings$roles$arm
  if (!is.null(arm_col)) {
    # "Arm", not "Treatment arm": it pairs with Response on one line, and
    # there is nothing else on this card the word could mean.
    add("Arm", chr(arm_col), tint = pp_info_tint(settings$arm_colors,
                                                 chr(arm_col)))
  }

  # The one outcome fact on this card, and the only thing it reads outside
  # ADSL. The best overall response is what a reviewer opening a patient in
  # an oncology study wants stated in words before any chart -- the response
  # LANE says how the patient got there, this says where they ended up.
  # Nothing else in adrs is printed: the rest of its subject-level parameters
  # are derivations of this one (confirmed, clinical benefit, response
  # yes/no), and a card that listed all twelve would bury the demographics.
  bor <- pp_resp_bor_field(dm_obj)
  if (!is.null(bor)) {
    # "Response", pairing with Arm, and in the response lane's own colour:
    # the two coloured facts on this card are the two the board colours
    # everywhere else.
    add("Response", bor$value,
        tint = pp_info_tint(settings$resp_colors, bor$value))
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
    add("Treatment period", period, span = TRUE)
  }

  eos <- date("RFENDT")
  if (!is.na(eos)) add("End of study", format(eos))
  dth <- date("DTHDT")
  if (!is.na(dth)) {
    add("Death", format(dth))
  } else if (identical(chr("DTHFL"), "Y")) {
    add("Death", "Yes (date unknown)", span = TRUE)
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
    Span  = vapply(acc$fields, `[[`, logical(1L), 3L),
    Tint  = vapply(acc$fields, `[[`, character(1L), 4L),
    stringsAsFactors = FALSE
  )
}

#' The chip style for a level, or "" when the board has no colour for it
#'
#' Reuses [pp_cohort_chip_style()] rather than computing a tint of its own,
#' so the arm chip on this card and the arm chip on the cohort row it was
#' opened from are the same object: same colour in, same 12% fill and 72%
#' text out.
#'
#' @param colors Named level -> hex vector, or `NULL`.
#' @param level The level to look up.
#' @return An inline style string, or `""`.
#' @noRd
pp_info_tint <- function(colors, level) {
  if (is.null(colors) || !length(colors)) return("")
  level <- trimws(as.character(level))
  if (length(level) != 1L || is.na(level) || !level %in% names(colors)) {
    return("")
  }
  pp_cohort_chip_style(unname(colors[[level]]))
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
  optional = list(
    adsl = c(
      "AGE", "AGEU", "SEX", "RACE", "ETHNIC", "COUNTRY", "SITEID",
      "TRTSDT", "TRTEDT", "RFENDT", "DTHDT", "DTHFL",
      "HEIGHTBL", "WEIGHTBL", "BMIBL"
    ),
    # Optional table AND optional columns: a study with no response data
    # loses the row, not the card. `tables` stays "adsl" on purpose --
    # listing adrs there would hide the whole patient info card from every
    # study that does not ship one.
    adrs = c("PARAMCD", "PARAM", "AVALC")
  ),
  # "arm" for the arm's own colour (resolved over the cohort, so the chip
  # here and the chip on the row this card was opened from are the same),
  # "response" for the RECIST palette the lane below draws in.
  uses = c("arm", "response"),
  render = function(dm_obj, time_range, settings = list(),
                    ref_ms = NA_real_, mode = "date") {
    info <- pp_patient_info_fields(dm_obj, settings)
    if (nrow(info) == 0L) {
      return(pp_empty_chart("No subject-level record"))
    }
    pp_info_grid(info)
  },
  exhibit = function(dm_obj, time_range, settings = list(),
                     ref_ms = NA_real_, mode = "date") {
    info <- pp_patient_info_fields(dm_obj, settings)
    if (nrow(info) == 0L) return(NULL)
    # The export is label and value, as it has always been: the span and the
    # tint are how the card LAYS the facts out, not facts of their own.
    info[, c("Field", "Value"), drop = FALSE]
  },
  exhibit_kind = "table"
)

#' The facts as a two-column grid
#'
#' A table, before: one fact per row, a fixed 160px label column and a
#' hairline under each. Eleven facts made a 400px card in a 311px-wide rail,
#' taller than the chart under it and five times the response lane, and every
#' one of those facts is short enough to sit two to a line. The pairs come out
#' at 226px.
#'
#' Not a `<table>`: the pairs are a layout, not a two-column relation, and a
#' table would claim the value in column 2 and the label in column 3 are the
#' same kind of thing. Definition-list semantics in a grid, so a screen reader
#' still reads label then value.
#'
#' @param info The frame from [pp_patient_info_fields()].
#' @return A `htmltools` tag.
#' @noRd
pp_info_grid <- function(info) {
  pairs <- lapply(seq_len(nrow(info)), function(i) {
    tint <- info$Tint[i]
    value <- if (nzchar(tint)) {
      # The cohort list's chip, same style string, same colour.
      htmltools::tags$span(class = "pp-info-chip", style = tint,
                           info$Value[i])
    } else {
      info$Value[i]
    }
    # A label and its value are ONE grid item, not two. Two items in a
    # four-column grid pack tightly but cannot fold: at a narrow rail the
    # third column stayed a column and the values broke a character at a
    # time down it. Wrapped as a pair, the outer grid folds to one column
    # when two will not fit, and the pair stays whole either way.
    htmltools::tags$div(
      class = paste("pp-info-pair", if (isTRUE(info$Span[i])) "is-wide"),
      htmltools::tags$dt(class = "pp-info-k", info$Field[i]),
      htmltools::tags$dd(class = "pp-info-v", value)
    )
  })
  htmltools::tags$dl(class = "pp-info-grid", pairs)
}
