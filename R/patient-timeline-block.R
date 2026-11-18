# The timeline, built from blockr.viz's table block.
#
# One row per patient with a stripe per layer drawn (response, treatment,
# AEs, con meds; discontinuation and death as marks); a click opens the
# patient into a row per AE term and con med, on the same axis, each layer
# under a small title. Treatment and the response do not open: their detail
# is in the tooltips. The table block's prepare step,
# patient_timeline_frame(), reads the ADaM tables from the input dm.
#
# One block for every use: the layers a reader ticks in the gear decide what
# it shows, and the gear's Lanes tab shows the settings of those layers only.

#' Timeline: response, treatment, AEs and con meds per patient
#'
#' One row per patient on a study-day axis, with any combination of layers:
#' the response (ADRS overall response by visit, first progression marked),
#' treatment (ADEX dose periods), discontinuation and death (a two-letter
#' tag for why treatment stopped at the last dose, one for why the patient
#' left the study at the end of study, a cross at death), adverse events
#' (ADAE) and con meds (ADCM). Each layer is a stripe on the patient row; a
#' click opens the row into one row per AE term and con med, each layer
#' under a small title when both open. Treatment and the response stay on
#' the patient row, their detail in the tooltips; with only those layers
#' ticked a patient does not open.
#'
#' A new block draws the AEs and discontinuation and death, with no listing
#' columns, the patients in ID order. AEs take the severity column the
#' board's "study_roles" option declares, as the patient profile does, and
#' both AEs and con meds can be narrowed to picked terms at any coding level.
#' Colours follow the board's scale map.
#'
#' @param summaries The listing columns and the timeline; see
#'   [blockr.viz::new_table_block()]. `NULL`: the timeline alone.
#' @param open Patient rows at start: `"none"` or `"all"` open.
#' @param script_values The settings, by name; see
#'   [patient_timeline_frame()] for each.
#' @param title,subtitle,caption Text above and below the block.
#' @param drill,drill_target,drill_table A click on a patient selects it, which
#'   filters the board's drill filter on ADSL by default. `drill_target = ""`
#'   keeps the click on the block, where it narrows the AE records under the
#'   table icon; see [blockr.viz::new_table_block()]. `drill = NULL`: clicks
#'   do nothing.
#' @param search,download The search field and the download button.
#' @param ... The settings by name, as in `script_values` (`layers`,
#'   `ae_terms`, `ae_colour`, ...), and arguments for
#'   [blockr.core::new_block()].
#' @return A table block of class `timeline_block`.
#' @examplesIf interactive()
#' new_timeline_block(layers = c("Response", "Treatment"))
#' @export
new_timeline_block <- function(summaries = NULL,
                               open = "none",
                               script_values = list(),
                               title = "Timeline",
                               subtitle = patient_timeline_subtitle(),
                               caption = getOption(
                                 "blockr.viz.default_caption"
                               ),
                               drill = "auto",
                               drill_target = "auto",
                               drill_table = "adsl",
                               search = TRUE,
                               download = TRUE,
                               ...) {
  dots <- table_state_drop(list(...), new_timeline_block)
  dots$doses <- NULL
  own <- intersect(names(dots), PT_SETTINGS)
  script_values[own] <- dots[own]
  # Drug lines draw the records as they are; the old choice of how to show
  # cycles has nothing left to choose.
  script_values$doses <- NULL
  sv <- tl_colour_values_migrate(script_values)
  if (!is.null(sv$layers)) {
    sv$layers <- intersect(PT_LAYERS, as.character(unlist(sv$layers)))
  }
  # A saved board keeps the subtitle it was saved with; an untouched
  # default of an earlier version becomes this version's, so new words
  # (the drugs) reach it. An edited subtitle stays as it is.
  if (is.character(subtitle) && length(subtitle) == 1L &&
        subtitle %in% PT_OLD_SUBTITLES) {
    subtitle <- patient_timeline_subtitle()
  }
  if (is.null(summaries)) summaries <- patient_timeline_summaries()
  summaries <- pt_columns_clean(summaries)
  # An empty "Group by" is the default, the first choice.
  if (!length(sv$group) || !any(nzchar(as.character(unlist(sv$group))))) {
    sv$group <- NULL
  }
  sv$sort_col <- pt_sort_col(summaries)
  dots[c(own, "ctor", "ctor_pkg", "class", "gear", "by", "layout",
         "group_rows", "sort_by", "missing", "row_sections",
         "row_groups")] <- NULL
  if (is.null(dots$block_name)) dots$block_name <- "Timeline"
  do.call(blockr.viz::new_table_block, c(
    list(
      layout = "groups",
      by = c("USUBJID", "Row"),
      group_rows = "summary",
      row_sections = ".section",
      row_groups = ".arm",
      open = match.arg(open, c("none", "all")),
      summaries = summaries,
      missing = "drop",
      sort_by = "data",
      title = title,
      subtitle = subtitle,
      caption = caption,
      drill = drill,
      drill_target = drill_target,
      drill_table = drill_table,
      search = search,
      download = download,
      script = patient_timeline_script(),
      script_values = sv
    ),
    dots,
    list(gear = patient_timeline_gear(), class = "timeline_block",
         ctor = "new_timeline_block", ctor_pkg = "blockr.pharma")
  ))
}

# The timeline's settings: its prepare script's values.
PT_SETTINGS <- c(
  "layers", "group", "response", "ae_terms", "ae_records", "ae_show",
  "ae_colour", "ae_lanes", "serious", "cm_terms", "cm_records", "cm_colour",
  "cm_lanes", "cm_prior", "treatment_as", "drugs", "dose_tags",
  "trt_colour", "disc_show", "origin", "cutoff", "day_from", "day_to",
  "ongoing", "sort_col"
)

#' The patient timeline's prepare script
#'
#' Declares the settings, which the gear shows as rows, then calls
#' [patient_timeline_frame()] on the input dm with the board's study roles
#' ([patient_timeline_roles()]).
#'
#' @return The script, one string.
#' @export
patient_timeline_script <- function() {
  # A lane setting names the layer switch it shows under: a row of the
  # gear's Lanes tab and a word of the subtitle only while its layer is drawn.
  when <- function(on) if (is.null(on)) "" else paste0(", when = \"", on, "\"")
  sel <- function(name, value, levels, label, multiple = FALSE, on = NULL) {
    paste0(name, " <- factor(", chr_code(value), ", levels = ",
           chr_code(levels), ")  #| select(label = \"", label, "\"",
           if (multiple) ", multiple = TRUE", "", when(on), ")")
  }
  flag <- function(name, value, label, on = NULL) {
    paste0(name, " <- ", value, "  #| flag(label = \"", label, "\"",
           when(on), ")")
  }
  args <- setdiff(PT_SETTINGS, "sort_col")
  paste(
    sel("layers", c("Adverse events", "Discontinuation and death"),
        PT_LAYERS, "Layers", multiple = TRUE),
    "rs_on <- \"Response\" %in% layers",
    "trt_on <- \"Treatment\" %in% layers",
    "ae_on <- \"Adverse events\" %in% layers",
    "cm_on <- \"Con meds\" %in% layers",
    "dth_on <- \"Discontinuation and death\" %in% layers",
    "roles <- blockr.pharma::patient_timeline_roles()",
    "gv <- blockr.pharma::patient_timeline_groups(data, roles)",
    paste0("group <- factor(gv[1], levels = gv)  #| select(label = ",
           "\"Group by\", labels = names(gv))"),
    paste0("sort_col <- \"\"  #| text(label = \"Sorted by\", ",
           "placeholder = \"patient\")"),
    "pv <- blockr.pharma::timeline_colour_fields(data, \"rs\")",
    paste0("response <- factor(pv[1], levels = pv)  #| select(label = ",
           "\"Colour by\", labels = names(pv), when = \"rs_on\")"),
    sel("ae_records", "treatment-emergent", TL_AE_RECORDS, "Which records",
        on = "ae_on"),
    sel("ae_show", "all AEs", TL_AE_SHOW, "Show", on = "ae_on"),
    "aef <- blockr.pharma::timeline_colour_fields(data, \"ae\", roles)",
    pt_colour_line("ae_colour", "aef[1]", "aef", "ae_on"),
    "ael <- blockr.pharma::timeline_lane_choices(data, \"ae\")",
    pt_lane_line("ae_lanes", "ael", "ae_on"),
    "aet <- blockr.pharma::patient_timeline_terms(data, \"ae\", ae_lanes)",
    pt_terms_line("ae_terms", "aet", "ae_on"),
    sel("serious", "outlined", TL_SERIOUS, "Serious AEs", on = "ae_on"),
    sel("cm_records", "active on treatment", TL_CM_RECORDS, "Which records",
        on = "cm_on"),
    "cmf <- blockr.pharma::timeline_colour_fields(data, \"cm\")",
    pt_colour_line("cm_colour", "\"before or on treatment\"", "cmf", "cm_on"),
    "cml <- blockr.pharma::timeline_lane_choices(data, \"cm\")",
    pt_lane_line("cm_lanes", "cml", "cm_on"),
    "cmt <- blockr.pharma::patient_timeline_terms(data, \"cm\", cm_lanes)",
    pt_terms_line("cm_terms", "cmt", "cm_on"),
    sel("cm_prior", "clipped at the start", TL_CM_PRIOR,
        "Started before the axis", on = "cm_on"),
    sel("treatment_as", "lines per drug", PT_TRT_DRAW, "Draw as",
        on = "trt_on"),
    "drv <- blockr.pharma::timeline_drug_choices(data)",
    paste0("drugs <- factor(drv, levels = drv)  #| select(label = \"Drugs\", ",
           "multiple = TRUE, when = \"trt_on\")"),
    flag("dose_tags", "FALSE", "Tag dose changes", on = "trt_on"),
    sel("trt_colour", "dose level", TL_TRT_COLOUR, "Colour by", on = "trt_on"),
    sel("disc_show", "both", PT_DISC_SHOW, "Reasons tagged", on = "dth_on"),
    sel("origin", "first dose", TL_ORIGIN, "Time from"),
    paste0("cutoff <- as.Date(NA)  #| date(label = \"Cutoff\", ",
           "placeholder = \"automatic\")"),
    paste0("day_from <- NA_real_  #| number(label = \"From study day\", ",
           "placeholder = \"first\")"),
    paste0("day_to <- NA_real_  #| number(label = \"To study day\", ",
           "placeholder = \"last\")"),
    sel("ongoing", "last contact", c("last contact", "data cutoff"),
        "No end runs to"),
    paste0("frame <- blockr.pharma::patient_timeline_frame(data, ",
           paste0(args, " = ", args, collapse = ", "),
           ", roles = roles, scale_map = blockr.pharma::timeline_scale_map())"),
    "# A column added here with one value per patient is a listing column.",
    "blockr.pharma::patient_timeline_sort(frame, sort_col)",
    sep = "\n"
  )
}

# A character vector as R code: c("a", "b").
chr_code <- function(x) {
  paste0("c(", paste0("\"", x, "\"", collapse = ", "), ")")
}

# The default subtitles of earlier versions, replaced on restore.
PT_OLD_SUBTITLES <- c(
  "{@layers} in study days",
  "{@layers} in study days[, AEs by {@ae_lanes}][, con meds by {@cm_lanes}]",
  paste0("{@layers} in study days[, AEs by {@ae_lanes}]",
         "[, con meds by {@cm_lanes}][, drugs: {@drugs}]"),
  paste0("{@layers} in study days {@day_from} to {@day_to}",
         "[, AEs by {@ae_lanes}][, con meds by {@cm_lanes}]",
         "[, drugs: {@drugs}]")
)

#' The timeline's sentence under the title
#'
#' The layers, the study days the axis runs over ("first" and "last" while
#' they are the data's), then the level the AEs and con meds open into, the
#' terms of that level shown ("all", or the picks), and the drugs drawn, each
#' only while that layer is drawn. Each is a word to click.
#'
#' @return A title template, see [blockr.viz::new_chart_block()].
#' @export
patient_timeline_subtitle <- function() {
  paste0("{@layers} in study days {@day_from} to {@day_to}",
         "[, AEs by {@ae_lanes}, showing {@ae_terms}]",
         "[, con meds by {@cm_lanes}, showing {@cm_terms}]",
         "[, drugs: {@drugs}]")
}

#' The timeline's gear
#'
#' @return A `viz_table_gear`. Its Patients tab groups the patients
#'   ("Group by") and picks the listing columns, in order: those of the
#'   timeline, then every column with one value per patient (ADSL and what
#'   the prepare script adds). The first sorts the patients.
#' @export
patient_timeline_gear <- function() {
  blockr.viz::table_gear(
    tabs = list(
      # What to draw comes first; the Lanes tab shows the settings of the
      # layers drawn only (the script's `when` switches).
      Data = list(Layers = "layers",
                  Time = c("origin", "cutoff", "day_from", "day_to",
                           "ongoing"),
                  Tables = "mapping"),
      Patients = list(Rows = "group", Columns = "columns"),
      Lanes = list(
        Response = "response",
        Treatment = c("treatment_as", "drugs", "dose_tags",
                      "trt_colour"),
        `Discontinuation and death` = "disc_show",
        `Adverse events` = c("ae_records", "ae_show",
                             "ae_colour", "ae_lanes", "serious"),
        `Con meds` = c("cm_records", "cm_colour", "cm_lanes",
                       "cm_prior"),
        `Open rows` = "open_rows",
        Timeline = c("lane_width", "lane_track")
      ),
      Titles = "titles",
      Interaction = list(`Drill-down` = "drill",
                         `Reader tools` = c("search", "download")),
      Script = list(`Prepare script` = "script")
    ),
    columns = patient_timeline_columns(),
    info = patient_timeline_mapping,
    open = "Data",
    deps = list(pharma_gear_dep()),
    pick_by = "USUBJID",
    pick_skip = TL_FRAME_OWN,
    sort_value = "sort_col",
    columns_note = paste(
      "The first column sorts the patients. Columns with several values per",
      "patient (AE term, dose, visit) are not listed: make a per-patient",
      "value in the prepare script to show one."
    )
  )
}

# The listing columns the gear offers first, none of them on. The arm is the
# group heading; ADSL's columns, the age and sex among them, follow from the
# data.
patient_timeline_columns <- function() {
  sum_of <- function(col) {
    list(type = "simple", func = "sum", col = col, show = "number")
  }
  cols <- list(
    `Days on treatment` = list(type = "simple", func = "max",
                               col = ".days_trt", show = "number"),
    `Best response` = list(type = "field", col = ".bor"),
    Status = list(type = "field", col = ".status"),
    AEs = sum_of(".ae"),
    Ongoing = sum_of(".ae_open"),
    Serious = sum_of(".ae_ser"),
    `Worst AE` = list(
      type = "expr",
      expr = paste0("if (all(is.na(.worst))) \"\" else ",
                    "levels(.worst)[max(as.integer(.worst), na.rm = TRUE)]")
    ),
    `Con meds` = sum_of(".cm"),
    `New on treatment` = sum_of(".cm_new")
  )
  lapply(cols, function(s) {
    s$on <- FALSE
    s
  })
}

#' @rdname patient_timeline_gear
#' @return `patient_timeline_summaries()`: the summaries of a new timeline,
#'   the timeline alone with no listing columns.
#' @export
patient_timeline_summaries <- function() {
  list(patient_timeline_lane())
}

# The spans column: a stripe per layer on the patient row, cut to the worst
# level where records overlap, reason tags, the drugs as thin lines and the
# response, AE and con-med stripes as full bars under them. The axis reads
# in study days as the patient profile's does: D1, no day 0.
patient_timeline_lane <- function() {
  list(type = "spans", name = "Study time", x = "Start", xend = "End",
       color = "Colour", label = ".label",
       tooltip = c("Type", "Details", "Days"), lane = ".lane",
       open = ".open", seg_style = ".style",
       event_x = ".event_x", event_shape = "Event", overlap = "worst",
       parent_lanes = TRUE, event_tag = ".tag",
       event_tag_group = ".tag_group", lane_full = ".full",
       lane_size = "md", x_unit = "Day", x_days = TRUE)
}

# What the patient timeline reads, per role (see pharma_mapping()).
PT_ROLES <- list(
  list(role = "Patients", table = "adsl", need = TRUE,
       cols = list("USUBJID", "TRTSDT", "TRTEDT")),
  list(role = "Adverse events", table = "adae", need = FALSE,
       cols = list("USUBJID", c("AEDECOD", "AETERM"), c("ASTDY", "ASTDT"))),
  list(role = "Con meds", table = "adcm", need = FALSE,
       cols = list("USUBJID", c("CMDECOD", "CMTRT"), c("ASTDY", "ASTDT"))),
  list(role = "Treatment", table = "adex", need = FALSE,
       cols = list("USUBJID", "EXTRT", c("ASTDT", "ASTDY"))),
  list(role = "Response", table = "adrs", need = FALSE,
       cols = list("USUBJID", "PARAMCD", "AVALC", c("ADT", "ADTM")))
)

#' What the patient timeline finds in its input
#'
#' @param data The block's input dm.
#' @return A list: `roles` (role, table, found, columns, missing, need)
#'   and `note`.
#' @export
patient_timeline_mapping <- function(data) {
  list(roles = pharma_mapping(data, PT_ROLES),
       note = paste("Names match without case. The AE severity column and",
                    "the arm come from the board's study roles when it",
                    "declares them."))
}

# A track's "Colour by" row: a select of the fields `pool` names (a helper
# line of timeline_colour_fields()), showing their labels, storing their
# names.
pt_colour_line <- function(name, value, pool, on) {
  paste0(name, " <- factor(", value, ", levels = ", pool, ")  #| select(",
         "label = \"Colour by\", labels = names(", pool, "), when = \"", on,
         "\")")
}

# A track's "showing" word: several of the terms of the level its rows open
# into (patient_timeline_terms() with a level), each with its patient count
# in the menu. Nothing picked reads "all".
pt_terms_line <- function(name, pool, on) {
  paste0(name, " <- factor(character(), levels = ", pool, ")  #| select(",
         "label = \"Showing\", multiple = TRUE, labels = attr(", pool,
         ", \"labels\"), notes = attr(", pool, ", \"notes\"), ",
         "placeholder = \"all\", when = \"", on, "\")")
}

# A track's "Open into one row per" row: a select of the term columns `pool`
# names (a helper line of timeline_lane_choices()), showing their labels,
# storing the column. The first choice the data has that is the default.
pt_lane_line <- function(name, pool, on) {
  paste0(name, " <- factor(", pool, "[attr(", pool, ", \"default\")], ",
         "levels = ", pool, ")  #| select(label = \"Open into one row per\", ",
         "labels = names(", pool, "), when = \"", on, "\")")
}

# The listing columns as the picker holds them: the arm goes (the group
# heading names it), and a field over an ADSL column is optional (a study
# without the column leaves it out). The timeline column takes this
# version's settings, so a saved board draws what a new block draws; its
# name and lane size stay as saved.
pt_columns_clean <- function(summaries) {
  if (!is.list(summaries)) return(summaries)
  summaries <- Filter(function(s) {
    !(is.list(s) && identical(s$type, "field") && identical(s$col, ".arm"))
  }, summaries)
  lapply(summaries, function(s) {
    if (is.list(s) && identical(s$type, "spans")) return(pt_lane_refresh(s))
    if (is.list(s) && identical(s$type, "field") && is.character(s$col) &&
          !startsWith(s$col, ".")) {
      s$optional <- TRUE
    }
    s
  })
}

# A saved timeline column with the settings of patient_timeline_lane(),
# keeping the ones a user can change.
pt_lane_refresh <- function(s) {
  lane <- patient_timeline_lane()
  own <- setdiff(names(lane), c("name", "lane_size"))
  s[own] <- lane[own]
  missing <- setdiff(names(lane), names(s))
  s[missing] <- lane[missing]
  s
}

# The script value the gear sets to the first listing column: its name for
# one of the timeline's, the data column for a field over one, else empty.
pt_sort_col <- function(summaries) {
  own <- names(patient_timeline_columns())
  for (s in summaries) {
    if (!is.list(s)) next
    nm <- as.character(s$name %||% "")[1L]
    if (nm %in% own) return(nm)
    if (identical(s$type, "field") && is.character(s$col)) return(s$col)
    return("")
  }
  ""
}
