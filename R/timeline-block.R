# The timeline, built from blockr.viz's table block.
#
# One row per patient with a condensed stripe per source shown (adverse
# events, con meds, treatment); a click opens the patient into a row per AE
# term, con med and drug, on the same axis. The table block's prepare step,
# timeline_frame(), reads the ADaM tables from the input dm and returns the
# long frame the spans column draws, so the board needs no prep blocks.
#
# The table is the groups layout by patient and row, with summary outer rows
# (`group_rows = "summary"`). The spans column cuts overlaps to the worst
# level (`overlap = "worst"`) and draws its lanes on the patient rows too
# (`parent_lanes`). The block is a table block of class timeline_block that
# saves and restores under new_timeline_block(); its gear is its own
# (timeline_gear()).

#' Timeline of adverse events, con meds and treatment
#'
#' One row per patient: a condensed stripe per source shown, each the worst
#' level active that day, with serious AEs outlined and death as a cross. A
#' click on the row opens it into one row per AE term, con med and drug,
#' with their own counts. What is shown is any mix of adverse events (ADAE),
#' con meds (ADCM) and treatment (ADEX dose periods, else first to last
#' dose); AEs and treatment by default. An event without an end is ongoing
#' and runs, open, to last contact or the cutoff.
#'
#' The input is a dm of ADaM tables as they are, or named as the patient
#' profile allows (`dm`, `ae`, `cm`, `ex`; SDTM columns). The block computes
#' study days, ongoing ends and the listing columns ([timeline_frame()]).
#'
#' Its gear has the tabs Data (the tables found, what is shown, time),
#' Patients (order, listing columns), Lanes (per source, open rows, width),
#' Titles, Interaction and Script.
#'
#' @param summaries The listing columns and the timeline; see
#'   [blockr.viz::new_table_block()]. `NULL`: the columns of what is shown
#'   ([timeline_summaries()]).
#' @param open Patient rows at start: `"none"` open or `"all"` open.
#' @param script_values The timeline's settings, by name; see
#'   [timeline_frame()] for each.
#' @param title,subtitle,caption Text above and below the block. The subtitle
#'   is a template whose `{@name}` words readers click.
#' @param drill,drill_target,drill_table A click on a patient filters another
#'   block; see [blockr.viz::new_table_block()].
#' @param search,download The search field and the download button.
#' @param ... The timeline's settings by name, as in `script_values`: `show`
#'   (any of `"Adverse events"`, `"Con meds"`, `"Treatment"`; AEs and
#'   treatment by default), `group` (the arm column), `cm_colour =
#'   "class"`, ...; and arguments for [blockr.core::new_block()]
#'   (`block_name`). Time is in study days.
#' @return A table block of class `timeline_block`.
#' @examplesIf interactive()
#' new_timeline_block(show = c("Con meds", "Treatment"))
#' @export
new_timeline_block <- function(summaries = NULL,
                               open = "none",
                               script_values = list(),
                               title = "Timeline",
                               subtitle = timeline_subtitle(),
                               caption = getOption(
                                 "blockr.viz.default_caption"
                               ),
                               drill = NULL,
                               drill_target = "",
                               drill_table = "",
                               search = TRUE,
                               download = TRUE,
                               ...) {
  dots <- table_state_drop(list(...), new_timeline_block)
  # Boards saved while time could be in weeks, or with settings since
  # removed (TL_RETIRED).
  dots[c("unit", TL_RETIRED)] <- NULL
  script_values[c("unit", TL_RETIRED)] <- NULL
  summaries <- tl_summaries_migrate(summaries)
  own <- intersect(names(dots), TIMELINE_SETTINGS)
  script_values[own] <- dots[own]
  script_values <- timeline_migrate(script_values)
  subtitle <- timeline_subtitle_migrate(
    sub("by study {@unit}", "in study days", subtitle, fixed = TRUE)
  )
  if (!is.null(script_values$show)) {
    script_values$show <- intersect(TL_SHOW,
                                    as.character(unlist(script_values$show)))
  }
  if (is.null(summaries)) {
    summaries <- timeline_summaries(script_values$show %||%
                                      c("Adverse events", "Treatment"))
  }
  dots[c(own, "ctor", "ctor_pkg", "class", "gear", "by", "layout",
         "group_rows", "sort_by", "missing")] <- NULL
  if (is.null(dots$block_metadata)) {
    dots$block_metadata <- retired_block_metadata(
      "Timeline",
      "One row per patient: adverse events, con meds and treatment over time."
    )
  }
  if (is.null(dots$block_name)) dots$block_name <- "Timeline"
  do.call(blockr.viz::new_table_block, c(
    list(
      layout = "groups",
      by = c("USUBJID", "Row"),
      group_rows = "summary",
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
      script = timeline_script(),
      script_values = script_values
    ),
    dots,
    list(gear = timeline_gear(), class = "timeline_block",
         ctor = "new_timeline_block", ctor_pkg = "blockr.pharma")
  ))
}

# Settings since removed, which saved boards may still carry: the dose labels
# on the treatment bars and the row cap before "Other con meds".
TL_RETIRED <- c("dose_labels", "max_lanes")

# Saved listing columns and swimlane without what the swimlane no longer
# reads (the dose labels' `bar_label`).
tl_summaries_migrate <- function(summaries) {
  if (!is.list(summaries)) return(summaries)
  lapply(summaries, function(s) {
    # Overlapping episodes of one term no longer stack on two bars; the AE
    # records under the table icon list them.
    if (is.list(s)) s[c("bar_label", "inner_overlap")] <- NULL
    s
  })
}

# The timeline's own settings: its prepare script's values.
TIMELINE_SETTINGS <- c(
  "show", "group", "ae_records", "ae_show", "ae_colour", "ae_lanes",
  "serious", "death", "cm_records", "cm_colour", "cm_lanes", "cm_prior",
  "trt_colour", "origin", "cutoff", "ongoing",
  "order_by"
)

# Saved values of settings that changed: the old con-med colour default, and
# a board sorted by AEs that shows none (the old default sort) sorts by the
# con meds it shows.
timeline_migrate <- function(sv) {
  sv <- tl_colour_values_migrate(sv)
  show <- as.character(unlist(sv$show %||% c("Adverse events", "Treatment")))
  if (identical(as.character(unlist(sv$order_by)), "number of AEs") &&
        !"Adverse events" %in% show) {
    sv$order_by <- "number of events"
  }
  sv
}

# A saved subtitle without its sort clause: the table sorts by a click on a
# column header, so the order is not a word on the face.
timeline_subtitle_migrate <- function(subtitle) {
  if (!is.character(subtitle) || length(subtitle) != 1L) return(subtitle)
  out <- sub("^ *sorted by \\{@order_by\\},? *", "", subtitle)
  gsub(",? *sorted by \\{@order_by\\}", "", out)
}

#' The timeline's prepare script
#'
#' Declares the timeline's settings, which its gear shows as rows, then calls
#' [timeline_frame()] on the input dm.
#'
#' @return The script, one string.
#' @export
timeline_script <- function() {
  sel <- function(name, value, levels, label) {
    paste0(name, " <- factor(", swimmer_levels(value), ", levels = ",
           swimmer_levels(levels), ")  #| select(label = \"", label, "\")")
  }
  flag <- function(name, value, label) {
    paste0(name, " <- ", value, "  #| flag(label = \"", label, "\")")
  }
  args <- TIMELINE_SETTINGS
  paste(
    sel("show", c("Adverse events", "Treatment"), TL_SHOW, "Draw"),
    "group <- \"TRT01A\"  #| text(label = \"Arm column (ADSL)\")",
    sel("ae_records", "treatment-emergent", TL_AE_RECORDS, "Which records"),
    sel("ae_show", "all AEs", TL_AE_SHOW, "Show"),
    paste0("aef <- blockr.pharma::timeline_colour_fields(data, \"ae\", ",
           "grade_first = TRUE)"),
    pt_colour_line("ae_colour", "aef[1]", "aef"),
    sel("ae_lanes", "preferred term", names(TL_AE_LANES), "One row per"),
    sel("serious", "outlined", TL_SERIOUS, "Serious AEs"),
    flag("death", "TRUE", "Death as a cross"),
    sel("cm_records", "active on treatment", TL_CM_RECORDS, "Which records"),
    "cmf <- blockr.pharma::timeline_colour_fields(data, \"cm\")",
    pt_colour_line("cm_colour", "\"before or on treatment\"", "cmf"),
    sel("cm_lanes", "coded name", names(TL_CM_LANES), "One row per"),
    sel("cm_prior", "clipped at the start", TL_CM_PRIOR,
        "Started before the axis"),
    sel("trt_colour", "dose level", TL_TRT_COLOUR, "Colour by"),
    sel("origin", "first dose", TL_ORIGIN, "Time from"),
    paste0("cutoff <- as.Date(NA)  #| date(label = \"Cutoff\", ",
           "placeholder = \"automatic\")"),
    sel("ongoing", "last contact", c("last contact", "data cutoff"),
        "No end runs to"),
    sel("order_by", "number of events", TL_ORDER, "Sort patients by"),
    paste0("blockr.pharma::timeline_frame(data, ",
           paste0(args, " = ", args, collapse = ", "),
           ", scale_map = blockr.pharma::timeline_scale_map())"),
    sep = "\n"
  )
}

#' The timeline's sentence under the title
#'
#' Each `{@...}` word is a setting to click. The gear's "In the subtitle"
#' ticks add and remove the clauses of [timeline_clauses()].
#'
#' @return A title template, see [blockr.viz::new_chart_block()].
#' @export
timeline_subtitle <- function() {
  cl <- timeline_clauses()
  paste0("{@show} in study days, one stripe per patient, ", cl[["cutoff"]])
}

#' The clauses the timeline's settings put in its subtitle
#'
#' Per setting, the text its "In the subtitle" tick adds, in sentence order.
#'
#' @return A named character vector.
#' @export
timeline_clauses <- function() {
  c(
    ae_records = "{@ae_records} AEs",
    ae_colour = "AEs by {@ae_colour}",
    cm_records = "con meds {@cm_records}",
    cm_colour = "con meds by {@cm_colour}",
    trt_colour = "treatment by {@trt_colour}",
    cutoff = "cutoff {@cutoff}"
  )
}

#' The timeline's gear
#'
#' Its tabs, rows and listing columns, as [blockr.viz::table_gear()] takes
#' them.
#'
#' @return A `viz_table_gear`.
#' @export
timeline_gear <- function() {
  blockr.viz::table_gear(
    tabs = list(
      Data = list(Tables = "mapping", Show = "show",
                  Time = c("origin", "cutoff", "ongoing")),
      Patients = list(Rows = c("order_by", "group"), Columns = "columns"),
      Lanes = list(
        `Adverse events` = c("ae_records", "ae_show", "ae_colour", "ae_lanes",
                             "serious", "death"),
        `Con meds` = c("cm_records", "cm_colour", "cm_lanes", "cm_prior"),
        Treatment = "trt_colour",
        `Open rows` = "open_rows",
        Timeline = "lane_width"
      ),
      Titles = "titles",
      Interaction = list(`Drill-down` = "drill",
                         `Reader tools` = c("search", "download")),
      Script = list(`Prepare script` = "script")
    ),
    columns = timeline_columns(),
    clauses = timeline_clauses(),
    info = timeline_mapping,
    open = "Data",
    deps = list(pharma_gear_dep())
  )
}

# The listing columns the gear offers, the ones of what is shown ticked.
# Counts are sums over the records, so a patient row counts all of a
# patient's and an opened row its own. The timeline needs the width, so few
# are ticked.
timeline_columns <- function(show = c("Adverse events", "Treatment")) {
  ae <- "Adverse events" %in% show
  cm <- "Con meds" %in% show
  sum_of <- function(col, on) {
    list(type = "simple", func = "sum", col = col, show = "number", on = on)
  }
  list(
    Arm = list(type = "field", col = ".arm"),
    AEs = sum_of(".ae", ae),
    Ongoing = sum_of(".ae_open", FALSE),
    Serious = sum_of(".ae_ser", FALSE),
    `Worst AE` = list(
      type = "expr", on = ae,
      expr = paste0("if (all(is.na(.worst))) \"\" else ",
                    "levels(.worst)[max(as.integer(.worst), na.rm = TRUE)]")
    ),
    `Con meds` = sum_of(".cm", cm),
    `New on treatment` = sum_of(".cm_new", cm && !ae),
    `Days on treatment` = list(type = "simple", func = "max",
                               col = ".days_trt", show = "number",
                               on = FALSE),
    Age = list(type = "field", col = "AGE", on = FALSE),
    Sex = list(type = "field", col = "SEX", on = FALSE)
  )
}

#' @rdname timeline_gear
#' @param show What is shown, as in [timeline_frame()]: its listing columns
#'   are ticked.
#' @return `timeline_summaries()`: the summaries of a new timeline, the
#'   ticked listing columns and the timeline.
#' @export
timeline_summaries <- function(show = c("Adverse events", "Treatment")) {
  cols <- timeline_columns(show)
  on <- Filter(function(s) !isFALSE(s$on), cols)
  c(unname(Map(function(s, nm) {
    s$on <- NULL
    s$name <- nm
    s
  }, on, names(on))), list(timeline_lane()))
}

timeline_lane <- function() {
  list(type = "spans", name = "Study time", x = "Start", xend = "End",
       color = "Colour", label = ".label",
       tooltip = c("Type", "Details", "Days"), lane = ".lane",
       open = ".open", seg_style = ".style",
       event_x = ".event_x", event_shape = "Event", overlap = "worst",
       parent_lanes = TRUE)
}

# What the timeline reads, per role (see pharma_mapping()).
TIMELINE_ROLES <- list(
  list(role = "Patients", table = "adsl", need = TRUE,
       cols = list("USUBJID", "TRTSDT", "TRTEDT")),
  list(role = "Adverse events", table = "adae", need = FALSE,
       cols = list("USUBJID", c("AEDECOD", "AETERM"), c("ASTDY", "ASTDT"))),
  list(role = "Con meds", table = "adcm", need = FALSE,
       cols = list("USUBJID", c("CMDECOD", "CMTRT"), c("ASTDY", "ASTDT"))),
  list(role = "Treatment", table = "adex", need = FALSE,
       cols = list("USUBJID", "EXTRT", c("ASTDT", "ASTDY")))
)

#' What the timeline finds in its input
#'
#' Per role, whether its table is there and which columns it lacks, for the
#' gear's Tables row. Names match without case; tables and columns may be
#' named as the patient profile allows.
#'
#' @param data The timeline's input dm.
#' @return A list: `roles` (role, table, found, columns, missing, need) and
#'   `note`.
#' @export
timeline_mapping <- function(data) {
  list(roles = pharma_mapping(data, TIMELINE_ROLES),
       note = paste("Names match without case. Without ADEX the treatment",
                    "is one bar from TRTSDT to TRTEDT."))
}
