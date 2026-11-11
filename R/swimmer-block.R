# The efficacy swimmer, built from blockr.viz's table block.
#
# One row per patient (grouped by arm), listing columns beside a swimlane:
# the response bar at full height with its events, a thin line per drug
# above it. The table block's prepare step, swimmer_frame(), reads the ADaM
# tables from the input dm and returns the long frame the spans column draws,
# so the board needs no prep blocks.
#
# The block is a table block of class swimmer_block that saves and restores
# under new_swimmer_block(). Its gear is its own (swimmer_gear()): the
# swimmer's settings, not the table's. Its arguments are the table settings
# that gear changes, so they are what a board saves; the swimmer's own
# settings (doses, drugs, cutoff, ...) are values of its prepare script, in
# `script_values`.

#' Efficacy swimmer
#'
#' One row per patient: the overall response over time as a bar with first
#' progression, serious AEs and death on it, and a thin line per drug above
#' it, with an arrow while the drug is ongoing at the cutoff. Rows grow with
#' the number of drugs; past the lanes setting, study drugs keep their lanes
#' and the rest fold into a grey "Other" lane. Pointing at a drug lists its
#' cycles (number, dose, study day, delay). Time is weeks (or days) since the
#' first dose.
#'
#' The input is a dm of ADaM tables as they are: ADSL, ADEX, ADRS (overall
#' response per visit, `PARAMCD == "OVR"`) and, optional, ADAE and ADCM. The
#' block computes study time, drugs ongoing to the cutoff, response periods
#' and the best overall response ([swimmer_frame()]).
#'
#' Its gear has the tabs Data (the tables found, the response parameter, the
#' cutoff), Patients (grouping, sort, listing columns), Lanes (drugs, doses,
#' con meds, events, time unit), Titles and Interaction. A setting ticked "In
#' the subtitle" (Titles) becomes a word readers click.
#'
#' Returns a table block ([blockr.viz::new_table_block()], groups layout) of
#' class `swimmer_block`.
#'
#' @param by The rows: the grouping column, then `USUBJID`. A single
#'   `"USUBJID"` does not group.
#' @param summaries The listing columns and the swimlane; see
#'   [blockr.viz::new_table_block()].
#' @param sort_by,sort_dir Row order: a listing column, and its direction.
#' @param script_values The swimmer's settings, by name: `doses`
#'   (`"Exceptions"`, `"Dots"` or `"Plain"`), `drugs`, `cutoff` (a date,
#'   `NA` for the last date in the data), `con_meds`, `max_lanes`, `events`,
#'   `unit` (`"weeks"` or `"days"`), `parameter` (the ADRS PARAMCD).
#' @param title,subtitle,caption Text above and below the block. The subtitle
#'   is a template whose `{@name}` words readers click.
#' @param drill,drill_target,drill_table A click on a patient filters another
#'   block; see [blockr.viz::new_table_block()].
#' @param search,download The search field and the download button.
#' @param ... The swimmer's settings by name (`doses = "Dots"`, as in
#'   `script_values`) and `group`, the grouping column; arguments for
#'   [blockr.core::new_block()] (`block_name`).
#' @return A table block of class `swimmer_block`.
#' @examplesIf interactive()
#' new_swimmer_block(doses = "Dots")
#' @export
new_swimmer_block <- function(by = c("ARM", "USUBJID"),
                              summaries = swimmer_summaries(),
                              sort_by = "Weeks",
                              sort_dir = "desc",
                              script_values = list(),
                              title = "Efficacy swimmer",
                              subtitle = swimmer_subtitle(),
                              caption = getOption("blockr.viz.default_caption"),
                              drill = NULL,
                              drill_target = "",
                              drill_table = "",
                              search = TRUE,
                              download = TRUE,
                              ...) {
  dots <- list(...)
  own <- intersect(names(dots), SWIMMER_SETTINGS)
  if (length(own)) {
    if ("doses" %in% own) {
      dots$doses <- match.arg(dots$doses, c("Exceptions", "Dots", "Plain"))
    }
    script_values[own] <- dots[own]
  }
  if (!is.null(dots$group)) by <- c(dots$group, "USUBJID")
  dots[c(own, "group", "ctor", "ctor_pkg", "class", "gear", "max_lanes")] <- NULL
  if (is.null(dots$block_name)) dots$block_name <- "Efficacy swimmer"
  do.call(blockr.viz::new_table_block, c(
    list(
      layout = "groups",
      by = by,
      summaries = summaries,
      missing = "drop",
      sort_by = sort_by,
      sort_dir = sort_dir,
      title = title,
      subtitle = subtitle,
      caption = caption,
      drill = drill,
      drill_target = drill_target,
      drill_table = drill_table,
      search = search,
      download = download,
      script = swimmer_script(),
      script_values = script_values
    ),
    dots,
    list(gear = swimmer_gear(), class = "swimmer_block",
         ctor = "new_swimmer_block", ctor_pkg = "blockr.pharma")
  ))
}

# The swimmer's own settings: its prepare script's values.
SWIMMER_SETTINGS <- c("doses", "drugs", "cutoff", "con_meds", "max_lanes",
                      "events", "unit", "parameter", "assess")

#' The swimmer's prepare script
#'
#' Declares the swimmer's settings, which its gear shows as rows: the doses
#' style, the drugs to draw (choices from the input's ADEX), the cutoff
#' (automatic until a date is set), con meds, the lanes before "Other", the
#' events, the time unit and the ADRS parameter. Then it calls
#' [swimmer_frame()] on the input dm.
#'
#' @param max_lanes Lanes before drugs fold into "Other", by default.
#' @return The script, one string.
#' @export
swimmer_script <- function(max_lanes = 4L) {
  paste(
    "lv <- blockr.pharma::swimmer_drugs(data)",
    "av <- blockr.pharma::swimmer_assessments(data)",
    paste0("assess <- factor(av[1], levels = av)  #| select(label = ",
           "\"Response assessments\", multiple = TRUE)"),
    paste0("cutoff <- as.Date(NA)  #| date(label = \"Cutoff\", ",
           "placeholder = \"automatic\")"),
    paste0("unit <- factor(\"weeks\", levels = c(\"weeks\", \"days\"))  ",
           "#| select(label = \"Time unit\")"),
    "drugs <- factor(lv, levels = lv)  #| select(label = \"Drugs drawn\")",
    paste0("doses <- factor(\"Exceptions\", levels = c(\"Exceptions\", ",
           "\"Dots\", \"Plain\"))  #| select(label = \"Doses on the drug lines\")"),
    "con_meds <- FALSE  #| flag(label = \"Con meds as lanes\")",
    paste0("max_lanes <- ", as.integer(max_lanes), "  #| number(min = 1, ",
           "max = 8, step = 1, label = \"Lanes before \\\"Other\\\"\")"),
    paste0("events <- factor(c(\"Progression\", \"Serious AE\", \"Death\"), ",
           "levels = c(\"Progression\", \"Serious AE\", \"Death\"))  ",
           "#| select(label = \"Events\")"),
    paste0("blockr.pharma::swimmer_frame(data, doses = doses, drugs = drugs, ",
           "cutoff = cutoff, con_meds = con_meds, max_lanes = max_lanes, ",
           "events = events, unit = unit, assess = assess)"),
    sep = "\n"
  )
}

#' The swimmer's sentence under the title
#'
#' Each `{@...}` word is a setting to click: the cutoff, the doses style and
#' the drugs, and the column grouping the rows (`@parent`, the table's outer
#' `by`). The grouping's clause leaves when the rows are not grouped. The
#' gear's "In the subtitle" ticks add and remove the clauses of
#' [swimmer_clauses()].
#'
#' @return A title template, see [blockr.viz::new_chart_block()].
#' @export
swimmer_subtitle <- function() {
  cl <- swimmer_clauses()
  paste0("Response and drugs by study {@unit}, ",
         paste(cl[c("doses", "drugs", "cutoff", "group")], collapse = ", "))
}

#' The clauses the swimmer's settings put in its subtitle
#'
#' Per setting, the text its "In the subtitle" tick adds, in sentence order.
#'
#' @return A named character vector.
#' @export
swimmer_clauses <- function() {
  c(
    assess = "response by {@assess}",
    doses = "doses shown as {@doses}",
    drugs = "drugs {@drugs}",
    con_meds = "[{@con_meds}]",
    events = "events {@events}",
    cutoff = "cutoff {@cutoff}",
    group = "[by {label(@parent in ARM, BOR, status)}]"
  )
}

#' The swimmer's gear
#'
#' Its tabs, rows, listing columns and groupings, as
#' [blockr.viz::table_gear()] takes them.
#'
#' @return A `viz_table_gear`.
#' @export
swimmer_gear <- function() {
  blockr.viz::table_gear(
    tabs = list(
      Data = list(Tables = c("mapping", "assess"), Time = c("cutoff", "unit")),
      Patients = list(Rows = c("group", "sort"), Columns = "columns"),
      Lanes = list(
        `Drug lines` = c("drugs", "doses", "con_meds", "max_lanes"),
        `Response bar` = "events"
      ),
      Titles = "titles",
      Interaction = list(`Drill-down` = "drill",
                         `Reader tools` = c("search", "download"))
    ),
    columns = swimmer_columns(),
    groups = c(Arm = "ARM", `Best response` = "BOR", Status = "status"),
    clauses = swimmer_clauses(),
    info = swimmer_mapping,
    open = "Data",
    deps = list(swimmer_dep())
  )
}

# The listing columns the gear offers, the first three shown.
swimmer_columns <- function() {
  list(
    BOR = list(type = "field", col = "BOR"),
    Status = list(type = "field", col = "status"),
    Weeks = list(type = "simple", func = "max", col = "weeks",
                 show = "number"),
    Age = list(type = "field", col = "AGE", on = FALSE),
    Sex = list(type = "field", col = "SEX", on = FALSE),
    `Dose level` = list(type = "field", col = "dose_level", on = FALSE),
    # A second and third assessment's best response, headed by its name.
    BOR2 = list(type = "field", col = "BOR_2", name = NA, on = FALSE,
                title = "BOR, 2nd assessment"),
    BOR3 = list(type = "field", col = "BOR_3", name = NA, on = FALSE,
                title = "BOR, 3rd assessment")
  )
}

#' @rdname swimmer_gear
#' @return `swimmer_summaries()`: the summaries of a new swimmer, the shown
#'   listing columns and the swimlane.
#' @export
swimmer_summaries <- function() {
  blockr.viz::table_gear_summaries(swimmer_gear(), list(swimmer_lane()))
}

swimmer_lane <- function() {
  list(type = "spans", name = "Study time", x = "start",
       xend = "end", color = "colour", label = "label",
       tooltip = c("days", "dose"), size = "lg",
       event_x = "event_wk", event_shape = "event", open = "ongoing",
       lane = "lane", lane_full = "full", seg_style = "mark", strip = "strip")
}

# The mapping editor's script (inst/js/swimmer-gear.js).
swimmer_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-pharma-swimmer-gear",
    version = utils::packageVersion("blockr.pharma"),
    src = system.file("js", package = "blockr.pharma"),
    script = "swimmer-gear.js"
  )
}

# What the swimmer reads, per role: the table and its columns, each a set of
# names any one of which will do. NA: an optional table.
SWIMMER_ROLES <- list(
  list(role = "Subjects", table = "adsl", need = TRUE,
       cols = list("USUBJID", c("ARM", "TRT01A", "ACTARM"), "TRTSDT")),
  list(role = "Doses", table = "adex", need = TRUE,
       cols = list("USUBJID", c("EXTRT", "CMDECOD"),
                   c("ASTDT", "EXSTDT", "ASTDTM"))),
  list(role = "Response", table = "adrs", need = TRUE,
       cols = list("USUBJID", "PARAMCD", c("ADT", "ADTM"), "AVALC")),
  list(role = "Serious AEs", table = "adae", need = FALSE,
       cols = list("AESER", c("ASTDT", "AESTDT"))),
  list(role = "Con meds", table = "adcm", need = FALSE,
       cols = list(c("CMDECOD", "CMTRT"), c("ASTDT", "CMSTDT")))
)

#' What the swimmer finds in its input
#'
#' Per role, whether its table is there and which columns it lacks, and the
#' ADRS parameters, for the gear's Tables row. Names match without case.
#'
#' @param data The swimmer's input dm.
#' @return A list: `roles` (role, table, found, columns, missing, need) and
#'   `parameters`.
#' @export
swimmer_mapping <- function(data) {
  tabs <- if (inherits(data, "dm")) dm::dm_get_tables(data) else data
  if (!is.list(tabs) || is.data.frame(tabs)) tabs <- list()
  names(tabs) <- tolower(names(tabs))
  roles <- lapply(SWIMMER_ROLES, function(r) {
    tb <- tabs[[r$table]]
    have <- if (is.null(tb)) character() else toupper(names(tb))
    miss <- vapply(r$cols, function(alt) {
      if (any(alt %in% have)) "" else alt[[1L]]
    }, "")
    list(role = r$role, table = toupper(r$table), found = !is.null(tb),
         need = r$need,
         columns = vapply(r$cols, `[[`, "", 1L),
         missing = if (is.null(tb)) character() else miss[nzchar(miss)])
  })
  adrs <- tabs[["adrs"]]
  pc <- if (!is.null(adrs)) swimmer_col(as.data.frame(adrs), "PARAMCD")
  list(roles = roles,
       parameters = sort(unique(as.character(pc[!is.na(pc)]))))
}
