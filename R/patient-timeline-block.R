# The patient timeline: one block for what the swimmer and the timeline
# draw, built from blockr.viz's table block as both are.
#
# One row per patient with a stripe per layer drawn (response, treatment,
# AEs, con meds; discontinuation and death as marks); a click opens the
# patient into a row per AE term and con med, on the same axis, each layer
# under a small title. Treatment and the response do not open: their detail
# is in the tooltips. The table block's prepare step, patient_timeline_frame(), reads the
# ADaM tables from the input dm.
#
# Two presets build it: "efficacy" (response, treatment, discontinuation and
# death, longest on treatment first) and "safety" (AEs, discontinuation and
# death, most AEs first). Each preset is a constructor and a class of its
# own (new_efficacy_timeline_block(), new_safety_timeline_block()), so each
# has its own add-block menu entry; both save the layers they draw, so a
# saved board restores whatever the reader ticked.

#' Patient timeline: response, treatment, AEs and con meds
#'
#' One row per patient on a study-day axis, with any combination of layers:
#' the response (ADRS overall response by visit, first progression marked),
#' treatment (ADEX dose periods), discontinuation and death (a two-letter
#' tag for the reason at the last dose, a cross at death), adverse events
#' (ADAE) and con meds (ADCM). Each layer is a stripe on the patient row; a
#' click opens the row into one row per AE term and con med, each layer
#' under a small title when both open. Treatment and the response stay on
#' the patient row, their detail in the tooltips; with only those layers
#' ticked a patient does not open.
#'
#' AEs take the severity column the board's "study_roles" option declares,
#' as the patient profile does, and both AEs and con meds can be narrowed to
#' picked terms at any coding level. Colours follow the board's scale map.
#'
#' `new_efficacy_timeline_block()` and `new_safety_timeline_block()` are the
#' two presets of the add-block menu.
#'
#' @param preset `"safety"` or `"efficacy"`: the defaults of the layers, the
#'   sort, the listing columns and the title.
#' @param summaries The listing columns and the timeline; see
#'   [blockr.viz::new_table_block()]. `NULL`: those of the preset.
#' @param open Patient rows at start: `"none"` or `"all"` open.
#' @param script_values The settings, by name; see
#'   [patient_timeline_frame()] for each.
#' @param title,subtitle,caption Text above and below the block. `NULL`
#'   title: the preset's.
#' @param drill,drill_target,drill_table A click on a patient selects it, which
#'   narrows the AE records under the table icon, or filters another block;
#'   see [blockr.viz::new_table_block()]. `NULL`: clicks do nothing.
#' @param search,download The search field and the download button.
#' @param ... The settings by name, as in `script_values` (`layers`,
#'   `ae_terms`, `ae_colour`, ...), and arguments for
#'   [blockr.core::new_block()].
#' @return A table block of class `patient_timeline_block`, with the
#'   preset's class first.
#' @examplesIf interactive()
#' new_patient_timeline_block("efficacy",
#'                            layers = c("Response", "Adverse events"))
#' @export
new_patient_timeline_block <- function(preset = c("safety", "efficacy"),
                                       summaries = NULL,
                                       open = "none",
                                       script_values = list(),
                                       title = NULL,
                                       subtitle = patient_timeline_subtitle(),
                                       caption = getOption(
                                         "blockr.viz.default_caption"
                                       ),
                                       drill = "auto",
                                       drill_target = "",
                                       drill_table = "",
                                       search = TRUE,
                                       download = TRUE,
                                       ...) {
  preset <- match.arg(preset)
  def <- PT_PRESETS[[preset]]
  dots <- table_state_drop(list(...), new_patient_timeline_block)
  dots[TL_RETIRED] <- NULL
  script_values[TL_RETIRED] <- NULL
  summaries <- tl_summaries_migrate(summaries)
  own <- intersect(names(dots), c(PT_SETTINGS, PT_RETIRED))
  script_values[own] <- dots[own]
  dots[PT_RETIRED] <- NULL
  sv <- utils::modifyList(def$values,
                          tl_colour_values_migrate(script_values))
  subtitle <- timeline_subtitle_migrate(subtitle)
  sv$layers <- intersect(PT_LAYERS, as.character(unlist(sv$layers)))
  if (is.null(summaries)) summaries <- patient_timeline_summaries(preset)
  summaries <- pt_columns_migrate(summaries, sv$order_by)
  sv$order_by <- NULL
  # An empty "Group by" is the default, the first choice.
  if (!length(sv$group) || !any(nzchar(as.character(unlist(sv$group))))) {
    sv$group <- NULL
  }
  sv$sort_col <- pt_sort_col(summaries)
  if (is.null(title)) title <- def$title
  ctor <- paste0("new_", preset, "_timeline_block")
  dots[c(own, "ctor", "ctor_pkg", "class", "gear", "by", "layout",
         "group_rows", "sort_by", "missing", "preset", "row_sections",
         "row_groups")] <- NULL
  if (is.null(dots$block_name)) dots$block_name <- def$title
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
    list(gear = patient_timeline_gear(preset),
         class = c(paste0(preset, "_timeline_block"),
                   "patient_timeline_block"),
         ctor = ctor, ctor_pkg = "blockr.pharma")
  ))
}

# The presets' constructors. A board saves a block by its constructor's
# arguments, so each takes all of them.
#' @rdname new_patient_timeline_block
#' @export
new_efficacy_timeline_block <- function(summaries = NULL,
                                        open = "none",
                                        script_values = list(),
                                        title = NULL,
                                        subtitle = patient_timeline_subtitle(),
                                        caption = getOption(
                                          "blockr.viz.default_caption"
                                        ),
                                        drill = "auto",
                                        drill_target = "",
                                        drill_table = "",
                                        search = TRUE,
                                        download = TRUE,
                                        ...) {
  new_patient_timeline_block(
    "efficacy", summaries = summaries, open = open,
    script_values = script_values, title = title, subtitle = subtitle,
    caption = caption, drill = drill, drill_target = drill_target,
    drill_table = drill_table, search = search, download = download, ...
  )
}

#' @rdname new_patient_timeline_block
#' @export
new_safety_timeline_block <- function(summaries = NULL,
                                      open = "none",
                                      script_values = list(),
                                      title = NULL,
                                      subtitle = patient_timeline_subtitle(),
                                      caption = getOption(
                                        "blockr.viz.default_caption"
                                      ),
                                      drill = "auto",
                                      drill_target = "",
                                      drill_table = "",
                                      search = TRUE,
                                      download = TRUE,
                                      ...) {
  new_patient_timeline_block(
    "safety", summaries = summaries, open = open,
    script_values = script_values, title = title, subtitle = subtitle,
    caption = caption, drill = drill, drill_target = drill_target,
    drill_table = drill_table, search = search, download = download, ...
  )
}

# The patient timeline's settings: its prepare script's values.
PT_SETTINGS <- c(
  "layers", "group", "response", "ae_terms", "ae_records", "ae_show",
  "ae_colour", "ae_lanes", "serious", "cm_terms", "cm_records", "cm_colour",
  "cm_lanes", "cm_prior", "treatment_as", "doses", "trt_colour",

  "origin", "cutoff", "ongoing", "sort_col"
)

# A setting a saved board may still carry: "Sort patients by" is now the
# first listing column (pt_columns_migrate()).
PT_RETIRED <- "order_by"

# What a preset sets: layers, title and the listing columns shown, the
# first of which sorts the patients (the gear offers the rest).
PT_PRESETS <- list(
  efficacy = list(
    title = "Efficacy timeline",
    values = list(layers = c("Response", "Treatment",
                             "Discontinuation and death")),
    columns = "Days on treatment"
  ),
  safety = list(
    title = "Safety timeline",
    values = list(layers = c("Adverse events", "Discontinuation and death")),
    columns = "AEs"
  )
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
  sel <- function(name, value, levels, label, multiple = FALSE) {
    paste0(name, " <- factor(", swimmer_levels(value), ", levels = ",
           swimmer_levels(levels), ")  #| select(label = \"", label, "\"",
           if (multiple) ", multiple = TRUE", "", ")")
  }
  flag <- function(name, value, label) {
    paste0(name, " <- ", value, "  #| flag(label = \"", label, "\")")
  }
  args <- setdiff(PT_SETTINGS, "sort_col")
  paste(
    sel("layers", c("Adverse events", "Discontinuation and death"),
        PT_LAYERS, "Layers", multiple = TRUE),
    "roles <- blockr.pharma::patient_timeline_roles()",
    "gv <- blockr.pharma::patient_timeline_groups(data, roles)",
    paste0("group <- factor(gv[1], levels = gv)  #| select(label = ",
           "\"Group by\", labels = names(gv))"),
    paste0("sort_col <- \"\"  #| text(label = \"Sorted by\", ",
           "placeholder = \"patient\")"),
    "pv <- blockr.pharma::timeline_colour_fields(data, \"rs\")",
    paste0("response <- factor(pv[1], levels = pv)  #| select(label = ",
           "\"Colour by\", labels = names(pv))"),
    "aet <- blockr.pharma::patient_timeline_terms(data, \"ae\")",
    paste0("ae_terms <- factor(character(), levels = aet)  #| select(",
           "label = \"Only these AEs\", multiple = TRUE)"),
    sel("ae_records", "treatment-emergent", TL_AE_RECORDS, "Which records"),
    sel("ae_show", "all AEs", TL_AE_SHOW, "Show"),
    "aef <- blockr.pharma::timeline_colour_fields(data, \"ae\", roles)",
    pt_colour_line("ae_colour", "aef[1]", "aef"),
    sel("ae_lanes", "preferred term", names(TL_AE_LANES), "One row per"),
    sel("serious", "outlined", TL_SERIOUS, "Serious AEs"),
    "cmt <- blockr.pharma::patient_timeline_terms(data, \"cm\")",
    paste0("cm_terms <- factor(character(), levels = cmt)  #| select(",
           "label = \"Only these con meds\", multiple = TRUE)"),
    sel("cm_records", "active on treatment", TL_CM_RECORDS, "Which records"),
    "cmf <- blockr.pharma::timeline_colour_fields(data, \"cm\")",
    pt_colour_line("cm_colour", "\"before or on treatment\"", "cmf"),
    sel("cm_lanes", "coded name", names(TL_CM_LANES), "One row per"),
    sel("cm_prior", "clipped at the start", TL_CM_PRIOR,
        "Started before the axis"),
    sel("treatment_as", "lines per drug", PT_TRT_DRAW, "Draw as"),
    sel("doses", "Gaps and cuts", PT_DOSES, "Doses shown as"),
    sel("trt_colour", "dose level", TL_TRT_COLOUR, "Colour by"),
    sel("origin", "first dose", TL_ORIGIN, "Time from"),
    paste0("cutoff <- as.Date(NA)  #| date(label = \"Cutoff\", ",
           "placeholder = \"automatic\")"),
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

#' The patient timeline's sentence under the title
#'
#' @return A title template, see [blockr.viz::new_chart_block()].
#' @export
patient_timeline_subtitle <- function() {
  "{@layers} in study days"
}

#' The patient timeline's gear
#'
#' @param preset `"safety"` or `"efficacy"`: which listing columns a new
#'   block shows.
#' @return A `viz_table_gear`. Its Patients tab groups the patients
#'   ("Group by") and picks the listing columns, in order: those of the
#'   timeline, then every column with one value per patient (ADSL and what
#'   the prepare script adds). The first sorts the patients.
#' @export
patient_timeline_gear <- function(preset = c("safety", "efficacy")) {
  preset <- match.arg(preset)
  blockr.viz::table_gear(
    tabs = list(
      Data = list(Tables = "mapping", Layers = "layers",
                  Time = c("origin", "cutoff", "ongoing")),
      Patients = list(Rows = "group", Columns = "columns"),
      Lanes = list(
        Response = "response",
        Treatment = c("treatment_as", "doses", "trt_colour"),
        `Adverse events` = c("ae_terms", "ae_records", "ae_show",
                             "ae_colour", "ae_lanes", "serious"),
        `Con meds` = c("cm_terms", "cm_records", "cm_colour", "cm_lanes",
                       "cm_prior"),
        `Open rows` = "open_rows",
        Timeline = c("lane_width", "lane_track")
      ),
      Titles = "titles",
      Interaction = list(`Drill-down` = "drill",
                         `Reader tools` = c("search", "download")),
      Script = list(`Prepare script` = "script")
    ),
    columns = patient_timeline_columns(preset),
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

# The listing columns the gear offers first, the preset's on. The arm is the
# group heading; ADSL's columns, the age and sex among them, follow from the
# data.
patient_timeline_columns <- function(preset = "safety") {
  on <- PT_PRESETS[[preset]]$columns
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
  Map(function(s, nm) {
    s$on <- nm %in% on
    s
  }, cols, names(cols))
}

#' @rdname patient_timeline_gear
#' @return `patient_timeline_summaries()`: the summaries of a new patient
#'   timeline, the preset's listing columns and the timeline.
#' @export
patient_timeline_summaries <- function(preset = c("safety", "efficacy")) {
  preset <- match.arg(preset)
  cols <- patient_timeline_columns(preset)
  on <- cols[PT_PRESETS[[preset]]$columns]
  c(unname(Map(function(s, nm) {
    s$on <- NULL
    s$name <- nm
    s
  }, on, names(on))), list(patient_timeline_lane()))
}

# The timeline's lane, with reason tags, the drugs as the swimmer's 5px lines
# and the response, AE and con-med stripes as full bars under them.
patient_timeline_lane <- function() {
  l <- timeline_lane()
  l$event_tag <- ".tag"
  l$lane_full <- ".full"
  l$lane_size <- "md"
  l$x_unit <- "Day"
  l
}

# What the patient timeline reads, per role (see pharma_mapping()). A
# function: the timeline's roles are defined in a file collated later.
pt_roles <- function() {
  c(TIMELINE_ROLES,
    list(list(role = "Response", table = "adrs", need = FALSE,
              cols = list("USUBJID", "PARAMCD", "AVALC", c("ADT", "ADTM")))))
}

#' What the patient timeline finds in its input
#'
#' @param data The block's input dm.
#' @return A list: `roles` and `note`, as [timeline_mapping()].
#' @export
patient_timeline_mapping <- function(data) {
  list(roles = pharma_mapping(data, pt_roles()),
       note = paste("Names match without case. The AE severity column and",
                    "the arm come from the board's study roles when it",
                    "declares them."))
}

# A track's "Colour by" row: a select of the fields `pool` names (a helper
# line of timeline_colour_fields()), showing their labels, storing their
# names.
pt_colour_line <- function(name, value, pool) {
  paste0(name, " <- factor(", value, ", levels = ", pool, ")  #| select(",
         "label = \"Colour by\", labels = names(", pool, "))")
}

# A saved board's listing columns as the picker holds them: the arm goes
# (the group heading names it), a field over an ADSL column is optional (a
# study without the column leaves it out), and the column the saved "Sort
# patients by" named, when it has one, comes first, added if it was not
# shown.
pt_columns_migrate <- function(summaries, order_by = NULL) {
  if (!is.list(summaries)) return(summaries)
  summaries <- Filter(function(s) {
    !(is.list(s) && identical(s$type, "field") && identical(s$col, ".arm"))
  }, summaries)
  summaries <- lapply(summaries, function(s) {
    if (is.list(s) && identical(s$type, "field") && is.character(s$col) &&
          !startsWith(s$col, ".")) {
      s$optional <- TRUE
    }
    s
  })
  ob <- as.character(unlist(order_by))
  col <- if (length(ob)) unname(PT_ORDER_COLUMNS[ob[[1L]]])
  if (!length(col) || is.na(col)) return(summaries)
  nm <- vapply(summaries, function(s) as.character(s$name %||% "")[1L], "")
  at <- which(nm == col)
  first <- if (length(at)) {
    summaries[at[[1L]]]
  } else {
    s <- patient_timeline_columns()[[col]]
    s$on <- NULL
    s$name <- col
    list(s)
  }
  c(first, if (length(at)) summaries[-at[[1L]]] else summaries)
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
