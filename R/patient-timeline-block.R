# The patient timeline: one block for what the swimmer and the timeline
# draw, built from blockr.viz's table block as both are.
#
# One row per patient with a stripe per layer drawn (response, treatment,
# AEs, con meds; discontinuation and death as marks); a click opens the
# patient into a row per response, drug, AE term and con med, on the same
# axis. The table block's prepare step, patient_timeline_frame(), reads the
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
#' click opens the row into one row per response, drug, AE term and con
#' med.
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
#' @param drill,drill_target,drill_table A click on a patient filters another
#'   block; see [blockr.viz::new_table_block()].
#' @param search,download The search field and the download button.
#' @param ... The settings by name, as in `script_values` (`layers`,
#'   `order_by`, `ae_terms`, ...), and arguments for
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
                                       drill = NULL,
                                       drill_target = "",
                                       drill_table = "",
                                       search = TRUE,
                                       download = TRUE,
                                       ...) {
  preset <- match.arg(preset)
  def <- PT_PRESETS[[preset]]
  dots <- table_state_drop(list(...), new_patient_timeline_block)
  own <- intersect(names(dots), PT_SETTINGS)
  script_values[own] <- dots[own]
  sv <- utils::modifyList(def$values, script_values)
  sv$layers <- intersect(PT_LAYERS, as.character(unlist(sv$layers)))
  if (is.null(summaries)) summaries <- patient_timeline_summaries(preset)
  if (is.null(title)) title <- def$title
  ctor <- paste0("new_", preset, "_timeline_block")
  dots[c(own, "ctor", "ctor_pkg", "class", "gear", "by", "layout",
         "group_rows", "sort_by", "missing", "preset")] <- NULL
  if (is.null(dots$block_name)) dots$block_name <- def$title
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
                                        drill = NULL,
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
                                      drill = NULL,
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
  "dose_labels", "max_lanes",
  "origin", "cutoff", "ongoing", "order_by"
)

# What a preset sets: layers, sort, title and the listing columns ticked.
PT_PRESETS <- list(
  efficacy = list(
    title = "Efficacy timeline",
    values = list(layers = c("Response", "Treatment",
                             "Discontinuation and death"),
                  order_by = "days on treatment"),
    columns = c("Arm", "Best response")
  ),
  safety = list(
    title = "Safety timeline",
    values = list(layers = c("Adverse events", "Discontinuation and death"),
                  order_by = "number of AEs"),
    columns = c("Arm", "AEs", "Worst AE")
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
  args <- PT_SETTINGS
  paste(
    sel("layers", c("Adverse events", "Discontinuation and death"),
        PT_LAYERS, "Layers", multiple = TRUE),
    paste0("group <- \"\"  #| text(label = \"Arm column (ADSL)\", ",
           "placeholder = \"the study's arm\")"),
    "pv <- blockr.pharma::swimmer_parameters(data)",
    paste0("response <- factor(pv[1], levels = pv)  #| select(label = ",
           "\"Response (ADRS PARAMCD)\")"),
    "aet <- blockr.pharma::patient_timeline_terms(data, \"ae\")",
    paste0("ae_terms <- factor(character(), levels = aet)  #| select(",
           "label = \"Only these AEs\", multiple = TRUE)"),
    sel("ae_records", "treatment-emergent", TL_AE_RECORDS, "Which AEs"),
    sel("ae_show", "all AEs", TL_AE_SHOW, "Show"),
    sel("ae_colour", "study severity", PT_AE_COLOUR, "Colour by"),
    sel("ae_lanes", "preferred term", names(TL_AE_LANES),
        "Open into one row per"),
    sel("serious", "outlined", TL_SERIOUS, "Serious AEs"),
    "cmt <- blockr.pharma::patient_timeline_terms(data, \"cm\")",
    paste0("cm_terms <- factor(character(), levels = cmt)  #| select(",
           "label = \"Only these con meds\", multiple = TRUE)"),
    sel("cm_records", "active on treatment", TL_CM_RECORDS, "Which con meds"),
    sel("cm_colour", "before or on treatment", TL_CM_COLOUR, "Colour by"),
    sel("cm_lanes", "coded name", names(TL_CM_LANES), "Open into one row per"),
    sel("cm_prior", "clipped at the start", TL_CM_PRIOR,
        "Started before the axis"),
    sel("treatment_as", "lines per drug", PT_TRT_DRAW, "Draw as"),
    sel("doses", "Gaps and cuts", PT_DOSES, "Doses shown as"),
    sel("trt_colour", "dose level", TL_TRT_COLOUR, "Bars coloured by"),
    flag("dose_labels", "TRUE", "Dose labels on the bars"),
    paste0("max_lanes <- 12  #| number(min = 1, max = 40, step = 1, ",
           "label = \"Rows before \\\"Other con meds\\\"\")"),
    sel("origin", "first dose", TL_ORIGIN, "Time from"),
    paste0("cutoff <- as.Date(NA)  #| date(label = \"Cutoff\", ",
           "placeholder = \"automatic\")"),
    sel("ongoing", "last contact", c("last contact", "data cutoff"),
        "No end runs to"),
    sel("order_by", "number of AEs", PT_ORDER, "Sort patients by"),
    paste0("blockr.pharma::patient_timeline_frame(data, ",
           paste0(args, " = ", args, collapse = ", "),
           ", roles = blockr.pharma::patient_timeline_roles())"),
    sep = "\n"
  )
}

#' The patient timeline's sentence under the title
#'
#' @return A title template, see [blockr.viz::new_chart_block()].
#' @export
patient_timeline_subtitle <- function() {
  "{@layers} in study days, sorted by {@order_by}"
}

#' The patient timeline's gear
#'
#' @param preset `"safety"` or `"efficacy"`: which listing columns are
#'   ticked.
#' @return A `viz_table_gear`.
#' @export
patient_timeline_gear <- function(preset = c("safety", "efficacy")) {
  preset <- match.arg(preset)
  blockr.viz::table_gear(
    tabs = list(
      Data = list(Tables = "mapping", Layers = "layers",
                  Time = c("origin", "cutoff", "ongoing")),
      Patients = list(Rows = c("order_by", "group"), Columns = "columns"),
      Lanes = list(
        Response = "response",
        Treatment = c("treatment_as", "doses", "trt_colour", "dose_labels"),
        `Adverse events` = c("ae_terms", "ae_records", "ae_show",
                             "ae_colour", "ae_lanes", "serious"),
        `Con meds` = c("cm_terms", "cm_records", "cm_colour", "cm_lanes",
                       "cm_prior"),
        `Open rows` = c("open_rows", "max_lanes"),
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
    deps = list(pharma_gear_dep())
  )
}

# The listing columns the gear offers, the preset's ticked.
patient_timeline_columns <- function(preset = "safety") {
  on <- PT_PRESETS[[preset]]$columns
  sum_of <- function(col) {
    list(type = "simple", func = "sum", col = col, show = "number")
  }
  cols <- list(
    Arm = list(type = "field", col = ".arm"),
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
    `New on treatment` = sum_of(".cm_new"),
    Age = list(type = "field", col = "AGE"),
    Sex = list(type = "field", col = "SEX")
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
  on <- Filter(function(s) isTRUE(s$on), cols)
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
