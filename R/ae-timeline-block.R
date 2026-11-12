# The AE timeline: a viz table block in the groups layout, one row per
# patient, whose prepare script turns ADAE (and ADSL) into the records its
# spans column draws (ae_timeline_frame()). The block's options are the
# script's controls, so they sit in the table's prepare band and gear, and
# the sentence under the title reads them as `{@lanes}`. Like the AE heatmap
# it saves as a table block.

#' AE timeline block
#'
#' One row per patient: each adverse event is a bar from onset to
#' resolution by study day, coloured by severity or grade. Overlapping AEs
#' draw over each other on one thin lane per patient, as the gantt chart
#' draws them, or stack in lanes of their own. An AE without an end is
#' ongoing and runs, open, to the patient's last contact or the data cutoff.
#' Serious AEs carry a mark,
#' death a cross, and a line shows the treatment period. Listing columns
#' (number of AEs, ongoing, serious, worst grade) sit before the timeline
#' and sort the rows.
#'
#' The input is ADAE and ADSL as they are: a `dm` with `adae` and `adsl`,
#' or ADAE alone (ADaM puts the ADSL columns on it). A thin constructor over
#' [blockr.viz::new_table_block()] in the groups layout; the prepare script
#' is [ae_timeline_script()], whose controls are the block's options.
#'
#' @param group Column grouping the patients, e.g. the treatment arm
#'   (`"TRT01A"`). Empty: no grouping.
#' @param title,subtitle Text above the table. The subtitle is the block's
#'   sentence, [ae_timeline_subtitle()]; its words open the settings.
#' @param script The prepare script. `NULL` (the default) is
#'   [ae_timeline_script()].
#' @param values The script's settings, by name (see [ae_timeline_frame()]
#'   for what each does): `events`, `colour`, `lanes`, `ongoing`, `serious`,
#'   `order_by`, `treatment`, `treatment_emergent`.
#' @param download Offer the table as a download.
#' @param ... Forwarded to [blockr.viz::new_table_block()] (`block_name`,
#'   ...).
#' @return A table block (class `table_block`).
#' @examplesIf interactive()
#' new_ae_timeline_block(values = list(lanes = "by body system"))
#' @export
new_ae_timeline_block <- function(group = "TRT01A",
                                  title = "Adverse events over time",
                                  subtitle = ae_timeline_subtitle(),
                                  script = NULL,
                                  values = list(),
                                  download = TRUE,
                                  ...) {
  group <- as.character(unlist(group %||% character()))
  group <- group[!is.na(group) & nzchar(group)]
  group <- if (length(group)) group[[1L]]
  args <- list(...)
  args[c("ctor", "ctor_pkg", "class")] <- NULL
  values <- if (is.list(values)) values else list()
  if (is.null(script)) script <- ae_timeline_script(group)

  do.call(blockr.viz::new_table_block, c(list(
    layout = "groups",
    by = c(group, "USUBJID"),
    summaries = ae_timeline_summaries(),
    sort_by = "data",
    # Overlay records have no severity; they must not add a "(Missing)"
    # colour.
    missing = "drop",
    drill = "auto",
    download = isTRUE(download),
    title = title,
    subtitle = subtitle,
    script = script,
    script_values = values
  ), args,
  # Named outright: called through do.call(), blockr.core cannot read the
  # constructor off the call stack.
  list(ctor = "new_table_block", ctor_pkg = "blockr.viz")))
}

# The listing columns and the timeline. The listing values repeat on every
# record of a patient, so `max` reads them back.
ae_timeline_summaries <- function() {
  num <- function(col, name) {
    list(type = "simple", func = "max", col = col, show = "number",
         name = name)
  }
  list(
    num("AEs", "AEs"),
    num("Ongoing", "Ongoing"),
    num("Serious AEs", "Serious"),
    list(type = "field", col = "Worst", name = "Worst"),
    list(
      type = "spans", name = "Study day",
      x = "Start", xend = "End", color = "Severity",
      label = "Term", bar_label = ".bar_label",
      tooltip = c("Body system", "Severity", "Serious", "Study days",
                  "Outcome"),
      lane = ".lane", open = ".open", seg_style = ".style",
      band_x = ".trt_from", band_xend = ".trt_to",
      event_x = ".event_day", event_shape = "Event"
    )
  )
}

#' The AE timeline's default sentence
#'
#' "All AEs by study day, coloured by Severity, overlaps on one lane, serious
#' AEs outlined, terms on the bars", then "Ongoing AEs to last contact, cutoff automatic,
#' sorted by number of AEs, by Actual Treatment": each `{@...}` word is a
#' setting of the prepare script and opens it. The grouping word offers the
#' patient columns only.
#'
#' @return A title template, see [blockr.viz::new_chart_block()].
#' @export
ae_timeline_subtitle <- function() {
  paste0(
    "{@events} by study day, coloured by {@colour}, overlaps {@lanes}, ",
    "serious AEs {@serious}, terms {@term_labels}\\n",
    "Ongoing AEs to {@ongoing}, cutoff {@cutoff}, sorted by {@order_by}",
    "[, by {label(@parent in ", paste(AET_PATIENT_COLS, collapse = ", "),
    ")}]"
  )
}

#' The AE timeline's default prepare script
#'
#' Declares the block's settings at the top, so each becomes a control and
#' a word in the sentence, and hands them to [ae_timeline_frame()].
#'
#' @param group The column grouping the patients, or `NULL`.
#' @return The script, one string.
#' @export
ae_timeline_script <- function(group = "TRT01A") {
  sel <- function(name, value, levels, label) {
    paste0(name, " <- factor(", deparse(value), ", levels = ",
           paste(deparse(levels, width.cutoff = 500L), collapse = ""),
           ")  #| select(label = ", deparse(label), ")")
  }
  grp <- if (length(group) && nzchar(group[[1L]])) deparse(group[[1L]]) else
    "NULL"
  paste(
    sel("events", "All AEs", c("All AEs", "Serious AEs", "Ongoing AEs"),
        "Show"),
    sel("colour", "Severity", c("Severity", "Grade"), "Colour by"),
    sel("lanes", "on one lane", AET_LANES, "Overlaps"),
    sel("ongoing", "last contact", c("last contact", "data cutoff"),
        "No end runs to"),
    sel("serious", "outlined", AET_SERIOUS, "Serious AEs"),
    sel("order_by", "number of AEs", AET_ORDER, "Sort patients by"),
    sel("term_labels", "on the bars", AET_TERM_LABELS, "Term labels"),
    paste0("cutoff <- as.Date(NA)  #| date(label = \"Cutoff\", ",
           "placeholder = \"automatic\")"),
    "treatment <- TRUE  #| flag(label = \"Treatment period line\")",
    "treatment_emergent <- TRUE  #| flag(label = \"Treatment-emergent only\")",
    paste0(
      "blockr.pharma::ae_timeline_frame(data, events = events, ",
      "colour = colour, lanes = lanes, ongoing = ongoing, ",
      "serious = serious, order_by = order_by, term_labels = term_labels, ",
      "cutoff = cutoff, treatment = treatment, ",
      "treatment_emergent = treatment_emergent, group = ", grp, ")"
    ),
    sep = "\n"
  )
}
