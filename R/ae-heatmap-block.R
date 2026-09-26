# The AE heatmap, as the pharma surface over blockr.viz's matrix engine.
#
# blockr.viz::new_heatmap_block() owns the machinery (aggregation, renderer,
# header row, gear, downloads) and stays UNREGISTERED there -- the count-plus-
# worst-grade cell is a safety-review idea, so the block that appears in
# pickers and serializes into study boards lives here, with AE defaults and
# AE words. The engine draws every column it gets; the "top 25 terms" cap is
# this surface's prepare script, and its `top_n` is a word in the sentence.
# Same formals as the engine ctor on purpose: saved state keys must match
# the restoring constructor's arguments.

#' AE heatmap block
#'
#' Renders long adverse-event rows (one row per AE record) as a subject x
#' term matrix: the cell shows the AE occurrence count and is painted by
#' the worst grade -- the classic CDEx safety heatmap. A prepare script
#' keeps the `top_n` most frequent terms, and "Top 25" in the sentence under
#' the title is the control for it. Rows group by treatment arm and order by
#' AE burden within it.
#'
#' A thin constructor over [blockr.viz::new_heatmap_block()], which owns
#' the rendering; this one carries the AE defaults and is what study
#' boards serialize.
#'
#' @param row Column identifying a matrix row. Default `"USUBJID"`.
#' @param col Column identifying a matrix column. Default `"AEDECOD"`
#'   (the preferred term).
#' @param color Column whose worst level per cell drives the paint.
#'   Default `"AETOXGR"` (CTCAE grade, numeric 1-5); `"AESEV"` works when
#'   the word scale is what the study carries (ordered factors keep their
#'   own level order).
#' @param group Optional column grouping the rows, e.g. the treatment arm
#'   (`"TRT01A"`, `"ACTARM"`). Empty: no grouping.
#' @param top_n LEGACY. A board saved with it restores it as the script's
#'   `top_n`.
#' @param max_height LEGACY. The matrix scrolls with its panel.
#' @param title,subtitle Text above the matrix. The subtitle is the block's
#'   sentence; its `{@top_n}` word is the control for the cap.
#' @param script The prepare script. `NULL` (the default) builds the top-n
#'   script for `col`, see [ae_heatmap_script()]; `""` means none.
#' @param cell_numbers,drill,download,filter_column,filter_values,ctrl_target,ctrl_table,caption,values
#'   As in [blockr.viz::new_heatmap_block()].
#' @param ... Forwarded to the engine constructor.
#' @return A transform block of class `heatmap_block`.
#' @examplesIf interactive()
#' new_ae_heatmap_block(group = "TRT01A", drill = TRUE)
#' @export
new_ae_heatmap_block <- function(row = "USUBJID",
                                 col = "AEDECOD",
                                 color = "AETOXGR",
                                 group = character(),
                                 top_n = NULL,       # LEGACY: script value
                                 cell_numbers = TRUE,
                                 drill = FALSE,
                                 download = FALSE,
                                 filter_column = NULL,
                                 filter_values = NULL,
                                 max_height = NULL,  # LEGACY: panel scroll
                                 ctrl_target = "",
                                 ctrl_table = "",
                                 title = "Adverse events by subject",
                                 subtitle = ae_heatmap_subtitle(),
                                 caption = NULL,
                                 script = NULL,
                                 values = list(),
                                 ...) {
  # Serialization identity. NOT formals: the framework injects ctor /
  # ctor_pkg itself when it calls a constructor (registry harvest, board
  # restore), and a formal would collide with that injection and leak into
  # the serialized state. When the caller supplies nothing (a direct call,
  # like the cdex board source), stamp THIS constructor, so boards restore
  # through the pharma surface that prod has installed.
  args <- list(...)
  if (!"ctor" %in% names(args)) {
    args$ctor <- "new_ae_heatmap_block"
    args$ctor_pkg <- utils::packageName()
  }
  # Own leading class, set AT CONSTRUCTION (the engine's registry-metadata
  # lookup keys on class[1] inside new_block, so a post-hoc class prepend
  # would warn "no registry entry for heatmap_block" on every build). A
  # restore delivers `class` through the state payload, hence via `...`.
  if (!"class" %in% names(args)) {
    args$class <- "ae_heatmap_block"
  }
  # The default script counts the column the block was built with. A board
  # that saved a script (or "" for none) restores that instead.
  if (is.null(script)) {
    script <- ae_heatmap_script(col)
  }
  do.call(blockr.viz::new_heatmap_block, c(
    list(
      row = row, col = col, color = color, group = group,
      top_n = top_n, cell_numbers = cell_numbers,
      drill = drill, download = download,
      filter_column = filter_column, filter_values = filter_values,
      max_height = max_height,
      ctrl_target = ctrl_target, ctrl_table = ctrl_table,
      title = title, subtitle = subtitle, caption = caption,
      script = script, values = values
    ),
    args
  ))
}

#' The AE heatmap's default sentence
#'
#' "Top 25 Dictionary-Derived Term, painted by worst Standard Toxicity Grade,
#' by Actual Treatment": each `{@...}` word opens its setting, and a bracketed
#' clause leaves when its setting is empty.
#'
#' @return A title template, see [blockr.viz::new_chart_block()].
#' @export
ae_heatmap_subtitle <- function() {
  paste0(
    "[Top {@top_n} ]{label(@col)}",
    "[, painted by worst {label(@color)}]",
    "[, by {label(@group)}]"
  )
}

#' The AE heatmap's default prepare script
#'
#' Keeps the `top_n` most frequent values of `col`, counted over the event
#' rows. `top_n` is declared at the top, so it becomes a setting.
#'
#' @param col The column the matrix spreads across, e.g. `"AEDECOD"`.
#' @param top_n The default cap.
#' @return The script, one string.
#' @export
ae_heatmap_script <- function(col = "AEDECOD", top_n = 25L) {
  col <- as.character(col %||% character())
  if (!length(col) || !nzchar(col[[1L]])) {
    return("")
  }
  sym <- deparse(as.name(col[[1L]]), backtick = TRUE)
  paste(
    paste0("top_n <- ", as.integer(top_n), "  #| number(min = 1, max = 200)"),
    paste0("top <- dplyr::count(data, ", sym, ", sort = TRUE)"),
    paste0("dplyr::filter(data, ", sym, " %in% utils::head(top$", sym,
           ", top_n))"),
    sep = "\n"
  )
}

#' @noRd
ae_heatmap_arguments <- function() {
  new_arg_specs(
    row = new_arg_spec(
      "Column identifying a matrix row. Default \"USUBJID\".",
      example = "USUBJID",
      type = arg_string()
    ),
    col = new_arg_spec(
      paste0(
        "Column identifying a matrix column. Default \"AEDECOD\" (the ",
        "preferred term); \"AESOC\" for a body-system matrix."
      ),
      example = "AEDECOD",
      type = arg_string()
    ),
    color = new_arg_spec(
      paste0(
        "Column whose WORST level per subject x term paints the cell. ",
        "Default \"AETOXGR\" (CTCAE grade); \"AESEV\" for the word scale. ",
        "The cell keeps displaying the occurrence count -- two channels. ",
        "Empty paints by count instead."
      ),
      example = "AETOXGR",
      type = arg_string()
    ),
    group = new_arg_spec(
      paste0(
        "Optional column grouping the rows, e.g. the treatment arm ",
        "(\"TRT01A\", \"ACTARM\") -- each arm opens with a header row; ",
        "rows order arm-first then burden."
      ),
      example = "TRT01A",
      type = blockr.core::arg_string()
    ),
    cell_numbers = new_arg_spec(
      paste0(
        "Show the occurrence count in each cell (default true). false = ",
        "pure color heatmap."
      ),
      example = TRUE,
      type = blockr.core::arg_boolean()
    ),
    drill = new_arg_spec(
      paste0(
        "true = a row click filters downstream on the subject (click ",
        "again to clear). Default false."
      ),
      example = TRUE,
      type = blockr.core::arg_boolean()
    ),
    download = new_arg_spec(
      paste0(
        "Offer the matrix as a download (xlsx / html / pptx of the count ",
        "columns). Default false."
      ),
      example = TRUE,
      type = blockr.core::arg_boolean()
    ),
    ctrl_target = new_arg_spec(
      paste0(
        "BETA. Block id of a value filter block on the same board the ",
        "drill claim is also pushed to. Empty = off."
      ),
      example = "cohort_filter",
      type = arg_string()
    ),
    ctrl_table = new_arg_spec(
      "BETA. Only with ctrl_target: the dm table the claim applies to.",
      example = "adsl",
      type = arg_string()
    )
  )
}

#' @noRd
ae_heatmap_guidance <- function() {
  paste0(
    "Feed LONG adverse-event rows (one row per AE record, e.g. a flattened ",
    "adae joined to adsl), never a pre-pivoted matrix -- the block ",
    "aggregates itself: cell = occurrence count, paint = worst grade. Do ",
    "NOT feed a worst-grade-per-patient dedup upstream; that flattens ",
    "every count to 1. Typical wiring: after the AE local filter (or the ",
    "AE flatten), group by the arm column the study carries (TRT01A / ",
    "ACTARM). The data output is a PASSTHROUGH of the input rows, filtered ",
    "to the clicked subject when drill is on -- downstream blocks (the ",
    "patient profile) receive AE rows, not the matrix."
  )
}
