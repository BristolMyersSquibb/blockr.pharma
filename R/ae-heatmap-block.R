# The AE heatmap, as the pharma surface over blockr.viz's matrix engine.
#
# blockr.viz::new_heatmap_block() owns the machinery (aggregation, renderer,
# toolbar, gear, downloads) and stays UNREGISTERED there -- the count-plus-
# worst-grade cell is a safety-review idea, so the block that appears in
# pickers and serializes into study boards lives here, with AE defaults and
# AE words. Same formals as the engine ctor on purpose: saved state keys
# must match the restoring constructor's arguments.

#' AE heatmap block
#'
#' Renders long adverse-event rows (one row per AE record) as a subject x
#' term matrix: the cell shows the AE occurrence count and is painted by
#' the worst grade -- the classic CDEx safety heatmap. Columns cap to the
#' top-N most frequent terms (the "Top n" toolbar slider); the cell counts
#' toggle off into a pure color heatmap. Rows group by treatment arm (a
#' rotated rail tile marks each arm) and order by AE burden within it.
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
#' @param top_n,cell_numbers,drill,download,filter_column,filter_values,max_height,ctrl_target,ctrl_table
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
                                 top_n = 25L,
                                 cell_numbers = TRUE,
                                 drill = FALSE,
                                 download = FALSE,
                                 filter_column = NULL,
                                 filter_values = NULL,
                                 max_height = "600px",
                                 ctrl_target = "",
                                 ctrl_table = "",
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
  do.call(blockr.viz::new_heatmap_block, c(
    list(
      row = row, col = col, color = color, group = group,
      top_n = top_n, cell_numbers = cell_numbers,
      drill = drill, download = download,
      filter_column = filter_column, filter_values = filter_values,
      max_height = max_height,
      ctrl_target = ctrl_target, ctrl_table = ctrl_table
    ),
    args
  ))
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
        "preferred term); \"AESOC\" for a body-system matrix. Columns cap ",
        "to the top_n most frequent."
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
        "(\"TRT01A\", \"ACTARM\") -- drawn as a rotated rail tile spanning ",
        "each arm, rows order arm-first then burden."
      ),
      example = "TRT01A",
      type = blockr.core::arg_string()
    ),
    top_n = new_arg_spec(
      "Cap on the term columns, most frequent first. Default 25.",
      example = 25L,
      type = blockr.core::arg_number()
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
