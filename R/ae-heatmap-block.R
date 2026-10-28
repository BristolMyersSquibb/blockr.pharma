# The AE heatmap, built from blockr.viz's table block.
#
# A heatmap of subjects by terms is a table block in the groups layout: one
# row per subject (grouped by arm), one column per term (a summary's
# `facet`), each cell a count drawn as a square tile painted by the worst
# grade (`fill`), in the compact look (`look = "compact"`). This
# constructor turns the AE heatmap's arguments into that call. It is kept
# so boards and code that name it keep working; the result is a table block
# and saves as one.

#' AE heatmap
#'
#' A subject x term matrix of adverse events: each cell shows the number of
#' events and is painted by the worst grade. Returns a
#' [blockr.viz::new_table_block()] in the groups layout with a tile summary;
#' the AE defaults (subject id, preferred term, grade, the top-25 prepare
#' script and the sentence under the title) are what this function adds.
#'
#' It is kept so boards saved with it restore; they save as a table block
#' from then on. New boards call [blockr.viz::new_table_block()] directly,
#' with [ae_heatmap_script()] and [ae_heatmap_subtitle()] (see the clinical
#' explorer example, `inst/examples/clinical-explorer.R`).
#'
#' @param row Column identifying a matrix row. Default `"USUBJID"`.
#' @param col Column identifying a matrix column. Default `"AEDECOD"`
#'   (the preferred term).
#' @param color Column whose worst level per cell paints it. Default
#'   `"AETOXGR"` (CTCAE grade); `"AESEV"` works when the study carries the
#'   word scale. Empty: cells are painted by their count.
#' @param group Optional column grouping the rows, e.g. the treatment arm.
#' @param top_n LEGACY. Restores as the prepare script's `top_n` value.
#' @param cell_numbers Show the count in each cell.
#' @param download Offer the table for download.
#' @param title,subtitle,caption Text above and below the matrix. The
#'   subtitle is the sentence whose `{@top_n}` word is the top-n control.
#' @param script The prepare script. `NULL` (the default) builds the top-n
#'   script for `col` ([ae_heatmap_script()]).
#' @param values The prepare script's values.
#' @param drill,filter_column,filter_values,max_height,ctrl_target,ctrl_table,locked_roles
#'   LEGACY, accepted so saved boards restore. A row click goes to the
#'   board's drill filter (`drill_target = "auto"`).
#' @param ... Forwarded to [blockr.viz::new_table_block()] (`block_name`, ...).
#' @return A table block (class `table_block`).
#' @examplesIf interactive()
#' new_ae_heatmap_block(group = "TRT01A")
#' @export
new_ae_heatmap_block <- function(row = "USUBJID",
                                 col = "AEDECOD",
                                 color = "AETOXGR",
                                 group = character(),
                                 top_n = NULL,
                                 cell_numbers = TRUE,
                                 drill = NULL,
                                 download = FALSE,
                                 filter_column = NULL,
                                 filter_values = NULL,
                                 max_height = NULL,
                                 ctrl_target = NULL,
                                 ctrl_table = NULL,
                                 title = "Adverse events by subject",
                                 subtitle = ae_heatmap_subtitle(),
                                 caption = NULL,
                                 script = NULL,
                                 values = list(),
                                 locked_roles = character(),
                                 ...) {
  one <- function(x) {
    x <- as.character(unlist(x %||% character()))
    x <- x[!is.na(x) & nzchar(x)]
    if (length(x)) x[[1L]]
  }
  row <- one(row) %||% "USUBJID"
  col <- one(col) %||% "AEDECOD"
  color <- one(color)
  group <- one(group)

  # The framework's channel (a restore passes the saved ctor and class):
  # the result is a table block and must save as one.
  args <- list(...)
  restore <- !is.null(args$ctor)
  args[c("ctor", "ctor_pkg", "class")] <- NULL
  # A restore also passes what the board saved beside the arguments
  # (blockr.dock's `visible`). Those go on the block as attributes, as
  # blockr.viz restores its own blocks; in code an unknown name still errors.
  extra <- list()
  if (restore) {
    known <- c(setdiff(names(formals(blockr.viz::new_table_block)), "..."),
               "block_name", "block_metadata")
    extra <- args[!names(args) %in% known]
    args <- args[names(args) %in% known]
  }

  # The default script counts the column the block was built with. A board
  # that saved a script (or "" for none) restores that instead.
  default_script <- is.null(script)
  if (default_script) script <- ae_heatmap_script(col)
  values <- if (is.list(values)) values else list()
  if (!is.null(top_n) && is.null(values$top_n)) {
    values$top_n <- as.integer(top_n)
  }
  # The sentence's "Top 25" reads the script's value; seed the default so it
  # shows before anyone changes it.
  if (default_script && is.null(values$top_n)) values$top_n <- 25L
  # A sentence saved before this block was a table names the old roles.
  if (is.character(subtitle) && length(subtitle) == 1L) {
    subtitle <- gsub("@col\\b", "@facet", subtitle, perl = TRUE)
    subtitle <- gsub("@color\\b", "@fill", subtitle, perl = TRUE)
  }

  summary <- c(
    list(type = "simple", func = "count", show = "tile", facet = col,
         name = "Events"),
    if (!is.null(color)) list(fill = color)
  )

  blk <- do.call(blockr.viz::new_table_block, c(
    list(
      layout = "groups",
      by = c(group, row),
      summaries = list(summary),
      look = "compact",
      value_labels = !isFALSE(cell_numbers),
      drill = "auto",
      drill_target = one(ctrl_target) %||% "auto",
      drill_table = one(ctrl_table) %||% "",
      download = isTRUE(download),
      title = title,
      subtitle = subtitle,
      caption = caption,
      script = script,
      script_values = values
    ),
    args,
    # Named outright: called through do.call(), blockr.core cannot read the
    # constructor off the call stack.
    list(ctor = "new_table_block", ctor_pkg = "blockr.viz")
  ))
  for (nm in names(extra)) attr(blk, nm) <- extra[[nm]]
  blk
}

#' The AE heatmap's default sentence
#'
#' "Top 25 Dictionary-Derived Term, painted by Standard Toxicity Grade, by
#' Actual Treatment": the `{@top_n}` word opens its setting, and a bracketed
#' clause leaves when its setting is empty.
#'
#' @return A title template, see [blockr.viz::new_chart_block()].
#' @export
ae_heatmap_subtitle <- function() {
  paste0(
    "[Top {@top_n} ]{label(@facet)}",
    "[, painted by {label(@fill)}]",
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
