# The AE heatmap, built from blockr.viz's table block.
#
# Deprecated. Boards saved between 2026-08-30 and 2026-10-26 name this
# constructor, and blockr.core finds a saved constructor in the namespace: a
# restore builds a table block, which saves as one, and tells the user to
# save the board. Code that calls it gets a deprecation warning. New boards
# call blockr.viz::new_table_block() with ae_heatmap_script() and
# ae_heatmap_subtitle() (see `inst/examples/clinical-explorer.R`).
#
# A heatmap of subjects by terms is a table block in the groups layout: one
# row per subject (grouped by arm), one column per term (a summary's
# `facet`), each cell a count drawn as a square tile painted by the worst
# grade (`fill`), in the compact look (`look = "compact"`).

#' AE heatmap (deprecated)
#'
#' Deprecated: build the table block directly with
#' [blockr.viz::new_table_block()], [ae_heatmap_script()] and
#' [ae_heatmap_subtitle()] (see the clinical explorer example,
#' `inst/examples/clinical-explorer.R`). Called from code it still returns
#' that table block, with a deprecation warning. Boards saved with it restore
#' as table blocks and save as one from then on.
#'
#' @param row Column identifying a matrix row. Default `"USUBJID"`.
#' @param col Column identifying a matrix column. Default `"AEDECOD"`.
#' @param color Column whose worst level per cell paints it. Default
#'   `"AETOXGR"`. Empty: cells are painted by their count.
#' @param group Optional column grouping the rows, e.g. the treatment arm.
#' @param top_n Restores as the prepare script's `top_terms` value.
#' @param cell_numbers Show the count in each cell.
#' @param download Offer the table for download.
#' @param title,subtitle,caption Text above and below the matrix.
#' @param script The prepare script. `NULL` builds the top-n script for `col`
#'   ([ae_heatmap_script()]).
#' @param values The prepare script's values.
#' @param drill,filter_column,filter_values,max_height,ctrl_target,ctrl_table,locked_roles
#'   Accepted so saved boards restore. `ctrl_target` and `ctrl_table` become
#'   the drill target; the others are ignored.
#' @param ... Forwarded to [blockr.viz::new_table_block()].
#' @return A table block (class `table_block`).
#' @keywords internal
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

  values <- if (is.list(values)) values else list()
  top_n <- if (!is.null(top_n)) as.integer(unlist(top_n))[1L]
  # The default script counts the column the block was built with. A board
  # that saved a script (or "" for none) restores that instead, unless it is
  # this block's own default from before its control was `top_terms`: `top_n`
  # is also the table's row cap, so the sentence's "Top 25" read the table's
  # unset value. That script is written anew.
  n_old <- ae_heatmap_old_script(script, col)
  if (!is.null(n_old)) script <- NULL
  if (is.null(script)) {
    script <- ae_heatmap_script(col)
    # The sentence's "Top 25" reads the script's value; seed it so it shows
    # before anyone changes it.
    values$top_terms <- values$top_terms %||% values$top_n %||% n_old %||%
      top_n %||% 25L
    values$top_n <- NULL
    if (is.character(subtitle) && length(subtitle) == 1L) {
      subtitle <- gsub("@top_n\\b", "@top_terms", subtitle, perl = TRUE)
    }
  } else if (!is.null(top_n) && is.null(values$top_n)) {
    values$top_n <- top_n
  }
  # A sentence saved before this block was a table names the old roles. The
  # table reads `@group` as its row column (`by`); the heatmap's group is the
  # outer `by`, which the table calls `parent`.
  if (is.character(subtitle) && length(subtitle) == 1L) {
    subtitle <- gsub("@col\\b", "@facet", subtitle, perl = TRUE)
    subtitle <- gsub("@color\\b", "@fill", subtitle, perl = TRUE)
    subtitle <- gsub("@group\\b", "@parent", subtitle, perl = TRUE)
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
  # Deprecated. Code that calls it gets a warning; a saved board that brings
  # it back in the app tells the board user instead. Goes with this
  # constructor.
  if (!restore) {
    warning(warningCondition(
      paste0(
        "new_ae_heatmap_block() is deprecated. Use ",
        "blockr.viz::new_table_block() with ae_heatmap_script() and ",
        "ae_heatmap_subtitle(), see inst/examples/clinical-explorer.R."
      ),
      class = c("deprecatedWarning", "lifecycle_warning_deprecated"),
      call = NULL
    ))
  }
  if (restore && !is.null(blockr.core::get_session())) {
    blockr.core::notify(
      "This board has an AE heatmap from an older version. Save the board to keep it.",
      type = "warning", duration = NULL
    )
  }
  blk
}

#' The AE heatmap's default sentence
#'
#' "Top 25 Dictionary-Derived Term, painted by worst Standard Toxicity
#' Grade, by Actual Treatment": `{@top_terms}` is the prepare script's cap,
#' `@parent` the column grouping the rows, and a bracketed clause leaves when
#' its setting is empty.
#'
#' @return A title template, see [blockr.viz::new_chart_block()].
#' @export
ae_heatmap_subtitle <- function() {
  paste0(
    "[Top {@top_terms} ]{label(@facet)}",
    "[, painted by worst {label(@fill)}]",
    "[, by {label(@parent)}]"
  )
}

#' The AE heatmap's default prepare script
#'
#' Keeps the `top_terms` most frequent values of `col`, counted over the
#' event rows. `top_terms` is declared at the top, so it becomes a setting.
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
    paste0("top_terms <- ", as.integer(top_n),
           "  #| number(min = 1, max = 200, label = \"Top terms\")"),
    paste0("top <- dplyr::count(data, ", sym, ", sort = TRUE)"),
    paste0("dplyr::filter(data, ", sym, " %in% utils::head(top$", sym,
           ", top_terms))"),
    sep = "\n"
  )
}

# The cap of a script that is this block's default before the control was
# renamed (`top_n`), or NULL for any other script.
ae_heatmap_old_script <- function(script, col) {
  if (!is.character(script) || length(script) != 1L || is.na(script)) {
    return(NULL)
  }
  m <- regmatches(script, regexec("^top_n <- ([0-9]+)L?  #", script))[[1L]]
  if (!length(m)) {
    return(NULL)
  }
  sym <- deparse(as.name(col), backtick = TRUE)
  old <- paste(
    paste0("top_n <- ", m[[2L]], "  #| number(min = 1, max = 200)"),
    paste0("top <- dplyr::count(data, ", sym, ", sort = TRUE)"),
    paste0("dplyr::filter(data, ", sym, " %in% utils::head(top$", sym,
           ", top_n))"),
    sep = "\n"
  )
  norm <- sub("^top_n <- ([0-9]+)L", "top_n <- \\1", script)
  if (identical(trimws(norm), old)) as.integer(m[[2L]])
}
