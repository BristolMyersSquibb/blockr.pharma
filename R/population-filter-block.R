# Population filter block -------------------------------------------------
#
# WHY THIS EXISTS, and why it is not in blockr.dm.
#
# A clinical board is read one split at a time: every chart colours by the
# treatment arm, every summary table puts it in the columns, every model uses
# it as a factor. Which column that is has to be a single board-wide choice --
# the arm, or sex, or race -- and the exhibits have to find it under a name
# they can all bind, which is `Group`.
#
# Both halves of that are conventions of this board, not of crossfiltering, so
# neither belongs in blockr.dm: the generic block knows only that one dimension
# can be pinned and picked from a list (`featured` / `pinned`). This block adds
# the two pharma-specific parts on top -- the name people read in the dock, and
# the `Group` column the pick is copied into -- and inherits everything else.
#
# The stamp is attached through `expr_server()`, the blockr.core generic whose
# default method runs the block's own server. Wrapping the returned expression
# is all it takes, so no crossfilter code is copied or exported for it.

#' Stamp the board's group column
#'
#' Copies `col` into a column named `as` on the parent table of `data`, so that
#' every exhibit downstream can bind one name (`Group`) instead of the name of
#' whichever column the board is currently split by.
#'
#' Only the parent table is stamped. Event tables reach the same value through
#' the join a flatten performs anyway, and stamping them too would give
#' `dm_flatten_to_tbl()` the same column twice: it renames the pair to
#' `Group.adsl` / `Group.ae` and every chart binding `Group` loses its column.
#'
#' @param data A [dm][dm::dm()] or a data frame.
#' @param col Column to copy. A column no table carries is a no-op, so a board
#'   whose study lacks it still renders.
#' @param as Name of the copy.
#'
#' @return `data`, with the copy added.
#' @export
dm_stamp_group <- function(data, col, as = "Group") {
  if (is.null(col) || !length(col) || !nzchar(col[[1L]])) {
    return(data)
  }
  col <- col[[1L]]

  if (is.data.frame(data)) {
    if (col %in% names(data)) {
      data[[as]] <- stamp_source(data[[col]], col)
    }
    return(data)
  }

  tbls <- names(dm::dm_get_tables(data))
  carriers <- tbls[vapply(tbls, function(tb) col %in% colnames(data[[tb]]),
                          logical(1))]
  if (!length(carriers)) {
    return(data)
  }

  # The parent first: with a star schema that is the subject table, which is
  # where a stratifier lives. Falling back to the first carrier keeps a
  # keyless dm working rather than silently doing nothing.
  fks <- dm::dm_get_all_fks(data)
  target <- carriers[[1L]]
  if (nrow(fks) > 0) {
    counts <- table(as.character(fks$parent_table))
    parents <- names(sort(counts, decreasing = TRUE))
    hit <- intersect(parents, carriers)
    if (length(hit)) {
      target <- hit[[1L]]
    }
  }

  # do.call rather than the tidyselect / `:=` spellings: the table and the new
  # column name are values here, not literals, and passing symbols through
  # do.call keeps this readable without pulling rlang in for two operators.
  zoomed <- do.call(dm::dm_zoom_to, list(data, as.symbol(target)))
  zoomed <- do.call(
    dplyr::mutate,
    c(
      list(zoomed),
      stats::setNames(
        list(
          as.call(
            list(quote(blockr.pharma::stamp_source), as.symbol(col), col)
          )
        ),
        as
      )
    )
  )
  # `dm_zoom_to()` + `dm_update_zoomed()` build a fresh dm, which drops the
  # attributes set on the old one -- including the filter trail the crossfilter
  # below just recorded. Carrying it across is the whole reason this is not a
  # bare `dm_update_zoomed()`: without it every board whose global filter is a
  # population filter block loses the trail here, and every caption and table
  # footnote downstream goes silent while the filter is plainly applied. See
  # `blockr.dm::filter_trail()`.
  blockr.dm::add_filter_trail(dm::dm_update_zoomed(zoomed), data)
}

#' Mark which column a copy came from
#'
#' The `Group` column names no study variable, so anything downstream that has
#' to report or filter on the real one (a composer table's `by =`, a drill
#' claim) reads it back off this attribute. Kept as a function rather than a
#' bare `attr<-` so the stamp is one expression inside a `mutate()`.
#'
#' @param x A column.
#' @param col Name of the column it was copied from.
#'
#' @return `x`, carrying a `blockr_source` attribute.
#' @export
stamp_source <- function(x, col) {
  attr(x, "blockr_source") <- col
  x
}

#' Population filter block
#'
#' The crossfilter block with the two conventions a clinical board runs on: the
#' pinned card holds the study's stratifier, and whatever it holds is copied
#' into a `Group` column that every chart, table and model on the board binds
#' by name. Switching the pick in the card header re-points the whole board.
#'
#' `featured` is the vocabulary the block offers up front: the columns that
#' appear as one-click chips, and, for those on the subject table, the choices
#' in the pinned card's picker. Everything else stays reachable through the
#' gear.
#'
#' @param featured Columns worth showing up front, e.g.
#'   `c("TRT", "SEX", "RACE", "AETOXGR")`.
#' @param pinned The column the board is split by at start, one of `featured`.
#' @param ... Forwarded to [blockr.dm::new_crossfilter_block()]. Pass
#'   `stamp_as` here to write the pick to a column other than `Group`, or
#'   `stamp_as = NULL` for no stamp at all, which leaves the plain
#'   crossfilter under this block's name.
#'
#' @return A crossfilter block carrying a `population_filter_block` subclass.
#' @importFrom blockr.core expr_server
#' @export
new_population_filter_block <- function(featured = character(),
                                        pinned = NULL,
                                        ...) {
  # Serialization identity and class, both set AT CONSTRUCTION. The framework
  # injects ctor / ctor_pkg itself on a restore, and block metadata is looked
  # up from class[1] inside new_block(), so a class prepended afterwards would
  # take the crossfilter's registry entry instead of this block's. `stamp_as`
  # rides in `...` rather than as a formal: constructor formals are read back
  # out of the SERVER's environment for the initial state, and the server
  # belongs to blockr.dm, which knows nothing about stamping.
  args <- list(...)
  if (!"ctor" %in% names(args)) {
    args$ctor <- "new_population_filter_block"
    args$ctor_pkg <- utils::packageName()
  }
  if (!"class" %in% names(args)) {
    args$class <- c("population_filter_block", "crossfilter_block")
  }
  if (!"stamp_as" %in% names(args)) {
    args$stamp_as <- "Group"
  } else if (is.null(args$stamp_as)) {
    # An attribute set to NULL is an attribute removed, which reads back as
    # "not supplied" and would stamp anyway. Empty string carries the refusal.
    args$stamp_as <- ""
  }

  do.call(
    blockr.dm::new_crossfilter_block,
    c(list(featured = featured, pinned = pinned), args)
  )
}

#' @method expr_server population_filter_block
#' @export
expr_server.population_filter_block <- function(x, data, ...) {
  out <- NextMethod()
  stamp_as <- attr(x, "stamp_as") %||% "Group"

  if (is.null(stamp_as) || !nzchar(stamp_as)) {
    return(out)
  }

  # Hold the inherited reactives in their own names: reading them back out of
  # `out` inside the wrapper would make the wrapper call itself.
  inner_expr <- out[["expr"]]
  pinned <- out[["state"]][["pinned"]]

  out[["expr"]] <- shiny::reactive({
    inner <- inner_expr()
    pin <- pinned()
    if (is.null(pin) || !length(pin) || !nzchar(pin[[1L]])) {
      return(inner)
    }
    # Self-qualified: the expression is also what a report or an export shows,
    # and it has to run outside this package's namespace.
    bquote(
      blockr.pharma::dm_stamp_group(.(inner), .(pin[[1L]]), .(stamp_as))
    )
  })

  out
}

#' @noRd
population_filter_arguments <- function() {
  new_arg_specs(
    featured = new_arg_spec(
      paste0(
        "Columns worth a one-click chip above the filter cards, and, for the ",
        "categorical ones on the subject table, the choices in the pinned ",
        "card's picker."
      ),
      example = list("TRT", "SEX", "RACE", "AETOXGR"),
      type = arg_array(arg_string())
    ),
    pinned = new_arg_spec(
      paste0(
        "Column the board is split by: held in an always-open card at the ",
        "top and copied into `Group`. One of `featured`, on the subject ",
        "table."
      ),
      example = "TRT",
      type = arg_string()
    )
  )
}
