# Whether the cohort on screen was narrowed by a drill, and by what.
#
# The profile never sees the click. A chart pushes its claim into the board's
# drill filter over the control channel, the filter narrows the dm, and the
# profile receives the narrowed dm -- so what it can know it has to read off
# that dm. The drill filter stamps its clause on the data it emits (the
# filter trail, blockr.dm/R/filter-trail.R), keyed by its module namespace,
# and the profile finds the filter's board id the way every sender does:
# by class, over the channel. Matching the two is what this file does.
#
# Read off the data rather than pushed by a sender on purpose: a label a
# sender writes is a copy, and the moment another chart claims the filter --
# or someone clears it by hand in its own widget -- the copy is stale. The
# trail rides on the dm the profile is drawing, so it cannot disagree with
# what is on screen.

#' The drill state behind a cohort
#'
#' @param trail The incoming dm's filter trail, from
#'   [blockr.dm::filter_trail()]: a character vector keyed by the module
#'   namespace of the block that wrote each clause.
#' @param target The drill filter's board id, from
#'   `blockr.viz::ctrl_targets("drill_filter_block")`. Anything other than
#'   exactly one id means there is no drill to report.
#' @return `NULL` when the cohort is not drilled, else `list(id=, clause=)`:
#'   the filter's board id (where a reset is sent) and its clause (what the
#'   pill says).
#' @noRd
pp_drill_state <- function(trail, target) {

  if (length(target) != 1L || !nzchar(target) || !length(trail)) {
    return(NULL)
  }

  # blockr.core mounts a block's server under `block_<id>`, and the block's
  # own server under `expr` inside that, so the namespace a filter stamps
  # its clause with ends in `block_<id>-expr-`.
  keys <- names(trail)
  hit <- keys[endsWith(keys, paste0("block_", target, "-expr-"))]

  if (length(hit) != 1L) {
    return(NULL)
  }

  clause <- trail[[hit]]

  if (!is.character(clause) || length(clause) != 1L || !nzchar(clause)) {
    return(NULL)
  }

  list(id = unname(target), clause = unname(clause))
}
