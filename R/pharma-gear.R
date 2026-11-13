# What the pharma blocks' gears share: the script with their own gear rows
# (inst/js/pharma-gear.js) and the Tables row's report of what a block found
# in its input.

# The script of the gear rows the pharma blocks register: the Tables row
# ("mapping"), the swimlane's Width ("lane_width") and the rows at start
# ("open_rows").
pharma_gear_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-pharma-gear",
    version = utils::packageVersion("blockr.pharma"),
    src = system.file("js", package = "blockr.pharma"),
    script = "pharma-gear.js"
  )
}

# Per role, whether its table is in `data` and which columns it lacks.
# `roles` is a list of role (its name), table (canonical), need (must be
# there) and cols (a list, each a set of names any one of which will do).
# Tables and columns may be named as the patient profile allows; names match
# without case.
pharma_mapping <- function(data, roles) {
  tabs <- if (inherits(data, "dm")) dm::dm_get_tables(data) else data
  if (is.data.frame(tabs)) tabs <- list(adae = tabs)
  if (!is.list(tabs)) tabs <- list()
  names(tabs) <- tolower(names(tabs))
  raw <- names(tabs)
  tabs <- pp_normalize_tables(tabs)$tables
  lapply(roles, function(r) {
    tb <- tabs[[r$table]]
    have <- if (is.null(tb)) character() else toupper(names(tb))
    miss <- vapply(r$cols, function(alt) {
      if (any(alt %in% have)) "" else alt[[1L]]
    }, "")
    # A table found under its SDTM-style name is reported by that name.
    alias <- pp_table_catalog()[[r$table]]
    name <- c(if (!r$table %in% raw) alias[alias %in% raw], r$table)[[1L]]
    list(role = r$role, table = toupper(name), found = !is.null(tb),
         need = r$need,
         columns = as.list(vapply(r$cols, `[[`, "", 1L)),
         missing = as.list(if (is.null(tb)) character() else
           miss[nzchar(miss)]))
  })
}

# A board saves a table block's whole state: every argument of
# blockr.viz::new_table_block(), `layout` and `script` included, not only
# those of the block's constructor. A block built on the table sets the rest
# itself, so when the call carries a saved state (it names `layout`), the
# table's arguments that `ctor` does not take are dropped. Passed on, they
# would clash with the block's own (`layout`, `script`) or overwrite its
# settings of the same name (the timeline's `unit`).
table_state_drop <- function(dots, ctor) {
  if (!"layout" %in% names(dots)) {
    return(dots)
  }
  table_args <- setdiff(names(formals(blockr.viz::new_table_block)),
                        c("...", names(formals(ctor))))
  dots[setdiff(names(dots), table_args)]
}
