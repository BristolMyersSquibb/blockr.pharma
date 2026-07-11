# Patient Profile Viz: constructor + requires resolver
#
# new_pp_viz()        — wraps a viz definition list, validates required fields
# pp_resolve_requires() — checks declared `requires` / `optional` columns
#                         against the actual dm, renames aliases to canonical
#                         names, returns either list(ok=TRUE, dm=renamed) or
#                         list(ok=FALSE, msg="...") for pp_empty_chart().

#' Construct a patient-profile viz definition
#'
#' Validates and classes a viz definition for use with the patient profile
#' block. The render function operates on a dm where any declared alias
#' columns have been renamed to their canonical names, so render code can
#' assume canonical column names exist.
#'
#' @param id Stable id used in `selected` and settings keys.
#' @param label Sidebar card title.
#' @param domain Sidebar category (Treatment, Adverse Events, Laboratory, ...).
#' @param icon Bootstrap icon name shown on the card.
#' @param color Hex color for the icon.
#' @param description One-line card description.
#' @param tables Character vector of required tables in the dm. Vizs whose
#'   tables are missing are hidden from the sidebar entirely.
#' @param requires Named list keyed by table. Each value is either a character
#'   vector of required column names (no aliases), or a named list where each
#'   key is the canonical column name and the value is a character vector of
#'   alias names to accept as substitutes. Missing required columns produce a
#'   message in the chart slot via [pp_empty_chart()].
#' @param optional Same shape as `requires`. Missing optional columns do not
#'   block render; present aliases are renamed to canonical names so render
#'   code can do `"FOO" %in% colnames(tbl)` instead of checking every alias.
#' @param requires_any Named list keyed by table, each value a list of
#'   character vectors. Each vector is a set of interchangeable columns, of
#'   which at least one must be present. Use it where `requires` cannot help
#'   because the alternatives are not aliases of one another: a study may ship
#'   an analysis date (`ASTDT`) or a study day (`ASTDY`), and renaming one to
#'   the other would put integers in a date slot. Declare the alternatives here
#'   and let the render function branch on which arrived.
#' @param controls Optional named list of per-viz UI controls (passed through
#'   unchanged to the existing controls toolbar).
#' @param render Function `function(dm_obj, time_range, settings, ref_ms, mode)`
#'   returning an htmlwidget. Receives the dm with aliases renamed to canonical.
#'
#' @return A list with class `"pp_viz"`.
#' @noRd
new_pp_viz <- function(id, label, domain, icon, color, description,
                       tables,
                       requires = list(),
                       optional = list(),
                       requires_any = list(),
                       controls = NULL,
                       render) {
  stopifnot(
    is.character(id), length(id) == 1L, nzchar(id),
    is.character(label), length(label) == 1L,
    is.character(domain), length(domain) == 1L,
    is.character(tables), length(tables) >= 1L,
    is.list(requires), is.list(optional), is.list(requires_any),
    is.function(render)
  )
  structure(
    list(
      id = id, label = label, domain = domain, icon = icon, color = color,
      description = description, tables = tables,
      requires = requires, optional = optional, requires_any = requires_any,
      controls = controls, render = render
    ),
    class = c("pp_viz", "list")
  )
}

#' Standard short CDISC -> ADaM canonical table-name aliases
#'
#' Prod studies frequently ship ADaM-shaped data under short SDTM-style
#' domain names (`ae`, `lb`, `vs`, ...) rather than the ADaM names
#' (`adae`, `adlb`, `advs`, ...). The patient-profile vizs declare their
#' data requirements against the ADaM names, so this map lets a viz find
#' its table regardless of which naming the sponsor used. Each entry is
#' `canonical = c(alias, ...)`.
#'
#' @return Named list of canonical -> alias character vectors.
#' @noRd
pp_table_aliases <- function() {
  list(
    adae = "ae",
    adcm = "cm",
    adex = "ex",
    adlb = "lb",
    advs = "vs",
    adeg = "eg"
  )
}

#' Rename known short CDISC table names to their ADaM canonical names
#'
#' For each `canonical = c(alias, ...)` entry: if the canonical table is
#' absent from the dm but an alias table is present, rename the alias to
#' the canonical name. An existing canonical table always wins (no
#' overwrite). Tables with no alias entry pass through untouched.
#'
#' Call this AFTER any FK-cascade filtering (e.g. `dm::dm_filter()`): the
#' rebuilt dm is flat (relational keys/FKs are dropped), but viz renderers
#' only pull tables by name via `dm::dm_get_tables()`, so this is
#' sufficient downstream.
#'
#' @param dm_obj A `dm` object.
#' @param aliases Named list of canonical -> alias vectors (see
#'   [pp_table_aliases()]).
#' @return A `dm`. Unchanged (same object) when no alias rename applied.
#' @noRd
pp_normalize_table_aliases <- function(dm_obj, aliases = pp_table_aliases()) {
  tbls <- dm::dm_get_tables(dm_obj)
  renamed <- FALSE
  for (canonical in names(aliases)) {
    if (canonical %in% names(tbls)) next
    hit <- aliases[[canonical]][aliases[[canonical]] %in% names(tbls)]
    if (length(hit) >= 1L) {
      use <- hit[[1L]]
      names(tbls)[names(tbls) == use] <- canonical
      renamed <- TRUE
    }
  }
  if (!renamed) return(dm_obj)
  do.call(dm::dm, lapply(tbls, as.data.frame))
}

#' Compute a data-coverage report for a set of vizs against a dm
#'
#' For each viz, determine whether it can render against `dm_obj` and, if
#' not, a short human-readable reason. Used to populate the gear-popover
#' "Data coverage" diagnostics so users can see which visuals are
#' unavailable for the current data and why — a missing source table or a
#' missing required column.
#'
#' @param dm_obj A `dm` object (already table-alias-normalized).
#' @param vizs Named list of `pp_viz` definitions.
#' @return List of `list(id, label, reason)`, one per viz that cannot
#'   render (empty list when all can).
#' @noRd
pp_coverage_report <- function(dm_obj, vizs) {
  tbl_names <- names(dm::dm_get_tables(dm_obj))
  out <- list()
  add <- function(v, reason) {
    out[[length(out) + 1L]] <<- list(id = v$id, label = v$label,
                                     reason = reason)
  }
  for (v in vizs) {
    missing_tbls <- setdiff(v$tables, tbl_names)
    if (length(missing_tbls)) {
      add(v, paste0("needs table ", paste(missing_tbls, collapse = ", ")))
      next
    }
    res <- pp_resolve_requires(dm_obj, v)
    if (!isTRUE(res$ok)) {
      # res$msg is "<label> unavailable: missing <cols>"; keep the cols.
      add(v, sub("^.*unavailable: missing ", "missing ", res$msg))
    }
  }
  out
}

#' Resolve required / optional column declarations against a dm
#'
#' Walks `viz$requires` and `viz$optional`. For each declared canonical
#' column, accept the canonical name if present; otherwise accept the first
#' alias that is present and rename it to canonical. If any required column
#' (canonical or alias) is missing, returns a failure result whose `msg`
#' lists what's missing.
#'
#' Tables in `viz$requires` / `viz$optional` that aren't in the dm are
#' silently ignored — the table-level filter (`viz$tables`) is the gate
#' for table presence.
#'
#' @param dm_obj A `dm` object.
#' @param viz A `pp_viz` definition.
#' @return List with `ok` (logical) and either `dm` (renamed dm) or `msg`.
#' @noRd
pp_resolve_requires <- function(dm_obj, viz) {
  tbls <- dm::dm_get_tables(dm_obj)

  # Normalize `requires` / `optional` shape:
  #   char vector              -> list(col = character()) per column
  #   list(col = NULL/c(...))  -> as-is
  normalize <- function(spec_for_table) {
    if (is.character(spec_for_table)) {
      stats::setNames(
        lapply(spec_for_table, function(x) character()),
        spec_for_table
      )
    } else if (is.list(spec_for_table)) {
      spec_for_table
    } else {
      list()
    }
  }

  rename_one <- function(df, canonical, aliases) {
    cols <- colnames(df)
    if (canonical %in% cols) return(list(ok = TRUE, df = df, used = canonical))
    hit <- aliases[aliases %in% cols]
    if (length(hit) >= 1L) {
      use <- hit[[1L]]
      colnames(df)[colnames(df) == use] <- canonical
      return(list(ok = TRUE, df = df, used = use))
    }
    list(ok = FALSE, missing = canonical, aliases = aliases)
  }

  missing_msgs <- character()
  renamed_tbls <- tbls

  resolve_block <- function(spec, required) {
    for (tbl_name in names(spec)) {
      if (!tbl_name %in% names(renamed_tbls)) next
      df <- as.data.frame(renamed_tbls[[tbl_name]])
      cols_spec <- normalize(spec[[tbl_name]])
      for (canonical in names(cols_spec)) {
        aliases <- cols_spec[[canonical]]
        if (is.null(aliases)) aliases <- character()
        res <- rename_one(df, canonical, aliases)
        if (isTRUE(res$ok)) {
          df <- res$df
        } else if (required) {
          alt <- if (length(aliases))
            paste0(" (or ", paste(aliases, collapse = ", "), ")") else ""
          missing_msgs <<- c(
            missing_msgs,
            paste0(canonical, alt, " in ", tbl_name)
          )
        }
      }
      renamed_tbls[[tbl_name]] <<- df
    }
  }

  resolve_block(viz$requires, required = TRUE)
  resolve_block(viz$optional, required = FALSE)

  # Interchangeable-column sets. Checked after the rename passes so an alias
  # that resolved to its canonical name counts as present.
  for (tbl_name in names(viz$requires_any %||% list())) {
    if (!tbl_name %in% names(renamed_tbls)) next
    cols <- colnames(as.data.frame(renamed_tbls[[tbl_name]]))
    for (alts in viz$requires_any[[tbl_name]]) {
      if (any(alts %in% cols)) next
      missing_msgs <- c(
        missing_msgs,
        paste0("one of ", paste(alts, collapse = ", "), " in ", tbl_name)
      )
    }
  }

  if (length(missing_msgs)) {
    return(list(
      ok = FALSE,
      msg = paste0(
        viz$label, " unavailable: missing ",
        paste(missing_msgs, collapse = "; ")
      )
    ))
  }

  # Rebuild a flat dm with the renamed tables. The relational metadata
  # (keys, FKs) from the original dm is dropped, but the viz renderers
  # only use `dm::dm_get_tables()` to pull tables by name, so this is
  # sufficient.
  new_dm <- do.call(dm::dm, renamed_tbls)
  list(ok = TRUE, dm = new_dm)
}
