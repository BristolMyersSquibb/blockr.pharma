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
                       controls = NULL,
                       render) {
  stopifnot(
    is.character(id), length(id) == 1L, nzchar(id),
    is.character(label), length(label) == 1L,
    is.character(domain), length(domain) == 1L,
    is.character(tables), length(tables) >= 1L,
    is.list(requires), is.list(optional),
    is.function(render)
  )
  structure(
    list(
      id = id, label = label, domain = domain, icon = icon, color = color,
      description = description, tables = tables,
      requires = requires, optional = optional,
      controls = controls, render = render
    ),
    class = c("pp_viz", "list")
  )
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
