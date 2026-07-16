# Patient Profile Viz: constructor + requires resolver
#
# new_pp_viz()          — wraps a viz definition list, validates fields
# pp_resolve_requires() — checks declared `requires` / `requires_any` columns
#                         against the (already normalized) dm; returns either
#                         list(ok=TRUE) or list(ok=FALSE, msg="...") for
#                         pp_empty_chart(). A presence CHECK only: all name
#                         reconciliation happens once, dm-wide, in
#                         pp_normalize_dm() (see pp-normalize.R), so nothing
#                         is renamed or rebuilt here.

#' Construct a patient-profile viz definition
#'
#' Validates and classes a viz definition for use with the patient profile
#' block. Render functions receive a dm normalized by [pp_normalize_dm()],
#' so they declare and read canonical ADaM column names only.
#'
#' @param id Stable id used in `selected` and settings keys.
#' @param label Sidebar card title.
#' @param domain Sidebar category (Treatment, Adverse Events, Laboratory, ...).
#' @param icon Bootstrap icon name shown on the card.
#' @param color Hex color for the icon.
#' @param description One-line card description.
#' @param tables Character vector of required tables in the dm. Vizs whose
#'   tables are missing are hidden from the sidebar entirely.
#' @param requires Named list keyed by table; each value a character vector
#'   of required canonical column names. Missing required columns produce a
#'   message in the chart slot via [pp_empty_chart()].
#' @param optional Same shape. Documentation of what the render reads when
#'   present; never gates anything.
#' @param requires_any Named list keyed by table, each value a list of
#'   character vectors. Each vector is a set of interchangeable columns, of
#'   which at least one must be present. Use it where the alternatives are
#'   not aliases of one another: a study may ship an analysis date (`ASTDT`)
#'   or a study day (`ASTDY`), and renaming one to the other would put
#'   integers in a date slot. Declare the alternatives here and let the
#'   render function branch on which arrived.
#' @param uses Character vector of role names the render consumes (see
#'   pp-roles.R). The block injects `settings$roles` (the resolved columns)
#'   for these, plus `settings$sev_colors` when `"severity"` is used and the
#'   board scale map resolves. This replaces any viz-id wiring in the block.
#' @param controls Optional named list of per-viz UI controls (passed through
#'   unchanged to the existing controls toolbar).
#' @param legend_ui Optional `function(dm_obj, settings)` returning a tag (or
#'   `NULL`) for the panel header, e.g. a severity legend. Declared here so
#'   the block needs no per-viz special cases.
#' @param render Function `function(dm_obj, time_range, settings, ref_ms, mode)`
#'   returning an htmlwidget. Receives the normalized, subject-scoped dm.
#'
#' @return A list with class `"pp_viz"`.
#' @noRd
new_pp_viz <- function(id, label, domain, icon, color, description,
                       tables,
                       requires = list(),
                       optional = list(),
                       requires_any = list(),
                       uses = character(),
                       controls = NULL,
                       legend_ui = NULL,
                       render) {
  stopifnot(
    is.character(id), length(id) == 1L, nzchar(id),
    is.character(label), length(label) == 1L,
    is.character(domain), length(domain) == 1L,
    is.character(tables), length(tables) >= 1L,
    is.list(requires), is.list(optional), is.list(requires_any),
    all(vapply(requires, is.character, logical(1L))),
    all(vapply(optional, is.character, logical(1L))),
    is.character(uses),
    is.null(legend_ui) || is.function(legend_ui),
    is.function(render)
  )
  structure(
    list(
      id = id, label = label, domain = domain, icon = icon, color = color,
      description = description, tables = tables,
      requires = requires, optional = optional, requires_any = requires_any,
      uses = uses, controls = controls, legend_ui = legend_ui,
      render = render
    ),
    class = c("pp_viz", "list")
  )
}

#' Compute a data-coverage report for a set of vizs against a dm
#'
#' For each viz, determine whether it can render against `dm_obj` and, if
#' not, a short human-readable reason. Used to populate the gear-popover
#' "Data coverage" diagnostics so users can see which visuals are
#' unavailable for the current data and why — a missing source table or a
#' missing required column.
#'
#' @param dm_obj A normalized `dm` object.
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

#' Does the current patient lack rows in all of a viz's tables?
#'
#' Viz availability is cohort-based, so a viz can be listed while the
#' patient on screen has no data for it at all. Returns TRUE when every one
#' of the viz's declared tables is empty in the (subject-scoped) dm — the
#' chart slot then shows a "No data for this patient" message instead of
#' calling the renderer. A viz with at least one non-empty table (e.g. one
#' that reads adsl alongside a findings table) renders normally and handles
#' its own partial emptiness, as before.
#'
#' @param dm_obj A subject-scoped `dm` object.
#' @param tables Character vector of the viz's declared tables.
#' @return TRUE when all declared tables present in the dm have zero rows.
#' @noRd
pp_no_patient_rows <- function(dm_obj, tables) {
  tbls <- dm::dm_get_tables(dm_obj)
  tables <- intersect(tables, names(tbls))
  if (length(tables) == 0L) return(FALSE)
  all(vapply(
    tables,
    function(tbl_name) nrow(as.data.frame(tbls[[tbl_name]])) == 0L,
    logical(1L)
  ))
}

#' Check a viz's declared columns against a normalized dm
#'
#' Walks `viz$requires` and `viz$requires_any` and reports what is missing.
#' A pure presence check: name reconciliation happened dm-wide in
#' [pp_normalize_dm()], so a column absent here is genuinely absent from the
#' study, not merely spelled differently. No dm is rebuilt (this used to
#' rebuild the whole dm per viz, and the coverage report calls it once per
#' viz per header render).
#'
#' Tables in `viz$requires` that aren't in the dm are silently ignored — the
#' table-level filter (`viz$tables`) is the gate for table presence.
#'
#' @param dm_obj A normalized `dm` object.
#' @param viz A `pp_viz` definition.
#' @return List with `ok` (logical) and, when `FALSE`, `msg`.
#' @noRd
pp_resolve_requires <- function(dm_obj, viz) {
  tbls <- dm::dm_get_tables(dm_obj)
  missing_msgs <- character()

  for (tbl_name in names(viz$requires)) {
    if (!tbl_name %in% names(tbls)) next
    cols <- colnames(tbls[[tbl_name]])
    miss <- setdiff(viz$requires[[tbl_name]], cols)
    if (length(miss)) {
      missing_msgs <- c(missing_msgs, paste0(miss, " in ", tbl_name))
    }
  }

  for (tbl_name in names(viz$requires_any %||% list())) {
    if (!tbl_name %in% names(tbls)) next
    cols <- colnames(tbls[[tbl_name]])
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

  list(ok = TRUE)
}
