#' The chart area: the stack of panel slots, or why there is none
#'
#' @param ns The module's namespace function.
#' @param single Whether exactly one patient is picked.
#' @param active_ids The panels on the profile, in order.
#' @noRd
pp_chart_area_ui <- function(ns, single, active_ids) {
  # One line, no icon (blockr.ui's empty state). The link in it opens the
  # list and puts the cursor in its search (pp-header.js).
  if (!isTRUE(single)) {
    # The line lives in its own output: reading the cohort size here would
    # put it back into this output's dependencies and redraw the whole
    # placeholder on every upstream filter.
    # The class sits on a wrapper: Shiny lays its output container out as
    # display: contents, which drops any padding put on it.
    return(shiny::div(class = "blockr-empty blockr-empty--block",
      shiny::uiOutput(ns("pp_empty_hint"), inline = TRUE)
    ))
  }
  if (length(active_ids) == 0) {
    return(shiny::p(class = "blockr-empty blockr-empty--block",
      "No panels on the profile. ",
      shiny::tags$button(type = "button", class = "blockr-slot pp-open-search",
                         "Add one")
    ))
  }
  shiny::tagList(lapply(active_ids, function(viz_id) {
    shiny::uiOutput(
      ns(paste0("viz_slot_", viz_id)),
      class = if (identical(viz_id, "patient_overview")) {
        "pp-chart-panel pp-treatment-strip"
      } else {
        "pp-chart-panel"
      }
    )
  }))
}
