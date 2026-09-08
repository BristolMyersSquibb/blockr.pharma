#' The chart area: the stack of panel slots, or why there is none
#'
#' @param ns The module's namespace function.
#' @param single Whether exactly one patient is picked.
#' @param active_ids The panels on the profile, in order.
#' @noRd
pp_chart_area_ui <- function(ns, single, active_ids) {
  if (!isTRUE(single)) {
    return(shiny::div(class = "pp-empty-state",
      shiny::div(class = "pp-empty-state-icon",
        shiny::HTML(paste0(
          '<svg xmlns="http://www.w3.org/2000/svg" width="40" ',
          'height="40" fill="currentColor" viewBox="0 0 16 16">',
          '<path d="M8 8a3 3 0 1 0 0-6 3 3 0 0 0 0 6m2-3a2 2 0 ',
          '1 1-4 0 2 2 0 0 1 4 0m4 8c0 1-1 1-1 1H3s-1 0-1-1 ',
          '1-4 6-4 6 3 6 4m-1-.004c-.001-.246-.154-.986-.832',
          '-1.664C11.516 10.68 10.289 10 8 10c-2.29 0-3.516 ',
          '.68-4.168 1.332-.678.678-.83 1.418-.832 ',
          '1.664z"/></svg>'
        ))
      ),
      shiny::p(class = "pp-empty-state-text",
        "No patient selected"),
      # The count lives in its own output: reading it here would
      # put the cohort size back into this output's dependencies
      # and flash the whole placeholder on every upstream filter.
      shiny::p(class = "pp-empty-state-hint",
        shiny::uiOutput(ns("pp_empty_hint"), inline = TRUE))
    ))
  }
  if (length(active_ids) == 0) {
    return(shiny::div(class = "pp-empty-state",
      shiny::div(class = "pp-empty-state-icon",
        shiny::HTML(paste0(
          '<svg xmlns="http://www.w3.org/2000/svg" width="40" ',
          'height="40" fill="currentColor" viewBox="0 0 16 16">',
          '<path d="M14 1a1 1 0 0 1 1 1v12a1 1 0 0 1-1 1H2a1 ',
          '1 0 0 1-1-1V2a1 1 0 0 1 1-1zM2 0a2 2 0 0 0-2 ',
          '2v12a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V2a2 2 0 0 ',
          '0-2-2z"/>',
          '<path d="M6.854 4.646a.5.5 0 0 1 0 .708L4.207 ',
          '8l2.647 2.646a.5.5 0 0 1-.708.708l-3-3a.5.5 0 0 ',
          '1 0-.708l3-3a.5.5 0 0 1 .708 0zm2.292 0a.5.5 0 0 ',
          '0 0 .708L11.793 8l-2.647 2.646a.5.5 0 0 0 .708',
          '.708l3-3a.5.5 0 0 0 0-.708l-3-3a.5.5 0 0 0-.708 ',
          '0z"/></svg>'
        ))
      ),
      shiny::p(class = "pp-empty-state-text",
        "No visualizations selected"),
      shiny::p(class = "pp-empty-state-hint",
        "Click cards in the sidebar to add charts")
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
