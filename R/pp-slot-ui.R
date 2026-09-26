#' A panel slot's chrome
#'
#' The header (grip, title, band tag, controls, legend, download menu,
#' remove button) over the chart body. The client drags the header, tags it
#' and removes it; see pp-panels.js.
#'
#' @param viz The `pp_viz` definition.
#' @param viz_id Its id on the profile.
#' @param chart The rendered chart.
#' @param controls_ui,legend_ui,download_ui The header pieces, or `NULL`.
#' @noRd
pp_slot_ui <- function(viz, viz_id, chart, controls_ui, legend_ui, download_ui) {
  shiny::tagList(
    pp_slot_header_ui(viz, viz_id, controls_ui, legend_ui, download_ui),
    shiny::div(class = "pp-chart-body", chart)
  )
}

#' The header alone. On a patient switch it travels in a `slot` message
#' and is swapped into the panel by the client, while the chart underneath
#' is updated in place (see pp_slot_update()).
#'
#' The design system's header row at panel size: the title and the panel's
#' sentence on the left, the tools on the right, the legend band under both.
#' A parameter's title is its name, with its code in the title's tooltip
#' (design system, "Column names and their labels": an exhibit prints the
#' decode). The tools, download and remove, show while the header is hovered
#' or has keyboard focus. There is no grip: the header itself drags.
#'
#' @inheritParams pp_slot_ui
#' @noRd
pp_slot_header_ui <- function(viz, viz_id, controls_ui, legend_ui, download_ui) {
  title <- viz$sublabel %||% viz$label
  shiny::div(
    class = if (is.null(legend_ui)) "pp-chart-header" else
      "pp-chart-header has-legend",
    shiny::div(
      class = "pp-chart-head",
      shiny::div(
        class = "pp-chart-titles",
        shiny::div(
          class = "pp-chart-title",
          `data-blockr-tooltip` = if (!is.null(viz$sublabel)) viz$label,
          title
        ),
        controls_ui
      ),
      shiny::div(
        class = "pp-chart-tools",
        download_ui,
        # Same toggle the sidebar fires: removing a panel here takes it off
        # the profile, and its row leaves the sidebar's list.
        shiny::tags$button(
          class = "blockr-tool pp-chart-remove",
          type = "button",
          `data-viz-id` = viz_id,
          `aria-label` = paste0("Remove ", title),
          `data-blockr-tooltip` = "Remove from the profile",
          shiny::HTML(PP_ICON_X)
        )
      )
    ),
    legend_ui
  )
}

#' The `slot` message: what the client needs to bring one panel to the
#' current patient without rebuilding it.
#'
#' The chart's option is serialised here with htmlwidgets' own encoder, so
#' the client receives byte for byte what a fresh render of the widget
#' would have carried, and `evals` lists the paths of the `JS()` strings
#' in it the way htmlwidgets does, so the client can turn them back into
#' functions. The header goes as HTML: it is plain DOM the client swaps
#' in, re-binding the download links.
#'
#' @param viz_id The panel's id.
#' @param header The header tag, from [pp_slot_header_ui()].
#' @param chart The echarts4r widget.
#' @noRd
pp_slot_update <- function(viz_id, header, chart) {
  stopifnot(inherits(chart, "echarts4r"))
  # Neither encoder is exported by htmlwidgets.
  to_json <- utils::getFromNamespace("toJSON", "htmlwidgets")
  js_evals <- utils::getFromNamespace("JSEvals", "htmlwidgets")
  list(
    viz_id = viz_id,
    header = as.character(htmltools::renderTags(header)$html),
    opts_json = as.character(to_json(chart$x$opts)),
    evals = as.list(js_evals(chart$x$opts)),
    height = chart$height %||% NULL
  )
}

#' The download menu of one panel, if it has an exhibit.
#'
#' An action menu (blockr.ui) of Shiny download links, one per format.
#'
#' @param viz The `pp_viz` definition.
#' @param viz_id Its id on the profile.
#' @param ns The module's namespace function.
#' @noRd
pp_slot_download_ui <- function(viz, viz_id, ns) {
  if (!is.function(viz$exhibit) || !pp_exhibit_ready()) return(NULL)
  row <- function(id, label, meta) {
    blockr.ui::menu_item(shiny::downloadLink(ns(paste0(id, viz_id)), label),
                         meta = meta)
  }
  rows <- if (identical(viz$exhibit_kind, "table")) {
    list(
      if (requireNamespace("openxlsx", quietly = TRUE)) {
        row("dl_xlsx_", "Excel", ".xlsx")
      },
      row("dl_html_", "Web page", ".html"),
      if (requireNamespace("officer", quietly = TRUE)) {
        row("dl_pptx_", "PowerPoint", ".pptx")
      }
    )
  } else {
    list(
      row("dl_png_", "Image", ".png"),
      if (requireNamespace("officer", quietly = TRUE)) {
        row("dl_pptx_", "PowerPoint", ".pptx")
      }
    )
  }
  title <- viz$sublabel %||% viz$label
  do.call(blockr.ui::action_menu, c(
    list(blockr.ui::tool_button(shiny::HTML(PP_ICON_DOWNLOAD),
                                paste("Download", pp_sentence_case(title)),
                                class = "pp-chart-download")),
    rows
  ))
}
