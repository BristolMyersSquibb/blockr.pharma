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
#' @inheritParams pp_slot_ui
#' @noRd
pp_slot_header_ui <- function(viz, viz_id, controls_ui, legend_ui, download_ui) {
  # The header stacks its rows unconditionally now (see "The header's rows"
  # in the CSS), so the class no longer turns anything on: it says the
  # header carries a legend, and it is what the client's header swap
  # carries across when a patient with no severity levels follows one with
  # them.
  shiny::div(
    class = if (is.null(legend_ui)) "pp-chart-header" else
      "pp-chart-header has-legend",
    # The handle. Reordering used to live in the sidebar's card
    # list -- a remote control for a stack a few hundred pixels
    # to the right. The card you are looking at is the card you
    # drag now, and the whole header is the target so you can be
    # imprecise; the controls inside it keep their own clicks.
    shiny::span(class = "pp-chart-grip",
                title = "Drag to reorder",
                shiny::HTML(pp_grip_glyph())),
    # The code, and the group it came from as the tooltip. A
    # PARAMCD is unique across a study's findings tables, so the
    # header does not have to spend width saying Chemistry --
    # but hovering still answers it.
    shiny::div(class = "pp-chart-title",
               title = viz$description %||% viz$label,
               viz$label),
    # The full parameter name, muted, the house form: ALB, then
    # Albumin (g/L) behind it. The code is what you scan a stack
    # of twelve cards by; the name is what you read once you
    # have found the one you want.
    if (!is.null(viz$sublabel)) {
      shiny::div(class = "pp-chart-sublabel", viz$sublabel)
    },
    # Which panel the cohort strip draws. Rendered on every
    # panel and shown on one, so saying so costs a class rather
    # than a re-render -- and the question "which one is first?"
    # is answered where you are looking rather than only in the
    # sidebar's caption.
    shiny::span(class = "pp-band-tag",
                title = paste("Shown for every patient in the",
                              "list of patients"),
                shiny::HTML(pp_band_glyph())),
    controls_ui,
    legend_ui,
    download_ui,
    # Same toggle the sidebar card fires: removing a viz here
    # deselects it, so the sidebar card slides back to AVAILABLE.
    shiny::tags$button(
      class = "pp-chart-remove",
      type = "button",
      `data-viz-id` = viz_id,
      title = paste0("Remove ", viz$label),
      shiny::HTML(paste0(
        '<svg xmlns="http://www.w3.org/2000/svg" width="12" ',
        'height="12" fill="currentColor" viewBox="0 0 16 16">',
        '<path d="M4.646 4.646a.5.5 0 0 1 .708 0L8 7.293l2.646',
        '-2.647a.5.5 0 0 1 .708.708L8.707 8l2.647 2.646a.5.5 0 ',
        '0 1-.708.708L8 8.707l-2.646 2.647a.5.5 0 0 1-.708-.708',
        'L7.293 8 4.646 5.354a.5.5 0 0 1 0-.708z"/></svg>'
      ))
    )
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
  list(
    viz_id = viz_id,
    header = as.character(htmltools::renderTags(header)$html),
    opts_json = as.character(htmlwidgets:::toJSON(chart$x$opts)),
    evals = as.list(htmlwidgets:::JSEvals(chart$x$opts)),
    height = chart$height %||% NULL
  )
}

#' The download menu of one panel, if it has an exhibit.
#'
#' @param viz The `pp_viz` definition.
#' @param viz_id Its id on the profile.
#' @param ns The module's namespace function.
#' @noRd
pp_slot_download_ui <- function(viz, viz_id, ns) {
  if (!is.function(viz$exhibit) || !pp_exhibit_ready()) return(NULL)
  entries <- if (identical(viz$exhibit_kind, "table")) {
    list(
      if (requireNamespace("openxlsx", quietly = TRUE)) {
        list(id = "dl_xlsx_", label = "Excel (.xlsx)")
      },
      list(id = "dl_html_", label = "Web page (.html)"),
      if (requireNamespace("officer", quietly = TRUE)) {
        list(id = "dl_pptx_", label = "PowerPoint (.pptx)")
      }
    )
  } else {
    list(
      list(id = "dl_png_", label = "PNG"),
      if (requireNamespace("officer", quietly = TRUE)) {
        list(id = "dl_pptx_", label = "PowerPoint")
      }
    )
  }
  entries <- Filter(Negate(is.null), entries)
  shiny::tags$details(
    class = "pp-chart-download",
    shiny::tags$summary(
      title = paste("Download", viz$label),
      shiny::HTML(paste0(
        '<svg xmlns="http://www.w3.org/2000/svg" width="12" ',
        'height="12" fill="currentColor" viewBox="0 0 16 16">',
        '<path d="M.5 9.9a.5.5 0 0 1 .5.5v2.5a1 1 0 0 0 1 1h12',
        'a1 1 0 0 0 1-1v-2.5a.5.5 0 0 1 1 0v2.5a2 2 0 0 1-2 ',
        '2H2a2 2 0 0 1-2-2v-2.5a.5.5 0 0 1 .5-.5"/>',
        '<path d="M7.646 11.854a.5.5 0 0 0 .708 0l3-3a.5.5 0 ',
        '0 0-.708-.708L8.5 10.293V1.5a.5.5 0 0 0-1 0v8.793L',
        '5.354 8.146a.5.5 0 1 0-.708.708z"/></svg>'
      ))
    ),
    shiny::div(
      class = "pp-chart-download-menu",
      lapply(entries, function(e) {
        shiny::downloadLink(ns(paste0(e$id, viz_id)), e$label)
      })
    )
  )
}
