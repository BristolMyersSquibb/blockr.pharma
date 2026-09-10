#' The sort clause on the cohort caption
#'
#' The house click-through pill: `by <key>`, cycling through the keys the
#' band offers. The client advances it and sends `cohort_sort`.
#'
#' @param choices Named sort keys, from `pp_cohort_sort_choices()`.
#' @param cur The current key.
#' @param ns The module's namespace function.
#' @noRd
pp_cohort_sort_ui <- function(choices, cur, ns) {
  if (length(choices) < 2L) return(NULL)
  if (!cur %in% names(choices)) cur <- names(choices)[[1L]]
  idx <- match(cur, names(choices))
  keys <- names(choices)
  short <- vapply(keys, pp_cohort_sort_short, character(1L))
  nxt <- unname(choices)[idx %% length(keys) + 1L]
  shiny::tags$button(
    class = "pp-cohort-sortby",
    id = ns("cohort_sort_by"),
    type = "button",
    `data-values` = jsonlite::toJSON(keys),
    `data-labels` = jsonlite::toJSON(unname(short)),
    `data-index` = idx - 1L,
    # The control names the state, so the tooltip carries the
    # action.
    title = paste0("Sort by ", tolower(nxt)),
    "by ",
    shiny::tags$b(unname(short)[idx]),
    shiny::HTML("&#9662;")
  )
}

#' The caption above the cohort list: what the band shows, the live find
#' picks, the shared id prefix, and the sort clause.
#'
#' @param src The band source, or `NULL`.
#' @param sorter The sort clause, or `NULL`.
#' @param pre The id prefix every patient shares.
#' @param picks The driving panel's find picks, if any.
#' @noRd
pp_band_caption_ui <- function(src, sorter, pre, picks) {
  shiny::div(
    class = "pp-cohort-bandcap",
    if (!is.null(src)) shiny::HTML(pp_band_glyph()),
    # A sentence, not a label. "Cohort band" was our word for the
    # strip and taught nowhere; what a reader wants to know is what
    # the little pictures beside each patient ARE.
    # No lead sentence. "Each row shows Adverse Events" plus "by
    # event count" measured 276px in a 231px row, and the half
    # that got clipped was the control. The glyph is the same mark
    # the panel header carries beside its name, the tooltip still
    # reads "Every patient below shows ...", and the panel that
    # drives it wears the same ring in its own header -- so the
    # sentence is said in two other places.
    if (!is.null(src)) {
      shiny::span(class = "pp-cohort-bandcap-what",
                  title = src$title, src$caption)
    },
    # The parameter's full name, muted, exactly as the card that
    # drives the band prints it. The code alone is the thing you
    # match against the cards; the name is what tells you what it
    # measures.
    if (!is.null(src) && !is.null(src$sub)) {
      shiny::span(class = "pp-cohort-bandcap-sub", src$sub)
    },
    # The panel's filter, echoed. Without it the bands go sparse
    # for no visible reason, which is the sidebar quietly lying
    # about the cohort. There is room for one pick and a count of
    # the rest; the tooltip names them all, and the click clears
    # them all, because a chip that removed only the one it names
    # would leave the caption saying "+2" with no way to reach the
    # two.
    if (!is.null(src) && length(pp_find_picks(picks))) {
      labs <- pp_find_labels(picks)
      shiny::span(
        class = "pp-cohort-bandcap-find",
        `data-viz-id` = src$viz_id,
        title = paste0("Showing only records matching ",
                       paste(labs, collapse = ", "), "; click to clear"),
        # One pick is named; several are counted. There is no width here for
        # "General disorders and administration site conditions +2" -- it
        # ellipsized to "G..", which says less than a number does. The
        # tooltip names them all either way.
        shiny::span(if (length(labs) == 1L) labs else {
          paste(length(labs), "filters")
        }),
        shiny::HTML("&times;")
      )
    },
    # Everything after this sits at the right edge.
    shiny::span(class = "pp-cohort-bandcap-gap"),
    if (nzchar(pre)) {
      shiny::span(class = "pp-cohort-prefix",
                  title = "Shared by every patient in the cohort",
                  pre)
    },
    sorter
  )
}
