#' The sort word on the patients' caption
#'
#' A live word, "by *patient id*", that opens a menu of the keys the data
#' offers (pp-slots.js); the pick goes to the `cohort_sort` input.
#'
#' @param choices Named sort keys, from `pp_cohort_sort_choices()`.
#' @param cur The current key.
#' @param ns The module's namespace function.
#' @return The word as HTML, or `NULL` when there is nothing to choose.
#' @noRd
pp_cohort_sort_ui <- function(choices, cur, ns) {
  if (length(choices) < 2L) return(NULL)
  if (!cur %in% names(choices)) cur <- names(choices)[[1L]]
  keys <- names(choices)
  opts <- lapply(keys, function(k) {
    list(value = k, label = unname(choices[[k]]))
  })
  pp_slot_word(pp_cohort_sort_short(cur), "by {}", list(
    id = ns("cohort_sort_by"),
    `data-kind` = "single",
    `data-input` = "cohort_sort",
    `data-title` = "Sort patients by",
    `data-label-first` = "false",
    `data-options` = as.character(jsonlite::toJSON(
      lapply(opts, function(o) list(value = o$label, key = o$value)),
      auto_unbox = TRUE
    )),
    `data-value` = as.character(jsonlite::toJSON(unname(choices[[cur]]),
                                                 auto_unbox = TRUE))
  ))
}

#' The patients' caption: a section title and one sentence
#'
#' "PATIENTS", the cohort's status on the right ("306 patients", or while a
#' drill narrows them the reset, "6 of 179 patients", painted by
#' pp-header.js), then what the strip beside each patient draws and how the
#' list is ordered: "Adverse events, by *patient id*". The id prefix every
#' patient shares follows the title, since the rows leave it out. A filter
#' on the strip is not repeated here; the panel that sets it says so.
#'
#' @param src The band source, or `NULL`.
#' @param sorter The sort word, from [pp_cohort_sort_ui()], or `NULL`.
#' @param pre The id prefix every patient shares.
#' @noRd
pp_band_caption_ui <- function(src, sorter, pre) {
  what <- if (!is.null(src)) htmltools::htmlEscape(src$caption) else ""
  # A parameter's strip names the code, then the parameter, muted.
  if (!is.null(src) && !is.null(src$sub)) {
    what <- paste0(what, '<span class="pp-gear-meta">',
                   htmltools::htmlEscape(src$sub), "</span>")
  }
  sentence <- paste0(
    what,
    if (!is.null(sorter)) paste0(if (nzchar(what)) ", " else "", sorter)
  )
  if (!nzchar(what) && !is.null(sorter)) {
    sentence <- pp_capitalise_word(sentence)
  }
  shiny::div(
    class = "pp-cohort-bandcap",
    shiny::div(
      class = "pp-cohort-bandcap-title",
      shiny::span("Patients"),
      if (nzchar(pre)) shiny::span(class = "pp-cohort-prefix", pre),
      shiny::span(class = "pp-cohort-status")
    ),
    if (nzchar(sentence)) {
      shiny::div(class = "pp-cohort-bandcap-what", shiny::HTML(sentence))
    }
  )
}
