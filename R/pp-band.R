# What the cohort strip draws, and which panel decides it.
#
# The sidebar's 176px band used to be the AE band, full stop: the profile
# could be showing nothing but laboratory panels and the strip still painted
# adverse events. It now follows the panels -- the FIRST selected viz that
# declares a band form draws, and the ones that have no honest reduction to
# one row are skipped.
#
# Two declarations, because there are two shapes worth drawing at this size:
#
#   pp_band_spans()  -- intervals on the study-day axis (adverse events,
#                       concomitant medications), optionally coloured by a
#                       role. This is the band that already existed.
#   pp_band_series() -- one value series (a findings parameter), drawn as a
#                       line with the upper reference limit as a hairline.
#
# A viz declares one or neither. Nothing here reads a viz id: the patient
# overview is skipped because it declares no band, not because it is named.

#' Declare an interval band
#'
#' @param table The dm table the intervals come from.
#' @param start,end Candidate columns for the interval bounds, day column
#'   first and analysis date second. Both are tried per ROW, not per column
#'   -- see [pp_cohort_span_events()] for why a partly populated day column
#'   must not win wholesale.
#' @param color Role name whose resolved column colours the spans
#'   (`"severity"`), or `NULL` for one flat colour.
#' @param search Columns a panel search filters the spans on, coarsest last.
#'   `NULL` for a band no search reaches.
#' @param open_ends Whether a missing end means "ongoing" (it runs to the
#'   end of that patient's own data) rather than a same-day event.
#' @return A `pp_band` declaration.
#' @noRd
pp_band_spans <- function(table, start, end, color = NULL, search = NULL,
                          open_ends = TRUE) {
  stopifnot(
    is.character(table), length(table) == 1L,
    is.character(start), is.character(end),
    is.null(color) || (is.character(color) && length(color) == 1L),
    is.null(search) || is.character(search)
  )
  structure(
    list(kind = "spans", table = table, start = start, end = end,
         color = color, search = search, open_ends = open_ends),
    class = c("pp_band", "list")
  )
}

#' Declare a value-series band
#'
#' The band draws ONE parameter, and which one is fixed at declaration: a
#' findings card covers a dozen and the strip has room for the first. The
#' caption in the sidebar names it, because a line of unlabelled numbers is
#' not readable otherwise.
#'
#' @param table The dm table the values come from.
#' @param paramcd The PARAMCD the band draws.
#' @param param Its display name, for the caption. Falls back to `paramcd`.
#' @param value,day,date,lo,hi Column names: the value, the study day, the
#'   analysis date (converted when no day is present), and the reference
#'   limits.
#' @return A `pp_band` declaration.
#' @noRd
pp_band_series <- function(table, paramcd, param = NULL, value = "AVAL",
                           day = "ADY", date = "ADT", lo = "A1LO",
                           hi = "A1HI") {
  stopifnot(
    is.character(table), length(table) == 1L,
    is.character(paramcd), length(paramcd) == 1L
  )
  structure(
    list(kind = "series", table = table, paramcd = paramcd,
         param = param %||% paramcd, value = value, day = day, date = date,
         lo = lo, hi = hi),
    class = c("pp_band", "list")
  )
}

#' The AE band, as a declaration
#'
#' The band the sidebar drew before it followed the panels, and still the
#' default when [pp_cohort_marks()] is called without a source.
#' @noRd
pp_band_ae <- function() {
  pp_band_spans(
    table = "adae",
    start = c("ASTDY", "ASTDT"),
    end = c("AENDY", "AENDT"),
    color = "severity",
    search = c("AETERM", "AEDECOD", "AEHLT", "AEBODSYS")
  )
}

#' Which panel drives the cohort band
#'
#' Walks the selected vizs in the order the sidebar lists them and returns
#' the first that is both available and declares a band form. A selected viz
#' the data cannot offer is passed over for the same reason the chart area
#' passes over it: it is not on screen.
#'
#' Returns `NULL` when nothing selected can be drawn, and the rows keep an
#' empty track. Falling back to adverse events was the alternative and was
#' rejected: the band would then show a panel that is not on screen, which
#' the caption would have to explain away.
#'
#' @param selected Selected viz ids, in sidebar order.
#' @param available Named list of available `pp_viz` definitions.
#' @param settings Per-viz settings (`r_viz_settings()`), so a series band
#'   follows the parameters the user has actually picked rather than the
#'   ones the card was declared with.
#' @return `list(viz_id, label, caption, title, band)` or `NULL`.
#' @noRd
pp_cohort_band_source <- function(selected, available, settings = list()) {
  for (viz_id in selected) {
    viz <- available[[viz_id]]
    if (is.null(viz) || is.null(viz$band)) next
    band <- pp_band_for_selection(viz, settings[[viz_id]]$items)
    return(list(
      viz_id = viz_id,
      label = viz$label,
      caption = pp_band_caption(viz$label, band),
      title = pp_band_title(viz$label, band),
      band = band
    ))
  }
  NULL
}

#' Point a series band at the parameter the panel is currently drawing first
#'
#' A findings card covers a dozen parameters and the strip has room for one:
#' the first CHART, which is the lowest PARAMCD among the chips that are on
#' (pp_render_findings() sorts by PARAMCD). Deselect Albumin and Alkaline
#' Phosphatase from a chemistry card and the panel leads with ALT, so the
#' strip does too.
#'
#' This was fixed at declaration in the first cut, on the theory that 254
#' rows should not re-derive on a chip click. That was the wrong trade: it
#' left the caption naming a parameter the panel was no longer drawing, which
#' is the exact disagreement the caption exists to prevent. The re-derivation
#' is one filtered pass over one findings table, the same cost the AE band
#' already pays on every search.
#'
#' A card with nothing selected keeps its declared parameter rather than
#' going blank: the panel in that state draws nothing, and a strip is more
#' use than an empty track while the user picks again.
#'
#' @param viz The `pp_viz` definition.
#' @param items The PARAMCDs currently chosen, or `NULL`.
#' @return The viz's band, possibly repointed.
#' @noRd
pp_band_for_selection <- function(viz, items = NULL) {
  band <- viz$band
  if (!identical(band$kind, "series")) return(band)
  chosen <- sort(intersect(as.character(items), names(viz$params)))
  if (!length(chosen)) return(band)
  band$paramcd <- chosen[[1L]]
  band$param <- unname(viz$params[[chosen[[1L]]]]) %||% chosen[[1L]]
  band
}

#' What the sidebar says the band is
#'
#' The panel's name, and for a series band the parameter behind it. The
#' parameter is the part a reader cannot guess and the part that changes
#' between studies, so it is never dropped.
#'
#' The CODE, not the name: the caption sits in a 232px sidebar where
#' "Chemistry \u00b7 Alanine Aminotransferase" ellipsizes to "Chemistry
#' \u00b7 Alanine Amino...", which is exactly the wrong half. The full name is
#' the tooltip ([pp_band_title()]), the same split the row's id and the
#' panel's chips already make.
#' @noRd
pp_band_caption <- function(label, band) {
  if (identical(band$kind, "series")) {
    paste0(label, " \u00b7 ", band$paramcd)
  } else {
    label
  }
}

#' The caption's tooltip: what the band draws, spelled out
#' @noRd
pp_band_title <- function(label, band) {
  what <- if (identical(band$kind, "series")) {
    paste0(label, ": ", band$param)
  } else {
    label
  }
  paste("The cohort band draws", what)
}

#' The strip glyph the caption and the panel card share
#'
#' A 12x7 shorthand for "this is what the cohort band draws", so the card
#' driving the band and the caption naming it carry the same mark.
#' @noRd
pp_band_glyph <- function() {
  paste0(
    '<svg class="pp-band-glyph" width="12" height="7" viewBox="0 0 12 7" ',
    'aria-hidden="true">',
    '<rect x="0" y="1" width="12" height="5" rx="1.5" fill="currentColor" ',
    'opacity="0.2"/>',
    '<rect x="1" y="1" width="3" height="5" fill="currentColor" ',
    'opacity="0.8"/>',
    '<rect x="7" y="1" width="4" height="5" fill="currentColor" ',
    'opacity="0.8"/></svg>'
  )
}

#' Does a record match a panel search?
#'
#' Case-insensitive substring against every declared coding level the study
#' actually carries, so `pneumonia` finds the verbatim term and `infections`
#' finds the body system whose preferred terms never contain the word.
#'
#' A term matching nothing returns all `FALSE` rather than falling back to
#' everything: "no records match what you typed" is an answer, and quietly
#' showing all of them instead is not.
#'
#' @param tbl A data frame of records.
#' @param cols Candidate columns, from the band or control declaration.
#' @param term The search term. Blank matches everything.
#' @return A logical vector, one per row of `tbl`.
#' @noRd
pp_search_match <- function(tbl, cols, term) {
  n <- nrow(tbl)
  if (!nzchar(term %||% "")) return(rep(TRUE, n))
  cols <- intersect(cols %||% character(), colnames(tbl))
  # Nothing to search in is not the same as nothing matching: a study
  # carrying none of the declared columns cannot answer the question, and
  # blanking its panel would be a worse answer than ignoring the box.
  if (!length(cols)) return(rep(TRUE, n))
  needle <- tolower(trimws(term))
  hit <- Reduce(`|`, lapply(cols, function(col) {
    grepl(needle, tolower(as.character(tbl[[col]])), fixed = TRUE)
  }))
  hit[is.na(hit)] <- FALSE
  hit
}

#' The hit count a search control reports
#'
#' Counted on the SCOPED dm, so it is this patient's records rather than the
#' cohort's -- the box sits in this patient's panel.
#'
#' @param ctrl The control declaration (carries `columns`).
#' @param dm_obj The subject-scoped dm.
#' @param tables The viz's declared tables.
#' @param term The search term.
#' @return `list(n, total)`, or `NULL` when nothing is being searched.
#' @noRd
pp_ctrl_search_hits <- function(ctrl, dm_obj, tables, term) {
  if (!nzchar(term %||% "")) return(NULL)
  tbls <- dm::dm_get_tables(dm_obj)
  nm <- intersect(tables, names(tbls))
  if (!length(nm)) return(NULL)
  tbl <- as.data.frame(tbls[[nm[[1L]]]])
  if (!nrow(tbl)) return(list(n = 0L, total = 0L))
  list(n = sum(pp_search_match(tbl, ctrl$columns, term)),
       total = nrow(tbl))
}

#' The magnifier the search control and the caption's chip share
#' @noRd
pp_search_icon <- function() {
  paste0(
    '<svg class="pp-search-icon" xmlns="http://www.w3.org/2000/svg" ',
    'width="10" height="10" fill="currentColor" viewBox="0 0 16 16" ',
    'aria-hidden="true">',
    '<path d="M11.742 10.344a6.5 6.5 0 1 0-1.397 1.398h-.001q.044.06.098',
    '.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1 1 0 0 0-.115-.1zM12 ',
    '6.5a5.5 5.5 0 1 1-11 0 5.5 5.5 0 0 1 11 0"/></svg>'
  )
}
