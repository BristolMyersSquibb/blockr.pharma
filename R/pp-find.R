# The panel's find control: picks, matching, options
#
# The find box used to hold one string, substring-matched against every coding
# level the study carries. It holds a SET now, and each pick names the column
# it came from:
#
#   list(col = "AEBODSYS", value = "INFECTIONS AND INFESTATIONS")  exact
#   list(col = "*",        value = "erythem")                      substring
#
# The free-text pick is the old box, unchanged, and a board saved before this
# change restores as exactly one of them (pp_find_picks()). Matching is an OR
# over the picks, which is the same shape one term always had -- one arm.
#
#   pp_find_picks()   -- normalise a stored or wire value into a pick list
#   pp_find_match()   -- the row filter
#   pp_find_options() -- the pickable rows, grouped by coding level, with counts
#   pp_find_hits()    -- matching / total, for the trigger's count
#   pp_find_labels()  -- the picks as display text, for captions and empty states

#' The free-text pick's column
#'
#' Not a column name, and it cannot collide with one: ADaM names are
#' `[A-Z0-9_]`.
#' @noRd
PP_FIND_FREE <- "*"

#' Normalise a find setting into a list of picks
#'
#' Takes what the client sends (an array of objects, so a list of lists once
#' Shiny has decoded it), what a board restores (the same, through the io
#' format registry) and what a board saved BEFORE this control existed
#' restores (a single string, which becomes one free-text pick).
#'
#' Anything unrecognised is dropped rather than erroring. A saved board is
#' data from a previous version of this package, and a filter it cannot read
#' is a filter that should not be applied -- not a profile that fails to open.
#'
#' @param value The raw setting.
#' @return An unnamed list of `list(col, value)`, both character scalars,
#'   de-duplicated and in the order they were picked.
#' @noRd
pp_find_picks <- function(value) {
  if (is.null(value) || !length(value)) return(list())

  one <- function(x) {
    # A bare string is the old box, or a client that sent just a term.
    if (is.character(x) && length(x) == 1L) {
      if (!nzchar(trimws(x))) return(NULL)
      return(list(col = PP_FIND_FREE, value = as.character(x)))
    }
    if (!is.list(x)) return(NULL)
    col <- as.character(x$col %||% PP_FIND_FREE)[1L]
    val <- x$value %||% x$val
    if (is.null(val) || !length(val)) return(NULL)
    val <- as.character(val)[1L]
    if (is.na(col) || is.na(val) || !nzchar(trimws(val))) return(NULL)
    list(col = col, value = val)
  }

  raw <- if (is.character(value)) as.list(value) else as.list(value)
  picks <- Filter(Negate(is.null), lapply(raw, one))
  # The same term picked twice is one filter. Keyed on both halves: the same
  # word can legitimately be a preferred term and a high level term.
  keys <- vapply(picks, function(p) paste0(p$col, "\r", toupper(p$value)),
                 character(1))
  unname(picks[!duplicated(keys)])
}

#' Are there any picks?
#' @noRd
pp_find_active <- function(value) length(pp_find_picks(value)) > 0L

#' The row filter
#'
#' An OR over the picks: a row survives if it matches any of them. No picks
#' matches everything, which is the unfiltered panel.
#'
#' A pick naming a column the data does not carry matches NOTHING, unlike
#' [pp_search_match()], which ignores a search it has no column for. The two
#' cases are different: a study carrying none of the four coding levels cannot
#' answer a typed term at all, while a pick that names `AEHLT` is a specific
#' question about a column that has gone. Blanking the panel is the honest
#' answer there, and the empty state names the picks and offers to clear them.
#'
#' @param tbl A data frame of records.
#' @param picks A pick list, from [pp_find_picks()].
#' @param cols Columns the free-text pick matches against.
#' @return A logical vector, one per row of `tbl`.
#' @noRd
pp_find_match <- function(tbl, picks, cols) {
  n <- nrow(tbl)
  picks <- pp_find_picks(picks)
  if (!length(picks)) return(rep(TRUE, n))
  Reduce(`|`, lapply(picks, function(p) {
    if (identical(p$col, PP_FIND_FREE)) {
      return(pp_search_match(tbl, cols, p$value))
    }
    if (!p$col %in% colnames(tbl)) return(rep(FALSE, n))
    hit <- toupper(trimws(as.character(tbl[[p$col]]))) ==
      toupper(trimws(p$value))
    hit[is.na(hit)] <- FALSE
    hit
  }))
}

#' The rows the picker offers, grouped by coding level
#'
#' Built from THIS patient's records, so the counts answer "how many of this
#' patient's events are cardiac" before anything is picked and before the
#' panel redraws. The list travels with the panel header (a few hundred bytes
#' for a typical patient), which is what lets the popover open and filter
#' without a round trip.
#'
#' The verbatim level is deliberately not offered by any caller: it is close
#' to one distinct value per record, so it is the one level whose list grows
#' with the patient rather than saturating on the coding dictionary. It stays
#' reachable by typing, which is how a verbatim term is looked for anyway.
#'
#' Values are ordered the way the chart orders its lanes (alphabetically, see
#' `sort(unique(tbl$..lane))` in the gantt renders), so the list a reader
#' scans is in the order of the panel they are reading.
#'
#' @param tbl A data frame of this patient's records.
#' @param levels Named character vector, column -> display label, coarsest
#'   first. Columns the study does not carry are skipped.
#' @param max_options Cap per level. Past it the group reports how many it
#'   left out and the popover asks the reader to keep typing.
#' @return An unnamed list of groups, each `list(col, label, options,
#'   truncated)`; groups with nothing in them are dropped.
#' @noRd
pp_find_options <- function(tbl, levels, max_options = 200L) {
  if (is.null(levels) || !length(levels)) return(list())
  cols <- names(levels)
  if (is.null(cols)) cols <- unname(levels)
  groups <- lapply(seq_along(levels), function(i) {
    col <- cols[[i]]
    if (!col %in% colnames(tbl)) return(NULL)
    v <- as.character(tbl[[col]])
    v <- v[!is.na(v) & nzchar(trimws(v))]
    if (!length(v)) return(NULL)
    tt <- table(v)
    ord <- order(names(tt))
    keep <- if (length(ord) > max_options) ord[seq_len(max_options)] else ord
    list(
      col = col,
      label = unname(levels[[i]]),
      options = unname(lapply(keep, function(k) {
        list(value = names(tt)[k], n = unname(as.integer(tt[k])))
      })),
      truncated = max(0L, length(ord) - length(keep))
    )
  })
  unname(Filter(Negate(is.null), groups))
}

#' How many of this patient's records the picks keep
#'
#' Counted on the SCOPED dm, like the hit count the find box printed before
#' it: the control sits in this patient's panel.
#'
#' @param ctrl The control declaration (carries `columns`).
#' @param dm_obj The subject-scoped dm.
#' @param tables The viz's declared tables.
#' @param picks A pick list.
#' @return `list(n, total)`, or `NULL` when nothing is being filtered.
#' @noRd
pp_find_hits <- function(ctrl, dm_obj, tables, picks) {
  if (!length(pp_find_picks(picks))) return(NULL)
  tbl <- pp_find_table(dm_obj, tables)
  if (is.null(tbl)) return(NULL)
  if (!nrow(tbl)) return(list(n = 0L, total = 0L))
  list(n = sum(pp_find_match(tbl, picks, ctrl$columns)),
       total = nrow(tbl))
}

#' The viz's first declared table, as a data frame
#'
#' Both callers want the same thing and neither should care that a dm's
#' tables can be lazy. Returns `NULL` when the study does not carry it.
#' @noRd
pp_find_table <- function(dm_obj, tables) {
  tbls <- dm::dm_get_tables(dm_obj)
  nm <- intersect(tables, names(tbls))
  if (!length(nm)) return(NULL)
  as.data.frame(tbls[[nm[[1L]]]])
}

#' The picks as display text
#'
#' For the sidebar caption, the empty state and the trigger's tooltip. A
#' coded term goes through [pp_term_label()] like the rest of the panel's
#' text; a free-text pick is quoted, because "erythem" is a fragment and
#' printing it bare would read as a term the study uses.
#'
#' @param picks A pick list.
#' @return A character vector, one per pick.
#' @noRd
pp_find_labels <- function(picks) {
  vapply(pp_find_picks(picks), function(p) {
    if (identical(p$col, PP_FIND_FREE)) {
      paste0("\u201c", p$value, "\u201d")
    } else {
      pp_term_label(p$value)
    }
  }, character(1))
}

#' The picks as one line
#'
#' The caption has room for one pick and a count of the rest; the tooltip and
#' the empty state print them all.
#' @param picks A pick list.
#' @param max_shown How many to name before counting the remainder.
#' @return A single string, or `""` when there are no picks.
#' @noRd
pp_find_summary <- function(picks, max_shown = 1L) {
  labs <- pp_find_labels(picks)
  if (!length(labs)) return("")
  if (length(labs) <= max_shown) return(paste(labs, collapse = " \u00b7 "))
  paste0(paste(labs[seq_len(max_shown)], collapse = " \u00b7 "),
         " +", length(labs) - max_shown)
}

#' The empty panel's sentence
#'
#' The old one named the term ("No adverse event matches ..."). With a
#' set there is no one term to name, and the reader needs two things the old
#' sentence did not have to carry: how many records were filtered away, and
#' what filtered them -- because picks survive a patient switch, so this is a
#' state a reviewer paging through a cohort lands in constantly and has to be
#' able to get out of.
#'
#' @param total Records this patient has in the panel's table.
#' @param picks The pick list.
#' @param noun What one record is called ("event", "medication").
#' @return A single string for [pp_empty_chart()].
#' @noRd
pp_find_empty_msg <- function(total, picks, noun = "record") {
  labs <- pp_find_labels(picks)
  n <- length(labs)
  paste0(
    "None of this patient's ", total, " ", noun,
    if (total == 1L) "" else "s",
    " match ",
    if (n == 1L) "" else paste0("any of your ", n, " filters: "),
    paste(labs, collapse = " \u00b7 ")
  )
}

#' Bring a saved board's per-viz settings up to date
#'
#' The gantt panels held their filter under `search`, as one string. They hold
#' it under `find`, as a pick list. A board saved before the change carries the
#' old key, and it restores as exactly one free-text pick -- which is what that
#' box always was.
#'
#' Done in the constructor, so nothing downstream has to know there were ever
#' two shapes. The old key is dropped rather than kept in step: a setting that
#' exists in two places is a setting that will disagree in one of them.
#'
#' @param viz_settings The `viz_settings` constructor argument.
#' @return The same list, with any `search` entry rewritten to `find`.
#' @noRd
pp_migrate_viz_settings <- function(viz_settings) {
  if (!is.list(viz_settings) || !length(viz_settings)) return(viz_settings)
  lapply(viz_settings, function(one) {
    if (!is.list(one) || !"search" %in% names(one)) return(one)
    picks <- pp_find_picks(one$search)
    one$search <- NULL
    # An empty box was not a filter, so it does not become one.
    if (length(picks)) one$find <- picks
    one
  })
}

#' The cohort's vocabulary, for terms this patient does not have
#'
#' The picker's list is built from the patient's own records, which is what
#' makes its counts mean something -- and what makes it useless for the
#' question "does this study code anything as pneumonia". A reviewer arming a
#' filter to page through the cohort with needs the terms the COHORT carries,
#' not the ones in front of them.
#'
#' So the cohort list is a second source, fetched once and only when the
#' reader types: unfiltered it is several hundred rows and would bury the
#' patient's eight. It is not shipped with the header for the reason the
#' patient's list is -- 24kB against that list's 500 bytes, and it does not
#' shrink with the study, because it is bounded by the coding dictionary
#' rather than by the record count.
#'
#' `n` counts PATIENTS, not records: for a term this patient does not have,
#' the useful number is how much of the cohort does.
#'
#' @param tbl The COHORT's records (the unscoped dm's table).
#' @param levels Named character vector, column -> display label.
#' @param max_options Cap per level.
#' @return The [pp_find_options()] shape, with patient counts.
#' @noRd
pp_find_vocab <- function(tbl, levels, max_options = 400L) {
  if (is.null(tbl) || !nrow(tbl) || !length(levels %||% character())) {
    return(list())
  }
  cols <- names(levels) %||% unname(levels)
  ids <- if ("USUBJID" %in% colnames(tbl)) {
    as.character(tbl$USUBJID)
  } else {
    seq_len(nrow(tbl))
  }
  groups <- lapply(seq_along(levels), function(i) {
    col <- cols[[i]]
    if (!col %in% colnames(tbl)) return(NULL)
    v <- as.character(tbl[[col]])
    keep <- !is.na(v) & nzchar(trimws(v))
    if (!any(keep)) return(NULL)
    # One row per (term, patient), so the count is patients and not records.
    pairs <- unique(data.frame(v = v[keep], id = ids[keep],
                               stringsAsFactors = FALSE))
    tt <- table(pairs$v)
    ord <- order(names(tt))
    keep_i <- if (length(ord) > max_options) ord[seq_len(max_options)] else ord
    list(
      col = col,
      label = unname(levels[[i]]),
      options = unname(lapply(keep_i, function(k) {
        list(value = names(tt)[k], n = unname(as.integer(tt[k])))
      })),
      truncated = max(0L, length(ord) - length(keep_i))
    )
  })
  unname(Filter(Negate(is.null), groups))
}
