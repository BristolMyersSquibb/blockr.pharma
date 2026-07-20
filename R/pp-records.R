# Preparing a findings table for display
#
# Every findings-shaped domain -- adlb/adlbc/adlbh, advs, adqs*, and adeg when
# it gets a consumer -- reaches a viz through this file. One seam, so the
# answer to "which rows do we draw, and what did we drop?" lives in one place
# instead of being re-decided per chart.
#
# The problem it solves: an ADaM findings table does NOT hold one row per
# measurement. Alongside each collected result a study stores derived records
# -- per-patient extremes, carried-forward values, replicate means -- and
# flags them in DTYPE (blank DTYPE = the patient was actually measured).
# pharmaverseadam::adlb is 29% derived rows, and 32% of its
# (USUBJID, PARAMCD, ADT) keys carry more than one row. A renderer that
# selects on PARAMCD alone draws all of them, so a derived value appears on
# the chart as a measurement that never happened.
#
# Why not one blanket "drop everything with a DTYPE": derived records are not
# one kind of thing. See PP_DTYPE_POLICY below -- that table IS the decision,
# and it is meant to be read and argued with.

#' What we do with each kind of derived record
#'
#' `DTYPE` values are study-defined, but the CDISC-conventional ones fall into
#' three behaviours:
#'
#' - `"drop"`  -- the row restates a value that is already present as a
#'   collected record, usually re-stamped onto a pseudo-visit like
#'   `"POST-BASELINE MAXIMUM"`. Dropping it loses no information.
#' - `"keep"`  -- the row is the value a reviewer wants, and the rows it
#'   summarizes are the raw material. `AVERAGE` is the case that matters:
#'   when a nurse takes blood pressure twice, ADaM stores both readings plus
#'   their mean. Drop the mean and the chart shows two raw readings and no
#'   summary; keep all three and the chart averages the mean back in.
#' - `"mark"`  -- the row carries information the collected data does not
#'   (a value carried forward onto a visit where nothing was measured).
#'   Shown, but must be distinguishable from a measurement.
#'
#' Anything not listed falls to `PP_DTYPE_DEFAULT`, which is `"mark"`. An
#' unrecognized derivation is the case we understand least, so it is the case
#' where a reviewer -- not this file -- should decide whether the value counts.
#' Dropping it would make that decision for them, silently.
#'
#' Note the asymmetry with the `"drop"` entries above: those are not doubtful.
#' A `MAXIMUM` row is a value-identical copy of a collected row at the same
#' date, so showing it draws a second point exactly on top of the first --
#' no information gained, ~30% more points on the chart. That is
#' de-duplication, not hiding.
#'
#' @noRd
PP_DTYPE_POLICY <- c(
  # Restatements of a collected record.
  MAXIMUM     = "drop",
  MINIMUM     = "drop",
  LOV         = "drop",
  LAST        = "drop",
  CALCULATION = "drop",
  PHANTOM     = "drop",
  # Summaries whose inputs are also present.
  AVERAGE     = "keep",
  MEAN        = "keep",
  # Imputations -- information the collected rows do not carry.
  LOCF        = "mark",
  BLOCF       = "mark",
  WOCF        = "mark",
  INTERP      = "mark"
)

#' @noRd
PP_DTYPE_DEFAULT <- "mark"

#' Rows a findings viz should draw, and what was held back
#'
#' The single record-selection seam. Call this immediately after reading a
#' findings table out of the dm and before any filtering, aggregation or
#' plotting.
#'
#' Records with a `"keep"` policy suppress the rows they summarize: an
#' `AVERAGE` row and its replicates share a (PARAMCD, ADT) key, and once the
#' summary is kept its inputs must not also be drawn or the summary is counted
#' twice. This is the only place that relationship is encoded.
#'
#' Tables with no `DTYPE` column pass through untouched -- SDTM findings
#' domains have no derived records by construction, and a study that ships
#' ADaM without DTYPE is asserting the same thing.
#'
#' @param tbl A findings data.frame (adlb/advs/adqs*/adeg shaped).
#' @param key Columns identifying one measurement occasion. Rows sharing a key
#'   with a kept summary are treated as that summary's inputs.
#' @return The input with derived rows resolved, plus two attributes:
#'   `pp_suppressed` (integer count of rows not drawn) and `pp_marked`
#'   (logical vector, TRUE where a drawn row is derived rather than
#'   collected). Callers that render points should surface both.
#' @noRd
pp_select_records <- function(tbl,
                              key = c("USUBJID", "PARAMCD", "ADT")) {
  n0 <- nrow(tbl)
  if (!"DTYPE" %in% colnames(tbl) || n0 == 0L) {
    attr(tbl, "pp_suppressed") <- 0L
    attr(tbl, "pp_marked") <- rep(FALSE, n0)
    return(tbl)
  }

  dtype <- toupper(trimws(as.character(tbl$DTYPE)))
  collected <- is.na(dtype) | !nzchar(dtype)

  policy <- rep(PP_DTYPE_DEFAULT, length(dtype))
  hit <- match(dtype, names(PP_DTYPE_POLICY))
  policy[!is.na(hit)] <- PP_DTYPE_POLICY[hit[!is.na(hit)]]
  policy[collected] <- "collected"

  # A kept summary displaces the rows it summarizes.
  key <- intersect(key, colnames(tbl))
  displaced <- rep(FALSE, nrow(tbl))
  if (length(key) && any(policy == "keep")) {
    k <- do.call(paste, c(unname(as.list(tbl[key])), sep = "\r"))
    displaced <- k %in% k[policy == "keep"] & policy == "collected"
  }

  drawn <- (policy %in% c("collected", "keep", "mark")) & !displaced
  out <- tbl[drawn, , drop = FALSE]
  attr(out, "pp_suppressed") <- as.integer(n0 - nrow(out))
  attr(out, "pp_marked") <- policy[drawn] %in% c("keep", "mark")
  out
}

#' Coerce a findings column to numeric without inventing values
#'
#' `pp_column_catalog()` copies `AVAL`, `A1LO`, `A1HI` and the `*DY` columns
#' through as-is, which is correct against CDISC -- those source variables are
#' Num per the SDTM IG. Real deliveries are not always conformant: a CSV or
#' SAS round-trip stringifies numerics, and `round()` on a character column
#' aborts the whole card.
#'
#' Goes through `as.character()` first on purpose. `as.numeric()` on a factor
#' returns level codes, which would replace a crash with a silently wrong
#' chart -- strictly the worse failure.
#'
#' @param x A vector that should be numeric.
#' @return A numeric vector; unparseable entries become `NA`, never 0.
#' @noRd
pp_as_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(as.numeric(as.character(x)))
}

#' Read a findings table out of a dm, ready to draw
#'
#' The viz-facing entry point: record selection ([pp_select_records()]) plus
#' the numeric coercions the renderers assume. Every findings viz starts here.
#'
#' `pp_marked` becomes a `.pp_derived` COLUMN rather than staying an
#' attribute, because every caller subsets the table afterwards (by PARAMCD,
#' by visit, by window) and attributes do not survive `[`. A column does, and
#' stays aligned with its row through any number of filters.
#'
#' @param dm_obj A normalized dm.
#' @param table_name Table to read.
#' @param num_cols Columns the renderer does arithmetic on.
#' @return A data.frame with a `.pp_derived` logical column, carrying a
#'   `pp_suppressed` attribute. `NULL` if the table is absent.
#' @noRd
pp_prepare_findings <- function(dm_obj, table_name,
                                num_cols = c("AVAL", "CHG", "A1LO", "A1HI")) {
  tbls <- dm::dm_get_tables(dm_obj)
  if (!table_name %in% names(tbls)) return(NULL)

  tbl <- as.data.frame(tbls[[table_name]])
  tbl <- pp_select_records(tbl)
  suppressed <- attr(tbl, "pp_suppressed")
  tbl$.pp_derived <- attr(tbl, "pp_marked")

  for (nm in intersect(num_cols, colnames(tbl))) {
    tbl[[nm]] <- pp_as_numeric(tbl[[nm]])
  }

  attr(tbl, "pp_suppressed") <- suppressed
  tbl
}

#' Note for a card whose data had derived records held back
#'
#' Returns `NULL` when nothing was suppressed, so a clean card gets no
#' furniture. Rendering this is what makes the `"drop"` policy honest: the
#' alternative is a chart that silently decided which rows count.
#'
#' @param tbl Output of [pp_prepare_findings()].
#' @return A single string, or `NULL`.
#' @noRd
pp_suppressed_note <- function(tbl) {
  n <- attr(tbl, "pp_suppressed") %||% 0L
  if (!length(n) || is.na(n) || n < 1L) return(NULL)
  sprintf("%d derived record%s hidden", n, if (n == 1L) "" else "s")
}

#' Rows to use when several collapse into one number
#'
#' `"mark"` works for a chart that draws one point per row: the point is
#' shown, drawn hollow, and a reviewer decides what it is worth. It does not
#' work where rows collapse into a single value -- a heatmap cell, a radar
#' vertex, a reference band. There is no half-including a value in a mean, and
#' a blended observed-plus-imputed number is exactly the "value that appears
#' in no record" this file exists to prevent.
#'
#' So aggregation prefers collected rows: where a cell holds any measurement,
#' derived rows in that cell are ignored. Where it holds none, the derived
#' rows are all there is and get used -- a carried-forward value is better
#' than an empty cell, provided the caller marks it.
#'
#' A kept summary (`AVERAGE`) is unaffected: [pp_select_records()] has already
#' displaced the replicates it summarizes, so its cell holds one row.
#'
#' @param rows Rows falling in one aggregation cell.
#' @return The subset to aggregate over.
#' @noRd
pp_prefer_collected <- function(rows) {
  if (!".pp_derived" %in% colnames(rows) || nrow(rows) == 0L) return(rows)
  collected <- !rows$.pp_derived
  if (any(collected)) rows[collected, , drop = FALSE] else rows
}
