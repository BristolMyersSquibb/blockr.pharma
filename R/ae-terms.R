# Adverse event term levels for a reader to choose from.
#
# An AE table or chart groups by a high level term (the system organ class or
# a level below it) and a low level term (the preferred term or a level below
# it). Which columns those are is the reader's choice, offered as two selects
# in a block's script. The two functions here are what such a script calls:
# one for the choices, one to copy the picked columns to fixed names the block
# groups by. Keeping them here gives every script the same two lists, the same
# labels and the same fallback.
#
# The choices are labels, not column names, so the select and the subtitle
# word read "Dictionary-Derived Term" rather than "AEDECOD". A saved pick is a
# label too, which is why the labels are fixed here instead of read off the
# data: a study whose column labels differ keeps its saved picks.
#
# A script that hands column names to its block (a composer table takes them
# as arguments) uses `unname()` on the choices and skips the copy.

AE_TERM_LEVELS <- list(
  high = c(
    "Body System or Organ Class" = "AEBODSYS",
    "Primary System Organ Class" = "AESOC",
    "High Level Group Term" = "AEHLGT",
    "High Level Term" = "AEHLT"
  ),
  low = c(
    "Dictionary-Derived Term" = "AEDECOD",
    "Lowest Level Term" = "AELLT",
    "Reported Term" = "AETERM"
  )
)

#' Adverse event term levels
#'
#' The columns an adverse event table or chart can group by, at the high level
#' (system organ class and the levels below it) and at the low level
#' (preferred term and the levels below it). `ae_term_choices()` lists them as
#' label-to-column pairs for a select in a block's script, and
#' `ae_term_columns()` copies the picked columns to the fixed names the block
#' groups by.
#'
#' The first choice is the default: Body System or Organ Class (`AEBODSYS`)
#' and Dictionary-Derived Term (`AEDECOD`).
#'
#' @param level `"high"` or `"low"`.
#' @param data A data frame, or `NULL`. With a data frame, only the columns it
#'   has are offered. If it has none of them, all are offered, so the select
#'   still draws while the data is not there yet.
#'
#' @return `ae_term_choices()`: a named character vector, labels as names and
#'   column names as values, in the order offered.
#'
#' @examples
#' ae_term_choices("high")
#'
#' ae <- data.frame(USUBJID = "01", AEBODSYS = "Cardiac disorders",
#'                  AEDECOD = "Palpitations", AELLT = "Heart racing")
#' ae_term_choices("low", ae)
#'
#' # In a block's script: two selects whose choices are the labels.
#' # .high <- blockr.pharma::ae_term_choices("high", data)
#' # .low <- blockr.pharma::ae_term_choices("low", data)
#' # high_level_term <- factor(names(.high)[1], levels = names(.high))
#' # low_level_term <- factor(names(.low)[1], levels = names(.low))
#' # data <- blockr.pharma::ae_term_columns(data, high_level_term, low_level_term)
#'
#' ae_term_columns(ae, "Body System or Organ Class", "Lowest Level Term")
#'
#' @export
ae_term_choices <- function(level = c("high", "low"), data = NULL) {
  level <- match.arg(level)
  all <- AE_TERM_LEVELS[[level]]
  if (!is.data.frame(data)) {
    return(all)
  }
  have <- all[all %in% names(data)]
  if (length(have)) have else all
}

#' @rdname ae_term_choices
#'
#' @param high,low The picked high and low level term: a label from
#'   `ae_term_choices()` or a column name, as a string or a one-element factor.
#'   `NULL` leaves that level out. A pick the data does not have falls back to
#'   the first column of that level the data has.
#' @param names The columns to write, for the high and the low level.
#'
#' @return `ae_term_columns()`: `data` with the two columns added (or
#'   overwritten), each carrying the picked label as its `label` attribute, so
#'   a table header or an axis reads the term's name. If the data has no
#'   column of a level, that column is not written.
#'
#' @export
ae_term_columns <- function(data, high = NULL, low = NULL,
                            names = c(high = "SOC", low = "PT")) {
  stopifnot(
    is.data.frame(data),
    is.character(names), length(names) == 2L,
    setequal(base::names(names), c("high", "low"))
  )
  picks <- list(high = high, low = low)
  for (lvl in c("high", "low")) {
    pick <- picks[[lvl]]
    if (is.null(pick)) next
    col <- ae_term_resolve(lvl, pick, data)
    if (is.na(col)) next
    v <- data[[col]]
    if (is.factor(v)) v <- as.character(v)
    attr(v, "label") <- names(col)
    data[[names[[lvl]]]] <- v
  }
  data
}

# The column behind a pick, named by its label. NA when the data has no
# column of the level at all.
ae_term_resolve <- function(level, pick, data) {
  pick <- as.character(pick)
  pick <- if (length(pick)) pick[[1L]] else NA_character_
  choices <- AE_TERM_LEVELS[[level]]
  have <- choices[choices %in% base::names(data)]
  hit <- have[!is.na(pick) & (base::names(have) == pick | have == pick)]
  if (length(hit)) {
    return(hit[1L])
  }
  # A column outside the lists, named directly: allowed, labelled by itself
  # or by its own label attribute.
  if (!is.na(pick) && pick %in% base::names(data)) {
    lab <- attr(data[[pick]], "label")
    if (!is.character(lab) || length(lab) != 1L) lab <- pick
    return(stats::setNames(pick, lab))
  }
  if (length(have)) have[1L] else NA_character_
}
