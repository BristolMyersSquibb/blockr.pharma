# Reading a findings table for display
#
# One seam for adlb/adlbc/adlbh, advs, adqs* and adeg when it gets a consumer.
#
# It does NOT filter. A study's derived records -- LOCF, replicate averages,
# per-patient extremes -- are there because a statistician followed a
# pre-specified rule, and a sponsor who prepared the ADaM knows their data
# better than this package does. Deciding which of those rows deserve to
# reach the screen is not a rendering decision.
#
# What it does is make the read total: a value the renderer will do
# arithmetic on comes back numeric, or NA, never a crash and never a level
# code.

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
#' @param dm_obj A normalized dm.
#' @param table_name Table to read.
#' @param num_cols Columns the renderer does arithmetic on.
#' @return A data.frame, or `NULL` if the table is absent.
#' @noRd
pp_prepare_findings <- function(dm_obj, table_name,
                                num_cols = c("AVAL", "CHG", "A1LO", "A1HI")) {
  tbls <- dm::dm_get_tables(dm_obj)
  if (!table_name %in% names(tbls)) return(NULL)

  tbl <- as.data.frame(tbls[[table_name]])
  for (nm in intersect(num_cols, colnames(tbl))) {
    tbl[[nm]] <- pp_as_numeric(tbl[[nm]])
  }
  tbl
}
