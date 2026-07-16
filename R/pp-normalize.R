# One dm-wide normalization pass: the study's names become the names the
# profile's vizs declare against, ONCE, before anything reads the dm.
#
# Two catalogs drive it. pp_table_catalog() maps canonical ADaM table names
# to the SDTM-style domain names a study may ship instead (adsl <- dm,
# adae <- ae, ...). pp_column_catalog() maps, per canonical table, canonical
# ADaM columns to their SDTM (or vendor) sources, with a type on entries a
# rename cannot express: SDTM `*DTC` is character by CDISC definition and may
# carry a time part, so a date-typed entry coerces through pp_as_date().
#
# Everything downstream -- the subject picker, pp_compute_time_range(),
# pp_compute_ref_ms(), role resolution, the viz requires checks and the
# renders -- assumes this pass has run. Resolving late (per viz, as
# pp_resolve_requires() once did) is what silently clipped the time axis for
# studies shipping ASTDTC/ADTM: the axis was computed before the aliases
# resolved. Resolving once, up front, fixes that class of bug by
# construction.
#
# Both passes derive (add the canonical column, keep the source) rather than
# rename: two canonicals may legitimately read the same source (TRTEDT and
# RFENDT both fall back to RFENDTC), and an existing canonical column always
# wins -- nothing is ever overwritten.

#' Canonical table name -> SDTM-style aliases
#'
#' A study may ship ADaM-shaped columns under short SDTM-style domain names,
#' or real SDTM domains. Either way the table answering to a viz declaration
#' is found here. `adex` / `adeg` have no consumer yet (see the completeness
#' review in the study-metadata spec); their entries cost nothing and spare
#' the next viz a catalog change.
#'
#' The ADaM split tables (adlbc/adlbh, adqsadas/adqsnpix) have NO alias on
#' purpose: they are derivations (a split by LBCAT / QSCAT), not renamings,
#' and a rename pass must not pretend otherwise.
#'
#' @return Named list of canonical -> alias character vectors.
#' @noRd
pp_table_catalog <- function() {
  list(
    adsl = "dm",
    adae = "ae",
    adcm = "cm",
    adex = "ex",
    adlb = "lb",
    advs = "vs",
    adeg = "eg"
  )
}

#' Canonical column sources, per canonical table
#'
#' Each entry is `canonical = list(from = c(source, ...), type = ...)`;
#' sources are tried in order, the first present wins. `type = "date"`
#' coerces through [pp_as_date()] (SDTM `*DTC` character timestamps, ADaM
#' `*DTM` datetimes); `"identity"` copies as-is.
#'
#' Only columns a viz, the timeline, or a role actually reads are listed --
#' this is a consumption catalog, not a CDISC mapping exercise. Value
#' derivations (VSORRES -> AVAL unit handling, QS category splits) do not
#' belong here; `*STRESN` is the standardized numeric result and maps
#' name-to-name.
#'
#' @return Named list: table -> (canonical -> spec).
#' @noRd
pp_column_catalog <- function() {
  d <- function(...) list(from = c(...), type = "date")
  i <- function(...) list(from = c(...), type = "identity")
  findings_visits <- function() {
    list(AVISIT = i("VISIT"), AVISITN = i("VISITNUM"))
  }
  c_adlb <- c(
    list(
      PARAMCD = i("LBTESTCD"),
      PARAM   = i("LBTEST"),
      AVAL    = i("LBSTRESN"),
      ADT     = d("ADTM", "LBDTC"),
      ADY     = i("LBDY"),
      A1LO    = i("LBSTNRLO"),
      A1HI    = i("LBSTNRHI"),
      ANRIND  = i("LBNRIND")
    ),
    findings_visits()
  )
  list(
    adsl = list(
      TRTSDT = d("RFXSTDTC", "RFSTDTC"),
      TRTEDT = d("RFXENDTC", "RFENDTC"),
      RFENDT = d("EOSDT", "RFENDTC"),
      DTHDT  = d("DTHDTC")
    ),
    adae = list(
      ASTDT    = d("ASTDTC", "AESTDTC"),
      AENDT    = d("AENDTC", "AEENDTC"),
      ASTDY    = i("AESTDY"),
      AENDY    = i("AEENDY"),
      AEBODSYS = i("AESOC")
    ),
    advs = c(
      list(
        PARAMCD = i("VSTESTCD"),
        PARAM   = i("VSTEST"),
        AVAL    = i("VSSTRESN"),
        ADT     = d("ADTM", "VSDTC"),
        ADY     = i("VSDY"),
        ATPT    = i("VSPOS")
      ),
      findings_visits()
    ),
    adlb = c_adlb,
    # The ADaM split lab tables may ship ADTM without ADT; same for the
    # questionnaire tables. Only the date entry applies -- their other
    # columns are ADaM-native by construction.
    adlbc = list(ADT = d("ADTM")),
    adlbh = list(ADT = d("ADTM")),
    adqsadas = list(ADT = d("ADTM")),
    adqsnpix = list(ADT = d("ADTM")),
    adcm = list(
      ASTDT = d("ASTDTC", "CMSTDTC"),
      AENDT = d("AENDTC", "CMENDTC"),
      ASTDY = i("CMSTDY"),
      AENDY = i("CMENDY")
    ),
    adex = list(
      ASTDT = d("ASTDTC", "EXSTDTC"),
      AENDT = d("AENDTC", "EXENDTC"),
      ASTDY = i("EXSTDY"),
      AENDY = i("EXENDY")
    )
  )
}

#' Coerce an ISO 8601 value to a Date
#'
#' SDTM `*DTC` variables are character by definition and may carry a time
#' part (`2013-02-15T10:30`) or be PARTIAL (`2013-02`, `2013` -- reduced
#' precision is legal SDTM). Tolerates all of that, the empty string, and
#' values that are already `Date`/`POSIXt`. Must be total over a whole
#' column: `as.Date()` on a character vector *errors* on the first partial
#' value, which would kill normalization for a study that dated one AE to a
#' month.
#'
#' Partial dates come back `NA` on purpose: imputing them is an analysis
#' decision (and ADaM's job), not a rename's.
#'
#' @param x Character, Date or POSIXt vector.
#' @return A `Date` vector, `NA` where `x` is missing, partial or
#'   unparseable.
#' @noRd
pp_as_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  # First 10 chars = the full-precision date part, with or without a time
  # part behind it; format= makes non-matching (partial) values NA instead
  # of an error.
  as.Date(substr(as.character(x), 1L, 10L), format = "%Y-%m-%d")
}

#' Reconcile a study's dm with the names the vizs declare against
#'
#' The single normalization seam: table aliases first (so the column catalog
#' finds its tables under canonical names), then the column catalog with its
#' typed coercions. Run once per incoming dm, before anything reads it.
#'
#' The rebuilt dm is flat (keys and FKs are dropped). Nothing downstream
#' needs them: renders pull tables by name, and subject scoping filters each
#' table on its own USUBJID column (see [pp_scope_subject()]) rather than
#' through an FK cascade.
#'
#' @param dm_obj A `dm` object.
#' @return A `dm`. Unchanged (same object) when nothing applied.
#' @noRd
pp_normalize_dm <- function(dm_obj) {
  tbls <- dm::dm_get_tables(dm_obj)
  changed <- FALSE

  for (canonical in names(pp_table_catalog())) {
    if (canonical %in% names(tbls)) next
    alias <- pp_table_catalog()[[canonical]]
    hit <- alias[alias %in% names(tbls)]
    if (length(hit)) {
      names(tbls)[names(tbls) == hit[[1L]]] <- canonical
      changed <- TRUE
    }
  }

  catalog <- pp_column_catalog()
  for (tbl_name in intersect(names(catalog), names(tbls))) {
    df <- as.data.frame(tbls[[tbl_name]])
    for (canonical in names(catalog[[tbl_name]])) {
      if (canonical %in% colnames(df)) next
      spec <- catalog[[tbl_name]][[canonical]]
      hit <- spec$from[spec$from %in% colnames(df)]
      if (!length(hit)) next
      val <- df[[hit[[1L]]]]
      df[[canonical]] <- if (identical(spec$type, "date")) {
        pp_as_date(val)
      } else {
        val
      }
      changed <- TRUE
    }
    tbls[[tbl_name]] <- df
  }

  if (!changed) return(dm_obj)
  do.call(dm::dm, lapply(tbls, as.data.frame))
}

#' Which table holds the subjects?
#'
#' `adsl` after normalization; on a raw dm (the block's `expr` runs against
#' its untouched input) the SDTM `dm` domain answers instead. `NULL` when
#' neither is present -- callers stay total.
#'
#' @param tbl_names Character vector of table names.
#' @return A single table name, or `NULL`.
#' @noRd
pp_subject_tbl_name <- function(tbl_names) {
  if ("adsl" %in% tbl_names) return("adsl")
  aliases <- pp_table_catalog()[["adsl"]]
  hit <- aliases[aliases %in% tbl_names]
  if (length(hit)) hit[[1L]] else NULL
}

#' Scope a normalized dm to one subject
#'
#' Filters every table carrying a `USUBJID` column down to `subject`. This
#' replaces the `dm::dm_filter()` FK cascade for the profile's internal
#' scoping: every CDISC table carries `USUBJID` outright, the normalized dm
#' is flat (no keys to cascade over), and a plain filter has no ordering
#' constraint against the normalization pass. The block's *output* (for
#' downstream blocks) still goes through `dm_filter()` on the raw, keyed
#' input -- see the block's `expr`.
#'
#' @param dm_obj A (normalized) `dm` object.
#' @param subject A single USUBJID.
#' @return A `dm` holding only that subject's rows.
#' @noRd
pp_scope_subject <- function(dm_obj, subject) {
  tbls <- dm::dm_get_tables(dm_obj)
  scoped <- lapply(tbls, function(tbl) {
    df <- as.data.frame(tbl)
    if (!"USUBJID" %in% colnames(df)) return(df)
    df[as.character(df$USUBJID) %in% subject, , drop = FALSE]
  })
  do.call(dm::dm, scoped)
}
