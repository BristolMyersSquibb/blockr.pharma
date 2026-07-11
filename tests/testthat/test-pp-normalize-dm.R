# A study shaped from SDTM ships the treatment window as reference-exposure
# timestamps (RFXSTDTC/RFXENDTC, ISO 8601 character) rather than ADaM's
# TRTSDT/TRTEDT. pp_normalize_dm() reconciles that once, dm-wide, before
# anything reads the dm — a per-viz alias would resolve too late for
# pp_compute_ref_ms() and pp_compute_time_range(), and could not coerce the
# character value to a Date anyway.

sdtm_adsl <- function(...) {
  data.frame(
    USUBJID = "x",
    RFXSTDTC = "2020-01-01T10:30",   # with a time part, as SDTM allows
    RFXENDTC = "2020-06-01",
    ...,
    stringsAsFactors = FALSE
  )
}

test_that("TRTSDT/TRTEDT are derived from the exposure timestamps", {
  out <- pp_derive_adsl_dates(dm::dm(adsl = sdtm_adsl()))
  adsl <- as.data.frame(dm::dm_get_tables(out)$adsl)
  expect_identical(adsl$TRTSDT, as.Date("2020-01-01"))
  expect_identical(adsl$TRTEDT, as.Date("2020-06-01"))
  # coerced, not merely renamed: the profile does date arithmetic on these
  expect_s3_class(adsl$TRTSDT, "Date")
})

test_that("RFSTDTC/RFENDTC are the fallback when the exposure dates are absent", {
  adsl <- data.frame(USUBJID = "x", RFSTDTC = "2020-02-02",
                     RFENDTC = "2020-07-07", stringsAsFactors = FALSE)
  out <- as.data.frame(
    dm::dm_get_tables(pp_derive_adsl_dates(dm::dm(adsl = adsl)))$adsl
  )
  expect_identical(out$TRTSDT, as.Date("2020-02-02"))
  expect_identical(out$TRTEDT, as.Date("2020-07-07"))
})

test_that("the exposure dates win over the weaker reference dates", {
  adsl <- sdtm_adsl(RFSTDTC = "1999-01-01", RFENDTC = "1999-12-31")
  out <- as.data.frame(
    dm::dm_get_tables(pp_derive_adsl_dates(dm::dm(adsl = adsl)))$adsl
  )
  expect_identical(out$TRTSDT, as.Date("2020-01-01"))
})

test_that("an existing TRTSDT is never overwritten", {
  adsl <- sdtm_adsl(TRTSDT = as.Date("2011-11-11"))
  out <- as.data.frame(
    dm::dm_get_tables(pp_derive_adsl_dates(dm::dm(adsl = adsl)))$adsl
  )
  expect_identical(out$TRTSDT, as.Date("2011-11-11"))
  expect_identical(out$TRTEDT, as.Date("2020-06-01"))  # still derived
})

test_that("a conformant ADaM dm passes through untouched", {
  skip_if_not_installed("pharmaverseadam")
  dm_obj <- dm::dm(adsl = as.data.frame(pharmaverseadam::adsl))
  expect_identical(pp_derive_adsl_dates(dm_obj), dm_obj)
})

test_that("the derived dates reach ref_ms and the time range", {
  # The reason this is dm-wide rather than a per-viz alias: these two read the
  # columns straight off the dm.
  dm_obj <- pp_normalize_dm(dm::dm(adsl = sdtm_adsl()))
  expect_false(is.na(pp_compute_ref_ms(dm_obj)))
  expect_equal(pp_compute_time_range(dm_obj),
               as.Date(c("2020-01-01", "2020-06-01")))
})

test_that("table and column normalization compose", {
  # short prod table names AND a SDTM adsl, in one pass
  raw <- dm::dm(
    adsl = sdtm_adsl(),
    ae = data.frame(USUBJID = "x", AEDECOD = "HEADACHE", ASTDY = 5L,
                    stringsAsFactors = FALSE)
  )
  out <- pp_normalize_dm(raw)
  tbls <- dm::dm_get_tables(out)
  expect_true("adae" %in% names(tbls))          # ae -> adae
  expect_true("TRTSDT" %in% colnames(as.data.frame(tbls$adsl)))

  # and the profile can now render the AE gantt off it
  res <- pp_resolve_requires(out, ae_gantt_viz)
  expect_true(isTRUE(res$ok))
})

test_that("pp_as_date tolerates ISO timestamps, blanks and typed input", {
  expect_identical(pp_as_date("2013-02-15"), as.Date("2013-02-15"))
  expect_identical(pp_as_date("2013-02-15T10:30"), as.Date("2013-02-15"))
  expect_true(is.na(pp_as_date("")))
  expect_identical(pp_as_date(as.Date("2013-02-15")), as.Date("2013-02-15"))
})
