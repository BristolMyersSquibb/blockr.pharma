# The patient facts the profile marks as the patient timeline marks them
# (R/pp-marks.R): treatment-emergent AEs by default, last contact as where
# an ongoing record ends, death and the disposition in the header.

test_that("only treatment-emergent AEs stay, where the flag exists", {
  adae <- data.frame(USUBJID = "x", AEDECOD = c("A", "B", "C"),
                     TRTEMFL = c("Y", NA, "Y"), stringsAsFactors = FALSE)
  d <- dm::dm(adsl = data.frame(USUBJID = "x"), adae = adae)
  kept <- dm::dm_get_tables(pp_teae_only(d))$adae
  expect_identical(kept$AEDECOD, c("A", "C"))
  expect_true(pp_has_teae_flag(d))

  adae$TRTEMFL <- NULL
  d2 <- dm::dm(adsl = data.frame(USUBJID = "x"), adae = adae)
  expect_identical(pp_teae_only(d2), d2)
  expect_false(pp_has_teae_flag(d2))
})

test_that("an ongoing record ends at last contact", {
  adsl <- data.frame(
    USUBJID = "x", TRTSDT = as.Date("2020-01-01"),
    TRTEDT = as.Date("2020-01-26"), LSTALVDT = as.Date("2020-02-24"),
    stringsAsFactors = FALSE
  )
  adae <- data.frame(USUBJID = "x", ASTDT = as.Date("2020-01-23"),
                     AENDT = as.Date("2020-06-15"), stringsAsFactors = FALSE)
  d <- dm::dm(adsl = adsl, adae = adae)
  tr <- pp_compute_time_range(d)
  expect_identical(attr(tr, "last_contact"), as.Date("2020-02-24"))
  ref <- pp_compute_ref_ms(d)
  # Day 55, not the window's end in June
  expect_equal(pp_gantt_open_end(23, tr, ref, "rday", 1), 55)
  # Without LSTALVDT, the last dose
  adsl$LSTALVDT <- NULL
  tr2 <- pp_compute_time_range(dm::dm(adsl = adsl, adae = adae))
  expect_identical(attr(tr2, "last_contact"), as.Date("2020-01-26"))
})

test_that("death: ADSL date, else a fatal AE's end, else nothing", {
  sl <- data.frame(USUBJID = "x", DTHDT = as.Date("2020-02-01"))
  expect_identical(pp_death_at(sl)$date, as.Date("2020-02-01"))
  sl$DTHDT <- as.Date(NA)
  ae <- data.frame(AESDTH = c("N", "Y"), AENDY = c(4, 9))
  expect_equal(pp_death_at(sl, ae)$day, 9)
  sl$DTHFL <- "Y"
  expect_null(pp_death_at(sl))
  expect_true(pp_death_undated(sl))
})

test_that("the header says the disposition and an undated death", {
  frame <- data.frame(
    USUBJID = c("a", "b"), TRTSDT = as.Date("2020-01-01"),
    TRTDURD = c(26, 30), EOSSTT = c("DISCONTINUED", "COMPLETED"),
    RFENDT = as.Date(c("2020-02-15", "2020-01-30")),
    DTHFL = c(NA, "Y"), stringsAsFactors = FALSE
  )
  col <- pp_cohort_sev_color(NULL)
  a <- as.character(pp_subject_sentence_ui(frame, 1L, NA, col))
  expect_match(a, "discontinued, end of study D46", fixed = TRUE)
  b <- as.character(pp_subject_sentence_ui(frame, 2L, NA, col))
  expect_match(b, "completed", fixed = TRUE)
  expect_match(b, "died, date not recorded", fixed = TRUE)
})
