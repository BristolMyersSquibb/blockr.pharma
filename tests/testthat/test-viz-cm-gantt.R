# Concomitant medications gantt: a declaration plus a render, no exceptions.
# Works against ADaM adcm and (via pp_normalize_dm()) an SDTM cm domain --
# the SDTM path is covered end-to-end in test-pp-sdtm.R.

cm_dm <- function(cm_extra = list()) {
  adcm <- data.frame(
    USUBJID = "x",
    CMTRT = c("ASPIRIN TAB", "PARACETAMOL"),
    CMDECOD = c("ASPIRIN", ""),
    ASTDT = as.Date(c("2020-02-01", "2020-03-01")),
    AENDT = as.Date(c("2020-02-20", NA)),
    CMDOSE = c(100, 500), CMDOSU = c("mg", "mg"),
    CMROUTE = c("ORAL", "ORAL"),
    CMINDC = c("PROPHYLAXIS", "HEADACHE"),
    stringsAsFactors = FALSE
  )
  for (nm in names(cm_extra)) adcm[[nm]] <- cm_extra[[nm]]
  dm::dm(
    adsl = data.frame(USUBJID = "x", TRTSDT = as.Date("2020-01-01"),
                      TRTEDT = as.Date("2020-06-01"),
                      stringsAsFactors = FALSE),
    adcm = adcm
  )
}

test_that("cm_gantt is declared against adcm and resolves", {
  dm_obj <- cm_dm()
  expect_true("cm_gantt" %in% names(patient_profile_static_vizs()))
  res <- pp_resolve_requires(dm_obj, cm_gantt_viz)
  expect_true(isTRUE(res$ok))
})

test_that("lanes prefer the coded name and fall back per row", {
  chart <- cm_gantt_viz$render(
    cm_dm(), time_range = as.Date(c("2020-01-01", "2020-06-01"))
  )
  lanes <- unlist(chart$x$opts$yAxis$data)
  # ASPIRIN from CMDECOD; PARACETAMOL's blank CMDECOD falls back to CMTRT
  expect_setequal(lanes, c("ASPIRIN", "PARACETAMOL"))

  bars <- chart$x$opts$series[[1]]$data
  tips <- vapply(bars, function(b) paste(unlist(b$value), collapse = " "), "")
  expect_true(any(grepl("100 mg", tips)))
  expect_true(any(grepl("PROPHYLAXIS", tips)))
})

test_that("the medication period feeds the shared time range", {
  # A med taken outside the treatment window must widen the axis, exactly
  # like an AE would.
  dm_obj <- cm_dm(list(AENDT = as.Date(c("2020-02-20", "2020-09-15"))))
  tr <- pp_compute_time_range(dm_obj)
  expect_gte(as.numeric(tr[2]), as.numeric(as.Date("2020-09-15")))
})

test_that("date mode without dates asks for relative day", {
  adcm <- data.frame(
    USUBJID = "x", CMTRT = "ASPIRIN", ASTDY = 10L, AENDY = 20L,
    stringsAsFactors = FALSE
  )
  dm_obj <- dm::dm(adcm = adcm)
  expect_true(isTRUE(pp_resolve_requires(dm_obj, cm_gantt_viz)$ok))

  chart <- cm_gantt_viz$render(dm_obj, time_range = NULL, mode = "date")
  expect_match(paste(unlist(chart$x$opts$title), collapse = " "),
               "relative day")

  chart <- cm_gantt_viz$render(dm_obj, time_range = NULL,
                               ref_ms = NA_real_, mode = "rday")
  starts <- vapply(chart$x$opts$series[[1]]$data,
                   function(d) as.numeric(d$value[[1]]), numeric(1))
  expect_equal(starts, 10)
})
