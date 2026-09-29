# The tooltips of the medications panel and the patient overview: built in
# R as a `tip` beside each data point, drawn by PatientProfile.tip().

tip_tr <- as.Date(c("2020-01-01", "2020-06-30"))
tip_ref <- pp_xval(as.Date("2020-01-01"))

tip_rows <- function(tip) {
  stats::setNames(
    vapply(tip$rows, function(r) r$value, ""),
    vapply(tip$rows, function(r) r$label, "")
  )
}
tip_meta <- function(tip, label) {
  hit <- Filter(function(r) identical(r$label, label), tip$rows)
  if (length(hit)) hit[[1L]]$meta else NULL
}

# -- Concomitant medications --------------------------------------------------

tip_cm_dm <- function() {
  adcm <- data.frame(
    USUBJID = "x",
    CMTRT = c("ARICEPT", "PARACETAMOL", "TYLENOL"),
    CMDECOD = c("DONEPEZIL HYDROCHLORIDE", "PARACETAMOL", "UNCODED"),
    CMCLAS = c("NERVOUS SYSTEM", NA, "UNCODED"),
    CMINDC = c("PRIMARY STUDY CONDITION", NA, NA),
    CMDOSE = c(5, NA, 1),
    CMDOSU = c("mg", NA, "TABLET"),
    CMROUTE = c("ORAL", "ORAL", NA),
    ASTDT = as.Date(c("2020-03-18", "2020-02-01", "2020-02-01")),
    ASTDY = c(78L, 32L, 32L),
    AENDT = as.Date(c("2020-04-17", "2020-02-01", NA)),
    AENDY = c(108L, 32L, NA),
    stringsAsFactors = FALSE
  )
  dm::dm(adcm = adcm)
}

cm_tips <- function(mode = "rday", settings = list()) {
  chart <- cm_gantt_viz$render(tip_cm_dm(), tip_tr, settings, tip_ref, mode)
  bars <- chart$x$opts$series[[1]]$data
  tips <- lapply(bars, function(b) b$tip)
  names(tips) <- vapply(bars, function(b) as.character(b$value[[4]]), "")
  list(tips = tips, series = chart$x$opts$series[[1]])
}

test_that("a medication bar names the drug, its class, indication and dose", {
  out <- cm_tips(settings = list(
    roles = list(indication = "CMINDC"),
    indc_colors = c("PRIMARY STUDY CONDITION" = "#111111")
  ))
  expect_identical(out$series$tooltip$formatter, PP_TIP_FORMATTER)

  tip <- out$tips[["DONEPEZIL HYDROCHLORIDE"]]
  expect_identical(tip$head, "Donepezil hydrochloride")
  expect_identical(tip$sub, "Nervous system")
  expect_identical(tip$color, "#111111")
  expect_identical(
    tip_rows(tip),
    c(Indication = "Primary study condition", Dose = "5 mg, oral",
      "Reported as" = "Aricept", From = "D78", To = "D108")
  )
  expect_identical(tip_meta(tip, "From"), "2020-03-18")
  expect_identical(tip_meta(tip, "To"), "31 days")

  # The bar without an indication is grey, and so is its swatch.
  expect_identical(out$tips[["PARACETAMOL"]]$color, "#9ca3af")
})

test_that("a medication tooltip drops what the record does not carry", {
  tips <- cm_tips()$tips

  # Same coded and reported name, no class, no amount, a one-day course.
  tip <- tips[["PARACETAMOL"]]
  expect_identical(tip$head, "Paracetamol")
  expect_null(tip$sub)
  expect_identical(tip$color, PP_CM_COLOR)
  expect_identical(tip_rows(tip), c(Dose = "Oral", Day = "D32"))

  # "UNCODED" is no name: the reported one heads, and no class is claimed.
  tip <- tips[["UNCODED"]]
  expect_identical(tip$head, "Tylenol")
  expect_null(tip$sub)
  expect_identical(tip_rows(tip),
                   c(Dose = "1 TABLET", From = "D32", To = "ongoing"))
})

test_that("in date mode a medication's dates lead and days follow", {
  tip <- cm_tips(mode = "date")$tips[["DONEPEZIL HYDROCHLORIDE"]]
  expect_identical(tip_rows(tip)[c("From", "To")],
                   c(From = "2020-03-18", To = "2020-04-17"))
  expect_identical(tip_meta(tip, "From"), "D78")
  expect_identical(tip_meta(tip, "To"), "31 days")
})

test_that("pp_cm_tip_dose() writes one value from its parts", {
  expect_identical(pp_cm_tip_dose(5, "mg", "ORAL"), "5 mg, oral")
  expect_identical(pp_cm_tip_dose(0.625, "mg", NA), "0.625 mg")
  expect_identical(pp_cm_tip_dose(NA, "mg", "ORAL"), "Oral")
  expect_identical(pp_cm_tip_dose(NA, NA, NA), "")
})

# -- Patient overview ---------------------------------------------------------

tip_overview_dm <- function(adsl_extra = list(), adex = TRUE) {
  adsl <- data.frame(
    USUBJID = "x", ACTARM = "Xanomeline High Dose",
    TRTSDT = as.Date("2020-01-01"), TRTEDT = as.Date("2020-03-31"),
    RFENDT = as.Date("2020-04-10"),
    stringsAsFactors = FALSE
  )
  for (nm in names(adsl_extra)) adsl[[nm]] <- adsl_extra[[nm]]
  adae <- data.frame(
    USUBJID = "x", AEDECOD = c("HEADACHE", "NAUSEA"),
    AESEV = c("SEVERE", "MILD"), AESER = c("Y", "N"),
    ASTDT = as.Date(c("2020-01-10", "2020-02-01")),
    ASTDY = c(10L, 32L),
    AENDT = as.Date(c("2020-01-10", NA)), AENDY = c(10L, NA),
    stringsAsFactors = FALSE
  )
  advs <- data.frame(
    USUBJID = "x", AVISIT = "WEEK 2", ADT = as.Date("2020-01-15"), ADY = 15L,
    stringsAsFactors = FALSE
  )
  tbls <- list(adsl = adsl, adae = adae, advs = advs)
  if (adex) {
    tbls$adex <- data.frame(
      USUBJID = "x", EXTRT = "XANOMELINE", EXDOSE = 54, EXDOSU = "mg",
      AVISIT = "WEEK 2",
      ASTDT = as.Date("2020-01-15"), ASTDY = 15L,
      AENDT = as.Date("2020-02-11"), AENDY = 42L,
      stringsAsFactors = FALSE
    )
  }
  do.call(dm::dm, tbls)
}

overview_series <- function(dm_obj = tip_overview_dm(), mode = "rday") {
  chart <- patient_overview_viz$render(
    dm_obj, tip_tr,
    settings = list(roles = list(arm = "ACTARM", severity = "AESEV")),
    ref_ms = tip_ref, mode = mode
  )
  series <- chart$x$opts$series
  stats::setNames(series, vapply(series, function(s) s$name, ""))
}

test_that("every overview series draws its tooltip from the tip", {
  series <- overview_series()
  for (s in series) expect_identical(s$tooltip$formatter, PP_TIP_FORMATTER)
  series <- overview_series(tip_overview_dm(adex = FALSE))
  expect_identical(series$Treatment$tooltip$formatter, PP_TIP_FORMATTER)
})

test_that("a dose bar names the treatment and the dose", {
  tip <- overview_series()$Exposure$data[[1]]$tip
  expect_identical(tip$head, "Xanomeline")
  expect_identical(tip$color, "#2563EB")
  expect_identical(
    tip_rows(tip),
    c(Dose = "54 mg", From = "D15", To = "D42", Visit = "Week 2")
  )
  expect_identical(tip_meta(tip, "To"), "28 days")
})

test_that("an overview AE bar says severity and seriousness only", {
  data <- overview_series()$`Adverse Events`$data
  tip <- data[[1]]$tip
  expect_identical(tip$head, "Headache")
  expect_identical(tip$color, "#DC2626")
  expect_null(tip$sub)
  # A one-day event is one "Day" row.
  expect_identical(tip_rows(tip),
                   c(Severity = "Severe", Serious = "Yes", Day = "D10"))
  expect_identical(tip_meta(tip, "Day"), "2020-01-10")

  tip <- data[[2]]$tip
  expect_identical(tip$color, "#CA8A04")
  expect_identical(tip_rows(tip),
                   c(Severity = "Mild", From = "D32", To = "ongoing"))
})

test_that("end of study and visits give their day as the timeline does", {
  series <- overview_series()
  eos <- series$Milestones$data[[1]]$tip
  expect_identical(eos$head, "End of study")
  expect_identical(eos$color, "#2563EB")
  expect_identical(tip_rows(eos), c(Day = "D101"))
  expect_identical(tip_meta(eos, "Day"), "2020-04-10")

  eos <- overview_series(mode = "date")$Milestones$data[[1]]$tip
  expect_identical(tip_rows(eos), c(Day = "2020-04-10"))
  expect_identical(tip_meta(eos, "Day"), "D101")

  vis <- series$Visits$data[[1]]$tip
  expect_identical(vis$head, "Week 2")
  expect_identical(tip_rows(vis), c(Day = "D15"))
})

test_that("a death flagged without a date says where it is drawn", {
  dm_obj <- tip_overview_dm(list(DTHFL = "Y"))
  ms <- overview_series(dm_obj)$Milestones$data
  tip <- ms[[length(ms)]]$tip
  expect_identical(tip$head, "Death")
  expect_identical(tip$color, "#DC2626")
  expect_length(tip$rows, 0L)
  expect_match(tip$note, "Date not recorded")
})

test_that("without exposure the treatment bar names the arm and its span", {
  tip <- overview_series(tip_overview_dm(adex = FALSE))$Treatment$data[[1]]$tip
  expect_identical(tip$head, "Xanomeline High Dose")
  expect_identical(tip$color, "#059669")
  expect_identical(tip_rows(tip), c(From = "D1", To = "D91"))
  expect_identical(tip_meta(tip, "To"), "91 days")
})
