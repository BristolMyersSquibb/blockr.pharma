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

cm_tips <- function(mode = "rday", settings = list(), dm_obj = tip_cm_dm()) {
  chart <- cm_gantt_viz$render(dm_obj, tip_tr, settings, tip_ref, mode)
  bars <- chart$x$opts$series[[1]]$data
  tips <- lapply(bars, function(b) b$tip)
  names(tips) <- vapply(bars, function(b) as.character(b$value[[4]]), "")
  list(tips = tips, series = chart$x$opts$series[[1]])
}

cm_tips_of <- function(dm_obj) cm_tips(dm_obj = dm_obj)$tips

test_that("a medication bar names the drug, its class, indication and dose", {
  out <- cm_tips(settings = list(
    roles = list(indication = "CMINDC"),
    indc_colors = c("PRIMARY STUDY CONDITION" = "#111111")
  ))
  expect_identical(out$series$tooltip$formatter, PP_TIP_FORMATTER)

  tip <- out$tips[["DONEPEZIL HYDROCHLORIDE"]]
  # Named as the patient timeline names a con med.
  expect_identical(tip$head, "Donepezil Hydrochloride")
  # The timeline's parts: the colour's level, then Type, Details, Days.
  expect_identical(tip$sub, "Started on treatment")
  # Coloured as the patient timeline colours con meds, not by indication.
  expect_identical(tip$color, PP_CM_STARTED[["Started on treatment"]])
  expect_identical(
    tip_rows(tip),
    c(Type = "Con med",
      Details = "For primary study condition, Nervous system",
      Dose = "5 mg, oral", "Reported as" = "Aricept",
      Days = "Day 78 to 108")
  )
  expect_identical(tip_meta(tip, "Days"), "31 days")
})

test_that("a medication started before day 1 takes the timeline's lighter blue", {
  dm_obj <- tip_cm_dm()
  cm <- dm::dm_get_tables(dm_obj)$adcm
  cm$ASTDY[2] <- -40L
  tips <- cm_tips_of(dm::dm(adcm = cm))
  expect_identical(tips[["PARACETAMOL"]]$color,
                   PP_CM_STARTED[["Started before treatment"]])
  expect_identical(tips[["DONEPEZIL HYDROCHLORIDE"]]$color,
                   PP_CM_STARTED[["Started on treatment"]])
})

test_that("a medication tooltip drops what the record does not carry", {
  tips <- cm_tips()$tips

  # Same coded and reported name, no class, no amount, a one-day course.
  tip <- tips[["PARACETAMOL"]]
  expect_identical(tip$head, "Paracetamol")
  expect_identical(tip$color, PP_CM_STARTED[["Started on treatment"]])
  expect_identical(tip_rows(tip),
                   c(Type = "Con med", Dose = "Oral", Days = "Day 32"))

  # "UNCODED" is no name: the reported one names the bar and heads the
  # tooltip, and no class is claimed.
  tip <- tips[["TYLENOL"]]
  expect_identical(tip$head, "Tylenol")
  expect_identical(tip_rows(tip),
                   c(Type = "Con med", Dose = "1 TABLET",
                     Days = "Day 32 to ongoing"))
})

test_that("in date mode a medication's dates lead", {
  tip <- cm_tips(mode = "date")$tips[["DONEPEZIL HYDROCHLORIDE"]]
  expect_identical(tip_rows(tip)[["Dates"]], "2020-03-18 to 2020-04-17")
  expect_identical(tip_meta(tip, "Dates"), "31 days")
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
  # The drug's colour on the patient timeline.
  expect_identical(tip$color, unname(pt_drug_palette("XANOMELINE")[[1]]))
  # As the timeline's drug line reads; no visit, which is the visits lane's.
  expect_identical(
    tip_rows(tip),
    c(Type = "Treatment", Details = "Xanomeline, 54 mg",
      Days = "Day 15 to 42")
  )
  expect_identical(tip_meta(tip, "Days"), "28 days")
})

test_that("a placebo's zero dose is no dose", {
  dm_obj <- tip_overview_dm()
  tb <- dm::dm_get_tables(dm_obj)
  tb$adex$EXTRT <- "PLACEBO"
  tb$adex$EXDOSE <- 0
  tip <- overview_series(do.call(dm::dm, tb))$Exposure$data[[1]]$tip
  expect_identical(tip_rows(tip)[["Details"]], "Placebo")
  expect_false(any(grepl("0 mg", tip_rows(tip))))
})

test_that("an overview AE bar says severity and seriousness only", {
  data <- overview_series()$`Adverse Events`$data
  # The worst is drawn last, over the others.
  tips <- lapply(data, `[[`, "tip")
  expect_identical(vapply(tips, `[[`, "", "head"), c("Nausea", "Headache"))
  tip <- tips[[2]]
  expect_identical(tip$head, "Headache")
  expect_identical(tip$color, "#8B0000")
  expect_identical(tip$sub, "Severe, serious")
  # A one-day event is "Day 10".
  expect_identical(tip_rows(tip),
                   c(Type = "Adverse event", Days = "Day 10"))

  tip <- tips[[1]]
  expect_identical(tip$color, "#CA8A04")
  expect_identical(tip$sub, "Mild")
  expect_identical(tip_rows(tip),
                   c(Type = "Adverse event", Days = "Day 32 to ongoing"))
})

test_that("end of study and visits give their day as the timeline does", {
  series <- overview_series()
  eos <- series$Milestones$data[[1]]$tip
  expect_identical(eos$head, "End of study")
  expect_identical(eos$color, "#2563EB")
  expect_identical(tip_rows(eos), c(Days = "Day 101"))
  expect_identical(tip_meta(eos, "Days"), "2020-04-10")

  eos <- overview_series(mode = "date")$Milestones$data[[1]]$tip
  expect_identical(tip_rows(eos), c(Date = "2020-04-10"))
  expect_identical(tip_meta(eos, "Date"), "D101")

  vis <- series$Visits$data[[1]]$tip
  expect_identical(vis$head, "Week 2")
  expect_identical(tip_rows(vis), c(Type = "Visit", Days = "Day 15"))
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
  expect_identical(tip_rows(tip), c(Type = "Treatment", Days = "Day 1 to 91"))
  expect_identical(tip_meta(tip, "Days"), "91 days")
})
