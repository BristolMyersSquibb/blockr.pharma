# What colours a track (timeline_colour_fields()): fields read off the data,
# a column coloured by its values in a known or pinned order, old words.

# Two patients on "Drug A" and "Drug A + Drug B": an action-taken column per
# drug, seriousness, causality, outcome, a grade and a severity.
lab <- function(x, l) structure(x, label = l)
cb_toy <- function() {
  adsl <- data.frame(
    USUBJID = c("P1", "P2"), ACTARM = c("Drug A", "Drug A + Drug B"),
    TRTSDT = as.Date("2024-01-01"), TRTEDT = as.Date("2024-03-01"),
    LSTALVDT = as.Date("2024-03-30"), stringsAsFactors = FALSE
  )
  adae <- data.frame(
    USUBJID = c("P1", "P1", "P2", "P2"),
    AEDECOD = c("HEADACHE", "NAUSEA", "RASH", "FATIGUE"),
    AEBODSYS = "X", TRTEMFL = "Y",
    ASTDY = c(2, 5, 3, 20), AENDY = c(10, 12, 30, 25),
    stringsAsFactors = FALSE
  )
  adae$AETOXGR <- lab(c("1", "3", "2", "1"), "Standard Toxicity Grade")
  adae$AESEV <- lab(c("MILD", "SEVERE", "MODERATE", "MILD"),
                    "Severity/Intensity")
  adae$AEACN <- lab(c("DOSE NOT CHANGED", "DRUG WITHDRAWN", "DOSE REDUCED",
                      "DOSE NOT CHANGED"), "Action Taken with Study Treatment")
  adae$AEACNA <- lab(c("DOSE NOT CHANGED", "DRUG WITHDRAWN",
                       "DRUG INTERRUPTED", "DOSE NOT CHANGED"),
                     "Action Taken with Drug A")
  adae$AEACNB <- lab(c(NA, NA, "DOSE REDUCED", "DOSE NOT CHANGED"),
                     "Action Taken with Drug B")
  adae$AESER <- c("N", "Y", "N", "N")
  adae$AEREL <- c("POSSIBLE", "PROBABLE", "REMOTE", "NONE")
  adae$AEOUT <- c("RECOVERED/RESOLVED", "FATAL", "RECOVERING/RESOLVING",
                  "RECOVERED/RESOLVED")
  adcm <- data.frame(USUBJID = "P1", CMDECOD = c("PARACETAMOL", "IBUPROFEN"),
                     CMTRT = "x", CMCLAS = c("ANALGESICS", "NSAIDS"),
                     CMROUTE = c("ORAL", "ORAL"), ASTDY = c(2, 4),
                     AENDY = c(6, 8), stringsAsFactors = FALSE)
  adrs <- data.frame(USUBJID = "P1", PARAMCD = c("OVR", "OVRIRC"),
                     PARAM = c("Overall response, investigator",
                               "Overall response, independent review"),
                     AVALC = c("PR", "SD"),
                     ADT = as.Date(c("2024-02-01", "2024-02-01")),
                     stringsAsFactors = FALSE)
  list(adsl = adsl, adae = adae, adcm = adcm, adrs = adrs)
}

test_that("each track lists the fields the data has, by their labels", {
  ae <- timeline_colour_fields(cb_toy(), "ae")
  expect_identical(unname(ae), c("AESEV", "AETOXGR", "AEACN", "AEACNA",
                                 "AEACNB", "AESER", "AEREL", "AEOUT",
                                 "one colour"))
  expect_identical(names(ae)[1:5], c("Severity/Intensity",
                                     "Standard Toxicity Grade",
                                     "Action Taken with Study Treatment",
                                     "Action Taken with Drug A",
                                     "Action Taken with Drug B"))
  # The timeline block lists the grade first; the study's role decides on
  # the patient timeline.
  expect_identical(unname(timeline_colour_fields(cb_toy(), "ae",
                                                 grade_first = TRUE))[1L],
                   "AETOXGR")
  expect_identical(unname(timeline_colour_fields(
    cb_toy(), "ae", roles = list(severity = "AETOXGR")
  ))[1L], "AETOXGR")
  # A column the study lacks is not offered.
  d <- cb_toy()
  d$adae$AEACNB <- NULL
  expect_false("AEACNB" %in% timeline_colour_fields(d, "ae"))
  cm <- timeline_colour_fields(cb_toy(), "cm")
  expect_identical(unname(cm), c("CMCLAS", "CMROUTE", "before or on treatment",
                                 "one colour"))
  rs <- timeline_colour_fields(cb_toy(), "rs")
  expect_identical(unname(rs), c("OVR", "OVRIRC"))
  expect_match(names(rs)[2L], "independent review")
  expect_identical(unname(timeline_colour_fields(cb_toy(), "ex")),
                   c("dose level", "drug", "one colour"))
})

test_that("a column colours by its values, in their known order, worst last", {
  for (fr in list(function(...) timeline_frame(cb_toy(), ...),
                  function(...) patient_timeline_frame(cb_toy(), ...))) {
    f <- fr(ae_colour = "AEACNA")
    ae <- f[f$.lane %in% "Adverse events" & is.finite(f$Start), ]
    expect_setequal(as.character(ae$Colour),
                    c("Dose not changed", "Drug withdrawn", "Drug interrupted"))
    # The legend order: the known order, so the withdrawal is the worst.
    lv <- levels(droplevels(ae$Colour))
    expect_identical(lv, c("Dose not changed", "Drug interrupted",
                           "Drug withdrawn"))
    # The scale map binds the column under its own name.
    keys <- attr(f$Colour, "blockr_scale_keys")
    expect_true(all(c("AEACNA") %in% keys$var))
    expect_identical(keys$key[keys$level == "Drug withdrawn" &
                                keys$var == "AEACNA"], "DRUG WITHDRAWN")
  }
})

test_that("the scale map's pins order the levels", {
  map <- blockr.theme::new_scale_map(blockr.theme::scale_binding(
    "AEACNA", color = c("DRUG WITHDRAWN" = "#000000",
                        "DRUG INTERRUPTED" = "#111111",
                        "DOSE NOT CHANGED" = "#222222")
  ))
  f <- timeline_frame(cb_toy(), ae_colour = "AEACNA", scale_map = map)
  lv <- levels(droplevels(f$Colour[f$.lane %in% "Adverse events"]))
  expect_identical(lv, c("Drug withdrawn", "Drug interrupted",
                         "Dose not changed"))
})

test_that("con meds colour by a column, the response by its parameter", {
  f <- timeline_frame(cb_toy(), show = "Con meds", cm_colour = "CMROUTE")
  expect_identical(unique(as.character(f$Colour[f$.lane %in% "Con meds"])),
                   "Oral")
  f <- timeline_frame(cb_toy(), show = "Con meds", cm_colour = "CMCLAS")
  expect_setequal(as.character(f$Colour[f$.lane %in% "Con meds"]),
                  c("Analgesics", "Nsaids"))
  # A column the data lacks: before or on treatment.
  f <- timeline_frame(cb_toy(), show = "Con meds", cm_colour = "CMINDC")
  expect_true(all(grepl("^Started", f$Colour[f$.lane %in% "Con meds"])))
  r <- patient_timeline_frame(cb_toy(), layers = "Response",
                              response = "OVRIRC")
  expect_identical(as.character(r$Colour[is.finite(r$Start)])[2L], "SD")
})

test_that("the old colour words restore as the fields they meant", {
  sv <- tl_colour_values_migrate(list(ae_colour = "grade", cm_colour = "class"))
  expect_identical(sv, list(ae_colour = "AETOXGR", cm_colour = "CMCLAS"))
  # A word the data decides is dropped: the field list's first entry.
  expect_null(tl_colour_values_migrate(list(ae_colour = "study severity"))$ae_colour)
  expect_identical(tl_colour_values_migrate(list(
    cm_colour = "class, else before or on treatment"
  ))$cm_colour, "before or on treatment")
  # The frames still read them.
  g <- timeline_frame(cb_toy(), ae_colour = "grade")
  expect_true("Grade 3" %in% g$Colour)
})

test_that("the colour selects show the fields' labels", {
  viz <- asNamespace("blockr.viz")
  specs <- viz$cb_specs(viz$cb_parse(patient_timeline_script()),
                        dm::as_dm(cb_toy()))
  sp <- Filter(function(s) identical(s$name, "ae_colour"), specs)[[1L]]
  expect_identical(sp$choices[1:2], c("AESEV", "AETOXGR"))
  expect_identical(sp$labels[[4L]], "Action Taken with Drug A")
  sp <- Filter(function(s) identical(s$name, "response"), specs)[[1L]]
  expect_identical(sp$choices, c("OVR", "OVRIRC"))
})
