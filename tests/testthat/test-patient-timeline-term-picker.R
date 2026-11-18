# The subtitle's "showing" words: the terms of the level the rows open into,
# most patients first with their counts; picks keep only the patients who
# have them, and a patient left with one row is drawn on its patient row.

test_that("the terms of one level come most patients first, with counts", {
  d <- pt_toy()
  d$adae <- rbind(d$adae, transform(d$adae[3, ], USUBJID = "C",
                                    AEDECOD = "NAUSEA"))
  t <- patient_timeline_terms(d, "ae", "AEDECOD")
  expect_identical(as.character(t),
                   paste0("Preferred term: ", c("Nausea", "Fall", "Headache")))
  expect_identical(attr(t, "labels"), c("Nausea", "Fall", "Headache"))
  expect_identical(attr(t, "notes"), c("2 patients", "1 patient", "1 patient"))
  # A level the data lacks offers nothing to pick.
  expect_match(patient_timeline_terms(d, "ae", "AEHLGT"), "No AE terms")
  cm <- patient_timeline_terms(d, "cm", "CMDECOD")
  expect_identical(attr(cm, "labels"), "Paracetamol")
})

test_that("picks keep only the patients who have them, in each layer", {
  # Response draws every patient; a picked AE keeps A alone.
  f <- patient_timeline_frame(pt_toy(), layers = c("Adverse events",
                                                   "Response"),
                              ae_terms = "Preferred term: Headache")
  expect_identical(unique(f$USUBJID), "A")
  # Picks in both layers: the patients with both, and the block says so
  # when none has.
  expect_error(
    patient_timeline_frame(pt_toy(), layers = c("Adverse events", "Con meds"),
                           ae_terms = "Preferred term: Fall",
                           cm_terms = "Standardized name: Paracetamol"),
    "No patient has the picked terms"
  )
  f <- patient_timeline_frame(pt_toy(), layers = c("Adverse events",
                                                   "Con meds"),
                              ae_terms = "Preferred term: Nausea",
                              cm_terms = "Standardized name: Paracetamol")
  expect_identical(unique(f$USUBJID), "A")
})

test_that("a patient with one row is drawn on its patient row", {
  # B has one AE; A has two.
  f <- patient_timeline_frame(pt_toy())
  ae <- f[f$.lane %in% "Adverse events", ]
  expect_true(all(is.na(ae$Row[ae$USUBJID == "B"])))
  expect_setequal(ae$Row[ae$USUBJID == "A"], c("Headache", "Nausea"))
  expect_false(any(f$.section[f$USUBJID == "B"] %in% "Adverse events"))
  # One term picked: every patient flat.
  f <- patient_timeline_frame(pt_toy(), ae_terms = "Preferred term: Nausea")
  expect_true(all(is.na(f$Row[f$.lane %in% "Adverse events"])))
  # Treatment drawn on the patient row: the AE keeps its own row.
  f <- patient_timeline_frame(pt_toy(),
                              layers = c("Adverse events", "Treatment"),
                              ae_terms = "Preferred term: Nausea")
  expect_identical(unique(f$Row[f$.lane %in% "Adverse events"]), "Nausea")
  # An AE and a con med are two rows.
  f <- patient_timeline_frame(pt_toy(), layers = c("Adverse events",
                                                   "Con meds"),
                              ae_terms = "Preferred term: Nausea")
  expect_false(anyNA(f$Row[f$.lane %in% c("Adverse events", "Con meds")]))
})

test_that("the showing word offers the lane level's terms and reads all", {
  viz <- asNamespace("blockr.viz")
  specs <- function(values) {
    viz$cb_specs(viz$cb_parse(patient_timeline_script()), pt_toy(),
                 c(list(layers = "Adverse events"), values))
  }
  sp <- specs(list())
  ae <- Filter(function(s) identical(s$name, "ae_terms"), sp)[[1L]]
  expect_identical(ae$labels, c("Fall", "Headache", "Nausea"))
  expect_identical(ae$notes, rep("1 patient", 3L))
  expect_identical(ae$placeholder, "all")
  sp <- specs(list(ae_lanes = "AEBODSYS"))
  ae <- Filter(function(s) identical(s$name, "ae_terms"), sp)[[1L]]
  expect_true(all(startsWith(ae$choices, "System organ class: ")))
  # The term rows are no longer in the gear: the word is their control.
  lanes <- unlist(patient_timeline_gear()$tabs$Lanes)
  expect_false(any(c("ae_terms", "cm_terms") %in% lanes))
})
