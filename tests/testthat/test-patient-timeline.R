# new_patient_timeline_block() and its frame, patient_timeline_frame(): the
# timeline's layers with the swimmer's response and discontinuation added.

# Patient A: two AEs (one severe, one serious), a con med, two dose periods,
# PR then PD, stopped for an adverse event. Patient B: one AE, SD, died.
# Patient C: on treatment, no AE, CR.
pt_toy <- function() {
  adsl <- data.frame(
    USUBJID = c("A", "B", "C"),
    ACTARM = c("Drug", "Placebo", "Drug"),
    TRT01A = c("Drug 54 mg", "Placebo", "Drug 81 mg"),
    TRTSDT = as.Date(c("2024-01-01", "2024-01-01", "2024-01-01")),
    TRTEDT = as.Date(c("2024-02-29", "2024-01-20", NA)),
    LSTALVDT = as.Date(c("2024-03-30", "2024-01-25", "2024-04-30")),
    DTHDT = as.Date(c(NA, "2024-01-25", NA)),
    DCTREAS = c("ADVERSE EVENT", "DEATH", NA),
    stringsAsFactors = FALSE
  )
  adae <- data.frame(
    USUBJID = c("A", "A", "B"),
    AEDECOD = c("HEADACHE", "NAUSEA", "FALL"),
    AETERM = c("headache", "nausea", "fall"),
    AEBODSYS = c("NERVOUS SYSTEM DISORDERS", "GASTROINTESTINAL DISORDERS",
                 "INJURY"),
    AESEV = c("MILD", "SEVERE", "MODERATE"),
    ATOXGR = c("1", "3", "2"),
    AESER = c("N", "Y", "N"),
    TRTEMFL = "Y",
    ASTDY = c(2, 5, 3),
    AENDY = c(10, NA, 4),
    stringsAsFactors = FALSE
  )
  adcm <- data.frame(
    USUBJID = "A", CMDECOD = "PARACETAMOL", CMTRT = "Tylenol",
    CMCLAS = "ANALGESICS", ASTDY = 2, AENDY = 6, stringsAsFactors = FALSE
  )
  adex <- data.frame(
    USUBJID = c("A", "A", "B", "C"),
    EXTRT = c("DRUG", "DRUG", "PLACEBO", "DRUG"),
    EXDOSE = c(54, 81, 0, 81),
    EXDOSU = "mg",
    ASTDT = as.Date(c("2024-01-01", "2024-01-31", "2024-01-01",
                      "2024-01-01")),
    AENDT = as.Date(c("2024-01-30", "2024-02-29", "2024-01-20", NA)),
    stringsAsFactors = FALSE
  )
  adrs <- data.frame(
    USUBJID = c("A", "A", "B", "C"),
    PARAMCD = "OVR",
    AVALC = c("PR", "PD", "SD", "CR"),
    ADT = as.Date(c("2024-01-29", "2024-02-26", "2024-01-15",
                    "2024-02-01")),
    stringsAsFactors = FALSE
  )
  list(adsl = adsl, adae = adae, adcm = adcm, adex = adex, adrs = adrs)
}

test_that("the safety default draws AEs with death and the reason tag", {
  f <- patient_timeline_frame(pt_toy())
  # Patients with an AE only; C has none.
  expect_setequal(unique(f$USUBJID), c("A", "B"))
  expect_identical(levels(droplevels(f$.lane)), "Adverse events")
  # Severity by the profile's rule: ASEV/AESEV before a grade.
  expect_identical(levels(f$Colour), c("Mild", "Moderate", "Severe"))
  ev <- f[!is.na(f$Event), ]
  expect_identical(as.character(ev$Event[ev$USUBJID == "B"]), "Death")
  # A stopped for an adverse event, tagged at the last dose (day 60); B's
  # reason is death, which the cross shows.
  tag <- ev[!is.na(ev$.tag), ]
  expect_identical(tag$USUBJID, "A")
  expect_identical(tag$.tag, "AE")
  expect_equal(tag$.event_x, 60)
})

test_that("the efficacy layers draw response runs, progression and treatment", {
  f <- patient_timeline_frame(
    pt_toy(), layers = c("Response", "Treatment", "Discontinuation and death"),
    order_by = "days on treatment"
  )
  expect_setequal(unique(f$USUBJID), c("A", "B", "C"))
  r <- f[f$.lane %in% "Response" & is.finite(f$Start), ]
  a <- r[r$USUBJID == "A", ]
  expect_identical(as.character(a$Colour), c("Not assessed", "PR", "PD"))
  expect_true(all(c("Response", "Drug") %in% levels(droplevels(f$.lane))))
  pr <- f[f$Event %in% "Progression" & is.na(f$Row), ]
  expect_identical(pr$USUBJID, "A")
  # Best response and status on the patient record.
  p <- f[is.na(f$Row) & is.na(f$.event_x), ]
  expect_identical(p$.bor[match(c("A", "B", "C"), p$USUBJID)],
                   c("PR", "SD", "CR"))
  expect_identical(p$.status[match(c("A", "B", "C"), p$USUBJID)],
                   c("Off treatment", "Died", "On treatment"))
  # Longest on treatment first: A (60 days), B (20); C has no end.
  expect_identical(unique(f$USUBJID)[1:2], c("A", "B"))
})

test_that("any combination of layers works, colours stay within 15", {
  layers <- PT_LAYERS
  for (k in seq_along(layers)) {
    for (set in utils::combn(layers, k, simplify = FALSE)) {
      f <- patient_timeline_frame(pt_toy(), layers = set)
      expect_lte(nlevels(f$Colour), 15L)
      drawn <- levels(droplevels(f$.lane))
      # Treatment draws a lane per drug.
      drawn[drawn %in% c("Drug", "Placebo")] <- "Treatment"
      expect_true(all(drawn %in% set))
    }
  }
})

test_that("the study's severity column and arm win", {
  roles <- list(severity = "ATOXGR", arm = "TRT01A")
  f <- patient_timeline_frame(pt_toy(), roles = roles)
  expect_identical(levels(f$Colour), c("Grade 1", "Grade 2", "Grade 3"))
  arm <- f$.arm[is.na(f$Row) & is.na(f$.event_x)]
  expect_true("Drug 54 mg" %in% arm)
  expect_error(patient_timeline_frame(pt_toy(),
                                      roles = list(severity = "NOPE")),
               "NOPE")
})

test_that("AEs and con meds narrow to picked terms at any coding level", {
  terms <- patient_timeline_terms(pt_toy(), "ae")
  expect_true(all(c("Body system: Gastrointestinal disorders",
                    "Preferred term: Headache") %in% terms))
  f <- patient_timeline_frame(
    pt_toy(), ae_terms = "Body system: Gastrointestinal disorders"
  )
  ae <- f[f$.lane %in% "Adverse events", ]
  expect_identical(unique(ae$Row), "Nausea")
  f <- patient_timeline_frame(pt_toy(), layers = "Con meds",
                              cm_terms = "Drug class: Analgesics")
  expect_identical(unique(f$Row[!is.na(f$Row)]), "Paracetamol")
  expect_match(patient_timeline_terms(list(adsl = pt_toy()$adsl), "cm"),
               "No con med terms")
})

test_that("the presets are classes and menu entries of their own", {
  e <- new_efficacy_timeline_block()
  s <- new_safety_timeline_block()
  expect_s3_class(e, "efficacy_timeline_block")
  expect_s3_class(e, "patient_timeline_block")
  expect_s3_class(s, "safety_timeline_block")
  expect_identical(blockr.core::registry_id_from_block(e),
                   "efficacy_timeline_block")
  expect_identical(blockr.core::registry_id_from_block(s),
                   "safety_timeline_block")
  sv <- blockr.core::blockr_ser(e)$payload$script_values
  expect_identical(sv$layers, c("Response", "Treatment",
                                "Discontinuation and death"))
  # No listing columns ticked: the patient and the timeline.
  types <- vapply(blockr.core::blockr_ser(e)$payload$summaries,
                  function(s) s$type, "")
  expect_identical(types, "spans")
})

test_that("the script's settings are its controls and run the frame", {
  ex <- parse(text = patient_timeline_script())
  vars <- vapply(ex[-length(ex)], function(e) as.character(e[[2L]]), "")
  expect_true(all(PT_SETTINGS %in% vars))
  env <- new.env()
  env$data <- pt_toy()
  expect_identical(eval(ex, env), patient_timeline_frame(pt_toy()))
})

test_that("the table draws a stripe per layer and opens into rows", {
  viz <- asNamespace("blockr.viz")
  b <- new_patient_timeline_block(layers = PT_LAYERS)
  spec <- viz$viz_block_spec("table")
  p <- blockr.core::blockr_ser(b)$payload
  p <- p[intersect(names(p), names(spec$args))]
  p[c("layout", "by", "group_rows", "missing", "sort_by")] <-
    list("groups", c("USUBJID", "Row"), "summary", "drop", "data")
  st <- viz$table_normalize(viz$viz_check(spec, p))
  m <- viz$table_model_groups(st, patient_timeline_frame(pt_toy(),
                                                         layers = PT_LAYERS))
  expect_null(m$prep$err)
  r <- m$prep$rows
  spans <- length(m$cells$cols)
  cell <- m$cells$cols[[spans]]
  a <- which(r$.label == "A")
  # The drug a thin lane, the AE and con-med stripes full bars under the
  # response, which is the main lane.
  sl <- cell$sl[[a]]
  expect_identical(vapply(sl, function(l) l$n, ""),
                   c("Drug", "Adverse events", "Con meds"))
  expect_identical(vapply(sl, function(l) isTRUE(l$f), NA),
                   c(FALSE, TRUE, TRUE))
  expect_gt(length(cell$segs[[a]]), 0L)
  kids <- r$.label[!r$.is_parent & r$.parent == "A"]
  expect_identical(kids, c("Response", "Drug", "Headache", "Nausea",
                           "Paracetamol"))
})

test_that("treatment draws a line per drug, or bars by dose", {
  f <- patient_timeline_frame(pt_toy(), layers = c("Response", "Treatment"))
  d <- f[f$.lane %in% "Drug", ]
  # One line from the first dose, cut from 54 to 81 mg: a step up, so solid.
  expect_identical(unique(d$USUBJID), c("A", "C"))
  expect_identical(levels(f$.lane)[1:2], c("Drug", "Placebo"))
  expect_false(any(f$.full[f$.lane %in% c("Drug", "Placebo")]))
  expect_true(all(f$.full[f$.lane %in% "Response"]))
  # The swimmer's drug colours.
  pal <- attr(f$Colour, "blockr_colors")
  expect_identical(unname(pal[c("Drug", "Placebo")]),
                   c(SWIMMER_DRUGS[[1L]], "#8f9bb3"))
  # C is on treatment at the cutoff: its line stays open.
  expect_true(any(d$.open[d$USUBJID == "C"]))
  # Bars by dose: the timeline's dose levels, each drug on its lane.
  b <- patient_timeline_frame(pt_toy(), layers = c("Response", "Treatment"),
                              treatment_as = "bars by dose")
  expect_true(all(c("54 mg", "81 mg") %in% levels(b$Colour)))
  expect_identical(sort(unique(as.character(b$.lane[b$Type %in%
                                                      "Treatment"]))),
                   c("Drug", "Placebo"))
})

test_that("the reason tag sits on the lane of the last drug", {
  f <- patient_timeline_frame(pt_toy(), layers = c("Treatment",
                                                   "Discontinuation and death"))
  tag <- f[!is.na(f$.tag), ]
  expect_identical(as.character(tag$.lane), "Drug")
  # Without treatment drawn it marks the patient row.
  f <- patient_timeline_frame(pt_toy())
  expect_true(is.na(f$.lane[!is.na(f$.tag)]))
})

test_that("a board restores the patient timeline with its layers", {
  blk <- new_safety_timeline_block(layers = c("Adverse events", "Con meds"),
                                   ae_terms = "Preferred term: Headache")
  rt <- board_round_trip(blk, pt_toy())
  expect_s3_class(rt$block, "safety_timeline_block")
  expect_equal(plain_state(rt$restored), plain_state(rt$saved),
               ignore_attr = TRUE)
  expect_identical(as.character(unlist(rt$restored$script_values$layers)),
                   c("Adverse events", "Con meds"))
})
