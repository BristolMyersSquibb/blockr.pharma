# new_timeline_block() and its frame, timeline_frame(): ADSL, ADAE, ADCM and
# ADEX in, one record per drawn piece out, drawn by a viz table block by
# patient and row.

# A small study: patient A with two overlapping AEs (one ongoing, one
# serious), one later AE, a con med from before treatment and one on it, and
# two dose periods; patient B with one AE, a death and no con meds.
tl_toy <- function() {
  adsl <- data.frame(
    USUBJID = c("A", "B"),
    TRT01A = c("Drug", "Placebo"),
    SEX = c("F", "M"),
    TRTSDT = as.Date(c("2024-01-01", "2024-01-01")),
    TRTEDT = as.Date(c("2024-02-29", "2024-01-20")),
    LSTALVDT = as.Date(c("2024-03-30", "2024-01-25")),
    DTHDT = as.Date(c(NA, "2024-01-25")),
    stringsAsFactors = FALSE
  )
  attr(adsl$TRT01A, "label") <- "Actual Treatment"
  adae <- data.frame(
    USUBJID = c("A", "A", "A", "B"),
    AEDECOD = c("HEADACHE", "NAUSEA", "RASH", "FALL"),
    AETERM = c("headache", "nausea", "rash", "fall"),
    AEBODSYS = c("NERVOUS", "GI", "SKIN", "INJURY"),
    AESEV = c("MILD", "SEVERE", "MODERATE", "MODERATE"),
    AESER = c("N", "Y", "N", "N"),
    AESDTH = c("N", "N", "N", "N"),
    AEOUT = c("RECOVERED", "NOT RECOVERED", "RECOVERED", "RECOVERED"),
    TRTEMFL = "Y",
    ASTDY = c(2, 5, 40, 3),
    AENDY = c(10, NA, 45, 4),
    stringsAsFactors = FALSE
  )
  adcm <- data.frame(
    USUBJID = c("A", "A", "A"),
    CMDECOD = c("UNCODED", "PARACETAMOL", "IBUPROFEN"),
    CMTRT = c("Vitamin D", "Tylenol", "Advil"),
    CMCLAS = c("UNCODED", "UNCODED", "UNCODED"),
    CMINDC = c("Prophylaxis", "Headache", "Pain"),
    ASTDY = c(-400, 2, -20),
    AENDY = c(NA, 6, -15),
    stringsAsFactors = FALSE
  )
  adex <- data.frame(
    USUBJID = c("A", "A", "B"),
    EXTRT = c("DRUG", "DRUG", "PLACEBO"),
    EXDOSE = c(54, 81, 0),
    EXDOSU = "mg",
    ASTDT = as.Date(c("2024-01-01", "2024-01-31", "2024-01-01")),
    AENDT = as.Date(c("2024-01-30", "2024-02-29", "2024-01-20")),
    stringsAsFactors = FALSE
  )
  list(adsl = adsl, adae = adae, adcm = adcm, adex = adex)
}

recs <- function(f, src = NULL) {
  r <- f[is.finite(f$Start), , drop = FALSE]
  if (!is.null(src)) r <- r[r$.lane %in% src, , drop = FALSE]
  r
}

test_that("AEs and treatment by default, one record per AE and dose period", {
  f <- timeline_frame(tl_toy())
  expect_identical(levels(f$.lane),
                   c("Adverse events", "Con meds", "Treatment"))
  ae <- recs(f, "Adverse events")
  expect_setequal(ae$Row, c("Headache", "Nausea", "Rash", "Fall"))
  trt <- recs(f, "Treatment")
  expect_identical(trt$Row[trt$USUBJID == "A"], c("Drug", "Drug"))
  expect_identical(as.character(trt$Colour[trt$USUBJID == "A"]),
                   c("54 mg", "81 mg"))
  expect_identical(trt$.bar_label[trt$USUBJID == "A"], c("54 mg", "81 mg"))
  expect_false(any(f$.lane %in% "Con meds"))
  # The treatment's greys run light to dark with the dose.
  cols <- attr(f$Colour, "blockr_colors")
  expect_identical(names(cols)[1:3], c("Mild", "Moderate", "Severe"))
  expect_true(all(c("0 mg", "54 mg", "81 mg") %in% names(cols)))
})

test_that("each patient has one record without a row, for the patient row", {
  f <- timeline_frame(tl_toy())
  p <- f[is.na(f$Row), , drop = FALSE]
  expect_setequal(p$USUBJID[!is.na(p$.arm)], c("A", "B"))
  expect_identical(p$.arm[p$USUBJID == "A" & !is.na(p$.arm)], "Drug")
  # Days on treatment: 2024-01-01 to 2024-02-29.
  expect_equal(p$.days_trt[p$USUBJID == "A" & is.finite(p$.days_trt)], 60)
  # The arm is on the patient record only, not on the rows.
  expect_true(all(is.na(f$.arm[!is.na(f$Row)])))
  # Death marks the patient row.
  d <- f[f$Event %in% "Death", ]
  expect_identical(d$USUBJID, "B")
  expect_true(is.na(d$Row))
  expect_identical(levels(f$Event)[[3L]], "Death")
})

test_that("an event without an end runs open to last contact or the cutoff", {
  tb <- tl_toy()
  n <- recs(timeline_frame(tb), "Adverse events")
  n <- n[n$Row == "Nausea", ]
  expect_true(n$.open)
  # Last contact 2024-03-30 is study day 90.
  expect_equal(n$End, 90)
  expect_identical(n$Days, "Day 5 to ongoing")
  c2 <- recs(timeline_frame(tb, ongoing = "data cutoff",
                            cutoff = as.Date("2024-03-10")), "Adverse events")
  expect_equal(c2$End[c2$Row == "Nausea"], 70)
  # Past the cutoff nothing draws.
  cut <- timeline_frame(tb, cutoff = as.Date("2024-01-20"))
  expect_false("Rash" %in% cut$Row)
  expect_false("Death" %in% as.character(cut$Event))
})

test_that("serious AEs are outlined or marked, and AEs can be filtered", {
  f <- timeline_frame(tl_toy())
  a <- recs(f, "Adverse events")
  expect_identical(a$.style[a$Row == "Nausea"], "outline")
  m <- timeline_frame(tl_toy(), serious = "marked at onset")
  s <- m[m$Event %in% "Serious AE", ]
  expect_equal(s$.event_x, 5)
  expect_identical(s$Row, "Nausea")
  expect_identical(
    recs(timeline_frame(tl_toy(), ae_show = "serious AEs"),
         "Adverse events")$Row, "Nausea")
  expect_identical(
    recs(timeline_frame(tl_toy(), ae_show = "ongoing AEs"),
         "Adverse events")$Row, "Nausea")
  bs <- recs(timeline_frame(tl_toy(), ae_lanes = "body system"),
             "Adverse events")
  expect_setequal(bs$Row, c("Nervous", "Gi", "Skin", "Injury"))
})

test_that("con meds: uncoded names, clipped starts, before or on treatment", {
  f <- timeline_frame(tl_toy(), show = c("Con meds", "Treatment"))
  cm <- recs(f, "Con meds")
  # The uncoded vitamin keeps its reported name; ibuprofen ended before day 1
  # and is not active on treatment.
  expect_setequal(cm$Row, c("Vitamin D", "Paracetamol"))
  vit <- cm[cm$Row == "Vitamin D", ]
  # Clipped at the axis start, day 1 here; the days keep the real start.
  expect_equal(vit$Start, 1)
  expect_identical(vit$Days, "Day -400 to ongoing")
  expect_identical(as.character(vit$Colour), "Started before treatment")
  expect_identical(as.character(cm$Colour[cm$Row == "Paracetamol"]),
                   "Started on treatment")
  # Patients with a con med only.
  expect_identical(unique(f$USUBJID), "A")
  all <- recs(timeline_frame(tl_toy(), show = "Con meds", cm_records = "all"))
  expect_true("Ibuprofen" %in% all$Row)
  hid <- recs(timeline_frame(tl_toy(), show = "Con meds",
                             cm_prior = "hidden"))
  expect_false("Vitamin D" %in% hid$Row)
  ind <- recs(timeline_frame(tl_toy(), show = "Con meds",
                             cm_colour = "indication"))
  expect_setequal(as.character(ind$Colour), c("Prophylaxis", "Headache"))
})

test_that("con meds past max_lanes rows fold into Other con meds", {
  tb <- tl_toy()
  f <- timeline_frame(tb, show = TL_SHOW, max_lanes = 5)
  rows <- unique(recs(f)$Row[recs(f)$USUBJID == "A"])
  # Three AE rows and the drug leave room for one con-med row plus Other.
  expect_true("Other con meds" %in% rows)
  expect_length(rows, 5L)
  expect_false("Other con meds" %in%
                 recs(timeline_frame(tb, show = TL_SHOW))$Row)
})

test_that("a name used by two sources stays two rows", {
  tb <- tl_toy()
  tb$adcm$CMDECOD[[2L]] <- "HEADACHE"
  f <- timeline_frame(tb, show = TL_SHOW)
  r <- unique(recs(f)$Row[recs(f)$USUBJID == "A"])
  expect_true(all(c("Headache", "Headache (con med)") %in% r))
})

test_that("without ADEX the treatment is one bar from first to last dose", {
  tb <- tl_toy()
  tb$adex <- NULL
  trt <- recs(timeline_frame(tb), "Treatment")
  expect_identical(trt$Row, c("Treatment", "Treatment"))
  expect_equal(trt$End[trt$USUBJID == "A"], 60)
})

test_that("rows order by the setting, sources and onset inside a patient", {
  f <- timeline_frame(tl_toy())
  expect_identical(unique(f$USUBJID), c("A", "B"))
  a <- recs(f)[recs(f)$USUBJID == "A", ]
  expect_identical(unique(a$Row), c("Headache", "Nausea", "Rash", "Drug"))
  expect_identical(unique(timeline_frame(tl_toy(),
                                         order_by = "subject")$USUBJID),
                   c("A", "B"))
})

test_that("flags read as Y/N, TRUE/FALSE or 1/0", {
  tb <- tl_toy()
  tb$adae$TRTEMFL <- c("Y", "Y", "N", "Y")
  want <- timeline_frame(tb)
  expect_false("Rash" %in% want$Row)
  lgl <- tb
  lgl$adae$TRTEMFL <- lgl$adae$TRTEMFL == "Y"
  lgl$adae$AESER <- lgl$adae$AESER == "Y"
  expect_identical(timeline_frame(lgl), want)
  num <- tb
  num$adae$TRTEMFL <- as.integer(num$adae$TRTEMFL == "Y")
  expect_identical(timeline_frame(num), want)
})

test_that("the colour default follows what ADAE has", {
  tb <- tl_toy()
  expect_identical(levels(timeline_frame(tb)$Colour)[1:3],
                   c("Mild", "Moderate", "Severe"))
  tb$adae$AETOXGR <- c("1", "3", "2", "2")
  g <- recs(timeline_frame(tb), "Adverse events")
  expect_identical(as.character(g$Colour[g$Row == "Nausea"]), "Grade 3")
  tb$adae$AESEV <- NULL
  tb$adae$AETOXGR <- NULL
  n <- recs(timeline_frame(tb), "Adverse events")
  expect_true(all(n$Colour == "AE"))
  expect_error(timeline_frame(tb, ae_colour = "severity"), "severity")
})

test_that("SDTM table and column names read through the profile's catalog", {
  tb <- tl_toy()
  want <- timeline_frame(tb, show = TL_SHOW)
  ae <- tb$adae
  names(ae)[names(ae) == "ASTDY"] <- "AESTDY"
  names(ae)[names(ae) == "AENDY"] <- "AEENDY"
  cm <- tb$adcm
  names(cm)[names(cm) == "ASTDY"] <- "CMSTDY"
  names(cm)[names(cm) == "AENDY"] <- "CMENDY"
  ex <- tb$adex
  ex$EXSTDTC <- format(ex$ASTDT)
  ex$EXENDTC <- format(ex$AENDT)
  ex$ASTDT <- NULL
  ex$AENDT <- NULL
  d <- dm::dm(dm = tb$adsl, ae = ae, cm = cm, ex = ex)
  got <- timeline_frame(d, show = TL_SHOW)
  expect_identical(recs(got)$Row, recs(want)$Row)
  expect_identical(recs(got)$Start, recs(want)$Start)
})

test_that("the block saves as itself and draws a row per patient", {
  b <- new_timeline_block(show = c("Con meds", "Treatment"))
  expect_s3_class(b, "timeline_block")
  sr <- blockr.core::blockr_ser(b)
  expect_identical(sr$payload$script_values$show, c("Con meds", "Treatment"))
  b2 <- blockr.core::blockr_deser(sr)
  expect_s3_class(b2, "timeline_block")
  # The con-med columns are ticked when con meds are shown.
  nm <- vapply(sr$payload$summaries, function(s) s$name %||% "", "")
  expect_true(all(c("Con meds", "New on treatment") %in% nm))
  expect_false("AEs" %in% nm)
  ae <- vapply(timeline_summaries(), function(s) s$name %||% "", "")
  expect_true(all(c("Arm", "AEs", "Worst AE", "Study time") %in% ae))
})

test_that("the script's settings are its controls and run the frame", {
  sc <- timeline_script()
  ex <- parse(text = sc)
  vars <- vapply(ex[-length(ex)], function(e) as.character(e[[2L]]), "")
  expect_setequal(vars, TIMELINE_SETTINGS)
  env <- new.env()
  env$data <- tl_toy()
  expect_identical(eval(ex, env), timeline_frame(tl_toy()))
  # Every word in the sentence is a setting of the script.
  words <- regmatches(timeline_subtitle(),
                      gregexpr("\\{@([a-z_]+)\\}", timeline_subtitle()))[[1]]
  expect_true(all(gsub("[{}@]", "", words) %in% vars))
})

test_that("the table draws a patient row with its lanes and opens into rows", {
  viz <- asNamespace("blockr.viz")
  b <- new_timeline_block(show = TL_SHOW)
  st <- b[["state"]] %||% NULL
  spec <- viz$viz_block_spec("table")
  p <- blockr.core::blockr_ser(b)$payload
  p <- p[intersect(names(p), names(spec$args))]
  p[c("layout", "by", "group_rows", "missing", "sort_by")] <-
    list("groups", c("USUBJID", "Row"), "summary", "drop", "data")
  st <- viz$table_normalize(viz$viz_check(spec, p))
  m <- viz$table_model_groups(st, timeline_frame(tl_toy(), show = TL_SHOW))
  expect_null(m$prep$err)
  r <- m$prep$rows
  expect_identical(r$.label[r$.is_parent], c("A", "B"))
  # The patient row of A: three lanes, AEs, con meds and treatment.
  spans <- length(m$cells$cols)
  sl <- m$cells$cols[[spans]]$sl[[which(r$.label == "A")]]
  expect_identical(vapply(sl, function(l) l$n, ""),
                   c("Adverse events", "Con meds", "Treatment"))
  # A's rows: three AE terms, two con meds, the drug.
  kids <- r$.label[!r$.is_parent & r$.parent == "A"]
  expect_identical(kids, c("Headache", "Nausea", "Rash", "Vitamin D",
                           "Paracetamol", "Drug"))
  # Starts closed.
  expect_true(viz$table_groups_to_model(m)$collapsed)
})

test_that("the mapping reports the tables found, by their own names", {
  tb <- tl_toy()
  mp <- timeline_mapping(dm::dm(dm = tb$adsl, ae = tb$adae))
  roles <- vapply(mp$roles, `[[`, "", "role")
  expect_identical(roles, c("Patients", "Adverse events", "Con meds",
                            "Treatment"))
  found <- vapply(mp$roles, `[[`, NA, "found")
  expect_identical(found, c(TRUE, TRUE, FALSE, FALSE))
  expect_identical(mp$roles[[2L]]$table, "AE")
})

test_that("per-patient summaries group the records once", {
  p <- aet_per_patient(c("B", "A", "B", "X", "B"), c("A", "B", "C"))
  expect_identical(p$count(), c(A = 1, B = 3, C = 0))
  expect_identical(p$sum(c(TRUE, FALSE, TRUE, TRUE, FALSE)),
                   c(A = 0, B = 2, C = 0))
  expect_identical(p$max(c(2L, NA, 5L, 9L, NA)), c(A = NA, B = 5, C = NA))
  expect_identical(p$min(c(4, 1, 3, 0, 7)), c(A = 1, B = 3, C = NA))
})

test_that("a board restores the timeline with its settings", {
  blk <- new_timeline_block(show = c("Adverse events", "Treatment"),
                            cm_colour = "class", open = "all")
  rt <- board_round_trip(blk, tl_toy())
  expect_s3_class(rt$block, "timeline_block")
  expect_equal(plain_state(rt$restored), plain_state(rt$saved),
               ignore_attr = TRUE)
  expect_identical(as.character(rt$restored$script_values$cm_colour),
                   "class")
})

# Patient A's con meds with one class each, and a dose per period.
tl_many <- function(n_cls = 30, n_dose = 40) {
  tb <- tl_toy()
  k <- max(n_cls, n_dose)
  tb$adcm <- data.frame(
    USUBJID = "A", CMDECOD = paste("DRUG", seq_len(n_cls)),
    CMTRT = paste("drug", seq_len(n_cls)),
    CMCLAS = paste("CLASS", seq_len(n_cls)), CMINDC = "PAIN",
    ASTDY = seq_len(n_cls), AENDY = seq_len(n_cls) + 3,
    stringsAsFactors = FALSE
  )
  tb$adex <- data.frame(
    USUBJID = "A", EXTRT = "DRUG", EXDOSE = 150 + 3 * seq_len(n_dose),
    EXDOSU = "mg",
    ASTDT = as.Date("2024-01-01") + seq_len(n_dose) - 1,
    AENDT = as.Date("2024-01-01") + seq_len(n_dose) - 1,
    stringsAsFactors = FALSE
  )
  tb
}

test_that("the colours stay within what the table draws", {
  tb <- tl_many()
  f <- timeline_frame(tb, show = TL_SHOW, cm_colour = "class")
  lv <- levels(f$Colour)
  expect_lte(length(lv), TL_MAX_COLOURS)
  # Con meds keep their most common classes and fold the rest.
  expect_true("Other class" %in% lv)
  # Doses fall into ranges, lowest first.
  dose <- lv[grepl(" mg$", lv)]
  expect_lte(length(dose), TL_MAX_TRT_COLOURS)
  expect_identical(dose[[1L]], sub(" to .*", "", dose[[1L]]) |>
                     paste0(" to ", sub(".* to ", "", dose[[1L]])))
  lo <- as.numeric(sub(" .*", "", dose))
  expect_identical(lo, sort(lo))
  rng <- recs(f, "Treatment")
  expect_true(all(as.character(rng$Colour) %in% dose))
  # Every show and colour choice the block offers stays within the limit.
  for (sh in list(TL_SHOW, c("Con meds", "Treatment"), "Con meds")) {
    for (cc in TL_CM_COLOUR) {
      for (tc in TL_TRT_COLOUR) {
        for (ac in TL_AE_COLOUR[TL_AE_COLOUR != "grade"]) {
          g <- timeline_frame(tb, show = sh, cm_colour = cc, trt_colour = tc,
                              ae_colour = ac)
          expect_lte(nlevels(g$Colour), TL_MAX_COLOURS)
        }
      }
    }
  }
  # Few doses stay one level each.
  few <- timeline_frame(tl_toy())
  expect_true(all(c("54 mg", "81 mg") %in% levels(few$Colour)))
})

test_that("con meds colour by before or on treatment by default", {
  f <- timeline_frame(tl_many(), show = "Con meds")
  expect_setequal(levels(f$Colour), "Started on treatment")
  # The old default's name still reads.
  g <- timeline_frame(tl_many(), show = "Con meds",
                      cm_colour = "class, else before or on treatment")
  expect_identical(levels(g$Colour), levels(f$Colour))
})

test_that("each colour level names the value it stands for", {
  tb <- tl_many(n_cls = 3, n_dose = 2)
  tb$adae$ATOXGR <- c("1", "3", "2", "2")
  f <- timeline_frame(tb, show = TL_SHOW, cm_colour = "class",
                      trt_colour = "drug")
  k <- attr(f$Colour, "blockr_scale_keys")
  expect_identical(k$key[k$level == "Grade 3"], c("3", "3"))
  expect_setequal(k$var[k$level == "Grade 3"], c("ATOXGR", "AETOXGR"))
  expect_identical(k$key[k$level == "Class 2" & k$var == "CMCLAS"], "CLASS 2")
  expect_identical(unique(k$key[k$level == "Drug"]), "DRUG")
  # A severity colour names both spellings.
  s <- attr(timeline_frame(tl_toy())$Colour, "blockr_scale_keys")
  expect_setequal(s$var[s$level == "Mild"], c("ASEV", "AESEV"))
})

test_that("a colour the board's scale map sets wins in the table", {
  skip_if_not_installed("blockr.theme")
  f <- timeline_frame(tl_many(n_cls = 3, n_dose = 2), show = TL_SHOW,
                      cm_colour = "class")
  map <- blockr.theme::new_scale_map(
    blockr.theme::scale_binding("AESEV", color = c(MILD = "#00ff00")),
    blockr.theme::scale_binding("CMCLAS", color = c("CLASS 2" = "#0000ff"))
  )
  lv <- levels(f$Colour)
  got <- blockr.viz:::groups_level_colors(map, "Colour", lv, f$Colour)
  own <- attr(f$Colour, "blockr_colors")
  expect_identical(got[["Mild"]], "#00ff00")
  expect_identical(got[["Class 2"]], "#0000ff")
  expect_identical(got[["Severe"]], own[["Severe"]])
  expect_identical(got[["Class 1"]], own[["Class 1"]])
})

test_that("patients sort by what is shown", {
  tb <- tl_toy()
  tb$adcm$USUBJID <- c("B", "B", "B")
  tb$adcm$ASTDY <- c(2, 2, 3)
  tb$adcm$AENDY <- NA
  f <- timeline_frame(tb, show = "Con meds")
  expect_identical(unique(f$USUBJID), "B")
  g <- timeline_frame(tb, show = TL_SHOW)
  # A: 3 AEs; B: 1 AE and 3 con meds.
  expect_identical(unique(g$USUBJID), c("B", "A"))
})

test_that("boards saved with weeks or the old defaults restore", {
  old <- new_timeline_block(
    script_values = list(show = c("Con meds", "Treatment"), unit = "weeks",
                         order_by = "number of AEs",
                         cm_colour = "class, else before or on treatment"),
    subtitle = paste0("{@show} by study {@unit}, one stripe per patient, ",
                      "sorted by {@order_by}")
  )
  st <- board_round_trip(old, tl_toy())$saved
  sv <- lapply(st$script_values, as.character)
  expect_null(sv$unit)
  expect_identical(sv$order_by, "number of events")
  expect_identical(sv$cm_colour, "before or on treatment")
  expect_false(grepl("unit", st$subtitle))
  expect_match(st$subtitle, "in study days")
  # A board with AEs keeps its sort by AEs.
  expect_identical(timeline_migrate(list(order_by = "number of AEs"))$order_by,
                   "number of AEs")
  expect_false(grepl("unit", timeline_script()))
})
