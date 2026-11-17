# new_timeline_block() and its frame, patient_timeline_frame(): the
# timeline's layers with the swimmer's response and discontinuation added.

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
  # The profile's colours, as it draws the same grades.
  expect_identical(unname(attr(f$Colour, "blockr_colors")),
                   unname(pp_sev_colors[c("1", "2", "3")]))
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

test_that("the timeline is one block and one menu entry", {
  b <- new_timeline_block()
  expect_s3_class(b, "timeline_block")
  expect_identical(blockr.core::registry_id_from_block(b), "timeline_block")
  reg <- blockr.core::list_blocks()
  expect_true("timeline_block" %in% reg)
  expect_false(any(c("efficacy_timeline_block", "safety_timeline_block") %in%
                     reg))
  p <- blockr.core::blockr_ser(b)$payload
  # No listing columns: the timeline alone, the patients in ID order.
  expect_identical(vapply(p$summaries, function(s) s$type, ""), "spans")
  expect_identical(p$script_values$sort_col, "")
  expect_identical(p$title, "Timeline")
  # The layers it draws are the script's default, the AEs.
  expect_identical(
    as.character(eval(parse(text = patient_timeline_script())[[1L]][[3L]])),
    c("Adverse events", "Discontinuation and death")
  )
  # Picked layers are kept.
  sv <- blockr.core::blockr_ser(
    new_timeline_block(layers = c("Treatment", "Response"))
  )$payload$script_values
  expect_identical(sv$layers, c("Response", "Treatment"))
})

test_that("the lane settings show only while their layer is drawn", {
  viz <- asNamespace("blockr.viz")
  specs <- function(layers) {
    parsed <- viz$cb_parse(patient_timeline_script())
    sp <- viz$cb_specs(parsed, pt_toy(), list(layers = layers))
    off <- vapply(sp, function(s) isTRUE(s$off), NA)
    stats::setNames(off, vapply(sp, `[[`, "", "name"))
  }
  off <- specs("Adverse events")
  expect_false(any(off[c("ae_terms", "ae_lanes", "ae_colour", "serious")]))
  expect_true(all(off[c("cm_lanes", "cm_colour", "response", "trt_colour",
                        "dose_tags")]))
  expect_false(any(off[c("layers", "group", "origin", "cutoff")]))
  off <- specs(c("Response", "Con meds"))
  expect_true(all(off[c("ae_lanes", "trt_colour")]))
  expect_false(any(off[c("cm_lanes", "response")]))
})

test_that("the subtitle names the term level of the layers drawn", {
  viz <- asNamespace("blockr.viz")
  word <- function(layers, values = list()) {
    parsed <- viz$cb_parse(patient_timeline_script())
    sp <- viz$cb_specs(parsed, pt_toy(),
                       c(list(layers = layers), values))
    args <- viz$script_title_args(list(), sp,
                                  c(list(layers = layers), values))
    viz$resolve_title_template(patient_timeline_subtitle(), data.frame(),
                               args)
  }
  expect_identical(word("Adverse events"),
                   paste("Adverse events in study days first to last, AEs by",
                         "Dictionary-Derived Term"))
  expect_identical(
    word(c("Adverse events", "Con meds"), list(ae_lanes = "AEBODSYS")),
    paste("Adverse events, Con meds in study days first to last, AEs by Body",
          "System or Organ Class, con meds by Standardized Medication Name")
  )
  expect_identical(word(c("Response", "Treatment")),
                   "Response, Treatment in study days first to last, drugs: all")
  expect_identical(word("Response", list(day_from = 1, day_to = 180)),
                   "Response in study days 1 to 180")
})

test_that("the term levels are the columns the data has", {
  ch <- timeline_lane_choices(pt_toy(), "ae")
  expect_true(all(ch %in% names(pt_toy()$adae)))
  expect_identical(unname(ch[attr(ch, "default")]), "AEDECOD")
  expect_true("Dictionary-Derived Term" %in% names(ch))
  cm <- timeline_lane_choices(pt_toy(), "cm")
  expect_identical(unname(cm[attr(cm, "default")]), "CMDECOD")
  expect_identical(names(timeline_lane_choices(list(), "cm")),
                   names(TL_CM_LANES))
  f <- patient_timeline_frame(pt_toy(), ae_lanes = "AEBODSYS")
  ae_rows <- unique(f$Row[f$.section %in% "Adverse events"])
  expect_true(all(toupper(ae_rows) %in% toupper(pt_toy()$adae$AEBODSYS)))
})

test_that("the script's settings are its controls and run the frame", {
  ex <- parse(text = patient_timeline_script())
  vars <- vapply(ex[-length(ex)], function(e) as.character(e[[2L]]), "")
  expect_true(all(PT_SETTINGS %in% vars))
  env <- new.env()
  env$data <- pt_toy()
  expect_identical(eval(ex, env), patient_timeline_frame(pt_toy()))
})

# The table model of a patient timeline with these layers on pt_toy().
pt_model <- function(layers) {
  viz <- asNamespace("blockr.viz")
  b <- new_timeline_block(layers = layers)
  spec <- viz$viz_block_spec("table")
  p <- blockr.core::blockr_ser(b)$payload
  p <- p[intersect(names(p), names(spec$args))]
  p[c("layout", "by", "group_rows", "missing", "sort_by", "row_sections")] <-
    list("groups", c("USUBJID", "Row"), "summary", "drop", "data", ".section")
  st <- viz$table_normalize(viz$viz_check(spec, p))
  viz$table_model_groups(st, patient_timeline_frame(pt_toy(), layers = layers))
}

test_that("the table draws a stripe per layer and opens into rows", {
  m <- pt_model(PT_LAYERS)
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
  # Opened, AE terms and con meds only, each layer under its title.
  kids <- r$.label[!r$.is_parent & r$.parent == "A"]
  expect_identical(kids, c("Headache", "Nausea", "Paracetamol"))
  title <- m$cells$title[!r$.is_parent & r$.parent == "A"]
  expect_identical(title, c("Adverse events", NA, "Con meds"))
  # The axis counts study days as the patient profile does: D1, no day 0.
  ax <- asNamespace("blockr.viz")$table_groups_axis(m$prep$plan[[spans]],
                                                     m$prep)
  expect_true("D1" %in% ax$labels)
  expect_false("D0" %in% ax$labels)
  expect_true(all(startsWith(ax$labels, "D")))
})

test_that("one layer open has no title; treatment or response alone no rows", {
  m <- pt_model(c("Response", "Adverse events"))
  r <- m$prep$rows
  expect_identical(r$.label[!r$.is_parent & r$.parent == "A"],
                   c("Headache", "Nausea"))
  expect_null(m$cells$title)
  # Nothing opens: one row per patient, none under it.
  for (layers in list("Treatment", "Response", c("Treatment", "Response"))) {
    m <- pt_model(layers)
    expect_null(m$prep$err)
    expect_false(any(m$prep$rows$.level > 0L))
    cell <- m$cells$cols[[length(m$cells$cols)]]
    expect_true(any(lengths(cell$segs) > 0L) || any(lengths(cell$sl) > 0L))
  }
})

test_that("treatment draws a line per drug, or bars by dose", {
  f <- patient_timeline_frame(pt_toy(), layers = c("Response", "Treatment"))
  d <- f[f$.lane %in% "Drug", ]
  # One line from the first dose, cut from 54 to 81 mg: a step up, so solid.
  expect_identical(unique(d$USUBJID), c("A", "C"))
  # The study lane (the end-of-study tag) heads the drugs.
  expect_identical(levels(f$.lane)[1:3], c("Study", "Drug", "Placebo"))
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
  tag <- f[!is.na(f$.tag) & !f$Event %in% "Dose change", ]
  expect_identical(as.character(tag$.lane), "Drug")
  # Tagged on request, the drug's dose change is a tag on its lane too,
  # with the new dose.
  expect_false(any(f$Event %in% "Dose change"))
  f <- patient_timeline_frame(pt_toy(), layers = c("Treatment",
                                                   "Discontinuation and death"),
                              dose_tags = TRUE)
  ch <- f[f$Event %in% "Dose change", ]
  expect_identical(unique(as.character(ch$.lane)), "Drug")
  expect_identical(c(ch$USUBJID, ch$.tag), c("A", "81"))
  expect_identical(ch$Details, "Drug, 54 mg to 81 mg")
  expect_identical(attr(f$.tag, "label"), "Tag")
  # Without treatment drawn it marks the patient row.
  f <- patient_timeline_frame(pt_toy())
  expect_true(is.na(f$.lane[!is.na(f$.tag)]))
})

test_that("a board restores the patient timeline with its layers", {
  blk <- new_timeline_block(layers = c("Adverse events", "Con meds"),
                            ae_terms = "Preferred term: Headache")
  rt <- board_round_trip(blk, pt_toy())
  expect_s3_class(rt$block, "timeline_block")
  expect_equal(plain_state(rt$restored), plain_state(rt$saved),
               ignore_attr = TRUE)
  expect_identical(as.character(unlist(rt$restored$script_values$layers)),
                   c("Adverse events", "Con meds"))
})

test_that("the response follows a study map under any of its names", {
  f <- patient_timeline_frame(pt_toy(), layers = "Response")
  viz <- asNamespace("blockr.viz")
  own <- attr(f$Colour, "blockr_colors")
  for (var in c("AVALC", "BEST_OVERALL_RESPONSE", "response")) {
    map <- blockr.theme::new_scale_map(
      blockr.theme::scale_binding(var, color = c(PR = "#FFD700"))
    )
    got <- viz$groups_theme_keyed(own, map, f$Colour)
    expect_identical(got[["PR"]], "#FFD700")
    expect_identical(got[["CR"]], own[["CR"]])
  }
})

test_that("an opened patient lists every con med", {
  d <- pt_toy()
  d$adcm <- data.frame(USUBJID = "A", CMDECOD = sprintf("DRUG %02d", 1:15),
                       CMTRT = "x", CMCLAS = "ANALGESICS", ASTDY = 2,
                       AENDY = 6, stringsAsFactors = FALSE)
  f <- patient_timeline_frame(d, layers = "Con meds")
  rows <- unique(f$Row[!is.na(f$Row)])
  expect_length(rows, 15L)
  expect_false("Other con meds" %in% rows)
})

test_that("two episodes of one AE term stay on one bar when opened", {
  # Diarrhoea on days 10-20 and 18-30 (overlapping), again 32-40, and
  # Dizziness 10-15, starting with the first.
  d <- pt_toy()
  d$adae <- data.frame(
    USUBJID = "A", AEDECOD = c("DIARRHOEA", "DIARRHOEA", "DIARRHOEA",
                               "DIZZINESS"),
    AEBODSYS = "GASTROINTESTINAL DISORDERS",
    AESEV = c("MILD", "MODERATE", "MILD", "MILD"), AESER = "N",
    TRTEMFL = "Y", ASTDY = c(10, 18, 32, 10), AENDY = c(20, 30, 40, 15),
    stringsAsFactors = FALSE
  )
  viz <- asNamespace("blockr.viz")
  # Each row's tracks: its main lane, then its full bars, as segment lists.
  tracks <- function(layers) {
    b <- new_timeline_block(layers = layers)
    spec <- viz$viz_block_spec("table")
    p <- blockr.core::blockr_ser(b)$payload
    p <- p[intersect(names(p), names(spec$args))]
    p[c("layout", "by", "group_rows", "missing", "sort_by")] <-
      list("groups", c("USUBJID", "Row"), "summary", "drop", "data")
    st <- viz$table_normalize(viz$viz_check(spec, p))
    m <- viz$table_model_groups(st, patient_timeline_frame(d, layers = layers))
    cell <- m$cells$cols[[length(m$cells$cols)]]
    out <- lapply(seq_along(m$cells$label), function(i) {
      full <- Filter(function(l) isTRUE(l$f), cell$sl[[i]] %||% list())
      c(list(list(s = cell$segs[[i]], t = cell$tips[[i]])),
        lapply(full, function(l) list(s = l$s, t = l$t)))
    })
    out <- lapply(out, function(x) Filter(function(k) length(k$s), x))
    stats::setNames(out, m$cells$label)
  }
  # AEs alone (the main lane), and under the response (a full bar).
  for (layers in list("Adverse events", c("Response", "Adverse events"))) {
    tr <- tracks(layers)
    # One track, cut to the worst grade: 10-18 mild, 18-30 moderate, 32-40
    # mild. The AE records under the table icon list the episodes.
    expect_length(tr$Diarrhoea, 1L)
    expect_length(tr$Diarrhoea[[1L]]$s, 3L)
    expect_length(tr$Dizziness, 1L)
  }
})

test_that("no tooltip names a field by its raw column name", {
  m <- pt_model(PT_LAYERS)
  cell <- m$cells$cols[[length(m$cells$cols)]]
  lane_tips <- unlist(lapply(cell$sl, function(ls) {
    lapply(ls, function(l) c(l$t, l$et))
  }))
  tips <- c(unlist(cell$tips), unlist(cell$et), lane_tips)
  expect_true(length(tips) > 0L)
  # "Name: value" parts after the head; none named by a dotted column.
  parts <- unlist(strsplit(tips, " \u00b7 ", fixed = FALSE))
  names <- sub(":.*$", "", parts[grepl(":", parts)])
  expect_false(any(startsWith(trimws(names), ".")), info = paste(names,
                                                                 collapse = ", "))
  expect_true(any(grepl("Study day: ", tips, fixed = TRUE)))
})

test_that("the subtitle does not name the sort", {
  expect_false(grepl("order_by|sort", patient_timeline_subtitle()))
})

test_that("per-patient summaries group the records once", {
  p <- aet_per_patient(c("B", "A", "B", "X", "B"), c("A", "B", "C"))
  expect_identical(p$count(), c(A = 1, B = 3, C = 0))
  expect_identical(p$sum(c(TRUE, FALSE, TRUE, TRUE, FALSE)),
                   c(A = 0, B = 2, C = 0))
  expect_identical(p$max(c(2L, NA, 5L, 9L, NA)), c(A = NA, B = 5, C = NA))
  expect_identical(p$min(c(4, 1, 3, 0, 7)), c(A = 1, B = 3, C = NA))
})
