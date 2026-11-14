# The patient timeline's Patients tab: Group by, the listing columns and the
# sort by the first of them (pt_toy() is in test-patient-timeline.R).

pt_ids <- function(f) unique(as.character(f$USUBJID))

test_that("Group by offers ADSL's columns, the Group column first", {
  d <- pt_toy()
  g <- patient_timeline_groups(d)
  expect_identical(unname(g[[1L]]), "ACTARM")
  expect_identical(unname(g[[length(g)]]), PT_NO_GROUP)
  expect_identical(names(g)[[length(g)]], "No grouping")
  expect_false(any(c("USUBJID", "TRTSDT") %in% g))
  d$adsl$Group <- c("x", "y", "x")
  g <- patient_timeline_groups(d)
  expect_identical(unname(g[1:2]), c("Group", "ACTARM"))
  expect_identical(names(g)[[1L]], "Group (population filter)")
})

test_that("the frame groups by Group, else the arm, or not at all", {
  d <- pt_toy()
  arm <- function(f) unique(f$.arm[!is.na(f$.arm)])
  expect_setequal(arm(patient_timeline_frame(d)), c("Drug", "Placebo"))
  d$adsl$Group <- c("x", "y", "x")
  expect_setequal(arm(patient_timeline_frame(d)), c("x", "y"))
  expect_setequal(arm(patient_timeline_frame(d, group = "TRT01A")),
                  c("Drug 54 mg", "Placebo"))
  expect_length(arm(patient_timeline_frame(d, group = PT_NO_GROUP)), 0L)
})

test_that("every ADSL column is on the patient's record, with its label", {
  d <- pt_toy()
  d$adsl$BMIBL <- c(22.5, 30.1, 25)
  attr(d$adsl$BMIBL, "label") <- "Baseline BMI"
  f <- patient_timeline_frame(d)
  expect_true(all(c("DCTREAS", "BMIBL") %in% names(f)))
  expect_identical(attr(f$BMIBL, "label"), "Baseline BMI")
  # One value per patient, on the patient row's records only.
  expect_true(all(is.na(f$BMIBL[!is.na(f$Row)])))
  expect_identical(unique(f$BMIBL[!is.na(f$BMIBL)]), c(22.5, 30.1))
})

test_that("the patients sort by the first column, records kept together", {
  d <- pt_toy()
  f <- patient_timeline_frame(d, layers = c("Treatment", "Response"))
  expect_identical(pt_ids(patient_timeline_sort(f, "")), c("A", "B", "C"))
  # Most days on treatment first; C, still on treatment, has none.
  expect_identical(pt_ids(patient_timeline_sort(f, "Days on treatment")),
                   c("A", "B", "C"))
  s <- patient_timeline_sort(f, "TRT01A")
  expect_identical(pt_ids(s), c("A", "C", "B"))
  for (id in pt_ids(s)) {
    at <- which(s$USUBJID == id)
    expect_identical(at, seq(min(at), max(at)))
  }
  # Attributes the table reads survive the reorder.
  expect_identical(attributes(s$Colour), attributes(f$Colour))
  expect_identical(attr(s, "viz_records"), attr(f, "viz_records"))
  # A column the frame lacks: by patient.
  expect_identical(pt_ids(patient_timeline_sort(f, "NOPE")), pt_ids(s)[
    order(pt_ids(s))])
})

test_that("best response sorts best first, worst AE worst first", {
  f <- patient_timeline_frame(pt_toy(),
                              layers = c("Response", "Adverse events"))
  expect_identical(pt_ids(patient_timeline_sort(f, "Best response")),
                   c("C", "A", "B"))
  expect_identical(pt_ids(patient_timeline_sort(f, "Worst AE"))[1:2],
                   c("A", "B"))
})

test_that("a column the script adds sorts and is offered", {
  f <- patient_timeline_frame(pt_toy())
  f$score <- ifelse(is.na(f$Row), c(A = 2, B = 1, C = 3)[f$USUBJID], NA)
  expect_identical(pt_ids(patient_timeline_sort(f, "score")), c("B", "A"))
  g <- patient_timeline_gear("safety")
  more <- vapply(blockr.viz:::table_gear_more(g, f), `[[`, "", "name")
  expect_true(all(c("score", "TRT01A", "DCTREAS") %in% more))
  # The frame's own columns, and AE terms (several per patient), are not.
  expect_false(any(c("Row", "Type", "Details", "Event") %in% more))
})

test_that("saved listing columns become the picks, the arm dropped", {
  b <- new_safety_timeline_block(
    summaries = list(
      list(type = "field", col = ".arm", name = "Arm"),
      list(type = "field", col = "AGE", name = "Age"),
      patient_timeline_lane()
    ),
    order_by = "worst AE", group = ""
  )
  p <- blockr.core::blockr_ser(b)$payload
  nm <- vapply(p$summaries, function(s) s$name %||% "", "")
  expect_identical(nm[1:2], c("Worst AE", "Age"))
  expect_true(isTRUE(p$summaries[[2L]]$optional))
  expect_identical(p$script_values$sort_col, "Worst AE")
  expect_null(p$script_values$order_by)
  expect_null(p$script_values$group)
  # A saved group stays; a sort without a column keeps the columns.
  b <- new_safety_timeline_block(
    summaries = list(list(type = "field", col = "AGE", name = "Age"),
                     patient_timeline_lane()),
    order_by = "first onset", group = "TRT01A"
  )
  p <- blockr.core::blockr_ser(b)$payload
  expect_identical(p$script_values$group, "TRT01A")
  expect_identical(p$script_values$sort_col, "AGE")
})

test_that("the gear has Group by and the several-picks columns", {
  g <- patient_timeline_gear("safety")
  pat <- g$tabs[[2L]]
  expect_identical(pat$title, "Patients")
  expect_identical(unlist(lapply(pat$sections, `[[`, "keys")),
                   c("group", "columns"))
  expect_identical(g$pick_by, "USUBJID")
  expect_identical(g$sort_value, "sort_col")
  nm <- vapply(g$columns, `[[`, "", "name")
  expect_false(any(c("Arm", "Age", "Sex") %in% nm))
})

test_that("the table heads each group of patients with its value", {
  viz <- asNamespace("blockr.viz")
  b <- new_safety_timeline_block()
  spec <- viz$viz_block_spec("table")
  p <- blockr.core::blockr_ser(b)$payload
  p <- p[intersect(names(p), names(spec$args))]
  p[c("layout", "by", "group_rows", "missing", "sort_by", "row_sections",
      "row_groups")] <- list("groups", c("USUBJID", "Row"), "summary", "drop",
                             "data", ".section", ".arm")
  st <- viz$table_normalize(viz$viz_check(spec, p))
  m <- viz$table_model_groups(st, patient_timeline_frame(pt_toy()))
  expect_identical(vapply(m$prep$sections, `[[`, "", "value"),
                   c("Drug", "Placebo"))
  expect_true(all(vapply(m$prep$sections, `[[`, TRUE, "open")))
})
