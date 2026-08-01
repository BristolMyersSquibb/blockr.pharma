# Findings source plans and visit ordering.

test_that("params living only in adlb get cards even when both splits exist", {
  # adlb was dropped entirely when adlbc AND adlbh were present, so any
  # PARAMCD living only in adlb got no viz and no coverage entry.
  mk <- function(pcs) {
    data.frame(
      USUBJID = "x", PARAMCD = pcs, AVAL = seq_along(pcs),
      ADT = as.Date("2020-01-01"), stringsAsFactors = FALSE
    )
  }
  dm_obj <- dm::dm(
    adlbc = mk(c("ALT", "AST")),
    adlbh = mk(c("WBC", "HGB")),
    adlb = mk(c("ALT", "TRIG"))  # TRIG lives ONLY in adlb
  )
  vizs <- pp_findings_vizs(dm_obj)

  # No category column anywhere here, so each table is one card.
  expect_true("adlb_all" %in% names(vizs))
  expect_identical(names(vizs$adlb_all$params), "TRIG")
  # ...and ALT is not duplicated: adlbc claimed it first.
  expect_identical(sort(names(vizs$adlbc_all$params)), c("ALT", "AST"))
})

test_that("cards group by the study's own category column", {
  mk <- function(pcs, params, cats) {
    data.frame(
      USUBJID = "x", PARAMCD = pcs, PARAM = params, PARCAT1 = cats,
      AVAL = seq_along(pcs), ADT = as.Date("2020-01-01"),
      stringsAsFactors = FALSE
    )
  }
  dm_obj <- dm::dm(
    adlb = mk(
      c("ALT", "AST", "WBC", "GLUC"),
      c("Alanine Aminotransferase (U/L)", "Aspartate Aminotransferase (U/L)",
        "Leukocytes (10^9/L)", "Glucose (mmol/L)"),
      c("CHEMISTRY", "CHEMISTRY", "HEMATOLOGY", NA)
    )
  )
  vizs <- pp_findings_vizs(dm_obj)

  expect_setequal(
    names(vizs),
    c("adlb_chemistry", "adlb_hematology", "adlb_uncategorized")
  )
  # Shouted category values are title-cased for display; the card holding a
  # parameter with no category says so rather than pretending to be "Other",
  # which is a value some studies ship themselves.
  expect_identical(vizs$adlb_chemistry$label, "Chemistry")
  expect_identical(vizs$adlb_uncategorized$label, "Laboratory: Uncategorized")
  expect_setequal(names(vizs$adlb_chemistry$params), c("ALT", "AST"))
})

test_that("a card is searchable by full parameter name, not just code", {
  # The reported bug: searching "alanine" matched nothing, because the only
  # text on a card was a fixed prose blurb naming some parameters and not
  # others. Every code AND every PARAM now rides on the card.
  dm_obj <- dm::dm(
    adlb = data.frame(
      USUBJID = "x", PARAMCD = c("ALT", "AST", "WBC"),
      PARAM = c("Alanine Aminotransferase (U/L)",
                "Aspartate Aminotransferase (U/L)",
                "Leukocytes (10^9/L)"),
      PARCAT1 = c("CHEMISTRY", "CHEMISTRY", "HEMATOLOGY"),
      AVAL = 1:3, ADT = as.Date("2020-01-01"),
      stringsAsFactors = FALSE
    )
  )
  vizs <- pp_findings_vizs(dm_obj)
  expect_match(tolower(vizs$adlb_chemistry$search), "alanine")
  expect_match(vizs$adlb_chemistry$search, "ALT")

  # ...and the parameter-level index points the click at the right card
  idx <- pp_param_index(vizs)
  hit <- Filter(function(p) grepl("alanine", p$search), idx)
  expect_length(hit, 1L)
  expect_identical(hit[[1L]]$viz_id, "adlb_chemistry")
  expect_identical(hit[[1L]]$code, "ALT")
  # the chip caption drops the unit parenthetical the axis already carries
  expect_identical(hit[[1L]]$short, "Alanine Aminotransferase")
})

test_that("a parameter with no PARAM falls back to its code", {
  dm_obj <- dm::dm(
    adlb = data.frame(
      USUBJID = "x", PARAMCD = "TRIG", AVAL = 1, ADT = as.Date("2020-01-01"),
      stringsAsFactors = FALSE
    )
  )
  vizs <- pp_findings_vizs(dm_obj)
  expect_identical(unname(vizs$adlb_all$params[["TRIG"]]), "TRIG")
})

test_that("a card shows three parameters until told otherwise", {
  dm_obj <- dm::dm(
    adlb = data.frame(
      USUBJID = "x", PARAMCD = c("A", "B", "C", "D", "E"),
      AVAL = 1:5, ADT = as.Date("2020-01-01"),
      stringsAsFactors = FALSE
    )
  )
  vizs <- pp_findings_vizs(dm_obj)
  expect_length(pp_viz_defaults(vizs$adlb_all)$items, 3L)
})

test_that("a category column with one value does not group anything", {
  # The split lab tables carry PARCAT1 = "CHEM" on every row. Honouring it
  # would title the card with the sponsor's abbreviation for a distinction
  # the table already makes, so the table stays the group.
  dm_obj <- dm::dm(
    adlbc = data.frame(
      USUBJID = "x", PARAMCD = c("ALT", "AST"), PARCAT1 = "CHEM",
      AVAL = 1:2, ADT = as.Date("2020-01-01"), stringsAsFactors = FALSE
    )
  )
  vizs <- pp_findings_vizs(dm_obj)
  expect_identical(names(vizs), "adlbc_all")
  expect_identical(vizs$adlbc_all$label, "Chemistry")
})

test_that("the catalog signature ignores card ORDER", {
  # pp_findings_vizs() emits per-param cards in the patient's PARAMCD
  # order, so the same cards arrive in different sequences across drills.
  # An order-sensitive compare read that as a change and re-rendered the
  # sidebar (prod: "CHANGED (32 vizs; + -)" -- set diff empty both ways).
  vizs <- patient_profile_static_vizs()
  expect_identical(
    pp_vizs_signature(vizs),
    pp_vizs_signature(rev(vizs))
  )
  # ...but a card whose CONTENT differs is still a change
  other <- vizs
  other[[1L]]$label <- "Renamed"
  expect_false(identical(pp_vizs_signature(vizs), pp_vizs_signature(other)))
})

test_that("equal data yields an identical catalog signature", {
  mk <- function() {
    dm::dm(
      adsl = data.frame(USUBJID = "x", ACTARM = "Placebo",
                        stringsAsFactors = FALSE),
      advs = data.frame(USUBJID = "x", PARAMCD = c("SYSBP", "PULSE"),
                        AVAL = c(120, 60), ADT = as.Date("2020-01-01"),
                        stringsAsFactors = FALSE)
    )
  }
  cat1 <- c(patient_profile_static_vizs(), pp_findings_vizs(mk()))
  cat2 <- c(patient_profile_static_vizs(), pp_findings_vizs(mk()))

  # fresh closures never compare identical -- the signature must
  expect_false(identical(cat1, cat2))
  expect_identical(pp_vizs_signature(cat1), pp_vizs_signature(cat2))

  # ...and a new PARAMCD is a real catalog change
  dm3 <- dm::dm(
    adsl = data.frame(USUBJID = "x", ACTARM = "Placebo",
                      stringsAsFactors = FALSE),
    advs = data.frame(USUBJID = "x", PARAMCD = c("SYSBP", "PULSE", "RESP"),
                      AVAL = c(120, 60, 16), ADT = as.Date("2020-01-01"),
                      stringsAsFactors = FALSE)
  )
  cat3 <- c(patient_profile_static_vizs(), pp_findings_vizs(dm3))
  expect_false(identical(pp_vizs_signature(cat1), pp_vizs_signature(cat3)))
})

test_that("the accumulated dictionary keeps definitions patient-independent", {
  # The drill architecture feeds the profile ONE patient per upstream update,
  # and every card field now comes from the data. A dictionary that shrank
  # with the patient would rewrite labels and chip lists on every drill and
  # re-render the whole sidebar, so it accumulates instead: the cards
  # describe the STUDY, whichever patient is on screen.
  mk <- function(pcs) {
    dm::dm(
      adsl = data.frame(USUBJID = "x", ACTARM = "Placebo",
                        stringsAsFactors = FALSE),
      adlbc = data.frame(USUBJID = "x", PARAMCD = pcs, PARCAT1 = "CHEMISTRY",
                         AVAL = seq_along(pcs), ADT = as.Date("2020-01-01"),
                         stringsAsFactors = FALSE)
    )
  }
  cohort <- pp_dm_param_dict(mk(c("ALT", "AST", "BILI", "GGT")))
  # ...then a drill leaves one patient, carrying two of the four
  drilled <- pp_dm_param_dict(mk(c("ALT", "GGT")))

  merged <- pp_param_dict_merge(cohort, drilled)
  expect_identical(merged, cohort)

  tbls <- "adlbc"
  expect_identical(
    pp_vizs_signature(pp_findings_vizs_from_dict(merged, tbls)),
    pp_vizs_signature(pp_findings_vizs_from_dict(cohort, tbls))
  )

  # ...and a parameter seen for the first time IS a real change, kept
  extra <- pp_param_dict_merge(
    cohort, pp_dm_param_dict(mk(c("ALT", "CHOL")))
  )
  expect_true("CHOL" %in% extra$paramcd)
  expect_true(all(cohort$paramcd %in% extra$paramcd))

  # the chips still resolve their choices against the data on hand
  ctrl <- pp_findings_vizs_from_dict(merged, tbls)$adlbc_all$controls$items
  expect_null(ctrl$choices)
  expect_identical(ctrl$choices_from, "PARAMCD")
  expect_true(all(c("ALT", "AST", "GGT") %in% ctrl$choices_subset))
})

test_that("visit levels order by AVISITN, not lexically", {
  tbl <- data.frame(
    AVISIT = c("Week 10", "Week 2", "Baseline", "Week 10"),
    AVISITN = c(10, 2, 0, 10),
    stringsAsFactors = FALSE
  )
  expect_identical(pp_visit_levels(tbl), c("Baseline", "Week 2", "Week 10"))

  # without AVISITN the lexical order is all there is
  expect_identical(
    pp_visit_levels(tbl[, "AVISIT", drop = FALSE]),
    c("Baseline", "Week 10", "Week 2")
  )
})
