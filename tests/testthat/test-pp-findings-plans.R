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

  expect_true("adlb_trig" %in% names(vizs))
  # ...but no duplicate card for ALT, which the adlbc plan already covers
  expect_false("adlb_alt" %in% names(vizs))
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
