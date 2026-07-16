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
