# AE-gantt consumption of the board scale map (mechanism in blockr.theme).

test_that("pp_sev_scale_colors resolves against patient adae", {
  adae <- data.frame(
    USUBJID = "x", AEDECOD = c("HEADACHE", "NAUSEA"),
    ASTDT = as.Date("2020-01-01") + 0:1,
    AESEV = c("MILD", "SEVERE")
  )
  dm_obj <- dm::dm(adae = adae)

  map <- blockr.theme::new_scale_map(
    blockr.theme::scale_binding(
      "AESEV",
      color = c(MILD = "#CA8A04", MODERATE = "#D97706", SEVERE = "#DC2626")
    )
  )

  cols <- blockr.pharma:::pp_sev_scale_colors(map, dm_obj)
  expect_identical(cols[["SEVERE"]], "#DC2626")
  expect_identical(cols[["MILD"]], "#CA8A04")

  expect_null(blockr.pharma:::pp_sev_scale_colors(NULL, dm_obj))
})
