# Severity column detection and coloring: studies code AE severity as a
# CTCAE grade (AETOXGR) or a word (AESEV); every severity consumer must
# agree on the column and the colors.

pp_sev_dm <- function(adae_extra) {
  adsl <- data.frame(
    USUBJID = "x",
    TRTSDT = as.Date("2020-01-01"),
    TRTEDT = as.Date("2020-06-01")
  )
  adae <- cbind(
    data.frame(
      USUBJID = "x",
      AEDECOD = paste0("AE", seq_len(nrow(adae_extra))),
      ASTDT = as.Date("2020-02-01"),
      AENDT = as.Date("2020-02-10")
    ),
    adae_extra
  )
  dm::dm(adsl = adsl, adae = adae)
}

test_that("pp_sev_column prefers the grade column", {
  expect_identical(
    blockr.pharma:::pp_sev_column(c("AESEV", "AETOXGR")),
    "AETOXGR"
  )
  expect_identical(
    blockr.pharma:::pp_sev_column(c("AEDECOD", "AESEV")),
    "AESEV"
  )
  expect_null(blockr.pharma:::pp_sev_column("AEDECOD"))
})

test_that("built-in constants cover grades and words", {
  expect_identical(blockr.pharma:::pp_sev_fallback_color("3"), "#c49102")
  expect_identical(blockr.pharma:::pp_sev_fallback_color(3), "#c49102")
  expect_identical(blockr.pharma:::pp_sev_fallback_color("Mild"), "#CA8A04")
  expect_identical(blockr.pharma:::pp_sev_fallback_color("???"), "#9ca3af")
})

test_that("pp_sev_label spells out grades", {
  expect_identical(blockr.pharma:::pp_sev_label("3"), "Grade 3")
  expect_identical(blockr.pharma:::pp_sev_label("SEVERE"), "Severe")
})

test_that("pp_sev_scale_colors resolves the detected column's binding", {
  skip_if_not_installed("blockr.theme")
  map <- blockr.theme::new_scale_map(
    blockr.theme::scale_binding(
      "AETOXGR",
      color = c("1" = "#111111", "3" = "#333333")
    ),
    blockr.theme::scale_binding("AESEV", color = c(MILD = "#aaaaaa"))
  )

  dm_obj <- pp_sev_dm(data.frame(AETOXGR = c(1, 3)))
  cols <- blockr.pharma:::pp_sev_scale_colors(map, dm_obj)
  expect_identical(cols[["1"]], "#111111")
  expect_identical(cols[["3"]], "#333333")

  # A map that only binds the word column does not answer for grades; the
  # vizs then fall back to the built-in grade constants.
  word_map <- blockr.theme::new_scale_map(
    blockr.theme::scale_binding("AESEV", color = c(MILD = "#aaaaaa"))
  )
  expect_null(blockr.pharma:::pp_sev_scale_colors(word_map, dm_obj))
})

test_that("AE gantt colors grade-coded severity without a map", {
  dm_obj <- pp_sev_dm(data.frame(AETOXGR = c(3, 5)))
  chart <- blockr.pharma:::ae_gantt_viz$render(
    dm_obj,
    time_range = blockr.pharma:::pp_compute_time_range(dm_obj)
  )
  bars <- chart$x$opts$series[[1]]$data
  cols <- vapply(bars, function(b) b$itemStyle$color, "")
  expect_setequal(cols, c("#c49102", "#FF0000"))
})

test_that("severity legend lists grades in order with Grade labels", {
  dm_obj <- pp_sev_dm(data.frame(AETOXGR = c(3, 1)))
  html <- as.character(blockr.pharma:::pp_sev_legend_ui(dm_obj))
  expect_match(html, "Grade 1")
  expect_match(html, "Grade 3")
  expect_lt(regexpr("Grade 1", html), regexpr("Grade 3", html))
  expect_match(html, "#43978D", fixed = TRUE)
  expect_match(html, "#c49102", fixed = TRUE)
})
