# Patient overview AE lane honors the injected board severity colors (same
# settings$sev_colors channel the block server feeds the AE gantt).

pp_overview_dm <- function(sev = c(AESEV = "SEVERE")) {
  adsl <- data.frame(
    USUBJID = "x",
    ACTARM = "Placebo",
    TRTSDT = as.Date("2020-01-01"),
    TRTEDT = as.Date("2020-06-01")
  )
  adae <- data.frame(
    USUBJID = "x", AEDECOD = "HEADACHE",
    ASTDT = as.Date("2020-02-01"), AENDT = as.Date("2020-02-10")
  )
  adae[[names(sev)]] <- unname(sev)
  dm::dm(adsl = adsl, adae = adae)
}

overview_js <- function(settings = list(), dm_obj = pp_overview_dm()) {
  chart <- blockr.pharma:::patient_overview_viz$render(
    dm_obj,
    time_range = blockr.pharma:::pp_compute_time_range(dm_obj),
    settings = settings
  )
  series <- chart$x$opts$series
  ae <- Filter(function(s) identical(s$name, "Adverse Events"), series)[[1]]
  paste(as.character(ae$renderItem), as.character(ae$tooltip$formatter))
}

test_that("overview uses board severity colors when injected", {
  js <- overview_js(settings = list(
    sev_colors = c(SEVERE = "#123456", MILD = "#654321")
  ))
  expect_match(js, "rgba(18,52,86,0.7)", fixed = TRUE)   # fill from #123456
  expect_match(js, "#123456", fixed = TRUE)              # tooltip hex
  expect_match(js, "#D97706", fixed = TRUE)              # MODERATE keeps default
})

test_that("overview falls back to built-in constants without a map", {
  js <- overview_js()
  expect_match(js, "rgba(220,38,38,0.7)", fixed = TRUE)  # SEVERE default
  expect_match(js, "#CA8A04", fixed = TRUE)              # MILD default
})

test_that("overview colors grade-coded severity (AETOXGR)", {
  dm_obj <- pp_overview_dm(sev = c(AETOXGR = "3"))
  js <- overview_js(dm_obj = dm_obj)
  expect_match(js, "rgba(196,145,2,0.7)", fixed = TRUE)  # grade 3 default fill
  expect_match(js, "'3': '#c49102'", fixed = TRUE)       # tooltip hex map

  # Injected board colors override by grade level, as for word levels.
  js <- overview_js(
    settings = list(sev_colors = c("3" = "#123456")),
    dm_obj = dm_obj
  )
  expect_match(js, "rgba(18,52,86,0.7)", fixed = TRUE)
})
