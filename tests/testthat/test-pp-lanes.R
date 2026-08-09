# Lane granularity: the gantts group their rows by whichever coding level
# the study carries and the user picked (pp-lanes.R).

lanes_ae_dm <- function(adae_extra = list()) {
  adae <- data.frame(
    USUBJID = "x",
    AETERM = c("bad headache", "sore stomach", "worse headache"),
    AEDECOD = c("HEADACHE", "ABDOMINAL PAIN", "MIGRAINE"),
    AEBODSYS = c(
      "NERVOUS SYSTEM DISORDERS",
      "GASTROINTESTINAL DISORDERS",
      "NERVOUS SYSTEM DISORDERS"
    ),
    AESEV = c("MILD", "MODERATE", "MILD"),
    ASTDT = as.Date(c("2020-02-01", "2020-03-01", "2020-04-01")),
    AENDT = as.Date(c("2020-02-05", "2020-03-10", NA)),
    stringsAsFactors = FALSE
  )
  for (nm in names(adae_extra)) adae[[nm]] <- adae_extra[[nm]]
  dm::dm(
    adsl = data.frame(USUBJID = "x", TRTSDT = as.Date("2020-01-01"),
                      stringsAsFactors = FALSE),
    adae = adae
  )
}

lanes_cm_dm <- function(adcm_extra = list()) {
  adcm <- data.frame(
    USUBJID = "x",
    CMTRT = c("ASPIRIN TAB", "PARACETAMOL", "IBUPROFEN"),
    CMDECOD = c("ASPIRIN", "", "IBUPROFEN"),
    CMCLAS = c("ANALGESICS", "ANALGESICS", "ANALGESICS"),
    ASTDT = as.Date(c("2020-02-01", "2020-03-01", "2020-04-01")),
    AENDT = as.Date(c("2020-02-20", NA, "2020-04-20")),
    stringsAsFactors = FALSE
  )
  for (nm in names(adcm_extra)) adcm[[nm]] <- adcm_extra[[nm]]
  dm::dm(
    adsl = data.frame(USUBJID = "x", TRTSDT = as.Date("2020-01-01"),
                      stringsAsFactors = FALSE),
    adcm = adcm
  )
}

tr <- as.Date(c("2020-01-01", "2020-06-01"))
ae_lanes <- function(chart) unlist(chart$x$opts$yAxis$data)
bar_values <- function(chart, idx) {
  vapply(chart$x$opts$series[[1]]$data,
         function(b) as.character(b$value[[idx]]), "")
}

test_that("a level the data does not carry is not a choice", {
  # The full ladder is declared; what is offered is what the study has.
  dm_obj <- lanes_ae_dm()
  cols <- pp_filled_columns(dm_obj, "adae")
  expect_true(all(c("AETERM", "AEDECOD", "AEBODSYS") %in% cols))
  expect_false("AEHLT" %in% cols)

  # A column that exists but carries nothing groups nothing.
  blank <- lanes_ae_dm(list(AEHLT = c("", NA, "  ")))
  expect_false("AEHLT" %in% pp_filled_columns(blank, "adae"))
})

test_that("pp_lane_column prefers the request, then the default", {
  tbl <- as.data.frame(dm::dm_get_tables(lanes_ae_dm())[["adae"]])
  expect_equal(
    pp_lane_column(tbl, PP_AE_LANES, "AEBODSYS", "AEDECOD"), "AEBODSYS"
  )
  expect_equal(pp_lane_column(tbl, PP_AE_LANES, NULL, "AEDECOD"), "AEDECOD")
  # A saved board naming a level this study lacks falls back, never errors.
  expect_equal(
    pp_lane_column(tbl, PP_AE_LANES, "AEHLT", "AEDECOD"), "AEDECOD"
  )
  # No rung at all (the ladder's columns are simply absent).
  expect_null(pp_lane_column(tbl[, "USUBJID", drop = FALSE], PP_AE_LANES))
})

test_that("blank levels fall back per row, then say so", {
  tbl <- data.frame(
    AEDECOD = c("HEADACHE", "MIGRAINE", ""),
    AEBODSYS = c("NERVOUS SYSTEM DISORDERS", NA, ""),
    stringsAsFactors = FALSE
  )
  expect_equal(
    pp_lane_values(tbl, "AEBODSYS", "AEDECOD"),
    c("NERVOUS SYSTEM DISORDERS", "MIGRAINE", PP_LANE_UNCODED)
  )
})

test_that("the AE gantt groups by the level the settings name", {
  by_pt <- ae_gantt_viz$render(lanes_ae_dm(), tr,
                               settings = list(roles = list(severity = "AESEV")))
  expect_setequal(ae_lanes(by_pt),
                  c("HEADACHE", "ABDOMINAL PAIN", "MIGRAINE"))

  by_soc <- ae_gantt_viz$render(
    lanes_ae_dm(), tr,
    settings = list(roles = list(severity = "AESEV"), lanes = "AEBODSYS")
  )
  expect_setequal(ae_lanes(by_soc), c("NERVOUS SYSTEM DISORDERS",
                                      "GASTROINTESTINAL DISORDERS"))
  # Two preferred terms now share one lane, so one lane label is written.
  labels <- bar_values(by_soc, 12)
  expect_equal(sum(nzchar(labels)), 2L)
  # The bar is still its own event: the tooltip names the preferred term at
  # every lane setting.
  expect_setequal(bar_values(by_soc, 4),
                  c("HEADACHE", "ABDOMINAL PAIN", "MIGRAINE"))

  by_term <- ae_gantt_viz$render(
    lanes_ae_dm(), tr,
    settings = list(roles = list(severity = "AESEV"), lanes = "AETERM")
  )
  expect_setequal(ae_lanes(by_term),
                  c("bad headache", "sore stomach", "worse headache"))
})

test_that("the CM gantt groups by the level the settings name", {
  by_med <- cm_gantt_viz$render(lanes_cm_dm(), tr)
  # PARACETAMOL's blank CMDECOD still falls back to its reported name.
  expect_setequal(ae_lanes(by_med),
                  c("ASPIRIN", "PARACETAMOL", "IBUPROFEN"))

  by_class <- cm_gantt_viz$render(lanes_cm_dm(), tr,
                                  settings = list(lanes = "CMCLAS"))
  expect_equal(ae_lanes(by_class), "ANALGESICS")
  expect_setequal(bar_values(by_class, 4),
                  c("ASPIRIN", "PARACETAMOL", "IBUPROFEN"))

  # A class column present for some rows only: the rest keep their own lane.
  partial <- lanes_cm_dm(list(CMCLAS = c("ANALGESICS", "", NA)))
  by_partial <- cm_gantt_viz$render(partial, tr,
                                    settings = list(lanes = "CMCLAS"))
  expect_setequal(ae_lanes(by_partial),
                  c("ANALGESICS", "PARACETAMOL", "IBUPROFEN"))
})

test_that("the printed twins group the way the screen does", {
  skip_if_not_installed("ggplot2")
  p <- pp_static_ae_gantt(
    lanes_ae_dm(), tr,
    settings = list(roles = list(severity = "AESEV"), lanes = "AEBODSYS")
  )
  expect_equal(length(unique(p$data$lane)), 2L)
  expect_setequal(p$data$label[nzchar(p$data$label)],
                  c("Nervous system disorders", "Gastrointestinal disorders"))

  p_cm <- pp_static_cm_gantt(lanes_cm_dm(), tr,
                             settings = list(lanes = "CMCLAS"))
  expect_equal(length(unique(p_cm$data$lane)), 1L)
})

test_that("both gantts declare the lane control", {
  # The house click-through pill: four rungs of prose do not fit a
  # panel header that also carries the title, the legend and the actions.
  expect_equal(ae_gantt_viz$controls$lanes$type, "pill")
  expect_equal(cm_gantt_viz$controls$lanes$type, "pill")
  expect_equal(ae_gantt_viz$controls$lanes$default, "AEDECOD")
  expect_true(isTRUE(ae_gantt_viz$controls$lanes$choices_present))
  expect_equal(cm_gantt_viz$controls$lanes$default, "CMDECOD")
  expect_true(isTRUE(cm_gantt_viz$controls$lanes$choices_present))
  # The control ships a default, so the viz's stored settings carry it.
  expect_equal(pp_viz_defaults(ae_gantt_viz)$lanes, "AEDECOD")
})
