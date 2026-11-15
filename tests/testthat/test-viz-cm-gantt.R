# Concomitant medications gantt: a declaration plus a render, no exceptions.
# Works against ADaM adcm and (via pp_normalize_dm()) an SDTM cm domain --
# the SDTM path is covered end-to-end in test-pp-sdtm.R.

cm_dm <- function(cm_extra = list()) {
  adcm <- data.frame(
    USUBJID = "x",
    CMTRT = c("ASPIRIN TAB", "PARACETAMOL"),
    CMDECOD = c("ASPIRIN", ""),
    ASTDT = as.Date(c("2020-02-01", "2020-03-01")),
    AENDT = as.Date(c("2020-02-20", NA)),
    CMDOSE = c(100, 500), CMDOSU = c("mg", "mg"),
    CMROUTE = c("ORAL", "ORAL"),
    CMINDC = c("PROPHYLAXIS", "HEADACHE"),
    stringsAsFactors = FALSE
  )
  for (nm in names(cm_extra)) adcm[[nm]] <- cm_extra[[nm]]
  dm::dm(
    adsl = data.frame(USUBJID = "x", TRTSDT = as.Date("2020-01-01"),
                      TRTEDT = as.Date("2020-06-01"),
                      stringsAsFactors = FALSE),
    adcm = adcm
  )
}

test_that("cm_gantt is declared against adcm and resolves", {
  dm_obj <- cm_dm()
  expect_true("cm_gantt" %in% names(patient_profile_static_vizs()))
  res <- pp_resolve_requires(dm_obj, cm_gantt_viz)
  expect_true(isTRUE(res$ok))
})

test_that("lanes prefer the coded name and fall back per row", {
  chart <- cm_gantt_viz$render(
    cm_dm(), time_range = as.Date(c("2020-01-01", "2020-06-01")),
    # The indication is a role: the tooltip reads the column the block
    # resolved, never a name detected in here.
    settings = list(roles = list(indication = "CMINDC"))
  )
  lanes <- unlist(chart$x$opts$yAxis$data)
  # ASPIRIN from CMDECOD; PARACETAMOL's blank CMDECOD falls back to CMTRT
  expect_setequal(lanes, c("ASPIRIN", "PARACETAMOL"))

  bars <- chart$x$opts$series[[1]]$data
  tips <- vapply(bars, function(b) paste(unlist(b$value), collapse = " "), "")
  expect_true(any(grepl("100 mg", tips)))
  expect_true(any(grepl("PROPHYLAXIS", tips)))
})

test_that("bars are coloured as the patient timeline colours con meds", {
  tr <- as.Date(c("2019-10-01", "2020-06-01"))
  bar_cols <- function(chart) {
    data <- chart$x$opts$series[[1]]$data
    stats::setNames(
      vapply(data, function(b) b$itemStyle$color, ""),
      vapply(data, function(b) as.character(b$value[[4]]), "")
    )
  }
  before <- PP_CM_STARTED[["Started before treatment"]]
  on <- PP_CM_STARTED[["Started on treatment"]]

  # Started before or on the first dose (ADSL TRTSDT), whatever indication
  # colours the board resolved: they no longer colour the bars.
  dm_obj <- cm_dm(list(ASTDT = as.Date(c("2019-11-01", "2020-03-01"))))
  chart <- cm_gantt_viz$render(
    dm_obj, time_range = tr,
    settings = list(roles = list(indication = "CMINDC"),
                    indc_colors = c(PROPHYLAXIS = "#111111"))
  )
  expect_identical(bar_cols(chart)[["ASPIRIN"]], before)
  expect_identical(bar_cols(chart)[["PARACETAMOL"]], on)

  # The study day decides where the record carries one.
  dm_obj <- cm_dm(list(ASTDY = c(5L, -3L)))
  chart <- cm_gantt_viz$render(dm_obj, time_range = tr, mode = "rday",
                               ref_ms = pp_xval(as.Date("2020-01-01")))
  expect_identical(bar_cols(chart)[["ASPIRIN"]], on)
  expect_identical(bar_cols(chart)[["PARACETAMOL"]], before)
})

test_that("the legend names the two colours the patient's records carry", {
  html <- as.character(cm_gantt_viz$legend_ui(cm_dm(), list()))
  expect_match(html, "Started on treatment")
  expect_no_match(html, "Started before treatment")
  html <- as.character(cm_gantt_viz$legend_ui(
    cm_dm(list(ASTDT = as.Date(c("2019-11-01", "2020-03-01")))), list()
  ))
  expect_match(html, "Started before treatment")
  expect_match(html, PP_CM_STARTED[["Started before treatment"]],
               fixed = TRUE)
})

test_that("an uncoded con med takes a lane of its reported name", {
  chart <- cm_gantt_viz$render(
    cm_dm(list(CMDECOD = c("ASPIRIN", "UNCODED"))),
    time_range = as.Date(c("2020-01-01", "2020-06-01"))
  )
  expect_identical(unlist(chart$x$opts$yAxis$data),
                   c("ASPIRIN", "PARACETAMOL"))
})

test_that("lanes come in order of first onset", {
  chart <- cm_gantt_viz$render(
    cm_dm(list(CMDECOD = c("ZINC", "ASPIRIN"),
               ASTDT = as.Date(c("2020-02-01", "2020-03-01")))),
    time_range = as.Date(c("2020-01-01", "2020-06-01"))
  )
  expect_identical(unlist(chart$x$opts$yAxis$data), c("ZINC", "ASPIRIN"))
})

test_that("the medication period feeds the shared time range", {
  # A med taken outside the treatment window must widen the axis, exactly
  # like an AE would.
  dm_obj <- cm_dm(list(AENDT = as.Date(c("2020-02-20", "2020-09-15"))))
  tr <- pp_compute_time_range(dm_obj)
  expect_gte(as.numeric(tr[2]), as.numeric(as.Date("2020-09-15")))
})

test_that("date mode without dates asks for relative day", {
  adcm <- data.frame(
    USUBJID = "x", CMTRT = "ASPIRIN", ASTDY = 10L, AENDY = 20L,
    stringsAsFactors = FALSE
  )
  dm_obj <- dm::dm(adcm = adcm)
  expect_true(isTRUE(pp_resolve_requires(dm_obj, cm_gantt_viz)$ok))

  chart <- cm_gantt_viz$render(dm_obj, time_range = NULL, mode = "date")
  expect_match(paste(unlist(chart$x$opts$title), collapse = " "),
               "relative day")

  chart <- cm_gantt_viz$render(dm_obj, time_range = NULL,
                               ref_ms = NA_real_, mode = "rday")
  starts <- vapply(chart$x$opts$series[[1]]$data,
                   function(d) as.numeric(d$value[[1]]), numeric(1))
  expect_equal(starts, 10)
})

test_that("the find control narrows the panel to the matching medications", {
  tr <- as.Date(c("2020-01-01", "2020-06-01"))
  dm_obj <- cm_dm(list(CMCLAS = c("ANALGESICS", "ANALGESICS")))

  lanes <- function(chart) unlist(chart$x$opts$yAxis$data)
  find <- function(picks) {
    lanes(cm_gantt_viz$render(dm_obj, tr, settings = list(find = picks)))
  }
  typed <- function(v) list(list(col = "*", value = v))
  # A typed pick, by name, coded or verbatim -- the box this control replaced.
  expect_identical(find(typed("aspirin")), "ASPIRIN")
  # By class: both rows are analgesics, and neither name says so.
  expect_setequal(find(typed("analg")), c("ASPIRIN", "PARACETAMOL"))
  # A picked class, matched on its own column rather than by luck.
  expect_setequal(find(list(list(col = "CMCLAS", value = "ANALGESICS"))),
                  c("ASPIRIN", "PARACETAMOL"))
  # Two picks are an OR. One term never could be, which is the whole reason
  # the box became a set. The second row of this fixture has no coded name,
  # so its pick comes off the verbatim column -- picks carry the column they
  # were offered from, and mixing levels in one filter is the normal case.
  expect_setequal(
    find(list(list(col = "CMDECOD", value = "ASPIRIN"),
              list(col = "CMTRT", value = "PARACETAMOL"))),
    c("ASPIRIN", "PARACETAMOL")
  )
  # A pick naming a column the study does not carry matches nothing, rather
  # than being quietly ignored: it is a specific question about a column that
  # has gone, and the empty state says so and offers a way out.
  expect_match(
    paste(unlist(cm_gantt_viz$render(dm_obj, tr, settings = list(
      find = list(list(col = "AEBODSYS", value = "ANYTHING"))
    ))$x$opts), collapse = " "),
    "None of this patient's", fixed = TRUE
  )
  # Nothing matching says so, rather than drawing an empty axis, and it names
  # what it was asked for -- the picks outlive a patient switch, so this is a
  # state a reviewer has to be able to get out of.
  none <- cm_gantt_viz$render(dm_obj, tr,
                              settings = list(find = typed("insulin")))
  txt <- paste(unlist(none$x$opts), collapse = " ")
  expect_match(txt, "None of this patient's 2 medications match", fixed = TRUE)
  expect_match(txt, "insulin", fixed = TRUE)
  # No picks is everything, and so is a filter saved as an empty list.
  expect_length(find(list()), 2L)
  expect_length(lanes(cm_gantt_viz$render(dm_obj, tr, settings = list())), 2L)
  # A board saved before the picker restores as one typed pick.
  expect_identical(
    lanes(cm_gantt_viz$render(
      dm_obj, tr,
      settings = pp_migrate_viz_settings(
        list(cm = list(search = "aspirin"))
      )$cm
    )),
    "ASPIRIN"
  )
})

test_that("the printed twin is filtered like the panel", {
  skip_if_not_installed("ggplot2")
  tr <- as.Date(c("2020-01-01", "2020-06-01"))
  gone <- list(list(col = "*", value = "insulin"))
  here <- list(list(col = "CMDECOD", value = "ASPIRIN"))
  expect_null(cm_gantt_viz$exhibit(cm_dm(), tr, settings = list(find = gone)))
  expect_s3_class(
    cm_gantt_viz$exhibit(cm_dm(), tr, settings = list(find = here)),
    "ggplot"
  )
})

test_that("the panel and the cohort band filter the same records", {
  dm_obj <- cm_dm()
  adcm <- as.data.frame(dm::dm_get_tables(dm_obj)$adcm)
  picks <- list(list(col = "CMDECOD", value = "ASPIRIN"))
  panel_n <- sum(pp_find_match(adcm, picks, PP_CM_SEARCH))
  band_n <- nrow(
    pp_cohort_marks(dm_obj, pp_resolve_roles(dm_obj), band = cm_gantt_viz$band,
                    picks = picks)$subjects[["x"]]$events
  )
  expect_identical(band_n, panel_n)
  expect_identical(band_n, 1L)
})

test_that("a chart of one lane still sends its axis as an array", {
  # htmlwidgets unboxes a length-one vector to a bare string, and ECharts
  # reads a string's characters as the categories: eighteen hair-thin lanes
  # for "LOSARTAN POTASSIUM", the bar on the first and its label cut off.
  tr <- as.Date(c("2020-01-01", "2020-06-01"))
  one <- cm_gantt_viz$render(cm_dm(), tr, settings = list(
    find = list(list(col = "CMDECOD", value = "ASPIRIN"))
  ))
  expect_identical(
    as.character(htmlwidgets:::toJSON(one$x$opts$yAxis$data)),
    "[\"ASPIRIN\"]"
  )
})

test_that("the header counts uncoded con meds by their reported names", {
  dm_obj <- cm_dm(list(CMDECOD = c("UNCODED", "UNCODED")))
  ctrl <- cm_gantt_viz$controls$find
  w <- pp_find_word(cm_gantt_viz, "cm_gantt", "find", ctrl, dm_obj, list())
  expect_identical(w$prefix, "2 medications")
})
