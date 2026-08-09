# The static exhibit path: pp_patient_exhibit(), the per-viz ggplot twins,
# the blockr.viz method seam and the report_call emission.

skip_if_not_installed("ggplot2")

pp_ex_dm <- function() {
  adsl <- data.frame(
    USUBJID = "S1",
    TRTSDT = as.Date("2024-01-01"),
    TRTEDT = as.Date("2024-03-01"),
    RFENDT = as.Date("2024-03-15"),
    ACTARM = "Drug A 10mg",
    AGE = 63L,
    AGEU = "YEARS",
    SEX = "F",
    RACE = "WHITE",
    COUNTRY = "USA",
    BMIBL = 24.35,
    stringsAsFactors = FALSE
  )
  adae <- data.frame(
    USUBJID = "S1",
    AEDECOD = c("HEADACHE", "NAUSEA"),
    ASTDT = as.Date(c("2024-01-10", "2024-02-01")),
    AENDT = as.Date(c("2024-01-20", NA)),
    AESEV = c("MILD", "SEVERE"),
    AESER = c("N", "Y"),
    stringsAsFactors = FALSE
  )
  adcm <- data.frame(
    USUBJID = "S1",
    CMTRT = "ASPIRIN",
    ASTDT = as.Date("2024-01-05"),
    AENDT = as.Date("2024-02-20"),
    stringsAsFactors = FALSE
  )
  adlb <- data.frame(
    USUBJID = "S1",
    PARAMCD = rep(c("ALT", "AST"), each = 3L),
    PARAM = rep(c("Alanine Aminotransferase (U/L)",
                  "Aspartate Aminotransferase (U/L)"), each = 3L),
    AVAL = c(30, 45, 28, 22, 25, 24),
    ADT = rep(as.Date(c("2024-01-02", "2024-01-20", "2024-02-15")), 2L),
    ANRIND = c("N", "H", "N", "N", "N", "N"),
    A1LO = rep(c(7, 10), each = 3L),
    A1HI = rep(c(40, 37), each = 3L),
    stringsAsFactors = FALSE
  )
  dm::dm(adsl = adsl, adae = adae, adcm = adcm, adlb = adlb)
}

# A two-patient cohort sharing the S1 fixture's shape.
pp_ex_cohort <- function() {
  adsl <- data.frame(
    USUBJID = c("S1", "S2"),
    TRTSDT = as.Date("2024-01-01"),
    TRTEDT = as.Date("2024-03-01"),
    ACTARM = c("Drug A", "Placebo"),
    stringsAsFactors = FALSE
  )
  adae <- data.frame(
    USUBJID = c("S1", "S2"),
    AEDECOD = c("HEADACHE", "RASH"),
    ASTDT = as.Date("2024-01-10"),
    AENDT = as.Date("2024-01-20"),
    AESEV = "MILD",
    stringsAsFactors = FALSE
  )
  dm::dm(adsl = adsl, adae = adae)
}

test_that("pp_patient_exhibit renders selected vizs as ggplots", {
  ex <- suppressMessages(pp_patient_exhibit(
    pp_ex_dm(),
    selected = c("patient_overview", "ae_gantt", "cm_gantt", "adlb_all")
  ))
  expect_s3_class(ex, "pp_exhibit")
  expect_s3_class(ex, "blockr_exhibit")
  expect_length(ex$patients, 1L)
  patient <- ex$patients[["S1"]]
  expect_identical(patient$subject, "S1")
  expect_named(patient$plots,
               c("patient_overview", "ae_gantt", "cm_gantt", "adlb_all"))
  for (p in patient$plots) {
    expect_s3_class(p, "ggplot")
    # The size contract blockr.viz's gg methods read.
    expect_true(is.numeric(attr(p, "pptx_width")))
    expect_true(is.numeric(attr(p, "pptx_height")))
  }
  expect_identical(unname(patient$labels[["ae_gantt"]]), "Adverse Events")
})

test_that("pp_patient_exhibit works in both timeline modes", {
  for (mode in c("rday", "date")) {
    ex <- suppressMessages(pp_patient_exhibit(
      pp_ex_dm(), selected = "ae_gantt", timeline_mode = mode
    ))
    expect_s3_class(ex$patients[["S1"]]$plots[["ae_gantt"]], "ggplot")
  }
})

test_that("viz_settings narrow the findings twin", {
  ex <- suppressMessages(pp_patient_exhibit(
    pp_ex_dm(), selected = "adlb_all",
    viz_settings = list(adlb_all = list(items = "AST"))
  ))
  p <- ex$patients[["S1"]]$plots[["adlb_all"]]
  # One facet per selected parameter: only AST made it into the panel data.
  expect_true(all(grepl("^AST", as.character(p$data$..panel))))
})

test_that("a cohort renders one patient group per subject", {
  ex <- suppressMessages(pp_patient_exhibit(pp_ex_cohort(),
                                            selected = "ae_gantt"))
  expect_named(ex$patients, c("S1", "S2"))
  for (patient in ex$patients) {
    expect_named(patient$plots, "ae_gantt")
  }
})

test_that("the cohort cap truncates loudly", {
  expect_message(
    ex <- pp_patient_exhibit(pp_ex_cohort(), selected = "ae_gantt",
                             max_subjects = 1L),
    "rendering the first 1"
  )
  expect_named(ex$patients, "S1")
})

test_that("a multi-subject dm scopes through the subject argument", {
  ex <- suppressMessages(pp_patient_exhibit(
    pp_ex_cohort(), selected = "ae_gantt", subject = "S2"
  ))
  expect_named(ex$patients, "S2")

  expect_null(suppressMessages(pp_patient_exhibit(
    pp_ex_cohort(), selected = "ae_gantt", subject = "nope"
  )))
})

test_that("pp_patient_exhibit is total on things it cannot render", {
  expect_null(suppressMessages(pp_patient_exhibit(data.frame(x = 1))))

  # Unknown and twin-less vizs are skipped, not fatal.
  ex <- suppressMessages(pp_patient_exhibit(
    pp_ex_dm(), selected = c("nope", "ae_gantt")
  ))
  expect_named(ex$patients[["S1"]]$plots, "ae_gantt")

  # Nothing renderable at all -> NULL.
  expect_null(suppressMessages(
    pp_patient_exhibit(pp_ex_dm(), selected = "nope")
  ))
})

test_that("the exhibit settings assembly matches the block's injection", {
  dm_obj <- pp_normalize_dm(pp_ex_dm())
  roles <- pp_resolve_roles(dm_obj)
  viz <- patient_profile_static_vizs()[["ae_gantt"]]
  s <- pp_viz_exhibit_settings(viz, list(foo = 1), roles, dm_obj,
                               smooth = "off")
  expect_identical(s$foo, 1)
  expect_identical(s$roles$severity, roles$severity)
  expect_identical(s$smooth, "off")
})

test_that("pptx_add_exhibit.pp_exhibit adds one slide per visual", {
  skip_if_not_installed("officer")
  skip_if_not_installed("blockr.viz")
  ex <- suppressMessages(pp_patient_exhibit(
    pp_ex_dm(), selected = c("patient_overview", "ae_gantt")
  ))
  doc <- officer::read_pptx()
  n0 <- length(doc)
  doc <- blockr.viz::pptx_add_exhibit(doc, ex, title = "Patient Profile")
  expect_identical(length(doc) - n0, 2L)
})

test_that("a cohort deck gets patient-per-slide titles", {
  skip_if_not_installed("officer")
  skip_if_not_installed("blockr.viz")
  ex <- suppressMessages(pp_patient_exhibit(pp_ex_cohort(),
                                            selected = "ae_gantt"))
  doc <- officer::read_pptx()
  doc <- blockr.viz::pptx_add_exhibit(doc, ex, title = "Profile")
  expect_identical(length(doc), 2L)
  titles <- vapply(seq_len(length(doc)), function(i) {
    s <- officer::slide_summary(doc, i)
    paste(s$text[!is.na(s$text)], collapse = " ")
  }, character(1L))
  expect_match(titles[1], "Patient S1: Adverse Events")
  expect_match(titles[2], "Patient S2: Adverse Events")
})

test_that("html_exhibit.pp_exhibit returns labelled image sections", {
  skip_if_not_installed("blockr.viz")
  ex <- suppressMessages(pp_patient_exhibit(pp_ex_dm(),
                                            selected = "ae_gantt"))
  html <- blockr.viz::html_exhibit(ex)
  txt <- paste(as.character(htmltools::tagList(html)), collapse = "")
  expect_true(grepl("Adverse Events", txt))
  expect_true(grepl("blockr-exhibit-img", txt))

  # Multi-patient output carries the subject headings.
  ex2 <- suppressMessages(pp_patient_exhibit(pp_ex_cohort(),
                                             selected = "ae_gantt"))
  txt2 <- paste(
    as.character(htmltools::tagList(blockr.viz::html_exhibit(ex2))),
    collapse = ""
  )
  expect_true(grepl("Patient S1", txt2))
  expect_true(grepl("Patient S2", txt2))
})

test_that("report_call emits a literal self-qualified exhibit call", {
  skip_if_not_installed("blockr.viz")
  blk <- new_patient_profile_block(
    selected = c("patient_overview", "ae_gantt"),
    viz_settings = list(
      ae_gantt = list(),                    # empty: must be dropped
      adlb_all = list(items = "ALT"),       # unselected: must be dropped
      patient_overview = list(keep = TRUE)  # selected + non-empty: kept
    ),
    subject = "S1",
    timeline_mode = "date",
    smooth = "off"
  )
  cl <- blockr.viz::report_call(blk, "res1")
  expect_true(is.call(cl))
  code <- paste(deparse(cl), collapse = " ")
  expect_match(code, "blockr.pharma::pp_patient_exhibit\\(res1")
  expect_match(code, 'timeline_mode = "date"')
  expect_match(code, 'smooth = "off"')
  # The pick is NOT emitted: a document renders the cohort (recovered from
  # the result's pp_cohort attribute), the block's true view.
  expect_false(grepl("subject", code))
  expect_false(grepl("adlb_all", code))
  expect_false(grepl("ae_gantt = list\\(\\)", code))

  # Defaults are dropped entirely.
  cl0 <- blockr.viz::report_call(new_patient_profile_block(), "res1")
  expect_identical(paste(deparse(cl0), collapse = ""),
                   "blockr.pharma::pp_patient_exhibit(res1)")

  # The emitted call evaluates in a fresh env holding only the result --
  # exactly how blockr.outline's deck renders a slide.
  env <- new.env(parent = baseenv())
  assign("res1", pp_ex_dm(), envir = env)
  ex <- suppressMessages(eval(cl, envir = env))
  expect_s3_class(ex, "pp_exhibit")
  expect_named(ex$patients[["S1"]]$plots,
               c("patient_overview", "ae_gantt"))

  # A block with no picked subject passes the cohort through, and the deck
  # then renders every patient.
  env2 <- new.env(parent = baseenv())
  assign("res1", pp_ex_cohort(), envir = env2)
  ex2 <- suppressMessages(eval(cl0, envir = env2))
  expect_named(ex2$patients, c("S1", "S2"))
})

test_that("a deck over a PICKED patient still renders the cohort", {
  skip_if_not_installed("blockr.viz")
  # The full chain a deck evaluates: the block's exported filter
  # expression (pp_pick_subject: scoped result + cohort attribute), then
  # the emitted report call over the result variable.
  env <- new.env(parent = baseenv())
  assign("data", pp_ex_cohort(), envir = env)
  filter_expr <- pp_subject_filter_expr("adsl", "S1")
  assign("res1", eval(filter_expr, envir = env), envir = env)

  # Downstream data contract holds: the result itself is one patient.
  expect_identical(pp_subject_ids(get("res1", env)), "S1")

  cl <- blockr.viz::report_call(
    new_patient_profile_block(selected = "ae_gantt", subject = "S1"),
    "res1"
  )
  ex <- suppressMessages(eval(cl, envir = env))
  expect_named(ex$patients, c("S1", "S2"))
})

test_that("write_exhibit_png writes a real file for a viz twin", {
  skip_if_not_installed("blockr.viz")
  skip_if_not_installed("ragg")
  ex <- suppressMessages(pp_patient_exhibit(pp_ex_dm(),
                                            selected = "ae_gantt"))
  f <- withr::local_tempfile(fileext = ".png")
  blockr.viz::write_exhibit_png(ex$patients[["S1"]]$plots[["ae_gantt"]], f)
  expect_true(file.exists(f))
  expect_gt(file.size(f), 1000)
})

test_that("the profile writes whole-block pptx and html files", {
  skip_if_not_installed("officer")
  skip_if_not_installed("blockr.viz")
  ex <- suppressMessages(pp_patient_exhibit(
    pp_ex_dm(), selected = c("patient_overview", "ae_gantt")
  ))
  f_pptx <- withr::local_tempfile(fileext = ".pptx")
  blockr.viz::write_exhibit_pptx(ex, f_pptx)
  expect_identical(length(officer::read_pptx(f_pptx)), 2L)

  f_html <- withr::local_tempfile(fileext = ".html")
  blockr.viz::write_exhibit_html(ex, f_html, title = "Patient S1")
  expect_true(file.size(f_html) > 10000)
  expect_true(grepl("blockr-exhibit-img",
                    paste(readLines(f_html, warn = FALSE), collapse = "")))
})

test_that("pp_patient_info_fields reads the subject's facts", {
  dm_obj <- pp_normalize_dm(pp_ex_dm())
  roles <- pp_resolve_roles(dm_obj)
  info <- pp_patient_info_fields(dm_obj, list(roles = roles))
  expect_identical(colnames(info), c("Field", "Value"))
  get <- function(f) info$Value[info$Field == f]
  expect_identical(get("Subject"), "S1")
  expect_identical(get("Age"), "63 years")
  expect_identical(get("Sex"), "F")
  expect_identical(get("Race"), "White")
  expect_identical(get("Treatment arm"), "Drug A 10mg")
  expect_match(get("Treatment period"), "2024-01-01 → 2024-03-01")
  expect_match(get("Treatment period"), "61 days")
  expect_identical(get("End of study"), "2024-03-15")
  expect_identical(get("BMI (baseline)"), "24.4")
  # Fields the study did not collect simply are not rows.
  expect_false("Ethnicity" %in% info$Field)
  expect_false("Death" %in% info$Field)
})

test_that("patient_info exports as a table exhibit", {
  skip_if_not_installed("officer")
  skip_if_not_installed("blockr.viz")
  ex <- suppressMessages(pp_patient_exhibit(
    pp_ex_dm(), selected = c("patient_info", "ae_gantt")
  ))
  patient <- ex$patients[["S1"]]
  expect_true(is.data.frame(patient$plots[["patient_info"]]))
  expect_identical(unname(patient$labels[["patient_info"]]),
                   "Patient Info")

  # The container method routes the data frame through blockr.viz's
  # DEFAULT pptx method: a native table slide, beside the plot slide.
  # blockr.viz builds that table with flextable, which it only suggests.
  skip_if_not_installed("flextable")
  doc <- officer::read_pptx()
  doc <- blockr.viz::pptx_add_exhibit(doc, ex, title = "Profile")
  expect_identical(length(doc), 2L)

  # The table-kind writers the per-viz download menu offers.
  skip_if_not_installed("openxlsx")
  f_xlsx <- withr::local_tempfile(fileext = ".xlsx")
  blockr.viz::write_annotated_xlsx(patient$plots[["patient_info"]], f_xlsx)
  expect_gt(file.size(f_xlsx), 1000)
  f_html <- withr::local_tempfile(fileext = ".html")
  blockr.viz::write_exhibit_html(patient$plots[["patient_info"]], f_html,
                                 title = "Patient S1")
  expect_true(grepl("Treatment arm",
                    paste(readLines(f_html, warn = FALSE),
                          collapse = "")))
})

test_that("patient_info is the first sidebar card and renders live", {
  vizs <- patient_profile_static_vizs()
  expect_identical(names(vizs)[1L], "patient_info")
  expect_identical(vizs[["patient_info"]]$exhibit_kind, "table")
  expect_identical(vizs[["ae_gantt"]]$exhibit_kind, "plot")

  dm_obj <- pp_normalize_dm(pp_ex_dm())
  roles <- pp_resolve_roles(dm_obj)
  tag <- vizs[["patient_info"]]$render(
    dm_obj, NULL, list(roles = roles), NA_real_, "date"
  )
  html <- as.character(htmltools::doRenderTags(tag))
  expect_match(html, "pp-info-table")
  expect_match(html, "Drug A 10mg")
})

test_that("vizs without a static twin have no exhibit field set", {
  vizs <- patient_profile_static_vizs()
  expect_null(vizs[["npix_radar"]]$exhibit)
  expect_true(is.function(vizs[["patient_overview"]]$exhibit))
  expect_true(is.function(vizs[["ae_gantt"]]$exhibit))
  expect_true(is.function(vizs[["cm_gantt"]]$exhibit))
  expect_true(is.function(vizs[["adas_trajectory"]]$exhibit))
  expect_true(is.function(vizs[["ortho_bp"]]$exhibit))
  expect_true(is.function(vizs[["questionnaire_heatmap"]]$exhibit))
})
