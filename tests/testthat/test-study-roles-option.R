# The "study_roles" board option: three fields (arm, severity, timeline),
# uniform semantics -- undeclared = the package convention,
# declared-but-missing = a named error, never a fallback. Table aliases are
# NOT a field: SDTM domain names resolve through pp_table_catalog() with no
# declaration, and a custom table name is an upstream rename (a block's
# job). The transform must be TOTAL over typed input -- it runs per
# keystroke inside an observer, where a stop() kills the session.

test_that("the option normalizes, serializes and restores", {
  opt <- new_study_roles_option()
  expect_s3_class(opt, "board_option")
  expect_identical(blockr.core::board_option_id(opt), "study_roles")
  # undeclared default
  expect_identical(
    blockr.core::board_option_value(opt),
    list(arm = "", severity = "", timeline = "")
  )

  # normalization: ctor keys, the editor's study_* input keys, whitespace
  expect_identical(
    blockr.core::board_option_value(opt, list(
      study_arm = " TRT ", study_severity = "", study_timeline = "RANDDT"
    )),
    list(arm = "TRT", severity = "", timeline = "RANDDT")
  )

  # TOTAL over anything a text field can hold: never an error mid-keystroke
  # (the transform runs per input change inside an observer; a stop() there
  # ends the whole session)
  expect_identical(
    blockr.core::board_option_value(opt, list(study_arm = NULL))$arm, ""
  )
  expect_identical(
    blockr.core::board_option_value(opt, list(study_arm = c("A", "B")))$arm,
    ""
  )

  # a declared value round-trips through ser/deser (the saved-board path)
  declared <- new_study_roles_option(arm = "TRT", severity = "AETOXGR")
  restored <- blockr.core::blockr_deser(blockr.core::blockr_ser(declared))
  expect_s3_class(restored, "board_option")
  val <- blockr.core::board_option_value(restored)
  expect_identical(val$arm, "TRT")
  expect_identical(val$severity, "AETOXGR")
})

test_that("a declared severity column wins, and errors when absent", {
  cols <- c("USUBJID", "AETOXGR", "AESEV", "SEVX")
  expect_identical(pp_sev_column(cols, "SEVX"), "SEVX")
  # A grade-coded study declares, and the declaration outranks the word
  # scale detection would otherwise pick.
  expect_identical(pp_sev_column(cols, "AETOXGR"), "AETOXGR")
  # undeclared: detection takes the word scale, the general default
  expect_identical(pp_sev_column(cols, NULL), "AESEV")
  # neither column, undeclared: legitimately no severity
  expect_null(pp_sev_column(c("USUBJID", "AEDECOD")))

  err <- tryCatch(pp_sev_column(cols, "NOPE"), error = function(e) e)
  expect_s3_class(err, "pp_sev_var_missing")
  expect_match(conditionMessage(err), "NOPE")
  expect_match(conditionMessage(err), "sidebar")
})

test_that("a declared timeline reference wins, and errors when absent", {
  cols <- c("USUBJID", "TRTSDT", "RANDDT")
  expect_identical(pp_timeline_column(cols, "RANDDT"), "RANDDT")
  expect_identical(pp_timeline_column(cols, NULL), "TRTSDT")
  # no TRTSDT, undeclared: relative-day mode is simply unavailable
  expect_null(pp_timeline_column(c("USUBJID", "AGE")))

  err <- tryCatch(pp_timeline_column(cols, "NOPE"), error = function(e) e)
  expect_s3_class(err, "pp_timeline_var_missing")
  expect_match(conditionMessage(err), "sidebar")
})

test_that("the timeline role anchors relative-day mode end to end", {
  adsl <- data.frame(
    USUBJID = "x",
    TRTSDT = as.Date("2020-02-01"),
    RANDDT = as.Date("2020-01-01"),
    stringsAsFactors = FALSE
  )
  dm_obj <- dm::dm(adsl = adsl)

  roles <- pp_resolve_roles(dm_obj, list(timeline = "RANDDT"))
  expect_identical(roles$timeline, "RANDDT")
  expect_identical(
    pp_compute_ref_ms(dm_obj, roles$timeline),
    pp_ms_ts(as.Date("2020-01-01"))
  )
  expect_true(pp_has_ref(dm_obj, roles$timeline))
  # and undeclared stays on the convention
  expect_identical(
    pp_compute_ref_ms(dm_obj, pp_resolve_roles(dm_obj)$timeline),
    pp_ms_ts(as.Date("2020-02-01"))
  )
})

test_that("pp_roles_blocker raises declared severity/timeline failures", {
  dm_obj <- dm::dm(
    adsl = data.frame(USUBJID = "x", ACTARM = "Placebo",
                      stringsAsFactors = FALSE),
    adae = data.frame(USUBJID = "x", AEDECOD = "HEADACHE", AESEV = "MILD",
                      stringsAsFactors = FALSE)
  )
  expect_null(pp_roles_blocker(dm_obj, list(severity = "AESEV")))

  blocker <- pp_roles_blocker(dm_obj, list(severity = "NOPE"))
  expect_error(eval(blocker, baseenv()), class = "pp_sev_var_missing")

  blocker <- pp_roles_blocker(dm_obj, list(timeline = "NOPE"))
  expect_error(eval(blocker, baseenv()), class = "pp_timeline_var_missing")
})

test_that("a declared severity flows through role resolution to consumers", {
  adae <- data.frame(
    USUBJID = "x", AEDECOD = "HEADACHE",
    ASTDT = as.Date("2020-02-01"),
    AETOXGR = "3", SEVX = "SEVERE",
    stringsAsFactors = FALSE
  )
  dm_obj <- dm::dm(
    adsl = data.frame(USUBJID = "x", ACTARM = "Placebo",
                      stringsAsFactors = FALSE),
    adae = adae
  )

  roles <- pp_resolve_roles(dm_obj, list(severity = "SEVX"))
  expect_identical(roles$severity, "SEVX")

  # the gantt colors by the declared column, not the detected grade
  chart <- ae_gantt_viz$render(
    dm_obj,
    time_range = as.Date(c("2020-01-01", "2020-06-01")),
    settings = list(roles = roles)
  )
  bars <- chart$x$opts$series[[1]]$data
  expect_identical(
    vapply(bars, function(b) b$itemStyle$color, ""),
    "#DC2626"  # SEVERE word constant, not the grade-3 color
  )
})
