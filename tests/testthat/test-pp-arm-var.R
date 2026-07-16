# The arm label has two consumers (subject picker meta, overview treatment
# lane). They must resolve the same ADSL column. The column is a study fact,
# declared per board via the "study_roles" board option: a deployment can
# serve several studies, so app-level config cannot express more than one
# answer.
# Undeclared, the column is ACTARM (the *actual* arm -- this is a safety
# view); a declared-but-missing column is an error, never a fallback, because
# a study carries several correct-looking arm columns at once and "whichever
# is present" is a quiet wrong answer.

test_that("a declared arm column wins", {
  cols <- c("USUBJID", "TRT", "ARM", "ACTARM", "TRT01P")
  expect_identical(pp_arm_column(cols, "TRT"), "TRT")
  expect_identical(pp_arm_column(cols, "ARM"), "ARM")
})

test_that("undeclared resolves to ACTARM and nothing else", {
  expect_identical(pp_arm_column(c("ACTARM", "ARM"), NULL), "ACTARM")
  # the old ADaM chain is gone: a planned-arm or TRT01x column alone does
  # not resolve, it errors
  expect_error(
    pp_arm_column(c("USUBJID", "ARM"), NULL),
    class = "pp_arm_var_undeclared"
  )
  expect_error(
    pp_arm_column(c("TRT01P", "TRT01A"), NULL),
    class = "pp_arm_var_undeclared"
  )
})

test_that("undeclared without ACTARM errors, pointing at the sidebar", {
  err <- tryCatch(
    pp_arm_column(c("USUBJID", "AGE"), NULL),
    error = function(e) e
  )
  expect_s3_class(err, "pp_arm_var_undeclared")
  expect_match(conditionMessage(err), "ACTARM")
  expect_match(conditionMessage(err), "sidebar")
})

test_that("a declared-but-missing column errors and never falls back", {
  # ACTARM is present, but the study declared TRT and does not carry it:
  # this must stop, not quietly draw ACTARM
  err <- tryCatch(
    pp_arm_column(c("USUBJID", "ACTARM", "ARM"), "TRT"),
    error = function(e) e
  )
  expect_s3_class(err, "pp_arm_var_missing")
  expect_match(conditionMessage(err), "TRT", fixed = TRUE)
  expect_match(conditionMessage(err), "sidebar")
})

test_that("pp_roles_blocker turns resolution failures into a quoted stop()", {
  adsl <- data.frame(USUBJID = "a", ACTARM = "Placebo",
                     stringsAsFactors = FALSE)
  dm_ok <- dm::dm(adsl = adsl)

  # resolvable (or nothing to resolve): no blocker
  expect_null(pp_roles_blocker(dm_ok, NULL))
  expect_null(pp_roles_blocker(data.frame(x = 1), list(arm = "TRT")))
  expect_null(pp_roles_blocker(dm::dm(other = adsl), list(arm = "TRT")))

  # declared-but-missing: a quoted stop() that re-raises the classed error
  blocker <- pp_roles_blocker(dm_ok, list(arm = "TRT"))
  expect_true(is.call(blocker))
  expect_error(eval(blocker, baseenv()), class = "pp_arm_var_missing")
})

test_that("the picker labels subjects with the resolved arm role", {
  adsl <- data.frame(
    USUBJID = c("a", "b"),
    TRT = c("Drug X", "Drug Y"),
    ACTARM = c("GROUPAB COMPOUND-A", "GROUPBP1 COMPOUND-A"),
    AGE = c(63L, 71L), SEX = c("F", "M"),
    stringsAsFactors = FALSE
  )
  dm_obj <- dm::dm(adsl = adsl)

  declared <- pp_subject_choices(dm_obj, pp_resolve_roles(dm_obj, list(arm = "TRT"))$arm)
  expect_identical(declared$meta, c("Drug X · 63F", "Drug Y · 71M"))

  # undeclared: ACTARM, the default
  undeclared <- pp_subject_choices(dm_obj, pp_resolve_roles(dm_obj)$arm)
  expect_identical(undeclared$meta,
                   c("GROUPAB COMPOUND-A · 63F", "GROUPBP1 COMPOUND-A · 71M"))
})

test_that("labels degrade to id-only when the arm does not resolve", {
  # The loud failure lives on the block's eval path (pp_arm_blocker); the
  # label helpers run inside reactives that observers consume, where an
  # error would end the session, so roles resolve totally ($arm_error) and
  # the labels degrade.
  adsl <- data.frame(
    USUBJID = c("a", "b"),
    ARM = c("X", "Y"),
    AGE = c(63L, 71L), SEX = c("F", "M"),
    stringsAsFactors = FALSE
  )
  dm_obj <- dm::dm(adsl = adsl)

  declared <- pp_resolve_roles(dm_obj, list(arm = "TRT"))
  expect_null(declared$arm)
  expect_s3_class(declared$errors$arm, "pp_arm_var_missing")
  expect_identical(pp_subject_choices(dm_obj, declared$arm)$meta,
                   c("63F", "71M"))

  undeclared <- pp_resolve_roles(dm_obj)
  expect_null(undeclared$arm)
  expect_s3_class(undeclared$errors$arm, "pp_arm_var_undeclared")
  expect_identical(pp_subject_choices(dm_obj, undeclared$arm)$meta,
                   c("63F", "71M"))
})

test_that("the overview lane honors the injected arm role", {
  dm_obj <- dm::dm(
    adsl = data.frame(
      USUBJID = "a",
      TRTSDT = as.Date("2020-01-01"), TRTEDT = as.Date("2020-06-01"),
      TRT = "Drug X", ACTARM = "GROUPAB COMPOUND-A",
      stringsAsFactors = FALSE
    )
  )
  lane_label <- function(settings) {
    chart <- patient_overview_viz$render(
      dm_obj, time_range = pp_compute_time_range(dm_obj), settings = settings
    )
    trt <- Filter(function(s) identical(s$name, "Treatment"), chart$x$opts$series)
    paste(as.character(unlist(trt)), collapse = " ")
  }
  expect_match(lane_label(list(roles = list(arm = "TRT"))), "Drug X")
  expect_match(
    lane_label(list(roles = pp_resolve_roles(dm_obj)["arm"])),
    "GROUPAB COMPOUND-A"
  )
  # no role injected (or unresolved): generic label, never a guess
  expect_match(lane_label(list()), "Treatment")
})

test_that("picker and overview agree on the column once arm_var is declared", {
  # The regression this whole setting exists to prevent: prod adsl carries both
  # TRT and ACTARM, and the two consumers used to resolve different ones.
  cols <- c("USUBJID", "TRT", "ACTARM")
  expect_identical(pp_arm_column(cols, "TRT"), pp_arm_column(cols, "TRT"))
  expect_identical(pp_arm_column(cols, "TRT"), "TRT")
})

test_that("arm_var is not a constructor argument", {
  # A formal would round-trip through block state and become reachable by
  # the assistant. A legacy arm_var= argument (boards saved before the
  # board option) must not break construction -- ignored with a warning.
  expect_warning(
    blk <- new_patient_profile_block(arm_var = "TRT"),
    "sidebar"
  )
  expect_s3_class(blk, "patient_profile_block")
  expect_s3_class(new_patient_profile_block(), "patient_profile_block")
  # ... and it is not block state (not persisted, not externally controllable)
  st <- attr(new_patient_profile_block(), "allow_empty_state")
  expect_false("arm_var" %in% st)
  expect_false("arm_var" %in% attr(new_patient_profile_block(), "external_ctrl"))
})
