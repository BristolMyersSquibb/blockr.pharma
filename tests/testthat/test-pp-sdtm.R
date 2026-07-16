# End-to-end: a REAL SDTM study (pharmaversesdtm) through the profile's
# pipeline -- domain table names (dm/ae/vs/lb/cm), SDTM column spellings
# (AESTDTC, VSTESTCD, LBSTRESN, CMSTDTC), character `*DTC` timestamps. This
# is the sharpest test of the role/normalization design: everything here
# used to die before the alias machinery ever ran, because the subject-table
# check looked for a literal `adsl` on the raw dm.

skip_if_not_installed("pharmaversesdtm")

sdtm_fixture <- function(n_subjects = 3L) {
  dm <- as.data.frame(pharmaversesdtm::dm)
  ae <- as.data.frame(pharmaversesdtm::ae)
  vs <- as.data.frame(pharmaversesdtm::vs)
  lb <- as.data.frame(pharmaversesdtm::lb)
  cm <- as.data.frame(pharmaversesdtm::cm)

  # Subjects that actually have AE, VS, LB and CM rows, so the renders have
  # something to draw.
  ids <- Reduce(intersect, list(
    unique(dm$USUBJID), unique(ae$USUBJID), unique(vs$USUBJID),
    unique(lb$USUBJID), unique(cm$USUBJID)
  ))
  ids <- utils::head(ids, n_subjects)
  stopifnot(length(ids) >= 1L)

  keep <- function(df) df[df$USUBJID %in% ids, , drop = FALSE]
  dm::dm(dm = keep(dm), ae = keep(ae), vs = keep(vs),
         lb = keep(lb), cm = keep(cm))
}

test_that("SDTM domains normalize to canonical tables and columns", {
  norm <- pp_normalize_dm(sdtm_fixture())
  tbls <- dm::dm_get_tables(norm)

  expect_setequal(names(tbls), c("adsl", "adae", "advs", "adlb", "adcm"))

  adsl <- as.data.frame(tbls$adsl)
  expect_s3_class(adsl$TRTSDT, "Date")   # from RFXSTDTC, coerced
  expect_s3_class(adsl$TRTEDT, "Date")
  expect_true("ACTARM" %in% colnames(adsl))  # native SDTM, the arm default

  adae <- as.data.frame(tbls$adae)
  expect_s3_class(adae$ASTDT, "Date")    # from AESTDTC (character)
  expect_true(all(c("ASTDY", "AENDY") %in% colnames(adae)))

  advs <- as.data.frame(tbls$advs)
  expect_true(all(
    c("PARAMCD", "PARAM", "AVAL", "ADT", "ATPT", "AVISIT", "AVISITN") %in%
      colnames(advs)
  ))
  expect_s3_class(advs$ADT, "Date")      # from VSDTC
  expect_type(advs$AVAL, "double")       # VSSTRESN, standardized numeric

  adlb <- as.data.frame(tbls$adlb)
  expect_true(all(
    c("PARAMCD", "AVAL", "ADT", "A1LO", "A1HI", "ANRIND") %in% colnames(adlb)
  ))

  adcm <- as.data.frame(tbls$adcm)
  expect_s3_class(adcm$ASTDT, "Date")    # from CMSTDTC
})

test_that("subject ids and scoping work on SDTM's dm domain", {
  raw <- sdtm_fixture()

  # BEFORE normalization (the block's expr runs on the raw input)
  ids <- pp_subject_ids(raw)
  expect_gt(length(ids), 0L)
  expect_identical(pp_subject_tbl_name(names(dm::dm_get_tables(raw))), "dm")

  # after normalization the ids are the same
  norm <- pp_normalize_dm(raw)
  expect_identical(pp_subject_ids(norm), ids)

  # scoping filters every table to the one subject
  one <- pp_scope_subject(norm, ids[[1L]])
  for (tbl in dm::dm_get_tables(one)) {
    df <- as.data.frame(tbl)
    if ("USUBJID" %in% colnames(df)) {
      expect_true(all(df$USUBJID == ids[[1L]]))
    }
  }
})

test_that("the output filter expression handles the SDTM dm domain", {
  # dm_filter(data, dm = ...) matches dm_filter's first FORMAL, not the
  # table, so the SDTM case must rename dm -> adsl (keys intact) first.
  raw <- sdtm_fixture()
  ids <- pp_subject_ids(raw)
  expr <- pp_subject_filter_expr("dm", ids[[1L]])

  out <- eval(expr, list(data = raw))
  tbls <- dm::dm_get_tables(out)
  expect_true("adsl" %in% names(tbls))
  expect_true(all(as.data.frame(tbls$adsl)$USUBJID == ids[[1L]]))

  # the ADaM case is the plain filter, untouched
  plain <- pp_subject_filter_expr("adsl", "x")
  expect_identical(
    plain,
    bquote(dm::dm_filter(data, adsl = USUBJID == "x"))
  )
})

test_that("roles resolve on SDTM: ACTARM natively, AESEV detected", {
  norm <- pp_normalize_dm(sdtm_fixture())
  roles <- pp_resolve_roles(norm)
  expect_identical(roles$arm, "ACTARM")
  expect_length(roles$errors, 0L)
  expect_identical(roles$severity, "AESEV")

  # and the picker labels carry the actual arm
  choices <- pp_subject_choices(norm, roles$arm)
  expect_gt(length(choices$ids), 0L)
  expect_true(any(nzchar(choices$meta)))
})

test_that("the time range covers AE and CM events, not just the ADSL window", {
  # The regression the dm-wide design kills: the axis was computed from raw
  # names before per-viz aliases resolved, so a study shipping AESTDTC got
  # its range from the ADSL treatment window alone and every event outside
  # it was clipped off the axis. Silently.
  norm <- pp_normalize_dm(sdtm_fixture())
  ids <- pp_subject_ids(norm)
  one <- pp_scope_subject(norm, ids[[1L]])

  tr <- pp_compute_time_range(one)
  expect_false(is.null(tr))

  adae <- as.data.frame(dm::dm_get_tables(one)$adae)
  ae_dates <- adae$ASTDT[!is.na(adae$ASTDT)]
  if (length(ae_dates)) {
    expect_lte(as.numeric(tr[1]), as.numeric(min(ae_dates)))
    expect_gte(as.numeric(tr[2]), as.numeric(max(ae_dates)))
  }
})

test_that("the profile's vizs are available and render on SDTM data", {
  norm <- pp_normalize_dm(sdtm_fixture())
  ids <- pp_subject_ids(norm)
  one <- pp_scope_subject(norm, ids[[1L]])
  roles <- pp_resolve_roles(norm)

  vizs <- c(patient_profile_static_vizs(), pp_findings_vizs(norm))
  tbl_names <- names(dm::dm_get_tables(norm))
  avail <- Filter(function(v) all(v$tables %in% tbl_names), vizs)
  expect_true(all(
    c("patient_overview", "ae_gantt", "cm_gantt", "ortho_bp") %in% names(avail)
  ))

  # requires resolve for the core vizs
  for (id in c("patient_overview", "ae_gantt", "cm_gantt")) {
    res <- pp_resolve_requires(norm, avail[[id]])
    expect_true(isTRUE(res$ok), info = id)
  }

  tr <- pp_compute_time_range(one)
  ref <- pp_compute_ref_ms(one)
  settings <- list(roles = roles)

  for (id in c("patient_overview", "ae_gantt", "cm_gantt")) {
    chart <- avail[[id]]$render(one, tr, settings, ref, "date")
    expect_s3_class(chart, "htmlwidget")
    # a real chart, not the pp_empty_chart placeholder
    expect_null(chart$x$opts$title$text, info = id)
  }

  # one findings viz (labs come from the combined adlb on SDTM)
  lab_ids <- grep("^adlb_", names(avail), value = TRUE)
  fallback <- intersect(c("cbc", "liver_panel", "blood_pressure"),
                        names(avail))
  pick <- c(lab_ids, fallback)[1L]
  chart <- avail[[pick]]$render(one, tr, list(), ref, "date")
  expect_s3_class(chart, "htmlwidget")
})

test_that("relative-day mode is study-wide even when one subject lacks dates", {
  norm <- pp_normalize_dm(sdtm_fixture())
  adsl <- as.data.frame(dm::dm_get_tables(norm)$adsl)
  # break the FIRST subject's treatment start; the study still has refs
  adsl$TRTSDT[1] <- as.Date(NA)
  tbls <- dm::dm_get_tables(norm)
  tbls$adsl <- adsl
  broken <- do.call(dm::dm, lapply(tbls, as.data.frame))

  expect_true(pp_has_ref(broken))          # study-level: still possible
  expect_true(is.na(pp_compute_ref_ms(broken)))  # per-patient row 1: not
})
