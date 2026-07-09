test_that("new_patient_profile_block constructs with expected class", {
  blk <- new_patient_profile_block()
  expect_s3_class(blk, "patient_profile_block")
  expect_s3_class(blk, "block")
})

test_that("constructor accepts selected and viz_settings", {
  blk <- new_patient_profile_block(
    selected = c("patient_overview", "ae_gantt"),
    viz_settings = list(adas_trajectory = list(items = "ACTOT"))
  )
  expect_s3_class(blk, "patient_profile_block")
})

test_that("timeline_mode is validated", {
  expect_error(new_patient_profile_block(timeline_mode = "nope"))
  expect_s3_class(
    new_patient_profile_block(timeline_mode = "rday"),
    "patient_profile_block"
  )
})

test_that("subject is normalized and length-checked", {
  expect_identical(pp_validate_subject(NULL), character())
  expect_identical(pp_validate_subject(character()), character())
  expect_identical(pp_validate_subject(""), character())
  expect_identical(pp_validate_subject(NA_character_), character())
  expect_identical(pp_validate_subject(list()), character())
  expect_identical(pp_validate_subject("01-701-1015"), "01-701-1015")
  expect_identical(pp_validate_subject(list("01-701-1015")), "01-701-1015")
  expect_error(pp_validate_subject(c("a", "b")), "single USUBJID")
})

test_that("constructor accepts subject", {
  blk <- new_patient_profile_block(subject = "01-701-1015")
  expect_s3_class(blk, "patient_profile_block")
})

test_that("pp_resolve_subject never auto-picks from a cohort", {
  ids <- c("a", "b", "c")
  # A lone subject renders regardless of `subject`: upstream already committed.
  expect_identical(pp_resolve_subject("a", character()), "a")
  expect_identical(pp_resolve_subject("a", "zzz"), "a")
  # A cohort renders only on an explicit, valid pick.
  expect_identical(pp_resolve_subject(ids, "b"), "b")
  expect_identical(pp_resolve_subject(ids, character()), NA_character_)
  expect_identical(pp_resolve_subject(ids, "zzz"), NA_character_)
  expect_identical(pp_resolve_subject(character(), character()), NA_character_)
})

test_that("pp_subject_ids is total on non-dm and malformed input", {
  skip_if_not_installed("dm")
  expect_identical(pp_subject_ids(NULL), character())
  expect_identical(pp_subject_ids(data.frame(USUBJID = "a")), character())
  # a dm without adsl, and an adsl without USUBJID
  expect_identical(pp_subject_ids(dm::dm(adae = data.frame(x = 1))), character())
  expect_identical(pp_subject_ids(dm::dm(adsl = data.frame(x = 1))), character())
})

test_that("pp_subject_choices labels with whatever demography exists", {
  skip_if_not_installed("dm")

  bare <- dm::dm(adsl = data.frame(USUBJID = c("a", "b")))
  expect_identical(pp_subject_choices(bare)$labels, c("a", "b"))

  full <- dm::dm(adsl = data.frame(
    USUBJID = c("a", "b"),
    ARM = c("Placebo", "Xanomeline"),
    AGE = c(63L, 71L),
    SEX = c("F", "M")
  ))
  got <- pp_subject_choices(full)
  expect_identical(got$ids, c("a", "b"))
  expect_identical(got$labels, c("a · Placebo · 63F", "b · Xanomeline · 71M"))

  # AGE alone, no SEX
  part <- dm::dm(adsl = data.frame(USUBJID = "a", AGE = 63L))
  expect_identical(pp_subject_choices(part)$labels, "a · 63")
})

test_that("pp_subject_choices splits the muted secondary label off the id", {
  skip_if_not_installed("dm")
  full <- dm::dm(adsl = data.frame(
    USUBJID = c("a", "b"),
    ARM = c("Placebo", "Xanomeline"),
    AGE = c(63L, 71L),
    SEX = c("F", "M")
  ))
  got <- pp_subject_choices(full)
  expect_identical(got$meta, c("Placebo · 63F", "Xanomeline · 71M"))
  # label is the flat form: the search haystack and the button text
  expect_identical(got$labels, c("a · Placebo · 63F", "b · Xanomeline · 71M"))

  # meta is always id-length, blank where no column was available, and a blank
  # meta must not leave a dangling separator in the label
  bare <- pp_subject_choices(dm::dm(adsl = data.frame(USUBJID = c("a", "b"))))
  expect_identical(bare$meta, c("", ""))
  expect_identical(bare$labels, c("a", "b"))

  # an NA arm blanks rather than printing "NA"
  na_arm <- pp_subject_choices(dm::dm(adsl = data.frame(
    USUBJID = c("a", "b"), ARM = c(NA, "Placebo")
  )))
  expect_identical(na_arm$meta, c("", "Placebo"))
  expect_identical(na_arm$labels, c("a", "b · Placebo"))
})

test_that("pp_subject_choices does not misalign labels on a duplicated adsl", {
  skip_if_not_installed("dm")
  dup <- dm::dm(adsl = data.frame(
    USUBJID = c("a", "a", "b"),
    ARM = c("Placebo", "Placebo", "Xanomeline")
  ))
  got <- pp_subject_choices(dup)
  expect_identical(got$ids, c("a", "b"))
  expect_identical(got$labels, c("a · Placebo", "b · Xanomeline"))
})

test_that("a single-patient pharmaverseadam dm can feed the block", {
  skip_if_not_installed("pharmaverseadam")
  skip_if_not_installed("dm")

  adsl <- pharmaverseadam::adsl
  adae <- pharmaverseadam::adae
  advs <- pharmaverseadam::advs

  one <- adsl$USUBJID[1]
  pp_dm <- dm::dm(
    adsl = adsl[adsl$USUBJID == one, ],
    adae = adae[adae$USUBJID == one, ],
    advs = advs[advs$USUBJID == one, ]
  )

  tbls <- dm::dm_get_tables(pp_dm)
  expect_true(all(c("adsl", "adae", "advs") %in% names(tbls)))
  expect_identical(unique(as.data.frame(tbls$adsl)$USUBJID), one)

  expect_s3_class(
    new_patient_profile_block(selected = "patient_overview"),
    "patient_profile_block"
  )
})
