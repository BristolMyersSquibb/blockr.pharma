# The `level` column kind: a board marks the columns that code the same thing
# at different detail (blockr.viz::mark_column_kinds()), and the timeline and
# the profile offer those, in the order marked, labelled by the data.

# pt_toy() with more AE and CM levels, marked as a board would mark them.
pt_toy_levels <- function() {
  d <- pt_toy()
  d$adae$AELLT <- c("Head pain", "Feeling sick", "Fall")
  d$adae$AEHLGT <- c("HEADACHES", "GI SIGNS", "INJURIES")
  d$adae$SMQ01NAM <- c(NA, "Gastrointestinal (SMQ)", NA)
  attr(d$adae$AELLT, "label") <- "Lowest Level Term"
  attr(d$adae$AEDECOD, "label") <- "Dictionary-Derived Term"
  attr(d$adae$AEHLGT, "label") <- "High Level Group Term"
  attr(d$adae$AEBODSYS, "label") <- "Body System or Organ Class"
  attr(d$adae, "blockr_kinds") <- c(
    AETERM = "level", AELLT = "level", AEDECOD = "level", AEHLT = "level",
    AEHLGT = "level", AEBODSYS = "level", SMQ01NAM = "level", AESEV = "group"
  )
  d$adcm$ATC2T <- "ANALGESICS, SYSTEMIC"
  attr(d$adcm, "blockr_kinds") <- c(CMTRT = "level", CMDECOD = "level",
                                    ATC2T = "level")
  d
}

test_that("the timeline offers the marked level columns, in order", {
  ch <- timeline_lane_choices(pt_toy_levels(), "ae")
  # AEHLT is marked but not in the data: left out.
  expect_identical(as.character(ch), c("AETERM", "AELLT", "AEDECOD", "AEHLGT",
                                 "AEBODSYS", "SMQ01NAM"))
  # Labels are the data's; a column without one reads as its name.
  expect_identical(names(ch)[2:4], c("Lowest Level Term",
                                     "Dictionary-Derived Term",
                                     "High Level Group Term"))
  expect_identical(names(ch)[[6L]], "SMQ01NAM")
  expect_identical(unname(ch[attr(ch, "default")]), "AEDECOD")

  cm <- timeline_lane_choices(pt_toy_levels(), "cm")
  expect_identical(as.character(cm), c("CMTRT", "CMDECOD", "ATC2T"))
  expect_identical(unname(cm[attr(cm, "default")]), "CMDECOD")
})

test_that("without marks the timeline offers what it always did", {
  ch <- timeline_lane_choices(pt_toy(), "ae")
  expect_true("Dictionary-Derived Term" %in% names(ch))
  expect_true(all(ch %in% names(pt_toy()$adae)))
})

test_that("the timeline draws a row per value of a marked level", {
  d <- pt_toy_levels()
  f <- patient_timeline_frame(d, ae_lanes = "AEHLGT")
  rows <- unique(f$Row[f$.section %in% "Adverse events"])
  expect_setequal(toupper(rows), c("HEADACHES", "GI SIGNS", "INJURIES"))
  # AEs outside an SMQ have no value there: they share one lane.
  f <- patient_timeline_frame(d, ae_lanes = "SMQ01NAM")
  rows <- f$Row[f$.section %in% "Adverse events"]
  expect_identical(sum(rows == PP_LANE_UNCODED), 2L)
  f <- patient_timeline_frame(d, layers = "Con meds", cm_lanes = "ATC2T")
  expect_identical(toupper(unique(f$Row[f$.section %in% "Con meds"])),
                   "ANALGESICS, SYSTEMIC")
})

test_that("a saved pick the data lacks falls back to the preferred term", {
  f <- patient_timeline_frame(pt_toy(), ae_lanes = "AEHLGT")
  rows <- unique(f$Row[f$.section %in% "Adverse events"])
  expect_setequal(toupper(rows), toupper(pt_toy()$adae$AEDECOD))
  # An old board's pick still draws as before.
  f <- patient_timeline_frame(pt_toy(), ae_lanes = "AEBODSYS")
  rows <- unique(f$Row[f$.section %in% "Adverse events"])
  expect_true(all(toupper(rows) %in% toupper(pt_toy()$adae$AEBODSYS)))
})

test_that("the profile's lanes pill offers the marked levels", {
  d <- pt_toy_levels()
  ctrl <- pp_lane_control(PP_AE_LANES, default = "AEDECOD")$lanes
  dm_obj <- dm::dm(adae = d$adae)
  ch <- pp_ctrl_present_choices(ctrl$choices, ctrl, dm_obj, "adae")
  expect_identical(as.character(ch), c("AETERM", "AELLT", "AEDECOD", "AEHLGT",
                                 "AEBODSYS", "SMQ01NAM"))
  # Unmarked: the viz's own ladder, the levels the data has.
  plain <- dm::dm(adae = pt_toy()$adae)
  ch <- pp_ctrl_present_choices(ctrl$choices, ctrl, plain, "adae")
  expect_identical(unname(ch), c("AETERM", "AEDECOD", "AEBODSYS"))
  # A marked level outside the ladder draws its lanes; an unknown pick falls
  # back to the default.
  expect_identical(pp_lane_column(d$adae, PP_AE_LANES, "AEHLGT", "AEDECOD"),
                   "AEHLGT")
  expect_identical(pp_lane_column(pt_toy()$adae, PP_AE_LANES, "AEHLGT",
                                  "AEDECOD"), "AEDECOD")
})

test_that("a mark on AESOC reads as the normalized AEBODSYS", {
  tbl <- data.frame(AEDECOD = "x", AEBODSYS = "y")
  attr(tbl, "blockr_kinds") <- c(AEDECOD = "level", AESOC = "level")
  expect_identical(unname(pp_level_choices(tbl, character())),
                   c("AEDECOD", "AEBODSYS"))
})
