# The term levels the timeline and the profile offer: a fixed ladder from
# specific to broad (PP_AE_LANES, PP_CM_LANES), the columns the data has
# values in, named by the data's own column labels where it has them.

# pt_toy() with more AE and CM levels, labelled as a board labels them.
pt_toy_levels <- function() {
  d <- pt_toy()
  d$adae$AELLT <- c("Head pain", "Feeling sick", "Fall")
  d$adae$AEHLGT <- c("HEADACHES", "GI SIGNS", "INJURIES")
  d$adae$AEHLT <- NA_character_
  attr(d$adae$AELLT, "label") <- "Lowest level term"
  attr(d$adae$AEDECOD, "label") <- "Preferred term"
  attr(d$adae$AEHLGT, "label") <- "High level group term"
  attr(d$adae$AEBODSYS, "label") <- "System organ class"
  d
}

test_that("the timeline offers the ladder's columns with values, in order", {
  ch <- timeline_lane_choices(pt_toy_levels(), "ae")
  # AEHLT has no values: left out.
  expect_identical(as.character(ch), c("AETERM", "AELLT", "AEDECOD", "AEHLGT",
                                       "AEBODSYS"))
  expect_identical(names(ch)[2:4], c("Lowest level term", "Preferred term",
                                     "High level group term"))
  expect_identical(unname(ch[attr(ch, "default")]), "AEDECOD")

  cm <- timeline_lane_choices(pt_toy_levels(), "cm")
  expect_identical(unname(cm[attr(cm, "default")]), "CMDECOD")
})

test_that("a column's own label names its level, else the ladder's", {
  tbl <- data.frame(AEDECOD = "x", AEBODSYS = "y")
  attr(tbl$AEDECOD, "label") <- "Dictionary-Derived Term"
  ch <- pp_level_labels(tbl, PP_AE_LANES[c("Preferred term",
                                           "System organ class")])
  expect_identical(names(ch), c("Dictionary-Derived Term",
                                "System organ class"))
})

test_that("AESOC beside AEBODSYS is not offered twice", {
  d <- pt_toy()
  d$adae$AESOC <- d$adae$AEBODSYS
  expect_false("AESOC" %in% timeline_lane_choices(d, "ae"))
})

test_that("the timeline draws a row per value of the picked level", {
  f <- patient_timeline_frame(pt_toy_levels(), ae_lanes = "AEHLGT")
  rows <- unique(f$Row[f$.section %in% "Adverse events"])
  expect_setequal(toupper(rows), c("HEADACHES", "GI SIGNS", "INJURIES"))
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

test_that("the profile's lanes word offers the levels the data has", {
  d <- pt_toy_levels()
  ctrl <- pp_lane_control(PP_AE_LANES, default = "AEDECOD")$lanes
  ch <- pp_ctrl_present_choices(ctrl$choices, ctrl, dm::dm(adae = d$adae),
                                "adae")
  expect_identical(unname(ch), c("AETERM", "AELLT", "AEDECOD", "AEHLGT",
                                 "AEBODSYS"))
  expect_identical(names(ch)[[3L]], "Preferred term")
  plain <- dm::dm(adae = pt_toy()$adae)
  ch <- pp_ctrl_present_choices(ctrl$choices, ctrl, plain, "adae")
  expect_identical(unname(ch), c("AETERM", "AEDECOD", "AEBODSYS"))
  expect_identical(pp_lane_column(d$adae, PP_AE_LANES, "AEHLGT", "AEDECOD"),
                   "AEHLGT")
  expect_identical(pp_lane_column(pt_toy()$adae, PP_AE_LANES, "AEHLGT",
                                  "AEDECOD"), "AEDECOD")
})

test_that("the term filters name a level by the data's label", {
  d <- pt_toy()
  attr(d$adae$AEBODSYS, "label") <- "Body System or Organ Class"
  terms <- patient_timeline_terms(d, "ae")
  expect_true("Body System or Organ Class: Gastrointestinal disorders" %in%
                terms)
  f <- patient_timeline_frame(
    d, ae_terms = "Body System or Organ Class: Gastrointestinal disorders"
  )
  expect_identical(unique(f[f$.lane %in% "Adverse events", ]$Row), "Nausea")
  # The profile's own name still reads.
  f <- patient_timeline_frame(
    d, ae_terms = "System organ class: Gastrointestinal disorders"
  )
  expect_identical(unique(f[f$.lane %in% "Adverse events", ]$Row), "Nausea")
})
