# The timeline's "Drugs" setting: which drugs the treatment layer draws.

test_that("the drug choices are the drugs ADEX names", {
  expect_identical(timeline_drug_choices(pt_toy()), c("Drug", "Placebo"))
  expect_identical(timeline_drug_choices(list(adsl = pt_toy()$adsl)),
                   "No drugs in the data")
})

test_that("picking one drug of two drops the other's lines and tags", {
  layers <- c("Treatment", "Discontinuation and death")
  trt <- function(f) f[f$Type %in% "Treatment", ]
  f <- patient_timeline_frame(pt_toy(), layers = layers, dose_tags = TRUE,
                              drugs = "Placebo")
  expect_identical(unique(as.character(trt(f)$.lane)), "Placebo")
  expect_false(any(f$Event %in% "Dose change"))
  expect_false("Drug" %in% levels(f$Colour))
  # Placebo keeps its colour among all drugs.
  pal <- attr(f$Colour, "blockr_colors")
  expect_identical(unname(pal[["Placebo"]]), "#8f9bb3")

  f <- patient_timeline_frame(pt_toy(), layers = layers, dose_tags = TRUE,
                              drugs = "Drug")
  expect_identical(unique(as.character(trt(f)$.lane)), "Drug")
  expect_false("Placebo" %in% levels(f$Colour))
  expect_true(any(f$Event %in% "Dose change"))
  # Bars by dose drop the other drug too.
  b <- patient_timeline_frame(pt_toy(), layers = layers, drugs = "Drug",
                              treatment_as = "bars by dose")
  expect_identical(unique(as.character(trt(b)$.lane)), "Drug")
})

test_that("a patient without a picked drug keeps its row while AEs draw it", {
  f <- patient_timeline_frame(pt_toy(), layers = c("Treatment",
                                                   "Adverse events"),
                              drugs = "Drug")
  expect_true("B" %in% f$USUBJID)
  expect_false(any(f$Type %in% "Treatment" & f$USUBJID == "B"))
})

test_that("by default the timeline draws every drug", {
  layers <- c("Treatment", "Discontinuation and death")
  f <- patient_timeline_frame(pt_toy(), layers = layers)
  lanes <- function(f) sort(unique(as.character(f$.lane[f$Type %in%
                                                          "Treatment"])))
  expect_identical(lanes(f), c("Drug", "Placebo"))
  all <- patient_timeline_frame(pt_toy(), layers = layers,
                                drugs = c("Drug", "Placebo"))
  expect_identical(lanes(all), c("Drug", "Placebo"))
  # The script picks every drug the data has.
  viz <- asNamespace("blockr.viz")
  sp <- viz$cb_specs(viz$cb_parse(patient_timeline_script()), pt_toy(),
                     list(layers = layers))
  drugs <- Filter(function(s) identical(s$name, "drugs"), sp)[[1L]]
  expect_identical(as.character(drugs$default), c("Drug", "Placebo"))
})

test_that("the subtitle names the drugs drawn", {
  viz <- asNamespace("blockr.viz")
  word <- function(layers, values = list()) {
    v <- c(list(layers = layers), values)
    sp <- viz$cb_specs(viz$cb_parse(patient_timeline_script()), pt_toy(), v)
    args <- viz$script_title_args(list(), sp, v)
    viz$resolve_title_template(patient_timeline_subtitle(), data.frame(),
                               args)
  }
  expect_identical(word("Treatment"), "Treatment in study days, drugs: all")
  expect_identical(word("Treatment", list(drugs = "Placebo")),
                   "Treatment in study days, drugs: Placebo")
  expect_identical(word("Response"), "Response in study days")
})

test_that("a restored timeline keeps its drug pick", {
  blk <- new_timeline_block(layers = "Treatment", drugs = "Placebo")
  rt <- board_round_trip(blk, pt_toy())
  expect_identical(as.character(unlist(rt$restored$script_values$drugs)),
                   "Placebo")
})

test_that("a saved default subtitle gains the drug word, an edited one stays", {
  old <- "{@layers} in study days[, AEs by {@ae_lanes}][, con meds by {@cm_lanes}]"
  sub_of <- function(b) blockr.core::blockr_ser(b)$payload$subtitle
  expect_identical(sub_of(new_timeline_block(subtitle = old)),
                   patient_timeline_subtitle())
  expect_identical(sub_of(new_timeline_block(subtitle = "My own words")),
                   "My own words")
})
