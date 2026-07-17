# Study data (arm labels, visit names, questionnaire items) is pasted into the
# hand-written JS of the echarts renderers. Hand-escaping the single quote is
# not enough: a newline or a backslash breaks the string literal, and a JS
# syntax error takes down the whole widget -- the panel renders its header and
# an empty body, with nothing in the R log. Encode, never paste.

test_that("pp_js_str() encodes characters that break a JS literal", {
  expect_identical(pp_js_str("Placebo"), "\"Placebo\"")

  # The ones that actually broke it: a line break and a backslash.
  expect_identical(pp_js_str("A\nB"), "\"A\\nB\"")
  expect_identical(pp_js_str("A\\B"), "\"A\\\\B\"")

  # Quotes of both kinds survive.
  expect_identical(pp_js_str("O'Brien"), "\"O'Brien\"")
  expect_identical(pp_js_str("say \"hi\""), "\"say \\\"hi\\\"\"")

  # A missing arm reads as empty, not as the string "NA".
  expect_identical(pp_js_str(NA_character_), "\"\"")
})

test_that("pp_js_arr() stays an array even for one element", {
  expect_identical(pp_js_arr(c("V1", "V2")), "[\"V1\",\"V2\"]")
  expect_identical(pp_js_arr("V1"), "[\"V1\"]")
})

overview_arm_js <- function(arm) {
  adsl <- safetyData::adam_adsl[1, ]
  adsl$TRT <- arm
  dm_obj <- dm::dm_add_pk(dm::dm(adsl = adsl), adsl, USUBJID)

  chart <- patient_overview_viz$render(
    dm_obj,
    time_range = c(-10, 200),
    settings = list(roles = list(arm = "TRT")),
    ref_ms = NA_real_,
    mode = "rday"
  )
  chart$x$opts$series[[1L]]$renderItem
}

test_that("the treatment lane folds a multi-line arm label to one line", {
  skip_if_not_installed("safetyData")

  # A study's own arm column may carry line breaks the ADaM arm variables
  # never do. The lane draws one line of text inside the bar.
  js <- overview_arm_js("XYZ-000000 2.5mg+\nOxaliplatin")

  expect_false(grepl("2.5mg+\nOxaliplatin", js, fixed = TRUE))
  expect_true(grepl("2.5mg+ Oxaliplatin", js, fixed = TRUE))
})

test_that("a hostile arm label is encoded, not pasted, into the lane's JS", {
  skip_if_not_installed("safetyData")

  # Characters the whitespace fold does not touch, and which a hand-rolled
  # quote escape gets wrong. A syntax error here blanks the whole panel.
  js <- overview_arm_js("XYZ \\ 'high' \"dose\"")

  expect_true(grepl('XYZ \\\\ \'high\' \\"dose\\"', js, fixed = TRUE))
})
