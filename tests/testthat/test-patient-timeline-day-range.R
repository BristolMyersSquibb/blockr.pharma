# The study days the timeline's axis runs over: two subtitle words,
# `day_from` and `day_to`, empty for the data's range.

pt_range_model <- function(...) {
  viz <- asNamespace("blockr.viz")
  b <- new_timeline_block(layers = PT_LAYERS)
  spec <- viz$viz_block_spec("table")
  p <- blockr.core::blockr_ser(b)$payload
  p <- p[intersect(names(p), names(spec$args))]
  p[c("layout", "by", "group_rows", "missing", "sort_by", "row_sections")] <-
    list("groups", c("USUBJID", "Row"), "summary", "drop", "data", ".section")
  st <- viz$table_normalize(viz$viz_check(spec, p))
  frame <- patient_timeline_sort(
    patient_timeline_frame(pt_toy(), layers = PT_LAYERS, ...)
  )
  m <- viz$table_model_groups(st, frame)
  m$cells$cols[[length(m$cells$cols)]]
}

test_that("the frame carries the range set as the start column's domain", {
  f <- patient_timeline_frame(pt_toy())
  expect_null(attr(f$Start, "blockr_domain"))
  f <- patient_timeline_frame(pt_toy(), day_from = 1, day_to = 30)
  expect_identical(attr(f$Start, "blockr_domain"), c(1, 30))
  f <- patient_timeline_frame(pt_toy(), day_from = NA, day_to = 30)
  expect_identical(attr(f$Start, "blockr_domain"), c(NA, 30))
  # The sort keeps it.
  expect_identical(attr(patient_timeline_sort(f)$Start, "blockr_domain"),
                   c(NA, 30))
})

test_that("the timeline's axis runs over the range set", {
  free <- pt_range_model()
  set <- pt_range_model(day_from = 1, day_to = 30)
  expect_equal(c(set$d0, set$d1), c(1, 30))
  expect_false(isTRUE(all.equal(c(free$d0, free$d1), c(1, 30))))
  # Nothing is drawn past the right end: every bar ends by 100%.
  ends <- unlist(lapply(set$segs, function(r) {
    vapply(r, function(g) g[[1L]] + g[[2L]], numeric(1L))
  }))
  expect_true(all(ends <= 100 + 1e-9))
  one <- pt_range_model(day_to = 30)
  expect_equal(one$d0, free$d0)
  expect_equal(one$d1, 30)
})

test_that("the subtitle names the range, the gear's Time tab has its rows", {
  expect_match(patient_timeline_subtitle(),
               "in study days {@day_from} to {@day_to}", fixed = TRUE)
  sc <- patient_timeline_script()
  expect_match(sc, "day_from <- NA_real_", fixed = TRUE)
  expect_match(sc, "placeholder = \"last\"", fixed = TRUE)
  g <- patient_timeline_gear()
  time <- Filter(function(x) identical(x$title, "Time"),
                 g$tabs[[1L]]$sections)[[1L]]
  expect_identical(time$keys,
                   c("origin", "cutoff", "day_from", "day_to", "ongoing"))
})

test_that("an earlier default subtitle gains the range words", {
  before <- paste0("{@layers} in study days[, AEs by {@ae_lanes}]",
                   "[, con meds by {@cm_lanes}][, drugs: {@drugs}]")
  sub_of <- function(b) blockr.core::blockr_ser(b)$payload$subtitle
  expect_identical(sub_of(new_timeline_block(subtitle = before)),
                   patient_timeline_subtitle())
})

test_that("a saved timeline without the range restores unchanged", {
  blk <- new_timeline_block(layers = "Adverse events")
  rt <- board_round_trip(blk, pt_toy())
  sv <- rt$restored$script_values
  expect_true(is.null(sv$day_from) || is.na(sv$day_from))
  expect_true(is.null(sv$day_to) || is.na(sv$day_to))
  set <- new_timeline_block(layers = "Adverse events", day_from = 1,
                            day_to = 90)
  rt <- board_round_trip(set, pt_toy())
  expect_equal(as.numeric(unlist(rt$restored$script_values$day_to)), 90)
})
