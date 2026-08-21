# The relative-day axis chooses its own ticks so that D1 -- treatment start,
# the origin every other day is counted from -- is always one of them. Left to
# echarts the sequence is round numbers (0, 50, 100) that never land on day 1,
# and the tick nearest treatment start sits at x=0, which the axis formatter
# reads as the day BEFORE it and labels "D-1".

test_that("D1 is a tick whenever it is inside the window", {
  expect_true(1 %in% pp_rday_ticks(-30, 182))
  expect_true(1 %in% pp_rday_ticks(1, 200))
  expect_true(1 %in% pp_rday_ticks(-5, 12))
  expect_true(1 %in% pp_rday_ticks(-400, 900))
  expect_true(1 %in% pp_rday_ticks(0, 3))
})

test_that("D1 is absent when the window does not reach it", {
  expect_false(1 %in% pp_rday_ticks(50, 400))
  expect_false(1 %in% pp_rday_ticks(-400, -10))
})

test_that("the window's own end points are always ticks", {
  ticks <- pp_rday_ticks(-30, 182)
  expect_equal(min(ticks), -30)
  expect_equal(max(ticks), 182)
})

test_that("ticks are whole days, ascending and unique", {
  for (r in list(c(-30, 182), c(0, 3), c(-1, 2), c(-5, 12), c(-400, 900))) {
    ticks <- pp_rday_ticks(r[1], r[2])
    # A fractional tick would print as "D0.5" -- there is no such day.
    expect_equal(ticks, round(ticks))
    expect_false(is.unsorted(ticks))
    expect_equal(anyDuplicated(ticks), 0L)
  }
})

test_that("no two ticks are close enough to overprint each other", {
  for (r in list(c(-30, 182), c(-5, 12), c(-400, 900), c(-60, 60))) {
    ticks <- pp_rday_ticks(r[1], r[2])
    span <- r[2] - r[1]
    # Labels are ~4 characters; anything under a twentieth of the axis would
    # collide. The generator's own guard is far wider than this.
    expect_true(all(diff(ticks) > span / 20))
  }
})

test_that("an unusable window asks for no custom ticks at all", {
  # `time_range` is optional: a render without one leaves the axis unbounded
  # and echarts fits it to the data. An empty customValues would mean "draw
  # no ticks", so the caller must get nothing rather than an empty request.
  expect_length(pp_rday_ticks(numeric(0), numeric(0)), 0L)
  expect_length(pp_rday_ticks(NA_real_, 10), 0L)
  expect_length(pp_rday_ticks(10, NA_real_), 0L)
  expect_length(pp_rday_ticks(10, 10), 0L)
  expect_length(pp_rday_ticks(10, -10), 0L)
})

test_that("pp_time_axis carries the ticks into both the labels and the grid", {
  ax <- pp_time_axis(
    c(as.Date("2020-01-01"), as.Date("2020-06-30")),
    ref_ms = pp_ms_ts(as.Date("2020-02-01")), mode = "rday"
  )
  expect_true(1 %in% ax$axisLabel$customValues)
  # The gridlines have to sit under the labels, not on echarts' own sequence,
  # or D1 gets a label with no line under it.
  expect_equal(ax$splitLine$customValues, ax$axisLabel$customValues)
  expect_true(ax$splitLine$show)
})

test_that("an unbounded rday axis carries no customValues field", {
  ax <- pp_time_axis(NULL, ref_ms = pp_ms_ts(as.Date("2020-02-01")),
                     mode = "rday")
  expect_null(ax$axisLabel$customValues)
  expect_null(ax$splitLine$customValues)
  # ... and still draws its gridlines, wherever echarts decides to put them.
  expect_true(ax$splitLine$show)
})

test_that("date mode is untouched by the relative-day tick logic", {
  ax <- pp_time_axis(
    c(as.Date("2020-01-01"), as.Date("2020-06-30")), mode = "date"
  )
  expect_equal(ax$type, "time")
  expect_null(ax$axisLabel$customValues)
})
