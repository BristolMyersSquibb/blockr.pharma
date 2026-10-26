test_that("pp_drill_state finds the drill filter's clause by its board id", {
  trail <- c(
    "board-block_global_filter-expr-" = "SEX = F",
    "board-block_pt_drill-expr-" = "USUBJID = 01-701-1015"
  )
  expect_identical(
    pp_drill_state(trail, "pt_drill"),
    list(id = "pt_drill", clause = "USUBJID = 01-701-1015")
  )
  # The global filter's clause is not a drill.
  expect_null(pp_drill_state(trail[1L], "pt_drill"))
  # A drill filter with nothing claimed leaves no entry.
  expect_null(pp_drill_state(trail["board-block_global_filter-expr-"], "pt_drill"))
})

test_that("pp_drill_state reports nothing without exactly one target or a trail", {
  trail <- c("board-block_pt_drill-expr-" = "SEX = M")
  expect_null(pp_drill_state(trail, character()))
  expect_null(pp_drill_state(trail, c("a", "b")))
  expect_null(pp_drill_state(trail, ""))
  expect_null(pp_drill_state(NULL, "pt_drill"))
  expect_null(pp_drill_state(character(), "pt_drill"))
  # Two blocks whose ids share a suffix do not collide: the match is on the
  # whole `block_<id>` segment.
  expect_null(pp_drill_state(c("board-block_xpt_drill-expr-" = "A = 1"), "pt_drill"))
})
