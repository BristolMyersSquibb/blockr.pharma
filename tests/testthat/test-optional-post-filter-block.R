opf_eval <- function(expr, data) {
  eval(expr, list(data = data, . = function(x) x))
}

test_that("apply_optional_post_filter passes through when disabled", {
  d <- data.frame(x = c("a", "b"), stringsAsFactors = FALSE)

  out <- apply_optional_post_filter(
    d,
    enabled = FALSE,
    transform = "base::toupper",
    note_col = "note",
    note = "Transformed"
  )

  expect_identical(out$x, d$x)
  expect_identical(out$note, c("", ""))
})

test_that("apply_optional_post_filter runs a namespaced transform when enabled", {
  d <- data.frame(x = c(1L, 2L), stringsAsFactors = FALSE)

  out <- apply_optional_post_filter(
    d,
    enabled = TRUE,
    transform = "stats::na.omit",
    note_col = "note",
    note = "Filtered"
  )

  expect_s3_class(out, "data.frame")
  expect_identical(out$x, d$x)
  expect_identical(out$note, c("Filtered", "Filtered"))
})

test_that("make_optional_post_filter_expr emits a stable quoted transform", {
  d <- data.frame(x = c(1, NA, 2))
  e <- blockr.pharma:::make_optional_post_filter_expr(
    TRUE,
    "stats::na.omit",
    note_col = "note",
    note = "Filtered"
  )

  out <- opf_eval(e, d)

  expect_equal(nrow(out), 2L)
  expect_identical(out$note, c("Filtered", "Filtered"))
})

test_that("optional post-filter block round-trips through board JSON", {
  blk <- new_optional_post_filter_block(
    enabled = TRUE,
    label = "Apply transform",
    transform = "stats::na.omit",
    note = "Filtered",
    note_col = "note"
  )

  ser <- blockr.core::blockr_ser(blk)
  back <- jsonlite::fromJSON(
    jsonlite::toJSON(ser, null = "null"),
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )
  blk2 <- blockr.core::blockr_deser(back)

  expect_s3_class(blk2, "optional_post_filter_block")
  expect_equal(unlist(ser$payload$enabled), TRUE)
  expect_equal(unlist(ser$payload$transform), "stats::na.omit")
  expect_equal(unlist(ser$payload$note_col), "note")
})
