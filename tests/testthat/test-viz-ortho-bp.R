# Orthostatic BP sources body position from ATPT (ADaM timepoint phrase) or
# VSPOS (SDTM position term), and places every reading on the category it was
# actually measured at.

ortho_dm <- function(pos_col, rows) {
  df <- do.call(rbind, lapply(names(rows), function(pc) {
    data.frame(PARAMCD = pc, pos = rows[[pc]][["pos"]],
               AVAL = rows[[pc]][["val"]], stringsAsFactors = FALSE)
  }))
  names(df)[names(df) == "pos"] <- pos_col
  df$AVISIT <- "BASELINE"
  df$USUBJID <- "x"
  dm::dm(advs = df)
}

ortho_opts <- function(dm_obj) {
  res <- blockr.pharma:::pp_resolve_requires(
    dm_obj, blockr.pharma:::ortho_bp_viz
  )
  testthat::expect_true(isTRUE(res$ok))
  blockr.pharma:::ortho_bp_viz$render(
    res$dm, time_range = NULL, settings = list()
  )$x$opts
}

# Category each point of `series_name` was drawn on.
drawn_at <- function(opts, series_name) {
  cats <- unlist(opts$xAxis$data)
  s <- Filter(function(s) identical(s$name, series_name), opts$series)[[1]]
  vapply(s$data, function(d) cats[d$value[[1]] + 1L], character(1))
}

adam_rows <- list(
  SYSBP = list(pos = c("AFTER LYING DOWN FOR 5 MINUTES",
                       "AFTER STANDING FOR 1 MINUTE",
                       "AFTER STANDING FOR 3 MINUTES"),
               val = c(120, 125, 130))
)

test_that("ADaM ATPT timepoint phrases map to positions", {
  opts <- ortho_opts(ortho_dm("ATPT", adam_rows))
  expect_equal(unlist(opts$xAxis$data),
               c("Lying", "Standing 1m", "Standing 3m"))
})

test_that("SDTM VSPOS satisfies the ATPT requirement", {
  opts <- ortho_opts(ortho_dm("VSPOS", list(
    SYSBP = list(pos = c("SUPINE", "SITTING", "STANDING"),
                 val = c(120, 125, 130))
  )))
  expect_equal(unlist(opts$xAxis$data), c("Lying", "Sitting", "Standing"))
})

test_that("positions are ordered by orthostatic challenge, not by appearance", {
  opts <- ortho_opts(ortho_dm("VSPOS", list(
    SYSBP = list(pos = c("STANDING", "SUPINE", "SITTING"),
                 val = c(130, 120, 125))
  )))
  expect_equal(unlist(opts$xAxis$data), c("Lying", "Sitting", "Standing"))
})

test_that("position terms are matched case-insensitively", {
  opts <- ortho_opts(ortho_dm("VSPOS", list(
    SYSBP = list(pos = c("supine", "Standing"), val = c(120, 130))
  )))
  expect_equal(unlist(opts$xAxis$data), c("Lying", "Standing"))
})

test_that("a series missing a position leaves a gap instead of shifting left", {
  # DIABP was never measured lying down; its two readings must stay on
  # Standing 1m / Standing 3m rather than sliding onto Lying / Standing 1m.
  opts <- ortho_opts(ortho_dm("ATPT", c(adam_rows, list(
    DIABP = list(pos = c("AFTER STANDING FOR 1 MINUTE",
                         "AFTER STANDING FOR 3 MINUTES"),
                 val = c(85, 90))
  ))))
  expect_equal(unlist(opts$xAxis$data),
               c("Lying", "Standing 1m", "Standing 3m"))
  expect_equal(drawn_at(opts, "Systolic BASELINE"),
               c("Lying", "Standing 1m", "Standing 3m"))
  expect_equal(drawn_at(opts, "Diastolic BASELINE"),
               c("Standing 1m", "Standing 3m"))
})

test_that("unrecognized position terms fail loudly rather than plotting", {
  dm_obj <- ortho_dm("VSPOS", list(
    SYSBP = list(pos = c("PRE-DOSE", "POST-DOSE"), val = c(120, 130))
  ))
  res <- blockr.pharma:::pp_resolve_requires(
    dm_obj, blockr.pharma:::ortho_bp_viz
  )
  chart <- blockr.pharma:::ortho_bp_viz$render(
    res$dm, time_range = NULL, settings = list()
  )
  expect_match(
    paste(unlist(chart$x$opts$title), collapse = " "),
    "No recognized BP positions"
  )
})

test_that("neither ATPT nor VSPOS present is reported as a missing column", {
  df <- data.frame(USUBJID = "x", PARAMCD = "SYSBP", AVAL = 120,
                   VSTPT = "PRE-DOSE")
  res <- blockr.pharma:::pp_resolve_requires(
    dm::dm(advs = df), blockr.pharma:::ortho_bp_viz
  )
  expect_false(res$ok)
  expect_match(res$msg, "missing ATPT \\(or VSPOS\\) in advs")
})
