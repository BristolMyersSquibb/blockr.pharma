ae_params_eval <- function(expr, data) {
  eval(expr, list(data = data, . = function(x) x))
}

test_that("AE parameters expression applies pooling after flag filtering", {
  ae <- data.frame(
    FLAG = c("Y", ""),
    value = c(1, NA),
    stringsAsFactors = FALSE
  )
  flag_expr <- quote(dplyr::filter(.(data), FLAG %in% c("Y", "y")))

  out <- ae_params_eval(
    blockr.pharma:::ae_parameters_expr(
      flag_expr,
      pool_aedecod = TRUE,
      pool_transform = "stats::na.omit",
      pool_note_col = "pool_note",
      pool_note = "Pooled"
    ),
    ae
  )

  expect_equal(nrow(out), 1L)
  expect_identical(out$pool_note, "Pooled")
})

test_that("AE parameters expression leaves data unpooled when disabled", {
  ae <- data.frame(
    FLAG = c("Y", "Y"),
    value = c(1, NA),
    stringsAsFactors = FALSE
  )
  flag_expr <- quote(dplyr::filter(.(data), FLAG %in% c("Y", "y")))

  out <- ae_params_eval(
    blockr.pharma:::ae_parameters_expr(
      flag_expr,
      pool_aedecod = FALSE,
      pool_transform = "stats::na.omit",
      pool_note_col = "pool_note",
      pool_note = "Pooled"
    ),
    ae
  )

  expect_equal(nrow(out), 2L)
  expect_identical(out$pool_note, c("", ""))
})

test_that("AE parameters block uses the flag checkbox treatment for pooling", {
  block <- new_ae_parameters_block(
    columns = c("PREFL", "TRTEMFL"),
    selected = "TRTEMFL",
    footnotes = c(TRTEMFL = "Treatment-emergent"),
    definitions = c(TRTEMFL = "Treatment-emergent flag"),
    pool_aedecod = TRUE,
    pool_transform = "stats::na.omit",
    pool_note = "Pooled",
    pool_note_col = "pool_note",
    block_name = "AE Parameters"
  )
  ui <- paste(as.character(block$expr_ui("ae_parameters")), collapse = "\n")

  expect_s3_class(block, "ae_parameters_block")
  expect_equal(length(gregexpr('class="block-container"', ui, fixed = TRUE)[[1]]), 1L)
  expect_match(ui, "Flags", fixed = TRUE)
  expect_match(ui, "Pooling", fixed = TRUE)
  expect_match(ui, "Pool AEDECOD terms", fixed = TRUE)
  expect_match(ui, "blockr-checkbox ffb-row ae-parameters-pool-row", fixed = TRUE)
  expect_match(ui, "AE Flags Definition:", fixed = TRUE)

  payload <- blockr.core::blockr_ser(block)$payload
  expect_equal(unname(unlist(payload$footnotes["TRTEMFL"])), "Treatment-emergent")
  expect_true(unlist(payload$pool_aedecod))
  expect_equal(unlist(payload$pool_transform), "stats::na.omit")
  expect_equal(unlist(payload$pool_note_col), "pool_note")
})

test_that("AE parameters block server returns pooling state", {
  block <- new_ae_parameters_block(
    columns = "TRTEMFL",
    selected = "TRTEMFL",
    pool_aedecod = TRUE,
    pool_transform = "stats::na.omit"
  )
  data <- shiny::reactive(data.frame(
    TRTEMFL = c("Y", ""),
    value = c(1, NA),
    stringsAsFactors = FALSE
  ))

  shiny::testServer(
    function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        session$userData$expr <- block$expr_server("expr", data)
      })
    },
    {
      state <- session$userData$expr$state
      expect_true("pool_aedecod" %in% names(state))
      expect_true(state$pool_aedecod())
      expect_equal(state$pool_transform(), "stats::na.omit")
    }
  )
})
