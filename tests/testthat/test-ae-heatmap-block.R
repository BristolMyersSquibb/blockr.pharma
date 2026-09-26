# new_ae_heatmap_block(): the pharma surface over blockr.viz's matrix
# engine. The engine renders; here the contract is IDENTITY -- what a study
# board serializes and restores through.

test_that("the block carries pharma identity and its own class", {
  b <- new_ae_heatmap_block(group = "TRT01A", drill = TRUE)
  expect_s3_class(b, "ae_heatmap_block")
  expect_s3_class(b, "heatmap_block")
  sr <- blockr.core::blockr_ser(b)
  expect_identical(sr$constructor$constructor, "new_ae_heatmap_block")
  expect_identical(sr$constructor$package, "blockr.pharma")
  # ctor / ctor_pkg / class are the framework's channel, never state
  expect_false(any(c("ctor", "ctor_pkg") %in% names(sr$payload)))
})

test_that("a serialized block restores through the pharma constructor", {
  b <- new_ae_heatmap_block(col = "AESOC", top_n = 10)
  b2 <- blockr.core::blockr_deser(blockr.core::blockr_ser(b))
  expect_identical(class(b2), class(b))
})

test_that("construction is warning-free (registry metadata resolves)", {
  expect_no_warning(new_ae_heatmap_block())
})

test_that("the registry offers it as ae_heatmap_block", {
  expect_true("ae_heatmap_block" %in% blockr.core::list_blocks())
})

test_that("the default script keeps the top_n values of the block's column", {
  sc <- ae_heatmap_script("CMDECOD")
  expect_match(sc, "top_n <- 25", fixed = TRUE)
  expect_match(sc, "dplyr::count(data, CMDECOD, sort = TRUE)", fixed = TRUE)
  d <- data.frame(CMDECOD = c("A", "A", "B", "C"), USUBJID = c(1, 2, 1, 3))
  env <- new.env()
  env$data <- d
  out <- eval(parse(text = sub("top_n <- 25[^\n]*", "top_n <- 1", sc)),
              envir = env)
  expect_identical(unique(out$CMDECOD), "A")
  # a name that is not syntactic is backticked, not broken
  expect_match(ae_heatmap_script("AE Term"), "`AE Term`", fixed = TRUE)
  expect_identical(ae_heatmap_script(character()), "")
})

test_that("the surface ships its title, sentence and script as state", {
  b <- new_ae_heatmap_block(col = "CMDECOD", top_n = 10)
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", b),
    {
      st <- session$returned$state
      expect_identical(st$title(), "Adverse events by subject")
      expect_identical(st$subtitle(), ae_heatmap_subtitle())
      expect_match(st$script(), "count(data, CMDECOD", fixed = TRUE)
      # the legacy argument lands as the script's value
      expect_identical(st$values()$top_n, 10L)
    },
    args = list(x = b, data = list(data = function() data.frame(
      USUBJID = c("s1", "s2"), CMDECOD = c("A", "B"), AETOXGR = c(1, 2)
    )))
  )
})

test_that("a saved script, or none, restores as saved", {
  b <- new_ae_heatmap_block(script = "")
  b2 <- blockr.core::blockr_deser(blockr.core::blockr_ser(b))
  expect_identical(class(b2), class(b))
})
