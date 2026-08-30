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
