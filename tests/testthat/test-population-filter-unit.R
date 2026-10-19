# The population filter counts patients. The generic crossfilter in blockr.dm
# counts rows; the word is set here, where the subject table is known to be
# one row a patient.

test_that("the population filter's header counts patients", {
  adsl <- data.frame(USUBJID = c("a", "b", "c"), SEX = c("F", "M", "F"))
  adae <- data.frame(USUBJID = c("a", "b"), AESEV = c("MILD", "SEVERE"))
  d <- dm::dm(adsl = adsl, adae = adae) |>
    dm::dm_add_pk(adsl, USUBJID) |>
    dm::dm_add_fk(adae, USUBJID, adsl)
  blk <- new_population_filter_block(
    active_dims = list(adsl = "SEX", adae = "AESEV")
  )

  # the unit is an option of the block; the client passes it on with the
  # payload request
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() d)),
    {
      sent <- list()
      root <- session$rootScope()
      root$sendInputMessage <- function(inputId, message) {
        sent[[length(sent) + 1L]] <<- message
        invisible()
      }
      session$setInputs(`expr-block` = list(messages = list(list(type = "ready"))))
      session$flushReact()
      expect_identical(sent[[1L]]$context$options$subject_unit, "patients")
    }
  )
  msg <- blockr.dm::crossfilter_payload(
    d, list(active_dims = list(adsl = "SEX", adae = "AESEV")),
    subject_unit = "patients"
  )
  expect_identical(msg$subject_unit, "patients")
  expect_identical(msg$parent_n, 3L)
})
