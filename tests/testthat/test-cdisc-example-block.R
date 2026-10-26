test_that("cdisc example block constructor creates correct class hierarchy", {
  block <- new_cdisc_example_block()
  expect_s3_class(
    block,
    c("cdisc_example_block", "dm_block", "data_block", "block")
  )
})

test_that("cdisc example block has no render trigger", {
  expect_null(block_render_trigger.cdisc_example_block(
    new_cdisc_example_block(),
    session = NULL
  ))
})

test_that("cdisc_example_choices offers only the three CDISC ids", {
  choices <- cdisc_example_choices()
  expect_true(all(
    choices %in% c("pharmaverseadam", "pharmaversesdtm", "safetydata_adam")
  ))
})

test_that("pharmaverseadam_expr produces a keyed dm", {
  skip_if_not_installed("pharmaverseadam")
  result <- eval(pharmaverseadam_expr())

  expect_s3_class(result, "dm")
  expect_equal(
    sort(names(dm::dm_get_tables(result))),
    c("adae", "adcm", "adeg", "adex", "adlb", "adsl", "advs")
  )
  expect_true("adsl" %in% dm::dm_get_all_pks(result)$table)
  expect_true(all(dm::dm_get_all_fks(result)$parent_table == "adsl"))
})

test_that("pharmaversesdtm_expr produces a dm keyed on the DM domain", {
  skip_if_not_installed("pharmaversesdtm")
  result <- eval(pharmaversesdtm_expr())

  expect_s3_class(result, "dm")
  expect_equal(
    sort(names(dm::dm_get_tables(result))),
    c("ae", "cm", "dm", "eg", "ex", "lb", "vs")
  )
  expect_true("dm" %in% dm::dm_get_all_pks(result)$table)
  expect_true(all(dm::dm_get_all_fks(result)$parent_table == "dm"))
})

test_that("safetydata_adam_expr produces a keyed dm", {
  skip_if_not_installed("safetyData")
  result <- eval(safetydata_adam_expr())

  expect_s3_class(result, "dm")
  tables <- names(dm::dm_get_tables(result))
  expect_length(tables, 10L)
  expect_true("adsl" %in% dm::dm_get_all_pks(result)$table)
  expect_true(all(dm::dm_get_all_fks(result)$parent_table == "adsl"))
})

test_that("cdisc_example_expr errors on unknown id", {
  expect_error(cdisc_example_expr("bi_star_schema"), "Unknown CDISC example")
})

test_that("cdisc example block returns the selected dm", {
  skip_if_not_installed("safetyData")
  block <- new_cdisc_example_block(dataset = "safetydata_adam")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "dm")
      expect_true("adsl" %in% names(dm::dm_get_tables(result)))
    },
    args = list(x = block)
  )
})

test_that("cdisc example feeds the cdisc dm block", {
  skip_if_not_installed("pharmaverseadam")
  dm_input <- eval(cdisc_example_expr("pharmaverseadam"))
  block <- new_cdisc_dm_block()

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "dm")
      expect_true("adsl" %in% dm::dm_get_all_pks(result)$table)
    },
    args = list(x = block, data = list(data = function() dm_input))
  )
})

test_that("both blocks are registered under blockr.pharma", {
  reg <- blockr.core::available_blocks()
  for (uid in c("cdisc_dm_block", "cdisc_example_block")) {
    expect_true(uid %in% names(reg))
    expect_identical(attr(reg[[uid]], "package"), "blockr.pharma")
  }
})
