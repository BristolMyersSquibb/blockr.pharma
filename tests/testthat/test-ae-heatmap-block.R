# new_ae_heatmap_block() is deprecated: boards saved with it restore as
# blockr.viz table blocks, and code that calls it gets a warning. New boards build the table block
# with ae_heatmap_script() and ae_heatmap_subtitle(); heatmap_table() below
# is that call.

heatmap_table <- function(...) {
  blockr.viz::new_table_block(
    layout = "groups",
    by = c("TRT01A", "USUBJID"),
    summaries = list(list(
      type = "simple", func = "count", show = "tile",
      facet = "AEDECOD", fill = "AETOXGR", name = "Events"
    )),
    look = "compact",
    drill = "auto",
    drill_target = "auto",
    title = "Adverse events by subject",
    subtitle = ae_heatmap_subtitle(),
    script = ae_heatmap_script("AEDECOD"),
    script_values = list(top_terms = 25L),
    ...
  )
}

test_that("the old constructor is exported and warns when code calls it", {
  expect_true("new_ae_heatmap_block" %in%
                getNamespaceExports("blockr.pharma"))
  expect_warning(blk <- blockr.pharma::new_ae_heatmap_block(),
                 class = "deprecatedWarning")
  expect_s3_class(blk, "table_block")
  expect_no_warning(
    new_ae_heatmap_block(ctor = "new_ae_heatmap_block",
                         ctor_pkg = "blockr.pharma")
  )
})

test_that("a restore gives the table a board builds by hand", {
  ser <- function(b) blockr.core::blockr_ser(b)$payload
  restore <- function(...) {
    new_ae_heatmap_block(..., ctor = "new_ae_heatmap_block",
                         ctor_pkg = "blockr.pharma")
  }
  old <- restore(group = "TRT01A", download = TRUE)
  expect_s3_class(old, "table_block")
  expect_identical(ser(old), ser(heatmap_table()))
  # The ConMed heatmap: no paint column, cells painted by count.
  cm <- ser(restore(col = "CMDECOD", color = character()))
  expect_identical(cm$summaries[[1L]]$facet, "CMDECOD")
  expect_null(cm$summaries[[1L]]$fill)
})

test_that("an old board restores as a table block", {
  # What a board saved with the matrix engine holds for this block.
  old <- list(
    object = c("ae_heatmap_block", "heatmap_block", "transform_block",
               "block"),
    payload = list(row = "USUBJID", col = "AEDECOD", color = "AETOXGR",
                   group = "Group", top_n = 10L, cell_numbers = TRUE,
                   drill = NULL, download = TRUE, filter_column = NULL,
                   filter_values = NULL, max_height = NULL,
                   ctrl_target = NULL, ctrl_table = NULL,
                   title = "Adverse events by subject",
                   subtitle = "[Top {@top_n} ]{label(@col)}[, painted by {label(@color)}]",
                   caption = NULL, script = NULL, values = list(),
                   locked_roles = c("color", "group"),
                   block_name = "AE Heatmap"),
    constructor = list(object = "blockr_ctor",
                       constructor = "new_ae_heatmap_block",
                       package = "blockr.pharma", version = "0.1.0")
  )
  b <- tryCatch(blockr.core::blockr_deser(old),
                block_deser_class_error = function(e) {
                  skip("blockr.core without the relaxed class check (#379)")
                })
  expect_s3_class(b, "table_block")
  p <- blockr.core::blockr_ser(b)$payload
  expect_identical(p$by, c("Group", "USUBJID"))
  expect_equal(p$script_values$top_terms, 10)
  expect_match(p$subtitle, "{@top_terms}", fixed = TRUE)
  expect_true(p$download)
  expect_match(p$subtitle, "@facet", fixed = TRUE)
  expect_match(p$subtitle, "@fill", fixed = TRUE)
  expect_identical(p$block_name, "AE Heatmap")
})

test_that("a production board's heatmap restores with the dock's names", {
  # As the production board saved it: blockr.dock adds `visible`, which is
  # not an argument and comes back as an attribute.
  saved <- list(
    object = c("ae_heatmap_block", "heatmap_block", "transform_block",
               "block", "vctrs_vctr", "list"),
    payload = list(row = "USUBJID", col = "AEDECOD", color = "AETOXGR",
                   group = "Group", top_n = NULL, cell_numbers = TRUE,
                   drill = NULL, download = TRUE, filter_column = NULL,
                   filter_values = NULL, max_height = NULL,
                   ctrl_target = NULL, ctrl_table = NULL,
                   title = "Adverse events by subject",
                   subtitle = paste0("[Top {@top_n} ]{label(@col)}[, painted by ",
                                     "worst {label(@color)}][, by {label(@group)}]"),
                   caption = NULL,
                   script = paste(
                     "top_n <- 25  #| number(min = 1, max = 200)",
                     "top <- dplyr::count(data, AEDECOD, sort = TRUE)",
                     "dplyr::filter(data, AEDECOD %in% utils::head(top$AEDECOD, top_n))",
                     sep = "\n"),
                   values = list(), block_name = "AE Heatmap",
                   visible = "inputs"),
    constructor = list(object = "blockr_ctor",
                       constructor = "new_ae_heatmap_block",
                       package = "blockr.pharma", version = "0.0.0.9108")
  )
  b <- tryCatch(blockr.core::blockr_deser(saved),
                block_deser_class_error = function(e) {
                  skip("blockr.core without the relaxed class check (#379)")
                })
  expect_s3_class(b, "table_block")
  expect_identical(attr(b, "visible"), "inputs")
  # The old default script is written anew with its control renamed, and
  # the sentence follows: the arm is the outer `by`, the grade is "worst".
  p <- blockr.core::blockr_ser(b)$payload
  expect_match(p$script, "^top_terms <- 25")
  expect_equal(p$script_values$top_terms, 25)
  expect_null(p$script_values$top_n)
  expect_identical(
    p$subtitle,
    "[Top {@top_terms} ]{label(@facet)}[, painted by worst {label(@fill)}][, by {label(@parent)}]"
  )
})

test_that("the default script keeps the top_n values of the block's column", {
  sc <- ae_heatmap_script("CMDECOD")
  expect_match(sc, "top_terms <- 25", fixed = TRUE)
  expect_match(sc, "facet <- \"CMDECOD\"  #| setting", fixed = TRUE)
  d <- data.frame(CMDECOD = c("A", "A", "B", "C"), USUBJID = c(1, 2, 1, 3))
  env <- new.env()
  env$data <- d
  out <- eval(parse(text = sub("top_terms <- 25[^\n]*", "top_terms <- 1", sc)),
              envir = env)
  expect_identical(unique(out$CMDECOD), "A")
  # a name that is not syntactic is a string, not broken
  expect_match(ae_heatmap_script("AE Term"), "facet <- \"AE Term\"",
               fixed = TRUE)
  expect_identical(ae_heatmap_script(character()), "")
})

test_that("the sentence names the table's facet and fill", {
  expect_match(ae_heatmap_subtitle(), "{label(@facet)}", fixed = TRUE)
  expect_match(ae_heatmap_subtitle(), "painted by worst {label(@fill)}",
               fixed = TRUE)
})

test_that("the sentence reads the cap, the grade and the arm", {
  viz <- asNamespace("blockr.viz")
  b <- heatmap_table()
  st <- blockr.core:::initial_block_state(b)
  v <- lapply(st, function(x) if (shiny::is.reactive(x)) x() else x)
  args <- viz$script_title_args(
    viz$table_groups_title_args(viz$viz_block_spec("table"), v),
    viz$cb_specs(viz$cb_parse(v$script)), v$script_values
  )
  d <- data.frame(TRT01A = "Placebo", USUBJID = "01", AEDECOD = "RASH",
                  AETOXGR = "1")
  attr(d$TRT01A, "label") <- "Actual Treatment"
  attr(d$AEDECOD, "label") <- "Dictionary-Derived Term"
  expect_identical(
    viz$viz_titles(v, d, list(), args)$subtitle$text,
    "Top 25 Dictionary-Derived Term, painted by worst AETOXGR, by Actual Treatment"
  )
})

test_that("the heatmap computes: worst grade paints, rows by burden", {
  d <- data.frame(
    TRT01A = c("A", "A", "A", "B"),
    USUBJID = c("s1", "s1", "s2", "s3"),
    AEDECOD = c("RASH", "RASH", "COUGH", "RASH"),
    AETOXGR = c(1, 3, 2, 1)
  )
  b <- heatmap_table()
  shiny::testServer(b$expr_server, args = list(data = shiny::reactive(d)), {
    session$flushReact()
    expect_identical(session$returned$state$layout(), "groups")
  })
  p <- blockr.viz:::groups_prepare_summaries(
    d, by = c("TRT01A", "USUBJID"),
    summaries = blockr.core::blockr_ser(b)$payload$summaries
  )
  expect_null(p$err)
  s1 <- p$rows[p$rows$.label == "s1", ]
  rash <- which(vapply(p$plan, function(x) x$label, "") == "RASH")
  expect_identical(s1[[p$plan[[rash]]$key]], 2)
  expect_identical(s1[[p$plan[[rash]]$wkey]], 3L)  # grade 3, the worst
})

test_that("only a restored heatmap tells the user it is deprecated", {
  said <- character()
  local_mocked_bindings(
    get_session = function(...) structure(list(), class = "ShinySession"),
    notify = function(msg, ...) said <<- c(said, msg),
    .package = "blockr.core"
  )
  suppressWarnings(new_ae_heatmap_block())
  expect_length(said, 0L)
  new_ae_heatmap_block(ctor = "new_ae_heatmap_block", ctor_pkg = "blockr.pharma")
  expect_length(said, 1L)
  expect_match(said, "older version")
})
