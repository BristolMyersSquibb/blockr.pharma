arms <- c("Placebo", "Low", "High")

make_arm_dm <- function() {
  adsl <- data.frame(
    USUBJID = sprintf("S%02d", 1:9),
    TRT = rep(arms, each = 3),
    AGE = c(60, 62, 64, 70, 72, 74, 80, 82, 84),
    stringsAsFactors = FALSE
  )
  ae <- data.frame(
    USUBJID = c("S01", "S04", "S07", "S07"),
    AEDECOD = c("Headache", "Nausea", "Rash", "Nausea"),
    stringsAsFactors = FALSE
  )
  dm::dm_add_fk(
    dm::dm_add_pk(dm::dm(adsl = adsl, ae = ae), adsl, USUBJID),
    ae, USUBJID, adsl
  )
}

overlapping <- list(
  show = c("Placebo", "High"),
  pools = list(list(name = "All active", members = c("Low", "High")))
)

test_that("an untouched definition stamps a plain copy", {
  x <- c("Placebo", "Low", "High")
  expect_null(attr(stamp_group(x, "TRT"), "blockr_groups"))
  # Every level shown, in level order, no pools: the same as no definition.
  out <- stamp_group(x, "TRT", list(show = sort(arms), pools = list()))
  expect_null(attr(out, "blockr_groups"))
  expect_equal(as.vector(out), x)
  expect_equal(attr(out, "blockr_source"), "TRT")
})

test_that("a partition writes the group name, NA for a dropped level", {
  x <- c("Placebo", "Low", "High", "Low")
  out <- stamp_group(x, "TRT", list(
    show = "Placebo",
    pools = list(list(name = "All active", members = c("Low", "High")))
  ))
  expect_equal(as.vector(out), c("Placebo", "All active", "All active", "All active"))
  def <- attr(out, "blockr_groups")
  expect_false(def$overlap)
  expect_equal(names(def$groups), c("Placebo", "All active"))

  dropped <- stamp_group(x, "TRT", list(show = c("Placebo", "High")))
  expect_equal(as.vector(dropped), c("Placebo", NA, "High", NA))
})

test_that("overlapping groups keep the raw level and carry the definition", {
  x <- c("Placebo", "Low", "High")
  out <- stamp_group(x, "TRT", overlapping)
  expect_equal(as.vector(out), x)
  def <- attr(out, "blockr_groups")
  expect_true(def$overlap)
  expect_equal(
    def$groups,
    list(Placebo = "Placebo", High = "High", "All active" = c("Low", "High"))
  )
  expect_equal(attr(out, "blockr_source"), "TRT")
})

test_that("a pool with no member present is left out, a clashing name is numbered", {
  x <- c("Placebo", "Low")
  out <- stamp_group(x, "TRT", list(
    pools = list(list(name = "Empty", members = "Nothing"))
  ))
  expect_null(attr(out, "blockr_groups"))

  out <- stamp_group(x, "TRT", list(
    show = "Placebo",
    pools = list(list(name = "Placebo", members = "Low"))
  ))
  expect_equal(names(attr(out, "blockr_groups")$groups), c("Placebo", "Placebo 2"))
})

test_that("the definition lands on the parent table and survives a flatten", {
  d <- dm_stamp_group(make_arm_dm(), "TRT", groups = overlapping)
  expect_false("Group" %in% colnames(d$ae))
  expect_true(attr(d$adsl$Group, "blockr_groups")$overlap)

  flat <- dm::dm_flatten_to_tbl(d, ae)
  expect_true(attr(flat$Group, "blockr_groups")$overlap)

  filtered <- dplyr::filter(as.data.frame(flat), AEDECOD == "Nausea")
  expect_true(attr(filtered$Group, "blockr_groups")$overlap)
})

test_that("group_by_args follows the definition", {
  plain <- data.frame(Group = c("B", "A", NA, "B"))
  expect_equal(group_by_args(plain), list(variable = "Group", levels = c("A", "B")))

  part <- data.frame(USUBJID = 1:3)
  part$Group <- stamp_group(c("Placebo", "Low", "High"), "TRT", list(
    show = "Placebo",
    pools = list(list(name = "All active", members = c("Low", "High")))
  ))
  expect_equal(
    group_by_args(part),
    list(variable = "Group", levels = c("Placebo", "All active"))
  )

  over <- data.frame(USUBJID = 1:3)
  over$Group <- stamp_group(c("Placebo", "Low", "High"), "TRT", overlapping)
  expect_equal(
    group_by_args(over),
    list(
      variable = "Group",
      levels = c("Placebo", "High", "All active"),
      pools = list("All active" = c("Low", "High"))
    )
  )
})

test_that("a composer table over overlapping groups counts each column right", {
  skip_if_not_installed("composer")
  adsl <- dm::dm_get_tables(
    dm_stamp_group(make_arm_dm(), "TRT", groups = overlapping)
  )$adsl
  adsl <- as.data.frame(adsl)

  tbl <- composer::table(
    title = "x", population = "All", data = adsl,
    denominator = composer::make_denom(adsl)
  ) |>
    composer::colgroup(do.call(composer::by, group_by_args(adsl))) |>
    composer::block_continuous(
      label = "Age", variable = "AGE", statistic = c("{N:xx}", "{mean:xx.x}")
    ) |>
    composer::compose()

  d <- tbl[["panes"]][[1]]$data
  n_row <- d[trimws(d$label) == "N", ]
  expect_equal(trimws(n_row[["Placebo"]]), "3")
  expect_equal(trimws(n_row[["High"]]), "3")
  expect_equal(trimws(n_row[["All active"]]), "6")
  mean_row <- d[trimws(d$label) == "Mean", ]
  expect_equal(trimws(mean_row[["All active"]]), "77.0")
})

test_that("the block carries its groups state into the stamp", {
  blk <- new_population_filter_block(
    featured = "TRT",
    pinned = "TRT",
    groups = list(TRT = list(
      show = list("Placebo", "High"),
      pools = list(list(
        name = "All active", members = list("Low", "High"), custom = FALSE
      ))
    ))
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() make_arm_dm())),
    {
      session$flushReact()
      txt <- paste(
        deparse(session$returned$expr(), width.cutoff = 500L),
        collapse = ""
      )
      expect_match(txt, "groups = list(", fixed = TRUE)
      expect_no_match(txt, "custom", fixed = TRUE)

      result <- eval(session$returned$expr(), list(data = make_arm_dm()))
      def <- attr(result$adsl$Group, "blockr_groups")
      expect_true(def$overlap)
      expect_equal(names(def$groups), c("Placebo", "High", "All active"))
    }
  )
})
