# Scale map: constructors, resolver, serialization. The "agreement fixture"
# test at the bottom pins the hash assignment of the cross-package scale-map
# convention (blockr.design/open/cdex-attribute-map) — every consumer package
# vendoring resolve_scales() ships the same fixture, so a drifted copy fails
# its own tests.

test_that("scale_binding validates channels", {
  b <- scale_binding("BOR", color = c(CR = "#006400", PD = "#8b0000"))
  expect_s3_class(b, "scale_binding")
  expect_named(b$color, c("CR", "PD"))

  expect_error(
    scale_binding("X", color = c(CR = "#1", "#2")),
    "fully named"
  )
  expect_error(
    scale_binding("X", color = c(CR = "#1", CR = "#2")),
    "duplicated"
  )
  expect_error(scale_binding(""), "nzchar")

  # shape coerced to integer, names kept
  b <- scale_binding("V", shape = c(EOT = 18, SCHEDULED = 19))
  expect_identical(b$shape, c(EOT = 18L, SCHEDULED = 19L))
})

test_that("new_scale_map flattens with later-wins by variable", {
  m <- new_scale_map(
    scale_binding("A", color = c(x = "#111111")),
    scale_binding("B", color = c(y = "#222222"))
  )
  m2 <- new_scale_map(
    m,
    scale_binding("A", color = c(x = "#999999"))
  )

  expect_identical(names(m2), c("A", "B"))
  expect_identical(m2$A$color, c(x = "#999999"))
  expect_identical(m2$B$color, c(y = "#222222"))

  # whole-binding replacement, no channel merge
  m3 <- new_scale_map(
    new_scale_map(scale_binding("A", color = c(x = "#1"), shape = c(x = 1))),
    scale_binding("A", color = c(x = "#2"))
  )
  expect_null(m3$A$shape)
})

test_that("as_scale_map normalizes deser shapes and rejects junk", {
  plain <- list(
    BOR = list(color = list(CR = "#006400", PD = "#8b0000")),
    POOL = list(color = list("#111111", "#222222")),
    BARE = list()
  )
  m <- as_scale_map(plain)

  expect_s3_class(m, "scale_map")
  expect_identical(m$BOR$color, c(CR = "#006400", PD = "#8b0000"))
  expect_identical(m$POOL$color, c("#111111", "#222222"))
  expect_length(m$BARE, 0L)

  expect_error(as_scale_map(list(list(color = "#1"))), "named")
  expect_error(as_scale_map(list(X = list(fill = "#1"))), "Unknown channel")
  expect_null(as_scale_map(NULL))
})

test_that("resolve_scales: fixed values, pool fallback, order", {
  m <- new_scale_map(
    scale_binding(
      "BOR",
      color = c(CR = "#006400", PR = "#FFD700", PD = "#8b0000")
    )
  )
  pal <- c("#aaaaaa", "#bbbbbb", "#cccccc")

  r <- resolve_scales(m, "BOR", levels = c("PD", "CR", "NEW"), palette = pal)

  # fixed beats palette; unbound level gets a palette color
  expect_identical(r$color[["PD"]], "#8b0000")
  expect_identical(r$color[["CR"]], "#006400")
  expect_true(r$color[["NEW"]] %in% pal)

  # order: fixed levels in binding order first, then the rest in input order
  expect_identical(r$order, c("CR", "PD", "NEW"))

  # unregistered variable -> NULL; empty levels -> NULL
  expect_null(resolve_scales(m, "NOPE", levels = "a"))
  expect_null(resolve_scales(m, "BOR", levels = character()))
  expect_null(resolve_scales(NULL, "BOR", levels = "CR"))
})

test_that("resolve_scales: hash assignment is stable across level subsets", {
  m <- new_scale_map(scale_binding("TRT"))
  pal <- c("#0072B2", "#D55E00", "#F0E442")

  all_lv <- resolve_scales(m, "TRT", levels = c("A", "B", "C"), palette = pal)
  one_lv <- resolve_scales(m, "TRT", levels = "B", palette = pal)

  expect_identical(all_lv$color[["B"]], one_lv$color[["B"]])
})

test_that("resolve_scales: shape has no fallback pool", {
  m <- new_scale_map(
    scale_binding("V", shape = c(EOT = 18L))
  )
  r <- resolve_scales(m, "V", levels = c("EOT", "WEEK 1"), palette = "#111111")

  expect_identical(r$shape, c(EOT = 18L))
  # color channel absent entirely: no fixed values, palette only applies to
  # a color channel... but bare-color bindings DO auto-assign:
  expect_true("WEEK 1" %in% names(r$color))
})

test_that("binding pool beats caller palette", {
  pool <- c("#101010", "#202020")
  m <- new_scale_map(scale_binding("ID", color = pool))
  r <- resolve_scales(m, "ID", levels = c("p1", "p2", "p3"),
                      palette = c("#aaaaaa"))
  expect_true(all(r$color %in% pool))
})

test_that("scale_map option round-trips through core JSON serdes", {
  opt <- new_scale_map_option(new_scale_map(
    default_clinical_map(),
    scale_binding("BEST_OVERALL_RESPONSE",
                  color = c(CR = "#111111", PD = "#222222")),
    scale_binding("USUBJID", color = c("#101010", "#202020"))
  ))

  ser <- blockr.core::blockr_ser(opt)
  json <- jsonlite::toJSON(ser, null = "null")
  back <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE,
                             simplifyMatrix = FALSE)
  opt2 <- blockr.core::blockr_deser(back)

  expect_identical(
    blockr.core::board_option_value(opt),
    blockr.core::board_option_value(opt2)
  )
  expect_identical(blockr.core::board_option_id(opt2), "scale_map")
})

test_that("derive_visit_type normalizes teal-style labels", {
  expect_identical(
    as.character(derive_visit_type(c(
      "BASELINE", "WEEK 4", "END OF TREATMENT (WEEK 12)", "EOT",
      "UNSCHEDULED 3.01", "Week 8", NA
    ))),
    c("BASELINE", "SCHEDULED", "EOT", "EOT", "UNSCHEDULED", "SCHEDULED", NA)
  )
  expect_identical(
    levels(derive_visit_type("x")),
    c("BASELINE", "SCHEDULED", "UNSCHEDULED", "EOT")
  )
})

test_that("default_clinical_map content", {
  m <- default_clinical_map()
  expect_true(all(
    c("BEST_OVERALL_RESPONSE", "AETOXGR", "AESEV", "VISIT_TYPE", "TRT") %in%
      names(m)
  ))
  expect_identical(m$VISIT_TYPE$shape[["EOT"]], 18L)
  # AE severity defaults match the gantt constants (behavior unchanged
  # without a study override)
  expect_identical(m$AESEV$color[["SEVERE"]], "#DC2626")
})

test_that("pp_sev_scale_colors resolves against patient adae", {
  adae <- data.frame(
    USUBJID = "x", AEDECOD = c("HEADACHE", "NAUSEA"),
    ASTDT = as.Date("2020-01-01") + 0:1,
    AESEV = c("MILD", "SEVERE")
  )
  dm_obj <- dm::dm(adae = adae)

  cols <- blockr.pharma:::pp_sev_scale_colors(default_clinical_map(), dm_obj)
  expect_identical(cols[["SEVERE"]], "#DC2626")
  expect_identical(cols[["MILD"]], "#CA8A04")

  expect_null(blockr.pharma:::pp_sev_scale_colors(NULL, dm_obj))
})

# --- scale-map convention agreement fixture ---------------------------------
# Identical in every package that vendors resolve_scales(). Do not edit
# without updating the convention (blockr.docs) and all copies.

test_that("AGREEMENT FIXTURE: hash assignment matches the convention", {
  pal <- c("#0072B2", "#D55E00", "#F0E442", "#009E73", "#56B4E9",
           "#E69F00", "#CC79A7")
  m <- new_scale_map(scale_binding("X"))
  r <- resolve_scales(
    m, "X",
    levels = c("CR", "PR", "Drug A", "Placebo", "WEEK 4", "01-701-1015"),
    palette = pal
  )

  expect_identical(r$color, c(
    "CR" = "#D55E00",
    "PR" = "#CC79A7",
    "Drug A" = "#56B4E9",
    "Placebo" = "#CC79A7",
    "WEEK 4" = "#56B4E9",
    "01-701-1015" = "#009E73"
  ))
})
