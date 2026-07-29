# Concomitant-medication indication coloring. This package owns the ROLE
# (which column) and the decision to color at all; blockr.theme owns the
# colors. No indication vocabulary and no hexes are defined here on purpose:
# CMINDC has no controlled terminology, so a level list shipped in this
# package would be one sponsor's spellings passing as a standard.

test_that("the indication role resolves, declared or by convention", {
  cols <- c("USUBJID", "CMTRT", "CMINDC", "REASON")
  expect_identical(pp_indc_column(cols, "REASON"), "REASON")
  expect_identical(pp_indc_column(cols, NULL), "CMINDC")
  # no CMINDC, undeclared: legitimately no indication (uniform bars)
  expect_null(pp_indc_column(c("USUBJID", "CMTRT")))

  err <- tryCatch(pp_indc_column(cols, "NOPE"), error = function(e) e)
  expect_s3_class(err, "pp_indc_var_missing")
  expect_match(conditionMessage(err), "NOPE")
  expect_match(conditionMessage(err), "sidebar")
})

test_that("pp_resolve_roles carries the indication, and the blocker raises", {
  dm_obj <- dm::dm(
    adsl = data.frame(USUBJID = "x", ACTARM = "Placebo",
                      stringsAsFactors = FALSE),
    adcm = data.frame(USUBJID = "x", CMTRT = "ASPIRIN",
                      CMINDC = "PROPHYLAXIS", stringsAsFactors = FALSE)
  )
  expect_identical(pp_resolve_roles(dm_obj)$indication, "CMINDC")
  expect_identical(
    pp_resolve_roles(dm_obj, list(indication = "CMTRT"))$indication, "CMTRT"
  )
  # a role that fails to resolve lands in $errors, never raised here
  roles <- pp_resolve_roles(dm_obj, list(indication = "NOPE"))
  expect_null(roles$indication)
  expect_s3_class(roles$errors$indication, "pp_indc_var_missing")

  expect_null(pp_roles_blocker(dm_obj, list(indication = "CMINDC")))
  blocker <- pp_roles_blocker(dm_obj, list(indication = "NOPE"))
  expect_error(eval(blocker, baseenv()), class = "pp_indc_var_missing")
})

indc_dm <- function(indc) {
  dm::dm(adcm = data.frame(
    USUBJID = "x", CMTRT = paste0("MED", seq_along(indc)),
    CMINDC = indc, stringsAsFactors = FALSE
  ))
}

test_that("colors come from the theme palette when the board pins nothing", {
  dm_obj <- indc_dm(c("Prophylaxis", "Adverse Event", "Prophylaxis"))
  cols <- pp_indc_scale_colors(NULL, dm_obj, "CMINDC")

  expect_named(cols, c("Prophylaxis", "Adverse Event"),
               ignore.order = TRUE)
  # every color is the theme's, none of this package's
  expect_true(all(cols %in% blockr.theme::theme_palette("categorical")))
  # ... and distinguishable, which is the whole point of coloring
  expect_length(unique(cols), 2L)

  # keyed by the level, not by its position: a patient carrying the
  # indications in the other order, or a different subset of them, still gets
  # the same color per level (the property the scale map exists for, and the
  # reason blockr.theme assigns the color rather than this package)
  other <- pp_indc_scale_colors(
    NULL, indc_dm(c("Adverse Event", "Other", "Prophylaxis")), "CMINDC"
  )
  expect_identical(other[["Adverse Event"]], cols[["Adverse Event"]])
  expect_identical(other[["Prophylaxis"]], cols[["Prophylaxis"]])
})

test_that("a board binding outranks the palette, and leads the legend", {
  dm_obj <- indc_dm(c("Prophylaxis", "Adverse Event", "HEADACHE"))
  map <- blockr.theme::new_scale_map(
    blockr.theme::scale_binding(
      "CMINDC", color = c("Adverse Event" = "#D55E00")
    )
  )

  cols <- pp_indc_scale_colors(map, dm_obj, "CMINDC")
  expect_identical(cols[["Adverse Event"]], "#D55E00")
  # the pinned level leads (resolve_scales() order), the rest follow from the
  # theme palette
  expect_identical(names(cols)[[1L]], "Adverse Event")
  expect_setequal(names(cols), c("Adverse Event", "Prophylaxis", "HEADACHE"))
})

test_that("coloring is skipped where it would carry no information", {
  # No column, no rows, nothing but blanks: nothing to color.
  expect_null(pp_indc_scale_colors(NULL, indc_dm("PROPHYLAXIS"), NULL))
  expect_null(pp_indc_scale_colors(NULL, indc_dm("PROPHYLAXIS"), "NOPE"))
  expect_null(pp_indc_scale_colors(NULL, indc_dm(c(NA, "", " ")), "CMINDC"))
  expect_null(pp_indc_scale_colors(NULL, dm::dm(adsl = data.frame(x = 1)),
                                   "CMINDC"))

  # One indication and some blanks: coloring would only paint the blanks grey,
  # which reads as a finding and is not one.
  expect_null(pp_indc_scale_colors(
    NULL, indc_dm(c("PROPHYLAXIS", NA, "PROPHYLAXIS")), "CMINDC"
  ))

  # More distinct indications than the palette holds -- the verbatim-condition
  # study, where every medication has its own reason. Repeating hues over
  # forty levels says less than one honest color.
  many <- paste("CONDITION", seq_len(length(
    blockr.theme::theme_palette("categorical")
  ) + 1L))
  expect_null(pp_indc_scale_colors(NULL, indc_dm(many), "CMINDC"))
})

test_that("the legend draws the resolved vector, in its order", {
  cols <- c(Prophylaxis = "#0072B2", "Adverse Event" = "#D55E00")
  html <- as.character(pp_indc_legend_ui(cols))
  expect_match(html, "#0072B2", fixed = TRUE)
  # the study's own wording, sentence-cased like every other panel term
  expect_match(html, "Adverse event", fixed = TRUE)
  expect_lt(
    regexpr("Prophylaxis", html, fixed = TRUE),
    regexpr("Adverse event", html, fixed = TRUE)
  )

  # uniform bars -> no legend, or it would claim a distinction not drawn
  expect_null(pp_indc_legend_ui(NULL))
  expect_null(pp_indc_legend_ui(character()))
})
