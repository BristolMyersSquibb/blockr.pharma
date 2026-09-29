# The patient profile's stylesheet reads the design system's meaning tokens
# (blockr.ui, vignettes/articles/design-system.Rmd, "Token grammar"):
# `--blockr-color-<property>-<role>`, the type, radius, control, shadow,
# focus and transition tokens. Never a palette token (`--blockr-grey-300`),
# which only other tokens read, and never a legacy alias. No fallbacks
# either: pp_block_ui() loads blockr.ui::controls_dep(), which brings the
# token sheet, so a fallback would be a second copy of a value that can
# drift.

pp_css <- function() {
  readLines(
    system.file("assets", "css", "patient-profile.css", package = "blockr.pharma")
  )
}

pp_css_tokens <- function() {
  css <- pp_css()
  css <- css[!grepl("^\\s*(/\\*|\\*)", css)]
  unlist(regmatches(css, gregexpr("var\\(--blockr-[^)]*\\)", css)))
}

# The tokens the design system defines, from blockr.ui's light sheet.
ui_tokens <- function() {
  path <- system.file("assets", "css", "blockr-tokens.css", package = "blockr.ui")
  lines <- readLines(path)
  unique(sub(":.*$", "",
             regmatches(lines, regexpr("--blockr-[a-z0-9-]+(?=:)", lines,
                                       perl = TRUE))))
}

test_that("the stylesheet reads tokens without fallbacks", {
  refs <- pp_css_tokens()
  expect_gt(length(refs), 100)
  expect_identical(grep(",", refs, value = TRUE), character())
})

test_that("every token the stylesheet reads is a meaning token blockr.ui defines", {
  names <- unique(sub("^var\\((--blockr-[a-z0-9-]+)\\)$", "\\1", pp_css_tokens()))

  palette <- grep("^--blockr-(grey|blue|red|amber|green|accent)-[0-9]+$",
                  names, value = TRUE)
  expect_identical(palette, character(), info = "palette tokens are for tokens")

  legacy <- intersect(names, c(
    "--blockr-color-text-primary", "--blockr-color-text-secondary",
    "--blockr-color-text-subtle", "--blockr-color-text-meta",
    "--blockr-color-border", "--blockr-color-border-hover",
    "--blockr-color-bg-input", "--blockr-color-primary",
    "--blockr-color-primary-hover", "--blockr-color-primary-bg"
  ))
  expect_identical(legacy, character(), info = "legacy aliases")

  # Local tokens are the profile's own, defined in its stylesheet.
  local <- grep("^--blockr-pharma-", names, value = TRUE)
  defined <- unlist(regmatches(pp_css(), regexpr("--blockr-pharma-[a-z0-9-]+(?=:)",
                                                 pp_css(), perl = TRUE)))
  expect_identical(setdiff(local, defined), character(),
                   info = "local tokens the stylesheet does not define")
  expect_identical(setdiff(setdiff(names, local), ui_tokens()), character(),
                   info = "tokens blockr.ui does not define")
})

test_that("the stylesheet keeps no grey, blue or white literal a token covers", {
  css <- pp_css()
  decl <- grep("^\\s*[a-z-]+\\s*:", css, value = TRUE)
  bare <- tolower(unlist(regmatches(decl, gregexpr("#[0-9a-fA-F]{3,8}\\b", decl))))
  covered <- c("#fff", "#ffffff",
               "#f9fafb", "#f3f4f6", "#e5e7eb", "#d1d5db", "#9ca3af", "#6b7280",
               "#4b5563", "#374151", "#1f2937", "#111827",
               "#eff6ff", "#dbeafe", "#3b82f6", "#2563eb", "#1d4ed8")
  expect_identical(intersect(bare, covered), character())
})

test_that("the block draws with blockr.ui's controls and tokens", {
  deps <- htmltools::findDependencies(pp_block_ui("pp"))
  names <- vapply(deps, `[[`, character(1L), "name")
  expect_true(all(c("blockr-tokens", "blockr-ui-js", "blockr-blocks-css") %in% names))
})
