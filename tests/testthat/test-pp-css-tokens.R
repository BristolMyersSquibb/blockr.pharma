# The patient profile's stylesheet references the design system's semantic
# tokens with a literal fallback: `var(--blockr-color-border, #e5e7eb)`. The
# fallback is what renders when the token sheet is not on the page, so it
# must equal the token's value; a drifted fallback is a bug the eye will not
# catch (blockr.docs/design-system/README.md, the token contract).

pp_css_fallbacks <- function() {
  css <- readLines(
    system.file("assets", "css", "patient-profile.css", package = "blockr.pharma")
  )
  m <- regmatches(css, gregexpr("var\\(--blockr-[a-z0-9-]+,\\s*#[0-9a-fA-F]{3,8}\\)", css))
  hits <- unlist(m)
  data.frame(
    token = sub("^var\\((--[a-z0-9-]+),.*$", "\\1", hits),
    fallback = tolower(sub("^.*,\\s*(#[0-9a-fA-F]+)\\)$", "\\1", hits)),
    stringsAsFactors = FALSE
  )
}

# The token sheet as a named vector, `var()` references resolved.
blockr_tokens <- function() {
  path <- system.file("assets", "css", "blockr-tokens.css", package = "blockr.ui")
  if (!nzchar(path)) return(NULL)
  lines <- readLines(path)
  m <- regmatches(lines, regexpr("--blockr-[a-z0-9-]+:\\s*[^;]+", lines))
  vals <- setNames(
    trimws(sub("^--[a-z0-9-]+:\\s*", "", m)),
    sub(":.*$", "", m)
  )
  for (i in seq_len(5)) {
    ref <- grepl("^var\\(", vals)
    if (!any(ref)) break
    vals[ref] <- vals[sub("^var\\((--[a-z0-9-]+)\\)$", "\\1", vals[ref])]
  }
  tolower(vals)
}

test_that("every token fallback in the stylesheet equals the token's value", {
  skip_if_not_installed("blockr.ui")
  tokens <- blockr_tokens()
  skip_if(is.null(tokens), "blockr.ui ships no token sheet")

  fb <- pp_css_fallbacks()
  expect_gt(nrow(fb), 100)
  fb <- unique(fb)

  unknown <- setdiff(fb$token, names(tokens))
  expect_identical(unknown, character(), info = "tokens the sheet does not define")

  known <- fb[fb$token %in% names(tokens), ]
  drift <- known[known$fallback != tokens[known$token], ]
  expect_identical(
    nrow(drift), 0L,
    info = paste(sprintf("%s falls back to %s, token is %s",
                         drift$token, drift$fallback, tokens[drift$token]),
                 collapse = "\n")
  )
})

test_that("the stylesheet keeps no grey or blue literal a token covers", {
  css <- readLines(
    system.file("assets", "css", "patient-profile.css", package = "blockr.pharma")
  )
  # Declarations only, with any var(..., #fallback) removed first.
  decl <- grep("^\\s*[a-z-]+\\s*:", css, value = TRUE)
  decl <- gsub("var\\(--blockr-[a-z0-9-]+,\\s*#[0-9a-fA-F]{3,8}\\)", "", decl)
  bare <- tolower(unlist(regmatches(decl, gregexpr("#[0-9a-fA-F]{3,8}\\b", decl))))
  covered <- c("#f9fafb", "#f3f4f6", "#e5e7eb", "#d1d5db", "#9ca3af", "#6b7280",
               "#4b5563", "#374151", "#1f2937", "#111827",
               "#eff6ff", "#dbeafe", "#3b82f6", "#2563eb", "#1d4ed8")
  expect_identical(intersect(bare, covered), character())
})
