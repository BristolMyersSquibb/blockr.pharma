ae_toy <- function() {
  data.frame(
    USUBJID = c("01", "01", "02"),
    AEBODSYS = c("Cardiac disorders", "Cardiac disorders", "Skin disorders"),
    AESOC = c("Cardiac", "Cardiac", "Skin"),
    AEDECOD = factor(c("Palpitations", "Tachycardia", "Rash")),
    AELLT = c("Heart racing", "Fast pulse", "Skin rash"),
    stringsAsFactors = FALSE
  )
}

test_that("choices are labels to columns, defaults first", {
  high <- ae_term_choices("high")
  expect_identical(unname(high[1]), "AEBODSYS")
  expect_identical(names(high)[1], "Body System or Organ Class")
  low <- ae_term_choices("low")
  expect_identical(unname(low[1]), "AEDECOD")
  expect_identical(names(low)[1], "Dictionary-Derived Term")
})

test_that("choices keep only the columns the data has", {
  d <- ae_toy()
  expect_identical(unname(ae_term_choices("high", d)), c("AEBODSYS", "AESOC"))
  expect_identical(unname(ae_term_choices("low", d)), c("AEDECOD", "AELLT"))
})

test_that("choices fall back to the full list without any term column", {
  d <- data.frame(USUBJID = "01")
  expect_identical(ae_term_choices("high", d), ae_term_choices("high"))
  expect_identical(ae_term_choices("low", NULL), ae_term_choices("low"))
})

test_that("columns copy the picks and carry the label", {
  d <- ae_term_columns(ae_toy(), "Primary System Organ Class",
                       "Lowest Level Term")
  expect_identical(as.vector(d$SOC), ae_toy()$AESOC)
  expect_identical(as.vector(d$PT), ae_toy()$AELLT)
  expect_identical(attr(d$SOC, "label"), "Primary System Organ Class")
  expect_identical(attr(d$PT, "label"), "Lowest Level Term")
})

test_that("a column name or a factor pick works like a label", {
  d1 <- ae_term_columns(ae_toy(), "AESOC", factor("AEDECOD"))
  expect_identical(attr(d1$SOC, "label"), "Primary System Organ Class")
  expect_identical(as.vector(d1$PT), as.character(ae_toy()$AEDECOD))
  expect_false(is.factor(d1$PT))
})

test_that("a pick the data lacks falls back to the first column it has", {
  d <- ae_term_columns(ae_toy(), "High Level Term", "Reported Term")
  expect_identical(as.vector(d$SOC), ae_toy()$AEBODSYS)
  expect_identical(attr(d$PT, "label"), "Dictionary-Derived Term")
})

test_that("NULL leaves a level out, and other names can be written", {
  d <- ae_term_columns(ae_toy(), NULL, "Dictionary-Derived Term",
                       names = c(high = "HIGH", low = "LOW"))
  expect_false("HIGH" %in% names(d))
  expect_false("SOC" %in% names(d))
  expect_true("LOW" %in% names(d))
})

test_that("data without a term column comes back unchanged", {
  d <- data.frame(USUBJID = "01")
  expect_identical(ae_term_columns(d, "AEBODSYS", "AEDECOD"), d)
})

test_that("a zero-row frame keeps its shape", {
  d <- ae_term_columns(ae_toy()[0, ], "AEBODSYS", "AEDECOD")
  expect_identical(nrow(d), 0L)
  expect_true(all(c("SOC", "PT") %in% names(d)))
})
