# The AE records the patient timeline hands to the table block's data view
# (R/patient-timeline-records.R).

ptr_toy <- function() {
  adsl <- data.frame(
    USUBJID = c("A", "B", "C"),
    ACTARM = c("Drug", "Placebo", "Drug"),
    TRTSDT = as.Date("2024-01-01"),
    TRTEDT = as.Date(c("2024-02-29", "2024-01-20", NA)),
    LSTALVDT = as.Date(c("2024-03-30", "2024-01-25", "2024-04-30")),
    stringsAsFactors = FALSE
  )
  adae <- data.frame(
    USUBJID = c("A", "A", "A", "B", "Z"),
    AESEQ = c(1, 2, 3, 1, 1),
    AEDECOD = c("DIARRHOEA", "DIARRHOEA", "DIZZINESS", "FALL", "RASH"),
    AETERM = c("diarrhoea", "loose stools", "dizziness", "fall", "rash"),
    AEBODSYS = "SOC",
    AESEV = c("MILD", "MODERATE", "SEVERE", "MILD", "MILD"),
    ATOXGR = c("1", "2", "3", "1", "1"),
    AEREL = c("POSSIBLE", "REMOTE", "NONE", "NONE", "NONE"),
    AESER = c("N", "N", "Y", "N", "N"),
    TRTEMFL = c("Y", "Y", "Y", "N", "Y"),
    ASTDY = c(22, 35, 61, 3, 5),
    AENDY = c(40, 48, NA, 4, 6),
    ASTDT = as.Date("2024-01-01") + c(21, 34, 60, 2, 4),
    AENDT = as.Date("2024-01-01") + c(39, 47, NA, 3, 5),
    AEOUT = "RECOVERED",
    stringsAsFactors = FALSE
  )
  list(adsl = adsl, adae = adae)
}

ptr_records <- function(...) {
  f <- patient_timeline_frame(ptr_toy(), ...)
  attr(f, "viz_records", exact = TRUE)
}

test_that("the frame hands the drawn AE records with the listing columns", {
  r <- ptr_records()
  expect_identical(r$label, "Adverse events")
  expect_identical(
    names(r$data),
    c("USUBJID", "AESEQ", "ASTDY", "AENDY", "AETERM", "ATOXGR", "AEREL",
      "AESER", "ASTDT", "AENDT")
  )
  # Treatment-emergent only, and only the patients ADSL holds.
  expect_identical(r$data$USUBJID, c("A", "A", "A"))
  expect_identical(r$data$AESEQ, c(1, 2, 3))
  expect_identical(r$data$AETERM, c("diarrhoea", "loose stools", "dizziness"))
})

test_that("the records read by patient, start day and sequence", {
  d <- ptr_toy()
  d$adae <- d$adae[rev(seq_len(nrow(d$adae))), ]
  d$adae$ASTDY[d$adae$AESEQ == 2 & d$adae$USUBJID == "A"] <- 22
  r <- attr(patient_timeline_frame(d), "viz_records", exact = TRUE)
  expect_identical(r$data$AESEQ, c(1, 2, 3))
  expect_identical(rownames(r$data), c("1", "2", "3"))
})

test_that("the records follow the AEs the timeline draws", {
  expect_identical(ptr_records(ae_records = "all")$data$USUBJID,
                   c("A", "A", "A", "B"))
  expect_identical(ptr_records(ae_show = "serious AEs")$data$AESEQ, 3)
  expect_identical(ptr_records(ae_show = "ongoing AEs")$data$AESEQ, 3)
  picked <- ptr_records(ae_terms = "Preferred term: DIARRHOEA")
  expect_identical(picked$data$AESEQ, c(1, 2))
})

test_that("the grade is the board's severity column when declared", {
  r <- ptr_records(roles = list(severity = "AESEV"))
  expect_true("AESEV" %in% names(r$data))
  expect_false("ATOXGR" %in% names(r$data))
})

test_that("without the AE layer there are no records", {
  f <- patient_timeline_frame(ptr_toy(), layers = "Discontinuation and death")
  expect_null(attr(f, "viz_records", exact = TRUE))
})

test_that("a click on a patient narrows the block's records view", {
  blk <- new_safety_timeline_block(ae_records = "all")
  dat <- shiny::reactive(dm::dm(adsl = ptr_toy()$adsl,
                                adae = ptr_toy()$adae))
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      expr <- session$makeScope("expr")
      session$flushReact()
      payload <- function() {
        out <- expr$output$table
        if (!is.character(out)) out <- jsonlite::toJSON(out, auto_unbox = TRUE)
        jsonlite::fromJSON(out, simplifyVector = FALSE)$derived
      }
      expect_true(payload()$data_view_tool)
      expr$setInputs(table_data = list(on = TRUE, tab = "records", n = 1))
      session$flushReact()
      expect_identical(payload()$data_view$rows, 4L)
      expr$setInputs(table_select = blockr.viz:::viz_read_selection(list(
        selection = list(type = "categorical",
                         filters = list(USUBJID = list("A")),
                         columns = list()),
        nonce = 1
      )))
      session$flushReact()
      dv <- payload()$data_view
      expect_identical(dv$label, "Adverse events")
      expect_identical(c(dv$rows, dv$total), c(3L, 4L))
      expect_match(payload()$receipt, "A", fixed = TRUE)
    },
    args = list(x = blk, data = list(data = dat))
  )
})
