# new_ae_timeline_block() and its frame, ae_timeline_frame(): ADAE (and
# ADSL) in, one record per AE plus the overlays out, drawn by a viz table
# block in the groups layout.

# A small study: patient A with two overlapping AEs (one ongoing, one
# serious) and one after them; patient B with one AE and a death.
aet_toy <- function() {
  adsl <- data.frame(
    USUBJID = c("A", "B"),
    TRT01A = c("Drug", "Placebo"),
    TRTSDT = as.Date(c("2024-01-01", "2024-01-01")),
    TRTEDT = as.Date(c("2024-02-29", "2024-01-20")),
    LSTALVDT = as.Date(c("2024-03-30", "2024-01-25")),
    DTHDT = as.Date(c(NA, "2024-01-25")),
    stringsAsFactors = FALSE
  )
  attr(adsl$TRT01A, "label") <- "Actual Treatment"
  adae <- data.frame(
    USUBJID = c("A", "A", "A", "B"),
    AEDECOD = c("HEADACHE", "NAUSEA", "RASH", "FALL"),
    AEBODSYS = c("NERVOUS", "GI", "SKIN", "INJURY"),
    AESEV = c("MILD", "SEVERE", "MODERATE", "MODERATE"),
    AESER = c("N", "Y", "N", "N"),
    AESDTH = c("N", "N", "N", "N"),
    AEOUT = c("RECOVERED", "NOT RECOVERED", "RECOVERED", "RECOVERED"),
    TRTEMFL = "Y",
    ASTDY = c(2, 5, 40, 3),
    AENDY = c(10, NA, 45, 4),
    stringsAsFactors = FALSE
  )
  list(adsl = adsl, adae = adae)
}

aes_of <- function(f) f[is.finite(f$Start), , drop = FALSE]

test_that("overlapping AEs stack, a later one reuses the first lane", {
  f <- ae_timeline_frame(aet_toy())
  a <- aes_of(f)
  a <- a[a$USUBJID == "A", ]
  expect_identical(as.character(a$.lane[a$Term == "Headache"]), "1")
  expect_identical(as.character(a$.lane[a$Term == "Nausea"]), "2")
  expect_identical(as.character(a$.lane[a$Term == "Rash"]), "1")
})

test_that("an AE without an end runs open to last contact or the cutoff", {
  tb <- aet_toy()
  a <- aes_of(ae_timeline_frame(tb))
  n <- a[a$Term == "Nausea", ]
  expect_true(n$.open)
  # Last contact 2024-03-30 is study day 90.
  expect_equal(n$End, 90)
  expect_identical(n[["Study days"]], "Day 5 to ongoing")
  # To the cutoff it has no end, so the table runs it to the axis end.
  a2 <- aes_of(ae_timeline_frame(tb, ongoing = "data cutoff"))
  expect_true(is.na(a2$End[a2$Term == "Nausea"]))
  expect_equal(a2$End[a2$Term == "Headache"], 10)
  tb$adsl$LSTALVDT[[1L]] <- as.Date("2024-03-01")
  a3 <- aes_of(ae_timeline_frame(tb))
  expect_equal(a3$End[a3$Term == "Nausea"], 61)
})

test_that("serious AEs, death and the treatment line", {
  f <- ae_timeline_frame(aet_toy())
  expect_identical(f$.style[f$Term %in% "Nausea"], "outline")
  expect_true(all(is.na(f$.style[f$Term %in% c("Headache", "Rash")])))
  ev <- f[!is.na(f$Event), ]
  expect_identical(as.character(ev$Event), "Death")
  expect_equal(ev$.event_day, 25)
  # A death date marks the whole row.
  expect_true(is.na(ev$.lane))
  # The cross is the third shape.
  expect_identical(levels(f$Event)[[3L]], "Death")
  m <- ae_timeline_frame(aet_toy(), serious = "marked at onset")
  s <- m[m$Event %in% "Serious AE", ]
  expect_equal(s$.event_day, 5)
  # The onset mark sits on its AE's lane.
  expect_identical(as.character(s$.lane), "2")
  expect_true(all(is.na(m$.style)))
  trt <- f[is.finite(f$.trt_to), ]
  expect_equal(trt$.trt_to[trt$USUBJID == "A"], 60)
  expect_false(any(is.finite(ae_timeline_frame(aet_toy(),
                                               treatment = FALSE)$.trt_to)))
})

test_that("the listing values repeat per patient and sort the rows", {
  f <- ae_timeline_frame(aet_toy())
  a <- f[f$USUBJID == "A", ]
  expect_true(all(a$AEs == 3))
  expect_true(all(a$Ongoing == 1))
  expect_true(all(a[["Serious AEs"]] == 1))
  expect_identical(as.character(unique(a$Worst)), "Severe")
  expect_identical(unique(f$USUBJID), c("A", "B"))
  f2 <- ae_timeline_frame(aet_toy(), order_by = "first onset")
  expect_identical(unique(f2$USUBJID), c("A", "B"))
  f3 <- ae_timeline_frame(aet_toy(), order_by = "subject")
  expect_identical(unique(f3$USUBJID), c("A", "B"))
})

test_that("severity keeps clinical order and its own colours", {
  f <- ae_timeline_frame(aet_toy())
  expect_identical(levels(f$Severity), c("Mild", "Moderate", "Severe"))
  expect_identical(names(attr(f$Severity, "blockr_colors")),
                   c("Mild", "Moderate", "Severe"))
  expect_error(ae_timeline_frame(aet_toy(), colour = "Grade"),
               "toxicity grade")
  tb <- aet_toy()
  tb$adae$AETOXGR <- c("1", "3", "2", "2")
  g <- aes_of(ae_timeline_frame(tb, colour = "Grade"))
  expect_identical(as.character(g$Severity[g$Term == "Nausea"]), "Grade 3")
})

test_that("events, lanes and the group follow their settings", {
  tb <- aet_toy()
  s <- aes_of(ae_timeline_frame(tb, events = "Serious AEs"))
  expect_identical(s$Term, "Nausea")
  o <- aes_of(ae_timeline_frame(tb, events = "Ongoing AEs"))
  expect_identical(o$Term, "Nausea")
  t <- aes_of(ae_timeline_frame(tb, lanes = "by body system"))
  expect_identical(as.character(t$.lane[t$Term == "Rash"]), "Skin")
  one <- aes_of(ae_timeline_frame(tb, lanes = "on one lane"))
  expect_true(all(one$.lane == "1"))
  f <- ae_timeline_frame(tb)
  expect_identical(attr(f$TRT01A, "label"), "Actual Treatment")
  # The other patient columns stay for grouping.
  expect_true("TRT01A" %in% names(ae_timeline_frame(tb, group = NULL)))
  expect_error(ae_timeline_frame(tb, group = "NOPE"), "neither")
})

test_that("a fatal AE without a death date marks death on its lane", {
  tb <- aet_toy()
  tb$adsl$DTHDT <- as.Date(NA)
  tb$adae$AESDTH[[4L]] <- "Y"
  f <- ae_timeline_frame(tb)
  ev <- f[f$Event %in% "Death", ]
  expect_equal(ev$.event_day, 4)
  expect_identical(as.character(ev$.lane), "1")
})

test_that("term labels go on the bars or only in the tooltip", {
  a <- aes_of(ae_timeline_frame(aet_toy()))
  expect_identical(a$.bar_label, a$Term)
  t <- aes_of(ae_timeline_frame(aet_toy(), term_labels = "in the tooltip"))
  expect_true(all(is.na(t$.bar_label)))
  expect_identical(t$Term, a$Term)
})

test_that("a cutoff leaves out what comes after it", {
  tb <- aet_toy()
  # 2024-01-20 is study day 20 for both patients.
  f <- ae_timeline_frame(tb, cutoff = as.Date("2024-01-20"))
  a <- aes_of(f)
  expect_false("Rash" %in% a$Term)
  # Ongoing at the cutoff, so it runs to the capped last contact.
  n <- a[a$Term == "Nausea", ]
  expect_true(n$.open)
  expect_equal(n$End, 20)
  expect_false("Death" %in% as.character(f$Event))
  # NA is automatic.
  expect_identical(ae_timeline_frame(tb, cutoff = as.Date(NA)),
                   ae_timeline_frame(tb))
})

test_that("a dm and ADAE alone both work", {
  tb <- aet_toy()
  d <- dm::dm(adae = tb$adae, adsl = tb$adsl)
  expect_identical(ae_timeline_frame(d)$Term, ae_timeline_frame(tb)$Term)
  # ADaM puts the ADSL columns on ADAE.
  ae <- merge(tb$adae, tb$adsl, by = "USUBJID")
  expect_identical(sort(aes_of(ae_timeline_frame(ae))$Term),
                   sort(aes_of(ae_timeline_frame(tb))$Term))
  expect_error(ae_timeline_frame(list(adsl = tb$adsl)), "adae")
})

test_that("the block is a groups table that saves as one", {
  b <- new_ae_timeline_block()
  expect_s3_class(b, "table_block")
  sr <- blockr.core::blockr_ser(b)
  ctor <- blockr.core::blockr_deser(sr$constructor)
  expect_identical(blockr.core::ctor_name(ctor), "new_table_block")
  p <- sr$payload
  expect_identical(p$layout, "groups")
  expect_identical(p$by, c("TRT01A", "USUBJID"))
  sp <- p$summaries[[length(p$summaries)]]
  expect_identical(sp$type, "spans")
  expect_identical(sp$lane, ".lane")
  expect_identical(sp$bar_label, ".bar_label")
  expect_identical(p$sort_by, "data")
})

test_that("the script's settings are its controls and run the frame", {
  sc <- ae_timeline_script()
  ex <- parse(text = sc)
  vars <- vapply(ex[-length(ex)], function(e) as.character(e[[2L]]), "")
  expect_identical(vars, c("events", "colour", "lanes", "ongoing", "serious",
                           "order_by", "term_labels", "cutoff", "treatment",
                           "treatment_emergent"))
  env <- new.env()
  env$data <- aet_toy()
  out <- eval(ex, env)
  expect_identical(out, ae_timeline_frame(aet_toy()))
  # Every word in the sentence is a setting of the script.
  words <- regmatches(ae_timeline_subtitle(),
                      gregexpr("\\{@([a-z_]+)\\}", ae_timeline_subtitle()))[[1]]
  expect_length(words, 8L)
  expect_true(all(gsub("[{}@]", "", words) %in% vars))
})
