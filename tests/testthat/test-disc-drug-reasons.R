# Per-drug treatment reasons: a DS disposition record whose DSSCAT names a
# drug tags that drug's line at its end. ADSL DCTREAS stays at the last dose,
# on the lane above the drugs.

# Patient A on two drugs: Drug A stops on day 30, Drug B on day 60 (TRTEDT).
drug_toy <- function(ds = NULL) {
  d <- pt_toy()
  d$adsl$DCTREAS <- c("PROGRESSIVE DISEASE", NA, NA)
  d$adsl$DCSREAS <- c("WITHDRAWAL BY SUBJECT", NA, NA)
  d$adsl$EOSDT <- as.Date(c("2024-03-20", NA, NA))
  # Mixed case: an all-caps name would read "Drug b" (swimmer_drug_name()).
  d$adex$EXTRT <- c("Drug A", "Drug B", "PLACEBO", "Drug A")
  d$adex$ASTDT[[2L]] <- as.Date("2024-01-01")
  d$ds <- ds
  d
}

drug_ds <- function(scat = "DRUG A", decod = "ADVERSE EVENT") {
  data.frame(
    USUBJID = "A", DSCAT = "DISPOSITION EVENT", DSSCAT = scat,
    DSDECOD = decod, DSSTDTC = "2024-01-30", stringsAsFactors = FALSE
  )
}

drug_tags <- function(f) {
  f <- f[!is.na(f$.tag) & !f$Event %in% "Dose change", , drop = FALSE]
  paste(f$USUBJID, f$.tag, f$.event_x, as.character(f$.lane))
}

drug_layers <- c("Treatment", "Discontinuation and death")

test_that("a DS record naming a drug tags that drug's end", {
  f <- patient_timeline_frame(drug_toy(drug_ds()), layers = drug_layers)
  expect_setequal(drug_tags(f), c("A AE 30 Drug A", "A PD 60 Study",
                                  "A WD 80 Study"))
  tg <- f[f$.tag %in% "AE", ]
  expect_identical(tg$Type, "Treatment discontinuation")
  expect_identical(as.character(tg$.tag_group), "Treatment discontinuation")
})

test_that("a DSSCAT that names no drug drawn tags nothing", {
  f <- patient_timeline_frame(drug_toy(drug_ds("DRUG C")),
                              layers = drug_layers)
  expect_setequal(drug_tags(f), c("A PD 60 Study", "A WD 80 Study"))
})

test_that("an unpicked drug's reason drops with its line", {
  f <- patient_timeline_frame(drug_toy(drug_ds()), layers = drug_layers,
                              drugs = "Drug B")
  expect_setequal(drug_tags(f), c("A PD 60 Study", "A WD 80 Study"))
})

test_that("a per-drug reason that repeats the ADSL one at the last dose is
          drawn once", {
  ds <- drug_ds("DRUG B", "PROGRESSIVE DISEASE")
  f <- patient_timeline_frame(drug_toy(ds), layers = drug_layers)
  expect_setequal(drug_tags(f), c("A PD 60 Study", "A WD 80 Study"))
})

test_that("the end-of-study tag sits on the study lane above the drugs", {
  f <- patient_timeline_frame(drug_toy(), layers = drug_layers)
  lv <- levels(f$.lane)
  expect_identical(lv[[1L]], "Study")
  expect_lt(match("Study", lv), match("Drug A", lv))
  # Without drug lines it stays on the patient's main lane, as before.
  f <- patient_timeline_frame(drug_toy(),
                              layers = "Discontinuation and death")
  eos <- f[f$.tag %in% "WD", ]
  expect_true(is.na(eos$.lane))
})

test_that("drugs that stop on the same day leave the ADSL reason above
          them, whatever the ADEX row order", {
  d <- drug_toy()
  d$adex$AENDT[[1L]] <- as.Date("2024-02-29")
  a <- patient_timeline_frame(d, layers = drug_layers)
  d$adex <- d$adex[c(2L, 1L, 3L, 4L), ]
  b <- patient_timeline_frame(d, layers = drug_layers)
  expect_setequal(drug_tags(a), c("A PD 60 Study", "A WD 80 Study"))
  expect_identical(sort(drug_tags(a)), sort(drug_tags(b)))
  # Without drug lines both stay on the patient's main lane.
  f <- patient_timeline_frame(d, layers = "Discontinuation and death")
  expect_true(all(is.na(f$.lane[!is.na(f$.tag)])))
})

test_that("ADSL-only data tags as before", {
  a <- patient_timeline_frame(drug_toy(), layers = drug_layers)
  b <- patient_timeline_frame(drug_toy(drug_ds(NA)), layers = drug_layers)
  expect_identical(drug_tags(a), drug_tags(b))
  expect_setequal(drug_tags(a), c("A PD 60 Study", "A WD 80 Study"))
})

# The patient profile tags the same drugs, in each drug's slot.
drug_profile_dm <- function(ds) {
  d <- drug_toy(ds)
  d$adsl <- d$adsl[1, ]
  d$adex <- d$adex[d$adex$USUBJID == "A", ]
  d$adex$ASTDY <- d$adex$AENDY <- NULL
  dm::dm(adsl = d$adsl, adex = d$adex, ds = d$ds)
}

drug_milestones <- function(dm_obj) {
  chart <- patient_overview_viz$render(dm_obj, as.Date(c(NA, NA)),
                                       mode = "date")
  ms <- Filter(function(s) identical(s$name, "Milestones"),
               chart$x$opts$series)[[1]]
  lapply(ms$data, `[[`, "value")
}

test_that("the profile tags a drug DS names in that drug's slot", {
  v <- drug_milestones(drug_profile_dm(drug_ds()))
  tags <- Filter(function(x) identical(x[[3]], "tag"), v)
  got <- vapply(tags, function(x) paste(x[[4]], x[[5]], x[[6]]), "")
  # The ADSL reason above the drugs; Drug A's in the first of two slots.
  expect_setequal(got, c("PD -1 2", "AE 2 0"))
  ae <- Filter(function(x) identical(x[[4]], "AE"), tags)[[1]]
  expect_identical(ae[[1]], pp_xval(as.Date("2024-01-30"), NA, "date"))

  v <- drug_milestones(drug_profile_dm(drug_ds("DRUG C")))
  expect_false(any(vapply(v, function(x) identical(x[[4]], "AE"), NA)))
})

test_that("the profile's export draws the per-drug tag too", {
  skip_if_not_installed("ggplot2")
  p <- pp_static_overview(drug_profile_dm(drug_ds()), as.Date(c(NA, NA)),
                          mode = "date")
  lab <- unlist(lapply(p$layers, function(l) l$data$label))
  expect_true("AE" %in% lab)
})

test_that("the profile's ADSL tags sit above the drugs and know each other", {
  d <- drug_toy()
  d$adsl <- d$adsl[1, ]
  d$adex <- d$adex[d$adex$USUBJID == "A", ]
  d$adex$ASTDY <- d$adex$AENDY <- NULL
  v <- drug_milestones(dm::dm(adsl = d$adsl, adex = d$adex))
  adsl <- Filter(function(x) x[[4]] %in% c("PD", "WD"), v)
  expect_length(adsl, 2L)
  expect_true(all(vapply(adsl, function(x) x[[5]] == -1L, NA)))
  # Each carries the other's x, for the split when they would touch.
  expect_identical(adsl[[1]][[7]], adsl[[2]][[1]])
  expect_identical(adsl[[2]][[7]], adsl[[1]][[1]])
  # Without ADEX the tags stay on the lane's centre.
  d2 <- dm::dm(adsl = d$adsl)
  v2 <- drug_milestones(d2)
  expect_true(all(vapply(v2, function(x) x[[5]] == 0L, NA)))
})

test_that("the export puts the earlier of two touching ADSL tags left", {
  skip_if_not_installed("ggplot2")
  d <- drug_toy()
  d$adsl <- d$adsl[1, ]
  d$adsl$EOSDT <- as.Date("2024-03-01")
  d$adex <- d$adex[d$adex$USUBJID == "A", ]
  d$adex$ASTDY <- d$adex$AENDY <- NULL
  p <- pp_static_overview(dm::dm(adsl = d$adsl, adex = d$adex),
                          as.Date(c(NA, NA)), mode = "date")
  lab <- do.call(rbind, lapply(unname(p$layers), function(l) {
    if (any(l$data$label %in% c("PD", "WD"))) {
      l$data[c("label", "y", "hjust")]
    }
  }))
  # One row: the treatment reason ends at the last dose, the end of study
  # starts at its day.
  expect_identical(lab$y[[1]], lab$y[[2]])
  h <- stats::setNames(lab$hjust, lab$label)
  expect_gt(h[["PD"]], 1)
  expect_lt(h[["WD"]], 0)
})
