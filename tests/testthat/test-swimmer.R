# The swimmer's frame from ADaM and its block.

swim_adam <- function() {
  d <- function(x) as.Date("2024-01-01") + x
  adsl <- data.frame(
    USUBJID = c("p1", "p2", "p3"), ARM = c("A", "A", "B"),
    TRTSDT = d(c(0, 0, 0)), DTHDT = d(c(NA, 90, NA)),
    LSTALVDT = d(c(200, 90, 100))
  )
  # p1: Drug X daily periods (ongoing, a dose cut); Drug Y every 21 days,
  # one dose 7 days late (day 49 instead of 42), one cycle skipped (day 91),
  # a cut from 75 to 50 at the last. p2: one closed period. p3: no response.
  y <- c(0, 21, 49, 70, 112)
  adex <- data.frame(
    USUBJID = c("p1", "p1", rep("p1", 5), "p2", "p3"),
    PARAMCD = "DOSE",
    EXTRT = c("DRUG X", "DRUG X", rep("Drug Y", 5), "DRUG X", "DRUG X"),
    ASTDT = d(c(0, 50, y, 0, 0)),
    AENDT = d(c(49, NA, y, 60, 30)),
    EXDOSE = c(100, 50, 75, 75, 75, 75, 50, 100, 100),
    EXDOSU = "mg"
  )
  adrs <- data.frame(
    USUBJID = c("p1", "p1", "p1", "p2", "p2"), PARAMCD = "OVR",
    ADT = d(c(42, 84, 126, 42, 80)), AVALC = c("SD", "PR", "PR", "SD", "PD")
  )
  list(adsl = adsl, adex = adex, adrs = adrs)
}

test_that("swimmer_frame builds response, lanes, events and listing", {
  f <- swimmer_frame(swim_adam())
  expect_setequal(unique(f$USUBJID), c("p1", "p2"))
  # Response: not assessed, then assessment to next, the last to follow-up.
  r <- f[f$USUBJID == "p1" & is.na(f$lane) & !is.na(f$start), ]
  expect_identical(as.character(r$colour), c("Not assessed", "SD", "PR", "PR"))
  expect_equal(r$start[[2]], 6)
  # Best response and status.
  expect_identical(unique(f$BOR[f$USUBJID == "p1"]), "PR")
  expect_identical(unique(f$status[f$USUBJID == "p2"]), "Died")
  expect_identical(unique(f$status[f$USUBJID == "p1"]), "On treatment")
  # Events: p2's progression and death.
  ev <- f[f$USUBJID == "p2" & !is.na(f$event), ]
  expect_identical(as.character(ev$event), c("Progression", "Death"))
  # Lanes: one per drug, an all-caps name in title case.
  expect_setequal(levels(f$lane), c("Drug x", "Drug Y"))
  # Own colours for every colour level.
  expect_true(all(levels(f$colour) %in% names(attr(f$colour,
                                                    "blockr_colors"))))
})

test_that("cycle drugs: late doses, skipped cycles and cuts", {
  f <- swimmer_frame(swim_adam(), doses = "Exceptions")
  y <- f[f$USUBJID == "p1" & f$lane %in% "Drug Y", ]
  late <- y[y$mark %in% "hollow" & !is.na(y$end), ]
  expect_equal(late$start * 7, 42)
  expect_equal(late$end * 7, 49)
  skip <- y[y$mark %in% "dashed" & !is.na(y$end), ]
  expect_equal(skip$start * 7, 91)
  cut <- y[y$mark %in% "step", ]
  expect_equal(cut$start * 7, 112)
  expect_identical(cut$dose, "75 mg to 50 mg")
  # The strip: five cycles and the skip, the late one marked.
  st <- y[!is.na(y$strip), ]
  expect_length(st$strip, 6L)
  expect_match(st$strip[[3]], "^C3\n75 mg\nd 50\n\\+7 d$")
  expect_identical(st$mark[[3]], "hollow")
  expect_match(st$strip[[6]], "^skip")
  # Daily Drug X: its dose cut from the periods, its arrow (ongoing).
  x <- f[f$USUBJID == "p1" & f$lane %in% "Drug x", ]
  expect_equal(x$start[x$mark %in% "step"] * 7, 50)
  expect_true(any(x$ongoing, na.rm = TRUE))
})

test_that("doses: dots ring each administration, plain draws only the bars", {
  dots <- swimmer_frame(swim_adam(), doses = "Dots")
  expect_identical(sum(dots$mark %in% "dot"), 5L)
  plain <- swimmer_frame(swim_adam(), doses = "Plain")
  drawn <- plain[is.na(plain$strip), ]
  expect_false(any(drawn$mark %in% c("dot", "step", "hollow", "dashed")))
})

test_that("past max_lanes con meds fold into Other, study drugs keep lanes", {
  a <- swim_adam()
  a$adcm <- data.frame(
    USUBJID = "p1", CMDECOD = c("ASPIRIN", "IBUPROFEN", "PARACETAMOL"),
    ASTDT = as.Date("2024-01-10"), AENDT = as.Date("2024-02-10"),
    CMDOSE = 1, CMDOSU = "tablet"
  )
  f <- swimmer_frame(a, con_meds = TRUE, max_lanes = 4L)
  expect_setequal(levels(f$lane), c("Drug x", "Drug Y", "Other"))
  expect_identical(levels(f$lane)[[3]], "Other")
  o <- f[f$lane %in% "Other" & !is.na(f$strip), ]
  expect_length(o$strip, 3L)
  f2 <- swimmer_frame(a, con_meds = TRUE, max_lanes = 6L)
  expect_true("Aspirin" %in% levels(f2$lane))
})

test_that("swimmer_frame names what is missing", {
  a <- swim_adam()
  expect_error(swimmer_frame(a["adsl"]), "adex, adrs")
  a$adrs$PARAMCD <- "BOR"
  expect_error(swimmer_frame(a), "overall response")
  skip_if_not_installed("dm")
  expect_s3_class(swimmer_frame(do.call(dm::dm, swim_adam())), "data.frame")
})

test_that("new_swimmer_block is a groups table with the swimmer script", {
  blk <- new_swimmer_block(doses = "Dots")
  expect_s3_class(blk, "table_block")
  expect_match(swimmer_script(), "swimmer_frame\\(data, doses = doses")
  expect_error(new_swimmer_block(doses = "nope"))
})
