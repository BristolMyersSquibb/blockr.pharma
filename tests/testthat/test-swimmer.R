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
  # Time is the study day: day 1 the first dose.
  expect_equal(r$start[[1]], 1)
  expect_equal(r$start[[2]], 43)
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
  expect_equal(late$start, 43)
  expect_equal(late$end, 50)
  skip <- y[y$mark %in% "dashed" & !is.na(y$end), ]
  expect_equal(skip$start, 92)
  cut <- y[y$mark %in% "step", ]
  expect_equal(cut$start, 113)
  expect_identical(cut$dose, "75 mg to 50 mg")
  # The strip: five cycles and the skip, the late one marked.
  st <- y[!is.na(y$strip), ]
  expect_length(st$strip, 6L)
  expect_match(st$strip[[3]], "^C3\n75 mg\nd 50\n\\+7 d$")
  expect_identical(st$mark[[3]], "hollow")
  expect_match(st$strip[[6]], "^skip")
  # Daily Drug X: its dose cut from the periods, its arrow (ongoing).
  x <- f[f$USUBJID == "p1" & f$lane %in% "Drug x", ]
  expect_equal(x$start[x$mark %in% "step"], 51)
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

test_that("drugs and cutoff narrow the frame", {
  a <- swim_adam()
  expect_identical(swimmer_drugs(a), c("Drug Y", "Drug x"))
  f <- swimmer_frame(a, drugs = "Drug Y")
  expect_identical(levels(f$lane), "Drug Y")
  # A cutoff at day 60: later visits and doses go, drugs given past it run on.
  g <- swimmer_frame(a, cutoff = as.Date("2024-03-01"))
  expect_true(all(g$start[!is.na(g$start)] <= 61 + 1e-9))
  expect_identical(unique(g$status[g$USUBJID == "p1"]), "On treatment")
  # NA is automatic.
  expect_identical(swimmer_frame(a, cutoff = as.Date(NA)), swimmer_frame(a))
  expect_match(swimmer_subtitle(), "{label(@parent)}", fixed = TRUE)
})

test_that("the swimmer is in the menu and saves and restores as itself", {
  # Its own uid: one taken from the table's class would replace viz's Table.
  expect_true("swimmer_block" %in% blockr.core::list_blocks())
  tbl <- blockr.core:::get_registry_entry("table_block")
  expect_identical(attr(tbl, "ctor_name"), "new_table_block")

  blk <- blockr.core::create_block("swimmer_block")
  expect_identical(class(blk)[1:2], c("swimmer_block", "table_block"))
  expect_identical(blockr.core::block_name(blk), "Efficacy swimmer")
  ctor <- attr(blk, "ctor")
  expect_identical(attr(ctor, "fun"), "new_swimmer_block")
  expect_identical(attr(ctor, "pkg"), "blockr.pharma")
  expect_identical(blockr.core::block_metadata(blk)$id, "swimmer_block")

  # What a board saves: the swimmer's arguments, its script left out.
  blk <- new_swimmer_block(doses = "Dots", group = "BOR")
  ser <- blockr.core::blockr_ser(blk)
  expect_identical(ser$constructor$constructor, "new_swimmer_block")
  expect_setequal(names(ser$payload)[names(ser$payload) %in%
                                       names(formals(new_swimmer_block))],
                  setdiff(names(formals(new_swimmer_block)), "..."))
  expect_false("script" %in% names(ser$payload))
  expect_identical(ser$payload$script_values$doses, "Dots")
  expect_identical(ser$payload$by, c("BOR", "USUBJID"))
  back <- blockr.core::blockr_deser(ser)
  expect_identical(class(back)[1], "swimmer_block")
  expect_identical(attr(attr(back, "ctor"), "fun"), "new_swimmer_block")
  expect_identical(blockr.core::blockr_ser(back)$payload, ser$payload)
  # Others may set only the swimmer's own arguments.
  expect_false("layout" %in% blockr.core::external_ctrl_vars(blk))
  expect_true("script_values" %in% blockr.core::external_ctrl_vars(blk))
})

test_that("the swimmer's settings go to its script values; its gear is its own", {
  blk <- new_swimmer_block(doses = "Plain", con_meds = TRUE, max_lanes = 3)
  sv <- blockr.core::blockr_ser(blk)$payload$script_values
  expect_identical(sv[c("doses", "con_meds", "max_lanes")],
                   list(doses = "Plain", con_meds = TRUE, max_lanes = 3))
  expect_error(new_swimmer_block(doses = "nope"))
  g <- swimmer_gear()
  expect_s3_class(g, "viz_table_gear")
  expect_identical(vapply(g$tabs, `[[`, "", "title"),
                   c("Data", "Patients", "Lanes", "Titles", "Interaction",
                     "Script"))
  on <- vapply(g$columns, `[[`, NA, "on")
  expect_identical(names(on[on]), NULL)
  expect_identical(vapply(g$columns[on], `[[`, "", "name"),
                   "BOR")
  expect_identical(vapply(swimmer_summaries(), `[[`, "", "name"),
                   c("BOR", "Study time"))
  # Days on treatment is offered, off; the rows sort by it. The lane is as
  # wide as the listing leaves it.
  expect_false(g$columns[[4L]]$on)
  expect_identical(g$columns[[4L]]$name, "Days")
  expect_identical(formals(new_swimmer_block)$sort_by, "duration")
  lane <- swimmer_summaries()[[2L]]
  expect_identical(lane$size, "xl")
  expect_identical(lane$event_x, "event_day")
  # No weeks anywhere a reader sees them.
  expect_false(grepl("week", swimmer_script(), ignore.case = TRUE))
  expect_false(grepl("week", swimmer_subtitle(), ignore.case = TRUE))
  # The subtitle is made of the clauses the gear ticks.
  expect_match(swimmer_subtitle(), swimmer_clauses()[["doses"]], fixed = TRUE)
  expect_false(grepl(swimmer_clauses()[["con_meds"]], swimmer_subtitle(),
                     fixed = TRUE))
})

test_that("swimmer_mapping() says which tables and columns it found", {
  a <- swim_adam()
  m <- swimmer_mapping(a)
  roles <- vapply(m$roles, `[[`, "", "role")
  expect_identical(roles, c("Subjects", "Doses", "Response",
                            "Reason off treatment", "Serious AEs",
                            "Con meds"))
  expect_true(all(vapply(m$roles[1:3], `[[`, NA, "found")))
  expect_identical(m$parameters, "OVR")
  tabs <- if (inherits(a, "dm")) dm::dm_get_tables(a) else a
  tabs$adrs$ADT <- NULL
  m2 <- swimmer_mapping(tabs[c("adsl", "adrs")])
  expect_false(m2$roles[[2L]]$found)
  expect_identical(m2$roles[[3L]]$missing, "ADT")
  expect_identical(swimmer_mapping(NULL)$parameters, character())
})

test_that("the frame draws study days and only the events asked for", {
  a <- swim_adam()
  a$adae <- data.frame(USUBJID = "p1", AESER = "Y",
                       ASTDT = as.Date("2024-01-01") + 30)
  f <- swimmer_frame(a)
  expect_identical(attr(f$start, "label"), "Start, study day")
  expect_false(any(grepl("week", vapply(f, function(x) {
    attr(x, "label") %||% ""
  }, ""), ignore.case = TRUE)))
  # Serious AEs are off by default.
  expect_false("Serious AE" %in% f$event)
  expect_true("Serious AE" %in% swimmer_frame(a, events = NULL)$event)
  ev <- swimmer_frame(a, events = "Death")
  expect_true(all(is.na(ev$event) | ev$event == "Death"))
  expect_true(all(c("AGE", "SEX", "dose_level") %in% names(f)))
})

test_that("the reason off treatment comes from ADSL, also under other names", {
  a <- swim_adam()
  a$adsl$TRTEDT <- as.Date("2024-01-01") + c(NA, 60, 30)
  a$adsl$DCTREAS <- c("", "ADVERSE EVENT", "Physician Decision")
  f <- swimmer_frame(a)
  why <- tapply(f$reason, f$USUBJID, unique)
  expect_identical(why[["p2"]], "Adverse event")
  expect_true(is.na(why[["p1"]]))
  expect_identical(attr(f$reason, "label"), "Reason off treatment")
  # The study reason or the disposition term when there is no DCTREAS; a
  # completed disposition is no reason, nor one while on treatment.
  a$adsl$DCTREAS <- NULL
  a$adsl$DSTERM <- c("PROGRESSIVE DISEASE", "COMPLETED", "DEATH")
  g <- swimmer_frame(a)
  why <- tapply(g$reason, g$USUBJID, unique)
  expect_true(is.na(why[["p1"]]))
  expect_true(is.na(why[["p2"]]))
  a$adsl$DCSREAS <- c(NA, "WITHDRAWAL BY SUBJECT", NA)
  expect_identical(unique(swimmer_frame(a)$reason[g$USUBJID == "p2"]),
                   "Withdrawal by subject")
})

test_that("a board saved with weeks restores in days", {
  old <- swimmer_summaries()
  old[[2L]]$event_x <- "event_wk"
  old <- c(old[1L], list(list(type = "simple", name = "Weeks",
                              func = "max", col = "weeks", show = "number")),
           old[2L])
  blk <- new_swimmer_block(summaries = old, sort_by = "weeks",
                           script_values = list(unit = "weeks"),
                           unit = "weeks")
  p <- blockr.core::blockr_ser(blk)$payload
  expect_identical(p$sort_by, "duration")
  expect_null(p$script_values$unit)
  expect_identical(p$summaries[[2L]]$col, "duration")
  expect_identical(p$summaries[[2L]]$name, "Days")
  expect_identical(p$summaries[[3L]]$event_x, "event_day")
})

test_that("an end of treatment closes a study drug and ends the time on it", {
  a <- swim_adam()
  d <- function(x) as.Date("2024-01-01") + x
  # p1's Drug X has no end, but p1 ended treatment on day 140: Drug X stops
  # there, and p1 is off treatment.
  a$adsl$TRTEDT <- d(c(140, 60, 30))
  f <- swimmer_frame(a)
  x <- f[f$USUBJID == "p1" & f$lane %in% "Drug x" & !is.na(f$end), ]
  expect_equal(max(x$end), 141)
  expect_false(any(x$ongoing %in% TRUE))
  expect_identical(unique(f$status[f$USUBJID == "p1"]), "Off treatment")
  expect_equal(unique(f$duration[f$USUBJID == "p1"]), 141)
  # A record that starts after the end of treatment stays a point.
  a$adex <- rbind(a$adex, data.frame(
    USUBJID = "p2", PARAMCD = "DOSE", EXTRT = "DRUG X", ASTDT = d(70),
    AENDT = as.Date(NA), EXDOSE = 100, EXDOSU = "mg"
  ))
  g <- swimmer_frame(a)
  late <- g[g$USUBJID == "p2" & g$lane %in% "Drug x" & !is.na(g$start) &
              g$start > 63 & is.na(g$mark) & is.na(g$strip), ]
  expect_equal(late$end, late$start)
  # Without an end of treatment the open drug runs on, as before.
  a$adsl$TRTEDT <- d(c(NA, 60, 30))
  h <- swimmer_frame(a)
  expect_identical(unique(h$status[h$USUBJID == "p1"]), "On treatment")
})

test_that("the rows can be grouped by any of ADSL's grouping columns", {
  a <- swim_adam()
  a$adsl$TRT <- c("Low", "Low", "High")
  a$adsl$RACE <- factor(c("A", "B", "A"))
  a$adsl$NOTE <- paste("free text", 1:3)
  f <- swimmer_frame(a)
  expect_identical(unique(f$TRT[f$USUBJID == "p1"]), "Low")
  expect_true("RACE" %in% names(f))
  blk <- new_swimmer_block(group = "TRT")
  expect_identical(blockr.core::blockr_ser(blk)$payload$by, c("TRT", "USUBJID"))
})

test_that("an ADSL data cutoff is the automatic cutoff", {
  a <- swim_adam()
  a$adsl$DCUTDT <- as.Date("2024-03-01")
  f <- swimmer_frame(a)
  expect_true(max(f$end, na.rm = TRUE) <= 61 + 1e-9)
})

test_that("SDTM-style tables and columns read as their ADaM counterparts", {
  a <- swim_adam()
  iso <- function(x) format(x, "%Y-%m-%d")
  a$adae <- data.frame(USUBJID = "p1", AESER = "Y",
                       ASTDT = as.Date("2024-01-01") + 30)
  a$adcm <- data.frame(USUBJID = "p1", CMDECOD = "ASPIRIN",
                       ASTDT = as.Date("2024-01-01") + c(5, 60),
                       AENDT = as.Date("2024-01-01") + c(20, NA))
  sdtm <- list(
    dm = data.frame(USUBJID = a$adsl$USUBJID, ARM = a$adsl$ARM,
                    RFXSTDTC = iso(a$adsl$TRTSDT),
                    DTHDTC = iso(a$adsl$DTHDT), LSTALVDT = a$adsl$LSTALVDT),
    ex = data.frame(USUBJID = a$adex$USUBJID, EXTRT = a$adex$EXTRT,
                    EXSTDTC = iso(a$adex$ASTDT),
                    EXENDTC = iso(a$adex$AENDT), EXDOSE = a$adex$EXDOSE,
                    EXDOSU = a$adex$EXDOSU),
    rs = data.frame(USUBJID = a$adrs$USUBJID, RSTESTCD = "OVRLRESP",
                    RSSTRESC = a$adrs$AVALC, RSDTC = iso(a$adrs$ADT)),
    ae = data.frame(USUBJID = "p1", AESER = "Y", AESTDTC = iso(a$adae$ASTDT)),
    cm = data.frame(USUBJID = "p1", CMDECOD = "ASPIRIN",
                    CMSTDTC = iso(a$adcm$ASTDT), CMENDTC = iso(a$adcm$AENDT))
  )
  cols <- c("USUBJID", "start", "end", "colour", "lane", "mark", "strip",
            "label", "event_day", "event", "ongoing", "days", "dose", "ARM",
            "BOR", "duration", "status", "reason")
  want <- swimmer_frame(a, con_meds = TRUE, events = NULL)[cols]
  got <- swimmer_frame(sdtm, con_meds = TRUE, parameter = "OVRLRESP",
                       events = NULL)
  expect_identical(got[cols], want)
  expect_true("Serious AE" %in% got$event)
  expect_true("Aspirin" %in% got$label)
  # The same as a dm, with upper-case names.
  up <- do.call(dm::dm, stats::setNames(sdtm, toupper(names(sdtm))))
  expect_identical(swimmer_frame(up, con_meds = TRUE,
                                 parameter = "OVRLRESP", events = NULL)[cols],
                   want)
  expect_identical(swimmer_drugs(up), swimmer_drugs(a))
  # The gear's Tables row names each table as the input does.
  m <- swimmer_mapping(sdtm)
  expect_identical(vapply(m$roles, `[[`, "", "table"),
                   c("DM", "EX", "RS", "DM", "AE", "CM"))
  expect_true(all(vapply(m$roles, `[[`, NA, "found")))
  # Only the optional reason off treatment is not in this DM.
  expect_identical(unlist(lapply(m$roles, `[[`, "missing")), "DCTREAS")
  expect_identical(m$parameters, "OVRLRESP")
  expect_identical(swimmer_mapping(a)$roles[[1L]]$table, "ADSL")
})
