# Population join block -----------------------------------------------------
#
# WHY THIS EXISTS.
#
# A rate needs a denominator, and for anything event-level the denominator is
# not in the events. Subjects with no adverse event are not in ADAE at all, so
# a percentage computed from the AE table alone divides by "subjects who had an
# AE" rather than by "subjects who were treated". Measured on pharmaverseadam:
# 29 of 254 safety subjects have no AE, and the gap is uneven across arms --
# 0.7 points in the high-dose arm, 4.8 in placebo. It inflates the comparator
# most, which for a safety chart is the worst direction to be wrong in.
#
# WHY IT SITS AT THE END OF A BRANCH.
#
# The obvious move is to carry the population from the top, so everything
# downstream has it. That fails: every event filter then has to be written
# "matches the event OR is not an event", and it is not only OUR filters --
# a user adding a value filter in the gear silently deletes the population
# rows and every percentage rises. No convention survives that.
#
# So the population goes in LAST, after the filters have run. Nothing filters
# it because nothing downstream of here filters at all. That is the one rule
# this block carries, and it is a single visible link on the board rather than
# a contract every filter in the ecosystem has to honour.
#
# WHY IT IS A JOIN BLOCK AND NOT A CHART FEATURE.
#
# Its output feeds both kinds of consumer unchanged. A chart counts distinct
# subjects per panel (blockr.viz `func = "pct_distinct"` with
# `na_group = "drop"`); a summary table derives one row per subject from the
# same frame and counts those. Both give the same N, so the denominator is
# defined once for a branch rather than per view.

#' Restore the subjects an event table never mentions
#'
#' Appends one row per subject that is present in `population` and absent from
#' `events`, carrying the population's columns and `NA` for everything
#' event-level. The result is the events **plus their population**: counting
#' distinct subjects in it gives the denominator a rate needs, while the events
#' themselves are untouched.
#'
#' Population columns the events already carry are left alone, so nothing is
#' renamed and no `.x` / `.y` suffixes appear. Columns the events lack are
#' joined on, which is how the appended rows get a treatment arm to be grouped
#' by. That asymmetry is deliberate: the events are the subject here, the
#' population only supplies what they are missing.
#'
#' @param events Event-level data frame, already filtered.
#' @param population Subject-level data frame -- one row per subject, e.g.
#'   ADSL restricted to a population flag.
#' @param id Subject identifier, present in both. Default `"USUBJID"`.
#'
#' @return `events` with the absent subjects appended.
#'
#' @examples
#' ev  <- data.frame(USUBJID = c("S1", "S1", "S2"),
#'                   AEDECOD = c("Diarrhoea", "Diarrhoea", "Nausea"))
#' pop <- data.frame(USUBJID = c("S1", "S2", "S3"), TRT01A = "A")
#' join_population(ev, pop)
#'
#' @export
join_population <- function(events, population, id = "USUBJID") {

  if (!is.data.frame(events) || !is.data.frame(population)) {
    stop("`events` and `population` must both be data frames.", call. = FALSE)
  }
  if (!id %in% names(events) || !id %in% names(population)) {
    stop("Subject identifier '", id, "' must be present in both inputs.",
         call. = FALSE)
  }

  # Only what the events lack. Joining a column they already have would
  # produce a .x/.y pair and leave the caller to guess which one to group by --
  # and on ADAE the pair is TRT01A and SAFFL, i.e. exactly the columns this
  # block exists to supply.
  add <- setdiff(names(population), names(events))

  missing <- population[!population[[id]] %in% events[[id]], , drop = FALSE]

  if (length(add)) {
    events <- merge(
      events, population[, c(id, add), drop = FALSE],
      by = id, all.x = TRUE, sort = FALSE
    )
  }

  # rbind through a common column set: the appended rows have no event columns
  # and must arrive as NA, not as a recycled value.
  cols <- union(names(events), names(missing))
  fill <- function(d) {
    # rep() rather than a bare NA: assigning a length-1 value into a ZERO-row
    # frame errors ("replacement has 1 row, data has 0"), which is exactly the
    # no-op case where every subject already has an event.
    for (nm in setdiff(cols, names(d))) d[[nm]] <- rep(NA, nrow(d))
    d[, cols, drop = FALSE]
  }
  out <- rbind(fill(events), fill(missing))
  rownames(out) <- NULL
  out
}

#' Population join block
#'
#' Two inputs: `data`, the events, and `population`, the subject-level table
#' they came from. Appends the subjects the events never mention, so a
#' downstream chart or table can divide by the population rather than by
#' whoever happens to appear in the event rows. See [join_population()] for the
#' exact rule.
#'
#' # Where it goes
#'
#' **Last in the branch, immediately before the consumer.** Everything that
#' filters events -- the flag filter, value filters, a term cut -- belongs
#' upstream of it. A filter placed *after* it would delete the appended rows
#' and put the denominator quietly back to subjects-with-an-event, which is
#' the bug this block exists to fix. That single link is the whole contract.
#'
#' # What to do downstream
#'
#' * a chart: `func = "pct_distinct"` with `na_group = "drop"`, so the
#'   appended rows draw no bar and still count
#' * a summary table: derive the denominator by taking one row per subject
#'   (the frame is unique on `id` once deduplicated, and every subject carries
#'   the population's columns)
#'
#' Both give the same N, which is the point of defining it once per branch.
#'
#' @param id Subject identifier present in both inputs. Default `"USUBJID"`.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_population_join_block(),
#'     data = list(data = my_adae, population = my_adsl)
#'   )
#' }
#'
#' @export
new_population_join_block <- function(id = "USUBJID", ...) {

  # NOT `id`: the server's first parameter is the MODULE id, so a formal of
  # the same name is shadowed the moment the closure is entered and the
  # constructor argument becomes unreachable. blockr.core restores a block by
  # re-calling the constructor with the saved state, so shadowing it does not
  # look like a bug -- it looks like a block that quietly forgets its setting.
  # The state key must stay `id` to match this formal, hence the rename here
  # rather than in the signature.
  init_id <- as.character(id %||% "")[1L]
  if (is.na(init_id)) init_id <- ""

  blockr.core::new_transform_block(
    server = function(id, data, population) {
      shiny::moduleServer(id, function(input, output, session) {

        r_id <- shiny::reactiveVal(init_id)

        # Whether the CURRENT identifier is usable against the CURRENT inputs.
        # Kept as a reactiveVal so `expr` reads no data: blockr.core re-runs a
        # block whenever its expression is a new object, and an expr touching
        # data() would rebuild the chain on every upstream blip. Same
        # discipline as the flag filter's shape_rv.
        r_ok <- shiny::reactiveVal(FALSE)

        usable <- function(x, choices) {
          length(x) == 1L && !is.na(x) && nzchar(x) && x %in% choices
        }

        # The identifier has to exist in BOTH inputs, so the choices are the
        # intersection. Keep the current pick when it survives a data change;
        # a board that restored with a valid id must not have it swapped out
        # from under it by the first observer to fire.
        shiny::observeEvent(list(data(), population()), {
          d <- tryCatch(data(), error = function(e) NULL)
          p <- tryCatch(population(), error = function(e) NULL)
          if (!is.data.frame(d) || !is.data.frame(p)) return()
          choices <- intersect(names(d), names(p))
          cur <- shiny::isolate(r_id())
          r_ok(usable(cur, choices))
          # An empty intersection does not wipe a good value. Data arrives
          # late, and a panel not yet opened can see an empty frame first;
          # clearing the setting there would lose it for good, because the
          # clear is what gets saved. `r_ok` is what stops the join running on
          # an identifier the inputs do not actually have.
          if (!length(choices)) return()
          sel <- if (usable(cur, choices)) cur else choices[[1L]]
          shiny::updateSelectInput(session, "id", choices = choices,
                                   selected = sel)
          if (!identical(sel, cur)) r_id(sel)
          r_ok(TRUE)
        })

        # The guard makes ignoreInit redundant: the server's own
        # updateSelectInput echo arrives equal to r_id() and writes nothing.
        shiny::observeEvent(input$id, {
          if (!identical(input$id, shiny::isolate(r_id()))) r_id(input$id)
        })

        list(
          expr = shiny::reactive({
            key <- r_id()
            # Both inputs go in as `.()` slots. A bare `population` symbol
            # works in the app (the eval env binds it) and breaks in EXPORTED
            # code, which is the trap data_slot() was written for -- see its
            # note in flag-filter-block.R.
            d <- data_slot()
            p <- input_slot("population")
            # Unset, or naming a column the inputs do not share: pass the
            # events through untouched. An unconfigured block returns its
            # input, it does not error (blockr.docs
            # design-system/ux-principles.md), and erroring here would also
            # make a board unopenable while its data was still loading.
            if (is.null(key) || !nzchar(key) || !isTRUE(r_ok())) {
              return(bquote(dplyr::filter(.(d), TRUE), list(d = d)))
            }
            bquote(
              blockr.pharma::join_population(.(d), .(p), id = .(key)),
              list(d = d, p = p, key = key)
            )
          }),
          state = list(id = r_id)
        )
      })
    },
    ui = function(id) {
      shiny::tagList(
        shiny::div(
          class = "block-container",
          shiny::selectInput(
            inputId = shiny::NS(id, "id"),
            label = "Subject identifier",
            choices = character(),
            selected = NULL
          )
        )
      )
    },
    dat_valid = function(data, population) {
      if (!is.data.frame(data)) {
        stop("`data` must be a data frame of events. Flatten a dm first.")
      }
      if (!is.data.frame(population)) {
        stop("`population` must be a subject-level data frame, e.g. ADSL.")
      }
    },
    class = "population_join_block",
    expr_type = "bquoted",
    allow_empty_state = "id",
    ...
  )
}
