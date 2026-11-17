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

  # Held before `events` is rebuilt below; see the last line.
  input <- events
  column_attrs <- join_population_column_attrs(events, population)

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
  # merge() and rbind() return plain vectors, so every column lost its
  # `label` and any blockr grouping metadata. Put those attributes back,
  # preferring population attributes for stamped group columns because that is
  # where custom pool definitions live.
  out <- restore_population_column_attrs(out, column_attrs)
  # merge() and rbind() drop the filter trail the inputs carried, and the
  # tables downstream build their "Filtered:" footnote from it. Both trails
  # are kept. The population can be filtered on its own branch (a safety
  # flag on ADSL), and a board that wires the two inputs the other way round
  # still gets the events' filters. The trail is keyed by block, so the
  # global filter's clauses, which reach both sides, appear once.
  # They drop the provenance too (dataset name and note for download names
  # and footers). It is taken from the events, else the population.
  attr(out, "blockr_provenance") <-
    attr(input, "blockr_provenance", exact = TRUE) %||%
    attr(population, "blockr_provenance", exact = TRUE)
  # And the column kinds (blockr.viz::mark_column_kinds()), which decide what
  # a block's menus offer. Both inputs' marks, the events' first and in their
  # order, which is the order a kind's columns are offered in.
  kinds <- join_kinds(input, population)
  if (length(kinds)) attr(out, "blockr_kinds") <- kinds
  trail <- join_trails(input, population)
  for (key in names(trail)) {
    out <- blockr.dm::add_filter_trail(out, key, trail[[key]])
  }
  out
}

#' Restore the absent subjects, taking both tables from a dm
#'
#' The dm form of [join_population()]: takes the events table and the dm's
#' subject-level table and joins them, so a board needs no pull or flatten in
#' front of the join. Anything that filtered the dm upstream (a population
#' filter, a flag on ADSL) reaches both the events and the denominator.
#'
#' @param data A [dm][dm::dm()].
#' @param table Name of the events table. `NULL` takes `adae`, else `ae`,
#'   else the first table with a foreign key to the population table.
#' @param population Name of the subject-level table. `NULL` takes `adsl`,
#'   else the table `table` points to by foreign key.
#' @param id Subject identifier. `NULL` takes the foreign key between the two
#'   tables, else `"USUBJID"`, else the first column they share.
#'
#' @return A data frame, as [join_population()].
#'
#' @examples
#' adsl <- data.frame(USUBJID = c("S1", "S2", "S3"), TRT01A = "A")
#' adae <- data.frame(USUBJID = c("S1", "S1", "S2"),
#'                    AEDECOD = c("Diarrhoea", "Diarrhoea", "Nausea"))
#' join_population_dm(dm::dm(adsl, adae), "adae")
#'
#' @export
join_population_dm <- function(data, table = NULL, population = NULL,
                               id = NULL) {

  if (!inherits(data, "dm")) {
    stop("`data` must be a dm.", call. = FALSE)
  }
  tbls <- dm::dm_get_tables(data)

  population <- population %||% dm_population_table(data)
  if (is.null(population) || !population %in% names(tbls)) {
    stop("The dm has no subject-level table (adsl).", call. = FALSE)
  }
  table <- table %||% dm_events_default(data, population)
  if (is.null(table) || !table %in% setdiff(names(tbls), population)) {
    stop("The dm carries no events table \"", table %||% "", "\".",
         call. = FALSE)
  }

  # A dm keeps the filter trail and provenance on each table, so both come
  # along with the tables and join_population() merges them.
  events <- tbls[[table]]
  pop <- tbls[[population]]

  id <- id %||% dm_population_id(data, table, population, events, pop)
  join_population(events, pop, id = id)
}

# The subject-level table: adsl by name, else the one table every other
# table's foreign keys point to.
dm_population_table <- function(data, table = NULL) {
  nms <- names(dm::dm_get_tables(data))
  hit <- nms[tolower(nms) == "adsl"]
  if (length(hit)) return(hit[[1L]])
  fks <- dm::dm_get_all_fks(data)
  if (!is.null(table)) fks <- fks[fks$child_table == table, , drop = FALSE]
  parents <- unique(fks$parent_table)
  if (length(parents) == 1L) parents else NULL
}

# The events table a fresh block starts on.
dm_events_default <- function(data, population) {
  nms <- setdiff(names(dm::dm_get_tables(data)), population)
  for (cand in c("adae", "ae")) {
    hit <- nms[tolower(nms) == cand]
    if (length(hit)) return(hit[[1L]])
  }
  fks <- dm::dm_get_all_fks(data)
  kids <- intersect(fks$child_table[fks$parent_table == population], nms)
  if (length(kids)) return(kids[[1L]])
  if (length(nms)) nms[[1L]] else NULL
}

# The identifier: the foreign key when it is one column of the same name on
# both sides, else USUBJID, else the first shared column.
dm_population_id <- function(data, table, population, events, pop) {
  shared <- intersect(names(events), names(pop))
  fks <- dm::dm_get_all_fks(data)
  fk <- fks[fks$child_table == table & fks$parent_table == population, ,
            drop = FALSE]
  if (nrow(fk)) {
    child <- fk$child_fk_cols[[1L]]
    parent <- fk$parent_key_cols[[1L]]
    if (length(child) == 1L && identical(child, parent) && child %in% shared) {
      return(child)
    }
  }
  if ("USUBJID" %in% shared) return("USUBJID")
  if (length(shared)) return(shared[[1L]])
  stop("Tables \"", table, "\" and \"", population,
       "\" share no subject identifier.", call. = FALSE)
}

# The two inputs' column kinds as one, events first, each column once.
join_kinds <- function(events, population) {
  kinds <- c(attr(events, "blockr_kinds", exact = TRUE),
             attr(population, "blockr_kinds", exact = TRUE))
  if (!length(kinds) || is.null(names(kinds))) return(NULL)
  kinds[!duplicated(names(kinds))]
}

# The two inputs' trails as one, events first, each key once.
join_trails <- function(events, population) {
  trail <- c(blockr.dm::filter_trail(events),
             blockr.dm::filter_trail(population))
  trail[!duplicated(names(trail))]
}

join_population_column_attrs <- function(events, population) {
  cols <- union(names(events), names(population))
  stats::setNames(lapply(cols, function(nm) {
    event_attrs <- if (nm %in% names(events)) attributes(events[[nm]]) else NULL
    pop_attrs <- if (nm %in% names(population)) attributes(population[[nm]]) else NULL

    if (!is.null(pop_attrs$blockr_groups) || !is.null(pop_attrs$blockr_source)) {
      return(pop_attrs)
    }
    event_attrs %||% pop_attrs
  }), cols)
}

restore_population_column_attrs <- function(data, column_attrs) {
  for (nm in intersect(names(column_attrs), names(data))) {
    attrs <- column_attrs[[nm]]
    if (!length(attrs)) next
    for (attr_nm in setdiff(names(attrs), c("names", "row.names", "class", "levels"))) {
      attr(data[[nm]], attr_nm) <- attrs[[attr_nm]]
    }
  }
  data
}

#' Population join block
#'
#' Appends the subjects the events never mention, so a downstream chart or
#' table can divide by the population rather than by whoever happens to appear
#' in the event rows. See [join_population()] for the exact rule.
#'
#' # Two ways in
#'
#' * **A dm on `data`.** The block takes the events table named by `table`
#'   and the dm's subject-level table itself ([join_population_dm()]), and
#'   `population` stays unconnected. The panel shows an events table select;
#'   the population table and the identifier come from the dm.
#' * **Two data frames.** `data` is the events, `population` the
#'   subject-level table they came from. The panel shows the identifier.
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
#'   Used for two data frames; with a dm the identifier comes from the dm.
#' @param table With a dm on `data`: the events table. `NULL` (default)
#'   starts on `adae`, else `ae`. Ignored for data frames.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_population_join_block(),
#'     data = list(data = my_adae, population = my_adsl)
#'   )
#'   serve(
#'     new_population_join_block(table = "adae"),
#'     data = list(data = my_dm)
#'   )
#' }
#'
#' @export
new_population_join_block <- function(id = "USUBJID", table = NULL, ...) {

  # NOT `id`: the server's first parameter is the MODULE id, so a formal of
  # the same name is shadowed the moment the closure is entered and the
  # constructor argument becomes unreachable. blockr.core restores a block by
  # re-calling the constructor with the saved state, so shadowing it does not
  # look like a bug -- it looks like a block that quietly forgets its setting.
  # The state key must stay `id` to match this formal, hence the rename here
  # rather than in the signature.
  init_id <- as.character(id %||% "")[1L]
  if (is.na(init_id)) init_id <- ""
  init_table <- as.character(table %||% "")[1L]
  if (is.na(init_table)) init_table <- ""

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

        # What the picker offers. It drives a RENDERED control rather than an
        # updateSelectInput() push -- see the renderUI below for why.
        r_choices <- shiny::reactiveVal(character())

        # dm mode. `r_dm` says which form the input has, `r_table` is the
        # events table and `r_tables` what its select offers. Held as
        # reactiveVals for the same reason as `r_ok`: `expr` reads no data.
        r_dm <- shiny::reactiveVal(FALSE)
        r_table <- shiny::reactiveVal(init_table)
        r_tables <- shiny::reactiveVal(character())

        # Its own observer, on `data` alone: with a dm, `population` is
        # unconnected and an event expression reading it would never fire.
        shiny::observeEvent(data(), {
          d <- tryCatch(data(), error = function(e) NULL)
          is_dm <- inherits(d, "dm")
          if (!identical(shiny::isolate(r_dm()), is_dm)) r_dm(is_dm)
          if (!is_dm) return()
          pop <- dm_population_table(d)
          choices <- setdiff(names(dm::dm_get_tables(d)), pop)
          if (!length(choices)) return()
          r_tables(choices)
          cur <- shiny::isolate(r_table())
          if (!usable(cur, choices)) {
            sel <- if (is.null(pop)) choices[[1L]] else
              dm_events_default(d, pop)
            r_table(sel)
          }
        })

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
          if (inherits(d, "dm")) return()
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
          r_choices(choices)
          if (!identical(sel, cur)) r_id(sel)
          r_ok(TRUE)
        })

        # The control is RENDERED, and that is the whole point of it.
        #
        # A block sitting on a dock panel nobody has opened still runs its
        # server -- a visible block downstream needs its data -- while its UI
        # does not exist yet. `updateSelectInput()` resolves against a bound
        # element and is dropped, silently, when there is none. So the block
        # came up correct and its picker came up EMPTY, and the first thing
        # that empty picker did on being opened was report "" back (below).
        #
        # Shiny suspends a render whose output is not on the page and runs it
        # when the element binds, so a rendered control arrives configured on
        # first paint however late that is. It is the only mechanism that
        # covers this for a block with no JS of its own to announce itself.
        # `isolate()` on the selection keeps the widget from being rebuilt
        # under the user's cursor on their own pick.
        output$control <- shiny::renderUI({
          if (r_dm()) {
            tables <- r_tables()
            tbl <- shiny::isolate(r_table())
            if (!length(tables) && nzchar(tbl)) tables <- tbl
            return(shiny::selectInput(
              inputId = session$ns("table"),
              label = "Events table",
              choices = tables,
              selected = if (nzchar(tbl)) tbl
            ))
          }
          choices <- r_choices()
          sel <- shiny::isolate(r_id())
          # No data yet: show the identifier the block holds rather than an
          # empty box, so the panel never renders a picker that disagrees
          # with the block.
          if (!length(choices) && nzchar(sel)) choices <- sel
          shiny::selectInput(
            inputId = session$ns("id"),
            label = "Subject identifier",
            choices = choices,
            selected = if (nzchar(sel)) sel
          )
        })

        # Stock inputs echo: a select reports its value the first time it
        # binds, and that is the widget talking about itself, not a user
        # decision. Only a value the current choices actually OFFER counts as
        # a pick. Comparing against r_id() alone is not enough -- the echo
        # from an empty select is "", which differs from a restored
        # identifier and would be adopted, taking the population back out of
        # the frame and saving the clear.
        shiny::observeEvent(input$id, {
          incoming <- as.character(input$id)[1L]
          if (identical(incoming, shiny::isolate(r_id()))) return()
          if (!usable(incoming, shiny::isolate(r_choices()))) return()
          r_id(incoming)
          r_ok(TRUE)
        })

        # Same echo guard for the table select.
        shiny::observeEvent(input$table, {
          incoming <- as.character(input$table)[1L]
          if (identical(incoming, shiny::isolate(r_table()))) return()
          if (!usable(incoming, shiny::isolate(r_tables()))) return()
          r_table(incoming)
        })

        list(
          expr = shiny::reactive({
            if (r_dm()) {
              tbl <- r_table()
              # An unset table is resolved by the helper, so a fresh block
              # on a dm runs before anyone opens its panel.
              return(bquote(
                blockr.pharma::join_population_dm(.(d), table = .(tbl)),
                list(d = data_slot(), tbl = if (nzchar(tbl)) tbl)
              ))
            }
            key <- r_id()
            # Both inputs go in as `.()` slots. A bare `population` symbol
            # works in the app (the eval env binds it) and breaks in EXPORTED
            # code, which is the trap data_slot() was written for -- see its
            # note in flag-filter-block.R.
            d <- data_slot()
            p <- input_slot("population")
            # Unset, or naming a column the inputs do not share: pass the
            # events through untouched. An unconfigured block returns its
            # input, it does not error, and erroring here would also make a
            # board unopenable while its data was still loading.
            if (is.null(key) || !nzchar(key) || !isTRUE(r_ok())) {
              return(bquote(dplyr::filter(.(d), TRUE), list(d = d)))
            }
            bquote(
              blockr.pharma::join_population(.(d), .(p), id = .(key)),
              list(d = d, p = p, key = key)
            )
          }),
          state = list(id = r_id, table = r_table)
        )
      })
    },
    ui = function(id) {
      shiny::tagList(
        shiny::div(
          class = "block-container",
          shiny::uiOutput(shiny::NS(id, "control"))
        )
      )
    },
    dat_valid = function(data, population = NULL) {
      if (inherits(data, "dm")) {
        if (!is.null(population)) {
          stop("With a dm on `data` the population comes from the dm. ",
               "Disconnect the `population` input.")
        }
        return(invisible())
      }
      if (!is.data.frame(data)) {
        stop("`data` must be a data frame of events, or a dm.")
      }
      if (!is.data.frame(population)) {
        stop("`population` must be a subject-level data frame, e.g. ADSL.")
      }
    },
    class = "population_join_block",
    expr_type = "bquoted",
    allow_empty_state = list(input = c("id", "table"), data = "population"),
    ...
  )
}
