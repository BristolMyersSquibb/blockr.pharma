# Flag filter block --------------------------------------------------------
#
# WHY THIS EXISTS, and why it is not in blockr.dm.
#
# ADaM encodes booleans as text: "Y" against "N", "" or NA, because SAS has no
# boolean type and the XPT transport format carries only numeric and character.
# blockr.dm's value filter renders a value SELECT for those, where the only
# sensible pick is "Y".
#
# We tried teaching the value filter to spot the vocabulary and render a
# checkbox. That failed structurally (blockr.dm e71fd0b): it derived the
# CONTROL from the column's VALUES, and values change at runtime, so an
# upstream filter that emptied the frame flipped the checkbox to a select --
# and the two renderings disagree about what an empty selection MEANS, so an
# unchecked box silently became a real constraint.
#
# This block cannot have that bug, because a checkbox is its ONLY rendering.
# Empty has one meaning here: no constraint. That is the whole reason the
# domain-specific block is the right home rather than a general one.

#' Flag filter block
#'
#' Filters rows on a family of ADaM-style indicator columns. Each chosen
#' column gets an include-style checkbox: ticked means "keep the rows carrying
#' this flag", unticked means **no constraint from this column** -- never
#' "exclude them". Ticked boxes **union**, so a family of period flags
#' (`PREFL` / `TRTEMFL` / `FUPFL`) answers "any of these periods" in one
#' control.
#'
#' The union is the point. Period flags are mutually exclusive by
#' construction, so combining them with AND returns zero rows; and a record
#' can legitimately carry two flags (an AE starting after the last dose can
#' still be treatment-emergent), which a union counts once.
#'
#' # Why unticked is never a negative
#'
#' A flag has up to four non-affirmative states: `"N"`, `"U"`, `""` and `NA`,
#' which mean explicitly-no, unknown, and two flavours of not-recorded.
#' `AESER == ""` may mean "not serious" or "never assessed". A control that
#' only ever ADDS an inclusion never has to choose between them. To ask for
#' the negative, or to separate "assessed as N" from "never assessed", use
#' [blockr.dm::new_value_filter_block()] and pick values directly.
#'
#' # Filtering one table of a dm
#'
#' `table` exists because a patient profile takes a `dm`, not a flat frame,
#' so a filter that only speaks data frames can never reach it. What it must
#' not do is reduce the cohort: getting rid of the non-treatment-emergent AEs
#' is the point, getting rid of the patients who have none is a bug. That
#' rules out `dm::dm_filter()`, whose FK cascade does exactly that -- see
#' `flag_zoom_expr()`.
#'
#' Handed a dm, the gear shows a Table select over the dm's tables that carry
#' a Y/N flag column. A block added from the UI starts on `adae` when the dm
#' has it, else on the first such table. Changing the table clears the ticks
#' and lists the new table's flags. The table is saved with the board.
#'
#' In dm mode the block previews as blockr.dm's dm diagram, click a table to
#' page through it, because the table preview cannot render a dm. The clause
#' the block writes to the filter trail goes on `table` only, so a branch
#' that leaves the dm through another table does not print it; see
#' [blockr.dm::filter_trail()].
#'
#' # What the caption says
#'
#' The clause written to the filter trail, and printed by a `{filters}`
#' caption, names each ticked flag by its footnote text from `footnotes`,
#' falling back to the column's `label` attribute and then to its name.
#' Ticked flags are joined with "or". The footnote text is study-specific
#' (the definition of "treatment-emergent" differs between studies), so it is
#' block state, set in the gear and saved with the board.
#'
#' # What a ticked box emits
#'
#' * a `logical` column: `col %in% TRUE`
#' * anything else: `col %in% c("Y", "y")`
#'
#' The choice is made on the column's TYPE, never on its values, so it cannot
#' change when the rows change. Point the block at a column that is not a flag
#' and you get a checkbox that matches nothing: zero rows, with the emitted
#' expression on screen saying why. That is deliberate, not a guard we forgot.
#'
#' @param columns Character vector of flag column names. All of them union.
#'   To AND a flag against something else, chain a second filter block.
#' @param selected Character vector of the columns whose box starts ticked.
#'   Defaults to none, which passes every row through.
#' @param footnotes Named character vector (or list) of footnote text keyed
#'   by column name, e.g. `c(TRTEMFL = "Treatment-emergent (AEs ...)")`. A
#'   flag without an entry reads as its label in the filter trail.
#' @param table Name of a table in an incoming `dm` to filter. With a dm the
#'   block is dm-in, dm-out: that table is narrowed and every other one
#'   passes through with its rows and keys intact. One table per block; chain
#'   a second to narrow another. `NULL` (the default) filters a data frame,
#'   or, handed a dm, starts on the default table.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_flag_filter_block(
#'       columns  = c("PREFL", "TRTEMFL", "FUPFL"),
#'       selected = "TRTEMFL",
#'       footnotes = c(TRTEMFL = "Treatment-emergent")
#'     ),
#'     data = list(data = my_adae)
#'   )
#' }
#'
#' @importFrom blockr.dm new_value_filter_block
#' @export
new_flag_filter_block <- function(columns = character(),
                                  selected = character(),
                                  footnotes = character(),
                                  table = NULL,
                                  ...) {
  columns <- as.character(columns %||% character())
  selected <- intersect(as.character(selected %||% character()), columns)
  footnotes <- flag_clean_footnotes(footnotes)
  table <- flag_validate_table(table)

  blockr.core::new_transform_block(
    function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        r_cols <- shiny::reactiveVal(columns)
        r_sel <- shiny::reactiveVal(selected)
        r_notes <- shiny::reactiveVal(footnotes)
        r_table <- shiny::reactiveVal(table)

        # Echo guard: a state change that came FROM the client must not be
        # pushed back, or the widget fights the user mid-click.
        self_write <- new.env(parent = emptyenv())
        self_write$active <- FALSE

        input_data <- function() tryCatch(data(), error = function(e) NULL)

        # Everything that INSPECTS the input goes through here, so dm mode
        # and frame mode differ in exactly one place.
        target <- function() {
          d <- input_data()
          flag_target_df(d, flag_resolve_table(d, r_table()))
        }

        # `meta_only` carries the labels and counts WITHOUT the column set:
        # the client already has that set, having just picked it, and echoing
        # it back would rebuild the picker under the user's cursor.
        # `tables` is NULL for a data frame, which hides the Table select.
        push_meta <- function(meta_only = FALSE) {
          raw <- input_data()
          d <- flag_target_df(raw, flag_resolve_table(raw, r_table()))
          is_dm <- inherits(raw, "dm")
          session$sendCustomMessage(
            "pharma-flag-meta",
            list(
              id        = ns("flags"),
              columns   = flag_column_meta(d, r_cols()),
              choices   = flag_choice_meta(d),
              selected  = as.list(r_sel()),
              footnotes = r_notes(),
              table     = if (is_dm) r_table(),
              tables    = if (is_dm) as.list(flag_tables(raw, r_table())),
              meta_only = meta_only
            )
          )
        }

        # Point the block at another table of the dm. The ticks belong to the
        # old table, so they go, and the flags listed are the new table's.
        # Going back to a data frame keeps the columns: `table = NULL` is
        # frame mode, and a frame has no Table select to set it back with.
        switch_table <- function(tbl) {
          old <- r_table()
          if (identical(tbl, old)) return(invisible(FALSE))
          r_table(tbl)
          if (!is.null(tbl) && (!is.null(old) || !length(r_cols()))) {
            r_cols(flag_flag_columns(flag_target_df(input_data(), tbl)))
            r_sel(character())
          }
          invisible(TRUE)
        }

        # A block added from the UI starts with `table = NULL`; handed a dm it
        # settles on the default table, handed a frame after a dm it drops
        # the table. A saved table the dm no longer carries is kept, so the
        # error says which one is gone and the select can repoint it.
        shiny::observeEvent(data(), {
          switch_table(flag_resolve_table(input_data(), r_table()))
        }, priority = 10)

        # The TRUE union count, not the client's sum of per-flag counts: a
        # row carrying two flags is one row, so summing overshoots and can
        # print more rows than the table has. Pushed on every change,
        # including the client's own, which the metadata echo deliberately
        # suppresses.
        push_counts <- function() {
          d <- target()
          session$sendCustomMessage(
            "pharma-flag-counts",
            list(
              id      = ns("flags"),
              matched = flag_matched_rows(d, r_sel()),
              total   = if (is.data.frame(d)) nrow(d) else NULL
            )
          )
        }

        shiny::observeEvent(data(), {
          push_meta()
          push_counts()
        })

        shiny::observeEvent(list(r_sel(), r_table()), push_counts(),
                            ignoreInit = TRUE)

        # The binding announces itself on every (re)bind. In a deferred dock
        # panel the block's script arrives WITH the panel on first visit, so
        # anything pushed before that had no handler and was dropped outright.
        shiny::observeEvent(input$flags_ready, {
          push_meta()
          push_counts()
        })

        shiny::observeEvent(input$flags, {
          incoming <- input$flags
          # A table picked in the gear resets columns and ticks server-side,
          # so the rest of this payload, which describes the old table, is
          # ignored. The full metadata push rebuilds the client.
          tbl <- flag_validate_table(incoming$table)
          if (!is.null(tbl) && !identical(tbl, r_table()) &&
                inherits(input_data(), "dm")) {
            self_write$active <- FALSE
            switch_table(tbl)
            push_meta()
            return()
          }
          cols <- as.character(unlist(incoming$columns %||% list()))
          sel <- as.character(unlist(incoming$selected %||% list()))
          # Text only, so it is not part of the echo guard below: nothing is
          # pushed back when it changes, and the field keeps its cursor.
          r_notes(flag_clean_footnotes(incoming$footnotes))
          sel <- intersect(sel, cols)
          # Arm the guard only when the observer below will fire, or a
          # footnote-only edit leaves it armed for the next change.
          if (identical(cols, r_cols()) && identical(sel, r_sel())) return()
          self_write$active <- TRUE
          r_cols(cols)
          r_sel(sel)
        })

        shiny::observeEvent(list(r_cols(), r_sel(), r_table()), {
          if (self_write$active) {
            self_write$active <- FALSE
            # A column the user just added through the gear has no label and
            # no count on the client -- only the server can supply them. So
            # the echo guard suppresses the COLUMN SET, not the metadata;
            # returning here left every gear-picked column bare.
            push_meta(meta_only = TRUE)
            return()
          }
          push_meta()
        }, ignoreInit = TRUE)

        # Derive the expression SHAPE in an observer, so `expr` reads only
        # reactiveVals. blockr.core skips re-evaluating a block only when the
        # expression it is handed is the SAME OBJECT as last time, and
        # bquote() allocates a fresh call tree on every read, so an `expr`
        # that touched data() would re-run the whole chain on every spurious
        # upstream invalidation. Same discipline as blockr.dm's value filter.
        #
        # The table is resolved here from the data, not read from `r_table`
        # alone, so the expression never pairs a frame with a table (or a dm
        # with none) in the flush before the data observer above catches up.
        shape_rv <- shiny::reactiveVal(NULL)
        shiny::observe({
          shape_rv(
            tryCatch({
              d <- input_data()
              tbl <- flag_resolve_table(d, r_table())
              flag_check_input(d, tbl)
              list(ok = TRUE, table = tbl,
                   shape = flag_input_shape(flag_target_df(d, tbl)))
            }, error = function(e) list(ok = FALSE, cond = e))
          )
        })

        list(
          expr = shiny::reactive({
            derived <- shape_rv()
            shiny::req(derived)
            if (!isTRUE(derived$ok)) stop(derived$cond)
            make_flag_filter_expr(r_sel(), derived$shape, derived$table,
                                  trail_key(session), r_notes())
          }),
          # `table` is state so a saved board comes back on the same table.
          # NULL is frame mode.
          state = list(columns = r_cols, selected = r_sel,
                       footnotes = r_notes, table = r_table)
        )
      })
    },
    function(id) {
      shiny::tagList(
        blockr.dplyr::blockr_core_js_dep(),
        blockr.dplyr::blockr_blocks_css_dep(),
        blockr.dplyr::blockr_select_dep(),
        flag_filter_block_dep(),
        shiny::div(
          class = "block-container",
          shiny::div(id = shiny::NS(id, "flags"), class = "ffb-container")
        )
      )
    },
    # The table is picked at runtime, so this sees neither the table nor the
    # mode; it only rules out inputs no mode can read. The table checks are
    # in flag_check_input(), which the server runs against the live table.
    dat_valid = function(data) {
      if (!is.data.frame(data) && !inherits(data, "dm")) {
        stop("Input must be a data frame or a dm.")
      }
      invisible(NULL)
    },
    # The preview follows the result, as in blockr.dm's dual-mode blocks: a
    # dm draws blockr.dm's diagram, a frame the ordinary table. See
    # block_output.flag_filter_block().
    class = "flag_filter_block",
    expr_type = "bquoted",
    allow_empty_state = c("columns", "selected", "footnotes", "table"),
    ...
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Every column name in the upstream data, for the gear's picker.
#'
#' Deliberately UNFILTERED (decision 1a): the block cannot tell a flag from
#' any other column without reading values, and doing that would make the
#' picker go empty whenever the upstream is empty. Offering everything means
#' the block stays configurable no matter what the data is doing.
#' @noRd
flag_all_columns <- function(data) {
  if (!is.data.frame(data)) return(character())
  names(data)
}

#' Every upstream column as a `{value, label}` option, so the gear's picker
#' shows the ADaM label beside the name the way blockr.dm's value filter
#' does. A bare string option would render the name alone.
#' @noRd
flag_choice_meta <- function(data) {
  lapply(flag_all_columns(data), function(cn) {
    list(value = cn, label = flag_column_label(data[[cn]]))
  })
}

#' One column's label attribute, "" when it has none.
#' @noRd
flag_column_label <- function(col) {
  lbl <- attr(col, "label", exact = TRUE)
  if (is.null(lbl)) "" else as.character(lbl)[1L]
}

#' Per-column metadata the client renders: label, and the count of rows the
#' box would keep if ticked.
#' @noRd
flag_column_meta <- function(data, columns) {
  lapply(columns, function(cn) {
    out <- list(name = cn, label = "", count = NA, total = NA)
    if (!is.data.frame(data) || !cn %in% names(data)) return(out)
    col <- data[[cn]]
    out$label <- flag_column_label(col)
    out$total <- nrow(data)
    out$count <- sum(flag_is_true(col), na.rm = TRUE)
    out
  })
}

#' Rows kept by the current selection. No selection is every row, matching
#' the pass-through expression.
#' @noRd
flag_matched_rows <- function(data, selected) {
  if (!is.data.frame(data)) return(NULL)
  selected <- intersect(as.character(selected %||% character()), names(data))
  if (length(selected) == 0L) return(nrow(data))
  keep <- Reduce(`|`, lapply(selected, function(cn) flag_is_true(data[[cn]])))
  sum(keep, na.rm = TRUE)
}

#' Which rows carry the flag. Keyed on the column's TYPE, so it matches what
#' [flag_condition_expr()] emits.
#' @noRd
flag_is_true <- function(col) {
  if (is.logical(col)) return(col %in% TRUE)
  as.character(col) %in% FLAG_AFFIRMATIVE
}

# Deliberately narrow, and deliberately NOT derived from the data: a literal
# set means the emitted expression cannot change when the rows change.
FLAG_AFFIRMATIVE <- c("Y", "y")  # nolint: object_name_linter.

#' Condition for one ticked box.
#' @noRd
flag_condition_expr <- function(name, df) {
  sym <- as.name(name)
  is_lgl <- is.data.frame(df) && name %in% names(df) && is.logical(df[[name]])
  if (is_lgl) bquote(.(sym) %in% TRUE) else bquote(.(sym) %in% .(FLAG_AFFIRMATIVE))
}

#' 0-row template of the input, enough to read column types without touching
#' the rows.
#'
#' Base `[` drops the attributes of a plain vector, so the `label` the filter
#' trail clause falls back to is copied back onto the empty columns. See
#' [flag_filter_clause()].
#' @noRd
flag_input_shape <- function(data) {
  if (!is.data.frame(data)) return(NULL)
  data <- as.data.frame(data)
  shape <- data[0L, , drop = FALSE]
  for (i in seq_along(shape)) {
    attr(shape[[i]], "label") <- attr(data[[i]], "label", exact = TRUE)
  }
  shape
}

# The data SLOT, as blockr.core's `.()` placeholder.
#
# `expr_type = "bquoted"` means blockr.core substitutes `.()` placeholders and
# NOTHING else. A bare `data` symbol resolves at RUNTIME (the block's eval env
# binds it) and looks fine in the app, but the EXPORTED code keeps the bare
# name: in a generated script it falls through to utils::data, the chunk
# "succeeds" holding a function, and every dependent dies with "no applicable
# method for 'filter' applied to an object of class \"function\"". Same helper
# blockr.dm's filters carry, and not exported from blockr.core.
#' @noRd
data_slot <- function() {
  input_slot("data")
}

#' The same, for a block with more than one data input (see
#' [new_population_join_block()], whose second input is `population`).
#' @noRd
input_slot <- function(name) {
  call(".", as.name(name))
}

#' Union of the ticked boxes. No ticks is no constraint, NOT an empty result.
#'
#' With `table` set the same condition is applied to ONE table of a dm and
#' every other table passes through untouched. See [flag_zoom_expr()] for why
#' that is not `dm::dm_filter()`.
#' @noRd
# `key` is the filter-trail key, NOT the `id` the block server is handed:
# blockr.core calls every block's server with `id = "expr"`, so keying on it
# would have each filter overwrite the one upstream of it. See `trail_key()`.
make_flag_filter_expr <- function(selected, shape, table = NULL, key = NULL,
                                  footnotes = NULL) {
  d <- data_slot()
  selected <- as.character(selected %||% character())
  if (!is.null(shape)) selected <- selected[selected %in% names(shape)]

  cond <- if (length(selected) == 0L) {
    TRUE
  } else {
    conds <- lapply(selected, flag_condition_expr, df = shape)
    Reduce(function(a, b) bquote(.(a) | .(b)), conds)
  }

  clause <- flag_filter_clause(selected, shape, footnotes)

  inner <- if (is.null(table)) {
    as.call(list(quote(dplyr::filter), d, cond))
  } else {
    flag_zoom_expr(d, table, cond)
  }

  # This block's entry in the filter trail (see `blockr.dm::filter_trail()`).
  # The flags are OR-ed, so the clause reads that way. Nothing selected adds
  # nothing: the trail upstream rides on the data untouched.
  #
  # In dm mode the clause goes on `table` only, the one table this block
  # narrowed, so a branch that leaves the dm through another table (the lab
  # chart, off `lb`) does not print a filter that never touched its rows. The
  # crossfilter writes on every table, because `dm_filter()` cascades.
  filter_trail_expr(inner, key, clause, tables = table)
}

# The block's expression wrapped so it writes its clause to the filter trail.
# With no clause or key, the expression as it is. Self-qualified, because the
# expression is also what an exported script runs.
filter_trail_expr <- function(inner, key, clause, tables = NULL) {
  if (is.null(key) || !length(clause) || is.na(clause[[1L]]) ||
        !nzchar(clause[[1L]])) {
    return(inner)
  }
  args <- list(quote(blockr.dm::add_filter_trail), inner, key, clause[[1L]])
  if (length(tables)) {
    args <- c(args, list(tables = as.character(tables)))
  }
  as.call(args)
}

# The key a block writes its trail clause under: its module namespace, which
# carries the board's block id and is unique per block per session. Not the
# `id` the block server gets, which blockr.core sets to "expr" for every
# block. pp_drill_state() matches the drill filter's key on the same form.
trail_key <- function(session = shiny::getDefaultReactiveDomain()) {
  ns <- tryCatch(session$ns(""), error = function(e) NULL)
  if (is.null(ns) || !length(ns) || !nzchar(ns)) "block" else ns
}

# Selected flags as one clause, which is what a `{filters}` caption prints.
# Each flag reads as its footnote text, then its label, then its name. The
# footnote text is study-specific and lives in block state; this package
# ships none. The affirmative value a flag is tested against is implied, so
# it is not repeated.
flag_filter_clause <- function(selected, shape = NULL, footnotes = NULL) {
  if (!length(selected)) {
    return(NULL)
  }
  footnotes <- flag_clean_footnotes(footnotes)
  parts <- vapply(
    selected,
    function(cn) {
      if (cn %in% names(footnotes)) return(footnotes[[cn]])
      label <- flag_column_label(shape[[cn]])
      if (is.na(label) || !nzchar(label)) cn else label
    },
    character(1L),
    USE.NAMES = FALSE
  )
  paste(parts, collapse = " or ")
}

#' Footnote text as a named list of strings, blanks dropped
#'
#' A LIST because it is block state: a named character vector loses its
#' names on the way to JSON, a named list saves as an object. Arrives as a
#' named list from the client and from a restored board, and as either from
#' R. An entry cleared in the gear is dropped here, so the flag falls back
#' to its label.
#' @noRd
flag_clean_footnotes <- function(x) {
  if (!length(x) || is.null(names(x))) {
    return(list())
  }
  x <- lapply(x, function(v) {
    v <- as.character(unlist(v, use.names = FALSE))
    if (length(v) && !is.na(v[[1L]])) trimws(v[[1L]]) else ""
  })
  x[nzchar(names(x)) & nzchar(unlist(x))]
}

#' Filter one table of a dm, and only that table
#'
#' `dm::dm_filter()` is the obvious call and the wrong one: it cascades over
#' foreign keys, so filtering `adae` down to the treatment-emergent records
#' also drops from `adsl` every patient who has none. Verified on a
#' three-patient dm -- adsl went 3 rows to 1. In a safety review those
#' patients are a finding, and the cohort list is exactly where their absence
#' would be noticed.
#'
#' Rebuilding the dm from filtered tables (what [pp_scope_subject()] does)
#' keeps the rows but drops every primary and foreign key, which is fine for
#' the profile's internal flat dm and wrong for a block whose output feeds
#' other blocks.
#'
#' `dm_zoom_to()` / `dm_update_zoomed()` is the only one that gets both
#' halves right: other tables keep their rows, and the keys survive.
#'
#' Because nothing cascades, the clause this block writes to the filter trail
#' goes on `table` only: a pull or flatten that leaves the dm through another
#' table does not carry it, so only the exhibits built on the narrowed table
#' print it.
#' See [blockr.dm::filter_trail()].
#'
#' @param d The data slot.
#' @param table Table name.
#' @param cond The filter condition (or `TRUE`).
#' @return A call.
#' @noRd
flag_zoom_expr <- function(d, table, cond) {
  bquote(
    dm::dm_update_zoomed(
      dplyr::filter(dm::dm_zoom_to(.(d), .(tbl)), .(cond))
    ),
    list(d = d, tbl = as.name(table), cond = cond)
  )
}

#' Normalize the `table` constructor argument
#'
#' `NULL` and `""` both mean "filter the data frame I am handed", which is
#' the block's original behaviour and stays the default.
#' @noRd
flag_validate_table <- function(table) {
  if (is.null(table)) return(NULL)
  table <- as.character(unlist(table, use.names = FALSE))
  table <- table[!is.na(table) & nzchar(table)]
  if (length(table) == 0L) return(NULL)
  if (length(table) > 1L) {
    stop("`table` names ONE table of the dm, got ", length(table),
         ". A second flag filter can narrow another table.", call. = FALSE)
  }
  table
}

#' Is this column a Y/N flag?
#'
#' A logical column, or a text column whose recorded values are all Y, N or
#' U (any case). Blanks and NA are not-recorded and do not count either way,
#' but a column with nothing recorded is not a flag. Used to list a dm's
#' tables and a table's flags when the block is pointed at it; the gear's
#' picker still offers every column.
#'
#' Most text columns fail on their first values, so those are checked first
#' and the full column only when they pass.
#' @noRd
flag_is_flag_col <- function(col) {
  if (is.logical(col)) return(TRUE)
  if (is.factor(col)) col <- levels(col)
  if (!is.character(col)) return(FALSE)
  ok <- function(v) {
    v <- unique(v)
    v <- v[!is.na(v) & nzchar(v)]
    if (!length(v)) NA else all(toupper(v) %in% FLAG_VOCABULARY)
  }
  head_ok <- ok(utils::head(col, 1000L))
  if (isFALSE(head_ok)) return(FALSE)
  isTRUE(ok(col))
}

FLAG_VOCABULARY <- c("Y", "N", "U")  # nolint: object_name_linter.

#' Names of the flag columns of a frame
#' @noRd
flag_flag_columns <- function(data) {
  if (!is.data.frame(data)) return(character())
  names(data)[vapply(data, flag_is_flag_col, logical(1L))]
}

#' Tables of a dm the Table select offers: those with a flag column
#'
#' `current` is kept in the list even without flags, so a restored board
#' shows the table it was saved on. Lazy tables (a dm on a database) are not
#' read.
#' @noRd
flag_tables <- function(dm, current = NULL) {
  if (!inherits(dm, "dm")) return(character())
  tbls <- dm::dm_get_tables(dm)
  has <- vapply(tbls, function(t) {
    is.data.frame(t) && length(flag_flag_columns(as.data.frame(t))) > 0L
  }, logical(1L))
  out <- names(tbls)[has]
  if (!is.null(current) && current %in% names(tbls) && !current %in% out) {
    out <- c(current, out)
  }
  out
}

#' The table a dm opens on: adae when it has flags, else the first table
#' that does, else NULL.
#' @noRd
flag_default_table <- function(dm) {
  tbls <- flag_tables(dm)
  if (!length(tbls)) return(NULL)
  adae <- tbls[tolower(tbls) == "adae"]
  if (length(adae)) adae[[1L]] else tbls[[1L]]
}

#' The table the block works on for this input
#'
#' A frame is always frame mode, so `NULL`. A dm keeps `current`, or gets
#' the default table when there is none. Anything else (no data yet) keeps
#' `current`, so a board restoring before its upstream has run does not lose
#' its table.
#' @noRd
flag_resolve_table <- function(data, current = NULL) {
  if (is.data.frame(data)) return(NULL)
  if (!inherits(data, "dm")) return(current)
  current %||% flag_default_table(data)
}

#' Raise the input errors the user can act on
#' @noRd
flag_check_input <- function(data, table) {
  if (!inherits(data, "dm")) return(invisible(NULL))
  if (is.null(table)) {
    stop("The dm carries no table with Y/N flag columns.", call. = FALSE)
  }
  if (!table %in% names(dm::dm_get_tables(data))) {
    stop("The dm carries no table \"", table, "\". Pick another in the gear.",
         call. = FALSE)
  }
  invisible(NULL)
}

#' The frame the block reads: the dm's table, or the input itself
#'
#' Returns `NULL` when there is nothing to read (no dm yet, or a dm without
#' that table), which every caller already treats as "no metadata".
#' @noRd
flag_target_df <- function(data, table = NULL) {
  if (is.null(table)) {
    return(if (is.data.frame(data)) as.data.frame(data) else NULL)
  }
  if (!inherits(data, "dm")) return(NULL)
  tbls <- dm::dm_get_tables(data)
  if (!table %in% names(tbls)) return(NULL)
  as.data.frame(tbls[[table]])
}

# The preview, chosen from the result at render time because the table, and
# with it the mode, is picked at runtime. A dm cannot go through the table
# preview: nrow(<dm>) is NULL, so the preview's slice arithmetic ends in
# `if (NA)`. A dm goes to blockr.dm's diagram (click a table to page through
# it), a frame to the board's table display, rendered as a nested output.
# The container is a uiOutput in both modes: blockr.dm's renderUI payload in
# a DT container would be bound as an htmlwidget and take down the whole
# Shiny message batch it rides in.

#' @rdname new_flag_filter_block
#' @param id Namespace ID.
#' @param x The block.
#' @importFrom blockr.core block_ui
#' @method block_ui flag_filter_block
#' @export
block_ui.flag_filter_block <- function(id, x, ...) {
  shiny::tagList(shiny::uiOutput(shiny::NS(id, "result")))
}

#' @rdname new_flag_filter_block
#' @param result Evaluation result.
#' @param session Shiny session object.
#' @importFrom blockr.core block_output
#' @method block_output flag_filter_block
#' @export
block_output.flag_filter_block <- function(x, result, session) {
  if (inherits(result, "dm")) {
    dm_output <- utils::getS3method("block_output", "dm_block",
                                    envir = asNamespace("blockr.dm"))
    return(dm_output(x, result, session))
  }
  display <- blockr.core::tabular_display()
  session$output$flag_table <- blockr.core::tabular_output(
    display, result, x, session
  )
  shiny::renderUI(
    blockr.core::tabular_ui(display, session$ns("flag_table"))
  )
}

#' @noRd
flag_filter_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "blockr-pharma-flag-filter-js",
      version = utils::packageVersion("blockr.pharma"),
      src = system.file("js", package = "blockr.pharma"),
      script = "flag-filter-block.js"
    ),
    htmltools::htmlDependency(
      name = "blockr-pharma-flag-filter-css",
      version = utils::packageVersion("blockr.pharma"),
      src = system.file("css", package = "blockr.pharma"),
      stylesheet = "flag-filter-block.css"
    )
  )
}
