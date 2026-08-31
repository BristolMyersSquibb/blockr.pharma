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
#' [flag_zoom_expr()].
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
#' @param table Name of a table in an incoming `dm` to filter, or `NULL` (the
#'   default) to filter the data frame the block is handed. Naming a table
#'   makes the block dm-in, dm-out: that table is narrowed and every other
#'   one passes through with its rows and keys intact. One table per block;
#'   chain a second to narrow another.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_flag_filter_block(
#'       columns  = c("PREFL", "TRTEMFL", "FUPFL"),
#'       selected = "TRTEMFL"
#'     ),
#'     data = list(data = my_adae)
#'   )
#' }
#'
#' @export
new_flag_filter_block <- function(columns = character(),
                                  selected = character(),
                                  table = NULL,
                                  ...) {
  columns <- as.character(columns %||% character())
  selected <- intersect(as.character(selected %||% character()), columns)
  table <- flag_validate_table(table)

  blockr.core::new_transform_block(
    function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        r_cols <- shiny::reactiveVal(columns)
        r_sel <- shiny::reactiveVal(selected)

        # Echo guard: a state change that came FROM the client must not be
        # pushed back, or the widget fights the user mid-click.
        self_write <- new.env(parent = emptyenv())
        self_write$active <- FALSE

        # `meta_only` carries the labels and counts WITHOUT the column set:
        # the client already has that set, having just picked it, and echoing
        # it back would rebuild the picker under the user's cursor.
        # Everything that INSPECTS the input goes through here, so dm mode
        # and frame mode differ in exactly one place.
        target <- function() {
          flag_target_df(tryCatch(data(), error = function(e) NULL), table)
        }

        push_meta <- function(meta_only = FALSE) {
          d <- target()
          session$sendCustomMessage(
            "pharma-flag-meta",
            list(
              id        = ns("flags"),
              columns   = flag_column_meta(d, r_cols()),
              choices   = flag_choice_meta(d),
              selected  = as.list(r_sel()),
              meta_only = meta_only
            )
          )
        }

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

        shiny::observeEvent(r_sel(), push_counts(), ignoreInit = TRUE)

        # The binding announces itself on every (re)bind. In a deferred dock
        # panel the block's script arrives WITH the panel on first visit, so
        # anything pushed before that had no handler and was dropped outright.
        shiny::observeEvent(input$flags_ready, {
          push_meta()
          push_counts()
        })

        shiny::observeEvent(input$flags, {
          incoming <- input$flags
          cols <- as.character(unlist(incoming$columns %||% list()))
          sel <- as.character(unlist(incoming$selected %||% list()))
          self_write$active <- TRUE
          r_cols(cols)
          r_sel(intersect(sel, cols))
        })

        shiny::observeEvent(list(r_cols(), r_sel()), {
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
        shape_rv <- shiny::reactiveVal(NULL)
        shiny::observe({
          shape_rv(
            tryCatch(
              list(ok = TRUE, shape = flag_input_shape(target())),
              error = function(e) list(ok = FALSE, cond = e)
            )
          )
        })

        list(
          expr = shiny::reactive({
            derived <- shape_rv()
            shiny::req(derived)
            if (!isTRUE(derived$ok)) stop(derived$cond)
            make_flag_filter_expr(r_sel(), derived$shape, table)
          }),
          # `table` is configuration, not a control: nothing in the UI edits
          # it, but it must round-trip or a saved board comes back filtering
          # a data frame it is no longer being handed.
          state = list(columns = r_cols, selected = r_sel,
                       table = shiny::reactiveVal(table))
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
    dat_valid = function(data) {
      if (is.null(table)) {
        if (!is.data.frame(data)) {
          stop("Input must be a data frame. Flatten a dm first, or set ",
               "`table` to filter one table of a dm.")
        }
        return(invisible(NULL))
      }
      if (!inherits(data, "dm")) {
        stop("`table = \"", table, "\"` filters one table of a dm, so the ",
             "input must be a dm. Drop `table` to filter a data frame.")
      }
      if (!table %in% names(dm::dm_get_tables(data))) {
        stop("The dm carries no table \"", table, "\".")
      }
    },
    class = "flag_filter_block",
    expr_type = "bquoted",
    allow_empty_state = c("columns", "selected", "table"),
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
#' @noRd
flag_input_shape <- function(data) {
  if (!is.data.frame(data)) return(NULL)
  as.data.frame(data)[0L, , drop = FALSE]
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
make_flag_filter_expr <- function(selected, shape, table = NULL) {
  d <- data_slot()
  selected <- as.character(selected %||% character())
  if (!is.null(shape)) selected <- selected[selected %in% names(shape)]

  cond <- if (length(selected) == 0L) {
    TRUE
  } else {
    conds <- lapply(selected, flag_condition_expr, df = shape)
    Reduce(function(a, b) bquote(.(a) | .(b)), conds)
  }

  if (is.null(table)) {
    return(as.call(list(quote(dplyr::filter), d, cond)))
  }
  flag_zoom_expr(d, table, cond)
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
