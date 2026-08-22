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
                                  ...) {
  columns <- as.character(columns %||% character())
  selected <- intersect(as.character(selected %||% character()), columns)

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

        push_meta <- function() {
          d <- tryCatch(data(), error = function(e) NULL)
          session$sendCustomMessage(
            "pharma-flag-meta",
            list(
              id       = ns("flags"),
              columns  = flag_column_meta(d, r_cols()),
              choices  = as.list(flag_all_columns(d)),
              selected = as.list(r_sel())
            )
          )
        }

        # The TRUE union count, not the client's sum of per-flag counts: a
        # row carrying two flags is one row, so summing overshoots and can
        # print more rows than the table has. Pushed on every change,
        # including the client's own, which the metadata echo deliberately
        # suppresses.
        push_counts <- function() {
          d <- tryCatch(data(), error = function(e) NULL)
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
              list(ok = TRUE, shape = flag_input_shape(data())),
              error = function(e) list(ok = FALSE, cond = e)
            )
          )
        })

        list(
          expr = shiny::reactive({
            derived <- shape_rv()
            shiny::req(derived)
            if (!isTRUE(derived$ok)) stop(derived$cond)
            make_flag_filter_expr(r_sel(), derived$shape)
          }),
          state = list(columns = r_cols, selected = r_sel)
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
      if (!is.data.frame(data)) {
        stop("Input must be a data frame. Flatten a dm first.")
      }
    },
    class = "flag_filter_block",
    expr_type = "bquoted",
    allow_empty_state = c("columns", "selected"),
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

#' Per-column metadata the client renders: label, and the count of rows the
#' box would keep if ticked.
#' @noRd
flag_column_meta <- function(data, columns) {
  lapply(columns, function(cn) {
    out <- list(name = cn, label = "", count = NA, total = NA)
    if (!is.data.frame(data) || !cn %in% names(data)) return(out)
    col <- data[[cn]]
    lbl <- attr(col, "label", exact = TRUE)
    out$label <- if (is.null(lbl)) "" else as.character(lbl)[1L]
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
  call(".", as.name("data"))
}

#' Union of the ticked boxes. No ticks is no constraint, NOT an empty result.
#' @noRd
make_flag_filter_expr <- function(selected, shape) {
  d <- data_slot()
  selected <- as.character(selected %||% character())
  if (!is.null(shape)) selected <- selected[selected %in% names(shape)]
  if (length(selected) == 0L) {
    return(bquote(dplyr::filter(.(d), TRUE), list(d = d)))
  }
  conds <- lapply(selected, flag_condition_expr, df = shape)
  combined <- Reduce(function(a, b) bquote(.(a) | .(b)), conds)
  as.call(list(quote(dplyr::filter), d, combined))
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
