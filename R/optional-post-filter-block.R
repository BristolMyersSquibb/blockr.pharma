# Optional post-filter block -------------------------------------------------
#
# A small, domain-agnostic switch for study-specific transforms that belong
# after filters in a branch. The transform itself can live in a study package;
# this block owns the persisted on/off control and the pass-through behaviour.

#' Apply an optional post-filter transform
#'
#' Applies `transform` to `data` only when `enabled` is `TRUE`. When `note_col`
#' is supplied, the returned data also carries that column: `note` when enabled,
#' and `""` when disabled.
#'
#' @param data A data frame.
#' @param enabled Logical flag controlling whether `transform` is applied.
#' @param transform Function or function name. Namespaced strings such as
#'   `"pkg::fun"` are recommended for saved workflows.
#' @param note_col Optional column name for a display note.
#' @param note Optional display note written into `note_col` when enabled.
#'
#' @return A data frame.
#' @export
apply_optional_post_filter <- function(data,
                                       enabled,
                                       transform = "identity",
                                       note_col = NULL,
                                       note = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  out <- data
  if (isTRUE(enabled)) {
    fun <- resolve_optional_post_filter_transform(transform)
    out <- fun(out)
    if (!is.data.frame(out)) {
      stop("`transform` must return a data frame.", call. = FALSE)
    }
  }

  note_col <- optional_post_filter_scalar(note_col)
  if (!is.null(note_col) && nzchar(note_col)) {
    out[[note_col]] <- if (isTRUE(enabled)) {
      rep(optional_post_filter_scalar(note) %||% "", nrow(out))
    } else {
      rep("", nrow(out))
    }
  }

  out
}

resolve_optional_post_filter_transform <- function(transform) {
  if (is.function(transform)) {
    return(transform)
  }

  transform <- optional_post_filter_scalar(transform)
  if (is.null(transform) || !nzchar(transform) ||
      identical(transform, "identity")) {
    return(identity)
  }

  parts <- strsplit(transform, "::", fixed = TRUE)[[1L]]
  if (length(parts) == 2L && all(nzchar(parts))) {
    return(getExportedValue(parts[[1L]], parts[[2L]]))
  }

  stop(
    "`transform` must be a function or a namespaced function string like ",
    "\"pkg::fun\".",
    call. = FALSE
  )
}

optional_post_filter_scalar <- function(x) {
  if (is.null(x) || !length(x)) {
    return(NULL)
  }
  x <- as.character(x)[[1L]]
  if (is.na(x)) NULL else x
}

make_optional_post_filter_expr <- function(enabled,
                                           transform,
                                           note_col = NULL,
                                           note = NULL) {
  d <- data_slot()
  enabled <- isTRUE(enabled)
  transform <- optional_post_filter_scalar(transform) %||% "identity"
  note_col <- optional_post_filter_scalar(note_col)
  note <- optional_post_filter_scalar(note)
  bquote(
    blockr.pharma::apply_optional_post_filter(
      .(d),
      enabled = .(enabled),
      transform = .(transform),
      note_col = .(note_col),
      note = .(note)
    ),
    list(d = d, enabled = enabled, transform = transform,
         note_col = note_col, note = note)
  )
}

#' Optional post-filter block
#'
#' A data-frame transform block with one persisted checkbox. When the checkbox
#' is off, the input passes through unchanged apart from the optional note
#' column. When it is on, the named transform function is applied.
#'
#' This is meant for reusable, board-visible study options that should sit
#' after filtering in a branch, such as pooling terms before downstream
#' summaries and plots.
#'
#' @param enabled Whether the transform starts enabled.
#' @param label Checkbox label.
#' @param transform Function name to apply when enabled. Use a namespaced
#'   string, e.g. `"pkg::fun"`, so saved workflows restore cleanly.
#' @param note Optional note to write when enabled.
#' @param note_col Optional column to receive `note` when enabled and `""`
#'   when disabled.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A blockr transform block.
#' @export
new_optional_post_filter_block <- function(enabled = FALSE,
                                           label = "Apply optional transform",
                                           transform = "identity",
                                           note = NULL,
                                           note_col = NULL,
                                           ...) {
  enabled <- isTRUE(enabled)
  label <- optional_post_filter_scalar(label) %||% "Apply optional transform"
  transform <- optional_post_filter_scalar(transform) %||% "identity"
  note <- optional_post_filter_scalar(note)
  note_col <- optional_post_filter_scalar(note_col)

  blockr.core::new_transform_block(
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        r_enabled <- shiny::reactiveVal(enabled)
        r_label <- shiny::reactiveVal(label)
        r_transform <- shiny::reactiveVal(transform)
        r_note <- shiny::reactiveVal(note)
        r_note_col <- shiny::reactiveVal(note_col)

        shiny::observeEvent(input$enabled, {
          r_enabled(isTRUE(input$enabled))
        })

        list(
          expr = shiny::reactive({
            make_optional_post_filter_expr(
              r_enabled(),
              r_transform(),
              r_note_col(),
              r_note()
            )
          }),
          state = list(
            enabled = r_enabled,
            label = r_label,
            transform = r_transform,
            note = r_note,
            note_col = r_note_col
          )
        )
      })
    },
    ui = function(id) {
      shiny::tags$div(
        class = "block-container",
        shiny::checkboxInput(
          inputId = shiny::NS(id, "enabled"),
          label = label,
          value = enabled
        )
      )
    },
    dat_valid = function(data) {
      if (!is.data.frame(data)) {
        stop("Input must be a data frame.", call. = FALSE)
      }
    },
    class = "optional_post_filter_block",
    expr_type = "bquoted",
    allow_empty_state = c("enabled", "label", "transform", "note", "note_col"),
    ...
  )
}
