# AE parameters block -------------------------------------------------------

#' AE parameters block
#'
#' Combines an AE flag filter and an optional AEDECOD pooling switch in one
#' panel. The flag section filters rows first; the pooling section then
#' optionally applies a named transform and writes a note column for downstream
#' captions.
#'
#' @param columns Character vector of AE flag column names.
#' @param selected Character vector of initially selected flag columns.
#' @param footnotes Named character vector or list used for `{filters}` text.
#' @param definitions Named character vector or list of flag definitions to
#'   display.
#' @param definition_labels Named character vector or list of display labels for
#'   `definitions`.
#' @param pool_aedecod Whether AEDECOD pooling starts enabled.
#' @param pool_label Checkbox label for the pooling control.
#' @param pool_transform Namespaced transform function string to apply when
#'   pooling is enabled.
#' @param pool_note Optional note written into `pool_note_col` when pooling is
#'   enabled.
#' @param pool_note_col Optional column to receive `pool_note` when pooling is
#'   enabled and `""` when disabled.
#' @param table Name of a table in an incoming `dm`, or `NULL`.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A blockr transform block.
#' @export
new_ae_parameters_block <- function(columns = character(),
                                    selected = character(),
                                    footnotes = character(),
                                    definitions = character(),
                                    definition_labels = footnotes,
                                    pool_aedecod = FALSE,
                                    pool_label = "Pool AEDECOD terms",
                                    pool_transform = "identity",
                                    pool_note = NULL,
                                    pool_note_col = NULL,
                                    table = NULL,
                                    ...) {
  footnotes <- flag_clean_footnotes(footnotes)
  definitions <- ae_parameters_named_text(definitions)
  definition_labels <- ae_parameters_named_text(definition_labels)
  pool_aedecod <- isTRUE(pool_aedecod)
  pool_label <- optional_post_filter_scalar(pool_label) %||% "Pool AEDECOD terms"
  pool_transform <- optional_post_filter_scalar(pool_transform) %||% "identity"
  pool_note <- optional_post_filter_scalar(pool_note)
  pool_note_col <- optional_post_filter_scalar(pool_note_col)

  base <- new_flag_filter_block(
    columns = columns,
    selected = selected,
    footnotes = footnotes,
    table = table
  )
  base_server <- base$expr_server
  base_ui <- base$expr_ui

  blockr.core::new_transform_block(
    server = function(id, data) {
      force(columns)
      force(selected)
      force(footnotes)
      force(definitions)
      force(definition_labels)
      force(pool_aedecod)
      force(pool_label)
      force(pool_transform)
      force(pool_note)
      force(pool_note_col)
      force(table)

      out <- base_server(id, data)
      shiny::moduleServer(id, function(input, output, session) {
        r_pool <- shiny::reactiveVal(pool_aedecod)

        shiny::observeEvent(input$pool_aedecod, {
          r_pool(isTRUE(input$pool_aedecod))
        })

        flag_expr <- out$expr
        out$expr <- shiny::reactive({
          ae_parameters_expr(
            flag_expr(),
            r_pool(),
            pool_transform,
            pool_note_col,
            pool_note
          )
        })
        out$state$definitions <- shiny::reactiveVal(definitions)
        out$state$definition_labels <- shiny::reactiveVal(definition_labels)
        out$state$pool_aedecod <- r_pool
        out$state$pool_label <- shiny::reactiveVal(pool_label)
        out$state$pool_transform <- shiny::reactiveVal(pool_transform)
        out$state$pool_note <- shiny::reactiveVal(pool_note)
        out$state$pool_note_col <- shiny::reactiveVal(pool_note_col)
        out
      })
    },
    ui = function(id) {
      ae_parameters_ui(
        base_ui(id),
        definitions,
        definition_labels,
        pool_aedecod,
        pool_label,
        id
      )
    },
    dat_valid = base$dat_valid,
    class = c("ae_parameters_block", "flag_filter_block"),
    expr_type = attr(base, "expr_type", exact = TRUE),
    allow_empty_state = c(
      "columns", "selected", "footnotes", "definitions",
      "definition_labels", "pool_aedecod", "pool_label",
      "pool_transform", "pool_note", "pool_note_col", "table"
    ),
    ...
  )
}

ae_parameters_expr <- function(flag_expr,
                               pool_aedecod,
                               pool_transform,
                               pool_note_col = NULL,
                               pool_note = NULL) {
  pool_aedecod <- isTRUE(pool_aedecod)
  pool_transform <- optional_post_filter_scalar(pool_transform) %||% "identity"
  pool_note_col <- optional_post_filter_scalar(pool_note_col)
  pool_note <- optional_post_filter_scalar(pool_note)

  bquote(
    blockr.pharma::apply_optional_post_filter(
      .(flag_expr),
      enabled = .(pool_aedecod),
      transform = .(pool_transform),
      note_col = .(pool_note_col),
      note = .(pool_note)
    ),
    list(
      flag_expr = flag_expr,
      pool_aedecod = pool_aedecod,
      pool_transform = pool_transform,
      pool_note_col = pool_note_col,
      pool_note = pool_note
    )
  )
}

ae_parameters_ui <- function(ui,
                             definitions,
                             definition_labels,
                             pool_aedecod,
                             pool_label,
                             id) {
  htmltools::tagList(
    ae_parameters_style(),
    ae_parameters_transform_first_block_container(ui, function(children) {
      c(
        list(ae_parameters_section_title("Flags")),
        children,
        list(
          ae_parameters_definition_ui(definitions, definition_labels),
          ae_parameters_pooling_ui(id, pool_aedecod, pool_label)
        )
      )
    })
  )
}

ae_parameters_pooling_ui <- function(id, pool_aedecod, pool_label) {
  htmltools::tags$div(
    class = "ae-parameters-section ae-parameters-pooling",
    ae_parameters_section_title("Pooling"),
    htmltools::tags$label(
      class = "blockr-checkbox ffb-row ae-parameters-pool-row",
      htmltools::tags$input(
        id = shiny::NS(id, "pool_aedecod"),
        type = "checkbox",
        checked = if (isTRUE(pool_aedecod)) "checked" else NULL
      ),
      htmltools::tags$span(
        class = "blockr-checkbox__box",
        htmltools::HTML(
          paste0(
            '<svg width="10" height="10" viewBox="0 0 16 16" ',
            'fill="currentColor"><path d="M13.854 3.646a.5.5 0 ',
            '0 1 0 .708l-7 7a.5.5 0 0 1-.708 ',
            '0l-3.5-3.5a.5.5 0 1 1 .708-.708L6.5 ',
            '10.293l6.646-6.647a.5.5 0 0 1 .708 0"/></svg>'
          )
        )
      ),
      htmltools::tags$span(class = "ffb-name", pool_label)
    )
  )
}

ae_parameters_section_title <- function(label) {
  htmltools::tags$div(class = "ae-parameters-title", label)
}

ae_parameters_definition_ui <- function(definitions, labels = character()) {
  definitions <- ae_parameters_named_text(definitions)
  labels <- ae_parameters_named_text(labels)
  if (!length(definitions)) {
    return(htmltools::tagList())
  }

  rows <- Map(
    function(name, definition) {
      label <- if (name %in% names(labels)) labels[[name]] else name
      htmltools::tags$div(
        class = "ae-parameters-flag-definition",
        htmltools::tags$span(class = "ae-parameters-flag-definition-label", label),
        htmltools::tags$span(class = "ae-parameters-flag-definition-text", definition)
      )
    },
    names(definitions),
    unname(definitions)
  )

  htmltools::tags$div(
    class = "ae-parameters-flag-definitions",
    htmltools::tags$div(
      class = "ae-parameters-flag-definitions-title",
      "AE Flags Definition:"
    ),
    rows
  )
}

ae_parameters_style <- function() {
  htmltools::tags$style(htmltools::HTML(
    paste(
      ".ae-parameters-title {",
      "font-weight: 600;",
      "color: var(--blockr-color-text-primary, #111827);",
      "font-size: var(--blockr-font-size-xs, 0.75rem);",
      "margin-bottom: 6px;",
      "}",
      ".ae-parameters-section,",
      ".ae-parameters-flag-definitions {",
      "margin-top: 10px;",
      "padding-top: 10px;",
      "border-top: 1px solid var(--blockr-color-border, #e5e7eb);",
      "}",
      ".ae-parameters-flag-definitions {",
      "font-size: var(--blockr-font-size-xs, 0.75rem);",
      "color: var(--blockr-color-text-muted, #6b7280);",
      "line-height: 1.35;",
      "}",
      ".ae-parameters-flag-definitions-title {",
      "font-weight: 600;",
      "color: var(--blockr-color-text-primary, #111827);",
      "margin-bottom: 4px;",
      "}",
      ".ae-parameters-flag-definition {",
      "display: block;",
      "padding: 2px 0;",
      "}",
      ".ae-parameters-flag-definition-label {",
      "font-weight: 600;",
      "color: var(--blockr-color-text-primary, #111827);",
      "}",
      ".ae-parameters-flag-definition-text {",
      "margin-left: 4px;",
      "}",
      ".ae-parameters-pool-row {",
      "margin-bottom: 0;",
      "}",
      sep = "\n"
    )
  ))
}

ae_parameters_transform_first_block_container <- function(x, transform) {
  found <- FALSE

  transform_node <- function(node) {
    if (found) {
      return(node)
    }
    if (inherits(node, "shiny.tag")) {
      class <- node$attribs$class %||% ""
      classes <- strsplit(class, "\\s+", perl = TRUE)[[1L]]
      if (identical(node$name, "div") && "block-container" %in% classes) {
        node$children <- transform(node$children)
        found <<- TRUE
        return(node)
      }
      node$children <- lapply(node$children, transform_node)
      return(node)
    }
    if (inherits(node, "shiny.tag.list")) {
      out <- lapply(node, transform_node)
      class(out) <- class(node)
      return(out)
    }
    node
  }

  out <- transform_node(x)
  class(out) <- class(x)
  out
}

ae_parameters_named_text <- function(x) {
  if (!length(x) || is.null(names(x))) {
    return(character())
  }
  x <- vapply(x, function(value) {
    value <- as.character(unlist(value, use.names = FALSE))
    if (length(value) && !is.na(value[[1L]])) trimws(value[[1L]]) else ""
  }, character(1L), USE.NAMES = TRUE)
  x[nzchar(names(x)) & nzchar(x)]
}
