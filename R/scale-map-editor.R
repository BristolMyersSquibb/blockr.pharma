# Sidebar editor for the "scale_map" board option. v1 scope (see
# blockr.design/open/cdex-attribute-map): per-binding rows of level + color
# swatch (recolor, add/remove levels, add/remove bindings); pools, shapes and
# linetypes are shown read-only. Edits write the option value through
# set_board_option_value(), so they serialize with the board and consumers
# re-render reactively.
#
# Mechanics: the option UI slot is a container div; the server re-renders its
# content via removeUI()/insertUI() whenever the map *structure* changes
# (color-only changes update the colour inputs in place). All buttons funnel
# through a single "sm_action" input via Shiny.setInputValue, so there are no
# dynamic observers to manage.

scale_map_editor_ui <- function(id) {
  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML(
      ".bsm-editor { font-size: 0.875rem; }
       .bsm-binding { margin-bottom: 0.5rem; }
       .bsm-binding > summary { cursor: pointer; font-weight: 600; }
       .bsm-row { display: flex; align-items: center; gap: 0.4rem;
                  margin: 0.2rem 0 0.2rem 1rem; }
       .bsm-row .form-group, .bsm-row .shiny-input-container {
         margin-bottom: 0; width: 110px; }
       .bsm-row .bsm-level { flex: 1; min-width: 0; overflow: hidden;
         text-overflow: ellipsis; white-space: nowrap; }
       .bsm-note { color: var(--bs-secondary-color, #6c757d);
         margin-left: 1rem; font-size: 0.8em; }
       .bsm-rm { border: none; background: none; color: inherit;
         opacity: 0.5; padding: 0 0.25rem; }
       .bsm-rm:hover { opacity: 1; }
       .bsm-add { display: flex; align-items: center; gap: 0.4rem;
                  margin: 0.3rem 0 0.3rem 1rem; }
       .bsm-add .form-group, .bsm-add .shiny-input-container {
         margin-bottom: 0; }
       .bsm-add input[type='text'] { font-size: 0.875rem; }
       .bsm-addvar { margin-top: 0.5rem; }
       /* colourpicker's popup is 173px wide and anchored left:0 to the
          input; with the 110px swatch at the sidebar's right edge it
          overflows into the scrollbar. Anchor it right instead so it
          grows leftward into the sidebar. */
       .bsm-editor .colourpicker-panel { left: auto; right: 0; }"
    )),
    htmltools::div(
      id = shiny::NS(id, "sm_editor"),
      class = "bsm-editor"
    )
  )
}

scale_map_editor_server <- function(..., session) {
  ns <- session$ns

  st <- new.env(parent = emptyenv())
  st$gen <- 0L
  st$registry <- list() # raw input id -> list(var, level)
  st$rendered_sig <- NULL
  # Bumped on every render. The colour-scanner observer reads it FIRST so a
  # re-render (new generation of input ids) invalidates the observer and it
  # re-subscribes to the new inputs — otherwise its reactive deps stay on
  # the previous generation's ids, which never change again, and swatch
  # edits after any add/remove go unheard.
  st$render_count <- shiny::reactiveVal(0L)

  current_map <- function() {
    as_scale_map(
      blockr.core::get_board_option_or_null("scale_map", session)
    ) %||% new_scale_map()
  }

  write_map <- function(map) {
    blockr.core::set_board_option_value("scale_map", map, session)
  }

  structure_sig <- function(map) {
    rlang::hash(lapply(unclass(map), function(binding) {
      lapply(binding, function(spec) {
        names(spec) %||% paste0("pool", length(spec))
      })
    }))
  }

  js_str <- function(x) {
    as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
  }

  funnel_btn <- function(label, payload_js, class = "bsm-rm") {
    htmltools::tags$button(
      type = "button",
      class = class,
      onclick = sprintf(
        "Shiny.setInputValue(%s, %s, {priority: 'event'})",
        js_str(ns("sm_action")), payload_js
      ),
      label
    )
  }

  static_payload <- function(action, var, level = NULL) {
    entries <- c(
      sprintf("action:%s", js_str(action)),
      sprintf("var:%s", js_str(var)),
      if (!is.null(level)) sprintf("level:%s", js_str(level))
    )
    sprintf("{%s}", paste(entries, collapse = ","))
  }

  binding_tags <- function(var, binding, gen, bi) {
    color <- binding$color
    fixed <- if (!is.null(color) && !is.null(names(color))) color

    level_rows <- if (!is.null(fixed)) {
      lapply(seq_along(fixed), function(li) {
        lv <- names(fixed)[[li]]
        input_id <- sprintf("sm_c_%d_%d_%d", gen, bi, li)
        st$registry[[input_id]] <<- list(var = var, level = lv)
        htmltools::div(
          class = "bsm-row",
          htmltools::span(class = "bsm-level", title = lv, lv),
          colourpicker::colourInput(ns(input_id), NULL,
                                    value = unname(fixed[[li]])),
          funnel_btn("\u00d7", static_payload("rmlev", var, lv))
        )
      })
    }

    notes <- c(
      if (!is.null(color) && is.null(names(color))) {
        sprintf("pool of %d colors, auto-assigned", length(color))
      },
      if (is.null(color)) "auto colors (theme palette)",
      if (!is.null(binding$shape)) {
        sprintf("shapes: %s", paste(
          paste0(names(binding$shape), "=", binding$shape),
          collapse = ", "
        ))
      },
      if (!is.null(binding$linetype)) {
        sprintf("linetypes: %s", paste(
          paste0(names(binding$linetype), "=", binding$linetype),
          collapse = ", "
        ))
      }
    )

    lev_id <- sprintf("sm_nl_%d_%d", gen, bi)
    col_id <- sprintf("sm_nc_%d_%d", gen, bi)
    add_payload <- sprintf(
      "{action:'addlev', var:%s, level:document.getElementById(%s).value, color:document.getElementById(%s).value}",
      js_str(var), js_str(ns(lev_id)), js_str(ns(col_id))
    )

    htmltools::tags$details(
      class = "bsm-binding",
      open = if (!is.null(fixed)) NA,
      htmltools::tags$summary(
        var,
        funnel_btn("\u00d7", static_payload("rmvar", var))
      ),
      level_rows,
      lapply(notes, function(n) htmltools::div(class = "bsm-note", n)),
      htmltools::div(
        class = "bsm-add",
        shiny::textInput(ns(lev_id), NULL, placeholder = "add level..."),
        colourpicker::colourInput(ns(col_id), NULL, value = "#888888"),
        funnel_btn("+", add_payload, class = "bsm-rm")
      )
    )
  }

  render_editor <- function(map) {
    st$gen <- st$gen + 1L
    st$registry <- list()

    gen <- st$gen
    var_id <- sprintf("sm_nv_%d", gen)
    addvar_payload <- sprintf(
      "{action:'addvar', var:document.getElementById(%s).value}",
      js_str(ns(var_id))
    )

    content <- htmltools::tagList(
      htmltools::tags$label("Scales"),
      if (length(map)) {
        lapply(seq_along(map), function(bi) {
          binding_tags(names(map)[[bi]], map[[bi]], gen, bi)
        })
      } else {
        htmltools::div(class = "bsm-note", "No bindings defined")
      },
      htmltools::div(
        class = "bsm-add bsm-addvar",
        shiny::textInput(ns(var_id), NULL, placeholder = "add variable..."),
        funnel_btn("+", addvar_payload, class = "bsm-rm")
      )
    )

    shiny::removeUI(
      selector = sprintf("#%s > *", ns("sm_editor")),
      multiple = TRUE,
      immediate = TRUE,
      session = session
    )
    shiny::insertUI(
      selector = sprintf("#%s", ns("sm_editor")),
      where = "beforeEnd",
      ui = content,
      immediate = TRUE,
      session = session
    )

    st$rendered_sig <- structure_sig(map)
    st$render_count(shiny::isolate(st$render_count()) + 1L)
  }

  obs_value <- shiny::observe({
    map <- current_map()

    if (!identical(structure_sig(map), st$rendered_sig)) {
      render_editor(map)
      return()
    }

    # Structure unchanged: sync colour inputs that drifted (external edits,
    # e.g. assistant or restore).
    for (input_id in names(st$registry)) {
      entry <- st$registry[[input_id]]
      target <- map[[entry$var]][["color"]][[entry$level]]
      cur <- shiny::isolate(session$input[[input_id]])
      if (!is.null(target) && !is.null(cur) &&
            !identical(tolower(cur), tolower(target))) {
        colourpicker::updateColourInput(session, input_id, value = target)
      }
    }
  })

  obs_colors <- shiny::observe({
    st$render_count() # re-subscribe to the current generation's inputs
    map <- shiny::isolate(current_map())
    changed <- FALSE

    for (input_id in names(st$registry)) {
      entry <- st$registry[[input_id]]
      val <- session$input[[input_id]]
      cur <- map[[entry$var]][["color"]][[entry$level]]
      if (!is.null(val) && !is.null(cur) &&
            !identical(tolower(val), tolower(cur))) {
        map[[entry$var]][["color"]][[entry$level]] <- val
        changed <- TRUE
      }
    }

    if (changed) {
      write_map(as_scale_map(unclass(map)))
    }
  })

  obs_action <- shiny::observeEvent(session$input$sm_action, {
    act <- session$input$sm_action
    map <- unclass(current_map())
    var <- act$var %||% ""

    if (identical(act$action, "rmvar") && var %in% names(map)) {
      map[[var]] <- NULL
    } else if (identical(act$action, "rmlev") && var %in% names(map)) {
      color <- map[[var]][["color"]]
      color <- color[setdiff(names(color), act$level)]
      map[[var]][["color"]] <- if (length(color)) color
    } else if (identical(act$action, "addlev") && var %in% names(map)) {
      lv <- trimws(act$level %||% "")
      if (nzchar(lv)) {
        color <- map[[var]][["color"]]
        if (!is.null(color) && is.null(names(color))) {
          return() # pool channel: not editable in v1
        }
        color <- color[setdiff(names(color), lv)]
        map[[var]][["color"]] <- c(color, stats_setNames(act$color, lv))
      }
    } else if (identical(act$action, "addvar")) {
      var <- trimws(var)
      if (nzchar(var) && !var %in% names(map)) {
        map[[var]] <- list()
      }
    }

    new <- as_scale_map(map)
    write_map(new)
    render_editor(new)
  })

  list(obs_value, obs_colors, obs_action)
}

stats_setNames <- function(x, nm) {
  names(x) <- nm
  x
}
