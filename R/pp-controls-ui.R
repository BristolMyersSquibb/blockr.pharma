#' A panel's header controls
#'
#' One builder per control type in a viz definition (`viz$controls`):
#' checkbox chips, a toggle, a click-through pill, a find box, radios. Each
#' carries `data-viz-id` and `data-param`; the client turns a click into one
#' `viz_ctrl` input (see pp-panels.js).
#'
#' @param viz The `pp_viz` definition.
#' @param viz_id Its id on the profile.
#' @param dm_obj The patient's dm, for choices read off the data.
#' A control's `label` is optional: declared empty or absent, the dimension
#' name is left off and the control speaks for itself.
#'
#' @param settings The current settings for this panel.
#' @noRd
pp_controls_ui <- function(viz, viz_id, dm_obj, settings) {
  controls <- viz$controls
  if (is.null(controls) || length(controls) == 0) return(NULL)

  # The dimension's name, or nothing.
  #
  # A control that names its own dimension does not need saying twice. The
  # gantts' "LANES  Preferred term" is a dimension and a setting, and the
  # label is what makes the setting mean something; a findings card's value
  # pill reads "% change" under a header that already says ALB, Albumin
  # (g/L), and the word VALUE in front of it only repeated on every card in a
  # 600px rail. So the label is optional, and a control declaring none draws
  # none rather than an empty span (which would still spend the group's 6px
  # gap).
  label_ui <- function(ctrl) {
    lab <- ctrl$label %||% ""
    if (!nzchar(lab)) return(NULL)
    shiny::span(class = "pp-ctrl-label", lab)
  }

  tags <- lapply(names(controls), function(param) {
    ctrl <- controls[[param]]
    input_id <- paste0(viz_id, "__", param)
    cur_val <- settings[[param]] %||% ctrl$default

    if (ctrl$type == "checkbox") {
      # Get choices from data
      choices <- ctrl$choices
      if (is.null(choices) && !is.null(ctrl$choices_from)) {
        tbls <- dm::dm_get_tables(dm_obj)
        for (tbl_name in viz$tables) {
          if (tbl_name %in% names(tbls)) {
            tbl <- as.data.frame(tbls[[tbl_name]])
            col <- ctrl$choices_from
            if (col %in% colnames(tbl)) {
              # Visits come in visit order (AVISITN when present):
              # lexical order puts "Week 10" before "Week 2".
              choices <- if (identical(col, "AVISIT")) {
                pp_visit_levels(tbl)
              } else {
                sort(unique(as.character(tbl[[col]])))
              }
              # Restrict to the viz's declared subset (a findings
              # group's PARAMCDs), in the subset's clinical order.
              if (!is.null(ctrl$choices_subset)) {
                choices <- intersect(ctrl$choices_subset, choices)
              }
              break
            }
          }
        }
      }
      if (is.null(choices)) choices <- character(0)
      if (is.null(cur_val)) cur_val <- choices

      # Build compact multi-select chips. A findings control ships
      # `choice_labels` (PARAMCD -> PARAM), so the chip reads
      # "Alanine Aminotransferase" rather than "ALT"; the code
      # stays the wire value and the untruncated name is the
      # tooltip. Controls with no label map (visits, questionnaire
      # domains) caption themselves, as before.
      labs <- ctrl$choice_labels
      chips <- lapply(choices, function(ch) {
        is_active <- ch %in% cur_val
        full <- if (!is.null(labs) && ch %in% names(labs)) {
          unname(labs[[ch]])
        } else {
          ch
        }
        shiny::tags$button(
          class = paste(
            "pp-ctrl-chip",
            if (is_active) "is-active"
          ),
          `data-viz-id` = viz_id,
          `data-param` = param,
          `data-value` = ch,
          title = if (!identical(full, ch)) paste0(full, " (", ch, ")"),
          pp_param_short(full)
        )
      })
      shiny::div(class = "pp-ctrl-group",
        label_ui(ctrl),
        shiny::div(class = "pp-ctrl-chips", chips)
      )
    } else if (ctrl$type == "toggle") {
      is_on <- isTRUE(cur_val)
      shiny::div(class = "pp-ctrl-group",
        label_ui(ctrl),
        shiny::tags$button(
          class = paste(
            "pp-ctrl-toggle",
            if (is_on) "is-on"
          ),
          `data-viz-id` = viz_id,
          `data-param` = param,
          shiny::span(class = "pp-ctrl-toggle-track",
            shiny::span(class = "pp-ctrl-toggle-thumb")
          )
        )
      )
    } else if (ctrl$type == "pill") {
      # The house click-through pill: one button carrying the
      # current value, cycling in place (blockr.docs
      # design-system/components/blockr-row.md). Used here for an
      # ordered ladder, so the cycle wraps coarse back to granular
      # rather than dead-ending. See pp_lane_control().
      choices <- ctrl$choices
      if (is.null(choices)) choices <- character(0)
      choices <- pp_ctrl_present_choices(
        choices, ctrl, dm_obj, viz$tables
      )
      # Fewer than two rungs is not a choice; draw nothing.
      if (length(choices) < 2L) return(NULL)
      if (is.null(cur_val) || !cur_val %in% choices) {
        cur_val <- choices[1]
      }
      choice_names <- unname(names(choices) %||% choices)
      idx <- match(cur_val, choices)
      nxt <- choice_names[idx %% length(choices) + 1L]

      shiny::div(class = "pp-ctrl-group",
        label_ui(ctrl),
        shiny::tags$button(
          class = "pp-ctrl-pill",
          `data-viz-id` = viz_id,
          `data-param` = param,
          `data-values` = jsonlite::toJSON(unname(choices)),
          `data-labels` = jsonlite::toJSON(choice_names),
          `data-index` = idx - 1L,
          # The pill names the state, so the tooltip is where the
          # action goes: what one click will make it.
          title = paste0("Switch to ", nxt),
          # Reserve the widest rung: the click target must not
          # move out from under the cursor as the label cycles.
          style = paste0(
            "min-width:", max(nchar(choice_names)), "ch"
          ),
          choice_names[idx]
        )
      )
    } else if (ctrl$type == "search") {
      # A find box for the panel's own records. It filters the
      # chart AND the sidebar's cohort band (see r_cohort_marks),
      # so the strip answers "who else had this" while the panel
      # answers "when did this patient have it".
      term <- as.character(cur_val %||% "")
      hits <- pp_ctrl_search_hits(ctrl, dm_obj, viz$tables, term)
      # The modifier names the one group in the row that can give way: on a
      # narrow panel the controls take a row of their own and the box
      # absorbs whatever the pill beside it leaves (see the container query
      # in the stylesheet). A class rather than `:has(> .pp-ctrl-search)`,
      # which restyles the whole document under Shiny (blockr.ui#41).
      shiny::div(class = "pp-ctrl-group pp-ctrl-group--search",
        shiny::div(
          class = paste("pp-ctrl-search",
                        if (nzchar(term)) "is-active"),
          shiny::HTML(pp_search_icon()),
          shiny::tags$input(
            type = "text",
            class = "pp-ctrl-search-input",
            `data-viz-id` = viz_id,
            `data-param` = param,
            placeholder = ctrl$placeholder %||% ctrl$label,
            value = term
          ),
          # The count is the honest feedback: it says how many of
          # this patient's records survived before the panel goes
          # blank, so an empty chart is never mistaken for a
          # patient with no records at all.
          if (!is.null(hits)) {
            shiny::span(class = "pp-ctrl-search-hits",
                        paste0(hits$n, "/", hits$total))
          },
          if (nzchar(term)) {
            shiny::tags$button(
              class = "pp-ctrl-search-clear",
              type = "button",
              `data-viz-id` = viz_id,
              `data-param` = param,
              title = "Clear",
              shiny::HTML("&times;")
            )
          }
        )
      )
    } else if (ctrl$type == "radio") {
      choices <- ctrl$choices
      if (is.null(choices)) choices <- character(0)
      choices <- pp_ctrl_present_choices(
        choices, ctrl, dm_obj, viz$tables
      )
      if (isTRUE(ctrl$choices_present)) {
        if (length(choices) < 2L) return(NULL)
        if (is.null(cur_val) || !cur_val %in% choices) {
          cur_val <- choices[1]
        }
      }
      if (is.null(cur_val)) cur_val <- choices[1]
      choice_names <- names(choices) %||% choices

      btns <- lapply(seq_along(choices), function(ci) {
        is_active <- choices[ci] == cur_val
        shiny::tags$button(
          class = paste(
            "pp-ctrl-radio",
            if (is_active) "is-active"
          ),
          `data-viz-id` = viz_id,
          `data-param` = param,
          `data-value` = choices[ci],
          choice_names[ci]
        )
      })
      shiny::div(class = "pp-ctrl-group",
        label_ui(ctrl),
        shiny::div(class = "pp-ctrl-radios", btns)
      )
    } else {
      NULL
    }
  })
  tags <- Filter(Negate(is.null), tags)
  if (length(tags) == 0) return(NULL)
  shiny::div(class = "pp-chart-controls", tags)
}
