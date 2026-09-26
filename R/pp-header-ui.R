# The one chevron of the design system (blockr.ui, Blockr.icons.chevron):
# a 12px box, 1.4px stroke. It points down; CSS turns it.
PP_ICON_CHEVRON <- paste0(
  '<svg width="12" height="12" viewBox="0 0 12 12" fill="none" ',
  'stroke="currentColor" stroke-width="1.4" stroke-linecap="round" ',
  'stroke-linejoin="round" aria-hidden="true">',
  '<polyline points="3 4.5 6 7.5 9 4.5"/></svg>'
)

PP_ICON_DOWNLOAD <- paste0(
  '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ',
  'stroke="currentColor" stroke-width="1.5" stroke-linecap="round" ',
  'stroke-linejoin="round" aria-hidden="true">',
  '<path d="M8 2.5v8M4.5 7l3.5 3.5L11.5 7M3 13.5h10"/></svg>'
)

# A thin x, the design system's remove glyph.
PP_ICON_X <- paste0(
  '<svg width="12" height="12" viewBox="0 0 12 12" fill="none" ',
  'stroke="currentColor" stroke-width="1.3" stroke-linecap="round" ',
  'aria-hidden="true"><path d="M3 3l6 6M9 3l-6 6"/></svg>'
)

# Bootstrap's gear-fill at 14px: the design system's gear.
PP_ICON_GEAR <- paste0(
  '<svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor" ',
  'aria-hidden="true"><path d="M9.405 1.05c-.413-1.4-2.397-1.4-2.81 0l-.1',
  '.34a1.464 1.464 0 0 1-2.105.872l-.31-.17c-1.283-.698-2.686.705-1.987 ',
  '1.987l.169.311c.446.82.023 1.841-.872 2.105l-.34.1c-1.4.413-1.4 2.397 0 ',
  '2.81l.34.1a1.464 1.464 0 0 1 .872 2.105l-.17.31c-.698 1.283.705 2.686 ',
  '1.987 1.987l.311-.169a1.464 1.464 0 0 1 2.105.872l.1.34c.413 1.4 2.397 ',
  '1.4 2.81 0l.1-.34a1.464 1.464 0 0 1 2.105-.872l.31.17c1.283.698 2.686-',
  '.705 1.987-1.987l-.169-.311a1.464 1.464 0 0 1 .872-2.105l.34-.1c1.4-.413 ',
  '1.4-2.397 0-2.81l-.34-.1a1.464 1.464 0 0 1-.872-2.105l.17-.31c.698-1.283-',
  '.705-2.686-1.987-1.987l-.311.169a1.464 1.464 0 0 1-2.105-.872zM8 10.93a',
  '2.929 2.929 0 1 1 0-5.86 2.929 2.929 0 0 1 0 5.858z"/></svg>'
)

#' The profile's header row
#'
#' The design system's header row for an output: the subject id is the
#' output title, the facts about them are the sentence under it, the tools
#' sit on the right with the gear last. The count of patients and the reset
#' of a drill-down lead the row as one segment, on the edge the list of
#' patients slides from.
#'
#' Static, so nothing in it re-renders on a pick: the title and the sentence
#' are outputs of their own, the download menu's labels and the gear tray's
#' controls are kept in step by messages (`dl_menu_state`, `gear_state`).
#'
#' @param ns The module's namespace function.
#' @noRd
pp_head_ui <- function(ns) {
  shiny::div(
    class = "pp-head",
    pp_cohort_seg_ui(ns),
    shiny::uiOutput(ns("subject_title"), class = "pp-head-title"),
    shiny::div(
      class = "pp-head-tools",
      pp_download_menu_ui(ns),
      shiny::tags$button(
        class = "blockr-gear-btn",
        id = ns("pp_gear_btn"),
        type = "button",
        shiny::HTML(PP_ICON_GEAR)
      )
    ),
    shiny::uiOutput(ns("subject_facts"), class = "pp-head-sentence")
  )
}

#' The count of patients and the drill-down reset, as one segment
#'
#' The count opens and shuts the list of patients; the reset beside it
#' undoes a drill-down upstream. Two buttons, because they are two actions;
#' the segment is what makes them read as one (design system, "Showing that a
#' block filters"). The reset is always there and disabled while nothing is
#' drilled, so the segment never changes width under the pointer.
#' `pp-header.js` fills in the count, the tooltips and the drilled state.
#'
#' @param ns The module's namespace function.
#' @noRd
pp_cohort_seg_ui <- function(ns) {
  shiny::span(
    class = "pp-cohort-seg is-hidden",
    id = ns("pp_cohort_seg"),
    shiny::tags$button(
      class = "pp-cohort-count",
      id = ns("pp_cohort_count"),
      type = "button",
      `data-blockr-tooltip` = "Hide the list of patients",
      shiny::span(class = "pp-cohort-count-car", shiny::HTML(PP_ICON_CHEVRON)),
      shiny::span(class = "pp-cohort-count-n")
    ),
    shiny::tags$button(
      class = "pp-cohort-reset",
      id = ns("pp_cohort_reset"),
      type = "button",
      disabled = NA,
      `aria-label` = "Reset drill-down",
      shiny::HTML(PP_ICON_RESET)
    )
  )
}

#' The profile's download menu
#'
#' The whole profile as one file, for the patient on screen or for the
#' cohort, in the formats whose writers are installed. An action menu
#' (blockr.ui): each row is a Shiny download link. Rendered once; the
#' `dl_menu_state` message names the cohort's size and hides the section that
#' does not apply, so a pick never re-renders the button. The gate is per
#' row: the exhibit formats need ggplot2 and blockr.viz, the cohort list is a
#' plain xlsx and stays reachable without them.
#'
#' @param ns The module's namespace function.
#' @noRd
pp_download_menu_ui <- function(ns) {
  has_exhibit <- pp_exhibit_ready()
  has_pptx <- has_exhibit && requireNamespace("officer", quietly = TRUE)
  row <- function(id, label, meta, scope) {
    htmltools::tagAppendAttributes(
      blockr.ui::menu_item(shiny::downloadLink(ns(id), label), meta = meta),
      `data-scope` = scope
    )
  }
  title <- function(id, text, scope) {
    htmltools::tagAppendAttributes(
      blockr.ui::menu_section(text),
      id = ns(id),
      `data-scope` = scope
    )
  }
  menu <- blockr.ui::action_menu(
    blockr.ui::tool_button(shiny::HTML(PP_ICON_DOWNLOAD), "Download",
                           id = ns("pp_dl_btn")),
    title("pp_dl_label_patient", "This patient", "patient"),
    if (has_pptx) row("dl_profile_pptx", "PowerPoint", ".pptx", "patient"),
    if (has_exhibit) row("dl_profile_html", "Web page", ".html", "patient"),
    title("pp_dl_label_cohort", "Cohort", "cohort"),
    # First, because it is the one people asked for: the cohort as a list,
    # not as N rendered profiles.
    row("dl_cohort_xlsx", "Patient list", ".xlsx", "cohort"),
    if (has_pptx) row("dl_cohort_pptx", "PowerPoint", ".pptx", "cohort"),
    if (has_exhibit) row("dl_cohort_html", "Web page", ".html", "cohort")
  )
  # Hidden until the first dl_menu_state says there is something to offer.
  htmltools::tagAppendAttributes(menu, id = ns("pp_dl_root"), hidden = NA)
}

#' The profile's gear tray
#'
#' In flow under the header row (blockr.ui's `.blockr-settings`, opened by
#' `Blockr.gearTray`). Its first section, Display, is built by `pp-header.js`
#' from blockr.ui's segmented control and checkboxes when the `gear_state`
#' message arrives; the rest is the `gear_coverage` output: the study
#' variables in use and the panels this study cannot draw.
#'
#' @param ns The module's namespace function.
#' @noRd
pp_gear_tray_ui <- function(ns) {
  shiny::div(
    class = "blockr-settings blockr-settings--beak pp-gear-tray",
    id = ns("pp_gear_tray"),
    shiny::div(class = "pp-gear-display", id = ns("pp_gear_display")),
    shiny::uiOutput(ns("gear_coverage"), class = "pp-gear-coverage")
  )
}

#' The gear tray's read-only sections
#'
#' Study variables: the column behind each role, its name first and its
#' label as meta, as every control that holds a column shows one. Then the
#' panels the incoming tables cannot feed, with what they miss; no section at
#' all when every panel can be drawn.
#'
#' @param cov `pp_coverage_report()` for the current dm.
#' @param roles The resolved study roles.
#' @param dm_obj The normalized dm, for the columns' labels.
#' @noRd
pp_gear_coverage_ui <- function(cov, roles, dm_obj = NULL) {
  label_of <- function(tbl, col) {
    if (is.null(col) || is.null(dm_obj)) return(NULL)
    tabs <- dm::dm_get_tables(dm_obj)
    if (!tbl %in% names(tabs) || !col %in% names(tabs[[tbl]])) return(NULL)
    lbl <- attr(tabs[[tbl]][[col]], "label", exact = TRUE)
    if (is.null(lbl) || !nzchar(lbl) || identical(lbl, col)) NULL else lbl
  }
  role <- function(label, col, tbl, none) {
    shiny::div(
      class = "blockr-settings__field pp-gear-role",
      shiny::span(class = "blockr-label", label),
      if (is.null(col)) {
        shiny::div(class = "pp-gear-value is-none", none)
      } else {
        shiny::div(
          class = "pp-gear-value",
          col,
          if (!is.null(m <- label_of(tbl, col))) {
            shiny::span(class = "pp-gear-meta", m)
          }
        )
      }
    )
  }
  shiny::tagList(
    shiny::div(class = "blockr-settings__title", "Study variables"),
    shiny::div(
      class = "blockr-settings__grid",
      role("Arm", roles$arm, "adsl", "Not found; see the block's error"),
      role("Severity", roles$severity, "adae", "None; the bars are not coloured"),
      role("Treatment start", roles$timeline, "adsl",
           "None; relative days are off")
    ),
    if (length(cov)) {
      shiny::tagList(
        shiny::div(class = "blockr-settings__title",
                   "Not available in this study"),
        shiny::div(
          class = "pp-gear-cov",
          lapply(cov, function(c) {
            shiny::div(
              class = "pp-gear-cov-row",
              c$label,
              shiny::span(class = "pp-gear-meta", c$reason)
            )
          })
        )
      )
    }
  )
}
