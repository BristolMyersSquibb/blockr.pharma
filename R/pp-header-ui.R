#' The profile's header bar: the download menu and the gear
#'
#' @param ns The module's namespace function.
#' @param gear_disabled Whether relative-day mode is unavailable.
#' @param mode,prestudy,smooth The current timeline mode, pre-treatment
#'   setting and value-line smoothing, as the gear's toggles show them.
#' @noRd
pp_header_bar_ui <- function(ns, gear_disabled, mode, prestudy, smooth) {
  init_mode <- mode
  init_prestudy <- prestudy
  init_smooth <- smooth

  gear_tag <- shiny::div(
    class = "pp-gear-wrap",
    shiny::tags$button(
      class = "pp-gear-btn",
      id = ns("pp_gear_btn"),
      type = "button",
      title = "Block settings",
      shiny::HTML(paste0(
        '<svg xmlns="http://www.w3.org/2000/svg" width="16" ',
        'height="16" fill="currentColor" viewBox="0 0 16 16">',
        '<path d="M8 4.754a3.246 3.246 0 1 0 0 6.492 3.246 ',
        '3.246 0 0 0 0-6.492M5.754 8a2.246 2.246 0 1 1 4.492 ',
        '0 2.246 2.246 0 0 1-4.492 0"/>',
        '<path d="M9.796 1.343c-.527-1.79-3.065-1.79-3.592 ',
        '0l-.094.319a.873.873 0 0 1-1.255.52l-.292-.16c-1.64-',
        '.892-3.433.901-2.54 2.541l.159.292a.873.873 0 0 1-.52 ',
        '1.255l-.319.094c-1.79.527-1.79 3.065 0 3.592l.319.094a',
        '.873.873 0 0 1 .52 1.255l-.16.292c-.892 1.64.901 3.434 ',
        '2.541 2.541l.292-.159a.873.873 0 0 1 1.255.52l.094.319c',
        '.527 1.79 3.065 1.79 3.592 0l.094-.319a.873.873 0 0 1 ',
        '1.255-.52l.292.16c1.64.893 3.434-.902 2.541-2.541l-.159',
        '-.292a.873.873 0 0 1 .52-1.255l.319-.094c1.79-.527 ',
        '1.79-3.065 0-3.592l-.319-.094a.873.873 0 0 1-.52-1.255',
        'l.16-.292c.892-1.64-.902-3.433-2.541-2.54l-.292.159a',
        '.873.873 0 0 1-1.255-.52zm-2.633.283c.246-.835 1.428-',
        '.835 1.674 0l.094.319a1.873 1.873 0 0 0 2.693 1.115l',
        '.291-.16c.764-.415 1.6.42 1.184 1.185l-.159.292a1.873 ',
        '1.873 0 0 0 1.116 2.692l.318.094c.835.246.835 1.428 0 ',
        '1.674l-.319.094a1.873 1.873 0 0 0-1.115 2.693l.16.291c',
        '.415.764-.42 1.6-1.185 1.184l-.291-.159a1.873 1.873 0 ',
        '0 0-2.693 1.116l-.094.318c-.246.835-1.428.835-1.674 ',
        '0l-.094-.319a1.873 1.873 0 0 0-2.692-1.115l-.292.16c-',
        '.764.415-1.6-.42-1.184-1.185l.159-.291A1.873 1.873 0 ',
        '0 0 1.945 8.93l-.319-.094c-.835-.246-.835-1.428 0-',
        '1.674l.319-.094A1.873 1.873 0 0 0 3.06 4.377l-.16-',
        '.292c-.415-.764.42-1.6 1.185-1.184l.292.159a1.873 ',
        '1.873 0 0 0 2.692-1.115z"/></svg>'
      ))
    ),
    shiny::div(
      class = "pp-gear-popover",
      id = ns("pp_gear_popover"),
      shiny::div(class = "pp-popover-row",
        shiny::span(class = "pp-popover-label", "Timeline"),
        shiny::tags$button(
          class = paste(
            "pp-popover-toggle",
            if (gear_disabled) "is-disabled"
          ),
          id = ns("pp_tl_toggle"),
          `data-tl-mode` = init_mode,
          `data-disabled` = if (gear_disabled) "1" else NULL,
          type = "button",
          title = if (gear_disabled) {
            "TRTSDT not available \u2014 relative day disabled"
          } else {
            "Click to switch"
          },
          if (identical(init_mode, "rday")) {
            "Relative day"
          } else {
            "Date"
          }
        )
      ),
      shiny::div(class = "pp-popover-row",
        shiny::span(class = "pp-popover-label", "Pre-treatment"),
        shiny::tags$button(
          class = "pp-popover-toggle",
          id = ns("pp_prestudy_toggle"),
          `data-prestudy` = if (init_prestudy) "1" else "0",
          type = "button",
          title = paste0(
            "Show the full pre-treatment history, or only the ",
            "30-day screening window before treatment start"
          ),
          if (init_prestudy) "Full history" else "Screening only"
        )
      ),
      shiny::div(class = "pp-popover-row",
        shiny::span(class = "pp-popover-label", "Value lines"),
        shiny::tags$button(
          class = "pp-popover-toggle",
          id = ns("pp_smooth_toggle"),
          `data-smooth` = init_smooth,
          type = "button",
          title = paste0(
            "Monotone-smoothed value lines (the curve stays ",
            "inside the measured range), or straight segments"
          ),
          if (identical(init_smooth, "off")) "Straight" else "Smooth"
        )
      ),
      # Data coverage: visuals that can't render for this data,
      # with the reason (missing table or required column). Lets
      # users see what's collected without each one having to be
      # selected first. Hidden behind the gear, not permanent.
      #
      # Its own output, because it is the one part of the header
      # that reads the data. Inlined here, every upstream emission
      # rebuilt the whole header -- including the gear button, which
      # made the gear flash and shut an open popover. Nested, the
      # button and the popover shell stay mounted and only the
      # coverage list re-renders.
      shiny::uiOutput(ns("gear_coverage"))
    )
  )

  # Block-level download menu: the WHOLE profile as one artifact,
  # in the scope the reviewer means. Two sections -- the picked
  # patient (interactive convenience) and the cohort (the
  # profile's true view, one slide group per patient) -- each
  # offering the formats whose writers are installed. Same
  # <details> pattern as the blockr.viz blocks' download control.
  #
  # RENDERED ONCE, like the gear beside it. An output re-rendered
  # per patient switch made the button visibly blink no matter how
  # the recalculating fade was styled -- the DOM swap itself reads
  # as a flicker next to a control that never moves. So the
  # structure is static (both scope sections in the DOM) and a
  # custom message updates the two labels and toggles section
  # visibility; see the dl_menu_state handler in the UI script.
  # The gate is per ENTRY, not per menu: the exhibit formats need
  # ggplot2 and blockr.viz, but the cohort list is a plain xlsx and
  # must stay reachable on a deployment without them.
  dl_tag <- local({
    has_exhibit <- pp_exhibit_ready()
    has_pptx <- has_exhibit &&
      requireNamespace("officer", quietly = TRUE)
    entry <- function(id, label) {
      shiny::downloadLink(ns(id), label)
    }
    shiny::tags$details(
      id = ns("pp_dl_root"),
      class = "pp-dl-menu is-hidden",
      shiny::tags$summary(
        class = "pp-dl-btn",
        title = "Download profile",
        `aria-label` = "Download profile",
        shiny::HTML(paste0(
          '<svg width="14" height="14" viewBox="0 0 16 16" ',
          'fill="none" stroke="currentColor" stroke-width="1.6" ',
          'stroke-linecap="round" stroke-linejoin="round">',
          '<path d="M8 2.5 V10 M4.8 7 L8 10.2 L11.2 7"/>',
          '<path d="M2.5 11.5 V12.8 A1.2 1.2 0 0 0 3.7 14 H12.3 ',
          'A1.2 1.2 0 0 0 13.5 12.8 V11.5"/></svg>'
        ))
      ),
      shiny::div(
        class = "pp-dl-menu-list", role = "menu",
        shiny::div(
          class = "pp-dl-scope pp-dl-scope-patient is-hidden",
          shiny::div(class = "pp-dl-menu-label",
                     id = ns("pp_dl_label_patient"),
                     "This patient"),
          if (has_pptx) {
            entry("dl_profile_pptx", "PowerPoint (.pptx)")
          },
          if (has_exhibit) {
            entry("dl_profile_html", "Web page (.html)")
          }
        ),
        shiny::div(
          class = "pp-dl-scope pp-dl-scope-cohort is-hidden",
          shiny::div(class = "pp-dl-menu-label",
                     id = ns("pp_dl_label_cohort"),
                     "Cohort"),
          # First, because it is the one people asked for: the
          # cohort as a LIST, not as N rendered profiles.
          entry("dl_cohort_xlsx", "Patient list (.xlsx)"),
          if (has_pptx) {
            entry("dl_cohort_pptx", "PowerPoint (.pptx)")
          },
          if (has_exhibit) {
            entry("dl_cohort_html", "Web page (.html)")
          }
        )
      )
    )
  })

  # The header row carries the download menu and the gear. The
  # subject picker lives in the static UI (see `ui=` below) so its
  # Blockr.Select container is present before the mount message
  # arrives.
  shiny::div(
    class = paste(
      "pp-cohort-hint d-flex justify-content-end",
      "align-items-center"
    ),
    dl_tag,
    gear_tag
  )
}

#' The gear's coverage section: the study variables in use and which
#' panels the incoming tables cannot feed.
#'
#' @param cov `pp_coverage_report()` for the current dm.
#' @param roles The resolved study roles.
#' @noRd
pp_gear_coverage_ui <- function(cov, roles) {
  shiny::tagList(
    shiny::div(class = "pp-popover-divider"),
    shiny::div(class = "pp-popover-section-label",
      "Study variables"),
    shiny::div(class = "pp-coverage-item",
      shiny::span(class = "pp-coverage-label", "Arm"),
      shiny::span(class = "pp-coverage-reason",
        roles$arm %||% "unresolved \u2014 see block error")
    ),
    shiny::div(class = "pp-coverage-item",
      shiny::span(class = "pp-coverage-label", "Severity"),
      shiny::span(class = "pp-coverage-reason",
        roles$severity %||% "none in adae (bars uncolored)")
    ),
    shiny::div(class = "pp-coverage-item",
      shiny::span(class = "pp-coverage-label", "Timeline"),
      shiny::span(class = "pp-coverage-reason",
        roles$timeline %||% "none (relative day off)")
    ),
    shiny::div(class = "pp-popover-divider"),
    shiny::div(class = "pp-popover-section-label",
      "Data coverage"),
    if (length(cov) == 0L) {
      shiny::div(class = "pp-coverage-ok", "All visuals available")
    } else {
      lapply(cov, function(c) {
        shiny::div(class = "pp-coverage-item",
          shiny::span(class = "pp-coverage-label", c$label),
          shiny::span(class = "pp-coverage-reason", c$reason)
        )
      })
    }
  )
}
