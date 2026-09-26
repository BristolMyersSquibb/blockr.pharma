# The reset glyph, the one the crossfilter's "Reset all" wears (blockr.dm,
# crossfilter-block.js ICON_RESET): both stand for an active filter with a
# way back, and a reader who has met one should recognise the other.
PP_ICON_RESET <- paste0(
  '<svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor">',
  '<path fill-rule="evenodd" d="M8 3a5 5 0 1 0 4.546 2.914.5.5 0 1 1 ',
  '.908-.418A6 6 0 1 1 8 2v1z"/>',
  '<path d="M8 4.466V.534a.25.25 0 0 1 .41-.192l2.36 1.966c.12.1.12.284 ',
  '0 .384L8.41 4.658A.25.25 0 0 1 8 4.466z"/></svg>'
)

#' The patient profile block's UI
#'
#' The static frame the server's outputs render into: the stylesheet and the
#' client script as versioned dependencies, the shared select, the layout
#' with its sidebar and chart area, and the one-line mount of the client.
#'
#' @param id The block's namespace.
#' @noRd
pp_block_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # The design system first: tokens, the shared controls and their
    # stylesheets. The profile's stylesheet reads the tokens without
    # fallbacks and its scripts build on blockr-ui.js.
    blockr.ui::controls_dep(),
    # As an htmlDependency, NOT a raw tags$link to the resource path: the
    # dependency's served URL embeds the package version, so a Version
    # bump busts browser caches. A bare link URL never changes, and the
    # browser happily keeps a stale stylesheet across reloads (and
    # load_all()s) -- the inst/js convention, applied to CSS.
    htmltools::htmlDependency(
      "blockr-pharma-pp",
      as.character(utils::packageVersion("blockr.pharma")),
      src = system.file("assets", package = "blockr.pharma"),
      stylesheet = "css/patient-profile.css"
    ),
    # The client half, in parts: pp-core.js first (it owns the registry
    # the others register with), then one file per region of the block.
    htmltools::htmlDependency(
      "blockr-pharma-pp-js",
      as.character(utils::packageVersion("blockr.pharma")),
      src = system.file("js", package = "blockr.pharma"),
      script = c("pp-core.js", "pp-header.js", "pp-cohort.js",
                 "pp-picker.js", "pp-panels.js", "pp-slots.js")
    ),
    shiny::div(
      class = "pp-layout", id = ns("pp_layout"),

      # Left sidebar
      shiny::div(
        class = "pp-sidebar", id = ns("pp_sidebar"),

        # No title row. "Profile" named the block you are already inside,
        # and the pin next to it toggled the sidebar -- which the cohort
        # tag in the toolbar now does from a place you can see when this
        # is shut.

        # Search: one box, two tenants, the panels and the patients.
        shiny::div(class = "pp-sidebar-search",
          shiny::div(class = "pp-sidebar-search-wrapper",
            shiny::span(class = "pp-sidebar-search-icon",
                        shiny::HTML(PP_ICON_SEARCH)),
            shiny::tags$input(
              type = "text",
              class = "pp-sidebar-search-input",
              id = ns("search"),
              placeholder = "Search panels and patients",
              `aria-label` = "Search panels and patients"
            ),
            # Clear: appears only while the box has text.
            shiny::tags$button(
              class = "pp-sidebar-search-clear is-hidden",
              id = ns("search_clear"),
              type = "button",
              `aria-label` = "Clear the search",
              `data-blockr-tooltip` = "Clear the search",
              shiny::HTML(PP_ICON_X)
            )
          )
        ),

        # The cohort. Above the panels because it is what you come back
        # to: panels are set once, patients are stepped through.
        # The panels, above the patients.
        #
        # They were a list here, then a menu behind a toolbar button,
        # and they are a list here again -- with the difference that
        # this one shows what is ON the profile rather than a catalogue
        # of everything. The catalogue only exists while the search box
        # has something in it, so the column costs five rows instead of
        # the whole study's parameter set.
        #
        # Above rather than below the patients for two reasons that only
        # showed up on screen: the list reads top-down in the same order
        # as the cards it controls, so a drag here is a drag next to the
        # thing it moves; and a search puts its hits under the box
        # rather than under 254 patient rows, where they would be off
        # screen.
        shiny::uiOutput(ns("panel_picker")),
        shiny::div(class = "pp-sidebar-section",
          # No heading either. A column of patient ids under a box that
          # says "Search patients", with "254 patients" against its edge,
          # does not need a 10px grey word telling you it is a cohort --
          # and "cohort" is not a term every reader shares.
          # One row above the list: what the strip draws, the id prefix
          # the rows no longer print, and how the list is ordered. They
          # were three rows; a caption saying "Each row shows ALB" and a
          # pill saying "Peak value" are two halves of one sentence, and
          # the sidebar has a well underneath that wants every pixel.
          shiny::uiOutput(ns("cohort_band_caption")),
          # tabindex, so the list can hold focus and the arrow keys
          # reach it. -1 keeps it out of the tab order: it is reached by
          # clicking a patient, not by tabbing past 254 of them.
          shiny::div(class = "pp-cohort-well", id = ns("pp_cohort_well"),
            tabindex = "-1",
            role = "listbox",
            `aria-label` = "Cohort",
            shiny::uiOutput(ns("sidebar_cohort"))
          )
        ),

        # No panel list. The sidebar answers WHO; the panels are a
        # stack of cards a few hundred pixels to the right, and choosing
        # and ordering them from over here meant doing the work in one
        # place and watching the result in another. Adding is the +
        # button in the toolbar, ordering is the grip in each card's own
        # header, and removing is the x that was always there.
        #
        # The sidebar keeps its search, which now searches patients: the
        # panels it used to find are in the picker's own box.
      ),

      # Chart area: a static subject picker, the dynamic header bar (gear
      # popover) and the dynamic chart list. The picker is static so its
      # Blockr.Select container exists before the mount message lands, and
      # so that stepping through patients never rebuilds it. The header bar
      # is split off so flipping r_timeline_mode only invalidates
      # chart_area and the gear popover stays open.
      shiny::div(class = "pp-chart-area",
        pp_head_ui(ns),
        pp_gear_tray_ui(ns),
        shiny::uiOutput(ns("chart_area"))
      )
    ),

    # The client half lives in inst/js/pp-*.js and is mounted here with
    # the two things it needs from R: the namespace every id derives from
    # and the spans band height. A jQuery-ready wrapper, because in a dock
    # panel this fragment can land before the layout it mounts on.
    shiny::tags$script(shiny::HTML(sprintf(
      "$(function() { PatientProfile.mount(%s); });",
      jsonlite::toJSON(
        list(id = id, bandH = pp_cohort_band_h_spans),
        auto_unbox = TRUE
      )
    )))
  )
}
