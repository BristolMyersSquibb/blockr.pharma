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
                 "pp-picker.js", "pp-panels.js")
    ),
    # Blockr.Select: the shared single-select primitive. Its dropdown is
    # portalled to <body>, which is what lets it escape `.pp-chart-area`'s
    # `overflow-y: auto` — a hand-rolled absolute popover gets clipped and
    # scrolls away with the chart list. blockr_blocks_css_dep() carries the
    # canonical `.blockr-field--required-empty` amber cue.
    blockr.dplyr::blockr_blocks_css_dep(),
    blockr.dplyr::blockr_select_dep(),
    shiny::div(
      class = "pp-layout", id = ns("pp_layout"),

      # The check-mark glyph, defined ONCE and referenced by every card,
      # group row and parameter row. Inlined, it was 286 bytes per row
      # and the search results put one on all ~75 of them -- 31KB of
      # identical markup in a sidebar payload of 72KB.
      shiny::HTML(paste0(
        '<svg xmlns="http://www.w3.org/2000/svg" style="display:none" ',
        'aria-hidden="true"><symbol id="', ns("check"), '" ',
        'viewBox="0 0 16 16"><path fill="currentColor" ',
        'd="M13.854 3.646a.5.5 0 0 1 0 .708l-7 7a.5.5 0 0 1-.708 0l-3.5',
        '-3.5a.5.5 0 1 1 .708-.708L6.5 10.293l6.646-6.647a.5.5 0 0 1 ',
        '.708 0z"/></symbol></svg>'
      )),

      # Left sidebar
      shiny::div(
        class = "pp-sidebar", id = ns("pp_sidebar"),

        # No title row. "Profile" named the block you are already inside,
        # and the pin next to it toggled the sidebar -- which the cohort
        # tag in the toolbar now does from a place you can see when this
        # is shut.

        # Search
        shiny::div(class = "pp-sidebar-search",
          shiny::div(class = "pp-sidebar-search-wrapper",
            shiny::span(class = "pp-sidebar-search-icon",
              shiny::HTML(paste0(
                '<svg xmlns="http://www.w3.org/2000/svg" width="16" ',
                'height="16" fill="currentColor" viewBox="0 0 16 16">',
                '<path d="M11.742 10.344a6.5 6.5 0 1 0-1.397 1.398h',
                '-.001q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-',
                '1.414l-3.85-3.85a1 1 0 0 0-.115-.1zM12 6.5a5.5 ',
                '5.5 0 1 1-11 0 5.5 5.5 0 0 1 11 0z"/></svg>'
              ))
            ),
            shiny::tags$input(
              type = "text",
              class = "pp-sidebar-search-input",
              id = ns("search"),
              # Patients only now: the panels this used to
              # find are in the picker's own box.
              # One box, two tenants: the panels above it and the
              # patients below it.
              placeholder = "Search panels and patients..."
            ),
            # Clear: appears only while the box has text. Restores the
            # full list, SELECTED section included.
            shiny::tags$button(
              class = "pp-sidebar-search-clear is-hidden",
              id = ns("search_clear"),
              type = "button",
              title = "Clear search",
              shiny::HTML(paste0(
                '<svg xmlns="http://www.w3.org/2000/svg" width="14" ',
                'height="14" fill="currentColor" viewBox="0 0 16 16">',
                '<path d="M4.646 4.646a.5.5 0 0 1 .708 0L8 7.293l2.646',
                '-2.647a.5.5 0 0 1 .708.708L8.707 8l2.647 2.646a.5.5 0 ',
                '0 1-.708.708L8 8.707l-2.646 2.647a.5.5 0 0 1-.708-.708',
                'L7.293 8 4.646 5.354a.5.5 0 0 1 0-.708z"/></svg>'
              ))
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
        shiny::div(class = "pp-chart-toolbar",
          # WHO is on screen, not a second way to choose them.
          #
          # This was a Blockr.Select over all 254 patients with a stepper
          # either side. The sidebar's cohort list is also a searchable
          # list of all 254, with a band, a sort and hit counts the
          # dropdown never had, so the two competed and the dropdown lost.
          # What is NOT duplicated is saying who you are looking at --
          # the line you want once you have scrolled and the selected row
          # is off screen -- so the control became that instead, plus the
          # facts the sidebar row has no room for. They all come from
          # pp_cohort_frame(), which computed them already.
          #
          # Stepping moved to the keyboard: arrow keys in the cohort
          # list, which is where a reader's hand already is.
          shiny::div(class = "pp-subject-picker", id = ns("pp_picker"),
            # The cohort, and the drawer it lives in.
            #
            # This sat at the far right with the download and the gear --
            # the opposite end of the screen from the thing it opens. On
            # the left it is against the edge the sidebar slides from, and
            # the chevron points at it.
          #
            # It says "254 patients", not "254". Shut, that is a sentence
            # about what is behind the edge; the bare number needed you to
            # already know what it counted. It is also the only way back
            # once the sidebar is closed, since the floating expand button
            # is gone.
            # One control, two jobs, a hairline between them. The count
            # opens the sidebar; the cell beside it undoes an upstream
            # drill and appears only while there is one (the `drill`
            # message, pp-header.js). Two buttons rather than one with two
            # regions, because they are two actions and a button inside a
            # button is not markup; `.pp-cohort-seg` is what makes them
            # read as one -- drilled, the pair takes the crossfilter's
            # "Reset all" colours at the pill's own size, and the divider
            # is the reset's own left border.
            shiny::span(
              class = "pp-cohort-seg",
              id = ns("pp_cohort_seg"),
              shiny::tags$button(
                class = "pp-cohort-count is-hidden",
                id = ns("pp_cohort_count"),
                type = "button",
                title = "Show or hide the cohort",
                shiny::span(class = "pp-cohort-count-car",
                            shiny::HTML("&lsaquo;")),
                shiny::span(class = "pp-cohort-count-n")
              ),
              shiny::tags$button(
                class = "pp-cohort-reset is-hidden",
                id = ns("pp_cohort_reset"),
                type = "button",
                title = "Reset drill-down",
                shiny::HTML(PP_ICON_RESET)
              )
            ),
            shiny::uiOutput(ns("subject_facts"), inline = TRUE),
            shiny::span(class = "pp-subject-gap")
          ),

          shiny::uiOutput(ns("header_bar"))
        ),
        shiny::uiOutput(ns("chart_area"))
      )
    ),

    # The client half lives in inst/js/pp-*.js and is mounted here with
    # the three things it needs from R: the namespace every id
    # derives from, the grip glyph the sidebar rows reuse, and the spans
    # band height. A jQuery-ready wrapper, because in a dock panel this
    # fragment can land before the layout it mounts on.
    shiny::tags$script(shiny::HTML(sprintf(
      "$(function() { PatientProfile.mount(%s); });",
      jsonlite::toJSON(
        list(id = id, grip = pp_grip_glyph(), bandH = pp_cohort_band_h_spans),
        auto_unbox = TRUE
      )
    )))
  )
}
