# Reordering the panels is a drag of the cards in the sidebar's SELECTED
# list, and an element is only draggable if it says so in the DOM. The client
# maintains the attribute as cards move between the two lists, but it cannot
# be the only place it is set: at boot the `sync_selected` message races the
# sidebar renderUI and usually arrives first, finding no cards to mark. A
# board that opens with vizs already selected -- every CDEx board does -- then
# had a SELECTED list where nothing was draggable, and reordering did not work
# at all until the user happened to toggle a card and force a resync.

skip_if_not_installed("shiny")

pp_drag_dm <- function() {
  dm::dm(
    adsl = data.frame(
      USUBJID = "A", ACTARM = "Placebo",
      TRTSDT = as.Date("2020-01-01"), TRTEDT = as.Date("2020-06-01"),
      stringsAsFactors = FALSE
    ),
    adae = data.frame(
      USUBJID = "A", AEDECOD = "HEADACHE",
      ASTDT = as.Date("2020-02-01"), AESEV = "MILD",
      stringsAsFactors = FALSE
    )
  )
}

# The rendered card for one viz id, as a single line of HTML.
pp_card_html <- function(html, viz_id) {
  cards <- unlist(strsplit(html, "<div class=\"pp-card", fixed = TRUE))
  hit <- grep(paste0("data-viz-id=\"", viz_id, "\""), cards, fixed = FALSE)
  if (length(hit) == 0L) return(NA_character_)
  cards[[hit[1L]]]
}

test_that("cards the server renders selected are draggable from the start", {
  blk <- new_patient_profile_block(selected = "patient_overview")
  srv <- blk[["expr_server"]]

  shiny::testServer(srv, args = list(data = function() pp_drag_dm()), {
    session$flushReact()
    html <- as.character(output$sidebar_cards$html)

    sel <- pp_card_html(html, "patient_overview")
    expect_false(is.na(sel))
    expect_match(sel, "is-selected")
    expect_match(sel, "draggable=\"true\"")
  })
})

test_that("unselected cards are not draggable", {
  blk <- new_patient_profile_block(selected = "patient_overview")
  srv <- blk[["expr_server"]]

  shiny::testServer(srv, args = list(data = function() pp_drag_dm()), {
    session$flushReact()
    html <- as.character(output$sidebar_cards$html)

    # ae_gantt is available for this dm but not selected: it sits in the
    # AVAILABLE list, where dragging means nothing.
    unsel <- pp_card_html(html, "ae_gantt")
    expect_false(is.na(unsel))
    expect_false(grepl("is-selected", unsel, fixed = TRUE))
    expect_false(grepl("draggable", unsel, fixed = TRUE))
  })
})
