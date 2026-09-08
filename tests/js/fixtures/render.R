# Render the markup the patient profile's JavaScript runs against.
#
#   Rscript tests/js/fixtures/render.R        (from the package root)
#
# The block's own R renders every fixture: the static UI, then each renderUI
# output after the module has seen a twelve-patient cohort and a pick. The
# JS tests mount these files, so the markup contract between R and JS is
# whatever this script last wrote, and a change in R shows up as a diff in
# git before it shows up as a JS test that no longer matches anything.
#
# The cohort is safetyData's first twelve subjects with three chemistry
# parameters and two vitals, which gives the sidebar a lab band with
# reference limits, a spans band, a parameter panel that expands to several
# On rows, and adverse events to search.

pkgload::load_all(".", quiet = TRUE)
library(safetyData)

ids <- head(unique(adam_adsl$USUBJID), 12)
adsl <- adam_adsl[adam_adsl$USUBJID %in% ids, ]
adsl$ACTARM <- adsl$TRT01A
adae <- adam_adae[adam_adae$USUBJID %in% ids, ]
adlbc <- adam_adlbc[adam_adlbc$USUBJID %in% ids &
                      adam_adlbc$PARAMCD %in% c("ALB", "ALP", "ALT"), ]
advs <- adam_advs[adam_advs$USUBJID %in% ids &
                    adam_advs$PARAMCD %in% c("TEMP", "PULSE"), ]
dm_obj <- dm::dm(adsl = adsl, adae = adae, adlbc = adlbc, advs = advs)

dir <- file.path("tests", "js", "fixtures")
put <- function(name, html) {
  html <- as.character(html)
  # htmlwidgets mints a random element id per render; pin it to the panel
  # so a regenerated fixture only differs where the markup did. Without the
  # `viz_slot_` prefix, which the block's own slot selector matches on.
  ids <- unique(unlist(regmatches(html, gregexpr("htmlwidget-[0-9a-f]{20}", html))))
  for (i in seq_along(ids)) {
    stem <- sub("^(lab_)?viz_slot_", "", name)
    html <- gsub(ids[[i]], sprintf("htmlwidget-%s-%d", stem, i), html, fixed = TRUE)
  }
  writeLines(html, file.path(dir, paste0(name, ".html")))
}

grab <- function(output, name) {
  v <- output[[name]]
  if (is.list(v) && !is.null(v$html)) v$html else as.character(v)
}

# One module run per profile: the block's outputs after a cohort and a pick,
# plus the static UI rendered under the same namespace the mock session
# hands the module, so every id in every file agrees.
render_profile <- function(prefix, selected) {
  blk <- new_patient_profile_block(selected = selected)
  ui <- blockr.core:::block_expr_ui(blk)
  srv <- blk[["expr_server"]]
  shiny::testServer(srv, args = list(data = function() dm_obj), {
    session$flushReact()
    id <- sub("-x$", "", session$ns("x"))
    put(paste0(prefix, "ui"), htmltools::renderTags(ui(id))$html)
    session$setInputs(pick_subject = ids[1])
    session$flushReact()
    for (nm in c("sidebar_cohort", "panel_picker", "cohort_band_caption",
                 "header_bar", "subject_facts", "chart_area")) {
      put(paste0(prefix, nm), grab(output, nm))
    }
    # The slots are outputs of their own, one per panel on the profile;
    # the chart area lists them.
    area <- as.character(grab(output, "chart_area"))
    slots <- unique(regmatches(area, gregexpr("viz_slot_[A-Za-z0-9_]+", area))[[1]])
    for (nm in slots) put(paste0(prefix, nm), grab(output, nm))
    cat(prefix, "namespace", id, "slots", length(slots), "\n")
  })
}

# The default profile: an events band in the sidebar, a lab panel that
# expands to three On rows.
render_profile("", c("patient_overview", "ae_gantt", "adlbc_all"))
# A lab-only profile: the sidebar band is a series with reference limits.
render_profile("lab_", "adlbc_all")
