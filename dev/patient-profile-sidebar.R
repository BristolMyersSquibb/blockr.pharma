# Patient Profile — the sidebar refinements (search, scroll, remove).
#
# Same three blocks as patient-profile-cohort.R, but with a patient already
# picked, so the charts (and their remove buttons) are there on load:
#
#   data (safetyData ADaM)  ->  cdisc (add dm keys/FKs)  ->  profile
#
# Run from the workspace root (or from the package dir):
#   Rscript blockr.pharma/dev/patient-profile-sidebar.R          # port 3838
#   Rscript blockr.pharma/dev/patient-profile-sidebar.R 4000     # any port
#   BLOCKR_PORT=4000 Rscript blockr.pharma/dev/patient-profile-sidebar.R
#
# Outside the dev container the workspace is the same bind-mounted tree, so
# the command is the same — it only needs an R with the blockr deps and
# safetyData available. The block's CSS is served out of inst/assets via
# system.file(), which pkgload shims onto the SOURCE tree: edit
# inst/assets/css/patient-profile.css and a browser reload picks it up. An
# *installed* blockr.pharma would keep serving its own stale copy.
#
# What to look at, in the Profile view:
#
#   1. Search. Browsing lists GROUPS (Chemistry, Hematology, Vital Signs,
#      read off the study's own PARCAT1/LBCAT); typing answers with the
#      individual SERIES, nested under the group that holds them. Type
#      "alanine": one result, "Alanine Aminotransferase" under "Chemistry".
#      Both rows are check marks — the group check puts the whole card on the
#      panel, the parameter check just that series. A parameter whose card is
#      not on the panel opens it showing that parameter ALONE; unchecking the
#      last one takes the card back off. Click the x (or press Escape) and the
#      browse lists come back.
#
#   2. Scroll. This cohort offers ~19 vizs. AVAILABLE is its own scroll box
#      under the SELECTED list, capped at 60vh, so the card list is no longer
#      what makes the block tall — the charts in use are.
#
#   3. Remove. Hover any chart panel: a muted x sits at its top right. Click
#      it and the viz is deselected — the panel goes and its card slides back
#      into AVAILABLE, exactly as if you had clicked the card.
#
#   4. Add one back. Click a card in AVAILABLE (say "Vitals Panel"): it moves
#      up into SELECTED and its panel appears at the bottom of the chart
#      column. Drag the SELECTED cards to reorder the panels.
#
# NOTE: load_all() ALL of them, never a mix — blockr.dag included. Two ways
# a mix bites:
#
#   - Assets. Packages resolve each other's htmlDependency files via
#     system.file(..., package = "blockr.theme"); pkgload shims that onto the
#     source inst/, but only inside namespaces it loaded. A load_all()'d
#     blockr.pharma behind an *installed* blockr.theme yields src = '' and
#     crashes addResourcePath on page render.
#   - Contracts. An `library(blockr.dag)` here would load whatever dag build is
#     INSTALLED, against a source blockr.dock. If the two disagree on the
#     extension server signature, dock's register_actions() calls the module
#     with the wrong arguments and the board dies at startup with
#     `argument "dag_extension" is missing, with no default`. Same tree, same
#     version string, different build: the source dag is the one to load.
root <- if (file.exists("blockr.pharma/DESCRIPTION")) "." else ".."
for (p in c("blockr.core", "blockr.theme", "blockr.dplyr", "blockr.dm",
            "blockr.pharma", "blockr.dag", "blockr.dock")) {
  pkgload::load_all(file.path(root, p), quiet = TRUE)
}

# Supplies the ADaM example tables (adsl/adae/adlbc/advs) behind the
# "safetydata_adam" choice of the dm example block.
library(safetyData)

# Port: positional arg wins, then BLOCKR_PORT, then 3838 (the only port the
# devcontainer forwards, so the one worth defaulting to).
pp_port <- local({
  arg <- commandArgs(trailingOnly = TRUE)[1L]
  env <- Sys.getenv("BLOCKR_PORT", unset = "")
  raw <- if (!is.na(arg)) arg else if (nzchar(env)) env else "3838"
  port <- suppressWarnings(as.integer(raw))
  if (is.na(port)) stop("Not a port: ", raw, call. = FALSE)
  port
})

# Bind every interface, not the 127.0.0.1 Shiny defaults to: the
# devcontainer forwards the port from OUTSIDE, so a loopback-only server is
# reachable from a browser in the container and from nowhere else.
options(
  shiny.port = pp_port,
  shiny.host = "0.0.0.0",
  "g6R.preserve_elements_position" = TRUE
)
message("Patient profile sidebar demo on http://127.0.0.1:", pp_port, "/")

serve(
  new_dock_board(
    blocks = c(
      data = new_dm_example_block(dataset = "safetydata_adam"),
      cdisc = new_cdisc_dm_block(),
      # `subject` restores the same way a saved board would: the block opens
      # on one patient, so the charts are drawn and the panel x is reachable
      # without picking anyone first.
      profile = new_patient_profile_block(
        selected = c("patient_overview", "ae_gantt", "adlbc_all"),
        subject = "01-701-1015"
      )
    ),
    links = list(
      list(from = "data", to = "cdisc", input = "data"),
      list(from = "cdisc", to = "profile", input = "data")
    ),
    extensions = blockr.dag::new_dag_extension(),
    # safetyData ships no ACTARM, so the arm column must be declared or the
    # profile stops with the undeclared error (see
    # patient-profile-arm-option.R for that flow). TRT01A is the actual arm.
    options = c(
      dock_board_options(),
      new_board_options(new_study_roles_option(arm = "TRT01A"))
    ),
    grids = list(
      Profile = dock_grid("profile"),
      Pipeline = dock_grid("dag"),
      Data = dock_grid(panels("data", "cdisc"))
    ),
    active = "Profile"
  )
)
