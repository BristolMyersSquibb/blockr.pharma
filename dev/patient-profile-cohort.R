# Patient Profile fed a WHOLE COHORT — the in-block patient picker.
#
# Served in the real product chrome: blockr.dock views + the blockr.dag
# pipeline editor. Three blocks, no upstream patient filter:
#
#   data (safetyData ADaM)  ->  cdisc (add dm keys/FKs)  ->  profile
#
# The profile receives 254 subjects. It renders no charts until you choose one:
# whose data you are looking at is never guessed. Pick a patient in the header
# and the block filters its own dm output down to that subject, so the charts
# and anything wired downstream can never disagree.
#
# Things to try:
#   - Open the picker. Search "01-701-10" to filter, click a patient.
#   - Step through the cohort with the < and > arrows.
#   - Open the gear popover, then pick a different patient: the popover stays
#     open (the header does not rebuild on a pick).
#   - The Pipeline view shows the dm flowing in unfiltered; the profile is the
#     block that narrows it.
#   - Pass `subject = "01-701-1015"` to the block below to see how a saved
#     board restores with a patient already selected.
#
# Run from the workspace root (or from the package dir):
#   Rscript blockr.pharma/dev/patient-profile-cohort.R          # port 3838
#   Rscript blockr.pharma/dev/patient-profile-cohort.R 3839     # any port
#   BLOCKR_PORT=3839 Rscript blockr.pharma/dev/patient-profile-cohort.R
#
# NOTE: load_all() ALL of them, never a mix. Packages resolve each other's
# htmlDependency assets via system.file(..., package = "blockr.theme"); pkgload
# shims that onto the source inst/, but only inside namespaces it loaded. A
# load_all()'d blockr.pharma behind an *installed* blockr.theme yields src = ''
# and crashes addResourcePath on page render.

# Works from the workspace root or from the package dir.
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

options(
  shiny.port = pp_port,
  "g6R.preserve_elements_position" = TRUE
)
message("Patient profile cohort demo on http://127.0.0.1:", pp_port, "/")

serve(
  new_dock_board(
    blocks = c(
      data = new_dm_example_block(dataset = "safetydata_adam"),
      cdisc = new_cdisc_dm_block(),
      profile = new_patient_profile_block(
        selected = c("patient_overview", "ae_gantt", "liver_panel")
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
    # Current dock API: named PLAIN list of `grids =` (the old `layouts =` is
    # swallowed by ... and silently ignored). Views are derived from the grids.
    # A grid leaf is a bare panel id, or panels(...) for a tabbed group;
    # "dag_extension" is the DAG panel's extension_id(). A block that appears
    # in no grid is HIDDEN, so all three are listed.
    grids = list(
      Profile = dock_grid("profile"),
      Pipeline = dock_grid("dag_extension"),
      Data = dock_grid(panels("data", "cdisc"))
    ),
    # Land on the profile: the picker is the thing to look at.
    active = "Profile"
  )
)
