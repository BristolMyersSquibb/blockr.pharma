# Patient Profile — the "study_roles" BOARD option (study-metadata spec).
#
# Same three blocks as patient-profile-cohort.R, but the board carries the
# arm-column option and the data (safetyData ADaM) has NO ACTARM — so the
# board boots into the undeclared error on purpose:
#
#   1. On load, the profile block shows the named error: ADSL carries no
#      ACTARM and no arm column is declared. Loud, not a fallback — the old
#      chain would have quietly drawn ARM, the *planned* arm, in a safety
#      view.
#   2. Open the board settings sidebar (gear, top right) > Study > Arm
#      column. Type TRT01A: the error clears, the picker meta and the
#      overview treatment lane relabel from the actual arm, and the
#      declaration serializes with the board.
#   3. Type a column the data does not carry (say TRT99): the block stops
#      again, naming the column. Declared-but-missing never falls back.
#   4. Clear the field: back to undeclared = ACTARM, which this dataset does
#      not have, so the boot error returns.
#
# Run from the workspace root (or from the package dir):
#   Rscript blockr.pharma/dev/patient-profile-study-roles.R         # port 3838
#   Rscript blockr.pharma/dev/patient-profile-arm-option.R 4000     # any port
#   BLOCKR_PORT=4000 Rscript blockr.pharma/dev/patient-profile-study-roles.R
#
# NOTE: load_all() ALL of them, never a mix (assets + extension contracts;
# see patient-profile-sidebar.R for the long form of why).
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
  "g6R.preserve_elements_position" = TRUE,
  # Match prod: construct a block only once it is needed (see
  # patient-profile-minimal.R for why dev must run the deployed code path).
  blockr.background_construction_delay = Inf
)
message("Patient profile arm-option demo on http://127.0.0.1:", pp_port, "/")

serve(
  new_dock_board(
    blocks = c(
      data = new_dm_example_block(dataset = "safetydata_adam"),
      cdisc = new_cdisc_dm_block(),
      profile = new_patient_profile_block(
        selected = c("patient_overview", "ae_gantt", "liver_panel"),
        subject = "01-701-1015"
      )
    ),
    links = list(
      list(from = "data", to = "cdisc", input = "data"),
      list(from = "cdisc", to = "profile", input = "data")
    ),
    extensions = blockr.dag::new_dag_extension(),
    # Undeclared on purpose: safetyData has no ACTARM, so the boot state IS
    # the error the option exists to make loud. Declare TRT01A in the sidebar
    # to fix it (pass new_study_roles_option(arm = "TRT01A") to boot declared).
    options = c(
      dock_board_options(),
      new_board_options(new_study_roles_option())
    ),
    grids = list(
      Profile = dock_grid("profile"),
      Pipeline = dock_grid(ext("dag")),
      Data = dock_grid(panels("data", "cdisc"))
    ),
    active = "Profile"
  )
)
